#include "parser/optimizers.h"
#include "parser/parser.h"

#include "utils/math.h"
#include "utils/parallel.h"

namespace{
using rnn::simple_model::Param;
auto write_to_ptr=[](lbfgsfloatval_t *x_ptr, Param const &param){
    for (auto x : param.w_left.span)  *x_ptr++=x;
    for (auto x : param.w_right.span) *x_ptr++=x;
    for (auto x : param.bias.span)    *x_ptr++=x;
    for (auto x : param.u_score.span) *x_ptr++=x;
};
auto write_from_ptr=[](lbfgsfloatval_t const *x_ptr, Param const &param){
    for (auto &x : param.w_left.span)  x=*x_ptr++;
    for (auto &x : param.w_right.span) x=*x_ptr++;
    for (auto &x : param.bias.span)    x=*x_ptr++;
    for (auto &x : param.u_score.span) x=*x_ptr++;
};



auto accum_adagrad_factor_vec = [](int64_t i, auto &out, auto const &vec){
    out[i] += vec[i]*vec[i];
};
auto accum_adagrad_factor_mat = [](int64_t i, int64_t j, auto &out, auto const &vec){
    out[i][j] += vec[i][j]*vec[i][j];
};
auto adaptive_update_vec = [](int64_t i, auto &out, auto x, auto const &grad, auto const &adagrad_factor){
    out[i] += x *grad[i]/std::sqrt(adagrad_factor[i]+0.0000001);
};
auto adaptive_update_mat = [](int64_t i, int64_t j, auto &out, auto x, auto const &grad, auto const &adagrad_factor){
    out[i][j] += x *grad[i][j]/std::sqrt(adagrad_factor[i][j]+0.0000001);
};
auto accum_rmsprop_factor_vec = [](int64_t i, auto &out, auto const &vec){
    out[i] *=0.98;
    out[i] += 0.02*vec[i]*vec[i];
};
auto accum_rmsprop_factor_mat = [](int64_t i, int64_t j, auto &out, auto const &vec){
    out[i][j] *= 0.98;
    out[i][j] += 0.02*vec[i][j]*vec[i][j];
};

}//nameless namespace


using namespace util;
using namespace util::math;
using namespace rnn::simple_model;
using namespace rnn::simple_model::optimizer;

struct LBFGSoptimizer::impl{
    using c_iter = LBFGSoptimizer::c_iter;
    impl(Param &param, VocaInfo const &rnn,
                         TokenizedSentences const &testset, 
                         c_iter beg, c_iter end)
    :param{param},rnn{rnn},testset{testset}, beg{beg},end{end} {}

    Param &param;
    VocaInfo const &rnn;
    TokenizedSentences const &testset;
    c_iter beg;
    c_iter end;
};

namespace rnn{
namespace simple_model{
namespace optimizer{

void GradientDescent::update(Param &param, Param const &grad_sum){
    update(param.w_left.span, grad_sum.w_left.span, scale);
    update(param.w_right.span, grad_sum.w_right.span, scale);
    update(param.bias.span, grad_sum.bias.span, scale);
    update(param.u_score.span, grad_sum.u_score.span, scale);
}

LBFGSoptimizer::LBFGSoptimizer(int n_dim, Param &param, VocaInfo const &rnn, 
                               TokenizedSentences const &testset, c_iter beg, c_iter end)
    : n_dim{n_dim}, m_x{lbfgs_malloc(n_dim)}, lbfgs_param{},
      pimpl{std::make_unique<impl>(param, rnn, testset, beg, end)} {
    //http://www.chokkan.org/software/liblbfgs/structlbfgs__parameter__t.html
    lbfgs_parameter_init(&lbfgs_param);
    // lbfgs_param.max_iterations=10;
    lbfgs_param.epsilon       =1e-2;
    lbfgs_param.delta         =1e-2;
    if (m_x == NULL) {
    printf("ERROR: Failed to allocate a memory block for variables.\n");
    std::bad_alloc exception;
    throw exception;
    }
}
LBFGSoptimizer::~LBFGSoptimizer() {lbfgs_free(m_x);}

// instance	The user data sent for lbfgs() function by the client.
// x	The current values of variables.
// g	The current gradient values of variables.
// fx	The current value of the objective function.
// xnorm	The Euclidean norm of the variables.
// gnorm	The Euclidean norm of the gradients.
// step	The line-search step used for this iteration.
// n	The number of variables.
// k	The iteration count.
// ls	The number of evaluations called for this iteration.
int LBFGSoptimizer::update() {
    write_to_ptr(m_x, pimpl->param);
    auto _evaluate = [](
                void *instance, const lfloat_t *x, 
                lfloat_t *g,const int n, const lfloat_t step){
        return reinterpret_cast<LBFGSoptimizer*>(instance)->evaluate(x, g, n, step);
    };
    auto _progress =  [](void * /*instance*/, const lfloat_t *x, const lfloat_t * /*g*/, const lfloat_t fx,
                        const lfloat_t xnorm, const lfloat_t gnorm, const lfloat_t step,
                        int /*n*/, int k, int /*ls*/) {
        printf("Iteration %d:\n", k);
        printf("  fx = %f, x[0] = %f, x[1] = %f\n", fx, x[0], x[1]);
        printf("  xnorm = %f, gnorm = %f, step = %f\n", xnorm, gnorm, step);
        printf("\n");
        return 0;
    };
    lfloat_t fx{};
    int ret = lbfgs(n_dim, m_x, &fx, _evaluate, _progress, this, &lbfgs_param);

    /* Report the result. */
    printf("L-BFGS optimization terminated with status code = %d\n", ret);
    printf("fx = %f, w_left=%e, w_right=%e bias=%e u_score=%e\n", 
            fx, norm_L1(pimpl->param.w_left.span), norm_L1(pimpl->param.w_right.span), 
            norm_L1(pimpl->param.bias.span), norm_L1(pimpl->param.u_score.span));
    
    return ret;
}

LBFGSoptimizer::lfloat_t LBFGSoptimizer::evaluate(const lfloat_t *x, lfloat_t *g, 
                                  const int /*n*/, const lfloat_t /*step*/) {
    auto f_grad = [this](Param const &param){
        auto get_grad = [&](auto sentence){
            auto nodes = this->pimpl->rnn.initialize_tree(sentence);
            return get_greedy_gradient(param, nodes);
        };

        return Gradient{} - parallel_reducer(this->pimpl->beg, this->pimpl->end, get_grad, Gradient{});
    };
    write_from_ptr(x, pimpl->param);
    auto grad_sum = f_grad(pimpl->param);
    write_to_ptr(g, grad_sum.param);
    lfloat_t fx = -greedy_scoring_minibatch(pimpl->rnn, pimpl->param, pimpl->beg, pimpl->end);
    // printf("fx = %f, w_left=%e, w_right=%e bias=%e u_score=%e   \n", 
    //        fx, norm_L1(param.w_left.span), norm_L1(param.w_right.span), 
    //        norm_L1(param.bias.span), norm_L1(param.u_score.span));
    // std::cout << "From LBFGSoptimizer::evaluate " << fx << " step: " << step<<std::endl;
    return fx;
}


AdaGrad::AdaGrad(val_t scale)
: ada_scale{scale} {}

void AdaGrad::update(Param &param, Param const &grad){
    matloop_void(accum_adagrad_factor_mat, ada_factor.w_left.span, grad.w_left.span);
    matloop_void(accum_adagrad_factor_mat, ada_factor.w_right.span,grad.w_right.span);
    vecloop_void(accum_adagrad_factor_vec, ada_factor.bias.span ,  grad.bias.span);
    vecloop_void(accum_adagrad_factor_vec, ada_factor.u_score.span,grad.u_score.span);
    matloop_void(adaptive_update_mat, param.w_left.span, ada_scale, grad.w_left.span, ada_factor.w_left.span);
    matloop_void(adaptive_update_mat, param.w_right.span, ada_scale, grad.w_right.span,ada_factor.w_right.span);
    vecloop_void(adaptive_update_vec, param.bias.span, ada_scale, grad.bias.span ,  ada_factor.bias.span);
    vecloop_void(adaptive_update_vec, param.u_score.span, ada_scale, grad.u_score.span,ada_factor.u_score.span);
}

RMSprop::RMSprop(val_t scale, WordBlock::idx_t voca_size)
: ada_factor_voca{voca_size}, ada_scale{scale} {}

void RMSprop::update(Param &param, Param const &grad){
    matloop_void(accum_rmsprop_factor_mat, ada_factor_param.w_left.span, grad.w_left.span);
    matloop_void(accum_rmsprop_factor_mat, ada_factor_param.w_right.span,grad.w_right.span);
    vecloop_void(accum_rmsprop_factor_vec, ada_factor_param.bias.span ,  grad.bias.span);
    vecloop_void(accum_rmsprop_factor_vec, ada_factor_param.u_score.span,grad.u_score.span);
    matloop_void(adaptive_update_mat, param.w_left.span, ada_scale, grad.w_left.span, ada_factor_param.w_left.span);
    matloop_void(adaptive_update_mat, param.w_right.span, ada_scale, grad.w_right.span,ada_factor_param.w_right.span);
    vecloop_void(adaptive_update_vec, param.bias.span, ada_scale, grad.bias.span ,  ada_factor_param.bias.span);
    vecloop_void(adaptive_update_vec, param.u_score.span, ada_scale, grad.u_score.span,ada_factor_param.u_score.span);
}

void RMSprop::update(WordBlock &voca_vecs, SparseGrad const &grad){
    std::vector<SparseGrad::key_t> idxs;
    for(auto const &x:grad.val) idxs.push_back(x.first);
    auto n=idxs.size();
    tbb::parallel_for(decltype(n){0},n,[&](auto i){
        auto idx=idxs[i];
        auto g = grad.val.find(idx);
        auto v=ada_factor_voca[idx];
        vecloop_void(accum_rmsprop_factor_vec, v ,  g->second.span);
        auto wordvec=voca_vecs[idx];
        vecloop_void(adaptive_update_vec, wordvec, ada_scale, g->second.span , v);
    });
}

}//namespace rnn::simple_model::optimizer
}//namespace rnn::simple_model
}//namespace rnn
