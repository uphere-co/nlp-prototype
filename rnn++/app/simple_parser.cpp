#include <iostream>
#include <cassert>
#include <cmath>

#include <lbfgs.h>

#include "utils/hdf5.h"
#include "utils/math.h"
#include "utils/linear_algebra.h"
#include "utils/print.h"
#include "utils/profiling.h"
#include "utils/parallel.h"

#include "parser/parser.h"

#include "tests/test_simple_model.h"

using namespace util;
using namespace util::io;
using namespace util::math;
using namespace rnn::wordrep;
using namespace rnn::config;
using namespace rnn::simple_model::test;
using namespace rnn::simple_model;
namespace rnn_t = rnn::type;

struct GradientDescent{
    GradientDescent(rnn::type::float_t scale) : scale{scale} {}
    template<typename T, typename TV>
    void update(T &param, T const &grad, TV scale){
        grad_update(param,grad, scale);
    }
    void update(Param &param, Param &grad_sum){
        update(param.w_left.span, grad_sum.w_left.span, scale);
        update(param.w_right.span, grad_sum.w_right.span, scale);
        update(param.bias.span, grad_sum.bias.span, scale);
        update(param.u_score.span, grad_sum.u_score.span, scale);
    }

    rnn::type::float_t scale;
};



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

class LBFGSoptimizer{
    using lfloat_t = lbfgsfloatval_t;
    using c_iter   = TokenizedSentences::c_iter;
public:
    LBFGSoptimizer(int n_dim, Param &param, VocaInfo const &rnn, 
                   TokenizedSentences const &testset, c_iter beg, c_iter end)
    : n_dim{n_dim}, m_x{lbfgs_malloc(n_dim)}, lbfgs_param{},
      param{param},rnn{rnn},testset{testset}, beg{beg},end{end}{
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
    virtual ~LBFGSoptimizer() {lbfgs_free(m_x);}

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
    int update() {
        write_to_ptr(m_x, param);
        auto _evaluate = [](
                  void *instance, const lfloat_t *x, 
                  lfloat_t *g,const int n, const lfloat_t step){
            return reinterpret_cast<LBFGSoptimizer*>(instance)->evaluate(x, g, n, step);
        };
        // std::function<lfloat_t(*)(void*, const lfloat_t*,lfloat_t*,const int, const lfloat_t)> evaluate = _evaluate;
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
               fx, norm_L1(param.w_left.span), norm_L1(param.w_right.span), 
               norm_L1(param.bias.span), norm_L1(param.u_score.span));
        
        return ret;
    }

protected:
    /*
      rnn, sentence -> nodes
      param, nodes -> grad
      i.e.
      foo : param, rnn, sentence -> grad
      bar : param, rnn, beg, end -> grad_sum

      get_grad : sentence -> grad
      get_grad = foo(param, rnn, _)
      f_grad : param -> grad_sum
      f_grad = bar(_, rnn, beg, end)

      scoring_dataset : rnn, param, testset -> score_sum

      
      write_from_ptr : x -> param
      write_to_ptr : param -> x
      write_to_ptr : grad_sum -> g
      evaluate : x, g -> score_sum
      

    */
    lfloat_t evaluate(const lfloat_t *x, lfloat_t *g, const int /*n*/, const lfloat_t /*step*/) {
        auto f_grad = [this](Param const &param){
            auto get_grad = [&](auto sentence){
                auto nodes = this->rnn.initialize_tree(sentence);
                return get_gradient(param, nodes);
            };
            return Param{} - parallel_reducer(this->beg, this->end, get_grad, Param{});
        };
        write_from_ptr(x, param);
        auto grad_sum = f_grad(param);
        write_to_ptr(g, grad_sum);
        lfloat_t fx = -scoring_minibatch(rnn, param, beg, end);
        // printf("fx = %f, w_left=%e, w_right=%e bias=%e u_score=%e   \n", 
        //        fx, norm_L1(param.w_left.span), norm_L1(param.w_right.span), 
        //        norm_L1(param.bias.span), norm_L1(param.u_score.span));
        // std::cout << "From LBFGSoptimizer::evaluate " << fx << " step: " << step<<std::endl;
        return fx;
    }

    
    int n_dim;
    lfloat_t *m_x;
    lbfgs_parameter_t lbfgs_param;

    Param &param;
    VocaInfo const &rnn;
    TokenizedSentences const &testset;
    c_iter beg;
    c_iter end;
};


int main(){
    try {
        // test_init_rnn();
        test_read_voca();
        test_read_word2vec_output();
        test_forwad_backward();
        test_parallel_reduce();
        test_rnn_full_step();
        return 0;

        auto timer=Timer{};
        // auto lines=util::string::readlines(rnn::config::trainset_name);
        auto lines=util::string::readlines(rnn::config::testset_name);
        auto testset=TokenizedSentences{rnn::config::testset_name};
        timer.here_then_reset("Read trainset");
        VocaInfo rnn{file_name, voca_name, w2vmodel_name, word_dim};
        // auto param = load_param(rnn_param_store_name, rnn_param_name, DataType::sp);
        auto param = randomParam(0.1);
        timer.here_then_reset("Preparing data");

        auto get_grad = [&](auto sentence){
            auto nodes = rnn.initialize_tree(sentence);
            return get_gradient(param, nodes);
        };
        using rnn::simple_model::Param;        
        print(scoring_dataset(rnn, param, testset));
        timer.here_then_reset("Begin training");
        for(auto it=lines.cbegin();it <lines.cend(); it+= rnn::config::n_minibatch){
            auto beg=it;
            auto end=beg+n_minibatch;
            end=end<lines.cend()?end:lines.cend();
            auto grad_sum = parallel_reducer(beg, end, get_grad, Param{});
            auto optimizer = LBFGSoptimizer{word_dim*(2*word_dim+2), param,rnn,testset, beg,end};
            optimizer.update();
            // auto optimizer = GradientDescent{0.0001};
            // optimizer.update(param, grad_sum);            
            auto test_score = scoring_dataset(rnn, param, testset);
            print(test_score);
            print(": test score\n");
        }
        timer.here_then_reset("Finish one iteration");
    } catch (H5::Exception &ex) {
        std::cerr << ex.getCDetailMsg() << std::endl;
    } catch (std::exception &e) {
        std::cerr<<"Got "<<e.what()<<std::endl;
    } catch (...) {
        std::cerr << "Unknown exception" << std::endl;
    }
    static_assert(std::is_nothrow_destructible<H5file>::value == true, "");
    static_assert(sizeof(WordBlock::idx_t) == 8, "");

    return 0;
}
