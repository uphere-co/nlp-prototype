#include <iostream>
#include <sstream>
#include <cassert>
#include <cmath>


#include "utils/hdf5.h"
#include "utils/math.h"
#include "utils/linear_algebra.h"
#include "utils/print.h"
#include "utils/profiling.h"
#include "utils/parallel.h"
#include "utils/logger.h"

#include "parser/optimizers.h"
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

void write_to_disk(Param const &param, std::string param_name){    
    auto param_raw = param.serialize();
    H5file h5store{H5name{"rnn_params.h5"}, hdf5::FileMode::rw_exist};
    h5store.writeRawData(H5name{param_name}, param_raw);
}

auto accum_adagrad_factor_vec = [](int64_t i, auto &out, auto const &vec){
    out[i] += vec[i]*vec[i];
};
auto accum_adagrad_factor_mat = [](int64_t i, int64_t j, auto &out, auto const &vec){
    out[i][j] += vec[i][j]*vec[i][j];
};
auto adagrad_update_vec = [](int64_t i, auto &out, auto x, auto const &grad, auto const &adagrad_factor){
    out[i] += x *grad[i]/std::sqrt(adagrad_factor[i]+0.0000001);
};
auto adagrad_update_mat = [](int64_t i, int64_t j, auto &out, auto x, auto const &grad, auto const &adagrad_factor){
    out[i][j] += x *grad[i][j]/std::sqrt(adagrad_factor[i][j]+0.0000001);
};
auto accum_rmsprop_factor_vec = [](int64_t i, auto &out, auto const &vec){
    out[i] *=0.9;
    out[i] += 0.1*vec[i]*vec[i];
};
auto accum_rmsprop_factor_mat = [](int64_t i, int64_t j, auto &out, auto const &vec){
    out[i][j] *= 0.9;
    out[i][j] += 0.1*vec[i][j]*vec[i][j];
};
class AdaGrad{
public:
    AdaGrad(rnn_t::float_t scale)
    : ada_scale{scale} {}

    void update(Param &param, Param const &grad){
        matloop_void(accum_adagrad_factor_mat, ada_factor.w_left.span, grad.w_left.span);
        matloop_void(accum_adagrad_factor_mat, ada_factor.w_right.span,grad.w_right.span);
        vecloop_void(accum_adagrad_factor_vec, ada_factor.bias.span ,  grad.bias.span);
        vecloop_void(accum_adagrad_factor_vec, ada_factor.u_score.span,grad.u_score.span);
        matloop_void(adagrad_update_mat, param.w_left.span, ada_scale, grad.w_left.span, ada_factor.w_left.span);
        matloop_void(adagrad_update_mat, param.w_right.span, ada_scale, grad.w_right.span,ada_factor.w_right.span);
        vecloop_void(adagrad_update_vec, param.bias.span, ada_scale, grad.bias.span ,  ada_factor.bias.span);
        vecloop_void(adagrad_update_vec, param.u_score.span, ada_scale, grad.u_score.span,ada_factor.u_score.span);
    }
private:
    util::math::VecLoop_void<rnn_t::float_t,word_dim> vecloop_void{};
    util::math::MatLoop_void<rnn_t::float_t,word_dim,word_dim> matloop_void{};
    Param ada_factor{};
    rnn_t::float_t ada_scale;
};

class RMSprop{
public:
    RMSprop(rnn_t::float_t scale)
    : ada_scale{scale} {}

    void update(Param &param, Param const &grad){
        matloop_void(accum_rmsprop_factor_mat, ada_factor.w_left.span, grad.w_left.span);
        matloop_void(accum_rmsprop_factor_mat, ada_factor.w_right.span,grad.w_right.span);
        vecloop_void(accum_rmsprop_factor_vec, ada_factor.bias.span ,  grad.bias.span);
        vecloop_void(accum_rmsprop_factor_vec, ada_factor.u_score.span,grad.u_score.span);
        matloop_void(adagrad_update_mat, param.w_left.span, ada_scale, grad.w_left.span, ada_factor.w_left.span);
        matloop_void(adagrad_update_mat, param.w_right.span, ada_scale, grad.w_right.span,ada_factor.w_right.span);
        vecloop_void(adagrad_update_vec, param.bias.span, ada_scale, grad.bias.span ,  ada_factor.bias.span);
        vecloop_void(adagrad_update_vec, param.u_score.span, ada_scale, grad.u_score.span,ada_factor.u_score.span);
    }
private:
    util::math::VecLoop_void<rnn_t::float_t,word_dim> vecloop_void{};
    util::math::MatLoop_void<rnn_t::float_t,word_dim,word_dim> matloop_void{};
    Param ada_factor{};
    rnn_t::float_t ada_scale;
};

int main(){
    Logger logger{"rnn_model1", "logs/basic.txt"};
    auto write_param=[&logger](auto i_minibatch, auto const &param){
        std::stringstream ss;
        ss << "model1." << logger.uid_str() <<"."<<i_minibatch;
        write_to_disk(param, ss.str());
    };
    try {
        // test_supervised_rnn_full_step();
        // return 0;
        logger.info("Process begins.");

        // auto testset_parsed=ParsedSentences{"1b.testset.sample.stanford"};
        // auto testset_orig=TokenizedSentences{"1b.testset.sample"};
        // auto testset = SentencePairs{testset_parsed,testset_orig};
        // auto trainset_parsed=ParsedSentences{"1b.testset.stanford"};
        // auto trainset_orig=TokenizedSentences{"1b.testset"};
        // auto testset_parsed=ParsedSentences{"1b.testset.stanford"};
        // auto testset_orig=TokenizedSentences{"1b.testset"};
        // auto trainset_parsed=ParsedSentences{"1b.trainset.stanford"};
        // auto trainset_orig=TokenizedSentences{"1b.trainset"};
        auto testset_parsed=ParsedSentences{"1b.s2010.testset.stanford"};
        auto testset_orig=TokenizedSentences{"1b.s2010.testset"};
        auto trainset_parsed=ParsedSentences{"1b.s2010.trainset.stanford"};
        auto trainset_orig=TokenizedSentences{"1b.s2010.trainset"};
        auto testset = SentencePairs{testset_parsed,testset_orig};
        auto trainset = SentencePairs{trainset_parsed,trainset_orig};
        auto &pairs = trainset.val;
        
        logger.info("Read trainset");
        VocaInfo rnn{file_name, voca_name, w2vmodel_name, w2vmodel_f_type};
        // auto param = load_param(rnn_param_store_name, rnn_param_name, DataType::sp);
        // auto param = load_param(rnn_param_store_name, "model1.563109e7.400", DataType::dp);
        auto param = randomParam(0.05);
        param.bias.span *= rnn::type::float_t{0.0};
        auto get_label_grad=[&](auto const &sent_pair){
            return get_directed_grad(rnn, param, sent_pair);
        };
        auto get_greedy_grad=[&](auto const &sent_pair){
            auto nodes = rnn.initialize_tree(sent_pair.original);
            return get_gradient(param, nodes);
        };
        logger.info("Prepared data.");

        logger.info("Begin training");
        int64_t i_minibatch{};
        logger.log_testscore(i_minibatch, scoring_parsed_dataset(rnn, param, testset));
        write_param(i_minibatch,param);
        for(auto epoch=0; epoch<n_epoch; ++epoch){
            for(auto it=pairs.cbegin();it <pairs.cend(); it+= rnn::config::n_minibatch){
                auto beg=it;
                auto end=beg+n_minibatch;
                end=end<pairs.cend()?end:pairs.cend();
                // optimizer::LBFGSoptimizer optimizer{word_dim*(2*word_dim+2), param,rnn,testset, beg,end};
                // optimizer.update();
                // auto grad_label = parallel_reducer(beg, end, get_label_grad, Param{});
                // auto grad_greedy = parallel_reducer(beg, end, get_greedy_grad, Param{});
                // // grad_label *=0.5;
                // grad_greedy *=0.8;
                // auto grad = grad_label;
                // auto optimizer = optimizer::GradientDescent{0.00001};
                // optimizer.update(param, grad);

                // AdaGrad optimizer{0.001};
                RMSprop optimizer{0.001};
                // optimizer::GradientDescent optimizer{0.0001};
                auto grad_label = parallel_reducer(beg, end, get_label_grad, Param{});
                grad_label *= 0.2;
                optimizer.update(param, grad_label);
                auto grad_greedy = parallel_reducer(beg, end, get_greedy_grad, Param{});
                grad_greedy *=-1.0;
                optimizer.update(param, grad_greedy);

                

                ++i_minibatch;
                if(i_minibatch%100==0) {
                    logger.log_testscore(i_minibatch,scoring_parsed_dataset(rnn, param, testset));
                    write_param(i_minibatch,param);
                }
            }
        }
        logger.info("Finish one iteration");
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
