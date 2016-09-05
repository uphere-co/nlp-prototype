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

int main(){
    Logger logger{"rnn_model0", "logs/basic.txt"};
    auto write_param=[&logger](auto i_minibatch, auto const &param){
        std::stringstream ss;
        ss << "model0." << logger.uid_str() <<"."<<i_minibatch;
        write_to_disk(param, ss.str());
    };
    try {
        // test_init_rnn();
        // test_read_voca();
        // test_read_word2vec_output();
        // test_forward_backward();
        // test_parallel_reduce();
        // test_rnn_full_step();
        // test_supervised_rnn_full_step();
        // test_read_voca_config();
        // test_SparseGrad();
        // test_backward_wordvec();
        test_fullstep_including_wordvec();
        return 0;
        logger.info("Process begins.");
        auto lines=util::string::readlines(rnn::config::trainset_name);
        // auto lines=util::string::readlines(rnn::config::testset_name);
        auto testset=TokenizedSentences{rnn::config::testset_name};
        logger.info("Read trainset");
        VocaInfo rnn{file_name, voca_name, w2vmodel_name, w2vmodel_f_type};
        // auto param = load_param(rnn_param_store_name, rnn_param_name, DataType::sp);
        auto param = randomParam(0.05);
        param.bias.span *= rnn::type::float_t{0.0};
        auto get_grad = [&](auto sentence){
            auto nodes = rnn.initialize_tree(sentence);
            return get_gradient(param, nodes);
        };                
        logger.info("Prepared data.");
        
        logger.info("Begin training");
        int64_t i_minibatch{};
        logger.log_testscore(i_minibatch,scoring_dataset(rnn, param, testset));
        write_param(i_minibatch,param);
        for(auto epoch=0; epoch<n_epoch; ++epoch){
            for(auto it=lines.cbegin();it <lines.cend(); it+= rnn::config::n_minibatch){
                auto beg=it;
                auto end=beg+n_minibatch;
                end=end<lines.cend()?end:lines.cend();
                optimizer::LBFGSoptimizer optimizer{word_dim*(2*word_dim+2), param,rnn,testset, beg,end};
                optimizer.update();
                // auto grad_sum = parallel_reducer(beg, end, get_grad, Gradient{});
                // auto optimizer = optimizer::GradientDescent{0.0001};
                // optimizer.update(param, grad_sum.param);            
                ++i_minibatch;
                if(i_minibatch%100==0) {
                    logger.log_testscore(i_minibatch,scoring_dataset(rnn, param, testset));
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
