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
        auto testset_parsed=ParsedSentences{"1b.testset.stanford"};
        auto testset_orig=TokenizedSentences{"1b.testset"};
        auto testset = SentencePairs{testset_parsed,testset_orig};
        auto trainset_parsed=ParsedSentences{"1b.trainset.stanford"};
        auto trainset_orig=TokenizedSentences{"1b.trainset"};
        auto trainset = SentencePairs{trainset_parsed,trainset_orig};
        auto &pairs = trainset.val;
        
        logger.info("Read trainset");
        VocaInfo rnn{file_name, voca_name, w2vmodel_name, w2vmodel_f_type};
        // auto param = load_param(rnn_param_store_name, rnn_param_name, DataType::sp);
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
                auto grad_label = parallel_reducer(beg, end, get_label_grad, Param{});
                auto grad_greedy = parallel_reducer(beg, end, get_greedy_grad, Param{});
                auto grad = grad_label-grad_greedy;
                auto optimizer = optimizer::GradientDescent{0.0001};
                optimizer.update(param, grad);            
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
