#include <iostream>
#include <sstream>
#include <cassert>


#include "utils/hdf5.h"
#include "utils/math.h"
#include "utils/linear_algebra.h"
#include "utils/print.h"
#include "utils/profiling.h"
#include "utils/parallel.h"
#include "utils/logger.h"

#include "parser/optimizers.h"
#include "parser/parser.h"

#include "tests/test_dp_merging.h"

using namespace util;
using namespace util::io;
// using namespace util::math;
// using namespace rnn::wordrep;
using namespace rnn::config;
using namespace rnn::simple_model::test;
using namespace rnn::simple_model;
using namespace rnn::simple_model::optimizer;

// namespace rnn_t = rnn::type;

int main(){
    Logger logger{"rnn_model4", "logs/basic.txt"};
    auto write_param=[&logger](auto i_minibatch, auto const &param){
        std::stringstream ss;
        ss << "model4." << logger.uid_str() <<"."<<i_minibatch;
        write_to_disk(param, ss.str());
    };
    try {
        // test_DPtable();
        // test_dp_merging();
        // test_dp_merging_with_penalty();
        // return 0;
        auto lambda=0.05;

        // auto testset_parsed=ParsedSentences{"1b.s2010.testset.stanford"};
        // auto testset_orig=TokenizedSentences{"1b.s2010.testset"};
        // auto trainset_parsed=ParsedSentences{"1b.s2010.trainset.stanford"};
        // auto trainset_orig=TokenizedSentences{"1b.s2010.trainset"};
        // auto testset_parsed=ParsedSentences{"wsj.test.tree"};
        // auto testset_orig=TokenizedSentences{"wsj.test"};
        // auto trainset_parsed=ParsedSentences{"wsj/wsj.train.known.tree"};
        // auto trainset_orig=TokenizedSentences{"wsj/wsj.train.known"};        
        auto testset_parsed=ParsedSentences{"news_wsj.s2010.test.stanford"};
        auto testset_orig=TokenizedSentences{"news_wsj.s2010.test"};
        // auto trainset_parsed=ParsedSentences{"news_wsj.s2010.train.stanford"};
        // auto trainset_orig=TokenizedSentences{"news_wsj.s2010.train"};
        auto trainset_parsed=ParsedSentences{"wsj.long.s2010.train.tree"};
        auto trainset_orig=TokenizedSentences{"wsj.long.s2010.train"};
        auto testset = SentencePairs{testset_parsed,testset_orig};
        auto trainset = SentencePairs{trainset_parsed,trainset_orig};
        
        logger.info("Read trainset");
        VocaInfo rnn{file_name, voca_name, w2vmodel_name, w2vmodel_f_type};
        // auto param = load_param(rnn_param_store_name, rnn_param_name, DataType::sp);
        auto param = load_param(rnn_param_store_name, "model4.d877053.2000", DataType::dp);
        // auto param = randomParam(0.05);
        // param.bias.span *= rnn::type::float_t{0.0};
        auto get_label_grad=[&](auto const &sent_pair){
            return get_directed_grad(rnn, param, sent_pair);
        };
        auto get_dp_grad=[&](auto const &sent_pair){
            auto nodes = rnn.initialize_tree(sent_pair.original);
            return get_dp_gradient(param, lambda, nodes, sent_pair);
        };
        auto score_diff=[&](){
            auto score_label = scoring_parsed_dataset(rnn, param, testset);
            auto score_dp= dp_scoring_dataset(rnn, param, lambda, testset);
            return score_label-score_dp;
        };        

        logger.info("Prepared data.");

        logger.info("Begin training");
        int64_t i_minibatch{};
        logger.log_testscore(i_minibatch, score_diff());
        write_param(i_minibatch,param);
        print(rnn.voca_vecs.size());
        print(":voca size.\n");
        // optimizer::GradientDescent optimizer{0.0001};
        // AdaGrad optimizer{0.001};
        RMSprop optimizer{0.001, rnn.voca_vecs.size()};
        auto &pairs = trainset.val;
        for(auto epoch=0; epoch<n_epoch; ++epoch){
            for(auto it=pairs.cbegin();it <pairs.cend(); it+= rnn::config::n_minibatch){
                auto beg=it;
                auto end=beg+n_minibatch;
                end=end<pairs.cend()?end:pairs.cend();
                
                auto grad_label = parallel_reducer(beg, end, get_label_grad, Gradient{});
                // grad_label.param *=0.5;
                optimizer.update(param, grad_label.param);
                // grad_label.words *=-1.0;
                // optimizer.update(rnn.voca_vecs, grad_label.words);

                auto grad_dp = parallel_reducer(beg, end, get_dp_grad, Gradient{});
                grad_dp.param *=-1.0;
                optimizer.update(param, grad_dp.param);
                // grad_dp.words *=-1.0;
                // optimizer.update(rnn.voca_vecs, grad_dp.words);

                ++i_minibatch;
                if(i_minibatch%5==0) {
                    logger.log_testscore(i_minibatch,score_diff());
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

    return 0;
}
