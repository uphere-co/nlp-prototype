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
using namespace rnn::simple_model::optimizer;
using namespace rnn::simple_model;
using namespace rnn::simple_model::test;

namespace rnn_t = rnn::type;


void l2_normalize(WordBlock &voca_vecs){
    auto n=voca_vecs.size();
    for(decltype(n)i=0; i!=n; ++i){
        auto v=voca_vecs[i];
        auto factor=1/norm_L2(v);
        v*=factor;
    }
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
        
        logger.info("Read trainset");
        VocaInfo rnn{file_name, voca_name, w2vmodel_name, w2vmodel_f_type};
        // auto param = load_param(rnn_param_store_name, rnn_param_name, DataType::sp);
        // auto param = load_param(rnn_param_store_name, "model1.563109e7.400", DataType::dp);
        // l2_normalize(rnn.voca_vecs);
        auto param = randomParam(0.05);
        param.bias.span *= rnn::type::float_t{0.0};
        auto get_label_grad=[&](auto const &sent_pair){
            return get_directed_grad(rnn, param, sent_pair);
        };
        auto get_greedy_grad=[&](auto const &sent_pair){
            auto nodes = rnn.initialize_tree(sent_pair.original);
            return get_greedy_gradient(param, nodes);
        };

        auto score_diff=[&](){
            auto score_label = scoring_parsed_dataset(rnn, param, testset);
            auto score_greedy= greedy_scoring_dataset(rnn, param, testset_orig);
            return score_label-score_greedy;
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
                
                //J_original = s_label - s_greedy
                //J = 0.2*s_label - s_greedy
                // grad.words = dJ/dWord
                // grad.param = dJ/dParam
                // param <- param + grad.param*rate_param
                // words <- words + grad.words*rate_words
                auto grad_label = parallel_reducer(beg, end, get_label_grad, Gradient{});
                grad_label.param *= 0.2;
                optimizer.update(param, grad_label.param);
                grad_label.words *= 0.02;
                optimizer.update(rnn.voca_vecs, grad_label.words);

                auto grad_greedy = parallel_reducer(beg, end, get_greedy_grad, Gradient{});
                grad_greedy.param *=-1.0;
                optimizer.update(param, grad_greedy.param);
                grad_greedy.words *=-0.1;
                optimizer.update(rnn.voca_vecs, grad_label.words);

                ++i_minibatch;
                if(i_minibatch%100==0) {
                    logger.log_testscore(i_minibatch, score_diff());
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
