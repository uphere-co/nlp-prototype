#include <iostream>
#include <cassert>
#include <cmath>

#include "utils/hdf5.h"
#include "utils/math.h"
#include "utils/linear_algebra.h"
#include "utils/print.h"
#include "utils/profiling.h"
#include "utils/parallel.h"

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
    try {
        // test_init_rnn();
        // test_read_voca();
        // test_read_word2vec_output();
        // test_forwad_backward();
        // test_parallel_reduce();
        // test_rnn_full_step();
        // return 0;

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
            auto optimizer = optimizer::LBFGSoptimizer{word_dim*(2*word_dim+2), param,rnn,testset, beg,end};
            optimizer.update();
            // auto optimizer = optimizer::GradientDescent{0.0001};
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
