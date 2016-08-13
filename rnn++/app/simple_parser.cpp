#include <iostream>
#include <cassert>

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

namespace rnn_t = rnn::type;

int main(){
    try {
        test_init_rnn();
        test_read_voca();
        test_forwad_backward();
        test_parallel_reduce();
        return 0;

        auto timer=Timer{};
        auto lines=util::string::readlines(rnn::config::trainset_name);
        timer.here_then_reset("Read trainset");

        using namespace rnn::simple_model;
        TrainData rnn{};
        auto param = load_param();
        auto get_grad = [&](auto sentence){
            auto nodes = rnn.initialize_tree(sentence);
            return get_gradient(param, nodes);
        };
        using rnn::simple_model::Param;
        for(auto it=lines.cbegin();it <lines.cend(); it+= rnn::config::n_minibatch){
            auto beg=it;
            auto end=beg+n_minibatch;
            auto grad_sum = parallel_reducer(beg, end, get_grad, rnn::simple_model::Param{});
            // rnn::simple_model::Param grad_serial{};
            // for(auto i=beg; i!=end; ++i){
            //     grad_serial += get_grad(*i); 
            // }      
        }
        // tbb::parallel_for(0UL,lines.size(),1UL,  [&](auto i){
        //     get_grad(lines[i]);
        // });
        // //single-thread counter part:
        // std::for_each(lines.cbegin(), lines.cend(), [=](std::string sentence) {
        //     get_grad(sentence);
        // });

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
