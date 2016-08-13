#include "tests/test_simple_model.h"

#include "utils/hdf5.h"
#include "utils/math.h"
#include "utils/linear_algebra.h"
#include "utils/print.h"
#include "utils/profiling.h"
#include "utils/parallel.h"

#include "parser/parser.h"

using namespace util;
using namespace util::io;
using namespace util::math;
using namespace rnn::wordrep;
using namespace rnn::config;

namespace rnn_t = rnn::type;

namespace{
void test_voca_index(Voca const &voca, VocaIndexMap const &word2idx){
    for(size_t i=0; i<voca.size(); ++i){
        if(i>100) break;
        assert(word2idx.getIndex(voca.getWord(i)) == i);
    }
    for(size_t i=0; i<voca.size(); ++i){
        std::cout << voca.getWord(i) <<std::endl;
    }
}
}//nameless namespace


void test_init_rnn(){
    /*
    -148.346
    ((((A (symbol (of British))) pound) is) (£ .))
    [2, 1, 0, 0, 0, 1, 0]
    Scores     :  [ 0.98579389  0.98854256  0.99147892  0.97225118  0.97717702  0.94854963
      0.9967401 ]
    Block shape:  (15, 100)
    (100, 200)
    W=3.248616 b=-50.581345 u=-0.190589
    u⋅W_left⋅b=0.675059
    u⋅W_right⋅b=1.5551
    */

    namespace rnn_model = rnn::simple_model;
    H5file param_storage{rnn_param_store_name, hdf5::FileMode::read_exist};
    auto param_raw = param_storage.getRawData<rnn_t::float_t>(rnn_param_name);
    // auto span = gsl::span<rnn_t::float_t>{param_raw};
    rnn_model::Param param = rnn_model::deserializeParam(param_raw);
    std::cerr << "Test:   3.248616=="<< sum(param.w_left.span)+sum(param.w_right.span) << std::endl;
    std::cerr << "Test: -50.581345=="<< sum(param.bias.span) << std::endl;
    std::cerr << "Test:  -0.190589=="<< sum(param.u_score.span) << std::endl;
    std::cerr << "Test:   0.675059=="<<dotdot(param.u_score, param.w_left, param.bias) << std::endl;
    std::cerr << "Test:   1.5551  =="<< dotdot(param.u_score, param.w_right, param.bias) << std::endl;
    // auto w_flat = gsl::as_span(w.data(), w.extent(0)*w.extent(1));
}
void test_read_voca(){
    H5file file{file_name, hdf5::FileMode::read_exist};
    Voca voca{file.getRawData<rnn_t::char_t>(voca_name), voca_max_word_len};
    WordBlock voca_vecs{file.getRawData<rnn_t::float_t>(w2vmodel_name), word_dim};
    std::cerr << voca_vecs.size() << " " << voca_size <<std::endl;
    VocaIndexMap word2idx = voca.indexing();

    test_voca_index(voca, word2idx);
    auto sentence = u8"A symbol of\tBritish pound is £ .";
    auto idxs = word2idx.getIndex(sentence);
    auto word_block = voca_vecs.getWordVec(idxs);

    std::cerr << sum(word_block.span) << std::endl;
}


void test_forwad_backward(){
    using namespace rnn::simple_model;
    using namespace rnn::simple_model::detail;
    using value_type = rnn::simple_model::Param::value_type;
    
    TrainData rnn{};
    auto param = load_param();

    auto timer=Timer{};

    auto sentence_test = u8"A symbol of British pound is £ .";
    auto initial_nodes = rnn.initialize_tree(sentence_test);
    auto &nodes = initial_nodes.val;
    auto n_words=nodes.size();
    assert(n_words==8);
    
    timer.here_then_reset("Setup word2vecs & nodes");

    auto top_nodes = merge_leaf_nodes(param, nodes);
    auto merge_history = foward_path(param, top_nodes);
    timer.here_then_reset("Forward path");

    rnn::simple_model::Param grad{};
    for(auto i=n_words; i<nodes.size(); ++i){
        auto const &node=nodes[i];
        assert(node.is_combined());
        // print_all_descents(node);
        backward_path(grad, param, node);
    }       

    timer.here_then_reset("Backward path");

    for(auto x : nodes)
        std::cerr<<x.score<<" "<<x.name.val<< '\n';

    rnn_t::float_t ds_grad{};
    auto matloop_void=MatLoop_void<value_type, rnn::config::word_dim, rnn::config::word_dim>{};
    auto dParam = rnn::simple_model::randomParam(0.001);        
    matloop_void(mul_sum, ds_grad, grad.w_left.span, dParam.w_left.span);
    matloop_void(mul_sum, ds_grad, grad.w_right.span, dParam.w_right.span);
    ds_grad += dot(grad.bias.span, dParam.bias.span);
    ds_grad += dot(grad.u_score.span, dParam.u_score.span);

    //h= delta
    //score1 = f(x+h) = f(x) + grad_x(f)*h + O(h^2)
    //score2 = f(x-h) = f(x) - grad_x(f)*h + O(h^2)
    //score1,2 : from forward path.
    //score1-score2 = 2*grad_x(f)*h +O(h^3)
    {
        // auto adder = [](auto const &x,auto const &y){return x+y;};
        // auto param1 = adder(param, dParam);
        auto param1 = std::plus<rnn::simple_model::Param>{}(param, dParam);
        // auto param1 = param + dParam;
        auto param2 = param - dParam;
        // auto param1{param};
        // auto param2{param};
        // param1 += dParam;
        // param2 -= dParam;
        // param1.w_left.span +=dParam.w_left.span;
        // param1.w_right.span+=dParam.w_right.span;
        // param2.w_left.span -=dParam.w_left.span;
        // param2.w_right.span-=dParam.w_right.span;
        // param1.bias.span   +=dParam.bias.span;
        // param2.bias.span   -=dParam.bias.span;
        // param1.u_score.span+=dParam.u_score.span;
        // param2.u_score.span-=dParam.u_score.span;

        auto words = util::string::split(sentence_test); 
        auto initial_nodes1 = rnn.initialize_tree(sentence_test);
        auto &nodes1 = initial_nodes1.val;
        auto initial_nodes2 = rnn.initialize_tree(sentence_test);
        auto &nodes2 = initial_nodes2.val; 
        // auto nodes1 = rnn.initialize_tree(sentence_test);
        // auto nodes2 = rnn.initialize_tree(sentence_test);

        auto top_nodes1 = merge_leaf_nodes(param1, nodes1);
        auto top_nodes2 = merge_leaf_nodes(param2, nodes2);
        foward_path(param1, top_nodes1);
        foward_path(param2, top_nodes2);
        auto score1{0.0}, score2{0.0};
        for(auto const & node:nodes1) score1+= node.score;
        for(auto const & node:nodes2) score2+= node.score;
        print(0.5*(score1-score2));
        print(score1);
        print('\n');
        print(ds_grad);
    }
}


void test_parallel_reduce(){
    using namespace rnn::simple_model;
    auto timer=Timer{};
    auto lines=util::string::readlines(rnn::config::trainset_name);
    timer.here_then_reset("Read trainset");

    TrainData rnn{};
    auto param = load_param();
    auto get_grad = [&](auto sentence){
        auto nodes = rnn.initialize_tree(sentence);
        return get_gradient(param, nodes);
    };
    using rnn::config::n_minibatch;
    using rnn::simple_model::Param;
    timer.here_then_reset("Setup");
    Param grad_serial{};
    for(auto i=0; i<n_minibatch; ++i){
        grad_serial += get_grad(lines[i]); 
    }
    timer.here_then_reset("Serial reduce");

    auto beg=lines.cbegin();
    auto grad_parallel=parallel_reducer(beg, beg+n_minibatch, get_grad, Param{});
    timer.here_then_reset("Parallel reduce");
    print(grad_serial.bias.span[0]);
    print(grad_parallel.bias.span[0]);
    print('\n');
    print(grad_serial.w_left.span[0][0]);
    print(grad_parallel.w_left.span[0][0]);
    print('\n');
}
