#include <regex>

#include "tests/test_simple_model.h"

#include "utils/hdf5.h"
#include "utils/binary_tree.h"
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
using namespace rnn::simple_model;

namespace rnn_t = rnn::type;

namespace{
void test_voca_index(Voca const &voca, VocaIndexMap const &word2idx){
    for(size_t i=0; i<voca.size(); ++i){
        if(i>100) break;
        // if(word2idx.getIndex(voca.getWord(i)) != i){
        //     print(i);
        //     print(voca.getWord(i).val);
        //     print(word2idx.getIndex(voca.getWord(i)));
        //     print(":conflict!\n");
        // }
        assert(word2idx.getIndex(voca.getWord(i)) == i);
    }
    for(size_t i=0; i<voca.size(); ++i){
        std::cout << voca.getWord(i) <<std::endl;
    }
}
}//nameless namespace


namespace rnn{
namespace simple_model{
namespace test{

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

    // auto span = gsl::span<rnn_t::float_t>{param_raw};
    Param param = load_param(rnn_param_store_name, rnn_param_name, param_f_type);    
    std::cerr << "Test:   3.248616=="<< sum(param.w_left.span)+sum(param.w_right.span) << std::endl;
    std::cerr << "Test: -50.581345=="<< sum(param.bias.span) << std::endl;
    std::cerr << "Test:  -0.190589=="<< sum(param.u_score.span) << std::endl;
    std::cerr << "Test:   0.675059=="<<dotdot(param.u_score, param.w_left, param.bias) << std::endl;
    std::cerr << "Test:   1.5551  =="<< dotdot(param.u_score, param.w_right, param.bias) << std::endl;
    // auto w_flat = gsl::as_span(w.data(), w.extent(0)*w.extent(1));
}
void test_read_voca(){
    H5file file{file_name, hdf5::FileMode::read_exist};
    Voca voca =load_voca("data.h5", "1b.model.voca");
    WordBlock voca_vecs = load_voca_vecs("data.h5", "1b.model", 100, util::DataType::sp);
    std::cerr << voca_vecs.size() << " " << voca.size() <<" "<< voca_size <<std::endl;
    VocaIndexMap word2idx = voca.indexing();

    test_voca_index(voca, word2idx);
    auto sentence = u8"A symbol of British pound is £ .";
    auto idxs = word2idx.getIndex(sentence);
    auto word_block = voca_vecs.getWordVec(idxs);

    std::cerr << "Test:  -148.346 =="<<sum(word_block.span) << std::endl;
}

void test_read_word2vec_output(){
    H5file file{file_name, hdf5::FileMode::read_exist};
    Voca voca =load_voca("word2vec.h5", "foo.word");
    WordBlock voca_vecs = load_voca_vecs("word2vec.h5", "foo.vec", 200, util::DataType::dp); 
    std::cerr << voca_vecs.size() << " " << voca.size() <<std::endl;
    VocaIndexMap word2idx = voca.indexing();

    test_voca_index(voca, word2idx);
    auto sentence = u8"this is a typical sentence";
    auto idxs = word2idx.getIndex(sentence);
    for(auto i:idxs) print(i);
    print(": idxs\n");
    print(voca_vecs.size());
    print(": voca_vec.size()\n");
    auto word_block = voca_vecs.getWordVec(idxs);

    std::cerr << "Test: -82.7994 =="<<sum(word_block.span) << std::endl;
}


void test_forwad_backward(){
    using namespace rnn::simple_model;
    using namespace rnn::simple_model::detail;
    using value_type = rnn::simple_model::Param::value_type;
    
    VocaInfo rnn{file_name, voca_name, w2vmodel_name, word_dim, w2vmodel_f_type};
    // auto param = load_param(rnn_param_store_name, rnn_param_name, param_f_type);
    auto param = rnn::simple_model::randomParam(0.1);

    auto timer=Timer{};

    //auto sentence_test = u8"A symbol of British pound is £ .";
    auto sentence_test = u8"a symbol of british pound is £ .";
    auto initial_nodes = rnn.initialize_tree(sentence_test);
    auto &nodes = initial_nodes.val;
    auto n_words=nodes.size();
    assert(n_words==8);
    
    timer.here_then_reset("Setup word2vecs & nodes");

    auto top_nodes = merge_leaf_nodes(param, nodes);
    auto merge_history = foward_path(param, top_nodes);
    auto score{0.0};
    for(auto const & node:nodes) score+= node.score;
    timer.here_then_reset("Forward path");

    rnn::simple_model::Param grad{};
    for(auto i=n_words; i<nodes.size(); ++i){
        auto const &node=nodes[i];
        assert(node.is_combined());
        // print_all_descents(node);
        backward_path(grad, param, node);
    }       

    timer.here_then_reset("Backward path");

    for(auto x : merge_history)
        std::cerr<<x<<" ";
    std::cerr<<'\n';
    for(auto x : nodes)
        std::cerr<<x.score<<" "<<x.name.val<< '\n';

    rnn_t::float_t ds_grad{};
    auto matloop_void=MatLoop_void<value_type, rnn::config::word_dim, rnn::config::word_dim>{};
    auto dParam = rnn::simple_model::randomParam(0.00001);
    dParam.bias.span    *= rnn_t::float_t{0.0000};
    dParam.u_score.span *= rnn_t::float_t{0.000};
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
        print(score1-score);
        print(score-score2);
        print(score);
        print('\n');
        print(ds_grad);
    }
}


void test_parallel_reduce(){
    using namespace rnn::simple_model;
    auto timer=Timer{};
    auto lines=util::string::readlines(rnn::config::testset_name);
    timer.here_then_reset("Read testset");

    VocaInfo rnn{file_name, voca_name, w2vmodel_name, word_dim, w2vmodel_f_type};
    auto param = load_param(rnn_param_store_name, rnn_param_name, param_f_type);
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

void test_rnn_full_step(){
    using namespace rnn::simple_model;
    using namespace rnn::simple_model::detail;

    auto timer=Timer{};
    auto testset=TokenizedSentences{rnn::config::testset_name};
    auto &lines = testset.val;
    // auto tmp=util::string::readlines(rnn::config::testset_name);
    // std::vector<std::string> lines={ u8"A symbol of British pound is £ .", u8"A symbol of British pound is £ ."};
    timer.here_then_reset("Read trainset");
    VocaInfo rnn{file_name, voca_name, w2vmodel_name, word_dim, w2vmodel_f_type};
    // auto param = load_param(rnn_param_store_name, rnn_param_name, param_f_type);
    auto param = randomParam(0.1);
    timer.here_then_reset("Preparing data");

    auto get_grad = [&](auto sentence){
        auto nodes = rnn.initialize_tree(sentence);
        return get_gradient(param, nodes);
    };
    auto dParam = randomParam(1.0);
    // dParam.w_left.span  *= 0.00001;
    // dParam.w_right.span *= 0.00001;
    // // dParam.w_left.span  *= 0.0000;
    // // dParam.w_right.span *= 0.0000;
    // dParam.bias.span    *= 0.0001;
    dParam.w_left.span  *= rnn_t::float_t{0.000001};
    dParam.w_right.span *= rnn_t::float_t{0.000001};
    dParam.bias.span    *= rnn_t::float_t{0.0000};
    dParam.u_score.span *= rnn_t::float_t{0.000};
    print(norm_L1(param.w_left.span));
    print(norm_L1(param.w_right.span));
    print(norm_L1(param.bias.span));
    print(norm_L1(param.u_score.span));
    print(": L1-norm\n");
    auto grad_sum = parallel_reducer(lines.cbegin(), lines.cend(), get_grad, Param{});
    timer.here_then_reset("Back-propagation for whole testset");
    auto score = scoring_dataset(rnn, param, testset);
    timer.here_then_reset("Scoring testset");
    // auto param1{param};
    // param1.u_score.span *= rnn::type::float_t{2};
    // auto param2{param};
    // param2.u_score.span *= rnn::type::float_t{0.5};
    // auto score1 = scoring_dataset(rnn, param1, testset);
    auto score1 = scoring_dataset(rnn, param+dParam, testset);
    timer.here_then_reset("Scoring testset");
    auto score2 = scoring_dataset(rnn, param-dParam, testset);
    // auto score2 = scoring_dataset(rnn, param2, testset);
    timer.here_then_reset("Scoring testset");

    auto &grad =grad_sum;
    rnn_t::float_t ds_grad{};
    auto matloop_void=util::math::MatLoop_void<rnn::type::float_t, rnn::config::word_dim, rnn::config::word_dim>{};
    matloop_void(mul_sum, ds_grad, grad.w_left.span, dParam.w_left.span);
    matloop_void(mul_sum, ds_grad, grad.w_right.span, dParam.w_right.span);
    ds_grad += dot(grad.bias.span, dParam.bias.span);
    ds_grad += dot(grad.u_score.span, dParam.u_score.span);

    print(0.5*(score1-score2));
    print(score1-score);
    print(score-score2);
    print(score);
    print('\n');
    print(ds_grad);
    print('\n');
}

auto to_original_sentence=[](auto sentence){
    std::regex bracket("[(|)]");
    return std::regex_replace(sentence, bracket, "");
};

void test_supervised_rnn_full_step(){
    using namespace rnn::simple_model;
    using namespace rnn::simple_model::detail;
    using namespace util;

    auto timer=Timer{};
    // auto testset=ParsedSentences{rnn::config::testset_name};
    // auto testset_orig=TokenizedSentences{rnn::config::testset_name};
    auto testset=ParsedSentences{"1b.testset.sample.stanford"};
    auto testset_orig=TokenizedSentences{"1b.testset.sample"};
    auto &lines = testset.val;
    VocaInfo rnn{file_name, voca_name, w2vmodel_name, word_dim, w2vmodel_f_type};
    // auto param = load_param(rnn_param_store_name, rnn_param_name, param_f_type);
    auto param = randomParam(0.1);
    timer.here_then_reset("Preparing data");

    auto get_merge_history = [](auto parsed_sentence){        
        auto tree = deserialize_binary_tree<Node>(parsed_sentence);
        auto merge_history=reconstruct_merge_history(std::move(tree));
        return merge_history;
    };
    auto get_directed_grad = [&get_merge_history](VocaInfo const &rnn, Param const &param, 
                                auto const &parsed_sentence,
                                auto const &original_sentence){
        auto merge_history = get_merge_history(parsed_sentence);
        auto nodes = rnn.initialize_tree(original_sentence);
        auto& all_nodes = nodes.val; 
        auto n_words=all_nodes.size();
        auto top_nodes = merge_leaf_nodes(param, all_nodes);
        directed_merge(param, top_nodes,merge_history);
        Param grad{};
        for(auto i=n_words; i<all_nodes.size(); ++i){
            auto const &node=all_nodes[i];
            assert(node.is_combined());
            backward_path(grad, param, node);
        }
        return grad;
    };
    auto get_grad=[&](auto const &parsed_sentence){
        // auto sentence2 = to_original_sentence(parsed_sentence);
        auto original_sentence = testset_orig.val[&parsed_sentence-testset.val.data()];
        return get_directed_grad(rnn, param, parsed_sentence, original_sentence);
    };
    auto scoring_parsed_sentence = [&get_merge_history](VocaInfo const &rnn, Param const &param,
                                       auto const &parsed_sentence,
                                       auto const &original_sentence){
        auto merge_history = get_merge_history(parsed_sentence);
        auto nodes = rnn.initialize_tree(original_sentence);
        assert(nodes.val.size()==merge_history.size()+1);
        auto& all_nodes = nodes.val; 
        auto n_words=all_nodes.size();
        auto top_nodes = merge_leaf_nodes(param, all_nodes);
        directed_merge(param, top_nodes,merge_history);
        Param::value_type score{};
        for(auto i=n_words; i<all_nodes.size(); ++i){
            score+= all_nodes[i].score;
        }
        return score;
    };
    auto scoring_parsed_dataset=[&](VocaInfo const &rnn, Param const &param, 
                                            ParsedSentences const &dataset){
        using rnn::type::float_t;
        auto &lines = dataset.val;
        auto get_score=[&](auto const &parsed_sentence){
            auto original_sentence = testset_orig.val[&parsed_sentence-testset.val.data()];
            return scoring_parsed_sentence(rnn, param, parsed_sentence, original_sentence);
        };
        auto score_accum = util::parallel_reducer(lines.cbegin(), lines.cend(), get_score, float_t{});
        return score_accum;
    };
    auto dParam = randomParam(1.0);
    dParam.w_left.span  *= rnn_t::float_t{0.001};
    dParam.w_right.span *= rnn_t::float_t{0.001};
    dParam.bias.span    *= rnn_t::float_t{0.001};
    dParam.u_score.span *= rnn_t::float_t{0.001};
    auto grad_sum = parallel_reducer(lines.cbegin(), lines.cend(), get_grad, Param{});
    timer.here_then_reset("Back-propagation for whole testset");
    auto score = scoring_parsed_dataset(rnn, param, testset);
    timer.here_then_reset("Scoring testset");
    auto score1 = scoring_parsed_dataset(rnn, param+dParam, testset);
    timer.here_then_reset("Scoring testset");
    auto score2 = scoring_parsed_dataset(rnn, param-dParam, testset);
    // auto score2 = scoring_dataset(rnn, param2, testset);
    timer.here_then_reset("Scoring testset");

    auto &grad =grad_sum;
    rnn_t::float_t ds_grad{};
    auto matloop_void=util::math::MatLoop_void<rnn::type::float_t, rnn::config::word_dim, rnn::config::word_dim>{};
    matloop_void(mul_sum, ds_grad, grad.w_left.span, dParam.w_left.span);
    matloop_void(mul_sum, ds_grad, grad.w_right.span, dParam.w_right.span);
    ds_grad += dot(grad.bias.span, dParam.bias.span);
    ds_grad += dot(grad.u_score.span, dParam.u_score.span);

    print(0.5*(score1-score2));
    print(score1-score);
    print(score-score2);
    print(score);
    print('\n');
    print(ds_grad);
    print('\n');
}

}//namespace rnn::simple_model::test
}//namespace rnn::simple_model
}//namespace rnn
