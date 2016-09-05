#include <regex>

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
        assert(word2idx.getIndex(Word{voca.getWord(i).val}) == i);
    }
    for(size_t i=0; i<voca.size(); ++i){
        std::cout << voca.getWord(i).val <<std::endl;
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
    Param param = load_param("rnnparser.h5", "rnn.Parser#447cc3c8.18800", util::DataType::sp);    
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
    auto voca_vecs = load_voca_vecs<100>("data.h5", "1b.model", util::DataType::sp);
    std::cerr << voca_vecs.size() << " " << voca.size() <<" "<< voca_size <<std::endl;
    VocaIndexMap word2idx = voca.indexing();

    test_voca_index(voca, word2idx);
    auto sentence = u8"A symbol of British pound is £ .";
    auto idxs = word2idx.getIndex(sentence);
    auto word_block = voca_vecs.getWordVec(idxs);

    std::cerr << "Test:  -148.346 =="<<sum(word_block.span) << std::endl;
}

void test_read_voca_config(){
    Voca voca =load_voca(file_name, voca_name);
    auto voca_vecs = load_voca_vecs<100>(file_name, w2vmodel_name, w2vmodel_f_type);
    std::cerr << voca_vecs.size() << " " << voca.size() <<" "<< voca_size <<std::endl;
    VocaIndexMap word2idx = voca.indexing();

    test_voca_index(voca, word2idx);
    auto sentence = u8"The cat on a hat .";
    auto idxs = word2idx.getIndex(sentence);
    auto word_block = voca_vecs.getWordVec(idxs);

    std::cerr <<"Sum of word block: "<< sum(word_block.span) << std::endl;
}

void test_read_word2vec_output(){
    H5file file{file_name, hdf5::FileMode::read_exist};
    Voca voca =load_voca("word2vec.test.h5", "foo.word");
    auto voca_vecs = load_voca_vecs<200>("word2vec.test.h5", "foo.vec", util::DataType::sp); 
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

    std::cerr << "Test: 5.46774 =="<<sum(word_block.span) << std::endl;
}


void test_forward_backward(){
    using namespace rnn::simple_model;
    using namespace rnn::simple_model::detail;
    using value_type = rnn::simple_model::Param::value_type;
    
    VocaInfo rnn{file_name, voca_name, w2vmodel_name, w2vmodel_f_type};
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
    matloop_void(mul_sum_mat, ds_grad, grad.w_left.span, dParam.w_left.span);
    matloop_void(mul_sum_mat, ds_grad, grad.w_right.span, dParam.w_right.span);
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

    VocaInfo rnn{file_name, voca_name, w2vmodel_name, w2vmodel_f_type};
    auto param = load_param(rnn_param_store_name, rnn_param_name, param_f_type);
    auto get_grad = [&](auto sentence){
        auto nodes = rnn.initialize_tree(sentence);
        return get_greedy_gradient(param, nodes);
    };
    using rnn::config::n_minibatch;
    using rnn::simple_model::Param;
    timer.here_then_reset("Setup");
    Gradient grad_serial{};
    for(auto i=0; i<n_minibatch; ++i){
        grad_serial += get_grad(lines[i]); 
    }
    timer.here_then_reset("Serial reduce");

    auto beg=lines.cbegin();
    auto grad_parallel=parallel_reducer(beg, beg+n_minibatch, get_grad, Gradient{});
    timer.here_then_reset("Parallel reduce");
    print(grad_serial.param.bias.span[0]);
    print(grad_parallel.param.bias.span[0]);
    print('\n');
    print(grad_serial.param.w_left.span[0][0]);
    print(grad_parallel.param.w_left.span[0][0]);
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
    VocaInfo rnn{file_name, voca_name, w2vmodel_name, w2vmodel_f_type};
    // auto param = load_param(rnn_param_store_name, rnn_param_name, param_f_type);
    auto param = randomParam(0.1);
    timer.here_then_reset("Preparing data");

    auto get_grad = [&](auto sentence){
        auto nodes = rnn.initialize_tree(sentence);
        return get_greedy_gradient(param, nodes);
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
    auto grad_sum = parallel_reducer(lines.cbegin(), lines.cend(), get_grad, Gradient{});
    timer.here_then_reset("Back-propagation for whole testset");
    auto score = greedy_scoring_dataset(rnn, param, testset);
    timer.here_then_reset("Scoring testset");
    // auto param1{param};
    // param1.u_score.span *= rnn::type::float_t{2};
    // auto param2{param};
    // param2.u_score.span *= rnn::type::float_t{0.5};
    // auto score1 = scoring_dataset(rnn, param1, testset);
    auto score1 = greedy_scoring_dataset(rnn, param+dParam, testset);
    timer.here_then_reset("Scoring testset");
    auto score2 = greedy_scoring_dataset(rnn, param-dParam, testset);
    // auto score2 = scoring_dataset(rnn, param2, testset);
    timer.here_then_reset("Scoring testset");

    auto &grad =grad_sum.param;
    rnn_t::float_t ds_grad{};
    auto matloop_void=util::math::MatLoop_void<rnn::type::float_t, rnn::config::word_dim, rnn::config::word_dim>{};
    matloop_void(mul_sum_mat, ds_grad, grad.w_left.span, dParam.w_left.span);
    matloop_void(mul_sum_mat, ds_grad, grad.w_right.span, dParam.w_right.span);
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

void test_supervised_rnn_full_step(){
    using namespace rnn::simple_model;
    using namespace rnn::simple_model::detail;
    using namespace util;

    auto timer=Timer{};
    // auto testset=ParsedSentences{rnn::config::testset_name};
    // auto testset_orig=TokenizedSentences{rnn::config::testset_name};
    auto testset_parsed=ParsedSentences{"1b.s2010.testset.stanford"};
    auto testset_orig=TokenizedSentences{"1b.s2010.testset"};
    auto testset = SentencePairs{testset_parsed,testset_orig};
    auto &lines = testset.val;
    VocaInfo rnn{file_name, voca_name, w2vmodel_name, w2vmodel_f_type};
    // auto param = load_param(rnn_param_store_name, rnn_param_name, param_f_type);
    auto param = randomParam(0.1);
    timer.here_then_reset("Preparing data");

    auto get_grad=[&](auto const &sent_pair){
        return get_directed_grad(rnn, param, sent_pair);
    };

    auto dParam = randomParam(1.0);
    dParam.w_left.span  *= rnn_t::float_t{0.001};
    dParam.w_right.span *= rnn_t::float_t{0.001};
    dParam.bias.span    *= rnn_t::float_t{0.001};
    dParam.u_score.span *= rnn_t::float_t{0.001};
    auto grad_sum = parallel_reducer(lines.cbegin(), lines.cend(), get_grad, Gradient{});
    timer.here_then_reset("Back-propagation for whole testset");
    auto score = scoring_parsed_dataset(rnn, param, testset);
    timer.here_then_reset("Scoring testset");
    auto score1 = scoring_parsed_dataset(rnn, param+dParam, testset);
    timer.here_then_reset("Scoring testset");
    auto score2 = scoring_parsed_dataset(rnn, param-dParam, testset);
    // auto score2 = scoring_dataset(rnn, param2, testset);
    timer.here_then_reset("Scoring testset");

    auto &grad =grad_sum.param;
    rnn_t::float_t ds_grad{};
    auto matloop_void=util::math::MatLoop_void<rnn::type::float_t, rnn::config::word_dim, rnn::config::word_dim>{};
    matloop_void(mul_sum_mat, ds_grad, grad.w_left.span, dParam.w_left.span);
    matloop_void(mul_sum_mat, ds_grad, grad.w_right.span, dParam.w_right.span);
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

void test_SparseGrad(){

    auto timer=Timer{};
    VocaInfo rnn{file_name, voca_name, w2vmodel_name, w2vmodel_f_type};
    timer.here_then_reset("Preparing data");
    SparseGrad grad{};

    auto sentence = u8"this is a typical sentence";
    auto sentence2 = u8"this is another typical sentence";
    auto idxs = rnn.word2idx.getIndex(sentence);
    for(auto i:idxs) grad.val[i] += rnn.voca_vecs[i];
    auto idxs2 = rnn.word2idx.getIndex(sentence2);
    for(auto i:idxs2) grad.val[i] += rnn.voca_vecs[i];
    
    auto grad2{grad};
    grad2 += grad;
    print(sum(grad2.val[idxs[0]].span));
    print("+");
    print(sum(rnn.voca_vecs[idxs[0]]));
    auto v=rnn.voca_vecs[idxs[0]];
    v+=grad2.val[idxs[0]].span;
    print("==?");
    print(sum(rnn.voca_vecs[idxs[0]]));
    print("\n");
    
}
void test_backward_wordvec(){
    auto timer=Timer{};
    
    SentencePair sent_pair{"(This word)", "This word"};
    VocaInfo rnn{file_name, voca_name, w2vmodel_name, w2vmodel_f_type};
    VocaInfo rnn1{file_name, voca_name, w2vmodel_name, w2vmodel_f_type};

    auto param = randomParam(0.1);
    timer.here_then_reset("Preparing data");

    auto get_grad=[&](auto const &sent_pair){
        return get_directed_grad(rnn, param, sent_pair);
    };
    print(sent_pair.original);
    print("\n");
    auto grad = get_grad(sent_pair);
    auto delta=grad.words*0.001;
    for(auto const &x:delta.val){
        auto v=rnn1.voca_vecs[x.first];
        v+=x.second.span;
        print(rnn1.voca.getWord(x.first).val);
    }
    print("\n");
    auto delta_grad=0.0;
    for(auto const &x:delta.val){
        delta_grad+=dot(x.second.span, grad.words.val[x.first].span);
    }
    timer.here_then_reset("Back-propagation for whole testset");
    auto score = scoring_parsed_sentence(rnn, param, sent_pair);
    timer.here_then_reset("Scoring testset");
    auto score1 = scoring_parsed_sentence(rnn1, param, sent_pair);
    timer.here_then_reset("Scoring testset");
    
    print(score1-score);
    print(score1);
    print(score);
    print('\n');
    print(delta_grad);
    print('\n');

}

void test_fullstep_including_wordvec(){
    auto timer=Timer{};
    auto testset_parsed=ParsedSentences{"1b.s2010.testset.stanford"};
    auto testset_orig=TokenizedSentences{"1b.s2010.testset"};
    auto testset = SentencePairs{testset_parsed,testset_orig};
    auto &lines = testset.val;

    VocaInfo rnn{file_name, voca_name, w2vmodel_name, w2vmodel_f_type};
    VocaInfo rnn1{file_name, voca_name, w2vmodel_name, w2vmodel_f_type};

    auto param = randomParam(0.1);
    timer.here_then_reset("Preparing data");

    auto get_grad=[&](auto const &sent_pair){
        return get_directed_grad(rnn, param, sent_pair);
    };
    auto grad = parallel_reducer(lines.cbegin(), lines.cend(), get_grad, Gradient{});
    auto delta=grad.words*0.000001;
    for(auto const &x:delta.val){
        auto v=rnn1.voca_vecs[x.first];
        v+=x.second.span;
    }
    auto delta_grad=0.0;
    for(auto const &x:delta.val){
        delta_grad+=dot(x.second.span, grad.words.val[x.first].span);
    }
    timer.here_then_reset("Back-propagation for whole testset");
    auto score = scoring_parsed_dataset(rnn, param, testset);
    timer.here_then_reset("Scoring testset");
    auto score1 = scoring_parsed_dataset(rnn1, param, testset);
    timer.here_then_reset("Scoring testset");
    
    print(score1-score);
    print(score1);
    print(score);
    print('\n');
    print(delta_grad);
    print('\n');
}

}//namespace rnn::simple_model::test
}//namespace rnn::simple_model
}//namespace rnn
