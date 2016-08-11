#include <iostream>
#include <cassert>
#include <algorithm> //forward path
#include <chrono> //profiling

#include "tbb/task_group.h"
#include "tbb/tbb.h"

#include "utils/hdf5.h"
#include "utils/math.h"
#include "utils/linear_algebra.h"
#include "parser/voca.h"
#include "parser/wordvec.h"
#include "parser/simple_model.h"
#include "parser/config.h"
#include "parser/node.h"
#include "parser/parser.h"

using namespace util::io;
using namespace util::math;
using namespace rnn::parser::wordrep;
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

struct Timer{
    using time_t= std::chrono::time_point<std::chrono::high_resolution_clock>;
    Timer(){}
    void here(std::string mesg) const {
        time_t t_end = std::chrono::high_resolution_clock::now();
        std::cerr << mesg << " Wall time: "<< std::chrono::duration<double, std::milli>(t_end-t_start).count() << std::endl;
    }
    void reset(){t_start = std::chrono::high_resolution_clock::now();}
    void here_then_reset(std::string mesg) {here(mesg); reset();}
    time_t t_start = std::chrono::high_resolution_clock::now();
};


rnn::parser::wordrep::Voca load_voca(){
    using namespace rnn::config;
    using namespace rnn::parser::wordrep;
    H5file file{file_name, hdf5::FileMode::read_exist};
    return Voca{file.getRawData<rnn_t::char_t>(voca_name), voca_max_word_len};
}

rnn::parser::wordrep::WordBlock load_voca_vecs(){
    using namespace rnn::config;
    using namespace rnn::parser::wordrep;
    H5file file{file_name, hdf5::FileMode::read_exist};
    auto vocavec_tmp=file.getRawData<float>(w2vmodel_name);
    std::vector<rnn::type::float_t> vocavec;
    for(auto x: vocavec_tmp) vocavec.push_back(x);
    return WordBlock{vocavec, word_dim};
}

rnn::simple_model::Param load_param(){
    using namespace rnn::config;
    H5file param_storage{rnn_param_store_name, hdf5::FileMode::read_exist};
    auto param_raw0 = param_storage.getRawData<float>(rnn_param_name);
    std::vector<rnn_t::float_t> param_raw;
    for(auto x: param_raw0) param_raw.push_back(x);
    return rnn::simple_model::deserializeParam(param_raw);
}
struct RNN{
    RNN() : voca{load_voca()}, word2idx{voca.indexing()},
            voca_vecs{load_voca_vecs()} {}

    std::vector<rnn::simple_model::tree::Node> initialize_tree(std::string sentence) const {
        auto idxs = word2idx.getIndex(sentence);
        auto word_block = voca_vecs.getWordVec(idxs);
        auto words = util::string::split(sentence);    
        auto nodes = rnn::simple_model::tree::construct_nodes_with_reserve(words);
        
        //TODO: following is inefficient. Make a separated class, LeafNode?? Or reuse word_block        
        for(decltype(nodes.size())i=0; i<nodes.size(); ++i){
            nodes[i].vec=rnn::simple_model::Param::vec_type{word_block[i]};
        }
        assert(words.size()==nodes.size());
        return nodes;
    }

    rnn::parser::wordrep::Voca voca;
    rnn::parser::wordrep::VocaIndexMap word2idx;
    rnn::parser::wordrep::WordBlock voca_vecs;
};

void test_forwad_backward(){
    using namespace rnn::simple_model::parser;
    using value_type = rnn::simple_model::Param::value_type;
    
    RNN rnn{};
    auto param = load_param();

    auto timer=Timer{};

    auto sentence = u8"A symbol of British pound is £ .";
    auto nodes = rnn.initialize_tree(sentence);
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
    using namespace rnn::simple_model::compute;
    auto matloop_void=MatLoop_void<value_type, rnn::config::word_dim, rnn::config::word_dim>{};
    auto dParam = rnn::simple_model::randomParam(0.001);        
    matloop_void(mul_sum, ds_grad, grad.w_left.span, dParam.w_left.span);
    matloop_void(mul_sum, ds_grad, grad.w_right.span, dParam.w_right.span);
    ds_grad += dot(grad.bias.span, dParam.bias.span);
    ds_grad += dot(grad.u_score.span, dParam.u_score.span);

    {
        auto param1{param};
        auto param2{param};
        param1.w_left.span +=dParam.w_left.span;
        param1.w_right.span+=dParam.w_right.span;
        param2.w_left.span -=dParam.w_left.span;
        param2.w_right.span-=dParam.w_right.span;
        param1.bias.span   +=dParam.bias.span;
        param2.bias.span   -=dParam.bias.span;
        param1.u_score.span+=dParam.u_score.span;
        param2.u_score.span-=dParam.u_score.span;

        auto words = util::string::split(sentence);  
        auto nodes1 = rnn.initialize_tree(sentence);
        auto nodes2 = rnn.initialize_tree(sentence);

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

using namespace rnn::simple_model::parser;
using value_type = rnn::simple_model::Param::value_type;
using vec_type = rnn::simple_model::Param::vec_type;
using mat_type = rnn::simple_model::Param::mat_type;

            
rnn::simple_model::Param get_gradient(rnn::simple_model::Param const &param,
                                      RNN const &rnn, 
                                      std::string sentence)  {
    // auto timer=Timer{};
    
    auto nodes = rnn.initialize_tree(sentence);
    auto n_words=nodes.size();
    // timer.here_then_reset("setup");
    auto top_nodes = merge_leaf_nodes(param, nodes);
    auto merge_history = foward_path(param, top_nodes);
    // timer.here_then_reset("forward path");
    rnn::simple_model::Param grad{};
    for(auto i=n_words; i<nodes.size(); ++i){
        auto const &node=nodes[i];
        assert(node.is_combined());
        // print_all_descents(node);
        backward_path(grad, param, node);
    }
    // timer.here_then_reset("backward path");
    print(grad.bias.span[0]);
    print('\n');
    return grad;
}

int main(){
    try {
        // test_init_rnn();
        // test_read_voca();
        // test_forwad_backward();
        // return 0;

        auto timer=Timer{};
        auto lines=util::string::readlines(rnn::config::trainset_name);
        timer.here_then_reset("Read trainset");

        RNN rnn{};
        auto param = load_param();
        auto get_grad = [&](auto sentence){return get_gradient(param, rnn, sentence);};
        tbb::parallel_for(0,static_cast<int>(lines.size()),1,  [&](int i){
            get_grad(lines[i]);
        });
        // //single-thread counter part:
        // std::for_each(lines.cbegin(), lines.cend(), [=](std::string sentence) {
        //     get_grad(sentence);
        // });

        
        timer.here_then_reset("Finish one iteration");
    } catch (H5::Exception &ex) {
        std::cerr << ex.getCDetailMsg() << std::endl;
    }catch (std::exception &e) {
        std::cerr<<"Got "<<e.what()<<std::endl;
    } catch (...) {
        std::cerr << "Unknown exception" << std::endl;
    }
    static_assert(std::is_nothrow_destructible<H5file>::value == true, "");
    static_assert(sizeof(WordBlock::idx_t) == 8, "");

    return 0;
}
