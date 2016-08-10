#include <iostream>
#include <cassert>
#include <algorithm> //forward path
#include <chrono> //profiling

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
        std::cout << mesg << " Wall time: "<< std::chrono::duration<double, std::milli>(t_end-t_start).count() << std::endl;
    }
    void reset(){t_start = std::chrono::high_resolution_clock::now();}
    void here_then_reset(std::string mesg) {here(mesg); reset();}
    time_t t_start = std::chrono::high_resolution_clock::now();
};
int main(){
    try {
        test_init_rnn();
        //test_directed_merge();
        // test_read_voca();
        namespace rnn_model = rnn::simple_model;

        H5file param_storage{rnn_param_store_name, hdf5::FileMode::read_exist};
        auto param_raw0 = param_storage.getRawData<float>(rnn_param_name);
        std::vector<rnn_t::float_t> param_raw;
        for(auto x: param_raw0) param_raw.push_back(x);
        auto param = rnn_model::deserializeParam(param_raw);
        rnn_model::Parser parser{param};

        H5file file{file_name, hdf5::FileMode::read_exist};
        Voca voca{file.getRawData<rnn_t::char_t>(voca_name), voca_max_word_len};
        auto vocavec_tmp=file.getRawData<float>(w2vmodel_name);
        std::vector<rnn_t::float_t> vocavec;
        for(auto x: vocavec_tmp) vocavec.push_back(x);
        WordBlock voca_vecs{vocavec, word_dim};
        VocaIndexMap word2idx = voca.indexing();

        auto timer=Timer{};

        // auto raw_text = "A symbol\nof British pound is £."
        auto sentence = u8"A symbol of British pound is £ .";
        auto idxs = word2idx.getIndex(sentence);
        auto word_block = voca_vecs.getWordVec(idxs);
        std::cerr << sum(word_block.span) << std::endl;

        timer.here_then_reset("Setup");

        using namespace rnn::simple_model::tree;
        auto words = util::string::split(sentence);
        auto nodes = construct_nodes_with_reserve(words);

        std::cerr<<"Assign word2vecs\n";
        //TODO: following is inefficient. Make a separated class, LeafNode?? Or reuse word_block
        print(nodes.size());
        print('\n');
        for(decltype(nodes.size())i=0; i<nodes.size(); ++i){
            //TODO: Use VectorView instead.
            nodes[i].vec=rnn_model::Param::vec_type{word_block[i]};
            std::cerr<<i<<"-th word2vecs\n";
        }
        assert(words.size()==nodes.size());
        auto top_nodes = parser.merge_leaf_nodes(nodes);
        // std::vector<decltype(nodes.size())> merge_history={2, 1, 0, 0, 0, 1, 0};
        // parser.directed_merge(top_nodes, merge_history);
        auto merge_history = parser.foward_path(top_nodes);
        timer.here_then_reset("Forward path");
        
        for(auto x : merge_history)
            std::cerr<<x<< " ";
        std::cerr<<"\n";
        for(auto x : nodes)
            std::cerr<<x.score<<" "<<x.name.val<< '\n';
        auto idx=8;
        auto const &node=nodes[idx];
        assert(node.is_combined());
        // print_all_descents(node);
        rnn_model::Param::mat_type grad_W_left, grad_W_right;
        rnn_model::Param::vec_type grad_bias, grad_u_score;
        parser.backward_path(grad_W_left, grad_W_right, grad_bias,grad_u_score, node);
        
        auto dParam = rnn::simple_model::randomParam(0.001);
        // auto dParam = rnn::simple_model::Param{};
        // for(auto &x : dParam.w_left.span) x=0.001;
        // for(auto &x : dParam.w_right.span)x=0.001;
        // print(param.w_left.span[1][1]);
        // print('\n');
        // dParam.w_left.span[1][1]=param.w_left.span[1][1]*0.01;
        using namespace rnn::simple_model::compute;
        
        rnn_t::float_t ds_grad{};
        auto matloop_void=MatLoop_void<rnn_t::float_t, word_dim, word_dim>{};        
        matloop_void(mul_sum, ds_grad, grad_W_left.span, dParam.w_left.span);
        matloop_void(mul_sum, ds_grad, grad_W_right.span, dParam.w_right.span);
        ds_grad += dot(grad_bias.span, dParam.bias.span);
        ds_grad += dot(grad_u_score.span, dParam.u_score.span);

        timer.here_then_reset("Backward path");

        auto param1{param};
        auto param2{param};
        param1.w_left.span +=dParam.w_left.span;
        param1.w_right.span+=dParam.w_right.span;
        param2.w_left.span -=dParam.w_left.span;
        param2.w_right.span-=dParam.w_right.span;
        param1.bias.span +=dParam.bias.span;
        param2.bias.span -=dParam.bias.span;
        param1.u_score.span+=dParam.u_score.span;
        param2.u_score.span-=dParam.u_score.span;


        auto score0 = node.score;
        {
            rnn_model::Parser parser1{param1};
            rnn_model::Parser parser2{param2};
            auto nodes1 = construct_nodes_with_reserve(words);
            auto nodes2 = construct_nodes_with_reserve(words);
            for(decltype(nodes2.size())i=0; i<nodes2.size(); ++i){
                nodes1[i].vec=rnn_model::Param::vec_type{word_block[i]};
                nodes2[i].vec=rnn_model::Param::vec_type{word_block[i]};
            }
            auto top_nodes1 = parser1.merge_leaf_nodes(nodes1);
            auto top_nodes2 = parser2.merge_leaf_nodes(nodes2);
            parser1.foward_path(top_nodes1);
            parser2.foward_path(top_nodes2);
            print('\n');
            print((nodes1[idx].score-nodes2[idx].score)*0.5);
            print(nodes1[idx].score-score0);
            print('\n');
            print(ds_grad);
            print('\n');
            print(node.score);
            print('\n');
        }


        // ds_exact= ;
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
