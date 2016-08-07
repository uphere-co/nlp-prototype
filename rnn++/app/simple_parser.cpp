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
        auto param_raw = param_storage.getRawData<rnn_t::float_t>(rnn_param_name);
        auto param = rnn_model::deserializeParam(param_raw);
        rnn_model::Parser parser{param};

        H5file file{file_name, hdf5::FileMode::read_exist};
        Voca voca{file.getRawData<rnn_t::char_t>(voca_name), voca_max_word_len};
        WordBlock voca_vecs{file.getRawData<rnn_t::float_t>(w2vmodel_name), word_dim};
        VocaIndexMap word2idx = voca.indexing();

        auto timer=Timer{};

        auto sentence = u8"A symbol of\tBritish pound is £ .";
        auto idxs = word2idx.getIndex(sentence);
        auto word_block = voca_vecs.getWordVec(idxs);
        std::cerr << sum(word_block.span) << std::endl;

        timer.here_then_reset("Setup");

        using namespace rnn::simple_model::tree;
        auto words = util::string::split(sentence);
        auto nodes = construct_nodes_with_reserve(words);
        //TODO: following is inefficient. Make a separated class, LeafNode?? Or reuse word_block
        for(decltype(nodes.size())i=0; i<nodes.size(); ++i)
            nodes[i].vec=rnn_model::Param::vec_type{word_block[i]};
        assert(words.size()==nodes.size());
        auto top_leaves = parser.merge_leaf_nodes(nodes);
        // std::vector<decltype(nodes.size())> merge_history={2, 1, 0, 0, 0, 1, 0};
        // parser.directed_merge(top_leaves, merge_history);
        auto merge_history = parser.foward_path(top_leaves);
        timer.here_then_reset("Forward path");

        for(auto x : merge_history)
            std::cerr<<x<< " ";
        std::cerr<<"\n";
        print_all_descents(nodes[13]);
    } catch (H5::Exception ex) {
        std::cerr << ex.getCDetailMsg() << std::endl;
    } catch (...) {
        std::cerr << "Unknown exception" << std::endl;
    }
    static_assert(std::is_nothrow_destructible<H5file>::value == true, "");
    static_assert(sizeof(WordBlock::idx_t) == 8, "");

    return 0;
}
