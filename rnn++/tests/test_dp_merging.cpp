//#include  <experimental/filesystem> // only works with g++. Needs libstdc++fs
#include <fstream>
#include <map>

#include "tests/test_dp_merging.h"

#include "parser/parser.h"
#include "parser/config.h"
#include "parser/compute.h"
#include "utils/print.h"
#include "utils/binary_tree.h"
#include "utils/profiling.h"

namespace rnn{
namespace simple_model{
namespace test{


class DPtable{
public:
    //using idx_t = rnn::type::idx_t;
    // using idx_t = std::ptrdiff_t;
    using idx_t = std::size_t;
    using node_t = rnn::simple_model::tree::Node;
    DPtable(idx_t n_words)
    : raw(n_words*n_words, node_t::blank_node()), n_words{n_words} {}
    auto best_score(idx_t i, idx_t j) const {return raw[i*n_words+j].score;}
    node_t& get(idx_t i, idx_t j) {return raw[i*n_words+j];}
private:
    std::vector<node_t> raw;
    idx_t n_words;
};

void test_DPtable(){
    DPtable table{10};
    assert(table.best_score(1,3)==0.0);
    {auto &tmp=table.get(1,3);
    tmp.score=2.2;}
    assert(table.best_score(1,3)==2.2);
    table.get(1,1).score=1.1;
    assert(table.best_score(1,1)==1.1);
}
void test_dp_merging(){
    using namespace rnn::simple_model;
    using namespace rnn::config;
    using namespace rnn::simple_model::detail;
    using namespace util;
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
    assert(nodes.capacity()==15);
    
    timer.here_then_reset("Setup word2vecs & nodes");

    auto top_nodes = merge_leaf_nodes(param, nodes);
    auto node=top_nodes.front();
    auto span_to_idx=[&nodes](auto node){return node-nodes.data();};
    auto left_span_idx =[=](auto node){return span_to_idx(get_left_span(node));};
    auto right_span_idx=[=](auto node){return span_to_idx(get_right_span(node));};
    // std::vector<std::ptrdiff_t> table_raw(n_words*n_words);
    // auto table = span_2d<std::ptrdiff_t,n_words,n_words>{table_raw};
    DPtable table{n_words};
    table.get(0,1)=merge_node(param, nodes[0],nodes[1]);
    print(table.get(0,1).score);
    print(":0,1 merged.\n");

    assert(left_span_idx(node)==0);
    assert(right_span_idx(node)==1);
    assert(right_span_idx(top_nodes.back())==7);
    for(auto node : top_nodes){        
        print(get_left_span(node)->name.val);
        print(get_right_span(node)->name.val);
        print("\n");
    }

    auto merge_history = foward_path(param, top_nodes);
    auto score{0.0};
    for(auto const &node:nodes){
        print(node.name.val);
        print(node.score);
        print("\n");
    } 
    
    timer.here_then_reset("Forward path");
}

}//namespace rnn::simple_model::test
}//namespace rnn::simple_model
}//namespace rnn
