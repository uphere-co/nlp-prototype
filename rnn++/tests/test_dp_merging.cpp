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

using util::print;
namespace rnn{
namespace simple_model{
namespace test{
void test_DPtable(){
    using namespace util;
    using namespace rnn::simple_model::detail;
    using rnn::simple_model::tree::Node;
    
    auto param = rnn::simple_model::randomParam(0.1);
    std::vector<Node> nodes(8, Node::blank_node());
    DPtable table{nodes};
    {auto &tmp=table.get(1,3);
    tmp.score=2.2;}
    assert(table.get(1,3).score==2.2);
    table.get(1,1).score=1.1;
    assert(table.get(1,1).score==1.1);
    auto n=nodes.size();
    for(decltype(n) len=1; len<n;++len){
        for(decltype(n) j=0; j<n-len;++j){
            table.search_best(param,j,j+len);
        }
    }
}
void test_dp_merging(){
    using namespace rnn::simple_model;
    using namespace rnn::config;
    using namespace rnn::simple_model::detail;
    using namespace util;
    
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

    DPtable table{nodes};
    table.compute(param);
    auto phrases = table.get_phrases();
    auto score_dp{0.0};
    for(auto phrase : phrases) score_dp += phrase->score;
    print(score_dp);
    print(":total score_dp.\n");
    print(table.score_sum(0,7));
    print(":score_sum.\n");
    auto root_node=table.get(0,n_words-1);
    print_all_descents(root_node);

    auto top_nodes = merge_leaf_nodes(param, nodes);
    auto merge_history = foward_path(param, top_nodes);
    auto score{0.0};
    for(auto const &node:nodes){
        if(node.is_leaf()) continue;
        print(node.name.val);
        print(node.score);
        print("\n");
        score+=node.score;
    } 
    print(score);
    print(":total score.\n");

    assert(score_dp>=score);
    
    timer.here_then_reset("Forward path");
}


void test_dp_merging_with_penalty(){
    using namespace rnn::simple_model;
    using namespace rnn::config;
    using namespace rnn::simple_model::detail;
    using namespace util;
    
    VocaInfo rnn{file_name, voca_name, w2vmodel_name, w2vmodel_f_type};
    // auto param = load_param(rnn_param_store_name, rnn_param_name, param_f_type);
    auto param = rnn::simple_model::randomParam(0.1);

    auto timer=Timer{};

    //auto sentence_test = u8"A symbol of British pound is £ .";
    auto sentence_test = u8"a symbol of british pound is £ .";
    auto sentence_parsed = u8"(((((a symbol) of) (british pound)) (is £)) .)";
    SentencePair sent_pair{sentence_parsed, sentence_test};
    
    DPtable table=dp_merging_with_penalty(rnn, param, 0.5, sent_pair);
    timer.here_then_reset("DP merging forward path with penalty terms.");

    auto phrases = table.get_phrases();
    print(table.score_sum(0,7));
    print(":score_sum including penalty.\n");
    auto score_dp{0.0};
    for(auto phrase : phrases) score_dp += phrase->score;
    print(score_dp);
    print(":total score_dp.\n");
    auto root_node=table.root_node();
    print_all_descents(root_node);

    auto initial_nodes = rnn.initialize_tree(sent_pair.original);
    auto &nodes = initial_nodes.val;
    auto top_nodes = merge_leaf_nodes(param, nodes);
    auto merge_history = foward_path(param, top_nodes);
    auto score{0.0};
    for(auto const &node:nodes){
        if(node.is_leaf()) continue;
        print(node.name.val);
        print(node.score);
        print("\n");
        score+=node.score;
    } 
    print(score);
    print(":total score_greedy.\n");
    
    timer.here_then_reset("Forward path");
}
}//namespace rnn::simple_model::test
}//namespace rnn::simple_model
}//namespace rnn
