#include "tests/test_tree_spans.h"

#include <vector>
#include <unordered_set>    
#include <string>
#include <algorithm>
#include <cassert>

#include "utils/binary_tree.h"
#include "utils/print.h"

using namespace util;

namespace{
auto assert_node=[](auto const &parent, auto const &left, auto const &right){
    assert(parent->left == left.get());
    assert(parent->right == right.get());
    assert(parent.get() == left->parent);
    assert(parent.get() == right->parent);
};
auto assert_nodes_span=[](auto &nodes, auto i_node, auto i_left_span, auto i_right_span){
    assert(get_span(nodes[i_node])==std::make_pair(nodes[i_left_span].get(),
                                                   nodes[i_right_span].get()));
};
}//nameless namespace

namespace test{

void test_simple(){
    {
        auto nodes = deserialize_binary_tree<Node>("(a b)");
        assert(nodes.size()==3);
        assert_node(nodes[2], nodes[0], nodes[1]);
    }
    {
        auto nodes = deserialize_binary_tree<Node>("((aaaa bb) (ccc dd))");
        assert(nodes.size()==7);
        assert_node(nodes[4], nodes[5], nodes[6]);
        assert_node(nodes[5], nodes[0], nodes[1]);
        assert_node(nodes[6], nodes[2], nodes[3]);
        assert_nodes_span(nodes, 4, 0, 3);
        assert_nodes_span(nodes, 5, 0, 1);
    }
    {
        auto nodes = deserialize_binary_tree<Node>("(((c d) b) ((c (c d)) d))");
        assert(nodes.size()==13);
        for(int i=0; i<7; ++i) assert(!is_composite(nodes[i]));
        for(int i=7; i<13; ++i) assert(is_composite(nodes[i]));
        assert_node(nodes[7], nodes[8], nodes[10]);
        assert_node(nodes[8], nodes[9], nodes[2]);
        assert_node(nodes[9], nodes[0], nodes[1]);
        assert_node(nodes[10], nodes[11], nodes[6]);
        assert_node(nodes[11], nodes[3], nodes[12]);
        assert_node(nodes[12], nodes[4], nodes[5]);
        assert_nodes_span(nodes, 7, 0, 6);
        assert_nodes_span(nodes, 8, 0, 2);
        assert_nodes_span(nodes, 9, 0, 1);
        assert_nodes_span(nodes, 10, 3, 6);
        assert_nodes_span(nodes, 11, 3, 5);
        assert_nodes_span(nodes, 12, 4, 5);
        auto spans=get_span_hashes(nodes);
        assert(spans.size()==6);
        assert(spans[0]==6);
        assert(spans[1]==2);
        assert(spans[2]==1);
        assert(spans[3]==45);
        assert(spans[4]==44);
        assert(spans[5]==57);
    }
    {
        auto tree1 = deserialize_binary_tree<Node>("(((c d) b) ((c (c d)) d))");
        auto tree2 = deserialize_binary_tree<Node>("((c (d b)) ((c (c d)) d))");
        auto tree3 = deserialize_binary_tree<Node>("((c (d b)) (((c c) d) d))");
        auto spans1=get_span_hashes(tree1);
        auto spans2=get_span_hashes(tree2);
        auto spans3=get_span_hashes(tree3);
        assert(span_diffs(spans1, spans2)==1);
        assert(span_diffs(spans1, spans3)==2);
    }
    
}

void test_reconstruct_merge_history(){    
}

}//namespace test