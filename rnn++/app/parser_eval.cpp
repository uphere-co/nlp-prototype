#include <vector>
#include <unordered_set>    
#include <string>
#include <algorithm>
#include <cassert>

struct Node{
    std::string name;
    Node* left=nullptr;
    Node* right=nullptr;
    Node* parent=nullptr;
};

auto node_begin = [](char x){return x=='(';};
auto node_end   = [](char x){return x==')';};
auto node_sep   = [](char x){return x==' ';};
auto node_name_field   = [](char x){return x!='(' && x!=')' && x!=' ';};

auto fill_child_node=[](auto parent, auto child){
    child->parent=parent;
    if(parent->left ==nullptr) parent->left=child;
    else if(parent->right ==nullptr) parent->right=child;
    else {assert(0);}
};

std::vector<Node> deserialize_binary_tree(std::string tree_str){
    auto n_composites = std::count_if(tree_str.cbegin(), tree_str.cend(), node_begin);
    std::vector<Node> nodes(n_composites*2+1);
    auto it=tree_str.cbegin();
    if(!node_begin(*it)){assert(0);}

    Node *current_leaf = &nodes[0];
    Node *current_node = &nodes[n_composites+1];
    Node *new_node = current_node;
    while(++it!=tree_str.cend()){
        if(node_begin(*it)){
            ++new_node;
            fill_child_node(current_node, new_node);
            current_node = new_node;
        } else if (node_end(*it)){
            current_node = current_node->parent;
        } else if (node_sep(*it)){
        } else{
            it = std::find_if_not(it, tree_str.cend(), node_name_field)-1;
            fill_child_node(current_node, current_leaf++);
        }
    }
    assert(current_node == nullptr);
    return nodes;
}
auto is_composite = [](auto const &node){return node.left && node.right;};

auto get_left_span = [](auto const &node){
    auto span=node.left; 
    while(span->left) span=span->left;
    return span;
};
auto get_right_span = [](auto const &node){
    auto span=node.right; 
    while(span->right) span=span->right;
    return span;
};
auto get_span = [](auto const &node){
    return std::make_pair(get_left_span(node), get_right_span(node));
};

auto get_span_hashes = [](auto const &nodes){
    const auto n_nodes = nodes.size();
    std::vector<std::size_t> span_hashes;
    for(auto const &node: nodes){
        if(!is_composite(node)) continue;
        auto left = get_left_span(node)-nodes.data();
        auto right = get_right_span(node)-nodes.data();
        auto hash = left*n_nodes + right;
        span_hashes.push_back(hash);
    }
    return span_hashes;
};

auto span_diffs= [](auto const &spans1, auto const &spans2){
    std::vector<std::size_t> diff;
    std::set_difference(spans1.cbegin(),spans1.cend(), 
                        spans2.cbegin(),spans2.cend(),std::back_inserter(diff));
    return diff.size();
};

auto assert_node=[](auto const &parent, auto const &left, auto const &right){
    assert(parent.left == &left);
    assert(parent.right == &right);
    assert(&parent == left.parent);
    assert(&parent == right.parent);
};
auto assert_nodes_span=[](auto &nodes, auto i_node, auto i_left_span, auto i_right_span){
    assert(get_span(nodes[i_node])==std::make_pair(&nodes[i_left_span],&nodes[i_right_span]));
};

void test_simple(){
    {
        auto nodes = deserialize_binary_tree("(a b)");
        assert(nodes.size()==3);
        assert_node(nodes[2], nodes[0], nodes[1]);
    }
    {
        auto nodes = deserialize_binary_tree("((aaaa bb) (ccc dd))");
        assert(nodes.size()==7);
        assert_node(nodes[4], nodes[5], nodes[6]);
        assert_node(nodes[5], nodes[0], nodes[1]);
        assert_node(nodes[6], nodes[2], nodes[3]);
        assert_nodes_span(nodes, 4, 0, 3);
        assert_nodes_span(nodes, 5, 0, 1);
    }
    {
        auto nodes = deserialize_binary_tree("(((c d) b) ((c (c d)) d))");
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
        // assert(spans.find(6)!=spans.cend());
        // assert(spans.find(2)!=spans.cend());
        // assert(spans.find(1)!=spans.cend());
        // assert(spans.find(45)!=spans.cend());
        // assert(spans.find(44)!=spans.cend());
        // assert(spans.find(57)!=spans.cend());
    }
    {
        auto tree1 = deserialize_binary_tree("(((c d) b) ((c (c d)) d))");
        auto tree2 = deserialize_binary_tree("((c (d b)) ((c (c d)) d))");
        auto spans1=get_span_hashes(tree1);
        auto spans2=get_span_hashes(tree2);
        assert(span_diffs(spans1, spans2)==1);
    }
    
}
int main(){
    test_simple();
    return 0;
}