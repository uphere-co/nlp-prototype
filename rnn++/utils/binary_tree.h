#pragma once
#include <vector>
#include <algorithm>
#include <cassert>
#include <iostream>

namespace util{

struct Node{
    std::string name;
    Node* left=nullptr;
    Node* right=nullptr;
    Node* parent=nullptr;
};

auto find_top_node_it=[](auto const& nodes){
    return std::find_if(nodes.cbegin(),nodes.cend(),
                        [](auto x){return x.parent==nullptr;});
};
auto find_top_node_ptr=[](auto const& nodes_ptr){
    return *std::find_if(nodes_ptr.cbegin(),nodes_ptr.cend(),
                         [](auto x){return x->parent==nullptr;});
};

template<typename NODE>
std::vector<NODE> deserialize_binary_tree(std::string tree_str){
    //define some helper functions in function body because it is implementation details.
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

    auto n_composites = std::count_if(tree_str.cbegin(), tree_str.cend(), node_begin);
    std::vector<NODE> nodes(n_composites*2+1);
    auto it=tree_str.cbegin();
    if(!node_begin(*it)){return nodes;}

    NODE *current_leaf = &nodes[0];
    NODE *current_node = &nodes[n_composites+1];
    NODE *new_node = current_node;
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
    if(current_node != nullptr){
        nodes.resize(0);
        return nodes;
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
auto get_left_span_idx = [](auto const &node, auto const &nodes){
    return get_left_span(node)-nodes.data();
};
auto get_right_span_idx = [](auto const &node, auto const &nodes){
    return get_right_span(node)-nodes.data();
};
auto get_span = [](auto const &node){
    return std::make_pair(get_left_span(node), get_right_span(node));
};
auto get_span_hash = [](auto const &node, auto const &nodes){
    auto left  = get_left_span_idx(node, nodes);
    auto right = get_right_span_idx(node, nodes);
    return left*nodes.size() + right;
};
auto get_span_hashes = [](auto const &nodes){
    std::vector<std::size_t> span_hashes;
    for(auto const &node: nodes){
        if(!is_composite(node)) continue;
        auto hash = get_span_hash(node, nodes);
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

}//namespace util
