#pragma once

namespace util{

struct Node{
    std::string name;
    Node* left=nullptr;
    Node* right=nullptr;
    Node* parent=nullptr;
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
    if(!node_begin(*it)){assert(0);}

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

}//namespace util