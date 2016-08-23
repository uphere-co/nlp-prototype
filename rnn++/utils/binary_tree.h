#pragma once
#include <vector>
#include <algorithm>
#include <cassert>
#include <iostream>
#include <memory>

namespace util{

struct Node{
    std::string name;
    Node* left=nullptr;
    Node* right=nullptr;
    Node* parent=nullptr;
};

auto find_top_node_it=[](auto const& nodes){
    return std::find_if(nodes.cbegin(),nodes.cend(),
                        [](auto const &x){return x->parent==nullptr;});
};
auto find_top_node_ptr=[](auto const& nodes_ptr){
    return *std::find_if(nodes_ptr.cbegin(),nodes_ptr.cend(),
                         [](auto const &x){return x->parent==nullptr;});
};

template<typename NODE>
std::vector<std::unique_ptr<Node>> deserialize_binary_tree(std::string tree_str){
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
    std::vector<std::unique_ptr<Node>> nodes;
    for(decltype(n_composites) i=0; i<n_composites*2+1; ++i) 
        nodes.push_back(std::make_unique<Node>());
    auto it=tree_str.cbegin();
    if(!node_begin(*it)){return nodes;}
    auto it_current_leaf = nodes.begin();
    NODE *current_leaf = it_current_leaf->get();
    auto it_new_node = nodes.begin()+n_composites+1;
    NODE *new_node = it_new_node->get();
    NODE *current_node = new_node;
    while(++it!=tree_str.cend()){
        if(node_begin(*it)){
            ++it_new_node;
            new_node = it_new_node->get();
            fill_child_node(current_node, new_node);
            current_node = new_node;
        } else if (node_end(*it)){
            current_node = current_node->parent;
        } else if (node_sep(*it)){
        } else{
            it = std::find_if_not(it, tree_str.cend(), node_name_field)-1;
            fill_child_node(current_node, current_leaf);
            ++it_current_leaf;
            current_leaf = it_current_leaf->get();
        }
    }
    if(current_node != nullptr){
        nodes.resize(0);
        return nodes;
    }
    assert(current_node == nullptr);
    return nodes;
}
auto is_composite = [](auto const &node){return node->left && node->right;};

auto get_left_span = [](auto const &node){
    auto span=node->left; 
    while(span->left) span=span->left;
    return span;
};
auto get_right_span = [](auto const &node){
    auto span=node->right; 
    while(span->right) span=span->right;
    return span;
};
auto get_span = [](auto const &node){
    return std::make_pair(get_left_span(node), get_right_span(node));
};

auto get_index=[](auto ptr, auto const& ptrs){
    auto idx = std::find_if(ptrs.cbegin(),ptrs.cend(),[&ptr](auto const&p){
                    return p.get()==ptr;
                }) - ptrs.cbegin();
    return idx;
};
auto get_left_span_idx = [](auto const &node, auto const &nodes){
    auto span=get_left_span(node);    
    return get_index(span, nodes);
};
auto get_right_span_idx = [](auto const &node, auto const &nodes){
    auto span=get_right_span(node);    
    return get_index(span, nodes);
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


auto is_leaf_node = [](auto node){
    return node->left==nullptr && node->right==nullptr;
};
auto is_dangling_node = [](auto node){
    return node->parent==nullptr&&is_leaf_node(node);
};
auto detach_leaf_node = [](auto child){
    auto parent = child->parent;
    assert(parent!=nullptr);
    if(parent->left==child) parent->left=nullptr;
    else if (parent->right==child) parent->right=nullptr;
    else {assert(0);}
    child->parent=nullptr;
};
auto is_mergable=[](auto const &node){
    return is_leaf_node(node->left)&&is_leaf_node(node->right);
};



template<typename T>
void nodes_height_helper(T const* node, std::vector<std::unique_ptr<T>> const &nodes, 
                         std::vector<int64_t> &heights){
    if(is_leaf_node(node)) {
        heights[get_index(node,nodes)]=0;
        return;
    } 
    nodes_height_helper(node->left, nodes, heights);    
    nodes_height_helper(node->right, nodes, heights);
    auto left=heights[get_index(node->left,nodes)];
    auto right=heights[get_index(node->right,nodes)];
    heights[get_index(node,nodes)] = std::max(left,right)+1;
    return ;
}
auto nodes_height=[](auto const &nodes){
    auto it_top_node = find_top_node_it(nodes);
    std::vector<int64_t> heights(nodes.size());
    nodes_height_helper(it_top_node->get(), nodes, heights);
    return heights;
};

auto merge_node=[](std::unique_ptr<Node> &node, auto &nodes){
    auto i_left = get_index(node->left, nodes);
    auto i_right= get_index(node->right, nodes);
    auto i_self = get_index(node.get(), nodes);
    node->left=nullptr;
    node->right=nullptr;
    assert(i_right-i_left==1);
    auto beg = nodes.begin();
    auto end = nodes.end();
    std::iter_swap(beg+i_left,beg+i_self);
    std::move(beg+i_right+1, beg+i_self, beg+i_right);
    std::move(beg+i_self+1, end, beg+i_self-1);
    nodes.pop_back();
    nodes.pop_back();
};

auto reconstruct_merge_history=[](auto &&nodes){
    std::vector<decltype(nodes.size())> merge_history;
    auto n_words = (nodes.size()+1)/2;
    while(n_words>1){
        auto beg = nodes.begin()+n_words;
        auto mergable_node=std::find_if(beg, nodes.end(), is_mergable);
        merge_history.push_back(get_index(mergable_node->get()->left, nodes));
        --n_words;
        merge_node(*mergable_node,nodes);
    }
    return merge_history;
};
auto get_merge_history = [](auto const &parsed_sentence){        
    auto tree = deserialize_binary_tree<Node>(parsed_sentence);
    auto merge_history=reconstruct_merge_history(std::move(tree));
    return merge_history;
};
}//namespace util
