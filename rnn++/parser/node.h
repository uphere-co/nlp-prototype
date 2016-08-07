#pragma once
#include <iostream>
#include <vector>

#include "parser/voca.h"

struct Node{
    using word_t = rnn::parser::wordrep::Word;
    Node(word_t word) : name{word}{}
    // Node() : name{word_t{std::string{}}}{}
    // Node(Node &&node) = default;
    // Node(Node &node) = delete;
    // Node& operator=(Node const &node) = delete;

    static auto merged_name(Node const &left, Node const &right){
        return "("+left.name.val+" "+right.name.val+")";
    }
    void update_name() { name = word_t{merged_name(*left,*right)}; }

    word_t name;
    Node const *left=nullptr;
    Node const *right=nullptr;
};

void print_all_descents(Node const & node) {
    std::cerr<< node.name.val << std::endl;
    if(node.left != nullptr) print_all_descents(*node.left);
    if(node.right!= nullptr) print_all_descents(*node.right);
}

Node merge_node(Node const &left, Node const &right){
    auto new_node = Node{Node::word_t{std::string{}}};
    new_node.left = &left;
    new_node.right= &right;
    new_node.update_name();
    return new_node;
}

auto construct_nodes_with_reserve(std::vector<std::string> const &words){
    std::vector<Node> nodes{};
    nodes.reserve(words.size()*2-1);
    for(auto const &word: words)
        nodes.push_back(Node{word});
    return nodes;
}

auto merge_leaf_nodes(std::vector<Node> &nodes){
    auto n_leaf = nodes.size();
    auto last_leaf = nodes.cend()-1;
    for(auto it=nodes.cbegin(); it!=last_leaf;)
        nodes.push_back( merge_node(*it,*++it ));

    std::vector<Node*> phrase_pool;
    for(auto it=nodes.data()+n_leaf; it!=nodes.data()+nodes.size(); ++it)
        phrase_pool.push_back(it);
    return phrase_pool;
}

auto directed_merge(std::vector<Node*> &phrase_pool,
                    std::vector<size_t> const &merge_history){
    for(auto idx_max : merge_history){
        auto it_new  = phrase_pool[idx_max];
        if(idx_max!=0){
            auto it_left = phrase_pool[idx_max-1];
            it_left->right=it_new;
            it_left->update_name();
        }
        if(idx_max!=phrase_pool.size()-1){
            auto it_right= phrase_pool[idx_max+1];
            it_right->left=it_new;
            it_right->update_name();
        }
        std::copy(phrase_pool.cbegin()+idx_max+1,phrase_pool.cend(),
                  phrase_pool.begin()+idx_max);
        phrase_pool.pop_back();
    }
}
