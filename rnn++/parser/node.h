#pragma once
#include <iostream>
#include <vector>

#include "parser/voca.h"
#include "parser/simple_model.h"

namespace rnn{
namespace simple_model{
namespace tree{

struct Node{
    using word_type = rnn::wordrep::Word;
    using vec_type  = rnn::simple_model::Param::vec_type;
    using value_type= rnn::simple_model::Param::value_type;
    Node(word_type word) : name{word}{}
    // Node() : name{word_t{std::string{}}}{}
    // Node(Node &&node) = default;
    // Node(Node &node) = delete;
    // Node& operator=(Node const &node) = delete;

    static auto merged_name(Node const &left, Node const &right){
        return "("+left.name.val+" "+right.name.val+")";
    }
    void set_name() { name = word_type{merged_name(*left,*right)}; }
    bool is_combined() const {return (left!=nullptr)&(right!=nullptr);}

    word_type name;
    vec_type  vec_wsum{};
    vec_type  vec{};
    value_type score{};
    Node const *left=nullptr;
    Node const *right=nullptr;
};

void print_all_descents(Node const & node) {
    std::cerr<< node.score << " : "<< node.name.val << std::endl;
    if(node.left != nullptr) print_all_descents(*node.left);
    if(node.right!= nullptr) print_all_descents(*node.right);
}

struct UninializedLeafNodes{
    UninializedLeafNodes(std::vector<Node> &&nodes) : val(std::move(nodes)) {}
    std::vector<Node> val;
};
auto construct_nodes_with_reserve=[](auto const &words){
    std::vector<Node> nodes;
    nodes.reserve(words.size()*2-1);
    for(auto const &word: words)
        nodes.push_back(Node{word});    
    return UninializedLeafNodes{std::move(nodes)};;
};

}//namespace rnn::simple_model::tree
}//namespace rnn::simple_model
}//namespace rnn
