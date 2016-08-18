#pragma once
#include <iostream>
#include <vector>

#include "parser/voca.h"
#include "parser/param.h"

namespace rnn{
namespace simple_model{
namespace tree{

struct Node{
    using word_type = rnn::wordrep::Word;
    using vec_type  = Param::vec_type;
    using value_type= Param::value_type;
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

void print_all_descents(Node const & node);

}//namespace rnn::simple_model::tree
}//namespace rnn::simple_model
}//namespace rnn

namespace rnn{
namespace simple_model{

struct UninializedLeafNodes{
    UninializedLeafNodes(std::vector<tree::Node> &&nodes) : val(std::move(nodes)) {}
    std::vector<tree::Node> val;
};

auto construct_nodes_with_reserve=[](auto const &words){
    using tree::Node;
    std::vector<Node> nodes;
    nodes.reserve(words.size()*2-1);
    for(auto const &word: words)
        nodes.push_back(Node{word});    
    return UninializedLeafNodes{std::move(nodes)};;
};

}//namespace rnn::simple_model
}//namespace rnn