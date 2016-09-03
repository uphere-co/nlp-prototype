#pragma once
#include <iostream>
#include <vector>

#include "parser/voca.h"
#include "parser/param.h"

namespace rnn{
namespace simple_model{
namespace tree{

struct Node{
    using word_type = rnn::wordrep::IndexedWord;
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
    static auto blank_node(){
        return Node{word_type::blank_word()};
    }
    void set_name() { 
        name = word_type{merged_name(*left,*right), 
                         std::numeric_limits<word_type::idx_t>::max()}; 
    }
    bool is_combined() const {return (left!=nullptr)&(right!=nullptr);}
    bool is_leaf() const {return (left==nullptr)&(right==nullptr);}

    word_type name;
    vec_type  vec_wsum{};
    vec_type  vec{};
    vec_type  vec_update{};
    value_type score{};
    Node const *left=nullptr;
    Node const *right=nullptr;
    Node const *parent=nullptr;
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

auto construct_nodes_with_reserve=[](auto const &words, auto const &idxs){
    using tree::Node;
    std::vector<Node> nodes;
    nodes.reserve(words.size()*2-1);
    auto n=words.size();
    for(decltype(n)i=0; i!=n; ++i){
        Node::word_type word{words[i],idxs[i]};
        nodes.push_back(Node{word});    
    }
    return UninializedLeafNodes{std::move(nodes)};;
};

}//namespace rnn::simple_model
}//namespace rnn
