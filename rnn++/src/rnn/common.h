#pragma once

namespace rnn{
namespace detail {

template<typename T>
struct Node {
    using prop_t = T;

    Node(T &&property) : prop{std::move(property)} {}
    Node(T const &property) : prop{property} {}
    Node(Node const &orig)
            : left{orig.left}, right{orig.right},
              prop{orig.prop} {}
    Node(Node &&orig)
            : left{orig.left}, right{orig.right},
              prop{std::move(orig.prop)} {}

    Node &operator=(Node const &orig) {
        prop = orig.prop;
        left = orig.left;
        right = orig.right;
        return *this;
    }

    static auto blank_node() { return Node{T{}}; }
    bool is_combined() const { return (left != nullptr) & (right != nullptr); }
    bool is_leaf() const { return (left == nullptr) & (right == nullptr); }

    Node const *left = nullptr;
    Node const *right = nullptr;
    T prop;
};

template<typename NODE>
struct InitializedNodes{
    InitializedNodes(std::vector<NODE> &&leaf_nodes_with_reserve)
            : val{std::move(leaf_nodes_with_reserve)}, nodes{val} {assert(val.size()*2-1==val.capacity());}
    std::vector<NODE> val;
    util::span_dyn<NODE> nodes;
};


template<typename RNN>
void backward_path_detail(typename RNN::param_t const &param,
                          typename RNN::param_t &grad_sum,
                          typename RNN::node_t  const &phrase,
                          typename RNN::mesg_t  mesg) {
    RNN::update_message(mesg, phrase);
    RNN::accum_param_gradient(grad_sum, mesg, phrase);

    if(phrase.left->is_combined()){
        backward_path_detail<RNN>(param, grad_sum, *phrase.left, param.left_message(mesg));
    } else if(phrase.left->is_leaf()){
        RNN::accum_left_wordvec_gradient(phrase, mesg, param);
    } else{
        assert(0);//it cannot happen on shape of tree constructed RNN.
    }
    if(phrase.right->is_combined()){
        backward_path_detail<RNN>(param, grad_sum, *phrase.right, param.right_message(mesg));
    } else if(phrase.right->is_leaf()){
        RNN::accum_right_wordvec_gradient(phrase, mesg, param);
    } else{
        assert(0);//it cannot happen on shape of tree constructed RNN.
    }
}

}//namespace rnn::detail
}//namespace rnn
