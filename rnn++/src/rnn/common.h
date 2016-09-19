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

}//namespace rnn::detail
}//namespace rnn