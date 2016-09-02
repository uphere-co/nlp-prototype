#pragma  once

#include "parser/param.h"
#include "parser/node.h"

#include "utils/math.h"
#include "utils/linear_algebra.h"
#include "utils/loop_gen.h"

namespace rnn{
namespace simple_model{
namespace detail{
using node_type = tree::Node;

Param::value_type scoring_node(Param const &param, node_type const &node);

void set_node_property(Param const &param,node_type &node);

node_type merge_node(Param const &param, node_type const &left, node_type const &right);

//top_node  : a node which has no parent
//leaf_node : a node which has no children
std::vector<node_type*> merge_leaf_nodes(Param const &param, std::vector<node_type> &leaves);
auto foward_path(Param const &param, std::vector<node_type*> top_nodes) ->
    std::vector<decltype(top_nodes.size())>;

void directed_merge(Param const &param, std::vector<node_type*> &top_nodes,
                    std::vector<size_t> const &merge_history);

// weighted_sum=W_left*word_left + W_right*word_right+bias
// s=u*h(g(f(weighted_sum)))
// dsdW_left = u cx .. h`.. g`... f`(weighted_sum) X word_left 
void backward_path_for_param(Param &grad, Param const &param,
                   node_type const &phrase);

}//namespace rnn::simple_model::detail
}//namespace rnn::simple_model
}//namespace rnn
