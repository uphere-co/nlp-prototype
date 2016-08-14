#pragma  once

#include "parser/param.h"
#include "parser/node.h"

#include "utils/math.h"
#include "utils/linear_algebra.h"
#include "utils/loop_gen.h"

namespace rnn{
namespace simple_model{
namespace detail{

auto weighted_sum=[](int64_t i,
                     auto const &w_left, auto const &w_right,
                     auto const &bias,
                     auto const &word_left, auto const &word_right) {
    using util::math::dot;
    return dot(w_left[i], word_left)+dot(w_right[i], word_right) + bias[i];
};
auto activation_fun=[](int64_t i, auto const &x) {
    return util::math::Fun<rnn::config::activation>(x[i]);
};
auto activation_dfun=[](int64_t i, auto const &x) {
    return util::math::Fun<rnn::config::activation_df>(x[i]);
};

auto update_mesg_common_part=[](int64_t i, auto &mesg, auto const &weighted_sum) {
    mesg[i]*=activation_dfun(i, weighted_sum);
};
auto update_mesg_finalize=[](int64_t i,int64_t j, auto &out, 
                             auto const &mesg, auto const &w)  {
    out[j]+=mesg[i]*w[i][j];
};
auto back_prop_grad_W=[](int64_t i,int64_t j, auto &grad, 
                         auto const &mesg, auto const &weighted_sum)  {
    grad[i][j]+=mesg[i]*weighted_sum[j];
};

auto mul_sum=[](int64_t i,int64_t j, auto &out, 
                auto const &a, auto const &b) {
    out+=a[i][j]*b[i][j];
};


//TODO: move these from this header to .cpp body. 
using value_type= Param::value_type;
using vec_type  = Param::vec_type;
using mat_type  = Param::mat_type;
using node_type = tree::Node;
//TODO:Move the following two to nameless namespace in .cpp file.
vec_type weighted_sum_word_pair(Param const &param, vec_type const &word_left,
                                vec_type const &word_right);
 
value_type scoring_node(Param const &param, node_type const &node);

void set_node_property(Param const &param,node_type &node);

node_type merge_node(Param const &param, node_type const &left, node_type const &right);

//top_node  : a node which has no parent
//leaf_node : a node which has no children
std::vector<node_type*> merge_leaf_nodes(Param const &param, std::vector<node_type> &leaves);
auto foward_path(Param const &param, std::vector<node_type*> &top_nodes) ->
    std::vector<decltype(top_nodes.size())>;

void directed_merge(Param const &param, std::vector<node_type*> &top_nodes,
                    std::vector<size_t> const &merge_history);

void backward_path(Param const &param,
                   mat_type &gradsum_left, mat_type &gradsum_right,
                   vec_type &gradsum_bias,  
                   node_type const &phrase, vec_type mesg);

void backward_path(Param const &param,
                   mat_type &gradsum_left, mat_type &gradsum_right,
                   vec_type &gradsum_bias, vec_type &gradsum_u_score,
                   node_type const &phrase);

// weighted_sum=W_left*word_left + W_right*word_right+bias
// s=u*h(g(f(weighted_sum)))
// dsdW_left = u cx .. h`.. g`... f`(weighted_sum) X word_left 
void backward_path(Param &grad, Param const &param,
                   node_type const &phrase);

}//namespace rnn::simple_model::detail
}//namespace rnn::simple_model
}//namespace rnn
