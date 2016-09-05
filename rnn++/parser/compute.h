#pragma  once
#include <limits>

#include "parser/param.h"
#include "parser/node.h"

#include "utils/math.h"
#include "utils/linear_algebra.h"
#include "utils/loop_gen.h"
#include "utils/binary_tree.h"

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
void backward_path(Param &grad, Param const &param,
                   node_type const &phrase);


class DPtable{
public:
    //using idx_t = rnn::type::idx_t;
    // using idx_t = std::ptrdiff_t;
    using idx_t = std::size_t;
    using node_t = rnn::simple_model::tree::Node;
    using val_t= node_t::value_type;

    DPtable(std::vector<node_t> const &nodes);
    node_t& get(idx_t i, idx_t j);
    node_t& root_node();
    val_t&  score_sum(idx_t i, idx_t j);
    val_t&  penalty(idx_t i, idx_t j);
    void search_best(Param const &param, idx_t i, idx_t j);
    void search_best_with_penalty(Param const &param, idx_t i, idx_t j);
    void compute(Param const &param);
    void compute(Param const &param, val_t lambda, std::string parsed_sentence);
    void set_penalty(val_t lambda, std::string parsed_sentence);
    std::vector<const node_t*> get_phrases();

private:
    void collect_phrases(const node_t* node, std::vector<const node_t*> &phrases);
    idx_t n_words;
    std::vector<node_t> raw;
    std::vector<val_t> score_sums;
    std::vector<val_t> penalties;
};
}//namespace rnn::simple_model::detail
}//namespace rnn::simple_model
}//namespace rnn
