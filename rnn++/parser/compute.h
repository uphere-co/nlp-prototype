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
void backward_path(Param &grad, Param const &param,
                   node_type const &phrase);


class DPtable{
public:
    //using idx_t = rnn::type::idx_t;
    // using idx_t = std::ptrdiff_t;
    using idx_t = std::size_t;
    using node_t = rnn::simple_model::tree::Node;
    DPtable(std::vector<node_t> const &nodes)
    : n_words{nodes.size()}, raw(n_words*n_words, node_t::blank_node()) {
        for(decltype(n_words)i=0; i<n_words; ++i){
            get(i,i)=nodes[i];
        }
    }
    node_t& get(idx_t i, idx_t j) {return raw[i*n_words+j];}
    void search_best(Param const &param, idx_t i, idx_t j){
        auto& node=get(i,j);
        for(idx_t k=i; k<j; ++k){
            // auto print_elm=[](auto i, auto j){
            //     print("(");
            //     print(i);
            //     print(j);
            //     print(")");
            // };
            // print_elm(i,k);
            // print("and");
            // print_elm(k+1,j);
            // print("|");
            auto& left =get(i,k);
            auto& right=get(k+1,j);
            auto phrase=merge_node(param, left,right);
            if(phrase.score>node.score){
                node=phrase;
            }
        }
        // print("\n");
    }
    void compute(Param const &param){
        for(idx_t len=1; len<n_words;++len){
            for(idx_t left=0; left<n_words-len;++left){
                search_best(param,left,left+len);
            }
            // print('\n');
        }
    }
    std::vector<const node_t*> get_phrases(){
        std::vector<const node_t*> phrases;
        collect_phrases(&get(0,n_words-1), phrases);
        return phrases;

    }

private:
    void collect_phrases(const node_t* node, std::vector<const node_t*> &phrases) {
        if(node->is_leaf()) return;
        phrases.push_back(node);
        if(node->left != nullptr) collect_phrases(node->left, phrases);
        if(node->right!= nullptr) collect_phrases(node->right, phrases);
    }
    idx_t n_words;
    std::vector<node_t> raw;
};
}//namespace rnn::simple_model::detail
}//namespace rnn::simple_model
}//namespace rnn
