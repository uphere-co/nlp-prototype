#pragma once

#include "parser/simple_model.h"
#include "parser/node.h"
#include "utils/linear_algebra.h"

namespace rnn{
namespace simple_model{
class Parser{
public:
    using value_type= Param::value_type;
    using vec_type  = Param::vec_type;
    using mat_type  = Param::mat_type;
    using node_type = tree::Node;

    Parser(Param const &param) : param{param}{}


    //TODO:Move the following two to nameless namespace in .cpp file.
    vec_type merge_to_phrase(vec_type const &word_left,
                             vec_type const &word_right){
        return rnn::simple_model::compute::merge_to_phrase(
                    param.w_left, param.w_right, param.bias,
                    //TODO: remove .span?
                    word_left.span,word_right.span);
    };
    value_type scoring_node(node_type const &node) const {
        return util::math::dot(param.u_score.span, node.vec.span);
    }
    void set_node_property(node_type &node){
        node.vec  = merge_to_phrase(node.left->vec, node.right->vec);
        node.score= scoring_node(node);
        node.set_name();
        // std::cerr<< "Merge!! : "<< new_node.score << std::endl;
    }
    node_type merge_node(node_type const &left, node_type const &right){
        auto new_node = node_type{node_type::word_type{std::string{}}};
        new_node.left = &left;
        new_node.right= &right;
        set_node_property(new_node);
        return new_node;
    }

    std::vector<node_type*> merge_leaf_nodes(std::vector<node_type> &leaves){
        auto n_leaf = leaves.size();
        auto last_leaf = leaves.cend()-1;
        for(auto it=leaves.cbegin(); it!=last_leaf;)
            leaves.push_back( merge_node(*it,*++it ));

        std::vector<node_type*> phrase_pool;
        for(auto it=leaves.data()+n_leaf; it!=leaves.data()+leaves.size(); ++it)
            phrase_pool.push_back(it);
        return phrase_pool;
    }

    void foward_path(std::vector<node_type*> &top_leaves){
        while(top_leaves.size()!=1){
            auto it_max=std::max_element(top_leaves.cbegin(), top_leaves.cend(),
                 [](auto const x, auto const y){
                     return x->score < y->score;
                 });
            auto i_max = it_max - top_leaves.cbegin();
            if(it_max!=top_leaves.cbegin()){
                auto it_left = *(it_max-1);
                it_left->right=*it_max;
                set_node_property(*it_left);
            }
            if(it_max!=top_leaves.cend()-1){
                auto it_right= *(it_max+1);
                it_right->left=*it_max;
                set_node_property(*it_right);
            }
            std::copy(it_max+1,top_leaves.cend(),
                      top_leaves.begin()+i_max);
            top_leaves.pop_back();
        }
    }

    void directed_merge(std::vector<node_type*> &top_leaves,
                        std::vector<size_t> const &merge_history){
        for(auto idx_max : merge_history){
            auto it_new  = top_leaves[idx_max];
            if(idx_max!=0){
                auto it_left = top_leaves[idx_max-1];
                it_left->right=it_new;
                set_node_property(*it_left);
            }
            if(idx_max!=top_leaves.size()-1){
                auto it_right= top_leaves[idx_max+1];
                it_right->left=it_new;
                set_node_property(*it_right);
            }
            std::copy(top_leaves.cbegin()+idx_max+1,top_leaves.cend(),
                      top_leaves.begin()+idx_max);
            top_leaves.pop_back();
        }
    }
    Param param;
};

}//namespace rnn::simple_model
}//namespace rnn
