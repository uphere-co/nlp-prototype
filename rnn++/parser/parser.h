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
    vec_type weighted_sum(vec_type const &word_left,
                          vec_type const &word_right) const {
        using rnn::simple_model::compute::WeightedSum;
        //TODO: change interface to remove .span?
        return vecloop_vec(WeightedSum<Param::value_type,Param::dim>{},//WeightedSum<Param::value_type,Param::dim>{},
                         param.w_left.span, param.w_right.span, param.bias.span,
                         word_left.span,word_right.span);
    };
    value_type scoring_node(node_type const &node) const {
        return util::math::dot(param.u_score.span, node.vec.span);
    }
    void set_node_property(node_type &node) const {
        using rnn::simple_model::compute::ActivationFun;
        node.vec_wsum  = weighted_sum(node.left->vec, node.right->vec);
        node.vec  = vecloop_vec(ActivationFun<Param::value_type,Param::dim>{},
                              node.vec_wsum.span);
        node.score= scoring_node(node);
        node.set_name();
    }
    node_type merge_node(node_type const &left, node_type const &right) const {
        auto new_node = node_type{node_type::word_type{std::string{}}};
        new_node.left = &left;
        new_node.right= &right;
        set_node_property(new_node);
        return new_node;
    }

    //top_node  : a node which has no parent
    //leaf_node : a node which has no children
    std::vector<node_type*> merge_leaf_nodes(std::vector<node_type> &leaves) const {
        auto n_leaf = leaves.size();
        auto last_leaf = leaves.cend()-1;
        for(auto it=leaves.cbegin(); it!=last_leaf;)
            leaves.push_back( merge_node(*it,*++it ));

        std::vector<node_type*> top_node;
        for(auto it=leaves.data()+n_leaf; it!=leaves.data()+leaves.size(); ++it)
            top_node.push_back(it);
        return top_node;
    }
    auto foward_path(std::vector<node_type*> &top_nodes) const {
        std::vector<decltype(top_nodes.size())> merge_history;
        while(top_nodes.size()){
            auto it_max=std::max_element(top_nodes.cbegin(), top_nodes.cend(),
                 [](auto const x, auto const y){
                     return x->score < y->score;
                 });
            auto i_max = it_max - top_nodes.cbegin();
            merge_history.push_back(i_max);
            if(it_max!=top_nodes.cbegin()){
                auto it_left = *(it_max-1);
                it_left->right=*it_max;
                set_node_property(*it_left);
            }
            if(it_max!=top_nodes.cend()-1){
                auto it_right= *(it_max+1);
                it_right->left=*it_max;
                set_node_property(*it_right);
            }
            std::copy(it_max+1,top_nodes.cend(),
                      top_nodes.begin()+i_max);
            top_nodes.pop_back();
        }
        return merge_history;
    }
    void backward_path_W(mat_type &gradsum_left, mat_type &gradsum_right,
                         node_type const &phrase,
                         vec_type mesg) const {
        //TODO:Fix bug. copy construct is incorrect.
        //mat_type grad{gradsum};
        constexpr auto dim = Param::dim;
        using val_t =Param::value_type; 
        vecloop_void(compute::UpdateMesg<val_t, dim>{}, mesg.span, phrase.vec_wsum.span);
        matloop_void(compute::BackPropGrad<val_t, dim, dim>{}, 
                     gradsum_left.span, mesg.span, phrase.left->vec.span);                             
        matloop_void(compute::BackPropGrad<val_t, dim, dim>{}, 
                     gradsum_right.span, mesg.span, phrase.right->vec.span);
        if(phrase.left->is_combined()){
            Param::vec_type left_mesg;
            matloop_void(compute::AccumMesg<val_t,dim,dim>{}, left_mesg.span, mesg.span, param.w_left.span);
            backward_path_W(gradsum_left, gradsum_right, *phrase.left, left_mesg);
        }
        if(phrase.right->is_combined()){
            Param::vec_type right_mesg;
            matloop_void(compute::AccumMesg<val_t,dim,dim>{}, right_mesg.span, mesg.span, param.w_right.span);
            backward_path_W(gradsum_left, gradsum_right, *phrase.right, right_mesg);
        }
    }
    void backward_path_W(mat_type &gradsum_left, mat_type &gradsum_right,
                         node_type const &phrase) const {
        auto mesg{param.u_score};
        backward_path_W(gradsum_left, gradsum_right, phrase, mesg);
    }

    void directed_merge(std::vector<node_type*> &top_nodes,
                        std::vector<size_t> const &merge_history) const {
        for(auto idx_max : merge_history){
            auto it_new  = top_nodes[idx_max];
            if(idx_max!=0){
                auto it_left = top_nodes[idx_max-1];
                it_left->right=it_new;
                set_node_property(*it_left);
            }
            if(idx_max!=top_nodes.size()-1){
                auto it_right= top_nodes[idx_max+1];
                it_right->left=it_new;
                set_node_property(*it_right);
            }
            std::copy(top_nodes.cbegin()+idx_max+1,top_nodes.cend(),
                      top_nodes.begin()+idx_max);
            top_nodes.pop_back();
        }
    }
    Param param;
};

}//namespace rnn::simple_model
}//namespace rnn
