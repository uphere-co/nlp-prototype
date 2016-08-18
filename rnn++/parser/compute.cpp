#include "parser/compute.h"
#include "utils/print.h"

namespace rnn{
namespace simple_model{
namespace detail{
//Cannot do string comparison at compile time.
// constexpr auto activation_factory(const char name[]){
//     if(name=="tanh") return Activation::tanh;
//     else if(name=="sig") return Activation::sig;
// }



//TODO:Move the following two to nameless namespace in .cpp file.
vec_type weighted_sum_word_pair(Param const &param, vec_type const &word_left,
                                vec_type const &word_right) {
    //TODO: change interface to remove .span?
    auto vecloop_vec = util::math::VecLoop_vec<Param::value_type,Param::dim>{};
    return vecloop_vec(weighted_sum, param.w_left.span, param.w_right.span, 
                        param.bias.span, word_left.span,word_right.span);
}
 
value_type scoring_node(Param const &param, node_type const &node) {
    using namespace util::math;    
    return dot(param.u_score.span, node.vec.span) / norm_L2(param.u_score.span);
}

void set_node_property(Param const &param,node_type &node) {
    auto vecloop_vec = util::math::VecLoop_vec<Param::value_type,Param::dim>{};
    node.vec_wsum  = weighted_sum_word_pair(param, node.left->vec, node.right->vec);
    node.vec  = vecloop_vec(activation_fun, node.vec_wsum.span);
    node.score= scoring_node(param, node);
    node.set_name();
}

node_type merge_node(Param const &param, node_type const &left, node_type const &right)  {
    auto new_node = node_type{node_type::word_type{std::string{}}};
    new_node.left = &left;
    new_node.right= &right;
    set_node_property(param, new_node);
    return new_node;
}

//top_node  : a node which has no parent
//leaf_node : a node which has no children
std::vector<node_type*> merge_leaf_nodes(Param const &param, std::vector<node_type> &leaves)  {
    auto n_leaf = leaves.size();
    auto last_leaf = leaves.cend()-1;
    for(auto it=leaves.cbegin(); it!=last_leaf;)
        leaves.push_back( merge_node(param, *it,*++it ));

    std::vector<node_type*> top_node;
    for(auto it=leaves.data()+n_leaf; it!=leaves.data()+leaves.size(); ++it)
        top_node.push_back(it);
    return top_node;
}
auto foward_path(Param const &param, std::vector<node_type*> top_nodes) ->
    std::vector<decltype(top_nodes.size())> {
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
            set_node_property(param, *it_left);
        }
        if(it_max!=top_nodes.cend()-1){
            auto it_right= *(it_max+1);
            it_right->left=*it_max;
            set_node_property(param, *it_right);
        }
        std::copy(it_max+1,top_nodes.cend(),
                  top_nodes.begin()+i_max);
        top_nodes.pop_back();
    }
    return merge_history;
}

void directed_merge(Param const &param, std::vector<node_type*> &top_nodes,
                    std::vector<size_t> const &merge_history) {
    for(auto idx_max : merge_history){
        auto it_new  = top_nodes[idx_max];
        if(idx_max!=0){
            auto it_left = top_nodes[idx_max-1];
            it_left->right=it_new;
            set_node_property(param, *it_left);
        }
        if(idx_max!=top_nodes.size()-1){
            auto it_right= top_nodes[idx_max+1];
            it_right->left=it_new;
            set_node_property(param, *it_right);
        }
        std::copy(top_nodes.cbegin()+idx_max+1,top_nodes.cend(),
                    top_nodes.begin()+idx_max);
        top_nodes.pop_back();
    }
}

void backward_path(Param const &param,
                   mat_type &gradsum_left, mat_type &gradsum_right,
                   vec_type &gradsum_bias,  
                   node_type const &phrase, vec_type mesg) {
    constexpr auto dim = Param::dim;
    using val_t =Param::value_type;
    using namespace util::math;
    auto vecloop_void = VecLoop_void<val_t,dim>{};
    auto matloop_void = MatLoop_void<val_t,dim,dim>{};
    vecloop_void(update_mesg_common_part, mesg.span, phrase.vec_wsum.span);
    gradsum_bias.span    += mesg.span;        
    matloop_void(back_prop_grad_W,
                 gradsum_left.span, mesg.span, phrase.left->vec.span);                             
    matloop_void(back_prop_grad_W,
                 gradsum_right.span, mesg.span, phrase.right->vec.span);
    if(phrase.left->is_combined()){
        Param::vec_type left_mesg;
        matloop_void(update_mesg_finalize, left_mesg.span, mesg.span, param.w_left.span);
        backward_path(param, gradsum_left, gradsum_right, gradsum_bias, 
                      *phrase.left, left_mesg);
    }
    if(phrase.right->is_combined()){
        Param::vec_type right_mesg;
        matloop_void(update_mesg_finalize, right_mesg.span, mesg.span, param.w_right.span);
        backward_path(param, gradsum_left, gradsum_right, gradsum_bias, 
                      *phrase.right, right_mesg);
    }
}

auto grad_u_score_L2norm_i=[](int64_t i, auto &grad, auto factor_u, auto const &u_score, 
                            auto factor_p, auto const &phrase)  {
    grad[i] += u_score[i]*factor_u + phrase[i]*factor_p;
};

auto grad_u_score_L2norm = [](auto &grad, auto const &u_score, auto const &phrase){
    using namespace util::math;
    constexpr auto dim = Param::dim;
    using val_t = Param::value_type;
    auto norm = norm_L2(u_score);
    auto score = dot(u_score, phrase);
    auto vecloop_void = VecLoop_void<val_t,dim>{};
    
    vecloop_void(grad_u_score_L2norm_i, grad, 
                 -score/(norm*norm*norm), u_score, 
                 val_t{1}/norm, phrase);
    // //Original scoring function without L2-norm factor
    // vecloop_void(grad_u_score_L2norm_i, grad, 0, u_score, 
    //              1, phrase);
};

void backward_path(Param const &param,
                   mat_type &gradsum_left, mat_type &gradsum_right,
                   vec_type &gradsum_bias, vec_type &gradsum_u_score,
                   node_type const &phrase) {
    grad_u_score_L2norm(gradsum_u_score.span, param.u_score.span, phrase.vec.span);
    // gradsum_u_score.span += phrase.vec.span;
    auto factor = Param::value_type{1}/util::math::norm_L2(param.u_score.span);
    auto mesg{param.u_score};
    mesg.span *=factor;
    backward_path(param, gradsum_left, gradsum_right, gradsum_bias, phrase, mesg);
}
// weighted_sum=W_left*word_left + W_right*word_right+bias
// s=u*h(g(f(weighted_sum)))
// dsdW_left = u cx .. h`.. g`... f`(weighted_sum) X word_left 
void backward_path(Param &grad, Param const &param,
                   node_type const &phrase) {
    backward_path(param, grad.w_left, grad.w_right, grad.bias, grad.u_score, phrase);
}

}//namespace rnn::simple_model::detail
}//namespace rnn::simple_model
}//namespace rnn