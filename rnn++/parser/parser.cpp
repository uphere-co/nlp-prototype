#include "parser/parser.h"

namespace rnn{
namespace simple_model{
    
Param get_gradient(Param const &param, InializedLeafNodes &nodes ) {
    using namespace detail;

    // auto timer=Timer{};
    auto& all_nodes = nodes.val; 
    auto n_words=all_nodes.size();
    // timer.here_then_reset("setup");
    auto top_nodes = merge_leaf_nodes(param, all_nodes);
    auto merge_history = foward_path(param, top_nodes);
    // timer.here_then_reset("forward path");
    rnn::simple_model::Param grad{};
    for(auto i=n_words; i<all_nodes.size(); ++i){
        auto const &node=all_nodes[i];
        assert(node.is_combined());
        // print_all_descents(node);
        backward_path(grad, param, node);
    }
    // timer.here_then_reset("backward path");
    return grad;
}

}//namespace rnn::simple_model
}//namespace rnn
