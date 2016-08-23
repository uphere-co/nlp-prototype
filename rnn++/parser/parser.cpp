#include "parser/parser.h"

#include "utils/parallel.h"
#include "utils/string.h"

namespace rnn{
namespace simple_model{


TokenizedSentences::TokenizedSentences(std::string tokenized_file)
    : val{util::string::readlines(tokenized_file)} {}

    
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

Param::value_type get_full_score(Param const &param, InializedLeafNodes &nodes ) {
    using namespace rnn::simple_model::detail;
    auto& all_nodes = nodes.val; 
    auto top_nodes = merge_leaf_nodes(param, all_nodes);
    foward_path(param, top_nodes);
    Param::value_type score{};
    for(auto node: top_nodes){
        score+= node->score;
    }
    return score;
}

Param::value_type scoring_dataset(VocaInfo const &rnn, Param const &param, 
                                  TokenizedSentences const &dataset){
    using rnn::type::float_t;
    auto &lines = dataset.val;
    auto get_score=[&rnn,&param](auto sentence){
        auto nodes = rnn.initialize_tree(sentence);
        return get_full_score(param, nodes);
    };
    // //Serial version for debugging:
    // float_t score_accum{};
    // for(auto sentence : lines) score_accum += get_score(sentence);
    auto score_accum = util::parallel_reducer(lines.cbegin(), lines.cend(), get_score, float_t{});
    return score_accum;
}

}//namespace rnn::simple_model
}//namespace rnn
