#include "parser/parser.h"

#include "utils/parallel.h"
#include "utils/string.h"
#include "utils/binary_tree.h"

namespace rnn{
namespace simple_model{
SparseGrad& operator +=(SparseGrad& out, const SparseGrad& x){
    for(auto const &p : x.val) out.val[p.first] += p.second;
    return out; 
}
SparseGrad& operator -=(SparseGrad& out, const SparseGrad& x){
    for(auto const &p : x.val) out.val[p.first] -= p.second;
    return out;
}
SparseGrad& operator *=(SparseGrad& out, Param::value_type x){
    for(auto &p : out.val) p.second*=x;
    return out;
}
SparseGrad operator +(const SparseGrad& x, const SparseGrad& y){
    SparseGrad out{x};
    out+=y;
    return out;
}
SparseGrad operator -(const SparseGrad& x, const SparseGrad& y){
    SparseGrad out{x};
    out-=y;
    return out;
}
SparseGrad operator *(const SparseGrad& x, Param::value_type v){
    SparseGrad out{x};
    out*=v;
    return out;
}
Gradient& operator +=(Gradient& out, const Gradient& x){
    out.param += x.param;
    out.words += x.words;
    return out;
}
Gradient& operator -=(Gradient& out, const Gradient& x){
    out.param -= x.param;
    out.words -= x.words;
    return out;
}
Gradient operator +(const Gradient& x, const Gradient& y){
    Gradient out{x};
    out+=y;
    return out;
}
Gradient operator -(const Gradient& x, const Gradient& y){
    Gradient out{x};
    out-=y;
    return out;
}


TokenizedSentences::TokenizedSentences(std::string tokenized_file)
    : val{util::string::readlines(tokenized_file)} {}
ParsedSentences::ParsedSentences(std::string parsed_file)
    : val{util::string::readlines(parsed_file)} {}

DPtable dp_merging(Param const &param, InializedLeafNodes &initialized_nodes){
    auto &nodes = initialized_nodes.val;
    DPtable table{nodes};
    table.compute(param);
    return table;
}
DPtable dp_merging_with_penalty(Param const &param,
                                InializedLeafNodes &initialized_nodes,
                                DPtable::val_t lambda,
                                SentencePair const &sent_pair){
    auto &nodes = initialized_nodes.val;
    DPtable table{nodes};
    table.compute(param, lambda, sent_pair.parsed);
    return table;
}
Param::value_type get_full_dp_score(Param const &param,
                                    InializedLeafNodes &initialized_nodes,
                                    DPtable::val_t lambda,
                                    SentencePair const &sent_pair) {
    DPtable table=dp_merging_with_penalty(param, initialized_nodes, lambda, sent_pair);
    auto phrases = table.get_phrases();    
    auto score_dp{0.0};
    for(auto phrase : phrases) score_dp += phrase->score;
    return score_dp;
}

Param::value_type dp_scoring_dataset(VocaInfo const &rnn, Param const &param, 
                                     rnn::type::float_t lambda, SentencePairs const &dataset){
    using rnn::type::float_t;
    auto &lines = dataset.val;
    auto get_score=[&rnn,&param,&lambda](auto sent_pair){
        auto initialized_nodes = rnn.initialize_tree(sent_pair.original);
        return get_full_dp_score(param, initialized_nodes, lambda, sent_pair);
    };
    auto score_accum = util::parallel_reducer(lines.cbegin(), lines.cend(), get_score, float_t{});
    return score_accum;
}

Param::value_type get_full_greedy_score(Param const &param, InializedLeafNodes &nodes ) {
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

Param::value_type greedy_scoring_dataset(VocaInfo const &rnn, Param const &param, 
                                  TokenizedSentences const &dataset){
    using rnn::type::float_t;
    auto &lines = dataset.val;
    auto get_score=[&rnn,&param](auto sentence){
        auto nodes = rnn.initialize_tree(sentence);
        return get_full_greedy_score(param, nodes);
    };
    // //Serial version for debugging:
    // float_t score_accum{};
    // for(auto sentence : lines) score_accum += get_score(sentence);
    auto score_accum = util::parallel_reducer(lines.cbegin(), lines.cend(), get_score, float_t{});
    return score_accum;
}

Param::value_type scoring_parsed_sentence(VocaInfo const &rnn, Param const &param,
                                          SentencePair const &sent_pair){
    using namespace util;
    using namespace detail;
    auto parsed_sentence=sent_pair.parsed;
    auto original_sentence=sent_pair.original;
    auto merge_history = get_merge_history(parsed_sentence);
    auto nodes = rnn.initialize_tree(original_sentence);
    assert(nodes.val.size()==merge_history.size()+1);
    auto& all_nodes = nodes.val; 
    auto n_words=all_nodes.size();
    auto top_nodes = merge_leaf_nodes(param, all_nodes);
    directed_merge(param, top_nodes,merge_history);
    Param::value_type score{};
    for(auto i=n_words; i<all_nodes.size(); ++i){
        score+= all_nodes[i].score;
    }
    return score;
}
Param::value_type scoring_parsed_dataset(VocaInfo const &rnn, Param const &param, 
                                         SentencePairs const &dataset){
    using rnn::type::float_t;
    auto &lines = dataset.val;
    auto get_score=[&](auto const &sent_pair){
        return scoring_parsed_sentence(rnn, param, sent_pair);
    };
    auto score_accum = util::parallel_reducer(lines.cbegin(), lines.cend(), get_score, float_t{});
    return score_accum;
}

Gradient get_greedy_gradient(Param const &param, InializedLeafNodes &nodes ) {
    using namespace detail;

    // auto timer=Timer{};
    auto& all_nodes = nodes.val; 
    auto n_words=all_nodes.size();
    // timer.here_then_reset("setup");
    auto top_nodes = merge_leaf_nodes(param, all_nodes);
    auto merge_history = foward_path(param, top_nodes);
    // timer.here_then_reset("forward path");
    Gradient grad{};

    for(auto i=n_words; i<all_nodes.size(); ++i){
        auto const &node=all_nodes[i];
        assert(node.is_combined());
        // print_all_descents(node);
        backward_path(grad.param, param, node);
    }
    for(decltype(n_words)i=0; i<n_words;++i){
        auto const &node=all_nodes[i];
        assert(node.is_leaf());
        grad.words.val[node.name.idx]+=node.vec_update;//collecting word_update
    }
    // timer.here_then_reset("backward path");
    return grad;
}
Gradient get_dp_gradient(Param const &param, rnn::type::float_t lambda,
                         InializedLeafNodes &init_nodes,
                         SentencePair const &sent_pair) {
    using namespace detail;
    DPtable table=dp_merging_with_penalty(param, init_nodes, lambda, sent_pair);
    auto phrases = table.get_phrases();
    Gradient grad{};
    for(auto node : phrases){
        assert(node->is_combined());
        backward_path(grad.param, param, *node);
    }
    auto leafs = table.get_leafs();
    for(auto node : leafs){
        assert(node->is_leaf());
        grad.words.val[node->name.idx]+=node->vec_update;//collecting word_update
    }
    return grad;
}

Gradient get_directed_grad(VocaInfo const &rnn, Param const &param, 
                        SentencePair const &sent_pair){
    using namespace util;
    using namespace detail;
    auto parsed_sentence=sent_pair.parsed;
    auto original_sentence=sent_pair.original;
    auto merge_history = get_merge_history(parsed_sentence);
    auto nodes = rnn.initialize_tree(original_sentence);
    auto& all_nodes = nodes.val; 
    auto n_words=all_nodes.size();
    auto top_nodes = merge_leaf_nodes(param, all_nodes);
    directed_merge(param, top_nodes,merge_history);
    Gradient grad{};
    for(auto i=n_words; i<all_nodes.size(); ++i){
        auto const &node=all_nodes[i];
        assert(node.is_combined());
        backward_path(grad.param, param, node);
    }
    for(decltype(n_words)i=0; i<n_words;++i){
        auto const &node=all_nodes[i];
        assert(node.is_leaf());
        grad.words.val[node.name.idx]+=node.vec_update;//collecting word_update
    }
    return grad;
}

}//namespace rnn::simple_model
}//namespace rnn
