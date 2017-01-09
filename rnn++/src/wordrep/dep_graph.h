#pragma once

#include <vector>
#include <map>

#include "wordrep/word_prob.h"
#include "wordrep/indexes.h"
#include "wordrep/sentence.h"

#include "utils/span.h"
#include "utils/optional.h"

namespace wordrep {

struct DependencyGraph {
    struct Node{
        using val_t = double;

        bool is_leaf() const;
        std::optional<DPTokenIndex> head_idx() const;
        Node const& root_node() const;
        template<typename T>
        void iter_to_top(T const &op) const;

        DPTokenIndex idx;
        std::optional<Node*> governor;
        std::vector<Node*> dependents;
        DependencyGraph* graph;
    };

    DependencyGraph(Sentence const & sent);

    void disconnect_head(DPTokenIndex idx);
    Node const& node(DPTokenIndex idx) const{return *idx2node.at(idx);}
    Node const& front() const {return nodes[0];};
    auto empty() const {return idx2node.empty();}
    util::span_dyn<const Node> all_nodes() const {return cspan;}

    template<typename OP>
    void iter_subgraph(Node const &node, OP const& op) const {
        op(node);
        for(auto child : node.dependents) iter_subgraph(*child, op);
    }
    template<typename OP>
    auto map_subgraph(Node const &node, OP const& op) const {
        std::vector<decltype(op(node))> vals;
        map_subgraph(node,op,vals);
        return vals;
    }

    Sentence const& sentence() const {return *sent;}

private:
    template<typename OP, typename TM>
    void map_subgraph(Node const &node, OP op, std::vector<TM> &vals) const {
        DPTokenIndex  idx = op(node);
        vals.push_back(idx);
        for(auto child : node.dependents) map_subgraph(*child, op, vals);
    }

    Sentence const* sent;
    std::unique_ptr<Node[]> const nodes;
    std::map<DPTokenIndex,Node *> idx2node;
    util::span_dyn<Node> span;
    util::span_dyn<const Node> cspan;
};


class ConnectionFragility{
public:
    using node_t = DependencyGraph::Node;
    using val_t = node_t::val_t;
    ConnectionFragility(DependencyGraph const &graph,
                        WordImportance const &importance);

    void set_score();
    val_t score(node_t const &node) const;
    auto score(DPTokenIndex idx ) const {return score(graph.node(idx));}
    DPTokenIndex max_score_node_idx() const;

private:
    std::map<DPTokenIndex, val_t> scores;
    DependencyGraph const &graph;
    wordrep::WordImportance const &importance;
};

struct Phrase{
    Phrase(Sentence const& sent, std::vector<DPTokenIndex> &&tokens)
            : idxs{std::move(tokens)}, sent{sent} {
        std::sort(idxs.begin(), idxs.end());
    }

    std::vector<WordUID> to_word_uids() const;
    bool isin(WordUID uid) const;
    std::vector<DPTokenIndex> idxs;
    Sentence const& sent;
};
struct PhraseSegmenter{
    using val_t = ConnectionFragility::val_t;

    PhraseSegmenter(WordImportance const& importance)
            : importance{importance}
    {}

    std::vector<DPTokenIndex> broke_into_phrases(DependencyGraph& graph, int n_phrase) const;
    std::vector<DPTokenIndex> broke_into_phrases(DependencyGraph& graph, val_t cutoff) const;

    std::vector<Phrase> broke_into_phrases(Sentence const& sent, val_t cutoff) const{
        DependencyGraph graph{sent};
        auto phrase_heads = broke_into_phrases(graph, cutoff);
        std::vector<Phrase> phrases;
        for(auto phrase_head : phrase_heads){
            auto tokens_in_phrase = graph.map_subgraph(graph.node(phrase_head), [](auto &node){
                return node.idx;
            });
            phrases.emplace_back(sent, std::move(tokens_in_phrase));
        }
        return phrases;
    }

private:
    WordImportance const& importance;
};

}//namespace wordrep;
