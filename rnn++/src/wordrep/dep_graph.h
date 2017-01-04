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
    util::span_dyn<const Node> all_nodes() const {return cspan;}

    template<typename T>
    void iter_subgraph(Node const &node, T const &op) const {
        op(node);
        for(auto child : node.dependents) iter_subgraph(*child, op);
    }

    Sentence const& sentence() const {return *sent;}

private:
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

}//namespace wordrep;
