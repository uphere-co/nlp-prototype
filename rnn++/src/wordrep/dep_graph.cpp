#include <fmt/printf.h>

#include "wordrep/dep_graph.h"

#include "utils/algorithm.h"
#include "wordrep/dep_parsed.h"

namespace wordrep {

bool DependencyGraph::Node::is_leaf() const {
    return !dependents.size();
}
std::optional<DPTokenIndex> DependencyGraph::Node::head_idx() const {
    if(governor) return governor.value()->idx;
    return {};
}
DependencyGraph::Node const& DependencyGraph::Node::root_node() const {
    auto node = this;
    while(node->governor) node = node->governor.value();
    return *node;
}
template<typename T>
void DependencyGraph::Node::iter_to_top(T const &op) const{
    auto node=this;
    for(; node->governor; node=node->governor.value()) op(*node);
    op(*node);
}

DependencyGraph::DependencyGraph(Sentence const & sent)
        : sent{&sent},
          nodes(std::make_unique<Node[]>(sent.size())),
          span{nodes.get(), util::to_type<uint32_t>(sent.size())}, //span_dyn use uint32...
          cspan{span} {
    for (auto idx = sent.beg; idx != sent.end; ++idx) {
        auto &node = nodes[sent.tokens->word_pos(idx).val];
        node.idx = idx;
        node.graph = this;
        auto head_pos = sent.tokens->head_pos(idx);
        if(!head_pos) continue;
        auto &head = nodes[head_pos.value().val];
        node.governor = &head;
        head.dependents.push_back(&node);
    }
    for(auto &node : span) idx2node[node.idx]=&node;
}

void DependencyGraph::disconnect_head(DPTokenIndex idx){
    auto node = idx2node.at(idx);
    auto& head = node->governor;
    if(!head) return;
    auto& sisters = head.value()->dependents;
    auto self = std::find_if(sisters.begin(), sisters.end(),[idx](auto &x){
        return x->idx == idx;
    });
    std::swap(*self, sisters.back());
    sisters.pop_back();
    node->governor = {};
}

ConnectionFragility::ConnectionFragility(DependencyGraph const &graph,
                                         WordImportance const &importance)
        : graph{graph}, importance(importance) {
    set_score();
}

void ConnectionFragility::set_score(){
    for(auto& pair : scores) pair.second=0;
    for(auto node : graph.all_nodes()) {
        auto uid = graph.sentence().tokens->word_uid(node.idx);
        auto score = importance.score(uid);
        node.iter_to_top([this,score](auto const &node){ scores[node.idx] += score;});
    }
}
ConnectionFragility::val_t ConnectionFragility::score(node_t const &node) const {
    auto self_weight_sum = scores.at(node.idx);
    auto uid = node.graph->sentence().tokens->word_uid(node.idx);
    auto self_weight     = importance.score(uid);
    auto head_idx = node.head_idx();
    auto head_weight_sum = head_idx? scores.at(head_idx.value()) : 0;
    head_weight_sum -= self_weight;
    return self_weight_sum*head_weight_sum / self_weight;
}

DPTokenIndex ConnectionFragility::max_score_node_idx() const {
    auto pair = std::max_element(scores.cbegin(), scores.cend(), [this](auto x, auto y){
        return score(x.first) < score(y.first);
    });
    return pair->first;
}

std::vector<DPTokenIndex> PhraseSegmenter::broke_into_phrases(DependencyGraph& graph, int n_phrase) const {
    ConnectionFragility subgrapher{graph, importance};
    std::vector<DPTokenIndex> sub_heads;
    sub_heads.push_back(graph.front().root_node().idx);
    for(int i=0; i<n_phrase; ++i){
        auto sub_head = graph.node(subgrapher.max_score_node_idx());
//        fmt::print("{} : {} score\n", i, subgrapher.score(sub_head));
        sub_heads.push_back(sub_head.idx);
        graph.disconnect_head(sub_head.idx);
        subgrapher.set_score(); //TODO: remove duplicated computation.
    }
    return sub_heads;
}

std::vector<DPTokenIndex> PhraseSegmenter::broke_into_phrases(DependencyGraph& graph, val_t cutoff) const {
    if(graph.empty()) return {};
    ConnectionFragility subgrapher{graph, importance};
    std::vector<DPTokenIndex> sub_heads;
    sub_heads.push_back(graph.front().root_node().idx);
    for(;;){
        auto sub_head = graph.node(subgrapher.max_score_node_idx());
//        fmt::print("{} : {} score\n", i, subgrapher.score(sub_head));
        if(subgrapher.score(sub_head) < cutoff) break;
        sub_heads.push_back(sub_head.idx);
        graph.disconnect_head(sub_head.idx);
        subgrapher.set_score(); //TODO: remove duplicated computation.
    }
    return sub_heads;
}


}//namespace wordrep;
