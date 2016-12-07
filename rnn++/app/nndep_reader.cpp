#include <memory>
#include <fmt/printf.h>

#include "similarity/dep_similarity.h"
#include "data_source/ygp_db.h"
#include "data_source/rss.h"
#include "data_source/corenlp_helper.h"

#include "utils/profiling.h"
#include "utils/string.h"
#include "utils/optional.h"
#include "utils/span.h"
#include "utils/algorithm.h"

namespace wordrep {

struct DependencyGraph {
    struct Node{
        using val_t = double;

        bool is_leaf() const {return !dependents.size();}
        std::optional<DPTokenIndex> head_idx() const {
            if(governor) return governor.value()->idx;
            return {};
        }

        Node const& root_node() const {
            auto node = this;
            while(node->governor) node = node->governor.value();
            return *node;
        }

        DPTokenIndex idx;
        std::optional<Node*> governor;
        std::vector<Node*> dependents;
    };

    DependencyGraph(Sentence const & sent)
            : sent{&sent},
              nodes(std::make_unique<Node[]>(sent.size())),
              span{nodes.get(), util::to_type<uint32_t>(sent.size())}, //span_dyn use uint32...
              cspan{span} {
        for (auto idx = sent.beg; idx != sent.end; ++idx) {
            auto &node = nodes[sent.tokens->word_pos(idx).val];
            node.idx = idx;
            auto head_pos = sent.tokens->head_pos(idx);
            if(!head_pos) continue;
            auto &head = nodes[head_pos.value().val];
            node.governor = &head;
            head.dependents.push_back(&node);
        }
    }


    Node const& front() const {return nodes[0];};
    util::span_dyn<const Node> all_nodes() const {return cspan;}

    template<typename T>
    void iter_subgraph(Node const &node, T const &op) const {
        op(node);
        for(auto child : node.dependents) iter_subgraph(*child, op);
    }
    template<typename T>
    void iter_subgraph(Node const &node, std::vector<Node const *> ascents, T const &op) const {
        op(node, ascents);
        ascents.push_back(&node);
        for(auto child : node.dependents) iter_subgraph(*child, ascents, op);
    }

    Sentence const* sent;
    std::unique_ptr<Node[]> const nodes;
    util::span_dyn<Node> span;
    util::span_dyn<const Node> cspan;
};



}//namespace wordrep;

namespace wordrep{
namespace test{

void dependency_graph(){
    data::CoreNLPjson test_input{std::string{"../rnn++/tests/data/sentence.1.corenlp"}  };
    data::CoreNLPjson test_input2{std::string{"../rnn++/tests/data/sentence.2.corenlp"}  };
    WordUIDindex wordUIDs{"../rnn++/tests/data/words.uid"};
    POSUIDindex const posUIDs{"../rnn++/tests/data/poss.uid"};
    ArcLabelUIDindex const arclabelUIDs{"../rnn++/tests/data/dep.uid"};
    WordImportance importance{"../rnn++/tests/data/word_importance"};

    DepParsedTokens tokens{};
    tokens.append_corenlp_output(wordUIDs, posUIDs, arclabelUIDs, test_input);
    tokens.append_corenlp_output(wordUIDs, posUIDs, arclabelUIDs, test_input2);
    tokens.build_sent_uid(0);
    //tokens.build_voca_index(voca.indexmap);

    auto sents = tokens.IndexSentences();
    fmt::print(std::cerr, "{} {}\n", tokens.n_tokens(), sents.size());
    for(auto sent : sents){
        DependencyGraph graph{sent};
        for(auto &node : graph.all_nodes()){
            auto uid = tokens.word_uid(node.idx);
            fmt::print(std::cerr, "{:<15} {:<5} ", wordUIDs[uid], importance.score(uid));
            if(node.governor) fmt::print(std::cerr, "head : {:<15}", wordUIDs[tokens.word_uid(node.governor.value()->idx)]);
            else fmt::print(std::cerr, "head :{:<15} ", " ");
            fmt::print(std::cerr, "child: ");
            for(auto child : node.dependents) fmt::print(std::cerr, "{:<15} ", wordUIDs[tokens.word_uid(child->idx)]);
            std::cerr<<std::endl;
        }
        fmt::print(std::cerr, ": {}. Root : {}\n", sent.size(), wordUIDs[tokens.word_uid(graph.front().root_node().idx)]);

        std::map<DPTokenIndex, double> scores;
        auto connection_fragility =[&graph, &importance, &scores](DependencyGraph::Node const &node){
            auto self_weight_sum = scores[node.idx];
            auto uid = graph.sent->tokens->word_uid(node.idx);
            auto self_weight     = importance.score(uid);
            auto head_weight_sum = scores[node.head_idx().value_or(-1)] - self_weight;
            return self_weight_sum*head_weight_sum / self_weight;
        };


            graph.iter_subgraph(graph.front().root_node(), {},
                            [&scores,&wordUIDs,&importance,&graph](auto &node, auto &ascents) {
                                auto uid = graph.sent->tokens->word_uid(node.idx);
                                auto score = importance.score(uid);
//                                fmt::print(std::cerr, "{} : {}. ", wordUIDs[uid], score);
                                scores[node.idx] += score;
                                for (auto ascent : ascents){
//                                    fmt::print(std::cerr, "{} ",wordUIDs[graph.sent->tokens->word_uid(ascent->idx)]);
                                    scores[ascent->idx] += score;
                                }
//                                std::cerr << std::endl;
                            });
        for(auto node : graph.all_nodes()){
            auto uid = graph.sent->tokens->word_uid(node.idx);
            fmt::print(std::cerr, "Visit node : {:<15}. score : {:<7} {:<7} {:<7} {:<7}   {:<7}\n",
                       wordUIDs[uid], importance.score(uid),
                       scores[node.idx],
                       scores[node.head_idx().value_or(-1)],
                       scores[node.idx]/importance.score(uid),
                       connection_fragility(node));
        }
    }
}


}//namespace wordrep::test
}//namespace wordrep

using namespace wordrep;
using namespace engine;

int main(int /*argc*/, char** argv){
    auto config = util::load_json(argv[1]);
    std::string input = argv[2];
    auto dumpfile_hashes = argv[3];
    wordrep::test::dependency_graph();
    return 0;

    data::CoreNLPwebclient corenlp_client{config["corenlp_client_script"].get<std::string>()};
    auto query_str = util::string::read_whole(input);
    auto query_json = corenlp_client.from_query_content(query_str);
    query_json["query_str"] = query_str;

    util::Timer timer{};

//    DepSimilaritySearch engine{config};
    RSSQueryEngine engine{config};
    timer.here_then_reset("Data loaded.");
    auto uids = engine.register_documents(query_json);
    uids["max_clip_len"] = query_json["max_clip_len"];
    //fmt::print("{}\n", uids.dump(4));
    timer.here_then_reset("Registered documents.");
    auto answers = engine.ask_query(uids);
    timer.here_then_reset("Processed a query.");
//    data::ygp::annotation_on_result(config, answers);
    timer.here_then_reset("Query output annotation.");
    fmt::print("{}\n", answers.dump(4));
    fmt::print("\n\n--------- ------------\nA chain query find results:\n", answers.dump(4));
    timer.here_then_reset("Begin a chain query.");
    auto chain_answers = engine.ask_chain_query(uids);
    timer.here_then_reset("Processed a chain query.");
    data::rss::annotation_on_result(config, chain_answers, dumpfile_hashes);
//    data::ygp::annotation_on_result(config, chain_answers);
    timer.here_then_reset("A chain query output annotatoin.");
    fmt::print("{}\n", chain_answers.dump(4));
    timer.here_then_reset("Queries are answered.");

    return 0;
}
