#include <memory>
#include <fmt/printf.h>

#include "wordrep/dep_graph.h"

#include "similarity/dep_similarity.h"
#include "data_source/ygp_db.h"
#include "data_source/rss.h"
#include "data_source/corenlp_helper.h"

#include "utils/profiling.h"
#include "utils/string.h"
#include "utils/optional.h"
#include "utils/span.h"
#include "utils/algorithm.h"

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

        ConnectionFragility subgrapher{graph, importance};
        subgrapher.set_score();
        for(auto node : graph.all_nodes()){
            auto uid = graph.sentence().tokens->word_uid(node.idx);
            fmt::print(std::cerr, "{:<15}  score : {:<7} {:<7}\n",
                       wordUIDs[uid], importance.score(uid), subgrapher.score(node));
        }
        fmt::print(std::cerr, "\n\n");
        std::vector<DPTokenIndex> sub_heads;
        sub_heads.push_back(graph.front().root_node().idx);
        for(int i=0; i<5; ++i){
            auto sub_head = graph.node(subgrapher.max_score_node_idx());
            sub_heads.push_back(sub_head.idx);
            graph.disconnect_head(sub_head.idx);
            subgrapher.set_score(); //TODO: remove duplicated computation.
        }
        for(auto sub_head_idx : sub_heads){
            auto sub_head = graph.node(sub_head_idx);
            fmt::print(std::cerr, "Head of subgraph : {}\n",
                       wordUIDs[sub_head.graph->sentence().tokens->word_uid(sub_head.idx)]);
            graph.iter_subgraph(sub_head, [&wordUIDs,&importance,&subgrapher](auto &node){
                auto uid = node.graph->sentence().tokens->word_uid(node.idx);
                fmt::print(std::cerr, "{:<15}  score : {:<7} {:<7}\n",
                           wordUIDs[uid], importance.score(uid), subgrapher.score(node));
            });
            fmt::print(std::cerr, "-----------------\n");
        }
    }
}


}//namespace wordrep::test
}//namespace wordrep

using namespace wordrep;
using engine::YGPQueryEngine;
using engine::RSSQueryEngine;

int main(int /*argc*/, char** argv){
    auto config = util::load_json(argv[1]);
    std::string input = argv[2];
//    auto dumpfile_hashes = argv[3];

    wordrep::test::dependency_graph();
    return 0;

    data::CoreNLPwebclient corenlp_client{config["corenlp_client_script"].get<std::string>()};
    auto query_str = util::string::read_whole(input);
    auto query_json = corenlp_client.from_query_content(query_str);
    query_json["query_str"] = query_str;

    util::Timer timer{};

    YGPQueryEngine engine{config};
//    RSSQueryEngine engine{config};
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
//    timer.here_then_reset("Processed a chain query.");
//    data::rss::annotation_on_result(config, chain_answers, dumpfile_hashes);
    data::ygp::annotation_on_result(config, chain_answers);
    fmt::print("{}\n", chain_answers.dump(4));

    auto stat_answer = engine.ask_query_stats(uids);
    timer.here_then_reset("Processed a stats query.");
//    data::rss::annotation_on_result(config, stat_answer["results"], dumpfile_hashes);
    fmt::print("{}\n", stat_answer.dump(4));
    util::json_t tmp;
    std::vector<int64_t> sents;
    for(auto& per_sent: stat_answer["stats"])
        for(auto& per_key : per_sent)
            for(auto& matches : per_key)
                for(auto uid : matches)
                    sents.push_back(uid);
    tmp["sents"]=sents;
    fmt::print("{}\n", tmp.dump(4));
    return 0;

    auto custom_query = uids;
    custom_query["sents"]=sents;
    custom_query["n_cut"]=30;
    fmt::print("{}\n", uids.dump(4));
    fmt::print("{}\n", custom_query.dump(4));
    auto chain_answers_custom = engine.ask_chain_query(custom_query);
    data::ygp::annotation_on_result(config, chain_answers_custom);
    fmt::print("{}\n", chain_answers_custom.dump(4));


//    auto content = engine.ask_sents_content(tmp);
    //fmt::print("{}\n", content.dump(4));
    timer.here_then_reset("Queries are answered.");

    return 0;
}

