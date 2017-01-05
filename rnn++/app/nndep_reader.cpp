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
#include "utils/versioned_name.h"

namespace wordrep{
namespace test {

void dependency_graph() {
    data::CoreNLPjson test_input{std::string{"../rnn++/tests/data/sentence.1.corenlp"}};
    data::CoreNLPjson test_input2{std::string{"../rnn++/tests/data/sentence.2.corenlp"}};
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

    PhraseSegmenter phrase_segmenter{importance};
    fmt::print(std::cerr, "{} {}\n", tokens.n_tokens(), sents.size());
    for (auto sent : sents) {
        DependencyGraph graph{sent};

        for (auto &node : graph.all_nodes()) {
            auto uid = sent.tokens->word_uid(node.idx);
            fmt::print(std::cerr, "{:<15} {:<5} ", wordUIDs[uid], importance.score(uid));
            if (node.governor)
                fmt::print(std::cerr, "head : {:<15}", wordUIDs[sent.tokens->word_uid(node.governor.value()->idx)]);
            else fmt::print(std::cerr, "head :{:<15} ", " ");
            fmt::print(std::cerr, "child: ");
            for (auto child : node.dependents)
                fmt::print(std::cerr, "{:<15} ", wordUIDs[sent.tokens->word_uid(child->idx)]);
            std::cerr << std::endl;
        }
        fmt::print(std::cerr, ": {}. Root : {}\n", sent.size(),
                   wordUIDs[sent.tokens->word_uid(graph.front().root_node().idx)]);

        auto sub_heads = phrase_segmenter.broke_into_phrases(graph, 5.0);
//        auto sub_heads = phrase_segmenter.broke_into_phrases(graph, 5);

        ConnectionFragility subgrapher{graph, importance};
        for (auto node : graph.all_nodes()) {
            auto uid = graph.sentence().tokens->word_uid(node.idx);
            fmt::print(std::cerr, "{:<15}  score : {:<7} {:<7}\n",
                       wordUIDs[uid], importance.score(uid), subgrapher.score(node));
        }
        fmt::print(std::cerr, "\n\n");

        for (auto sub_head_idx : sub_heads) {
            auto sub_head = graph.node(sub_head_idx);
            fmt::print(std::cerr, "Head of subgraph : {}\n",
                       wordUIDs[sub_head.graph->sentence().tokens->word_uid(sub_head.idx)]);
            graph.iter_subgraph(sub_head, [&wordUIDs, &importance, &subgrapher](auto &node) {
                auto uid = node.graph->sentence().tokens->word_uid(node.idx);
                fmt::print(std::cerr, "{:<15}  score : {:<7} {:<7}\n",
                           wordUIDs[uid], importance.score(uid), subgrapher.score(node));
            });
            fmt::print(std::cerr, "-----------------\n");
        }
    }
}

void phrases_in_sentence() {
    data::CoreNLPjson test_input{std::string{"../rnn++/tests/data/sentence.1.corenlp"}};
    data::CoreNLPjson test_input2{std::string{"../rnn++/tests/data/sentence.2.corenlp"}};
    WordUIDindex wordUIDs{"../rnn++/tests/data/words.uid"};
    POSUIDindex const posUIDs{"../rnn++/tests/data/poss.uid"};
    ArcLabelUIDindex const arclabelUIDs{"../rnn++/tests/data/dep.uid"};
    WordImportance importance{"../rnn++/tests/data/word_importance"};

    DepParsedTokens tokens{};
    tokens.append_corenlp_output(wordUIDs, posUIDs, arclabelUIDs, test_input);
    tokens.append_corenlp_output(wordUIDs, posUIDs, arclabelUIDs, test_input2);
    tokens.build_sent_uid(0);
    auto sents = tokens.IndexSentences();

    PhraseSegmenter phrase_segmenter{importance};
    fmt::print(std::cerr, "{} {}\n", tokens.n_tokens(), sents.size());
    for (auto sent : sents) {
        auto phrases = phrase_segmenter.broke_into_phrases(sent, 5.0);
        for (auto phrase : phrases) {
            for (auto idx : phrase.idxs) {
                fmt::print(std::cerr, "{} ", wordUIDs[tokens.word_uid(idx)]);
            }
            fmt::print(std::cerr, "\n");
        }
    }
}


void phrases_in_sentence(util::json_t const& config) {
    using util::io::h5read;
    fmt::print(std::cerr, "Read {}\n",
               util::get_latest_version(util::get_str(config, "dep_parsed_store")).fullname);
    DepParsedTokens tokens{util::get_latest_version(util::get_str(config, "dep_parsed_store")),
                           config["dep_parsed_prefix"]};
    WordUIDindex wordUIDs{util::get_str(config,"word_uids_dump")};
    WordImportance importance{h5read(util::get_str(config,"word_prob_dump"))};
    data::DBIndexer indexer{h5read(util::get_latest_version(util::get_str(config, "dep_parsed_store")).fullname),
                            config["dep_parsed_prefix"].get<std::string>()};
    auto sents = tokens.IndexSentences();

    PhraseSegmenter phrase_segmenter{importance};
    fmt::print(std::cerr, "{} {}\n", tokens.n_tokens(), sents.size());
    auto i=0;
    for (auto sent : sents) {
        if(util::diff(sent.end,sent.beg) > 30) continue;
        for(auto idx=sent.beg; idx!=sent.end; ++idx){
            fmt::print(std::cerr, "{} ", wordUIDs[tokens.word_uid(idx)]);
        }
        if(++i>100) break;
        auto phrases = phrase_segmenter.broke_into_phrases(sent, 5.0);
        fmt::print(std::cerr, "\n:Original sentence of {} words. {} phrases:\n",
                   util::diff(sent.end,sent.beg), phrases.size());
        for (auto phrase : phrases) {
            for (auto idx : phrase.idxs) {
                fmt::print(std::cerr, "{} ", wordUIDs[tokens.word_uid(idx)]);
            }
            fmt::print(std::cerr, "\n");
        }
    }
}

}//namespace wordrep::test
}//namespace wordrep

void test_all(int argc, char** argv){
    assert(argc>1);
    auto config = util::load_json(argv[1]);
    wordrep::test::dependency_graph();
    wordrep::test::phrases_in_sentence();
    wordrep::test::phrases_in_sentence(config);
}

using namespace wordrep;
using engine::YGPQueryEngine;
using engine::RSSQueryEngine;

int main(int argc, char** argv){
    assert(argc>2);
    auto config = util::load_json(argv[1]);
    std::string input = argv[2];
//    auto dumpfile_hashes = argv[3];

    test_all(argc,argv);
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

