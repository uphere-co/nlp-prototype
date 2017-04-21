#include <memory>
#include <fmt/printf.h>

#include "wordrep/dep_graph.h"
#include "wordrep/word_case_corrector.h"

#include "similarity/config.h"
#include "similarity/query_engine.h"
#include "similarity/phrase_suggestion.h"
#include "similarity/similarity_measure.h"

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
    WordImportance importance{"../rnn++/tests/data/word_importance",
                              "../rnn++/tests/data/words.uid"};

    DepParsedTokens tokens{};
    tokens.append_corenlp_output(test_input);
    tokens.append_corenlp_output(test_input2);
    tokens.build_sent_uid(0);
    //tokens.build_voca_index(voca.indexmap);

    auto sents = tokens.IndexSentences();

    PhraseSegmenter phrase_segmenter{importance};
    fmt::print(std::cerr, "{} {}\n", tokens.n_tokens(), sents.size());
    for (auto sent : sents) {
        DependencyGraph graph{sent};

        for (auto &node : graph.all_nodes()) {
            auto uid = sent.dict->word_uid(node.idx);
            fmt::print(std::cerr, "{:<15} {:<5} ", wordUIDs[uid], importance.score(uid));
            if (node.governor)
                fmt::print(std::cerr, "head : {:<15}", wordUIDs[sent.dict->word_uid(node.governor.value()->idx)]);
            else fmt::print(std::cerr, "head :{:<15} ", " ");
            fmt::print(std::cerr, "child: ");
            for (auto child : node.dependents)
                fmt::print(std::cerr, "{:<15} ", wordUIDs[sent.dict->word_uid(child->idx)]);
            std::cerr << std::endl;
        }
        fmt::print(std::cerr, ": {}. Root : {}\n", sent.size(),
                   wordUIDs[sent.dict->word_uid(graph.front().root_node().idx)]);

        auto sub_heads = phrase_segmenter.broke_into_phrases(graph, 5.0);
//        auto sub_heads = phrase_segmenter.broke_into_phrases(graph, 5);

        ConnectionFragility subgrapher{graph, importance};
        for (auto node : graph.all_nodes()) {
            auto uid = graph.sentence().dict->word_uid(node.idx);
            fmt::print(std::cerr, "{:<15}  score : {:<7} {:<7}\n",
                       wordUIDs[uid], importance.score(uid), subgrapher.score(node));
        }
        fmt::print(std::cerr, "\n\n");

        for (auto sub_head_idx : sub_heads) {
            auto sub_head = graph.node(sub_head_idx);
            fmt::print(std::cerr, "Head of subgraph : {}\n",
                       wordUIDs[sub_head.graph->sentence().dict->word_uid(sub_head.idx)]);
            graph.iter_subgraph(sub_head, [&wordUIDs, &importance, &subgrapher](auto &node) {
                auto uid = node.graph->sentence().dict->word_uid(node.idx);
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
    WordImportance importance{"../rnn++/tests/data/word_importance",
                              "../rnn++/tests/data/words.uid"};

    DepParsedTokens tokens{};
    tokens.append_corenlp_output(test_input);
    tokens.append_corenlp_output(test_input2);
    tokens.build_sent_uid(0);
    auto sents = tokens.IndexSentences();

    PhraseSegmenter phrase_segmenter{importance};
    fmt::print(std::cerr, "{} {}\n", tokens.n_tokens(), sents.size());
    for (auto sent : sents) {
        auto phrases = phrase_segmenter.broke_into_phrases(sent, 5.0);
        for (auto phrase : phrases) {
            fmt::print(std::cerr, "{}\n", phrase.repr(wordUIDs));
        }
    }
}


void phrases_in_sentence(engine::SubmoduleFactory const& factory) {
    DepParsedTokens tokens = factory.dep_parsed_tokens();
    WordUIDindex wordUIDs = factory.word_uid_index();
    WordImportance importance = factory.word_importance();
    auto sents = tokens.IndexSentences();

    PhraseSegmenter phrase_segmenter{importance};
    fmt::print(std::cerr, "{} tokens and {} sentences.\n", tokens.n_tokens(), sents.size());
    auto i=0;
    for (auto sent : sents) {
        if(sent.size() > 30) continue;
        if(++i>100) break;
        fmt::print("{}\n", sent.repr(wordUIDs));
        auto phrases = phrase_segmenter.broke_into_phrases(sent, 5.0);
        fmt::print(": --- Original sentence of {} words. {} phrases --- :\n",
                   sent.size(), phrases.size());
        for (auto phrase : phrases) {
            fmt::print("{}\n", phrase.repr(wordUIDs));
        }
        fmt::print("---------------------------------------\n\n");
    }
}


void dataset_indexing_quality(engine::SubmoduleFactory const& factory){
    DepParsedTokens tokens = factory.dep_parsed_tokens();
    WordUIDindex wordUIDs  = factory.word_uid_index();
    VocaInfo voca          = factory.voca_info();
    WordImportance importance = factory.word_importance();
    auto sents = tokens.IndexSentences();
    auto dist_measure = similarity::Similarity<similarity::measure::angle>{};

    std::string word1 = "drink";
    std::string word2 = "water";
    auto widx1 = voca.indexmap[wordUIDs[word1]];
    auto widx2 = voca.indexmap[wordUIDs[word2]];
    auto similarity = dist_measure(voca.wvecs[widx1], voca.wvecs[widx2]);
    fmt::print(std::cerr, "{} {} {} vs {} {} {}",
               word1, widx1, util::math::sum(voca.wvecs[widx1]),
               word2, widx2, util::math::sum(voca.wvecs[widx2]));
    fmt::print(std::cerr, "{} {} : {}", word1, word2, similarity);
    auto sent1 = sents[0];
    auto sent2 = sents[1];
    for(auto idx1 : sent1){
        auto widx1 = tokens.word(idx1);
        auto uid1  = voca.indexmap[widx1];
        auto word1 = wordUIDs[uid1];
        //0.6 is a cutoff for skip noisy words, words with low importance score.
        if(importance.score(uid1)<0.6) continue;
        for(auto idx2 : sent2) {
            auto widx2 = tokens.word(idx2);
            auto uid2  = voca.indexmap[widx2];
            auto word2 = wordUIDs[uid2];
            if(importance.score(uid2)<0.6) continue;
            //auto similarity = dist_measure(voca.wvecs[widx1], voca.wvecs[widx2]);
//            fmt::print(std::cerr, "{} {} {} vs {} {} {}",
//                       word1, widx1, util::math::sum(voca.wvecs[widx1]),
//                       word2, widx2, util::math::sum(voca.wvecs[widx2]));
//            fmt::print(std::cerr, " :  {}\n", similarity);
        }
    }
}



void phrase_stats(engine::SubmoduleFactory const& factory){
    DepParsedTokens tokens = factory.dep_parsed_tokens();
    WordUIDindex wordUIDs  = factory.word_uid_index();
    VocaInfo voca          = factory.voca_info();
    WordImportance importance = factory.word_importance();
    auto sents = tokens.IndexSentences();
    engine::WordUsageInPhrase phrase_finder{sents, importance};

    auto keywords = {"air", "China", "fire"};
    for(auto word : keywords){
        fmt::print("{} :\n", word);
        auto wuid = wordUIDs[word];
        auto usage = phrase_finder.usages(wuid, 5.0);
        auto& counts = usage.first;
        auto& reprs = usage.second;

        for(auto pair : counts){
            if(pair.second<2) continue;
//        fmt::print("{:<10} {:<10} {:<10} : ",
//                   score_phrase_count(pair), score_uids(pair.first), pair.second);
            fmt::print("{} : {}\n", pair.first.repr(wordUIDs), pair.second);
            for(auto& repr : reprs[pair.first]){
                fmt::print("  {} : {}\n", repr.first.repr(wordUIDs), repr.second);
            }
        }
        fmt::print("------------------------------------\n");
    }

}

void pos_info(engine::SubmoduleFactory const& factory){
    DepParsedTokens tokens = factory.dep_parsed_tokens();
    WordUIDindex wordUIDs  = factory.word_uid_index();
    POSUIDindex posUIDs    = factory.pos_uid_index();
    auto sents = tokens.IndexSentences();

    int i=0;
    for(auto sent : sents){
        if(++i>10) break;
        fmt::print("{}\n", sent.repr(wordUIDs));
        for(auto idx : sent){
            fmt::print("{}.{} ", wordUIDs[sent.dict->word_uid(idx)],
                                 posUIDs[sent.dict->pos(idx)]);
        }
        fmt::print("\n");
    }
}


void show_query_suggestion(engine::SubmoduleFactory const& factory,
                           std::string input){
    data::CoreNLPwebclient corenlp_client = factory.corenlp_webclient();
    auto query_str = util::string::read_whole(input);
    auto query_json = corenlp_client.from_query_content(query_str);
    data::CoreNLPjson query{query_json};

    WordUIDindex wordUIDs         = factory.word_uid_index();
    WordImportance importance     = factory.word_importance();

    DepParsedTokens tokens{};
    tokens.append_corenlp_output(query);
    tokens.build_sent_uid(0);
    auto sents = tokens.IndexSentences();
    PhraseSegmenter phrase_segmenter{importance};
    fmt::print(std::cerr, "{} {}\n", tokens.n_tokens(), sents.size());
    for (auto sent : sents) {
        fmt::print(std::cerr, "{}\n", sent.repr(wordUIDs));
        fmt::print(std::cerr, "-------------------------\n");
        auto phrases = phrase_segmenter.broke_into_phrases(sent, 5.0);
        for (auto phrase : phrases) {
            fmt::print(std::cerr, "{}\n", phrase.repr(wordUIDs));
        }
        fmt::print(std::cerr, "==============================================\n");
    }
}

void recover_wrong_case_query(engine::SubmoduleFactory const& factory){
    WordUIDindex wordUIDs          = factory.word_uid_index();
    WordImportance importance      = factory.word_importance();
    WordCaseCorrector did_you_mean = factory.word_case_corrector(importance);

    auto query_str = "china safety rohs afasrqwadfqf";
    auto expected_did_you_mean = "China safety RoHS afasrqwadfqf";
    auto query_words = util::string::split(query_str);
    auto corrected_words = util::map(query_words, [&did_you_mean](auto word){return did_you_mean.try_correct(word);});
    auto corrected_query = util::string::join(corrected_words, " ");
    fmt::print(std::cerr, "Original query : {}\n", query_str);
    fmt::print(std::cerr, "Did you mean   : {}\n", corrected_query);
    assert(corrected_query==expected_did_you_mean);
}

void test_all(int argc, char** argv){
    assert(argc>2);
    auto config = util::load_json(argv[1]);
    engine::SubmoduleFactory factory{{config}};
    dependency_graph();
    phrases_in_sentence();
    phrases_in_sentence(factory);
    dataset_indexing_quality(factory);
    phrase_stats(factory);
    pos_info(factory);
    show_query_suggestion(factory, argv[2]);
    recover_wrong_case_query(factory);
}

}//namespace wordrep::test
}//namespace wordrep

namespace engine{
namespace test{

void word_cache_thread_safety(util::json_t const& config) {
    engine::SubmoduleFactory factory{{config}};

    auto voca   = factory.voca_info();
    auto tokens = factory.dep_parsed_tokens();
    auto sents = tokens.IndexSentences();
    auto wordUIDs = factory.word_uid_index();

    wordrep::WordSimCache dists_cache{voca};
    auto dist_measure = similarity::Similarity<similarity::measure::angle>{};
    auto distance = [&dist_measure,&voca](auto vidx1, auto vidx2){
        return dist_measure(voca.wvecs[vidx1], voca.wvecs[vidx2]);};

    util::Timer timer;
    sents.resize(20000);
    auto n = sents.size();
    tbb::parallel_for(decltype(n){0}, n, [&](auto i) {
        auto& sent=sents[i];
        auto vidxs = util::map(sent, [&sent](auto idx){return sent.dict->word(idx);});
        dists_cache.cache(vidxs);
    });
    timer.here_then_reset(fmt::format("Cache {} sents. Cache size : {}", sents.size(), dists_cache.size()));
    auto cached_dist = dists_cache.get_cached_operator();
    tbb::parallel_for(decltype(n){0}, n, [&](auto i) {
        auto& sent=sents[i];
        wordrep::Words words = util::map(sent, [&sent](auto idx){return sent.dict->word_uid(idx);});
        //fmt::print("{}\n", words.repr(wordUIDs));;
        auto vidxs = util::map(sent, [&sent](auto idx){return sent.dict->word(idx);});
        for(size_t i=0; i!=vidxs.size()-1; ++i){
            auto vidx1=vidxs[i];
            auto vidx2=vidxs[i+1];
            //assert(dists_cache.distances(vidx1)[vidx2]==distance(vidx1,vidx2));
            assert(cached_dist(vidx1,vidx2)==distance(vidx1,vidx2));
        }
    });
    timer.here_then_reset(fmt::format("Multi-thread cache stress test : passed.", sents.size()));
}

void test_all(int argc, char** argv) {
    assert(argc > 1);
    auto config = util::load_json(argv[1]);
    word_cache_thread_safety(config);
}

}//namespace engine::test
}//namespace engine

void update_column(util::json_t const& config){
    auto file = util::io::h5rw_exist(util::get_str(config, "word_prob_dump"));
    auto dvals = file.getRawData<double>({"prob.ratio"});
    std::vector<float> fvals;
    for(auto x : dvals) fvals.push_back(x);
    util::PersistentVector<float,float> ratios{"prob.ratio", std::move(fvals)};
    ratios.write(file);
}


int main(int argc, char** argv){
//    wordrep::test::test_all(argc,argv);
//    engine::test::test_all(argc,argv);
//    return 0;

    assert(argc>2);
    auto config = util::load_json(argv[1]);
    engine::SubmoduleFactory factory{{config}};
    std::string input = argv[2];

    fmt::print(std::cerr, "Run nndep_reader with following config params:\n{}\n",config.dump(4));

    data::CoreNLPwebclient corenlp_client = factory.corenlp_webclient();
    auto raw_query_str = util::string::strip(util::string::read_whole(input));
    auto raw_query_json = corenlp_client.from_query_content(raw_query_str);
    raw_query_json["query_str"] = raw_query_str;

    util::Timer timer{};

    engine::QueryEngine engine{config};
    timer.here_then_reset("Data loaded.");

    auto corrected_query = engine.preprocess_query(raw_query_json);
    auto query_str = corrected_query["did_you_mean"];
    auto query_json = corenlp_client.from_query_content(query_str);
    query_json["query_str"] = query_str;
    fmt::print(std::cerr, "{}\n", corrected_query.dump(4));

    auto uids = engine.register_documents(query_json);
    uids["n_cut"]=10;
    uids["max_clip_len"] = query_json["max_clip_len"];
    if(false){
        //test YGP db column restriction.
        uids["confine_ygp_table_columns"].push_back("regulation.regtitle");
//    uids["confine_ygp_table_columns"].push_back("regulation.enregsummary");
//    uids["confine_ygp_table_columns"].push_back("regulation.enmainrequire");
        uids["confine_ygp_table_columns"].push_back("afasfdl14jh");
    }
    //fmt::print("{}\n", uids.dump(4));
    timer.here_then_reset("Registered documents.");
    auto answers = engine.ask_query(uids);
    timer.here_then_reset("Processed a query.");
    //fmt::print("{}\n", answers.dump(4));
    //fmt::print("\n\n---------------------\nA chain query find results:\n", answers.dump(4));
    timer.here_then_reset("Begin a chain query.");
    auto chain_answers = engine.ask_chain_query(uids);
    timer.here_then_reset("Processed a chain query.");
    engine.annotation_on_result(config, chain_answers);
    timer.here_then_reset("Annotate query output.");
    //fmt::print("chain_aswers:\n{}\n", chain_answers.dump(4));
    timer.here_then_reset("Ready to process a new query.");
    auto stat_answer = engine.ask_query_stats(uids);
    timer.here_then_reset("Processed a stats query.");
    engine.annotation_on_result(config, stat_answer["results"]);
    timer.here_then_reset("Annotate query output.");
    fmt::print("stats_answers:\n");
    fmt::print("{}\n", stat_answer.dump(4));

    timer.here_then_reset("Queries are answered.");

    return 0;
}
