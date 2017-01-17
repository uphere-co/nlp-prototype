#include <memory>
#include <fmt/printf.h>

#include "wordrep/dep_graph.h"

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
    POSUIDindex const posUIDs{"../rnn++/tests/data/poss.uid"};
    ArcLabelUIDindex const arclabelUIDs{"../rnn++/tests/data/dep.uid"};
    WordImportance importance{"../rnn++/tests/data/word_importance",
                              "../rnn++/tests/data/words.uid"};

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
    WordImportance importance{"../rnn++/tests/data/word_importance",
                              "../rnn++/tests/data/words.uid"};

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
            fmt::print(std::cerr, "{}\n", phrase.repr(wordUIDs));
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


void dataset_indexing_quality(util::json_t const& config){
    using util::io::h5read;
    fmt::print(std::cerr, "Read {}\n",
               util::get_latest_version(util::get_str(config, "dep_parsed_store")).fullname);
    DepParsedTokens tokens{util::get_latest_version(util::get_str(config, "dep_parsed_store")),
                           config["dep_parsed_prefix"]};
    WordUIDindex wordUIDs{util::get_str(config,"word_uids_dump")};

    VocaInfo voca{config["wordvec_store"], config["voca_name"],
         config["w2vmodel_name"], config["w2v_float_t"]};
    WordImportance importance{h5read(util::get_str(config,"word_prob_dump"))};
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



void phrase_stats(util::json_t const& config){
    using util::io::h5read;
    fmt::print(std::cerr, "Read {}\n",
               util::get_latest_version(util::get_str(config, "dep_parsed_store")).fullname);
    DepParsedTokens tokens{util::get_latest_version(util::get_str(config, "dep_parsed_store")),
                           config["dep_parsed_prefix"]};
    WordUIDindex wordUIDs{util::get_str(config,"word_uids_dump")};

    VocaInfo voca{config["wordvec_store"], config["voca_name"],
                  config["w2vmodel_name"], config["w2v_float_t"]};
    WordImportance importance{h5read(util::get_str(config,"word_prob_dump"))};

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

void pos_info(util::json_t const& config){
    using util::io::h5read;
    fmt::print(std::cerr, "Read {}\n",
               util::get_latest_version(util::get_str(config, "dep_parsed_store")).fullname);
    DepParsedTokens tokens{util::get_latest_version(util::get_str(config, "dep_parsed_store")),
                           config["dep_parsed_prefix"]};
    WordUIDindex wordUIDs{util::get_str(config,"word_uids_dump")};
    POSUIDindex posUIDs{util::get_str(config,"pos_uids_dump")};

    auto sents = tokens.IndexSentences();
    int i=0;
    for(auto sent : sents){
        if(++i>10) break;
        fmt::print("{}\n", sent.repr(wordUIDs));
        for(auto idx : sent){
            fmt::print("{}.{} ", wordUIDs[sent.tokens->word_uid(idx)],
                                 posUIDs[sent.tokens->pos(idx)]);
        }
        fmt::print("\n");
    }
}

void unknown_word_importance(util::json_t const& config){
    WordUIDindex wordUIDs{util::get_str(config,"word_uids_dump")};

    VocaInfo voca{config["wordvec_store"], config["voca_name"],
                  config["w2vmodel_name"], config["w2v_float_t"]};
    WordImportance importance{util::io::h5read(util::get_str(config,"word_prob_dump"))};

    assert(importance.score(wordrep::the_unknown_word_uid())==0.0);
}

struct CaseCount{
    using float_t = float;
    int64_t both{0};
    int64_t summary{0};
    int64_t full{0};
    float ratio{0};
};

struct ColumnPair{
    data::ColumnUID summary;
    data::ColumnUID full;
};

void accum_word_importance_count(util::json_t const& config,
                                 std::vector<ColumnPair> const& column_pairs,
                                 std::map<WordUID,CaseCount>& cases,
                                 int64_t& n_case){
    using util::io::h5read;
    fmt::print(std::cerr, "Read {}\n",
               util::get_latest_version(util::get_str(config, "dep_parsed_store")).fullname);

    util::Timer timer;
    auto file = h5read(util::get_latest_version(util::get_str(config, "dep_parsed_store")).fullname);
    std::string prefix = config["dep_parsed_prefix"];

    util::TypedPersistentVector<ChunkIndex> chunks_idx{file,prefix+".chunk_idx"};
    util::TypedPersistentVector<WordUID>    words_uid {file,prefix+".word_uid"};
    WordUIDindex wordUIDs{util::get_str(config,"word_uids_dump")};

    data::DBIndexer indexer{h5read(util::get_latest_version(util::get_str(config, "dep_parsed_store")).fullname),
                            config["dep_parsed_prefix"].get<std::string>()};
    timer.here_then_reset("Data loaded.");

    std::map<ChunkIndex, std::vector<WordUID>> chunks;

    auto beg_idx = chunks_idx.cbegin();
    auto end_idx = chunks_idx.end();
    auto beg_word = words_uid.cbegin();
    auto chunk_beg = beg_idx;
    while(chunk_beg!=end_idx){
        auto chunk_end = std::find_if_not(chunk_beg, end_idx, [chunk_beg](auto x){return *chunk_beg==x;});
        for(auto it=beg_word+std::distance(beg_idx, chunk_beg);
            it!=beg_word+std::distance(beg_idx, chunk_end); ++it) chunks[*chunk_beg].push_back(*it);
        chunk_beg=chunk_end;
    }

    using data::ColumnUID;
    using data::RowIndex;
    std::map<ColumnUID, std::map<RowIndex,ChunkIndex>> index_map;
    for(auto const& chunk : chunks){
        auto chunk_idx  = chunk.first;
        auto column_uid = indexer.column_uid(chunk_idx);
        auto row_idx    = indexer.row_idx(chunk_idx);
        index_map[column_uid][row_idx]=chunk_idx;
    }
    for(auto elm : index_map){
        fmt::print(std::cerr, "{} : {} chunks.\n", elm.first, elm.second.size());
    }

    for(auto pair : column_pairs){
        auto rows_full = index_map[pair.full];
        auto rows_summary = index_map[pair.summary];
        assert(index_map.find(pair.full)!=index_map.end());
        assert(index_map.find(pair.summary)!=index_map.end());

        for(auto elm : rows_summary){
            auto row_idx = elm.first;
            if(rows_full.find(row_idx)==rows_full.end()) continue;
            ++n_case;
            auto summary_chunk = rows_summary[row_idx];
            auto full_chunk = rows_full[row_idx];
            assert(rows_summary.find(row_idx)!=rows_summary.end());
            assert(rows_full.find(row_idx)!=rows_full.end());

            auto words_in_summary = util::unique_values(chunks[summary_chunk]);
            auto words_in_full    = util::unique_values(chunks[full_chunk]);

            for(auto idx : words_in_summary){
                cases[idx].summary +=1;
                if(util::isin(words_in_full, idx)) cases[idx].both +=1;
            }
            for(auto idx : words_in_full) cases[idx].full +=1;

            if(false){
                if(n_case>10) continue;
                fmt::print("RowIndex {}:\n", row_idx);
                fmt::print("summary: {}\n", summary_chunk);
                assert(chunks.find(summary_chunk)!=chunks.end());
                assert(chunks.find(full_chunk)!=chunks.end());
                for(auto idx : chunks[summary_chunk]) fmt::print("{} ", wordUIDs[idx]);
                fmt::print("\n");
                fmt::print("full: {}\n", full_chunk);
                for(auto idx : chunks[full_chunk]) fmt::print("{} ", wordUIDs[idx]);
                fmt::print("\n");
            }
        }
    }
}
void build_word_importance(){
    auto ygp_config = util::load_json("config.ygp.json");
    std::vector<ColumnPair> ygp_column_pairs = {{1,0},{2,1},{2,0}, {3,4},{3,5}, {4,5}};//for YGP
    auto rss_config = util::load_json("config.nyt.json");
    std::vector<ColumnPair> rss_column_pairs = {{1,2}};//for RSS. 0 is title and all capital.

    WordUIDindex wordUIDs{util::get_str(rss_config,"word_uids_dump")};

    int64_t n_case=0;
    std::map<WordUID,CaseCount> cases;
    accum_word_importance_count(ygp_config, ygp_column_pairs, cases, n_case);
    accum_word_importance_count(rss_config, rss_column_pairs, cases, n_case);

    auto ratio_per_uids = util::to_pairs(cases);
    auto norm_factor = 1.0/n_case;
    for(auto& elm : ratio_per_uids) {
        auto& x = elm.second;
        if(x.summary<5 || x.full<5)
            x.ratio=0.0;
        else
            x.ratio = x.both/(norm_factor*x.summary*x.full);
    }
    util::sort(ratio_per_uids, [](auto x, auto y){return x.second.ratio>y.second.ratio;});
    for(auto x : ratio_per_uids){
        fmt::print("{:<15}\t{:<5}\t{}\t{}\t{}\n", wordUIDs[x.first], x.second.ratio,
                    x.second.full, x.second.summary, x.second.both);
    }
    util::TypedPersistentVector<WordUID> uids{"prob.word_uid", util::map(ratio_per_uids, [](auto x){return x.first;})};
    util::PersistentVector<float,CaseCount::float_t> ratios{"prob.ratio", util::map(ratio_per_uids, [](auto x){return x.second.ratio;})};

    auto output = util::io::h5replace("prob.h5");
    uids.write(output);
    ratios.write(output);
    return;
}

void show_old_foramt_word_importance(util::json_t const& config){
    auto file = util::io::h5read(util::get_str(config, "word_prob_dump"));
    util::TypedPersistentVector<WordUID> uids{file,"prob.word_uid"};
    //util::PersistentVector<double,double> ratios{file,"prob.ratio"};
    auto ratios = file.getRawData<double>({"prob.ratio"});
    auto p_main = file.getRawData<double>({"prob.main"});
    auto p_summary = file.getRawData<double>({"prob.summary"});
    auto p_both = file.getRawData<double>({"prob.both"});

    WordUIDindex wordUIDs{util::get_str(config,"word_uids_dump")};
    auto ratio_per_uids = util::zip(uids.get(),ratios);
    util::sort(ratio_per_uids, [](auto x, auto y){return x.second>y.second;});
    auto n= ratios.size();
    for(decltype(n)i=0; i!=n; ++i){
        fmt::print("{} : {} {} {} {}\n", wordUIDs[uids[i]], ratios[i], p_main[i], p_summary[i],p_both[i]);
    }
}

void show_query_suggestion(int argc, char** argv){
    assert(argc>2);
    auto config = util::load_json(argv[1]);
    std::string input = argv[2];
    data::CoreNLPwebclient corenlp_client{config["corenlp_client_script"].get<std::string>()};
    auto query_str = util::string::read_whole(input);
    auto query_json = corenlp_client.from_query_content(query_str);
    data::CoreNLPjson query{query_json};

    WordUIDindex wordUIDs{util::get_str(config,"word_uids_dump")};
    POSUIDindex posUIDs{util::get_str(config,"pos_uids_dump")};
    ArcLabelUIDindex arclabelUIDs{util::get_str(config,"arclabel_uids_dump")};
    WordImportance importance{util::io::h5read(util::get_str(config,"word_prob_dump"))};

    DepParsedTokens tokens{};
    tokens.append_corenlp_output(wordUIDs, posUIDs, arclabelUIDs, query);
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

void test_all(int argc, char** argv){
    assert(argc>1);
    auto config = util::load_json(argv[1]);
    dependency_graph();
    phrases_in_sentence();
    phrases_in_sentence(config);
    dataset_indexing_quality(config);
    phrase_stats(config);
    pos_info(config);
    unknown_word_importance(config);
    build_word_importance();
//    show_old_foramt_word_importance(config);
    show_query_suggestion(argc, argv);
}

}//namespace wordrep::test
}//namespace wordrep

namespace engine{
namespace test{

void word_cache_thread_safety(util::json_t const& config) {
    wordrep::VocaInfo voca{config["wordvec_store"], config["voca_name"],
                           config["w2vmodel_name"], config["w2v_float_t"]};
    wordrep::DepParsedTokens tokens{util::get_latest_version(util::get_str(config, "dep_parsed_store")), config["dep_parsed_prefix"]};
    auto sents = tokens.IndexSentences();
    wordrep::WordUIDindex wordUIDs{util::get_str(config,"word_uids_dump")};

    WordSimCache dists_cache{voca};
    auto dist_measure = similarity::Similarity<similarity::measure::angle>{};
    auto distance = [&dist_measure,&voca](auto vidx1, auto vidx2){
        return dist_measure(voca.wvecs[vidx1], voca.wvecs[vidx2]);};

    util::Timer timer;
    sents.resize(20000);
    auto n = sents.size();
    tbb::parallel_for(decltype(n){0}, n, [&](auto i) {
        auto& sent=sents[i];
        auto vidxs = util::map(sent, [&sent](auto idx){return sent.tokens->word(idx);});
        dists_cache.cache(vidxs);
    });
    timer.here_then_reset(fmt::format("Cache {} sents. Cache size : {}", sents.size(), dists_cache.size()));
    auto cached_dist = dists_cache.get_cached_operator();
    tbb::parallel_for(decltype(n){0}, n, [&](auto i) {
        auto& sent=sents[i];
        wordrep::Words words = util::map(sent, [&sent](auto idx){return sent.tokens->word_uid(idx);});
        //fmt::print("{}\n", words.repr(wordUIDs));;
        auto vidxs = util::map(sent, [&sent](auto idx){return sent.tokens->word(idx);});
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
    std::string input = argv[2];

//    update_column(config);
//    return 0;

    data::CoreNLPwebclient corenlp_client{config["corenlp_client_script"].get<std::string>()};
    auto query_str = util::string::read_whole(input);
    auto query_json = corenlp_client.from_query_content(query_str);
    query_json["query_str"] = query_str;

    util::Timer timer{};

    engine::QueryEngine engine{config};
    timer.here_then_reset("Data loaded.");


    if(true){
        util::json_t suggestion_query{};
//        auto ideas = {"China", "air", "fire","metal"};
        //auto ideas = {"Yahoo", "Google","China","AI"};
        auto ideas = util::string::split(util::string::strip(util::string::read_whole(input)));
        suggestion_query["ideas"]=ideas;
        fmt::print("{}\n", suggestion_query.dump(4));
        auto suggestion_output = engine.ask_query_suggestion(suggestion_query);
        fmt::print("{}\n", suggestion_output.dump(4));
    }

    auto uids = engine.register_documents(query_json);
    uids["max_clip_len"] = query_json["max_clip_len"];
    if(false){
        uids["confine_ygp_table_columns"].push_back("regulation.regtitle");
//    uids["confine_ygp_table_columns"].push_back("regulation.enregsummary");
//    uids["confine_ygp_table_columns"].push_back("regulation.enmainrequire");
        uids["confine_ygp_table_columns"].push_back("afasfdl14jh");
    }
    fmt::print("{}\n", uids.dump(4));
    timer.here_then_reset("Registered documents.");
    auto answers = engine.ask_query(uids);
    timer.here_then_reset("Processed a query.");
    fmt::print("{}\n", answers.dump(4));
    fmt::print("\n\n---------------------\nA chain query find results:\n", answers.dump(4));
    timer.here_then_reset("Begin a chain query.");
    auto chain_answers = engine.ask_chain_query(uids);
    timer.here_then_reset("Processed a chain query.");
    engine.annotation_on_result(config, chain_answers);
    timer.here_then_reset("Annotate query output.");
    fmt::print("chain_snaswers:\n{}\n", chain_answers.dump(4));
    timer.here_then_reset("Ready to process a new query.");
    auto stat_answer = engine.ask_query_stats(uids);
    timer.here_then_reset("Processed a stats query.");
    engine.annotation_on_result(config, stat_answer["results"]);
    timer.here_then_reset("Annotate query output.");
    fmt::print("stats_snaswers:\n{}\n", stat_answer.dump(4));
    if(false){
        util::json_t tmp;
        std::vector<int64_t> sents;
        for(auto& per_sent: stat_answer["stats"])
            for(auto& per_key : per_sent)
                for(auto& matches : per_key)
                    for(auto uid : matches)
                        sents.push_back(uid);
        tmp["sents"]=sents;
//    fmt::print("{}\n", tmp.dump(4));


//        auto custom_query = uids;
//        custom_query["sents"]=sents;
//        custom_query["n_cut"]=30;
//        fmt::print("{}\n", uids.dump(4));
//        fmt::print("{}\n", custom_query.dump(4));
//        auto chain_answers_custom = engine.ask_chain_query(custom_query);
//        data::ygp::annotation_on_result(config, chain_answers_custom);
//        fmt::print("{}\n", chain_answers_custom.dump(4));
////    auto content = engine.ask_sents_content(tmp);
//        //fmt::print("{}\n", content.dump(4));
    }
    timer.here_then_reset("Queries are answered.");

    return 0;
}

