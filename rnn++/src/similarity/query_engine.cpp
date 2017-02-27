#include <algorithm>
#include <map>

#include <fmt/printf.h>

#include "data_source/ygp_db.h"
#include "data_source/db.h"
#include "data_source/rss.h"

#include "similarity/query_types.h"
#include "similarity/query_engine.h"
#include "similarity/phrase_suggestion.h"
#include "similarity/similarity_measure.h"

#include "utils/parallel.h"
#include "utils/profiling.h"
#include "utils/span.h"
#include "utils/string.h"
#include "utils/algorithm.h"
#include "utils/hdf5.h"
#include "utils/math.h"
#include "utils/linear_algebra.h"
#include "utils/versioned_name.h"

using namespace wordrep;
using namespace util::io;
namespace ygp = data::ygp;

using util::json_t;
using util::Timer;
using data::PerSentQueryResult;
using data::DBIndexer;
using data::ColumnUID;

namespace {
using engine::ScoredSentence;

std::vector<ScoredSentence> deduplicate_results(tbb::concurrent_vector<ScoredSentence> const &relevant_sents){
    using hash_t = size_t;
    std::map<hash_t, bool> is_seen{};
    std::vector<ScoredSentence> dedup_sents;
    for(auto const &scored_sent : relevant_sents){
        auto sent = scored_sent.sent;
        hash_t hash(0);
        for(auto idx : sent){
            hash += std::hash<VocaIndex>{}(sent.dict->word(idx));
        }
        if(is_seen.find(hash)!=is_seen.cend()) continue;
        is_seen[hash]=true;
        dedup_sents.push_back(scored_sent);
    }
    return dedup_sents;
}

}//nameless namespace

namespace engine {

WordSimCache::WordSimCache(voca_info_t const &voca) : voca{voca} {
    auto n= voca.wvecs.size();
    data_t::accessor a;
    distance_caches.insert(a, wordrep::VocaIndex{});
    a->second = dist_cache_t{n};//For unknown word
}

bool WordSimCache::find(wordrep::VocaIndex idx) const{
    data_t::const_accessor a;
    return distance_caches.find(a, idx);
}

const WordSimCache::dist_cache_t& WordSimCache::try_find(wordrep::VocaIndex idx){
    if(!find(idx)) cache({idx});
    return distances(idx);
}
bool WordSimCache::insert(wordrep::VocaIndex idx, dist_cache_t const &dist){
    data_t::accessor a;
    distance_caches.find(a, idx);
    if(distance_caches.find(a, idx)) return false;
    distance_caches.insert(a, idx);
    a->second = dist;
    return true;
}
const WordSimCache::dist_cache_t& WordSimCache::distances(wordrep::VocaIndex widx) const {
    data_t::const_accessor a;
    bool is_exist=distance_caches.find(a,widx);
    //TODO:     make cache private method and remove this assert.
    if(!is_exist) assert(0);
    return a->second;
}
void WordSimCache::cache(std::vector<VocaIndex> const &words) {
    auto n= voca.wvecs.size();
    std::vector<VocaIndex> words_to_cache;
    std::vector<dist_cache_t> dists;
    for(auto vidx : words) {
        if(find(vidx)) continue;
        words_to_cache.push_back(vidx);
        dists.push_back(dist_cache_t{n});
    }
    auto n_words = dists.size();

    auto dist_measure = similarity::Similarity<similarity::measure::angle>{};
    tbb::parallel_for(tbb::blocked_range<decltype(n)>(0,n,10000),
                      [&](tbb::blocked_range<decltype(n)> const &r){
                          for(decltype(n) i=r.begin(); i!=r.end(); ++i){
                              for(decltype(n_words)j=0; j!=n_words; ++j ){
                                  auto qidx = words_to_cache[j];
                                  auto q = voca.wvecs[qidx];
                                  auto widx = VocaIndex::from_unsigned(i);
                                  dists[j][widx] = dist_measure(voca.wvecs[widx], q);
                              }
                          }
                      });

    for(decltype(n_words)i=0; i!=n_words; ++i){
        auto vidx=words_to_cache[i];
        insert(vidx,dists[i]);
    }
}

WordSimCache::val_t WordSimCache::max_similarity(wordrep::VocaIndex widx) const{
    if(!find(widx)) return 0.0;
    auto dists = distances(widx);
    auto beg = dists.val.begin();
    std::partial_sort(beg, beg+2, dists.val.end(), std::greater<val_t>{});
    auto it = beg+1;
    return *it;
}


void QueryResultCache::insert(wordrep::SentUID uid, result_t const&result) {
    data_t::accessor a;
    caches.insert(a, uid);
    a->second = result;
    std::cerr<<fmt::format("Insert {} to a cache", uid.val)<<std::endl;
}
QueryResultCache::result_t QueryResultCache::get(wordrep::SentUID uid) const {
    data_t::const_accessor a;
    if(caches.find(a, uid)) {
        std::cerr<<fmt::format("Cache hits: {}", uid.val)<<std::endl;
        return a->second;
    }
    std::cerr<<fmt::format("Cache misses: {}", uid.val)<<std::endl;
    return result_t{};
}
bool QueryResultCache::find(wordrep::SentUID uid) const {
    data_t::const_accessor a;
    std::cerr<<fmt::format("Look for a cache {} ", uid.val)<<std::endl;
    return caches.find(a, uid);
}

DepSearchScore::val_t DepSearchScore::score_sum() const {return util::math::sum(scores);}

////////////////////////////////

template<typename OPC, typename OPR>
std::vector<data::PerSentQueryResult> write_output(
        Sentence const &query_sent,
        std::vector<ScoredSentence> const &relevant_sents,
        OPC op_cut, OPR op_results) {
    auto n_found = relevant_sents.size();
    std::cerr<<n_found << " results are found"<<std::endl;

    util::Timer timer;
    auto top_N_results = op_cut(relevant_sents);
    timer.here_then_reset("Get top N results.");

    std::vector<data::PerSentQueryResult> results;
    for(auto const &scored_sent : top_N_results){
        auto result = op_results(query_sent, scored_sent);
        results.push_back(result);
    }

    timer.here_then_reset("Generate JSON output.");
    return results;
}

//TODO : separated out helper functions

util::json_t to_json(std::vector<PerSentQueryResult> const &results){
    util::json_t answer{};
    answer["score"]=util::json_t::array();
    answer["result_sent_country"]= util::json_t::array();
    answer["result_table_name"]  = util::json_t::array();
    answer["result_column_name"] = util::json_t::array();
    answer["result_index_col_name"]=util::json_t::array();
    answer["highlight_offset"]   = util::json_t::array();
    answer["clip_offset"]        = util::json_t::array();
    answer["score_with_offset"]  = util::json_t::array();

    answer["result_sent_uid"]    = util::json_t::array();
    answer["result_row_uid"]     = util::json_t::array();
    answer["result_row_idx"]     = util::json_t::array();
    answer["result_column_uid"]  = util::json_t::array();
    answer["result_offset"]      = util::json_t::array();

    for(auto const &result : results){
        answer["score"].push_back(result.score);
        answer["result_sent_country"].push_back(result.country);
        data::build_db_info_field(answer, result);
        answer["result_table_name"].push_back(result.table_name);
        answer["result_column_name"].push_back(result.column_name);
        answer["result_index_col_name"].push_back(result.index_col_name);
        auto tmp2 = result.highlight_offset;
        answer["highlight_offset"].push_back({tmp2.beg, tmp2.end});
        auto tmp3 = result.clip_offset;
        answer["clip_offset"].push_back({tmp3.beg, tmp3.end});

        util::json_t score_with_offset{};
        for(auto elm :result.scores_with_offset) {
            score_with_offset.push_back({elm.score,
                                         elm.query_token.beg, elm.query_token.end,
                                         elm.matched_token.beg, elm.matched_token.end});
        }
        answer["score_with_offset"].push_back(score_with_offset);
    }
    return answer;
}

void annotate_input_info(util::json_t &answer, data::QuerySentInfo const &info){
    answer["input_offset"]={info.offset.beg,info.offset.end};
    answer["input_uid"] = info.sent_uid;
    answer["cutoffs"] = info.cutoffs;
    answer["words"] = info.words;
}
util::json_t to_json(std::vector<data::QueryResult> const &answers){
    util::json_t output = util::json_t::array();

    for(auto &answer : answers){
        auto answer_json = to_json(answer.results);
        answer_json["n_relevant_matches"] = answer.n_relevant_matches;
        annotate_input_info(answer_json, answer.query);
        auto cutoff = util::math::sum(answer_json["cutoffs"].get<std::vector<double>>());
//        double cutoff{0.0};
//        for(double x : answer_json["cutoffs"]) cutoff+=x;
        for(double x : answer_json["score"]) answer_json["is_highly_meet"].push_back(x>cutoff);
        output.push_back(answer_json);
    }
    return output;
}

data::QuerySentInfo construct_query_info(
        Sentence query_sent, wordrep::WordUIDindex const& wordUIDs,
        wordrep::WordImportance const& word_importance) {
    data::QuerySentInfo info;
    for(auto idx : query_sent) {
        auto wuid = query_sent.dict->word_uid(idx);
        auto word = wordUIDs[wuid];
        info.words.push_back(word);
        auto cutoff = word_importance.score(wuid);
        info.cutoffs.push_back(cutoff);
    }
    info.sent_uid = query_sent.uid.val;
    info.offset.beg = query_sent.dict->word_beg(query_sent.front()).val;
    info.offset.end = query_sent.dict->word_end(query_sent.back()).val;
    return info;
}

void cache_words(Sentence const &sent, WordSimCache &dists_cache) {
    std::vector<VocaIndex> vidxs;
    for(auto idx : sent) {
        auto vuid=sent.dict->word(idx);
        vidxs.push_back(vuid);
    }
    dists_cache.cache(vidxs);
}
////////////////////////////////////////////////////////////////

struct ProcessQuerySent{
    using val_t = WordSimCache::val_t;
    ProcessQuerySent(WordSimCache &dists_cache,
                     wikidata::EntityModule const& wiki,
                     wordrep::Scoring const& scoring)
            : dists_cache{dists_cache}, wiki{wiki},scoring{scoring}
    {}

    std::vector<ScoredSentence> operator()(Sentence query_sent,
                                           std::vector<val_t> const& /*cutoffs*/,
                                           std::vector<Sentence> const& data_sents) {
        auto op_similarity = dists_cache.get_cached_operator();
        auto vidxs = util::map(query_sent, [&query_sent](auto idx){return query_sent.dict->word(idx);});
        op_similarity.build_lookup_cache(vidxs);


        auto tagged_query_sent = wiki.annotator.annotate(query_sent);
        Scoring::Preprocess scoring_preprocessor{scoring, wiki.entity_reprs, wiki.op_named_entity};
        auto query_sent_to_scored = scoring_preprocessor.sentence(tagged_query_sent);
        query_sent_to_scored.filter_false_named_entity(wiki.posUIDs);
        auto named_entities = query_sent_to_scored.all_named_entities();
        if(named_entities.empty()) return {};
        fmt::print(std::cerr, "{} : {} named entities\n", query_sent.repr(wiki.wordUIDs), named_entities.size());
        for(auto& e : named_entities) {
            fmt::print(std::cerr, "NAMED ENTITY IN QUERY: ");
            for (auto uid : e.candidates) {
                auto entity = wiki.entity_reprs[uid];
                fmt::print(std::cerr, "({}) ", entity.repr(wiki.entityUIDs, wiki.wordUIDs));
            }
            fmt::print(std::cerr, "\n");
        }
        auto op_ne = wiki.entity_reprs.get_comparison_operator(named_entities);
        auto op_query_similarity = scoring.op_sentence_similarity(query_sent_to_scored);
        tbb::concurrent_vector<ScoredSentence> relevant_sents{};
        auto n = data_sents.size();
        tbb::parallel_for(decltype(n){0}, n, [&,this](auto i) {
            auto sent = data_sents[i];
            if(!op_ne.isin(sent)) return;
            auto tagged_sent = wiki.annotator.annotate(sent);
            auto sent_to_scored = scoring_preprocessor.sentence(tagged_sent);
            auto scored_sent = op_query_similarity.score(sent_to_scored);
            relevant_sents.push_back(output(scored_sent));
        });
        return deduplicate_results(relevant_sents);
    }
    WordSimCache& dists_cache;
    wikidata::EntityModule const& wiki;
    wordrep::Scoring const& scoring;
};


struct ProcessQuerySents{
    ProcessQuerySents(WordSimCache& dists_cache,
                      wikidata::EntityModule const& wiki,
                      wordrep::Scoring const& scoring)
            : processor{dists_cache,wiki,scoring}
    {}

    template<typename OP>
    void operator()(std::vector<SentenceQuery> const &query_sents,
                    std::vector<wordrep::Sentence> const &candidate_sents,
                    OP const &op_per_sent) {
        util::Timer timer{};
        tbb::task_group g;
        for(auto const &query : query_sents){
            if(query.sent.empty()) continue;
            g.run([&timer,query,&op_per_sent,&candidate_sents, this](){
                std::cerr<<fmt::format("Query : Find with {} candidate sentences.",candidate_sents.size())<<std::endl;
                auto relevant_sents = processor(query.sent, query.info.cutoffs, candidate_sents);
                op_per_sent(query.sent, query.info, relevant_sents);
            });
        }
        timer.here_then_reset("All sentences in QuerySents are processed.");
        g.wait();
    }

    ProcessQuerySent processor;
};


struct ProcessChainQuery{
    ProcessChainQuery(wordrep::Sentences const &uid2sent,
                      WordSimCache& dists_cache,
                      wikidata::EntityModule const& wiki,
                      wordrep::Scoring const& scoring)
            : uid2sent{uid2sent}, processor{dists_cache,wiki,scoring}
    {}

    template<typename OP>
    void operator()(std::vector<SentenceQuery> const &query_chain,
                    std::vector<wordrep::Sentence> candidate_sents,
                    OP const &op_per_sent) {
        util::Timer timer{};
        for(auto const &query : query_chain){
            auto relevant_sents = processor(query.sent, query.info.cutoffs, candidate_sents);//util::deserialize<val_t>(info.cutoffs)

            candidate_sents.clear();
            assert(candidate_sents.size()==0);

            op_per_sent(query.sent, query.info, relevant_sents);
            timer.here_then_reset("One pass in a query chain is finished.");

            if(!relevant_sents.size()) continue;
            auto best_candidate = std::max_element(relevant_sents.cbegin(), relevant_sents.cend(),
                                                   [](auto x, auto y){return x.score<y.score;});
            auto score_cutoff = best_candidate->score * 0.5;
            for(auto scored_sent : relevant_sents){
                if(scored_sent.score < score_cutoff) continue;
                auto sent = scored_sent.sent;
                //TODO: release following assumption that candidate_sents are only from dataset, not queries_sents.
                //TODO: fix inefficienty; collecting all uids first.
                auto uids = sent.dict->sentences_in_chunk(sent);
                for(auto uid : uids) candidate_sents.push_back(uid2sent[uid]);
                //std::cerr<<fmt::format("UID : {} : {} of {}", sent.uid.val, uids.front().val, uids.back().val)<<std::endl;
                assert(uids.cend()!=std::find(uids.cbegin(), uids.cend(), sent.uid));
                assert(uid2sent[sent.uid].uid == sent.uid);
            }
            timer.here_then_reset("Prepared next pass in a query chain.");
        }
        timer.here_then_reset("All sentences in ChainQuery are processed.");
    }

    wordrep::Sentences const& uid2sent;
    ProcessQuerySent processor;
};

///////////////////////////////////////////////////////////////
template<typename T>
QueryEngineT<T>::QueryEngineT(typename T::factory_t const &factory)
: word_importance{factory.common.word_importance()},
  did_you_mean{factory.common.word_case_corrector(word_importance)},
  phrase_segmenter{word_importance},
  db{factory.common.load_dataset()},
  dbinfo{factory},
  queries{factory.common.empty_dataset()},
  wiki{factory.common.wikientity_module()},
  scoring{word_importance,db.voca.wvecs},
  scoring_preprocessor{scoring, wiki.entity_reprs, wiki.op_named_entity},
  dists_cache{db.voca}
{}

template<typename T>
QueryEngineT<T>::QueryEngineT(json_t const &config)
        : QueryEngineT<T>{typename T::factory_t{{config}}}
{}

template<typename T>
QueryEngineT<T>::QueryEngineT(QueryEngineT&& engine)
: word_importance{std::move(engine.word_importance)},
  did_you_mean{std::move(engine.did_you_mean)},
  phrase_segmenter{word_importance},
  db{std::move(engine.db)},
  dbinfo{std::move(engine.dbinfo)},
  queries{std::move(engine.queries)},
  wiki{std::move(engine.wiki)},
  scoring{word_importance,db.voca.wvecs},
  scoring_preprocessor{scoring, wiki.entity_reprs, wiki.op_named_entity},
  dists_cache{db.voca}
{}

template<typename T>
json_t QueryEngineT<T>::preprocess_query(json_t const &ask) const {
    if (ask.find("sentences") == ask.end()) return json_t{};
    data::CoreNLPjson query{ask};

    std::vector<std::string> original_words;
    auto per_tokens = [&original_words](auto const &token){
        original_words.push_back(util::get_str(token,"originalText"));
    };
    query.iter_tokens(per_tokens);
    auto original_query = util::string::join(original_words, " ");
    auto corrected_words = util::map(original_words, [this](auto word){return did_you_mean.try_correct(word);});
    auto corrected_query = util::string::join(corrected_words, " ");

    json_t answer{};
    answer["received_query"]=original_query;
    answer["did_you_mean"]=corrected_query;
    answer["word_pair"] = util::json_t::array();
    for(auto pair : util::zip(original_words, corrected_words)){
        if(pair.first==pair.second) continue;
        answer["word_pair"].push_back({pair.first, pair.second});
    }
    return answer;
}

template<typename T>
json_t QueryEngineT<T>::register_documents(json_t const &ask) {
    if (ask.find("sentences") == ask.end()) return json_t{};
    ;
    auto uids = queries.append_chunk(data::CoreNLPjson{ask});
    json_t answer{};
    answer["sent_uids"]=util::serialize(uids);
    std::cerr<<fmt::format("# of sents : {}\n", uids.size()) << std::endl;
    dbinfo.tag_on_register_documents(ask, answer);
    return answer;
}

template<typename T>
json_t QueryEngineT<T>::ask_query(json_t const &ask) const {
    if (!dbinfo_t::query_t::is_valid(ask)) return json_t{};
    typename dbinfo_t::query_t query{ask};
    auto max_clip_len = util::find<int64_t>(ask, "max_clip_len").value_or(200);
    auto n_cut = util::find<int64_t>(ask, "n_cut").value_or(5);

    auto query_sents = dbinfo.get_query_sents(query, queries.uid2sent, db.uid2sent);
    auto queries = util::map(query_sents, [this](auto sent)->SentenceQuery{
        return {sent, construct_query_info(sent, db.token2uid.word, word_importance)};
    });

    auto candidate_sents = dbinfo.get_candidate_sents(query, db);
    fmt::print(std::cerr, "Find among {} sents\n", candidate_sents.size());

    ProcessQuerySents query_processor{dists_cache, wiki, scoring};
    util::ConcurrentVector<data::QueryResult> answers;
    auto op_cut =[this,n_cut](auto const& xs){return dbinfo.rank_cut(xs,n_cut);};
    auto op_results = [this,max_clip_len](auto const& query_sent, auto const& scored_sent){
        return dbinfo.build_result(query_sent, scored_sent, max_clip_len);
    };
    auto per_sent=[&answers,max_clip_len,op_cut,op_results](
            auto const &query_sent, auto const& query_sent_info, auto const &relevant_sents){
        data::QueryResult answer;
        answer.results = write_output(query_sent, relevant_sents, op_cut, op_results);
        answer.query = query_sent_info;
        answer.n_relevant_matches = relevant_sents.size();
        answers.push_back(answer);
    };
    query_processor(queries, candidate_sents, per_sent);
    return to_json(answers.to_vector());
}

template<typename T>
json_t QueryEngineT<T>::ask_chain_query(json_t const &ask) const {
    if (!dbinfo_t::query_t::is_valid(ask)) return json_t{};
    typename  dbinfo_t::query_t query{ask};
    auto max_clip_len = util::find<int64_t>(ask, "max_clip_len").value_or(200);
    auto n_cut = util::find<int64_t>(ask, "n_cut").value_or(5);

    auto query_sents = dbinfo.get_query_sents(query, queries.uid2sent, db.uid2sent);
    auto queries = util::map(query_sents, [this](auto sent)->SentenceQuery{
        return {sent, construct_query_info(sent, db.token2uid.word, word_importance)};
    });

    auto candidate_sents = dbinfo.get_candidate_sents(query, db);
    fmt::print(std::cerr, "Find among {} sents\n", candidate_sents.size());
    auto maybe_sents = util::find<std::vector<int64_t>>(ask, "sents");
    if(maybe_sents) {
        auto uids = util::deserialize<SentUID>(maybe_sents.value());
        std::vector<Sentence> custom_sents;
        for(auto uid : uids) custom_sents.push_back(db.uid2sent.find(uid).value());
        candidate_sents = custom_sents;
    }

    output_t answers{};
    auto op_cut =[this,n_cut](auto const& xs){return dbinfo.rank_cut(xs,n_cut);};
    auto op_results = [this,max_clip_len](auto const& query_sent, auto const& scored_sent){
        return dbinfo.build_result(query_sent, scored_sent, max_clip_len);
    };
    auto per_sent = [this,&answers,max_clip_len,op_cut,op_results](
            auto const &query_sent, auto const &query_sent_info, auto const &relevant_sents){
        for(auto pair : util::zip(query_sent_info.words, query_sent_info.cutoffs)) {
            fmt::print(std::cerr, "{} : {}\n", pair.first, pair.second);
        }
        std::cerr<<std::endl;

        data::QueryResult answer;
        answer.results = write_output(query_sent, relevant_sents, op_cut, op_results);
        answer.query = query_sent_info;
        answer.n_relevant_matches = relevant_sents.size();
        answers.push_back(answer);
    };

    ProcessChainQuery processor{db.uid2sent, dists_cache, wiki, scoring};
    processor(queries, candidate_sents, per_sent);

    return to_json(answers);
}

template<typename T>
json_t QueryEngineT<T>::ask_query_stats(json_t const &ask) const {
    std::cerr<<fmt::format("{}\n", ask.dump(4))<<std::endl;
    if (!dbinfo_t::query_t::is_valid(ask)) return json_t{};
    typename dbinfo_t::query_t query{ask};
    auto max_clip_len = util::find<int64_t>(ask, "max_clip_len").value_or(200);
    auto n_cut = util::find<int64_t>(ask, "n_cut").value_or(5);
    auto phrase_cutoff = util::find<float>(ask, "phrase_cutoff").value_or(5.0);

    auto query_sents = dbinfo.get_query_sents(query, queries.uid2sent, db.uid2sent);
    auto queries = util::map(query_sents, [this](auto sent)->SentenceQuery{
        return {sent, construct_query_info(sent, db.token2uid.word, word_importance)};
    });
    auto candidate_sents = dbinfo.get_candidate_sents(query, db);

    std::map<SentUID, std::map<WordUID,std::map<WordUID,std::vector<SentUID>>>> results_by_match;
    std::map<SentUID, std::map<WordUID,std::map<WordUID,std::size_t>>> stats;
    auto collect_result_stats = [&results_by_match,&stats](auto const &/*query_sent*/, auto const &, auto const &/*relevant_sents*/){
//TODO: Templorarily disable this. Make it phrase based.
//        auto sent_uid = query_sent.uid;
//        for(auto const &scored_sent : relevant_sents){
//            for(auto elm : scored_sent.scores.serialize()){
//                auto qidx = std::get<0>(elm);
//                auto midx = std::get<1>(elm);
//                auto quid = query_sent.dict->word_uid(qidx);
//                auto muid = scored_sent.sent.dict->word_uid(midx);
//                auto score = std::get<2>(elm);
//                if(score<0.6) continue;
//                ++stats[sent_uid][quid][muid];
//                results_by_match[sent_uid][quid][muid].push_back(scored_sent.sent.uid);
//            }
//        }
    };
    output_t answers{};
    auto op_cut =[this,n_cut](auto const& xs){return dbinfo.rank_cut(xs,n_cut);};
    auto op_results = [this,max_clip_len](auto const& query_sent, auto const& scored_sent){
        return dbinfo.build_result(query_sent, scored_sent, max_clip_len);
    };
    auto collect_query_result = [&answers,max_clip_len,op_cut,op_results](
            auto const &query_sent, auto const &query_sent_info, auto const &relevant_sents){
        for(auto pair : util::zip(query_sent_info.words, query_sent_info.cutoffs)) {
            fmt::print(std::cerr, "{} : {}\n", pair.first, pair.second);
        }
        std::cerr<<std::endl;

        data::QueryResult answer;
        answer.results = write_output(query_sent, relevant_sents, op_cut, op_results);
        answer.query = query_sent_info;
        answer.n_relevant_matches = relevant_sents.size();
        answers.push_back(answer);
    };
    json_t query_suggestions = json_t::array();
    auto get_query_suggestions = [this,&query_suggestions,phrase_cutoff](auto const &query_sent, auto const &relevant_sents){
        auto sents = util::map(relevant_sents, [](auto const& r_sent){return r_sent.sent;});
        fmt::print(std::cerr, "Find phrase suggestions in {} sentences.", sents.size());
        WordUsageInPhrase phrase_finder{sents, word_importance};
        std::vector<WordUID> wuids;
        for(auto idx : query_sent) {
            auto wuid = query_sent.dict->word_uid(idx);
            if (word_importance.is_noisy_word(wuid)) continue;
            wuids.push_back(wuid);
        }
        json_t query_suggestion;
        query_suggestion["query_suggestions"]= get_query_suggestion(wuids, phrase_finder, db.token2uid.word, phrase_cutoff);
        query_suggestion["sent_uid"] = query_sent.uid.val;
        query_suggestions.push_back(query_suggestion);
    };
    auto op_per_sent=[collect_result_stats,collect_query_result,get_query_suggestions](
            auto const &query_sent, auto const &query_sent_info, auto const &relevant_sents){
        collect_result_stats(query_sent,query_sent_info, relevant_sents);
        collect_query_result(query_sent,query_sent_info, relevant_sents);
        get_query_suggestions(query_sent, relevant_sents);
    };

    ProcessChainQuery processor{db.uid2sent, dists_cache, wiki, scoring};
    processor(queries, candidate_sents, op_per_sent);

    util::json_t stats_output = util::json_t::array();
    util::json_t stats_output_idxs = util::json_t::array();
    util::Timer timer;
    for(auto per_sent_stats : stats) {
        auto sent_uid = per_sent_stats.first;
//        fmt::print(std::cerr, "For query sent {}:\n", sent_uid.val);

        util::json_t stats_output_per_sent;
        stats_output_idxs.push_back(sent_uid.val);
        for (auto pair : per_sent_stats.second) {
            util::json_t per_qword{};
            auto quid = pair.first;
            for (auto elm : pair.second) {
                auto muid = elm.first;
                for (auto uid : results_by_match[sent_uid][quid][muid]) per_qword[db.token2uid.word[muid]].push_back(uid.val);
//                fmt::print(std::cerr, "{:<15} {:<15} : {:<15}\n",
//                           db.token2uid.word[quid], db.token2uid.word[muid], elm.second);
            }
            stats_output_per_sent[db.token2uid.word[quid]] = per_qword;
//            fmt::print(std::cerr, "------------------\n");
        }
//        fmt::print(std::cerr, "==================\n");
        stats_output.push_back(stats_output_per_sent);
    }
//    fmt::print(std::cerr, "//////////////////////////////////////////////////\n");
    timer.here_then_reset("Result stats\n");

    util::json_t output{};
    util::json_t results = to_json(answers);
    output["results"] = results;
    output["stats"]=stats_output;
    output["stats_uid"] = stats_output_idxs;
    output["query_suggestions_per_sent"] = query_suggestions;
    return output;
}

template<typename T>
json_t QueryEngineT<T>::ask_sents_content(json_t const &ask) const{
    json_t output{};
    for(int64_t uid : ask["sents"]) {
        //TODO: do not assume uid is from db. It may come from queries.
        auto sent = db.uid2sent[SentUID{uid}];
        auto chunk_idx = db.tokens.chunk_idx(sent.front());
        auto col_uid = dbinfo.indexer.column_uid(chunk_idx);
        auto row_idx = dbinfo.indexer.row_idx(chunk_idx);

        data::rss::HashIndexer hash2idx{"/home/jihuni/word2vec/nyt/nyt.raw"};
        auto hash = hash2idx.hash(data::rss::HashIndex{row_idx.val});
        auto column = dbinfo.db.column(col_uid);

        auto offset_beg = sent.beg_offset().val;
        auto offset_end = sent.end_offset().val;

        auto row_str = util::string::read_whole(fmt::format("/home/jihuni/word2vec/parsed/{}.{}", hash, column));
        auto substr = util::string::substring_unicode_offset(row_str, offset_beg, offset_end);
        output["sents"].push_back(substr);
    };
    return output;

}

template<typename T>
json_t QueryEngineT<T>::ask_query_suggestion(json_t const &ask) const{
    auto phrase_cutoff = util::find<float>(ask, "phrase_cutoff").value_or(5.0);
    fmt::print(std::cerr, "{} cutoff phrase.\n", phrase_cutoff);
    WordUsageInPhrase phrase_finder{db.sents, word_importance};
    std::vector<WordUID> wuids;
    for(std::string word : ask["ideas"]) {
        auto wuid = db.token2uid.word[word];
        if (word_importance.is_noisy_word(wuid)) continue;
        wuids.push_back(wuid);
    }

    json_t output;
    output["query_suggestions"]= get_query_suggestion(wuids, phrase_finder, db.token2uid.word, phrase_cutoff);
    return output;
}


class UnknownQueryEngineException: public std::exception {
    virtual const char* what() const throw() {
        return "Missing or unknown query engine type.";
    }
};

mapbox::util::variant<RSSQueryEngine,YGPQueryEngine> load_query_engine(util::json_t config){
    if(util::get_str(config,"engine_type")=="ygp")
        return YGPQueryEngine{config};
    if(util::get_str(config,"engine_type")!="rss") throw UnknownQueryEngineException{};
    return RSSQueryEngine{config};
};

//Explicit instantiation of query engines.
template class engine::QueryEngineT<data::rss::DBInfo>;
template class engine::QueryEngineT<data::ygp::DBInfo>;

QueryEngine::QueryEngine(util::json_t& config)
: engine{load_query_engine(config)}
{}

}//namespace engine
