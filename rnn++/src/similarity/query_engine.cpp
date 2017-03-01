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

#define BACKWARD_HAS_DW 1
#include <backward.hpp>
namespace backward {
backward::SignalHandling sh;
} // namespace backward

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
                                           PreprocessedSent const& data_sents) {
        util::Timer timer;
        auto op_word_sim = dists_cache.get_cached_operator();
        auto vidxs = util::map(query_sent, [&query_sent](auto idx){return query_sent.dict->word(idx);});
        timer.here_then_reset("Get voca indexes.");
        op_word_sim.build_lookup_cache(vidxs);
        timer.here_then_reset("Build word sim caches.");

        auto tagged_query_sent = wiki.annotator.annotate(query_sent);
        timer.here_then_reset("Annotate a query sentence.");
        Scoring::Preprocess scoring_preprocessor{scoring, wiki.entity_reprs};
        auto query_sent_to_scored = scoring_preprocessor.sentence(tagged_query_sent);
        query_sent_to_scored.filter_false_named_entity(wiki.op_named_entity, wiki.posUIDs);

        auto named_entities = query_sent_to_scored.all_named_entities();
        timer.here_then_reset("A query sentence is ready to be compared.");
        fmt::print(std::cerr, "{} : {} named entities\n", query_sent.repr(wiki.wordUIDs), named_entities.size());
        fmt::print(std::cerr, "WIKIDATA ENTITY IN QUERY:\n");
        for(auto& e : query_sent_to_scored.entities){
            fmt::print(std::cerr, "{} :", e.idxs.repr(*query_sent_to_scored.orig.dict, wiki.wordUIDs));
            for(auto uid : e.uid.candidates)
                fmt::print(std::cerr, " {}", wiki.entityUIDs[uid]);
            fmt::print(std::cerr, "\n");
        }
        fmt::print(std::cerr, "WORDS IN QUERY:\n");
        for(auto& e : query_sent_to_scored.words)
            fmt::print(std::cerr, "{}\n", e.repr(wiki.wordUIDs));

        //auto op_ne = wiki.entity_reprs.get_comparison_operator(named_entities);
        auto op_query_similarity = scoring.op_sentence_similarity(query_sent_to_scored);
//        auto op_query_similarity = scoring.op_sentence_similarity(query_sent_to_scored, op_word_sim);
        auto self_scored_sent = output(op_query_similarity.score(query_sent_to_scored));
        auto score_cut = self_scored_sent.score * 0.6;
        tbb::concurrent_vector<ScoredSentence> relevant_sents{};
        auto n = data_sents.size();
        tbb::parallel_for(decltype(n){0}, n, [&,this](auto i) {
            auto& sent_to_scored = data_sents.sents[i];
            //if(!op_ne.isin(sent_to_scored.orig)) return;
            auto scored_sent = output(op_query_similarity.score(sent_to_scored));
            //if(scored_sent.score>score_cut) relevant_sents.push_back((scored_sent));
            relevant_sents.push_back((scored_sent));
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
                    PreprocessedSent const &candidate_sents,
                    OP const &op_per_sent) {
        util::Timer timer{};
        tbb::task_group g;
        for(auto const &query : query_sents) {
            if(query.sent.empty()) continue;
            g.run([&timer,query,&op_per_sent,&candidate_sents, this](){
                std::cerr<<fmt::format("Query : Find with {} candidate sentences.",candidate_sents.size())<<std::endl;
                auto relevant_sents = processor(query.sent, query.info.cutoffs, candidate_sents);
                op_per_sent(query.sent, query.info, relevant_sents);
            });
        }
        g.wait();
        timer.here_then_reset("All sentences in QuerySents are processed.");
    }

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
  scoring_preprocessor{scoring, wiki.entity_reprs},
  data_sents{wiki, scoring, scoring_preprocessor, db.sents},
  dists_cache{db.voca}
{
    fmt::print(std::cerr, "Engine is constructed using factory.\n");
}

template<typename T>
QueryEngineT<T>::QueryEngineT(json_t const &config)
        : QueryEngineT<T>{typename T::factory_t{{config}}}
{
    fmt::print(std::cerr, "Engine is constructed.\n");
}

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
  scoring_preprocessor{scoring, wiki.entity_reprs},
  data_sents{wiki, scoring, scoring_preprocessor, db.sents},
  dists_cache{db.voca}
{
    fmt::print(std::cerr, "Engine is move constructed.\n");
}

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
    auto n_cut = util::find<int64_t>(ask, "n_cut").value_or(10);

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

    query_processor(queries, data_sents, per_sent);
    return to_json(answers.to_vector());
}

template<typename T>
json_t QueryEngineT<T>::ask_chain_query(json_t const &ask) const {
    return ask_query(ask);
}

template<typename T>
json_t QueryEngineT<T>::ask_query_stats(json_t const &ask) const {
    if (!dbinfo_t::query_t::is_valid(ask)) return json_t{};
    typename dbinfo_t::query_t query{ask};
    auto max_clip_len = util::find<int64_t>(ask, "max_clip_len").value_or(200);
    auto n_cut = util::find<int64_t>(ask, "n_cut").value_or(10);

    auto query_sents = dbinfo.get_query_sents(query, queries.uid2sent, db.uid2sent);
    auto queries = util::map(query_sents, [this](auto sent)->SentenceQuery{
        return {sent, construct_query_info(sent, db.token2uid.word, word_importance)};
    });

    auto candidate_sents = dbinfo.get_candidate_sents(query, db);
    fmt::print(std::cerr, "Find among {} sents\n", candidate_sents.size());

    auto phrase_cutoff = util::find<float>(ask, "phrase_cutoff").value_or(5.0);
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

    ProcessQuerySents query_processor{dists_cache, wiki, scoring};
    util::ConcurrentVector<data::QueryResult> answers;
    auto op_cut =[this,n_cut](auto const& xs){return dbinfo.rank_cut(xs,n_cut);};
    auto op_results = [this,max_clip_len](auto const& query_sent, auto const& scored_sent){
        return dbinfo.build_result(query_sent, scored_sent, max_clip_len);
    };
    auto per_sent=[&answers,max_clip_len,op_cut,op_results,get_query_suggestions](
            auto const &query_sent, auto const& query_sent_info, auto const &relevant_sents){
        data::QueryResult answer;
        answer.results = write_output(query_sent, relevant_sents, op_cut, op_results);
        answer.query = query_sent_info;
        answer.n_relevant_matches = relevant_sents.size();
        answers.push_back(answer);
        get_query_suggestions(query_sent, relevant_sents);
    };

    query_processor(queries, data_sents, per_sent);

    util::json_t out{};
    out["results"] = to_json(answers.to_vector());
    out["query_suggestions_per_sent"] = query_suggestions;
    return out;
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


template<typename T>
json_t QueryEngineT<T>::compare_sentences(json_t const &ask) const {
    if (!dbinfo_t::query_t::is_valid(ask)) return json_t{};
    typename dbinfo_t::query_t user_query{ask};

    auto query_sents = dbinfo.get_query_sents(user_query, queries.uid2sent, db.uid2sent);
    auto query = query_sents[0];
    auto sent  = query_sents[1];
    Scoring::Preprocess scoring_preprocessor{scoring, wiki.entity_reprs};

    util::Timer timer;
    fmt::print("Query : {}\n\n",query.repr(wiki.wordUIDs));
    auto tagged_query = wiki.annotator.annotate(query);
    fmt::print("Annoted Query : {}\n\n",tagged_query.repr(wiki.entity_reprs, wiki.entityUIDs, wiki.wordUIDs));
    auto query_to_scored = scoring_preprocessor.sentence(tagged_query);

    for(auto e : query_to_scored.entities)
        fmt::print(std::cerr, "{:<15} : Entity.\n", e.repr(*query_to_scored.orig.dict, wiki.wordUIDs));
    for(auto e : query_to_scored.words)
        fmt::print(std::cerr, "{:<15} : Word.\n", e.repr(wiki.wordUIDs));
    query_to_scored.filter_false_named_entity(wiki.op_named_entity, wiki.posUIDs);
    timer.here_then_reset("Annotate a query sentence.");

    auto tagged_sent = wiki.annotator.annotate(sent);
    auto sent_to_scored = scoring_preprocessor.sentence(tagged_sent);
    fmt::print("Annoted Sent : {}\n\n",tagged_sent.repr(wiki.entity_reprs, wiki.entityUIDs, wiki.wordUIDs));
    for(auto e : sent_to_scored.entities)
        fmt::print(std::cerr, "{:<15} : Entity.\n", e.repr(*query_to_scored.orig.dict, wiki.wordUIDs));
    for(auto e : sent_to_scored.words)
        fmt::print(std::cerr, "{:<15} : Word.\n", e.repr(wiki.wordUIDs));

    sent_to_scored.filter_false_named_entity(wiki.op_named_entity, wiki.posUIDs);
    timer.here_then_reset("Annotate a sentence.");

    auto op_query_similarity = scoring.op_sentence_similarity(query_to_scored);
    auto scored_sent = op_query_similarity.score(sent_to_scored);

    fmt::print(std::cerr, "\nEntities:\n");
    for(auto&e : scored_sent.entities){
        auto chunk = e.first;
        auto m_matched = e.second;
        if(!m_matched) {
            fmt::print(std::cerr, "{:<15} : No match.", chunk.repr(*scored_sent.orig.dict, wiki.wordUIDs));
            for(auto uid : chunk.uid.candidates)
                fmt::print(std::cerr, " {}", uid);
            fmt::print(std::cerr, " \n");
        }
        else{
            auto matched = m_matched.value();
            fmt::print(std::cerr, "{:<15} : {:<15}. Score:{}\n", chunk.repr(*scored_sent.orig.dict,wiki.wordUIDs),
                       matched.data.repr(*scored_sent.orig.dict, wiki.wordUIDs), matched.score);
        }
    }
    fmt::print(std::cerr, "\nWords:\n");
    for(auto&e : scored_sent.words){
        auto chunk = e.first;
        auto m_matched = e.second;
        if(!m_matched) {
            fmt::print(std::cerr, "{:<15} : No match.\n", chunk.repr(wiki.wordUIDs));
        }
        else{
            auto matched = m_matched.value();
            fmt::print(std::cerr, "{:<15} : {:<15}. Score:{}\n", chunk.repr(wiki.wordUIDs),
                       matched.data.repr(*scored_sent.orig.dict, wiki.wordUIDs), matched.score);
        }
    }
    fmt::print(std::cerr, "\nOutput scores:\n");
    auto output_sent = output(scored_sent);
    for(auto token : output_sent.scores.serialize()){
        auto lhs = std::get<0>(token);
        auto rhs = std::get<1>(token);
        auto score = std::get<2>(token);
        fmt::print("{:<15} : {:<15}. Score:{}\n",
                   lhs.repr(*output_sent.sent.dict, wiki.wordUIDs),
                   rhs.repr(*output_sent.sent.dict, wiki.wordUIDs),
                   score);
    }

    auto op_token=scoring.op_similarity();
    for(auto qword : query_to_scored.words)
        for(auto word : sent_to_scored.words) {
            fmt::print("{:<25}:query {:<25}:word : {}\n",
                       qword.repr(wiki.wordUIDs), word.repr(wiki.wordUIDs),
                       op_token.similarity(qword, word));
        }
    return json_t{};
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
