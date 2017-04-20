#include <algorithm>
#include <map>

#include <fmt/printf.h>
#include <fmt/format.h>

#include "data_source/ygp_db.h"
#include "data_source/db.h"
#include "data_source/rss.h"

#include "similarity/query_types.h"
#include "similarity/query_engine.h"
#include "similarity/phrase_suggestion.h"
#include "similarity/query_output.h"

#include "utils/parallel.h"
#include "utils/profiling.h"
#include "utils/span.h"
#include "utils/string.h"
#include "utils/algorithm.h"
#include "utils/math.h"
#include "utils/linear_algebra.h"

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

///////////////////////////////////////////////////////////////
template<typename T>
QueryEngineT<T> QueryEngineT<T>::factory(json_t const& config){
    QueryEngineT engine;
    typename T::factory_t factory{config};
    auto load_word_score_related = [&engine,&factory](){
        engine.word_importance = std::make_unique<const wordrep::WordImportance>(factory.common.word_importance());
        util::parallel_invoke([&](){
            engine.did_you_mean    = std::make_unique<wordrep::WordCaseCorrector>(factory.common.word_case_corrector(*engine.word_importance));
        },[&](){
            engine.phrase_segmenter= std::make_unique<wordrep::PhraseSegmenter>(*engine.word_importance);
        });
    };
    auto load_word_uids = [&engine,&factory](){
        engine.wordUIDs = std::make_unique<wordrep::WordUIDindex>(factory.common.word_uid_index());
    };
    auto load_dbinfo = [&engine,&factory](){
        engine.dbinfo = std::make_unique<const dbinfo_t>(factory);
    };
    auto load_queries = [&engine,&factory](){
        engine.queries = std::make_unique<Dataset>(factory.common.empty_dataset());
    };
    auto load_wiki = [&engine,&factory](){
        engine.wiki = std::make_unique<wikidata::EntityModule>(factory.common.wikientity_module());
    };
    auto load_processor = [&engine,&factory](){
        engine.processor = std::make_unique<QueryProcessor>(QueryProcessor::factory(factory.common));
    };

    util::parallel_invoke(load_word_score_related,
                          load_word_uids,
                          load_dbinfo,
                          load_queries,
                          load_wiki,
                          load_processor);
    return engine;
}

template<typename T>
QueryEngineT<T>::QueryEngineT(QueryEngineT&& engine)
: word_importance{std::move(engine.word_importance)},
  did_you_mean{std::move(engine.did_you_mean)},
  phrase_segmenter{std::make_unique<wordrep::PhraseSegmenter>(*word_importance)},
  wordUIDs{std::move(engine.wordUIDs)},
  dbinfo{std::move(engine.dbinfo)},
  queries{std::move(engine.queries)},
  wiki{std::move(engine.wiki)},
  processor{std::move(engine.processor)}
{
    fmt::print(std::cerr, "Engine is move constructed.\n");
}

template<typename T>
json_t QueryEngineT<T>::preprocess_query(json_t const &ask) const {
    util::Timer timer;
    timer.here_then_reset(fmt::format("QueryEngine::preprocess_query is called : {}", ask.dump()));
    if (ask.find("sentences") == ask.end()) return json_t{};
    data::CoreNLPjson query{ask};

    std::vector<std::string> original_words;
    auto per_tokens = [&original_words](auto const &token){
        original_words.push_back(util::get_str(token,"originalText"));
    };
    query.iter_tokens(per_tokens);
    auto original_query = util::string::join(original_words, " ");
    auto corrected_words = util::map(original_words, [this](auto word){return did_you_mean->try_correct(word);});
    auto corrected_query = util::string::join(corrected_words, " ");

    json_t answer{};
    answer["received_query"]=original_query;
    answer["did_you_mean"]=corrected_query;
    answer["word_pair"] = util::json_t::array();
    for(auto pair : util::zip(original_words, corrected_words)){
        if(pair.first==pair.second) continue;
        answer["word_pair"].push_back({pair.first, pair.second});
    }
    timer.here_then_reset("QueryEngine::preprocess_query is finished.");
    return answer;
}

template<typename T>
json_t QueryEngineT<T>::register_documents(json_t const &ask) {
    util::Timer timer;
    timer.here_then_reset(fmt::format("QueryEngine::register_documents is called : {}", ask.dump()));

    if (ask.find("sentences") == ask.end()) return json_t{};
    auto uids = queries->append_chunk(data::CoreNLPjson{ask});
    json_t answer{};
    answer["sent_uids"]=util::serialize(uids);
    std::cerr<<fmt::format("# of sents : {}\n", uids.size()) << std::endl;
    // tag_on_register_documents does
    // RSS : nothing
    // YGP : country tags
    dbinfo->tag_on_register_documents(ask, answer);
    timer.here_then_reset("QueryEngine::register_documents is finished.");
    return answer;
}

template<typename T>
json_t QueryEngineT<T>::ask_query(json_t const &ask) const {
    util::Timer timer;
    timer.here_then_reset(fmt::format("QueryEngine::ask_query is called : {}", ask.dump()));
    if (!dbinfo_t::query_t::is_valid(ask)) return json_t{};
    typename dbinfo_t::query_t query{ask};
    auto max_clip_len = util::find<int64_t>(ask, "max_clip_len").value_or(200);
    auto n_cut = util::find<int64_t>(ask, "n_cut").value_or(10);

    //Lookup sentences based on sentence UIDs
    auto query_sents = dbinfo->get_query_sents(query, queries->uid2sent);
    //Get words and their cutoffs in query sentence
    auto queries = util::map(query_sents, [this](auto sent)->SentenceQuery{
        return {sent, construct_query_info(sent, *wordUIDs, *word_importance)};
    });

        //input : candidates_sents
    //output : ScoredSentence
    //dbinfo.build_result : ScoredSentence -> PerSentQueryResult
    //QueryResult ->
    //output : util::ConcurrentVector<data::QueryResult> answers;

    util::ConcurrentVector<data::QueryResult> answers;
    auto op_cut = [](){};
    auto op_results = [this,max_clip_len](auto const& query_sent, auto const& scored_sent){
        return dbinfo->build_result(query_sent, scored_sent, max_clip_len);
    };
    auto per_sent=[&answers,max_clip_len,op_results,op_cut](
            auto const &query_sent, auto const& query_sent_info, auto const &relevant_sents){
        data::QueryResult answer;
        answer.results = write_output(query_sent, relevant_sents, op_cut, op_results);
        answer.query = query_sent_info;
        answer.n_relevant_matches = relevant_sents.size();
        answers.push_back(answer);
    };

    timer.here_then_reset("QueryEngine::ask_query is finished.");
    return to_json(answers.to_vector());
}

template<typename T>
json_t QueryEngineT<T>::ask_chain_query(json_t const &ask) const {
    return ask_query(ask);
}

template<typename T>
json_t QueryEngineT<T>::ask_query_stats(json_t const &ask) const {
    util::Timer timer;
    timer.here_then_reset(fmt::format("QueryEngine::ask_query_stats is called : {}", ask.dump()));
    if (!dbinfo_t::query_t::is_valid(ask)) return json_t{};
    typename dbinfo_t::query_t query{ask};
    auto max_clip_len = util::find<int64_t>(ask, "max_clip_len").value_or(200);
    auto n_cut = util::find<int64_t>(ask, "n_cut").value_or(10);

    auto query_sents = dbinfo->get_query_sents(query, queries->uid2sent);
    auto queries = util::map(query_sents, [this](auto sent)->SentenceQuery{
        return {sent, construct_query_info(sent, *wordUIDs, *word_importance)};
    });

    auto phrase_cutoff = util::find<float>(ask, "phrase_cutoff").value_or(5.0);
    json_t query_suggestions = json_t::array();
    auto get_query_suggestions = [this,&query_suggestions,phrase_cutoff](auto const &query_sent, auto const &relevant_sents){
        auto sents = util::map(relevant_sents, [](auto const& r_sent){return r_sent.sent;});
        fmt::print(std::cerr, "Find phrase suggestions in {} sentences.", sents.size());
        WordUsageInPhrase phrase_finder{sents, *word_importance};
        std::vector<WordUID> wuids;
        for(auto idx : query_sent) {
            auto wuid = query_sent.dict->word_uid(idx);
            if (word_importance->is_noisy_word(wuid)) continue;
            wuids.push_back(wuid);
        }
        json_t query_suggestion;
        query_suggestion["query_suggestions"]= get_query_suggestion(wuids, phrase_finder, *wordUIDs, phrase_cutoff);
        query_suggestion["sent_uid"] = query_sent.uid.val;
        query_suggestions.push_back(query_suggestion);
    };

    util::ConcurrentVector<data::QueryResult> answers;
    util::json_t out{};
    out["results"] = to_json(answers.to_vector());
    out["query_suggestions_per_sent"] = query_suggestions;
    return out;
}

class UnknownQueryEngineException: public std::exception {
    virtual const char* what() const throw() {
        return "Missing or unknown query engine type.";
    }
};

mapbox::util::variant<RSSQueryEngine,YGPQueryEngine> load_query_engine(util::json_t const& config){
    if(util::get_str(config,"engine_type")=="ygp")
        return YGPQueryEngine::factory(config);
    if(util::get_str(config,"engine_type")!="rss") throw UnknownQueryEngineException{};
    return RSSQueryEngine::factory(config);
};

//Explicit instantiation of query engines.
template class engine::QueryEngineT<data::rss::DBInfo>;
template class engine::QueryEngineT<data::ygp::DBInfo>;

QueryEngine::QueryEngine(util::json_t const& config)
: engine{load_query_engine(config)}
{}

}//namespace engine
