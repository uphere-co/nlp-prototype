#pragma once

#include "similarity/dataset.h"
#include "similarity/scoring.h"
#include "similarity/ygp.h"
#include "similarity/rss.h"
#include "similarity/config.h"

#include "wordrep/dep_parsed.h"
#include "wordrep/dep_graph.h"
#include "wordrep/voca_info.h"
#include "wordrep/word_prob.h"
#include "wordrep/word_case_corrector.h"
#include "wordrep/wordsim_cache.h"

#include "utils/parallel.h"
#include "utils/json.h"

#include "utils/variant.h"
#include "config.h"

namespace engine {

class QueryResultCache{
public:
    using result_t = data::QueryResult;
    using data_t = tbb::concurrent_hash_map<wordrep::SentUID,result_t,util::TBBHashCompare<wordrep::SentUID>>;
    QueryResultCache() {}
    void insert(wordrep::SentUID uid, result_t const&result);
    result_t get(wordrep::SentUID uid) const;
    bool find(wordrep::SentUID uid) const;
private:
    data_t caches;
};

struct PreprocessedSent{
    PreprocessedSent(wikidata::EntityModule const& wiki,
                     wordrep::Scoring::Preprocess const& scoring_preprocessor,
                     std::vector<wordrep::Sentence> const& orig_sents){
        if(!orig_sents.empty()){
            assert(orig_sents.front().uid==wordrep::SentUID{0});
            assert(orig_sents.back().uid==wordrep::SentUID::from_unsigned(orig_sents.size()-1));
        }
        auto n = orig_sents.size();
        tbb::parallel_for(decltype(n){0}, n, [&,this](auto i) {
            auto& sent = orig_sents[i];
            auto tagged_sent = wiki.annotator.annotate(sent);
            auto sent_to_scored = scoring_preprocessor.sentence(tagged_sent);
            sent_to_scored.filter_false_named_entity(wiki.posUIDs);
            sents.push_back(sent_to_scored);
        });
    }
    size_t size() const {return sents.size();}
    tbb::concurrent_vector<wordrep::Scoring::SentenceToScored> sents;
};

template<typename T>
class QueryEngineT {
public:
    using dbinfo_t = T;
    using Sentence = wordrep::Sentence;
    using json_t = util::json_t;
    using voca_info_t = wordrep::VocaInfo;
    using val_t = voca_info_t::voca_vecs_t::val_t;
    using output_t = std::vector<data::QueryResult>;

    QueryEngineT(typename T::factory_t const& factory);
    QueryEngineT(json_t const& config);
    QueryEngineT(QueryEngineT&& engine);

    json_t preprocess_query(json_t const &ask) const;
    json_t register_documents(json_t const &ask);
    json_t ask_query(json_t const &ask) const;
    json_t ask_chain_query(json_t const &ask) const;
    json_t ask_query_stats(json_t const &ask) const;
    json_t ask_sents_content(json_t const &ask) const;
    json_t ask_query_suggestion(json_t const &ask) const;
    json_t compare_sentences(json_t const &ask) const;

    static void annotation_on_result(util::json_t const& config, util::json_t &answers){
        T::annotation_on_result(config, answers);
    }

private:
    wordrep::WordImportance const word_importance;
    wordrep::WordCaseCorrector did_you_mean;
    wordrep::PhraseSegmenter phrase_segmenter;
    Dataset const db;
    dbinfo_t const dbinfo;
    Dataset queries;
    wikidata::EntityModule wiki;
    wordrep::Scoring scoring;
    wordrep::Scoring::Preprocess scoring_preprocessor;
    PreprocessedSent data_sents;
    mutable wordrep::WordSimCache dists_cache;
    mutable QueryResultCache result_cache{};
};

using RSSQueryEngine = engine::QueryEngineT<data::rss::DBInfo>;
using YGPQueryEngine = engine::QueryEngineT<data::ygp::DBInfo>;

struct QueryEngine {
    using json_t = util::json_t;
    QueryEngine(util::json_t& config);

    json_t preprocess_query(json_t const &ask) {
        return engine.match([&ask] (auto& e)  { return e.preprocess_query(ask);});
    }
    json_t register_documents(json_t const &ask) {
        return engine.match([&ask] (auto& e)  { return e.register_documents(ask);});
    }
    json_t ask_query(json_t const &ask) const{
        return engine.match([&ask] (auto& e)  { return e.ask_query(ask);});
    }
    json_t ask_chain_query(json_t const &ask) const{
        return engine.match([&ask] (auto& e)  { return e.ask_chain_query(ask);});
    }
    json_t ask_query_stats(json_t const &ask) const{
        return engine.match([&ask] (auto& e)  { return e.ask_query_stats(ask);});
    }
    json_t ask_sents_content(json_t const &ask) const{
        return engine.match([&ask] (auto& e)  { return e.ask_sents_content(ask);});
    }
    json_t ask_query_suggestion(json_t const &ask) const{
        return engine.match([&ask] (auto& e)  { return e.ask_query_suggestion(ask);});
    }
    json_t compare_sentences(json_t const &ask) const{
        return engine.match([&ask] (auto& e)  { return e.compare_sentences(ask);});
    }
    void annotation_on_result(util::json_t const& config, util::json_t &answers) const {
        engine.match([&config,&answers] (auto& e)  { return e.annotation_on_result(config, answers);});
    }
private:
    mapbox::util::variant<RSSQueryEngine,YGPQueryEngine> engine;
};

}//namespace engine
