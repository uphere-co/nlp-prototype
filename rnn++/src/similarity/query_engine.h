#pragma once

#include "similarity/dataset.h"
#include "similarity/scoring.h"
#include "similarity/ygp.h"
#include "similarity/rss.h"
#include "similarity/config.h"

#include "wordrep/word_prob.h"
#include "wordrep/word_case_corrector.h"
#include "wordrep/dep_graph.h"

#include "utils/json.h"
#include "utils/variant.h"

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

template<typename T>
class QueryEngineT {
public:
    using dbinfo_t = T;
    using Sentence = wordrep::Sentence;
    using json_t = util::json_t;
    using voca_info_t = wordrep::VocaInfo;
    using val_t = voca_info_t::voca_vecs_t::val_t;
    using output_t = std::vector<data::QueryResult>;

    static QueryEngineT factory(json_t const& config);

    QueryEngineT(QueryEngineT&& engine);

    json_t preprocess_query(json_t const &ask) const;
    json_t register_documents(json_t const &ask);
    json_t ask_query(json_t const &ask) const;
    json_t ask_chain_query(json_t const &ask) const;
    json_t ask_query_stats(json_t const &ask) const;
    json_t compare_sentences(json_t const &ask) const;

    static void annotation_on_result(util::json_t const& config, util::json_t &answers){
        T::annotation_on_result(config, answers);
    }

private:
    QueryEngineT(){}

    std::unique_ptr<const wordrep::WordImportance> word_importance;
    std::unique_ptr<wordrep::WordCaseCorrector> did_you_mean;
    std::unique_ptr<wordrep::PhraseSegmenter> phrase_segmenter;
    std::unique_ptr<wordrep::WordUIDindex> wordUIDs;
    std::unique_ptr<const dbinfo_t> dbinfo;
    std::unique_ptr<Dataset> queries;
    std::unique_ptr<wikidata::EntityModule> wiki;
};

using RSSQueryEngine = engine::QueryEngineT<data::rss::DBInfo>;
using YGPQueryEngine = engine::QueryEngineT<data::ygp::DBInfo>;

struct QueryEngine {
    using json_t = util::json_t;
    QueryEngine(util::json_t const& config);

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
    void annotation_on_result(util::json_t const& config, util::json_t &answers) const {
        engine.match([&config,&answers] (auto& e)  { return e.annotation_on_result(config, answers);});
    }
private:
    mapbox::util::variant<RSSQueryEngine,YGPQueryEngine> engine;
};

}//namespace engine
