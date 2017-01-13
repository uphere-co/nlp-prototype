#pragma once

#include "similarity/dataset.h"
#include "similarity/scoring.h"
#include "similarity/ygp.h"
#include "similarity/rss.h"

#include "wordrep/dep_parsed.h"
#include "wordrep/dep_graph.h"
#include "wordrep/voca_info.h"
#include "wordrep/word_prob.h"

#include "utils/parallel.h"
#include "utils/json.h"

namespace engine {

template<typename TV>
struct Distances{
    Distances() : val{} {}
    Distances(std::size_t n) : val(n) {}
    Distances(std::vector<TV> const &distances)
    : val{distances} {}
    Distances& operator=(Distances const &obj){
        val = std::move(obj.val);
        return *this;
    }
    TV& operator[](wordrep::VocaIndex vidx) {return val[vidx.val];}
    TV operator[](wordrep::VocaIndex vidx) const {return val[vidx.val];}
    std::vector<TV> val;
};

class WordSimCache{
public:
    using voca_info_t  = wordrep::VocaInfo;
    using val_t        = voca_info_t::val_t;
    using dist_cache_t = Distances<val_t>;
    using data_t = tbb::concurrent_hash_map<wordrep::VocaIndex, dist_cache_t,util::TBBHashCompare<wordrep::VocaIndex>>;

    WordSimCache(voca_info_t const &voca);
    void cache(std::vector<wordrep::VocaIndex> const &words);
    const dist_cache_t& distances(wordrep::VocaIndex widx) const;
    val_t max_similarity(wordrep::VocaIndex widx) const;
private:
    bool find(wordrep::VocaIndex idx) const;
    bool insert(wordrep::VocaIndex idx, dist_cache_t const &dists);
    data_t distance_caches;
    voca_info_t const &voca;
};

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
class QueryEngine {
public:
    using dbinfo_t = T;
    using Sentence = wordrep::Sentence;
    using json_t = util::json_t;
    using voca_info_t = wordrep::VocaInfo;
    using val_t = voca_info_t::voca_vecs_t::val_t;
    using output_t = std::vector<data::QueryResult>;

    QueryEngine(json_t const& config);
    QueryEngine(QueryEngine&& engine);

    json_t register_documents(json_t const &ask) ;
    json_t ask_query(json_t const &ask) const;
    json_t ask_chain_query(json_t const &ask) const;
    json_t ask_query_stats(json_t const &ask) const;
    json_t ask_sents_content(json_t const &ask) const;
    json_t ask_query_suggestion(json_t const &ask) const;

private:
    wordrep::WordImportance const word_importance;
    wordrep::PhraseSegmenter phrase_segmenter;
    Dataset const db;
    dbinfo_t const dbinfo;
    Dataset queries;
    mutable WordSimCache dists_cache;
    mutable QueryResultCache result_cache{};
};

using RSSQueryEngine = engine::QueryEngine<data::rss::DBInfo>;
using YGPQueryEngine = engine::QueryEngine<data::ygp::DBInfo>;

}//namespace engine
