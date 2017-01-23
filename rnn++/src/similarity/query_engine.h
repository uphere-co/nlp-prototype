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

#include "utils/variant.h"

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

    struct WordSimOp{
        WordSimOp(WordSimCache& cache)
                : cache{&cache}
        {}
        WordSimCache::val_t operator()(wordrep::VocaIndex vidx1, wordrep::VocaIndex vidx2) const {
            if(lookup_cache.find(vidx1)!=lookup_cache.end())
                return (*lookup_cache.find(vidx1)->second)[vidx2];
            return cache->try_find(vidx1)[vidx2];
        }
        void build_lookup_cache(wordrep::VocaIndex const& vidx) {
            lookup_cache[vidx]=&cache->try_find(vidx);
        }
        void build_lookup_cache(std::vector<wordrep::VocaIndex> const& vidxs) {
            for(auto vidx : vidxs)
                build_lookup_cache(vidx);
        }
        WordSimCache* cache;
        std::map<wordrep::VocaIndex,WordSimCache::dist_cache_t const*> lookup_cache;
    };

    WordSimCache(voca_info_t const &voca);
    void cache(std::vector<wordrep::VocaIndex> const &words);
    const dist_cache_t& distances(wordrep::VocaIndex widx) const;
    val_t max_similarity(wordrep::VocaIndex widx) const;
    auto size() const {return distance_caches.size();}

    WordSimOp get_cached_operator() {
        return WordSimOp(*this);
    }
    bool find(wordrep::VocaIndex idx) const;
    const dist_cache_t& try_find(wordrep::VocaIndex idx);
    voca_info_t const &voca;
private:
    bool insert(wordrep::VocaIndex idx, dist_cache_t const &dists);
    data_t distance_caches;
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
class QueryEngineT {
public:
    using dbinfo_t = T;
    using Sentence = wordrep::Sentence;
    using json_t = util::json_t;
    using voca_info_t = wordrep::VocaInfo;
    using val_t = voca_info_t::voca_vecs_t::val_t;
    using output_t = std::vector<data::QueryResult>;

    QueryEngineT(json_t const& config);
    QueryEngineT(QueryEngineT&& engine);

    json_t register_documents(json_t const &ask) ;
    json_t ask_query(json_t const &ask) const;
    json_t ask_chain_query(json_t const &ask) const;
    json_t ask_query_stats(json_t const &ask) const;
    json_t ask_sents_content(json_t const &ask) const;
    json_t ask_query_suggestion(json_t const &ask) const;

    static void annotation_on_result(util::json_t const& config, util::json_t &answers){
        T::annotation_on_result(config, answers);
    }

private:
    wordrep::WordImportance const word_importance;
    wordrep::PhraseSegmenter phrase_segmenter;
    Dataset const db;
    dbinfo_t const dbinfo;
    Dataset queries;
    mutable WordSimCache dists_cache;
    mutable QueryResultCache result_cache{};
};

using RSSQueryEngine = engine::QueryEngineT<data::rss::DBInfo>;
using YGPQueryEngine = engine::QueryEngineT<data::ygp::DBInfo>;

struct QueryEngine {
    using json_t = util::json_t;
    QueryEngine(util::json_t& config);

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
    void annotation_on_result(util::json_t const& config, util::json_t &answers) const {
        engine.match([&config,&answers] (auto& e)  { return e.annotation_on_result(config, answers);});
    }
private:
    mapbox::util::variant<RSSQueryEngine,YGPQueryEngine> engine;
};

}//namespace engine
