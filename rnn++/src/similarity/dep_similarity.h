#pragma once

#include <mutex>

#include "utils/json.h"

#include "data_source/ygp_db.h"

#include "wordrep/dep_parsed.h"
#include "wordrep/voca_info.h"
#include "wordrep/word_prob.h"

#include "utils/parallel.h"

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

template<typename T>
struct TBBHashCompare {
    static size_t hash(T const& x) {return std::hash<T>{}(x);}
    static bool equal(T const& x, T const& y ) {return x==y;}
};

class WordSimCache{
public:
    using voca_info_t  = wordrep::VocaInfo;
    using word_block_t = voca_info_t::voca_vecs_t;
    using val_t        = word_block_t::val_t;
    using dist_cache_t = Distances<val_t>;
    using data_t = tbb::concurrent_hash_map<wordrep::VocaIndex, dist_cache_t,TBBHashCompare<wordrep::VocaIndex>>;

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
    using data_t = tbb::concurrent_hash_map<wordrep::SentUID,result_t,TBBHashCompare<wordrep::SentUID>>;
    QueryResultCache() {}
    void insert(wordrep::SentUID uid, result_t const&result);
    result_t get(wordrep::SentUID uid) const;
    bool find(wordrep::SentUID uid) const;
private:
    data_t caches;
};


struct DepSearchScore{
    using val_t = WordSimCache::val_t;
    DepSearchScore(size_t len) : idxs_lhs(len),idxs_rhs(len), scores(len) {}
    void set(size_t j, wordrep::DPTokenIndex idx_lhs, wordrep::DPTokenIndex idx_rhs, val_t score){
        scores[j]=score;
        idxs_lhs[j]=idx_lhs;
        idxs_rhs[j]=idx_rhs;
    }
    std::vector<std::pair<wordrep::DPTokenIndex,val_t>> scores_with_idx() const {
        return util::zip(idxs_rhs, scores);
    };
    auto serialize() const {
        return util::zip(idxs_lhs, idxs_rhs, scores);
    };
    val_t score_sum() const;
private:
    std::vector<wordrep::DPTokenIndex> idxs_lhs;
    std::vector<wordrep::DPTokenIndex> idxs_rhs;
    std::vector<val_t> scores;
};

struct ScoredSentence{
    using val_t = WordSimCache::val_t;
    ScoredSentence(wordrep::Sentence sent, DepSearchScore const &scores)
    :sent{sent}, scores{scores}, score{scores.score_sum()} {
    }
    wordrep::Sentence sent;
    DepSearchScore scores;
    val_t score;
};

struct DepSimilaritySearch {
    using Sentence = wordrep::Sentence;
    using json_t = util::json_t;
    using voca_info_t = wordrep::VocaInfo;
    using val_t = voca_info_t::voca_vecs_t::val_t;
    using output_t = std::vector<data::QueryResult>;

    DepSimilaritySearch(json_t const& config);

    std::vector<ScoredSentence> process_query_sent(Sentence query_sent,
                                                   std::vector<val_t> const &cutoffs,
                                                   std::vector<Sentence> const &data_sents) const;
    output_t process_query_sents(std::vector<Sentence> const &query_sents,
                                 std::vector<std::string> const &countries) const;
    output_t process_chain_query(std::vector<Sentence> const &query_chain,
                                 std::vector<std::string> const &countries) const;
    json_t register_documents(json_t const &ask) ;
    json_t ask_query(json_t const &ask) const;
    json_t ask_chain_query(json_t const &ask) const;
    std::vector<data::PerSentQueryResult> write_output(
            Sentence const &query_sent,
            std::vector<ScoredSentence> const &relevant_sents,
            int64_t max_clip_len) const;

    voca_info_t voca;
    wordrep::DepParsedTokens const tokens;
    wordrep::WordUIDindex const wordUIDs;
    wordrep::POSUIDindex const posUIDs;
    wordrep::ArcLabelUIDindex const arclabelUIDs;
    wordrep::WordImportance const word_importance;
    std::vector<Sentence> sents;
    data::ygp::YGPdb const ygpdb;
    data::ygp::YGPindexer const ygp_indexer;
    data::ygp::DBbyCountry const ygpdb_country;
    data::ygp::CountryCodeAnnotator country_tagger;
    mutable WordSimCache dists_cache{voca};
    mutable QueryResultCache result_cache{};
    wordrep::DepParsedTokens query_tokens{};
    std::mutex query_tokens_update;
};

struct RSSQueryEngine {
    using Sentence = wordrep::Sentence;
    using json_t = util::json_t;
    using voca_info_t = wordrep::VocaInfo;
    using val_t = voca_info_t::voca_vecs_t::val_t;
    using output_t = std::vector<data::QueryResult>;

    RSSQueryEngine(json_t const& config);

    std::vector<ScoredSentence> process_query_sent(Sentence query_sent,
                                                   std::vector<val_t> const &cutoffs,
                                                   std::vector<Sentence> const &data_sents) const;
    output_t process_query_sents(std::vector<Sentence> const &query_sents) const;
    output_t process_chain_query(std::vector<Sentence> const &query_chain) const;
    json_t register_documents(json_t const &ask) ;
    json_t ask_query(json_t const &ask) const;
    json_t ask_chain_query(json_t const &ask) const;
    std::vector<data::PerSentQueryResult> write_output(
            Sentence const &query_sent,
            std::vector<ScoredSentence> const &relevant_sents,
            int64_t max_clip_len) const;

    voca_info_t voca;
    wordrep::DepParsedTokens const tokens;
    wordrep::WordUIDindex const wordUIDs;
    wordrep::POSUIDindex const posUIDs;
    wordrep::ArcLabelUIDindex const arclabelUIDs;
    wordrep::WordImportance const word_importance;
    std::vector<Sentence> sents;
    data::ygp::YGPindexer db_indexer;
    mutable WordSimCache dists_cache{voca};
    mutable QueryResultCache result_cache{};
    wordrep::DepParsedTokens query_tokens{};
    std::mutex query_tokens_update;
};
}//namespace engine