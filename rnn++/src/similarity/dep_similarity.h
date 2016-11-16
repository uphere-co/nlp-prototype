#pragma once

#include <mutex>

#include "utils/json.h"

#include "wordrep/dep_parsed.h"
#include "wordrep/voca.h"
#include "wordrep/word_prob.h"
#include "wordrep/wordvec.h"

#include "utils/hdf5.h"
#include "utils/parallel.h"

namespace wordrep{
struct VocaInfo{
    using voca_vecs_t = WordBlock_base<float,100>;
    VocaInfo(std::string h5file, std::string voca_name,
             std::string wvec_name, std::string w2v_float_t)
    : indexmap{load_voca(h5file, voca_name)},
      wvecs{load_raw_wvec(h5file, wvec_name, w2v_float_t)}
    {}
    VocaIndexMap indexmap;
    voca_vecs_t wvecs;
};

}//namespace wordrep

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
private:
    bool find(wordrep::VocaIndex idx) const;
    bool insert(wordrep::VocaIndex idx, dist_cache_t const &dists);
    data_t distance_caches;
    voca_info_t const &voca;
};

class QueryResultCache{
public:
    using json_t = nlohmann::json;
    using data_t = tbb::concurrent_hash_map<wordrep::SentUID,json_t,TBBHashCompare<wordrep::SentUID>>;
    QueryResultCache() {}
    void insert(wordrep::SentUID uid, json_t const&result);
    json_t get(wordrep::SentUID uid) const;
    json_t find(wordrep::SentUID uid) const;
private:
    data_t caches;
};


struct DepSearchScore{
    using val_t = WordSimCache::val_t;
    DepSearchScore(size_t len) : scores(len) {}z`

    void set(size_t j, wordrep::DPTokenIndex idx_query, wordrep::DPTokenIndex idx_matched, val_t score){
        scores[j]={idx_matched, score};
        //scores.push_back(score);
        //score_sum += score;
    }

//    val_t score_sum;
//    std::vector<val_t> scores;
//private:
    std::vector<std::pair<wordrep::DPTokenIndex, val_t>>  scores;
    //word;
//    word;
};

struct ScoredSentence{
    using val_t = WordSimCache::val_t;
    ScoredSentence(wordrep::Sentence sent, DepSearchScore const &scores)
    :sent{sent}, scores{scores}, score{0.0} {
        for(auto pair : scores.scores) score += pair.second;
    }
    wordrep::Sentence sent;
    DepSearchScore scores;
    val_t score;

};

struct DepSimilaritySearch {
    using json_t = nlohmann::json;
    using voca_info_t = wordrep::VocaInfo;
    using val_t = voca_info_t::voca_vecs_t::val_t;
    DepSimilaritySearch(json_t const& config);

    std::vector<ScoredSentence> process_query_sent(wordrep::Sentence query_sent,
                                                   std::vector<val_t> const &cutoffs,
                                                   std::vector<wordrep::Sentence> const &data_sents) const;
    json_t process_query_sents(std::vector<wordrep::Sentence> const &query_sents,
                               std::vector<std::string> const &countries) const;
    json_t process_chain_query(std::vector<wordrep::Sentence> const &query_chain,
                               std::vector<std::string> const &countries) const;
    json_t register_documents(json_t const &ask) ;
    json_t ask_query(json_t const &ask) const;
    json_t ask_chain_query(json_t const &ask) const;
    json_t write_output(std::vector<ScoredSentence> relevant_sents, int64_t max_clip_len) const;

    voca_info_t voca;
    wordrep::DepParsedTokens const tokens;
    wordrep::WordUIDindex const wordUIDs;
    wordrep::POSUIDindex const posUIDs;
    wordrep::ArcLabelUIDindex const arclabelUIDs;
    wordrep::WordImportance const word_cutoff;
    std::vector<wordrep::Sentence> sents;
    wordrep::ygp::YGPdb const ygpdb;
    wordrep::ygp::YGPindexer const ygp_indexer;
    wordrep::ygp::DBbyCountry const ygpdb_country;
    wordrep::ygp::CountryCodeAnnotator country_tagger;
    mutable WordSimCache dists_cache{voca};
    mutable QueryResultCache result_cache{};
    wordrep::DepParsedTokens query_tokens{};
    std::mutex query_tokens_update;
};

}//namespace engine