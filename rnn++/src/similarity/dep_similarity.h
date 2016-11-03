#pragma once

#include "utils/json.h"

#include "wordrep/dep_parsed.h"
#include "wordrep/voca.h"
#include "wordrep/word_prob.h"
#include "wordrep/wordvec.h"

#include "utils/hdf5.h"

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
struct DistanceCache{
    DistanceCache() : val{} {}
    DistanceCache(std::size_t n) : val(n) {}
    DistanceCache(std::vector<TV> const &distances)
    : val{distances} {}
    DistanceCache& operator=(DistanceCache const &obj){
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
    using word_block_t = voca_info_t::voca_vecs_t;
    using val_t        = word_block_t::val_t;
    using dist_cache_t = DistanceCache<val_t>;

    WordSimCache(voca_info_t const &voca) : voca{voca} {
        auto n= voca.wvecs.size();
        distance_caches[wordrep::VocaIndex{}] = dist_cache_t{n};//For unknown word
    }
    void cache(std::vector<wordrep::VocaIndex> const &words);
    const dist_cache_t& distances(wordrep::VocaIndex widx) const {return distance_caches[widx];}
    dist_cache_t& distances(wordrep::VocaIndex widx) {return distance_caches[widx];}
private:
    mutable std::map<wordrep::VocaIndex,dist_cache_t> distance_caches;
    voca_info_t const &voca;
};

struct ScoredSentence{
    using val_t = WordSimCache::val_t;
    using scores_t = std::vector<std::pair<wordrep::DPTokenIndex, val_t>>;
    ScoredSentence(wordrep::Sentence sent, scores_t const &scores)
    :sent{sent}, scores{scores}, score{0.0} {
        for(auto pair : scores) score += pair.second;
    }
    wordrep::Sentence sent;
    std::vector<std::pair<wordrep::DPTokenIndex, val_t>> scores;
    val_t score;

};

struct DepSimilaritySearch {
    using json_t = nlohmann::json;
    using voca_info_t = wordrep::VocaInfo;
    using val_t = voca_info_t::voca_vecs_t::val_t;
    DepSimilaritySearch(json_t const& config);

    std::vector<ScoredSentence> process_query_sent(wordrep::Sentence query_sent,
                                                   std::vector<val_t> const &cutoffs) const;
    json_t process_query_sents(std::vector<wordrep::Sentence> const &query_sents) const;
    json_t register_documents(json_t const &ask) ;
    json_t process_query(json_t const &ask) const;
    json_t write_output(std::vector<ScoredSentence> relevant_sents, int64_t max_clip_len) const;

    voca_info_t voca;
    wordrep::DepParsedTokens tokens;
    wordrep::WordUIDindex wordUIDs;
    wordrep::POSUIDindex posUIDs;
    wordrep::ArcLabelUIDindex arclabelUIDs;
    wordrep::WordImportance word_cutoff;
    std::vector<wordrep::Sentence> sents;
    wordrep::ygp::YGPdb ygpdb;
    wordrep::ygp::YGPindexer ygp_indexer;
    mutable WordSimCache dists_cache{voca};
    wordrep::DepParsedTokens query_tokens{};

};

}//namespace engine