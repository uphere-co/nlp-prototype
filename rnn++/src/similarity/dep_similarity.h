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

struct DepSimilaritySearch {
    using json_t = nlohmann::json;
    using voca_info_t = wordrep::VocaInfo;
    using val_t = voca_info_t::voca_vecs_t::val_t;
    using scored_sents_t = std::vector<std::pair<val_t, wordrep::Sentence>>;
    DepSimilaritySearch(json_t const& config);

    json_t process_query(json_t query) const;
    json_t process_queries(json_t ask) const;
    json_t process_queries_2(json_t ask) const;
    json_t write_output(scored_sents_t relevant_sents,
                        std::vector<std::string> const &words, std::vector<val_t> const &cutoff) const;

    voca_info_t voca;
    wordrep::DepParsedTokens tokens;
    wordrep::WordUIDindex wordUIDs;
    wordrep::POSUIDindex posUIDs;
    wordrep::ArcLabelUIDindex arclabelUIDs;
    wordrep::WordImportance word_cutoff;
    std::vector<wordrep::Sentence> sents;
    wordrep::ygp::YGPdump texts;
    wordrep::ygp::YGPindexer ygp_indexer;
};

}//namespace engine