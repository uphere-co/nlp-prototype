#pragma once

#include "utils/json.h"

#include "wordrep/dep_parsed.h"
#include "wordrep/voca.h"
#include "wordrep/word_prob.h"
#include "wordrep/wordvec.h"

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
    DepSimilaritySearch(json_t const& config);

    json_t process_query(json_t query) const;
    json_t process_queries(json_t ask) const;

    voca_info_t voca;
    wordrep::DepParsedTokens tokens;
    wordrep::WordUIDindex wordUIDs;
    wordrep::WordImportance word_cutoff;
    std::vector<wordrep::Sentence> sents;
    std::vector<std::string> sents_plain;
};

}//namespace engine