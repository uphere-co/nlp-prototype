#pragma once

#include <vector>
#include <map>

#include "wordrep/sentence.h"
#include "wordrep/words.h"
#include "wordrep/word_prob.h"
#include "wordrep/dep_graph.h"

#include "utils/json.h"

namespace engine{

struct PhraseUsage{

    std::vector<wordrep::WordUID> phrase;
    std::map<std::vector<wordrep::WordUID>,int> phrase_reprs;
    int64_t count;
};

struct WordUsageInPhrase{
    using float_t = float;
    using counts_t = std::vector<std::pair<wordrep::Words,int64_t>>;
    using reprs_t  = std::map<wordrep::Words,std::map<wordrep::Words,int64_t>>;
    WordUsageInPhrase(std::vector<wordrep::Sentence> const& sents,
                      wordrep::WordImportance const& importance_)
            : sents{sents}, importance{importance_}, phrase_segmenter{importance}
    {}
    std::pair<counts_t,reprs_t> usages(wordrep::WordUID word, float_t cutoff) const;

private:
    std::vector<wordrep::Sentence> const& sents;
    wordrep::WordImportance const& importance;
    wordrep::PhraseSegmenter phrase_segmenter;
    int const sentence_lenth_limit{100};
};

util::json_t get_query_suggestion(std::vector<wordrep::WordUID> const& wuids,
                                  WordUsageInPhrase const& phrase_finder,
                                  wordrep::WordUIDindex const& wordUIDs,
                                  WordUsageInPhrase::float_t cutoff);
} //namespace engine
