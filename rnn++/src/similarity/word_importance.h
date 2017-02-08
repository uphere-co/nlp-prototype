#pragma once

#include "wordrep/indexes.h"
#include "data_source/indexes.h"

#include "utils/json.h"
namespace engine{

struct CaseCount{
    using float_t = float;
    int64_t both{0};
    int64_t summary{0};
    int64_t full{0};
    float ratio{0};
};

struct ColumnPair{
    data::ColumnUID summary;
    data::ColumnUID full;
};


void accum_word_importance_count(util::json_t const& config,
                                 std::vector<ColumnPair> const& column_pairs,
                                 std::map<wordrep::WordUID,CaseCount>& cases,
                                 int64_t& n_case);
void build_word_importance();

}//namespace wordrep
