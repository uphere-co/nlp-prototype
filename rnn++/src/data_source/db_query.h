#pragma  once

#include <vector>
#include <string>

namespace data {

struct StringOffset {
    using idx_t = int64_t;
    idx_t beg;
    idx_t end;
};

struct ScoreWithOffset {
    using val_t = float;
    using idx_t = StringOffset::idx_t;

    val_t score;
    StringOffset query_token;
    StringOffset matched_token;
};

struct QuerySentInfo{
    using val_t = float;
    using idx_t = StringOffset::idx_t;
    using str_t = std::string;

    std::vector<str_t> words;
    std::vector<val_t> cutoffs;
    StringOffset offset;
    idx_t sent_uid;
};

struct PerSentQueryResult {
    using val_t = QuerySentInfo::val_t;
    using idx_t = QuerySentInfo::idx_t;
    using str_t = QuerySentInfo::str_t;

    val_t score;
    std::string country;
    idx_t sent_uid;
    idx_t row_uid;
    idx_t row_idx;
    idx_t column_uid;
    str_t table_name;
    str_t column_name;
    str_t index_col_name;
    StringOffset offset;
    StringOffset highlight_offset;
    StringOffset clip_offset;
    std::vector<ScoreWithOffset> scores_with_offset;
};

struct QueryResult{
    QuerySentInfo query;
    std::vector<PerSentQueryResult> results;
    int64_t n_relevant_matches;
};

}//namespace data
