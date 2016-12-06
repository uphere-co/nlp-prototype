#pragma  once

#include <vector>
#include <string>

namespace data {

struct WordOffset {
    using idx_t = int64_t;
    idx_t beg;
    idx_t end;
};

struct ScoreWithOffset {
    using val_t = float;
    using idx_t = WordOffset::idx_t;

    val_t score;
    WordOffset query_word;
    WordOffset matched_word;
};

struct QuerySentInfo{
    using val_t = float;
    using idx_t = WordOffset::idx_t;
    using str_t = std::string;

    std::vector<str_t> words;
    std::vector<val_t> cutoffs;
    WordOffset offset;
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
    WordOffset offset;
    WordOffset highlight_offset;
    WordOffset clip_offset;
    std::vector<ScoreWithOffset> scores_with_offset;
};

}//namespace data
