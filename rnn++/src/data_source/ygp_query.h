#pragma  once

#include <vector>
#include <string>

namespace data {
namespace ygp {

struct WordOffset {
    using idx_t = int64_t;
    idx_t beg;
    idx_t end;
};

struct ScoreWithOffset {
    using val_t = double;
    using idx_t = WordOffset::idx_t;

    val_t score;
    WordOffset query_word;
    WordOffset matched_word;
};

struct PerSentQueryResult {
    using val_t = double;
    using idx_t = WordOffset::idx_t;
    using str_t = std::string;

    val_t score;
    std::string result_sent_country;
    idx_t result_sent_uid;
    idx_t result_row_uid;
    idx_t result_row_idx;
    idx_t result_column_uid;
    str_t result_table_name;
    str_t result_column_name;
    str_t result_index_col_name;
    WordOffset result_offset;
    WordOffset highlight_offset;
    WordOffset clip_offset;
    std::vector<ScoreWithOffset> scores_with_offset;
};

}//namespace data::ygp
}//namespace data
