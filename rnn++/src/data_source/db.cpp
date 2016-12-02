#include "data_source/db.h"

namespace data {

void set_db_info(PerSentQueryResult &result, ColumnUID col_uid, RowUID row_uid, RowIndex ridx,
                 wordrep::Sentence const &sent) {
    result.column_uid = col_uid.val;
    result.row_uid = row_uid.val;
    result.row_idx = ridx.val;
    result.sent_uid = sent.uid.val;
    result.offset = {sent.beg_offset().val, sent.end_offset().val};
}

void build_db_info_field(util::json_t &answer, PerSentQueryResult const &result) {
    answer["result_sent_uid"].push_back(result.sent_uid);
    answer["result_row_uid"].push_back(result.row_uid);
    answer["result_row_idx"].push_back(result.row_idx);
    answer["result_column_uid"].push_back(result.column_uid);
    answer["result_offset"].push_back({result.offset.beg, result.offset.end});
}

}//namespace data

