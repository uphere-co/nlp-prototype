#pragma once

#include "data_source/db_query.h"

#include "wordrep/sentence.h"

#include "utils/json.h"
#include "utils/base_types.h"

namespace data {

struct TableUIDDummy {};
using  TableUID  = util::IntegerLike<TableUIDDummy>;
struct ColumUIDDummy {};
using  ColumnUID = util::IntegerLike<ColumUIDDummy>;
struct RowIndexDummy {};
using  RowIndex  = util::IntegerLike<RowIndexDummy>;
struct RowUIDDummy {};
using  RowUID    = util::IntegerLike<RowUIDDummy>;

void set_db_info(PerSentQueryResult &result, ColumnUID col_uid, RowUID row_uid, RowIndex ridx,
                 wordrep::Sentence const &sent);
void build_db_info_field(util::json_t &answer, PerSentQueryResult const &result);

}//namespace data

