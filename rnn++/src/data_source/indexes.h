#pragma once
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

}//namespace data
