#pragma once

#include <map>

#include "data_source/db_query.h"

#include "wordrep/sentence.h"

#include "utils/json.h"
#include "utils/base_types.h"

//Forward declarations
namespace util{
namespace io{
struct H5file;
}//namespace util::io
}//namespace util

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



struct DBIndexer{
    DBIndexer(util::io::H5file const &file, std::string prefix);
    ColumnUID column_uid(wordrep::ChunkIndex idx) const {return chunk2col_uid[idx.val];}
    RowIndex row_idx(wordrep::ChunkIndex idx) const {return chunk2idx[idx.val];}
    RowUID row_uid(wordrep::ChunkIndex idx) const {return chunk2row_uid[idx.val];}
    bool is_empty(ColumnUID uid, RowIndex idx) const {return map_to_uid.find({uid,idx})==map_to_uid.cend();}
    RowUID row_uid(ColumnUID uid, RowIndex idx) const {
        if(is_empty(uid,idx)) return RowUID{-1};
        auto it=map_to_uid.find({uid,idx});
        return it->second;
    }
    wordrep::ChunkIndex chunk_idx(RowUID uid) const {return row_uid2chunk.at(uid);}

    std::vector<RowIndex> chunk2idx;
    std::vector<RowUID> chunk2row_uid;
    std::vector<ColumnUID> chunk2col_uid;
    std::map<std::pair<ColumnUID,RowIndex>,RowUID> map_to_uid;
    std::map<RowUID, wordrep::ChunkIndex> row_uid2chunk;
};


}//namespace data

