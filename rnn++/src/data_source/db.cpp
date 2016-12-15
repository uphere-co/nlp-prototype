#include "data_source/db.h"

#include "utils/hdf5.h"

using util::io::H5file;
using util::io::H5name;

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



DBIndexer::DBIndexer(H5file const &file, std::string prefix)
        : chunk2idx{util::deserialize<RowIndex>(file.getRawData<int64_t>(H5name{prefix+".chunk2row_idx"}))},
          chunk2row_uid{util::deserialize<RowUID>(file.getRawData<int64_t>(H5name{prefix+".chunk2row"}))},
          chunk2col_uid{util::deserialize<ColumnUID>(file.getRawData<int64_t>(H5name{prefix+".chunk2col"}))}
{
    auto n = chunk2idx.size();
    assert(chunk2row_uid.size()==n);
    assert(chunk2col_uid.size()==n);
    //for(decltype(n)i=0; i!=n; ++i) {
    for(auto it=chunk2idx.cbegin(); it!=chunk2idx.cend(); ){
        auto i = std::distance(chunk2idx.cbegin(), it);
        auto row_idx=*it;
        auto row_uid=chunk2row_uid[i];
        auto col_uid=chunk2col_uid[i];
        map_to_uid[{col_uid,row_idx}]=row_uid;
        it = std::find_if_not(it, chunk2idx.cend(), [it](auto x){return x==*it;});
    }

    for(decltype(n)i=0; i!=n; ++i)
        row_uid2chunk[chunk2row_uid[i]]=wordrep::ChunkIndex::from_unsigned(i);
}
}//namespace data

