#include "data_source/rss.h"

#include <fmt/printf.h>

#include "similarity/dataset.h"

#include "utils/string.h"
#include "utils/flatbuffers/io.h"

#include "wordrep/dep_parsed.h"

#include "data_source/ygp_db.h" //TODO: some indexes should be separated out from YGP ETL.
#include "data_source/ygp_etl.h" //TODO: some ETL function should be separated out from YGP ETL.

using wordrep::Sentence;

namespace data{
namespace rss{

void write_column_indexes(std::string column_list_file,
                          std::string row_rawfiles,
                          std::vector<size_t> const &idxs,
                          std::string output_prefix){
    std::vector<ColumnUID> col_uids;
    std::vector<RowIndex> row_idxs;
    std::vector<RowUID> row_uids;

    Columns rssdb{column_list_file};

    RowUID row_uid{};
    auto files = util::string::readlines(row_rawfiles);
    for(auto idx : idxs){
        auto file_path = files[idx];
        RSSRowFilePath row{file_path};
        if(util::string::read_whole(file_path).size()<2){
            fmt::print(std::cerr, "Empty file : {}\n", file_path);
            continue;
        }
        auto col_uid = rssdb.col_uid(row.column);
        RowIndex row_idx{row.index};

        col_uids.push_back(col_uid);
        row_idxs.push_back(row_idx);
        row_uids.push_back(row_uid);

        ++row_uid;
    }
    util::io::to_file(util::serialize(row_uids), {output_prefix + ".chunk2row.i64v"});
    util::io::to_file(util::serialize(row_idxs), {output_prefix + ".chunk2row_idx.i64v"});
    util::io::to_file(util::serialize(col_uids), {output_prefix + ".chunk2col.i64v"});
}


void annotation_on_result(util::json_t const& config, util::json_t &answers){
    auto rawtext_dir     = util::get_str(config,"rawtext_dir");
    Columns rssdb{config["column_uids_dump"].get<std::string>()};
    for(auto &answer : answers){
        auto col_uids = answer["result_column_uid"];
        auto row_idxs = answer["result_row_idx"];
        auto offsets = answer["result_offset"];
        auto n = col_uids.size();
        for(decltype(n)i=0; i!=n; ++i){
            ColumnUID col_uid{col_uids[i].get<ColumnUID::val_t>()};
            RowIndex  row_idx{row_idxs[i].get<RowIndex::val_t>()};
            auto offset_beg = offsets[i][0].get<int64_t>();
            auto offset_end = offsets[i][1].get<int64_t>();
            auto table_name  = rssdb.table(col_uid);
            auto column_name = rssdb.column(col_uid);
            RSSRowFilePath row_elm{table_name,column_name, row_idx.val};
            auto row_str = util::string::read_whole(lookup_file(rawtext_dir, row_elm).value());
            auto substr = util::string::substring_unicode_offset(row_str, offset_beg, offset_end);
            answer["result_DEBUG"].push_back(substr);
            answer["result_row_DEBUG"].push_back(row_str);
        }
    }
}


Columns::Columns(std::string column_uids){
    auto lines = util::string::readlines(column_uids);
    assert(ColumnUID{}==ColumnUID{0});
    for(auto line : lines){
        auto cols = util::string::split(line, ".");
        tables.push_back(cols[0]);
        columns.push_back(cols[1]);
        index_cols.push_back(cols[2]);
        full_names.push_back(line);
    }
}
ColumnUID Columns::col_uid(std::string name) const{
    auto beg = columns.cbegin();
    auto it= std::find(beg, columns.cend(), name);
    return it - beg;
}

}//namespace rss::data
}//namespace data
