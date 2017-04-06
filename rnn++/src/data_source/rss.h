#pragma once

#include <string>
#include <vector>
#include <map>

#include "data_source/db.h"
#include "data_source/rnn_row_filepath.h"

#include "similarity/scoring.h"

#include "utils/base_types.h"
#include "utils/json.h"

namespace engine{
struct Dataset;
}

namespace data{
namespace rss{

void write_column_indexes(std::string column_list_file,
                          std::string row_rawfiles,
                          std::vector<size_t> const &idxs,
                          std::string output_filename);

void annotation_on_result(util::json_t const& config, util::json_t &answers);

struct Columns{
    Columns(std::string column_uids);
    std::string table(ColumnUID idx) const {return tables[idx.val];}
    std::string index_col(ColumnUID idx) const {return index_cols[idx.val];}
    std::string column(ColumnUID idx) const {return columns[idx.val];}
    ColumnUID col_uid(std::string name) const;
    ColumnUID beg() const {return ColumnUID{};}
    ColumnUID end() const {return ColumnUID::from_unsigned(tables.size());}

    std::vector<std::string> tables;
    std::vector<std::string> columns;
    std::vector<std::string> index_cols;
    std::vector<std::string> full_names;
};

}//namespace data::rss
}//namespace data
