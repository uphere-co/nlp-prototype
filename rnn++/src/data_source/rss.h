#pragma once

#include <string>
#include <vector>
#include <map>

#include "data_source/db.h"

#include "similarity/scoring.h"

#include "utils/base_types.h"
#include "utils/json.h"

namespace engine{
struct Dataset;
}

namespace data{
namespace rss{

struct HashIndexerDummy{};
using HashIndex = util::IntegerLike<HashIndexerDummy, -1>;

struct RSSRowFilePath{
    RSSRowFilePath(std::string full_path);
    std::string table;
    std::string column;
    std::string hash;
};

struct HashIndexer{
    HashIndexer(std::string filename);
    std::string hash(HashIndex idx) const {return idx2hash.at(idx.val);}
    HashIndex idx(std::string hash) const {return hash2idx.at(hash);}
    std::vector<std::string> idx2hash;
    std::map<std::string, HashIndex> hash2idx;
};

void write_column_indexes(util::json_t const &config,
                          std::string dumpfile_hashes,
                          std::string row_rawfiles,
                          std::vector<size_t> const &idxs);

void annotation_on_result(util::json_t const &config, util::json_t &answers,
                          std::string dumpfile_hashes);

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
