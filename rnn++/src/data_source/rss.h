#pragma once

#include <string>
#include <vector>
#include <map>

#include "utils/base_types.h"
#include "utils/json.h"

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
                          std::string row_rawfiles);


void annotation_on_result(util::json_t const &config, util::json_t &answers,
                          std::string dumpfile_hashes);

}//namespace rss::data
}//namespace data