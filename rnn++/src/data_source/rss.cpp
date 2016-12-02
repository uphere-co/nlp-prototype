#include "data_source/rss.h"

#include <fmt/printf.h>

#include "utils/string.h"
#include "utils/versioned_name.h"

#include "wordrep/dep_parsed.h"

#include "data_source/ygp_db.h" //TODO: some indexes should be separated out from YGP ETL.
#include "data_source/ygp_etl.h" //TODO: some ETL function should be separated out from YGP ETL.

namespace data{
namespace rss{



RSSRowFilePath::RSSRowFilePath(std::string full_path)
        : table{"nyt"}    {
    using util::string::split;
    auto tokens = split(split(full_path, "/").back(), ".");
    auto n = tokens.size();
    column = tokens[n-1];
    hash   = tokens[n-2];
}

HashIndexer::HashIndexer(std::string filename)
        : idx2hash{util::string::readlines(filename)} {
    HashIndex idx{-1};
    for(auto const& hash : idx2hash) hash2idx[hash] = ++idx;
}

void write_column_indexes(util::json_t const &config,
                          std::string dumpfile_hashes,
                          std::string row_rawfiles,
                          std::vector<size_t> const &idxs){
    std::vector<ColumnUID> col_uids;
    std::vector<RowIndex> row_idxs;
    std::vector<RowUID> row_uids;

    HashIndexer hash2idx{dumpfile_hashes};
    std::map<std::string,ColumnUID> col2uid;
    col2uid["title"]    = 0;
    col2uid["summary"]  = 1;
    col2uid["maintext"] = 2;

    RowUID row_uid{};
    auto cols_to_exports = config["column_uids_dump"].get<std::string>();

    auto files = util::string::readlines(row_rawfiles);
    for(auto idx : idxs){
        auto file_path = files[idx];
        RSSRowFilePath row{file_path};
        if(util::string::read_whole(file_path).size()<2){
            fmt::print(std::cerr, "Empty file : {}\n", file_path);
            continue;
        }
        auto col_uid = col2uid.at(row.column);
        RowIndex row_idx{hash2idx.idx(row.hash).val};

        col_uids.push_back(col_uid);
        row_idxs.push_back(row_idx);
        row_uids.push_back(row_uid);

        ++row_uid;
    }

    auto output_name = util::VersionedName{util::get_str(config,"dep_parsed_store"),
                                           wordrep::DepParsedTokens::major_version, 0};
    auto output_filename = output_name.fullname;
    auto prefix = config["dep_parsed_prefix"].get<std::string>();
    data::ygp::write_column(util::serialize(row_uids), output_filename, prefix, ".chunk2row");
    data::ygp::write_column(util::serialize(row_idxs), output_filename, prefix, ".chunk2row_idx");
    data::ygp::write_column(util::serialize(col_uids), output_filename, prefix, ".chunk2col");
}


void annotation_on_result(util::json_t const &config, util::json_t &answers,
                          std::string dumpfile_hashes){
    std::map<ColumnUID,std::string> uid2col;
    uid2col[0] = "title";
    uid2col[1] = "summary";
    uid2col[2] = "maintext";

    HashIndexer hash2idx{dumpfile_hashes};
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

            auto hash = hash2idx.hash(HashIndex{row_idx.val});
            auto column = uid2col.at(col_uid);

            auto row_str = util::string::read_whole(fmt::format("/home/jihuni/word2vec/parsed/{}.{}", hash, column));
            auto substr = util::string::substring_unicode_offset(row_str, offset_beg, offset_end);
            answer["result_DEBUG"].push_back(substr);
            answer["result_row_DEBUG"].push_back(row_str);
        }
    }
}

}//namespace rss::data
}//namespace data