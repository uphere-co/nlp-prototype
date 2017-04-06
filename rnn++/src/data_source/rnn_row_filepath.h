#pragma once

#include <string>
#include "data_source/indexes.h"
#include "wordrep/indexes.h"

namespace data{
namespace rss{

struct RSSRowFilePath{
    RSSRowFilePath(std::string full_path);
    RSSRowFilePath(std::string table, std::string column, int64_t index)
            : table{table}, column{column}, index{index}
    {}
    friend bool operator==(RSSRowFilePath const& a, RSSRowFilePath const& b){
        return a.table==b.table && a.column==b.column && a.index==b.index;
    }
    friend bool operator!=(RSSRowFilePath const& a, RSSRowFilePath const& b){
        return !(a==b);
    }
    std::string full_path;
    std::string table;
    std::string column;
    std::string hash;
    int64_t index;
};

std::optional<std::string> lookup_file(std::string dir_path, data::rss::RSSRowFilePath name);


}//namespace data::rss
}//namespace data
