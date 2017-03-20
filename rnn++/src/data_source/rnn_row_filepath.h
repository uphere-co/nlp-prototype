#pragma once

#include <string>
#include "data_source/indexes.h"

namespace data{
namespace rss{

struct RSSRowFilePath{
    RSSRowFilePath(std::string full_path);
    friend bool operator==(RSSRowFilePath const& a, RSSRowFilePath const& b){
        return a.table==b.table && a.column==b.column && a.index==b.index;
    }
    friend bool operator!=(RSSRowFilePath const& a, RSSRowFilePath const& b){
        return !(a==b);
    }
    std::string table;
    std::string column;
    int64_t index;
};

std::string get_row_filename(std::string table, std::string column, int64_t index);


}//namespace data::rss
}//namespace data
