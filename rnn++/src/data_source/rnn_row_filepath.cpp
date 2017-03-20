#include "data_source/rss.h"

#include <fmt/format.h>

#include "utils/string.h"
#include "utils/filesystem.h"

using util::io::h5read;
using wordrep::Sentence;

namespace data{
namespace rss{

//example path : NYT.10.maintext.corenlp
RSSRowFilePath::RSSRowFilePath(std::string full_path) {
    auto filename = util::file::get_filename(full_path);
    auto tokens = util::string::split(filename, ".");
    table = tokens[0];
    index   = std::stoi(tokens[1]);
    column = tokens[2];
    assert(tokens.size()==4&&tokens[3]=="corenlp");
}

std::string get_row_filename(std::string table, std::string column, int64_t index){
    return fmt::format("{}.{}.{}.corenlp", table,index,column);
}

}//namespace rss::data
}//namespace data
