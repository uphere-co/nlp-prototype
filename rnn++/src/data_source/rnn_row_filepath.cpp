#include "data_source/rss.h"

#include <experimental/filesystem>
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
    //tokens[2] is a hash of the article's URL
    column = tokens[3];
}

std::optional<std::string> lookup_file(std::string dir_path, data::rss::RSSRowFilePath name){
    namespace fs = std::experimental::filesystem;
    fs::directory_iterator dir{dir_path};

    for(fs::path file : dir) {
        data::rss::RSSRowFilePath row{file.filename()};
        if(row==name)
            return file.string();
    }
    return {};
}


}//namespace rss::data
}//namespace data
