#include <fstream>
#include <experimental/filesystem>

#include "utils/filesystem.h"

namespace util{
namespace file{

bool is_exist(std::string filename){
    std::ifstream f{filename};
    return f.good();
};

std::string get_filename(std::string const& path){
    std::experimental::filesystem::path p{path};
    auto name = p.make_preferred().filename();
    return name;
}
}//namespace util::file
}//namespace util

