#include <experimental/filesystem>

#include "utils/versioned_name.h"

#include "utils/string.h"
#include "utils/base_types.h"

namespace util{


VersionedName::VersionedName(std::string fullname){
    auto tokens = util::string::split(fullname, ".");
    minor = std::stoi(tokens.back());
    tokens.pop_back();
    major = std::stoi(tokens.back());
    tokens.pop_back();
    name = util::string::join(tokens, ".");
}
bool operator< (VersionedName const &x, VersionedName const &y){
    if(x.major< y.major) return true;
    if(x.major==y.major && x.minor<y.minor) return true;
    return false;
}
bool operator== (VersionedName const &x, VersionedName const &y){
    return x.name==y.name && x.major==y.major && x.minor==y.minor;
}

std::vector<std::string> get_files(std::string dir, std::string filename){
    namespace fs = std::experimental::filesystem;
    auto start_with=[](std::string str, std::string expr){
        return str.substr(0, expr.size())==expr;
    };
    std::vector<std::string> files;
    for(auto& p: fs::directory_iterator(dir)){
        auto path = fs::path(p);
        auto file = path.filename();
        if(!start_with(file, filename)) continue;
        files.push_back(file);
    }
    return files;
}
VersionedName get_latest_version(std::vector<std::string> files){
    auto vers = util::deserialize<VersionedName>(files);
    std::sort(vers.begin(), vers.end());
    return vers.back();
}
VersionedName get_latest_version(std::string data_path){
    namespace fs = std::experimental::filesystem;
    auto data_dir  = fs::path(data_path).parent_path();
    auto data_file = fs::path(data_path).filename();
    auto files = get_files(data_dir, data_file);
    return get_latest_version(files);
}
}//namespace util