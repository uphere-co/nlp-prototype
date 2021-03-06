#pragma once
#include <string>
namespace util{

struct VersionedName{
    VersionedName(std::string fullname);
    VersionedName(std::string name, int major, int minor);

    std::string name;
    int major;
    int minor;
    std::string fullname;
};
bool operator< (VersionedName const &x, VersionedName const &y);
bool operator== (VersionedName const &x, VersionedName const &y);

std::vector<std::string> get_versioned_files(std::string dir, std::string filename);
VersionedName get_latest_version(std::vector<std::string> files);
VersionedName get_latest_version(std::string data_path);

}//namespace util

