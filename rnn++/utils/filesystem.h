#pragma once

#include <string>

namespace util{
namespace file{

bool is_exist(std::string filename);
std::string get_filename(std::string const& path);

}//namespace util::file
}//namespace util

