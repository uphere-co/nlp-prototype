#pragma once
#include <vector>
#include <string>

namespace util{
namespace string{

std::vector<std::string> split(std::string words, const char *delim=" \t");
std::vector<std::string> readlines(std::string file);

}//namespace util::string
}//namespace util
