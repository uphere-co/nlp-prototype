#ifndef UTIL_STRING_STRING
#define UTIL_STRING_STRING
#include <vector>
#include <string>
#include <boost/algorithm/string.hpp>

namespace util{
namespace string{

auto split(std::string words, const char *delim=" \t"){
    std::vector<std::string> tokens;
    boost::split(tokens, words, boost::is_any_of(delim));
    return tokens;
}

}//namespace util::string
}//namespace util

#endif //UTIL_STRING_STRING
