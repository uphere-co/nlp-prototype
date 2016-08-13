#include "utils/string.h"

#include <sstream>
#include <fstream>
#include <boost/algorithm/string.hpp>

namespace util{
namespace string{

std::vector<std::string> split(std::string words, const char *delim){
    std::vector<std::string> tokens;
    boost::split(tokens, words, boost::is_any_of(delim));
    return tokens;
}

std::vector<std::string> readlines(std::string file){
    std::ifstream val{file};
    std::vector<std::string> lines;
    std::string line;
    while(std::getline(val, line)) lines.push_back(line);
    return lines;
}

}//namespace util::string
}//namespace util
