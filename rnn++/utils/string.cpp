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

std::vector<char> pack_words(std::vector<std::string> const &words){
    std::vector<char> vec;
    for(auto const &x:words){
        std::copy(x.cbegin(),x.cend(),std::back_inserter(vec));
        vec.push_back('\0');
    }
    return vec;
}

std::vector<std::string> unpack_words(std::vector<char> const &concat_words){
    std::vector<std::string> words;
    auto it =concat_words.cbegin();
    auto end=concat_words.cend();
    while(it!=end){
        words.push_back(std::string{&(*it)});
        it=std::find(it, end, '\0');
        ++it;
    }
    return words;
}

std::vector<const char *> unpack_word_views(std::vector<char> const &concat_words){
    std::vector<const char *> words;
    auto it =concat_words.cbegin();
    auto end=concat_words.cend();
    while(it!=end){
        words.push_back(&(*it));
        it=std::find(it, end, '\0');
        ++it;
    }
    return words;
}

}//namespace util::string
}//namespace util
