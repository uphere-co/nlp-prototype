#include "utils/string.h"

#include <sstream>
#include <fstream>
#include <algorithm>
#include <functional>
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
        it=std::find_if_not(it, end, [](auto x){return x=='\0';});
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
        it=std::find_if_not(it, end, [](auto x){return x=='\0';});
    }
    return words;
}

std::vector<util::cstring_span<>> unpack_tokenized_sentence(util::cstring_span<> sentence){
    util::cstring_span<> sent{sentence};
    std::vector<util::cstring_span<>> words;
    auto beg = sent.cbegin();
    auto end = sent.cend();
    auto it=beg;
    while(it!=end){
        auto word_beg=it-beg;
        it=std::find(it, end, ' ');
        auto word_end=it-beg;
        words.push_back(sent.subspan(word_beg, word_end-word_beg));
        it=std::find_if_not(it, end, [](auto x){return x==' ';});
    }
    return words;
}

}//namespace util::string
}//namespace util
