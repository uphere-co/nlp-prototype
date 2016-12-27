#pragma once
#include <vector>
#include <string>

#include "utils/span.h"
#include "utils/optional.h"

namespace util{
namespace string{

std::vector<std::string> split(std::string words, const char *delim=" \t");
std::string join(std::vector<std::string> words, std::string delim);
std::string strip(std::string const& str);

std::vector<std::string> readlines(std::string file);
std::string read_whole(std::string file);
template<typename T>
std::optional<std::vector<char>> read_chunk(T &is, int64_t n_buf){
    std::vector<char> buffer(n_buf);
    is.read(buffer.data(), buffer.size());
    if(!is.gcount()) return {};
    if(is.gcount()==n_buf){
        for(char c=is.get(); is&&c!='\n'; c=is.get())
            buffer.push_back(c);
    }
    buffer.push_back('\0');
    return buffer;
}

std::vector<char> pack_words(std::vector<std::string> const &words);
std::vector<char> pack_words(std::vector<const char*> const &words);

std::vector<std::string> unpack_words(std::vector<char> const &concat_words);
std::vector<const char *> unpack_word_views(std::vector<char> const &concat_words);

std::vector<util::cstring_span<>> unpack_tokenized_sentence(util::cstring_span<> sentence);

std::string substring_unicode_offset(std::string str, int beg, int end);


}//namespace util::string
}//namespace util
