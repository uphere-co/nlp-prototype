#pragma once
#include <vector>
#include <string>

namespace util{
namespace string{

std::vector<std::string> split(std::string words, const char *delim=" \t");
std::vector<std::string> readlines(std::string file);

std::vector<char> pack_words(std::vector<std::string> const &words);
std::vector<std::string> unpack_words(std::vector<char> const &concat_words);
std::vector<const char *> unpack_word_views(std::vector<char> const &concat_words);

}//namespace util::string
}//namespace util
