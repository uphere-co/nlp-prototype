#pragma once
#include <string>
#include <unordered_map>

namespace data {

struct StrCount{
//    using prr_wcounts_t = tbb::concurrent_unordered_map<std::string,size_t>;
    using wcounts_t = std::unordered_map<std::string,size_t>;
    wcounts_t val;
};

StrCount parallel_word_count(std::string file_names);

}//namespace data
