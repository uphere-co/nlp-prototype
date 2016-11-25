#pragma once
#include <string>
#include <unordered_map>

#include "data_source/corenlp.h"

#include "utils/parallel.h"

namespace data {

struct StrCount{
//    using prr_wcounts_t = tbb::concurrent_unordered_map<std::string,size_t>;
    using wcounts_t = std::unordered_map<std::string,size_t>;
    wcounts_t val;
};


using jsons_t = tbb::concurrent_vector<data::CoreNLPjson>;
jsons_t parallel_load_jsons(std::string file_names);
StrCount parallel_word_count(jsons_t const &jsons);

}//namespace data
