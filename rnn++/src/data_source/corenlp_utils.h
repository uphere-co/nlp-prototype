#pragma once
#include <string>
#include <unordered_map>

#include "data_source/corenlp.h"
#include "wordrep/dep_parsed.h"
#include "wordrep/voca_info.h"

#include "utils/parallel.h"
#include "utils/string.h"

namespace data {

struct StrCount{
//    using prr_wcounts_t = tbb::concurrent_unordered_map<std::string,size_t>;
    using wcounts_t = std::unordered_map<std::string,size_t>;
    wcounts_t val;
};


//using jsons_t = tbb::concurrent_vector<wordrep::DepParsedTokens>;
template<typename OP>
void parallel_load_jsons(std::string file_names, OP &op){
    auto files = util::string::readlines(file_names);
    auto n = files.size();
    tbb::parallel_for(decltype(n){0},n, [&](auto const &i) {
        auto const &file = files[i];
        data::CoreNLPjson json{file};
        op(i, json);
    });
}

struct CoreNLPoutputParser{
    CoreNLPoutputParser(util::json_t const &config);

    void operator() (size_t i, CoreNLPjson const &json);
    wordrep::DepParsedTokens get() const;

    wordrep::VocaInfo voca;
    wordrep::WordUIDindex wordUIDs;
    wordrep::POSUIDindex posUIDs;
    wordrep::ArcLabelUIDindex arclabelUIDs;
    tbb::concurrent_unordered_map<size_t, wordrep::DepParsedTokens> chunks;
};


struct WordCounter{
    void operator() (size_t , CoreNLPjson const &json);
    StrCount get() const;
    tbb::concurrent_vector<StrCount> counts;
};

}//namespace data
