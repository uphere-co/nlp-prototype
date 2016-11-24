#include "data_source/corenlp_utils.h"

#include "data_source/corenlp.h"

#include "utils/parallel.h"
#include "utils/string.h"

namespace data {

StrCount parallel_word_count(std::string file_names){
    StrCount wc;
    auto files = util::string::readlines(file_names);

    auto n = files.size();
    tbb::concurrent_vector<data::CoreNLPjson> jsons;
    tbb::parallel_for(decltype(n){0}, n, [&](auto i) {
        auto const &file = files[i];
        jsons.push_back(data::CoreNLPjson{file});
    });
    for(auto const &json : jsons){
        json.iter_tokens([&](auto const &token){
            auto word = token["word"].template get<std::string>();
            ++wc.val[word];
        });
    }
    return wc;
}

}//namespace data
