#include "data_source/corenlp_utils.h"

#include "data_source/corenlp.h"

#include "utils/parallel.h"
#include "utils/string.h"

namespace data {

jsons_t parallel_load_jsons(std::string file_names){
    auto files = util::string::readlines(file_names);
    auto n = files.size();
    jsons_t jsons;
    tbb::parallel_for(decltype(n){0}, n, [&](auto i) {
        auto const &file = files[i];
        jsons.push_back(data::CoreNLPjson{file});
    });
    return jsons;
}

StrCount parallel_word_count(jsons_t const &jsons){
    StrCount wc;
    for(auto const &json : jsons){
        json.iter_tokens([&](auto const &token){
            auto word = token["word"].template get<std::string>();
            ++wc.val[word];
        });
    }
    return wc;
}

}//namespace data
