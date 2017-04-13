#include <memory>
#include <iostream>

#include <fmt/printf.h>

#include "data_source/corenlp_utils.h"
#include "data_source/corenlp.h"

#include "wordrep/word_uid.h"
#include "wordrep/word_prob.h"
#include "wordrep/voca_info.h"

#include "utils/parallel.h"
#include "utils/string.h"
#include "utils/profiling.h"

namespace data {

void CoreNLPoutputParser::operator() (size_t i, CoreNLPjson const &json) {
    wordrep::DepParsedTokens tokens;
    tokens.append_corenlp_output(json);
    chunks[i]=tokens;
}

wordrep::DepParsedTokens CoreNLPoutputParser::serial_parse(std::vector<std::string> dump_files) {
    util::Timer timer;
    wordrep::DepParsedTokens tokens{};
    auto n = dump_files.size();
    //for(auto file : dump_files){
    for(decltype(n)i=0; i<n; ++i){
        auto file = dump_files[i];
        std::cout << fmt::format("{} is being checked", file) << std::endl;
        if(!util::file::is_exist(file)) {
            std::cerr <<fmt::format("{} is not exist", file) << std::endl;
            continue;
        }
        data::CoreNLPjson json{file};
        if(json.val["sentences"].size()==0) {
            std::cerr <<fmt::format("{} has null content", file) << std::endl;
            continue;
        }
        std::cout << fmt::format("{} will be parsed\n", file) << std::endl;
        tokens.append_corenlp_output(json);
        timer.here_then_reset(fmt::format("{} is parsed", file));
        chunks[i]={};
    }
    tokens.build_sent_uid(wordrep::SentUID::from_unsigned(0));
    timer.here_then_reset("Built sent UID.");
    return tokens;
}
std::vector<size_t> CoreNLPoutputParser::get_nonnull_idx() const{
    std::vector<size_t> idxs;
    for(auto elm : chunks) idxs.push_back(elm.first);
    std::sort(idxs.begin(), idxs.end());
    fmt::print("Total {} non-empty chunks\n", idxs.size());
    return idxs;
}
wordrep::DepParsedTokens CoreNLPoutputParser::get() const {
    wordrep::DepParsedTokens tokens{};
    util::Timer timer;
    auto idxs = get_nonnull_idx();
    timer.here_then_reset("Got non-null index.");
    for(auto idx : idxs) tokens.append(chunks.at(idx));
    timer.here_then_reset("Appended all chunks.");
    tokens.build_sent_uid(wordrep::SentUID::from_unsigned(0));
    timer.here_then_reset("Built sent UID.");
    return tokens;
}

void WordCounter::operator()(size_t , CoreNLPjson const &json) {
    StrCount wc;
    json.iter_tokens([&](auto const &token){
        auto word = token["word"].template get<std::string>();
        ++wc.val[word];
    });
    counts.push_back(wc);
}
StrCount WordCounter::get() const {
    StrCount wc;
    for(auto const &count :counts)
        for(auto &x : count.val)
            wc.val[x.first] += x.second;
    return wc;
}


}//namespace data
