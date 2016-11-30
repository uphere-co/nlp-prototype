#include <memory>
#include <iostream>

#include "data_source/corenlp_utils.h"

#include "data_source/corenlp.h"

#include "wordrep/word_uid.h"
#include "wordrep/word_prob.h"
#include "wordrep/voca_info.h"

#include "utils/parallel.h"
#include "utils/string.h"
#include "utils/profiling.h"

namespace data {

CoreNLPoutputParser::CoreNLPoutputParser(util::json_t const &config)
: voca{config["wordvec_store"], config["voca_name"],
       config["w2vmodel_name"], config["w2v_float_t"]},
  wordUIDs{config["word_uids_dump"].get<std::string>()},
  posUIDs{config["pos_uids_dump"].get<std::string>()},
  arclabelUIDs{config["arclabel_uids_dump"].get<std::string>()}
{}

void CoreNLPoutputParser::operator() (size_t i, CoreNLPjson const &json) {
    wordrep::DepParsedTokens tokens;
    tokens.append_corenlp_output(wordUIDs, posUIDs, arclabelUIDs, json);
    chunks[i]=tokens;
}
wordrep::DepParsedTokens CoreNLPoutputParser::get(std::string prefix) const {
    wordrep::DepParsedTokens tokens{prefix};
    std::vector<size_t> idxs;
    for(auto elm : chunks) idxs.push_back(elm.first);
    std::sort(idxs.begin(), idxs.end());
    for(auto idx : idxs) tokens.append(chunks.at(idx));
    tokens.build_sent_uid(wordrep::SentUID::from_unsigned(0));
    tokens.build_voca_index(voca.indexmap);
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
