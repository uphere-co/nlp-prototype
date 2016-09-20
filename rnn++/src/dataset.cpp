#include "dataset.h"

#include "utils/string.h"

namespace rnn {

TokenizedSentences::TokenizedSentences(std::string tokenized_file)
        : val{util::string::readlines(tokenized_file)} {}

ParsedSentences::ParsedSentences(std::string parsed_file)
        : val{util::string::readlines(parsed_file)} {}

SentencePairs::SentencePairs(ParsedSentences const & sentences1, TokenizedSentences const &sentences2){
    auto n = sentences1.val.size();
    assert(n==sentences2.val.size());
    for(decltype(n)i=0; i<n; ++i){
        val.push_back(SentencePair{sentences1.val[i],sentences2.val[i]});
    }
}
}//namespace rnn
