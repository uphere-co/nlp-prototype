#pragma once

#include <string>
#include <vector>

namespace rnn{

struct TokenizedSentences{
    using c_iter = std::vector<std::string>::const_iterator;
    TokenizedSentences(std::string tokenized_file);
    std::vector<std::string> val;
};
struct ParsedSentences{
    using c_iter = std::vector<std::string>::const_iterator;
    ParsedSentences(std::string parsed_file);
    std::vector<std::string> val;
};

struct SentencePair{
    SentencePair(std::string const &parsed, std::string const &original)
            :parsed{parsed},original{original} {}
    std::string parsed=parsed;
    std::string original=original;
};
struct SentencePairs{
    using val_t = std::vector<SentencePair>;
    using c_iter = val_t::const_iterator;
    SentencePairs(ParsedSentences const & sentences1, TokenizedSentences const &sentences2);
    val_t val;
};

}//namespace rnn