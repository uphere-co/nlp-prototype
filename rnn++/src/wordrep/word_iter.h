#pragma once

#include <string>
#include <algorithm>

#include "wordrep/word_uid.h"
#include "wordrep/word_hash.h" //TODO:move to .cpp file.


namespace wordrep{

template<typename KEY>
struct TokenHash{
    template<typename T>
    KEY operator() (std::string const &text, T text_beg, T beg, T end) const;
};

template<>
struct TokenHash<std::string>{
    template<typename T>
    std::string operator() (std::string const &text, T text_beg, T beg, T end) const {
        return text.substr(beg-text_beg, end-beg);
    }
    std::string operator() (std::string const &word) const {
        return word;
    }
};

template<>
struct TokenHash<WordUID>{
    template<typename T>
    WordUID  operator() (std::string const &text, T text_beg, T beg, T end) const {
        return WordUID::from_unsigned(hash(text.data()+(beg-text_beg), end-beg));
    }
    WordUID  operator() (std::string const &word) const {
        assert(word.size());
        return WordUID::from_unsigned(hash(word.data(), std::distance(word.cbegin(),word.cend())));
    }
};


template<typename KEY>
struct WordIterBase{
    using key_type    = KEY;
    using hasher_type = TokenHash<KEY>;
    WordIterBase(std::string text)
            : text_strs{std::move(text)}
    {}
    template<typename OP>
    void iter(OP const &op) const {
        auto text_beg = std::cbegin(text_strs);
        auto text_end = std::cend(text_strs);
        auto beg = std::find_if_not(text_beg, text_end, [](auto x){return std::isspace(x);});
        while(beg!=text_end){
            auto end=std::find_if(beg, text_end, [](auto x){return std::isspace(x);});
            auto word = get_hash(text_strs, text_beg, beg, end);
            op(word);
            if(end==text_end) break;
            beg = std::find_if_not(end, text_end, [](auto x){return std::isspace(x);});
        }
    }

    hasher_type get_hash{};
    std::string text_strs;
};

}//namespace wordrep
