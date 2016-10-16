#pragma once

#include <string>
#include <unordered_map>
#include <vector>


#include "utils/base_types.h"
namespace wordrep {

struct DummyWordUID{};
using WordUID = util::IntegerLike<DummyWordUID,-1>; //UID -1 for unknown words.

struct WordUIDindex{
    using uid_t = WordUID;
    WordUIDindex(std::string file);
    uid_t operator[] (std::string const &word) const {
        //return word2uid[word];
        auto it=word2uid.find(word);
        if(it==word2uid.cend()) return uid_t{};
        return it->second;
    }
    std::string operator[](uid_t uid) const {
        //return uid2word[uid];
        auto it=uid2word.find(uid);
        if(it==uid2word.cend()) it = uid2word.find(uid_t{});
        return it->second;
    }

    std::unordered_map<uid_t, std::string> uid2word;
    std::unordered_map<std::string, uid_t> word2uid;
};

}//namespace wordrep
