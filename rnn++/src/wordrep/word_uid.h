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
    uid_t operator[] (std::string const &word) {
        return word2uid[word];
    }
    std::string operator[](uid_t uid) {
        return uid2word[uid];
    }

    std::unordered_map<uid_t, std::string> uid2word;
    std::unordered_map<std::string, uid_t> word2uid;
};

}//namespace wordrep
