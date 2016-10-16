#pragma once

#include <string>
#include <unordered_map>
#include <vector>

namespace wordrep{

struct WordUID{
    using idx_t = int64_t;
    WordUID() : val{-1} {} //-1 for unknown words. This is a default value for WordUIDindex.
    WordUID(int64_t val) : val{val}{}
    WordUID(uint64_t uval);
    int64_t val;
};

struct WordUIDindex{
    WordUIDindex(std::string file);
    WordUID operator[](std::string const &word){
        return word2uid[word];
    }
    std::string operator[](WordUID  const &uid){
        if(uid.val<0) return "-UNKNOWN-";
        return uid2word[uid.val];
    }

    std::vector<std::string> uid2word;
    //TODO: implements hash and equal_to for WordUID to define std::unordered_map
//    std::unordered_map<WordUID, std::string> uid2words;
    std::unordered_map<std::string, WordUID> word2uid;
};

}//namespace wordrep
