#pragma once

#include <string>
#include <unordered_map>
#include <vector>



#include <string>
#include <unordered_map>
#include <vector>
#include <limits>

#include "wordrep/word_uid.h"

#include "utils/string.h"




#include "utils/base_types.h"
namespace wordrep {


template<typename TUID>
class UIDIndex{
public:
    using uid_t = TUID;
    UIDIndex(std::string file);
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

private:
    std::unordered_map<uid_t, std::string> uid2word;
    std::unordered_map<std::string, uid_t> word2uid;
};


//forward declarations.
struct DummyWordUID{};
using WordUID = util::IntegerLike<DummyWordUID,-1>; //UID -1 for unknown words.
using WordUIDindex = UIDIndex<WordUID>;

}//namespace wordrep
