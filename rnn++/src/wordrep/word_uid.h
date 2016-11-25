#pragma once

#include <string>
#include <unordered_map>
#include <vector>

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
    uid_t insert(std::string const &word) {
        auto it=word2uid.find(word);
        if(it!=word2uid.cend()) return it->second;
        uid2word[current_idx]=word;
        word2uid[word] = current_idx;
        ++current_idx;
        return current_idx;
    }
    std::string operator[](uid_t uid) const {
        //return uid2word[uid];
        auto it=uid2word.find(uid);
        if(it==uid2word.cend()) it = uid2word.find(uid_t{});
        return it->second;
    }
    void write_to_disk (std::string filename) const;
    auto size() const {return uid2word.size();}

private:
    std::unordered_map<uid_t, std::string> uid2word;
    std::unordered_map<std::string, uid_t> word2uid;
    uid_t current_idx;
};


//forward declarations.
struct DummyWordUID{};
using WordUID = util::IntegerLike<DummyWordUID,-1>; //UID -1 for unknown words.
using WordUIDindex = UIDIndex<WordUID>;

struct DummyPOSUID{};
using POSUID = util::IntegerLike<DummyPOSUID,-1>; //UID -1 for unknown words.
using POSUIDindex = UIDIndex<POSUID>;
struct DummyArcLabelUID{};
using ArcLabelUID = util::IntegerLike<DummyArcLabelUID,-1>; //UID -1 for unknown words.
using ArcLabelUIDindex = UIDIndex<ArcLabelUID>;

}//namespace wordrep
