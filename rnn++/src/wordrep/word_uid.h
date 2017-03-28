#pragma once

#include <string>
#include <unordered_map>
#include <vector>

#include "wordrep/indexes.h"

namespace wordrep {

template<typename TUID>
class UIDIndex{
public:
    using uid_t = TUID;
    UIDIndex(std::string file);
    bool isin(uid_t uid) const;
    uid_t operator[] (std::string const &word) const;
    std::string operator[](uid_t uid) const;
    uid_t uid(std::string const &word) const;
    std::string str(uid_t uid) const;
    void write_to_disk (std::string filename) const;
    auto size() const {return uid2word.size();}
    std::vector<uid_t> get_uids() const;

    static uid_t get_uid(std::string const &word);
private:
//    std::unordered_map<uid_t, std::string> uid2word;
    std::vector<std::pair<uid_t,std::string>> uid2word;
    uid_t current_idx;
};

//forward declarations.
using WordUIDindex = UIDIndex<WordUID>;
using POSUIDindex = UIDIndex<POSUID>;
using ArcLabelUIDindex = UIDIndex<ArcLabelUID>;
using WikidataUIDindex = UIDIndex<WikidataUID>;

std::string the_unknown_word();
WordUID the_unknown_word_uid();

}//namespace wordrep
