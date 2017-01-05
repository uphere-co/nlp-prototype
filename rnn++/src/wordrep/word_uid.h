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
    uid_t operator[] (std::string const &word) const;
    std::string operator[](uid_t uid) const;
    void write_to_disk (std::string filename) const;
    auto size() const {return uid2word.size();}

private:
    uid_t insert(std::string const &word);
    std::unordered_map<uid_t, std::string> uid2word;
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
