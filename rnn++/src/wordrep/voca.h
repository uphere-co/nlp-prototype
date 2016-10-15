#pragma once
#include <unordered_map>

#include "utils/base_types.h"
#include "wordrep/word_uid.h"

namespace wordrep{
struct VocaIndexDummy{};
using VocaIndex = util::IntegerLike<VocaIndexDummy,-1>; //UID -1 for unknown words.

class VocaIndexMap{
public:
    using idx_t = WordUID;
    VocaIndexMap(std::vector<idx_t::val_t> const &uids_val);
    VocaIndex operator[](idx_t uid) const;
    idx_t operator[](VocaIndex idx) const;

    bool isin(idx_t uid) {return uid2idx[uid]!=VocaIndex{};}
//private:
    std::unordered_map<idx_t, VocaIndex> uid2idx;
    std::unordered_map<VocaIndex, idx_t> idx2uid;
};

std::vector<VocaIndexMap::idx_t::val_t> load_voca(std::string h5name, std::string voca_name);
}//namespace wordrep