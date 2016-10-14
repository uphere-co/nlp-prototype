#pragma once
#include <unordered_map>

#include "utils/base_types.h"
#include "wordrep/word_uid.h"

#include "utils/string.h"

namespace wordrep{
struct VocaIndexDummy{};
using VocaIndex = util::IntegerLike<VocaIndexDummy,-1>; //UID -1 for unknown words.

class VocaIndexMap{
public:
    using idx_t = WordUID;
    VocaIndexMap(std::vector<idx_t::val_t> uids_val) {
        auto n = uids_val.size();
        for(decltype(n)i=0; i!=n; ++i){
            idx_t uid{uids_val[i]};
            VocaIndex idx{i};
            uid2idx[uid]=idx;
            idx2uid[idx]=uid;
        }
    }
    VocaIndex operator[](idx_t uid) const {
        //return uid2idx[uid];
        auto it=uid2idx.find(uid);
        if(it==uid2idx.cend()) return VocaIndex{};
        return it->second;
    }
    idx_t operator[](VocaIndex idx) const {
        //return idx2uid[idx];
        auto it=idx2uid.find(idx);
        if(it==idx2uid.cend()) return idx_t{};
        return it->second;
    }
    std::vector<VocaIndex> getIndex(std::string sentence) const {
        auto tokens = util::string::split(sentence);
        WordUIDindex wordUIDs{"/home/jihuni/word2vec/ygp/words.uid"};
        std::vector<VocaIndex> idxs;
        for(auto const &word : tokens){
            idxs.push_back((*this)[wordUIDs[word]]);
        }
        return idxs;
    }

    bool isin(idx_t uid) {return uid2idx[uid]!=VocaIndex{};}
//private:
    std::unordered_map<idx_t, VocaIndex> uid2idx;
    std::unordered_map<VocaIndex, idx_t> idx2uid;
};

std::vector<VocaIndexMap::idx_t::val_t> load_voca(std::string h5name, std::string voca_name);
}//namespace wordrep