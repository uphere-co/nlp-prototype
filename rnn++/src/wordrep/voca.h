#pragma once
#include <unordered_map>

#include "utils/base_types.h"
#include "wordrep/word_uid.h"

namespace wordrep{
struct VocaIndexDummy{};
using VocaIndex = util::IntegerLike<VocaIndexDummy,-1>; //UID -1 for unknown words.

class VocaIndexMap{
public:
    VocaIndexMap(std::vector<WordUID::val_t> uids_val) {
        auto n = uids_val.size();
        for(decltype(n)i=0; i!=n; ++i){
            WordUID uid{uids_val[i]};
            VocaIndex idx{i};
            uid2idx[uid]=idx;
            idx2uid[idx]=uid;
        }
    }
    VocaIndex operator[](WordUID uid) {return uid2idx[uid];}
    WordUID operator[](VocaIndex idx) {return idx2uid[idx];}
    bool isin(WordUID uid) {return uid2idx[uid]!=VocaIndex{};}
//private:
    std::unordered_map<WordUID, VocaIndex> uid2idx;
    std::unordered_map<VocaIndex, WordUID> idx2uid;
};

std::vector<WordUID::val_t> load_voca(std::string h5name, std::string voca_name);
}//namespace wordrep