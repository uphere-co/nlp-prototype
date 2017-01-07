#pragma once
#include <unordered_map>

#include "utils/base_types.h"
#include "wordrep/word_uid.h"

namespace wordrep{
struct VocaIndexDummy{};
using VocaIndex = util::IntegerLike<VocaIndexDummy,-1>; //UID -1 for unknown words.

class VocaIndexMap{
public:
    VocaIndexMap(std::vector<WordUID> const &uids_val);
    VocaIndex operator[](WordUID   uid) const;
    WordUID   operator[](VocaIndex idx) const;

    bool isin(WordUID uid) {return uid2idx.find(uid)!=uid2idx.end();}
private:
    std::unordered_map<WordUID, VocaIndex> uid2idx;
    std::vector<WordUID> uids;
    VocaIndex unknown_word;
};

std::vector<WordUID> load_voca(std::string h5name, std::string voca_name);
}//namespace wordrep