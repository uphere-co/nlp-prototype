#pragma once
#include <unordered_map>

#include "utils/base_types.h"
#include "wordrep/word_uid.h"

namespace wordrep{

class VocaIndexMap{
public:
    struct Binary{
        std::string name;
    };
    static VocaIndexMap factory(Binary file);
    VocaIndexMap(std::vector<WordUID> const &uids_val);
    VocaIndex operator[](WordUID   uid) const;
    WordUID   operator[](VocaIndex idx) const;

    bool isin(WordUID uid) {return uid2idx.find(uid)!=uid2idx.end();}
    auto all_words() const {return uids;}
    auto size() const {return uids.size();}
private:
    std::unordered_map<WordUID, VocaIndex> uid2idx;
    std::vector<WordUID> uids;
    VocaIndex unknown_word;
};

}//namespace wordrep