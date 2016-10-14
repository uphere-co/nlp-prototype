#include <string>
#include <unordered_map>
#include <vector>

#include <limits>
#include <cassert>

#include "wordrep/word_uid.h"

#include "utils/string.h"

namespace wordrep{

WordUID::WordUID(uint64_t uval) : val{static_cast<int64_t>(uval)}{
    assert(uval<std::numeric_limits<int64_t>::max());
}

WordUIDindex::WordUIDindex(std::string file) : uid2word{util::string::readlines(file)}{
    auto n = uid2word.size();
    for(decltype(n)i=0;i!=n; ++i) {
//        uid2words[WordUID{i}]=words[i];
        word2uid[uid2word[i]] = WordUID{i};
    }
//    auto unknown=WordUID{};
//    uid2words[unknown.val]="-UNKNOWN-";
}

}//namespace wordrep
