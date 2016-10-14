#include <string>
#include <unordered_map>
#include <vector>
#include <limits>

#include "wordrep/word_uid.h"

#include "utils/string.h"

namespace wordrep{

WordUID::WordUID(uint64_t uval) : val{static_cast<int64_t>(uval)}{
    assert(uval<std::numeric_limits<int64_t>::max());
}

WordUIDindex::WordUIDindex(std::string file) {
    auto words = util::string::readlines(file);
    auto n = words.size();
    for(decltype(n)i=0;i!=n; ++i) {
        uid2word[WordUID{i}]=words[i];
        word2uid[words[i]] = WordUID{i};
    }
    uid2word[WordUID{}]="-UNKNOWN-";
}

}//namespace wordrep
