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

WordUIDindex::WordUIDindex(std::string file) : uid2words{util::string::readlines(file)}{
    auto n = uid2words.size();
    for(decltype(n)i=0;i!=n; ++i)
        word2uid[uid2words[i]]=WordUID{i};
}

}//namespace wordrep
