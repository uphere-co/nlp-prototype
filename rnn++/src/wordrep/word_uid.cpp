#include <string>
#include <unordered_map>
#include <vector>
#include <limits>

#include "wordrep/word_uid.h"

#include "utils/string.h"

namespace wordrep{

WordUIDindex::WordUIDindex(std::string file) {
    auto words = util::string::readlines(file);
    auto n = words.size();
    for(decltype(n)i=0;i!=n; ++i) {
        uid2word[uid_t{i}]=words[i];
        word2uid[words[i]] = uid_t{i};
    }
    uid2word[uid_t{}]="-UNKNOWN-";
}

}//namespace wordrep
