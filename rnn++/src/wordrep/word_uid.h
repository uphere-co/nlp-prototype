#include <string>
#include <limits>
#include <unordered_map>
#include <vector>

namespace wordrep{

struct WordUID{
    using idx_t = int64_t;
    WordUID() : val{-1} {} //-1 for unknown words
    WordUID(int64_t val) : val{val}{}
    WordUID(uint64_t uval) : val{static_cast<int64_t>(uval)}{ assert(uval<std::numeric_limits<int64_t>::max());}
    int64_t val;
};
struct WordUIDindex{
    WordUIDindex(std::string file)
            : uid2words{util::string::readlines(file)}{
        auto n = uid2words.size();
        for(decltype(n)i=0;i!=n; ++i)
            word2uid[uid2words[i]]=WordUID{i};
    }
    WordUID operator[](std::string const &word){
        return word2uid[word];
    }
    std::string operator[](WordUID  const &uid){
        return uid2words[uid.val];
    }

    std::vector<std::string> uid2words;
    std::unordered_map<std::string, WordUID> word2uid;
};

}//namespace wordrep
