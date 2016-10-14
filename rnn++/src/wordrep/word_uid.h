#pragma once

#include <string>
#include <unordered_map>
#include <vector>

namespace wordrep {

struct WordUID {
    using idx_t = int64_t;

    WordUID() : val{-1} {} //-1 for unknown words. This is a default value for WordUIDindex.
    WordUID(int64_t val) : val{val} {}

    WordUID(uint64_t uval);

    int64_t val;
};
}//namespace wordrep

namespace std {
template<>
struct hash<wordrep::WordUID> {
    size_t operator()(wordrep::WordUID const& uid) const {
        auto val = uid.val;
        return hash<decltype(val)>{}(val);
    }
};
template<>
struct equal_to<wordrep::WordUID> {
    constexpr bool operator()(wordrep::WordUID const& lhs, wordrep::WordUID const & rhs) const{
        return lhs.val == rhs.val;
    }
};
}//namespace std

namespace wordrep {

struct WordUIDindex{
    WordUIDindex(std::string file);
    WordUID operator[] (std::string const &word) {
        return word2uid[word];
    }
    std::string operator[](WordUID uid) {
        return uid2word[uid];
    }

    std::unordered_map<WordUID, std::string> uid2word;
    std::unordered_map<std::string, WordUID> word2uid;
};

}//namespace wordrep
