#include "wordrep/word_hash.h"

namespace wordrep{

template<>
uint64_t hash<std::string>(std::string const& str) {
    return xxh64::hash(str.data(), str.size(), xxh64_seed);
}

}//namespace wordrep;