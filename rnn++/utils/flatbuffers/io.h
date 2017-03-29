#pragma once

#include "utils/flatbuffers/pairs_generated.h"

namespace util {
namespace io {
namespace fb {

inline bool operator<(Pair x, Pair y) {
    return x.key() < y.key();
}
inline bool operator==(Pair x, Pair y) {
    return x.key() == y.key();
}

void to_file(std::vector<Pair> const& vals, std::string filename);

}//namespace util::io::fb
}//namespace util::io
}//namesapce util
