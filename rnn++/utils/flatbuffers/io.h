#pragma once

#include <memory>

#include "utils/flatbuffers/pairs_generated.h"
#include "utils/parallel.h"

namespace util {
namespace io {
namespace fb {

struct PairsBinary{
    std::string name;
};

inline bool operator<(Pair x, Pair y) {
    return x.key() < y.key();
}
inline bool operator==(Pair x, Pair y) {
    return x.key() == y.key();
}

void to_file(std::vector<Pair> const& vals, PairsBinary file);
std::unique_ptr<char[]> load_binary_file(PairsBinary file);

template<typename T>
tbb::concurrent_vector<T> deserialize_pairs(std::unique_ptr<char[]> data){
    namespace fb = util::io::fb;
    auto rbuf = fb::GetPairs(data.get());
    auto& properties_buf = *rbuf->vals();
    auto n = properties_buf.size();
    tbb::concurrent_vector<T> properties;
    properties.resize(n,{-1,-1});

    tbb::parallel_for(decltype(n){0},n, [&properties_buf,&properties](auto i) {
        auto it=properties_buf[i];
        properties[i]={it->key(), it->value()};
    });
    return properties;
}

}//namespace util::io::fb
}//namespace util::io
}//namesapce util
