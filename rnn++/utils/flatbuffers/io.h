#pragma once

#include <memory>

#include "utils/flatbuffers/pairs_generated.h"
#include "utils/flatbuffers/i64vec_generated.h"
#include "utils/flatbuffers/f32vec_generated.h"

#include "utils/parallel.h"

namespace util {
namespace io {
namespace fb {

struct PairsBinary{
    std::string name;
};
struct I64Binary{
    std::string name;
};
struct F32Binary{
    std::string name;
};

inline bool operator<(Pair x, Pair y) {
    return x.key() < y.key();
}
inline bool operator==(Pair x, Pair y) {
    return x.key() == y.key();
}


void to_file(std::vector<Pair> const& vals, PairsBinary file);
void to_file(std::vector<int64_t> const& vals, I64Binary file);
void to_file(std::vector<float> const& vals, F32Binary file);

std::unique_ptr<char[]> load_binary_file(std::string file);
inline std::unique_ptr<char[]> load_binary_file(PairsBinary file){
    return load_binary_file(file.name);
};

template<typename T>
void deserialize_pairs(std::unique_ptr<char[]> data, tbb::concurrent_vector<T>& pairs){
    namespace fb = util::io::fb;
    auto rbuf = fb::GetPairs(data.get());
    auto& properties_buf = *rbuf->vals();
    auto n = properties_buf.size();

    pairs.resize(n,{-1,-1});
    tbb::parallel_for(decltype(n){0},n, [&properties_buf,&pairs](auto i) {
        auto it=properties_buf[i];
        pairs[i]={it->key(), it->value()};
    });
}

template<typename T>
auto deserialize_pairs(std::unique_ptr<char[]> data){
    auto pairs = std::make_unique<tbb::concurrent_vector<T>>();
    deserialize_pairs(std::move(data), *pairs);
    return pairs;
}

}//namespace util::io::fb
}//namespace util::io
}//namesapce util
