#pragma once

#include <memory>
#include <vector>

#include "utils/flatbuffers/pairs_generated.h"
#include "utils/flatbuffers/i64vec_generated.h"
#include "utils/flatbuffers/f32vec_generated.h"
#include "utils/flatbuffers/file_formats.h"

#include "utils/parallel.h"

namespace util {
namespace io {

inline bool operator<(Pair x, Pair y) {
    return x.key() < y.key();
}
inline bool operator==(Pair x, Pair y) {
    return x.key() == y.key();
}


void to_file(flatbuffers::FlatBufferBuilder const& builder, std::string filename);
void to_file(std::vector<Pair> const& vals, PairsBinary&& file);
void to_file(std::vector<int64_t> const& vals, I64Binary&& file);
void to_file(std::vector<float> const& vals, F32Binary&& file);


std::unique_ptr<char[]> load_binary_file(std::string file);

std::vector<int64_t> load_binary_file(I64Binary const& file);
std::vector<float> load_binary_file(F32Binary const& file);

inline std::unique_ptr<char[]> load_binary_file(PairsBinary const& file){
    return load_binary_file(file.name);
}

template<typename T>
void deserialize_i64vector(std::unique_ptr<char[]> data, T& vec){
    assert(vec.empty());
//    static_assert(std::is_same<typename T::value_type, int64_t>::value, "");

    auto rbuf = GetI64Vector(data.get());
    vec.reserve(rbuf->vals()->size());
    for(auto v : *rbuf->vals()) vec.push_back(v);
}
template<typename T>
void deserialize_f32vector(std::unique_ptr<char[]> data, T& vec){
    assert(vec.empty());
//    static_assert(std::is_same<typename T::value_type, float>::value, "");
    auto rbuf = GetF32Vector(data.get());
    vec.reserve(rbuf->vals()->size());
    for(auto v : *rbuf->vals()) vec.push_back(v);
}

template<typename T>
void deserialize_pairs(std::unique_ptr<char[]> data, tbb::concurrent_vector<T>& pairs){
    auto rbuf = GetPairs(data.get());
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

}//namespace util::io
}//namesapce util
