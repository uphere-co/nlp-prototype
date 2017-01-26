#pragma once

#include <string>
#include <vector>

#include <xxhashct/xxh64.hpp>

namespace util {
//For computer security reasons, xxh64 needs a seed value.
//For our use case(use hash as unique ID of each word), a global, fixed number will do its job.
constexpr size_t xxh64_seed = 113377;

template<typename T>
uint64_t hash(T* ptr, size_t len){
    return xxh64::hash(reinterpret_cast<const char*>(ptr), sizeof(T)*len, xxh64_seed);
}

inline uint64_t hash(std::string const& str){
    return xxh64::hash(str.data(), str.size(), xxh64_seed);
};
template<typename T>
uint64_t hash(std::vector<T> const& vec){
    return xxh64::hash(reinterpret_cast<const char*>(vec.data()), sizeof(T)*vec.size(), xxh64_seed);
};

template<typename T>
uint64_t hash(T const& x){
    return xxh64::hash(reinterpret_cast<const char*>(&x), sizeof(x), xxh64_seed);
};

}//namespace wordrep
