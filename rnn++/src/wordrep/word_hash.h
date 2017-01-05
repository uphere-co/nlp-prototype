#pragma once

#include <string>

#include <xxhashct/xxh64.hpp>

namespace wordrep {
//For computer security reasons, xxh64 needs a seed value.
//For our use case(use hash as unique ID of each word), a global, fixed number will do its job.
constexpr size_t xxh64_seed = 113377;

template<typename T>
uint64_t hash(T* ptr, size_t len){
    return xxh64::hash(reinterpret_cast<const char*>(ptr), len, xxh64_seed);
}

template<typename T>
uint64_t hash(T const& str);

}//namespace wordrep
