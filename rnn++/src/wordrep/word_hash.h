#pragma once

#include "utils/hash.h"

namespace wordrep {

template<typename T>
uint64_t hash(T* ptr, size_t len){
    return util::hash(ptr, len);
}

inline uint64_t hash(std::string const& str){
    return util::hash(str);
};

}//namespace wordrep
