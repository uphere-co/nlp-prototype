#pragma once

#include <vector>
#include <limits>

namespace util {

template<typename T>
void append(std::vector<T> &orig, std::vector<T> const &elms) {
    std::copy(elms.cbegin(), elms.cend(), std::back_inserter(orig));
}

template<typename T>
T to_signed(size_t uval){
    assert(uval <std::numeric_limits<T>::max());
    return static_cast<T>(uval);
}

}//namespace util