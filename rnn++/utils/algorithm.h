#pragma once

#include <vector>
#include <limits>
#include <algorithm>

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

template<typename T>
auto find(T const &elms, typename T::value_type const &elm){
    return std::find(elms.cbegin(), elms.cend(), elm);
}

template<typename T>
auto isin(T const &elms, typename T::value_type const &elm){
    return find(elms, elm)!=elms.cend();
}
}//namespace util
