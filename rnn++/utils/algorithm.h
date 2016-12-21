#pragma once

#include <tuple>
#include <vector>
#include <limits>
#include <algorithm>
#include <utility>

namespace util {

template<typename T>
void append(std::vector<T> &orig, std::vector<T> const &elms) {
    std::copy(elms.cbegin(), elms.cend(), std::back_inserter(orig));
}

template<typename T1, typename T2>
auto zip(T1 const &x, T2 const &y) {
    using elm_t = std::pair<typename T1::value_type, typename T2::value_type>;
    std::vector<elm_t> elms;
    auto n = std::min(x.size(),y.size());
    for(decltype(n)i=0; i!=n; ++i) elms.push_back({x[i],y[i]});
    return elms;
}

template<typename T1, typename T2, typename T3>
auto zip(T1 const &x, T2 const &y, T3 const &z) {
    using elm_t = std::tuple<typename T1::value_type, typename T2::value_type, typename T3::value_type>;
    std::vector<elm_t> elms;
    auto n = std::min({x.size(),y.size(), z.size()});
    for(decltype(n)i=0; i!=n; ++i) elms.push_back(std::make_tuple(x[i],y[i],z[i]));
    return elms;
}


template<typename T, typename OP>
auto map(T const &elms, OP const &op){
    using TV = typename T::value_type;
    using TM = typename std::result_of<OP(TV)>::type;
    std::vector<TM> vals;
    for(auto const &elm : elms) vals.push_back(op(elm));
    return vals;
};

template<typename T>
T to_type(size_t uval){
    assert(uval <std::numeric_limits<T>::max());
    return static_cast<T>(uval);
}

template<typename T>
T to_signed_positive(size_t u){
    T s = u;
    if(s<0) return -s;
    return s;
}

template<typename T>
auto find(T const &elms, typename T::value_type const &elm){
    return std::find(elms.cbegin(), elms.cend(), elm);
}

template<typename T>
auto isin(T const &elms, typename T::value_type const &elm){
    return find(elms, elm)!=elms.cend();
}


template<typename T>
auto map_to_vectors(T const &wcs){
    using TK = typename T::key_type;
    using TV = typename T::mapped_type;
    std::vector<TK> keys;
    std::vector<TV> vals;
    for(auto x : wcs) {
        keys.push_back(x.first);
        vals.push_back(x.second);
    }
    return std::make_pair(keys, vals);
}

template<typename T>
auto map_to_pairs(T const &wcs){
    using TK = typename T::key_type;
    using TV = typename T::mapped_type;
    std::vector<std::pair<TK,TV>> vec;
    for(auto x : wcs) vec.push_back(x);
    return vec;
}
template<typename T>
auto sort_by_values(T const &wcs){
    auto vals = map_to_pairs(wcs);
    std::sort(vals.begin(), vals.end(), [](auto x, auto y){return x.second>y.second;});
    return vals;
}
}//namespace util
