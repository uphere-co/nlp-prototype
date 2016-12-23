#pragma once

#include <tuple>
#include <vector>
#include <limits>
#include <algorithm>
#include <utility>
#include <map>

#include "utils/optional.h"

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


//template<typename T>
//auto map_to_vectors(T const &wcs){
//    using TK = typename T::key_type;
//    using TV = typename T::mapped_type;
//    std::vector<TK> keys;
//    std::vector<TV> vals;
//    for(auto x : wcs) {
//        keys.push_back(x.first);
//        vals.push_back(x.second);
//    }
//    return std::make_pair(keys, vals);
//}
//
//template<typename T>
//auto map_to_pairs(T const &wcs){
//    using TK = typename T::key_type;
//    using TV = typename T::mapped_type;
//    std::vector<std::pair<TK,TV>> vec;
//    for(auto x : wcs) vec.push_back(x);
//    return vec;
//}

template<typename T>
auto sort_by_values(T const &wcs){
    auto vals = map_to_pairs(wcs);
    std::sort(vals.begin(), vals.end(), [](auto x, auto y){return x.second>y.second;});
    return vals;
}


//algorithms for std::vector of std::pair
template<typename TK, typename TV>
auto to_pairs(std::map<TK,TV> const& src){
    std::vector<std::pair<TK,TV>> out;
    for(auto const& elm : src){
        out.push_back(elm);
    }
    return out;
}
template<typename TK, typename TV>
auto to_sorted_pairs(std::map<TK,TV> const& src){
    return to_pairs(src);
}


template<typename TI, typename T>
std::optional<TI> binary_find(TI beg, TI end, T val) {
    if(beg==end) return {};
    auto it = beg + (end-beg)/2;
    if(*it==val) return it;
    else if(end-beg==1) return {};
    else if(*it>val) return binary_find(beg,it, val);
    return binary_find(it, end, val);
}
template<typename TI, typename TE, typename TL>
std::optional<TI> binary_find(TI beg, TI end, TE const& eq,  TL const& less) {
    if(beg==end) return {};
    auto it = beg + (end-beg)/2;
    if(eq(*it)) return it;
    else if(end-beg==1) return {};
    else if(less(*it)) return binary_find(it, end, eq, less);
    return binary_find(beg, it, eq, less);
}

template<typename T>
auto binary_find(std::vector<T> const &vs, T val){
    auto beg = vs.cbegin();
    auto end = vs.cend();
    return binary_find(beg,end,val);
}

template<typename T, typename TE, typename TL>
auto binary_find(std::vector<T> const &vs, TE const& eq, TL const& less){
    auto beg = vs.cbegin();
    auto end = vs.cend();
    return binary_find(beg,end,eq, less);
}
template<typename TK, typename TV>
auto get_elm(std::vector<std::pair<TK,TV>> const &pairs, TK key){
    return binary_find(pairs,
                       [key](auto elm){return elm.first==key;},
                       [key](auto elm){return elm.first<key;});
}
template<typename TK, typename TV>
TV get_val(std::vector<std::pair<TK,TV>> const &pairs, TK key){
    return get_elm(pairs, key).value()->second;
};

template<typename TK, typename TV>
auto get_keys(std::map<TK,TV> const& vs){
    std::vector<TK> keys;
    keys.reserve(vs.size());
    for(auto const &elm : vs) keys.push_back(elm.first);
    return keys;
}
template<typename TK, typename TV>
auto get_values(std::map<TK,TV> const& vs){
    std::vector<TK> values;
    values.reserve(vs.size());
    for(auto const &elm : vs) values.push_back(elm.second);
    return values;
}

}//namespace util
