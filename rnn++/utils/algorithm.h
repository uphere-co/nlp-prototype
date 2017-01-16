#pragma once

#include <set>
#include <tuple>
#include <vector>
#include <limits>
#include <algorithm>
#include <utility>
#include <map>

#include "utils/optional.h"

namespace util {

template<typename T>
auto singed_size(T const& vs){
    return vs.cend()-vs.cbegin();
}

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
    using TM = typename std::result_of<OP(decltype(*(elms.begin())))>::type;
    std::vector<TM> vals;
    vals.reserve(elms.size());
    for(auto const &elm : elms) vals.push_back(op(elm));
    return vals;
};

template<typename T, typename OP>
void apply(T &elms, OP const &op){
    for(auto &elm : elms) elm = op(elm);
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

template<typename T, typename TO>
auto sort(T &elms, TO const& op){
    return std::sort(elms.begin(), elms.end(), op);
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
auto unique_values(std::vector<T> elms){
    std::sort(elms.begin(), elms.end());
    auto last = std::unique(elms.begin(), elms.end());
    elms.erase(last, elms.end());
    return elms;
}

template<typename TC, typename TO>
TC filter(TC const &vs, TO op){
    TC filtered{};
    std::copy_if(std::begin(vs),std::end(vs), std::back_inserter(filtered), op);
    return filtered;
};
template<typename TC, typename TO>
TC& filter_inplace(TC &vs, TO op){
    auto beg = std::begin(vs);
    auto it = std::partition(beg,std::end(vs), op);
    vs.resize(it-beg);
    return vs;
};

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
    auto pairs = to_pairs(src);
    std::sort(pairs.begin(),pairs.end(), [](auto x, auto y){return x.first<y.first;});
    return pairs;
}


template<typename TI, typename TL>
std::optional<TI> binary_find_cell(TI beg, TI last, TL const& comp) {
    if(last-beg==1) return last; //assert(*beg<=val && val<*last);
    auto it = beg + (last-beg)/2;
    if(comp(*it)) return binary_find_cell(beg,it, comp);
    return binary_find_cell(it, last, comp);
}

//comp{val} (x) :: true if val < x;
template<typename T, typename TL>
auto binary_find_cell(std::vector<T> const &vs, TL const& comp)
-> std::optional<decltype(vs.cbegin())> {
    if(vs.empty()) return {};
    else if(!comp(vs.back())) return {};
    auto beg = vs.cbegin();
    auto last = vs.cend()-1;
    if (comp(vs.front())) return beg;
    return binary_find_cell(beg,last,comp);
}
template<typename T>
auto binary_find_cell(std::vector<T> const &vs, T val)
-> std::optional<decltype(vs.cbegin())> {
    if(vs.empty()) return {};
    else if(vs.back()<=val) return {};
    auto beg = vs.cbegin();
    auto last = vs.cend()-1;
    if (vs.front() > val) return beg;
    return binary_find_cell(beg,last,[val](auto x){return val<x;});
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

//less{val}(x) :: true if val < x;
template<typename TI, typename TE, typename TL>
std::optional<TI> binary_find(TI beg, TI end, TE const& eq,  TL const& less) {
    if(beg==end) return {};
    auto it = beg + (end-beg)/2;
    if(eq(*it)) return it;
    else if(end-beg==1) return {};
    else if(less(*it)) return binary_find(beg, it, eq, less);
    return binary_find(it, end, eq, less);
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
                       [key](auto elm){return elm.first>key;});
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
    std::vector<TV> values;
    values.reserve(vs.size());
    for(auto const &elm : vs) values.push_back(elm.second);
    return values;
}

template<typename T, typename TO>
auto max_element(T const& vals, TO op){
    return std::max_element(std::begin(vals),std::end(vals), op);
};
}//namespace util

//Algorithms with function objects.
namespace util{


template<typename T>
struct IterChunkIndex{
    IterChunkIndex(T beg, T end)
            : data_beg{beg}, data_end{end}, now{data_beg}
    {}
    std::optional<std::pair<int64_t,int64_t>> next() {
        if(now==data_end) return {};
        auto val = *now;
        auto it = std::find_if_not(now, data_end,[val](auto x){return val==x;});
        auto chunk = std::make_pair(now-data_beg,it-data_beg);
        now=it;
        return chunk;
    }
    T const data_beg;
    T const data_end;
    T now;
};

template<typename T>
auto IterChunkIndex_factory(T const &vals){
    auto beg = vals.cbegin();
    auto end = vals.cend();
    return IterChunkIndex<decltype(beg)>{beg,end};
}

}//namespace util