#pragma once

#include "utils/parallel.h"
#include "utils/algorithm.h"

namespace util {

template<typename TK, typename TV, typename TH>
auto to_map(tbb::concurrent_hash_map<TK,TV,TH> const& src){
    std::map<TK,TV> out;
    for(auto const& elm : src){
        out[elm.first] = elm.second;
    }
    return out;
}

template<typename TK, typename TV, typename TH>
auto to_vector_pair(tbb::concurrent_hash_map<TK,TV,TH> const& src){
    std::pair<std::vector<TK>,std::vector<TV>> out;
    for(auto const& elm : src){
        out.first.push_back(elm.first);
        out.second.push_back(elm.second);
    }
    return out;
}
template<typename TK, typename TV, typename TH>
auto to_sorted_pairs(tbb::concurrent_hash_map<TK,TV,TH> const& src){
    auto out = to_pairs(src);
    std::sort(out.begin(),out.end(),[](auto x, auto y){return x.first<y.first;});
    return out;
}

template<typename TK, typename TV, typename TH>
auto to_pairs(tbb::concurrent_hash_map<TK,TV,TH> const& src){
    std::vector<std::pair<TK,TV>> out;
    for(auto const& elm : src){
        out.push_back(elm);
    }
    return out;
}


template<typename T>
auto binary_find(tbb::concurrent_vector<T> const &vs, T val){
    auto beg = vs.cbegin();
    auto end = vs.cend();
    return binary_find(beg,end,val);
}

template<typename T, typename TE, typename TL>
auto binary_find(tbb::concurrent_vector<T> const &vs, TE const& eq, TL const& less){
    auto beg = vs.cbegin();
    auto end = vs.cend();
    return binary_find(beg,end,eq, less);
}
template<typename T, typename TE, typename TL>
auto binary_find_block(tbb::concurrent_vector<T> const &vs, TE const& eq, TL const& less){
    auto beg = vs.cbegin();
    auto end = vs.cend();
    return binary_find_block(beg,end,eq, less);
}
template<typename T>
auto binary_find_block(tbb::concurrent_vector<T> const &vs, T val){
    auto eq   = [val](auto x){return val==x;};
    auto less = [val](auto x){return val<x;};
    auto beg = vs.cbegin();
    auto end = vs.cend();
    return binary_find_block(beg,end,eq, less);
}


}//namespace util

