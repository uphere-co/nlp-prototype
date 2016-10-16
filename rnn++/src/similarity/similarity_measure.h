#pragma once

#include "utils/linear_algebra.h"
#include "utils/loop_gen.h"

namespace similarity{

enum class measure{
    angle,
    inner,
    euclidean,
};

template<measure M>
struct Similarity{
    template<typename T, int64_t dim>
    T operator() (util::span_1d<T,dim> v, util::span_1d<T,dim> q) const;
};
template<>
struct Similarity<measure::angle>{
    template<typename T, int64_t dim>
    T operator() (util::span_1d<T,dim> v, util::span_1d<T,dim> q) const{
        using namespace util::math;
        return dot(v,q)/std::sqrt(dot(v,v)*dot(q,q));
    }
};
template<>
struct Similarity<measure::inner>{
    template<typename T, int64_t dim>
    T operator() (util::span_1d<T,dim> v, util::span_1d<T,dim> q) const{
        using namespace util::math;
        return dot(v,q);
    }
};
auto euclidean_distance_i=[](int64_t i, auto &out, auto const &x, auto const &y){
    auto tmp=x[i]-y[i];
    out += tmp*tmp;
};
template<>
struct Similarity<measure::euclidean>{
    template<typename T, int64_t dim>
    T operator() (util::span_1d<T,dim> v, util::span_1d<T,dim> q) const{
        using namespace util::math;
        VecLoop_void<T,dim> vecloop_void{};
        T distance{};
        vecloop_void(euclidean_distance_i, distance, v, q);
        return distance;
    }
};

}//namespace similarity
