#pragma once
#include <vector>
#include <algorithm>
#include <cmath>

#include "utils/span.h"
#include "utils/math_funname.h"

namespace util{
namespace math{

template<typename T, std::size_t M>
auto to_span(std::array<T,M> const &val){
    return span_1d<T,M>{val};
}
template<typename T>
auto to_span(std::vector<T> const &val){
    return span_dyn<T>{val};
}

template<typename T>
auto sum(std::vector<T> vec){
    return std::accumulate(vec.cbegin(), vec.cend(), T{});
}
template<typename T>
auto sum(span_dyn<T> vec){
    return std::accumulate(vec.cbegin(), vec.cend(), T{});
}
template<typename T, int64_t M>
auto sum(span_1d<T,M> const vec){
    //std::cerr << "sum<T,M> "<< M << std::endl;
    return std::accumulate(vec.cbegin(), vec.cend(), T{});
}
template<typename T, int64_t M,int64_t N>
auto sum(span_2d<T,M,N> const mat){
    //std::cerr << "sum<T,M,N> "<< M << " " << N << std::endl;
    auto flat = mat.subspan(0,M*N);
    return sum(flat);
}

template<typename T, int64_t M>
auto norm_L1(span_1d<T,M> const vec){
    T factor{};
    for (auto &x : vec) factor += std::abs(x);
    return factor;
};
template<typename T, int64_t M, int64_t N>
auto norm_L1(span_2d<T,M,N> const mat){
    T factor{};
    for (auto &x : mat) factor += std::abs(x);
    return factor;
};

template<typename T, int64_t M>
auto norm_L2(span_1d<T,M> const vec){
    T factor{};
    for (auto &x : vec) factor += x*x;
    return std::sqrt(factor);
};
template<typename T, int64_t M, int64_t N>
auto norm_L2(span_2d<T,M,N> const mat){
    T factor{};
    for (auto &x : mat) factor += x*x;
    return std::sqrt(factor);
};


template<FunName>
float_t Fun(float_t x);

}//namespace util::math
}//namespace util
