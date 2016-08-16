#pragma once
#include <vector>
#include <algorithm>
#include <cmath>

#include <gsl.h>

#include "utils/math_funname.h"

namespace util{
namespace math{

template<typename T, std::size_t M>
auto to_span(std::array<T,M> &val){
    return gsl::span<T,M>{val};
}
template<typename T>
auto to_span(std::vector<T> &val){
    return gsl::span<T>{val};
}

template<typename T>
auto sum(gsl::span<T> const vec){
    return std::accumulate(vec.cbegin(), vec.cend(), T{});
}
template<typename T, int64_t M>
auto sum(gsl::span<T,M> const vec){
    //std::cerr << "sum<T,M> "<< M << std::endl;
    return std::accumulate(vec.cbegin(), vec.cend(), T{});
}
template<typename T, int64_t M,int64_t N>
auto sum(gsl::span<T,M,N> const mat){
    //std::cerr << "sum<T,M,N> "<< M << " " << N << std::endl;
    auto flat = mat.subspan(0,M*N);
    return sum(flat);
}

template<typename T, int64_t M>
auto norm_L1(gsl::span<T,M> const vec){
    T factor{};
    for (auto &x : vec) factor += std::abs(x);
    return factor;
};
template<typename T, int64_t M, int64_t N>
auto norm_L1(gsl::span<T,M,N> const mat){
    T factor{};
    for (auto &x : mat) factor += std::abs(x);
    return factor;
};

template<typename T, int64_t M>
auto norm_L2(gsl::span<T,M> const vec){
    T factor{};
    for (auto &x : vec) factor += x*x;
    return std::sqrt(factor);
};
template<typename T, int64_t M, int64_t N>
auto norm_L2(gsl::span<T,M,N> const mat){
    T factor{};
    for (auto &x : mat) factor += x*x;
    return std::sqrt(factor);
};


template<FunName>
float_t Fun(float_t x);

}//namespace util::math
}//namespace util
