#pragma once
#include <vector>
#include<algorithm>

#include<gsl.h>

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

enum class FunName {
    tanh,
    sig,
};

template<FunName>
float_t Fun(float_t x);
template<>
float_t Fun<FunName::tanh>(float_t x){return tanh(x);}
template<>
float_t Fun<FunName::sig>(float_t x){return float_t{1}/(float_t{1}+exp(-x));}

}//namespace util::math
}//namespace util
