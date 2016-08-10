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
    alog,
    d_alog,
    tanh,
    sig,
    d_tanh,
    d_sig,
    ax,
    d_ax,
    test,
    d_test,
};

template<FunName>
float_t Fun(float_t x);
template<>
float_t Fun<FunName::alog>(float_t x){auto one=decltype(x){1}; return std::log(std::abs(x)+one);}
template<>
float_t Fun<FunName::d_alog>(float_t x){auto one=decltype(x){1}; return one/(std::abs(x)+one);}
template<>
float_t Fun<FunName::tanh>(float_t x){return std::tanh(x);}
template<>
float_t Fun<FunName::d_tanh>(float_t x){auto fx=std::cosh(x);return decltype(x){1}/(fx*fx);}
template<>
float_t Fun<FunName::sig>(float_t x){return float_t{1}/(float_t{1}+std::exp(-x));}

template<>
float_t Fun<FunName::ax>(float_t x){return float_t{0.5}*x;}
template<>
float_t Fun<FunName::d_ax>(float_t ){return float_t{0.5};}
template<>
float_t Fun<FunName::test>(float_t x){return x>0?std::sqrt(x):-std::sqrt(-x);}
template<>
float_t Fun<FunName::d_test>(float_t x){return x>0?float_t{0.5}/std::sqrt(x):float_t{0.5}/std::sqrt(-x);}

}//namespace util::math
}//namespace util
