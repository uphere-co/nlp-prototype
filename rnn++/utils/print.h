#pragma once

#include <iostream>

#include <gsl.h>

namespace util{
template<typename T>
void print(T const&x){
    std::cerr <<x << " ";
}

template<typename T, int64_t M>
void print(gsl::span<T,M> const span){
    for(auto x : span) print(x);
    std::cerr << " :1D"<<std::endl;
}

template<typename T, int64_t M,int64_t N>
void print(gsl::span<T,M,N> const span){
    ////ranged for iterates flattened span.
    //for(auto x : span) print(x);
    for(int64_t i=0; i<M; ++i) print(span[i]);
    std::cerr << " :2D static"<<std::endl;
}

template<typename T, int64_t M,int64_t N>
void print(gsl::span<const T,gsl::dynamic_range,M,N> span){
    for(int64_t i=0; i<M; ++i) print(span[i]);
    //for(auto it=span.cbegin(); it<span.cend(); ++it) print_span(*it);
    std::cerr << " :2D dynamic_range"<<std::endl;
}
// template<typename T>
// void print(std::vector<T> const & vec){
//     print(gsl::as_span(vec));
// }
// template<typename T, size_t M>
// void print(std::array<T,M> arr){
//     print(gsl::as_span(arr));
// }

}//namespcae util
