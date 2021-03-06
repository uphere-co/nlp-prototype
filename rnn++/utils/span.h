#pragma once
#include <cstddef>
#include <gsl/gsl.h>

namespace util{

template<typename T> 
using span_dyn = gsl::span<T>;
template<typename T, int64_t M> 
using span_1d = gsl::span<T,M>;
template<typename T, int64_t M, int64_t N>
using span_2d = gsl::span<T,M,N>;
template<typename T, int64_t L, int64_t M, int64_t N>
using span_3d = gsl::span<T,L,M,N>;

//Not works and param.cpp directly uses gsl.h
template<typename... Args>
auto as_span(Args&&... args){
    return gsl::as_span(std::forward<Args>(args)...);
}

template<int64_t T> 
using dim = gsl::dim<T>;

template<std::ptrdiff_t Extent = gsl::dynamic_range>
using cstring_span = gsl::basic_string_span<const char, Extent>;

}//namespace util

