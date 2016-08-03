#pragma once
#include <vector>
#include<algorithm>

#include<gsl.h>

namespace util{
namespace math{
auto sum(gsl::span<const float> vec){
    return std::accumulate(vec.cbegin(), vec.cend(), decltype(vec)::value_type{0});
}
auto sum(gsl::span<const double> vec){
    return std::accumulate(vec.cbegin(), vec.cend(), decltype(vec)::value_type{0});
}
}//namespace util::math
}//namespace util
