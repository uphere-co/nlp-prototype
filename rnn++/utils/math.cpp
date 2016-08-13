#include "utils/math.h"

namespace util{
namespace math{

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
