#pragma once
#include<vector>
#include<cmath>

#include<gsl.h>

#include"parser/basic_type.h"
#include"utils/string.h"
#include"utils/math.h"

namespace rnn{
namespace compute{
using char_t = rnn::type::char_t;
using float_t = rnn::type::float_t;

struct Tanh{
    float_t operator()(float_t x) const {
        return std::tanh(x);
    }
    float_t inverse(float_t x) const {
        return std::pow(std::cosh(x), float_t{-2.0});
    }
};
struct Add{
    float_t& operator()(float_t x, float_t y, float_t &z=float_t{0}) const {
        z=x+y;
        return z;
    }
};
struct Dot{
    float_t& operator()(gsl::span<const float_t> x, gsl::span<const float_t> y,
                        float_t &z=float_t{0}) const {
        std::inner_product(x.cbegin(), x.cend(), y.cbegin(), z);
        return z;
    }
};

};


}//namespace rnn::compute
}//namespace rnn
