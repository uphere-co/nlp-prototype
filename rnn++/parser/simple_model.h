#pragma once
#include<vector>
#include<cmath>

#include<gsl.h>

#include"parser/basic_type.h"
#include"utils/string.h"
#include"utils/math.h"

namespace rnn{

namespace simple_model{
template<int dim>
struct Param{
    using value_type = rnn::type::float_t;
    Param(gsl::span<value_type, dim*2, dim> w_span,
          gsl::span<value_type, dim> bias_span,
          gsl::span<value_type, dim> u_score_span)
          : w{w_span}, bias{bias_span}, u_score{u_score_span} {}
    util::math::Matrix<value_type, 2*dim, dim> w;
    util::math::Vector<value_type, dim> bias;
    util::math::Vector<value_type, dim> u_score;
};

template<int dim>
auto deserializeParam(std::vector<rnn::type::float_t> &param_raw){
    auto span2d = gsl::as_span(param_raw.data(), gsl::dim<dim*2+2>(),gsl::dim<dim>());
    auto w_span = gsl::as_span(span2d.data(), gsl::dim<dim*2>(),gsl::dim<dim>());
    auto bias_span = span2d[2*dim];
    auto u_score_span = span2d[2*dim+1];
    return Param<dim>{w_span, bias_span, u_score_span};
}
}//namespace rnn::simple_model
namespace compute{
using char_t = rnn::type::char_t;
using float_t = rnn::type::float_t;

// struct Tanh{
//     float_t operator()(float_t x) const {
//         return std::tanh(x);
//     }
//     float_t inverse(float_t x) const {
//         return std::pow(std::cosh(x), float_t{-2.0});
//     }
// };
// struct Add{
//     float_t& operator()(float_t x, float_t y, float_t &z=float_t{0}) const {
//         z=x+y;
//         return z;
//     }
// };
// struct Dot{
//     float_t& operator()(gsl::span<const float_t> x, gsl::span<const float_t> y,
//                         float_t &z=float_t{0}) const {
//         std::inner_product(x.cbegin(), x.cend(), y.cbegin(), z);
//         return z;
//     }
// };
//
// };


}//namespace rnn::compute
}//namespace rnn
