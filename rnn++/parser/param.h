#pragma once
#include <vector>
#include <cmath>
#include <utility>
#include <random>

#include <gsl.h>

#include "parser/basic_type.h"
#include "parser/config.h"

#include "utils/linear_algebra.h"

namespace rnn{
namespace simple_model{
struct Param{
    static constexpr auto dim = rnn::config::word_dim;
    using value_type = rnn::type::float_t;
    using mat_type   = util::math::Matrix<value_type, dim, dim>;
    using vec_type   = util::math::Vector<value_type, dim>;
    Param(gsl::span<value_type, dim, dim> w_left_span,
          gsl::span<value_type, dim, dim> w_right_span,
          gsl::span<value_type, dim> bias_span,
          gsl::span<value_type, dim> u_score_span)
          : w_left{w_left_span}, w_right{w_right_span},
            bias{bias_span}, u_score{u_score_span} {}
    Param(mat_type &&w_left, mat_type &&w_right,
          vec_type &&bias, vec_type &&u_score)
          : w_left{std::move(w_left)}, w_right{std::move(w_right)},
            bias{std::move(bias)}, u_score{std::move(u_score)} {}
    Param() {}
    mat_type w_left;
    mat_type w_right;
    vec_type bias;
    vec_type u_score;
};
Param& operator +=(Param& out, const Param& x);
Param& operator -=(Param& out, const Param& x);
Param operator +(const Param& x, const Param& y);
Param operator -(const Param& x, const Param& y);
// Param operator *(Param::value_type x, const Param& y);

Param deserializeParam(std::vector<rnn::type::float_t> &param_raw);
Param randomParam(Param::value_type scale);
Param load_param();

}//namespace rnn::simple_model
}//namespace rnn


