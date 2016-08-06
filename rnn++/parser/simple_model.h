#pragma once
#include<vector>
#include<cmath>
#include<utility>

#include<gsl.h>

#include"parser/basic_type.h"
#include"utils/string.h"
#include"utils/math.h"
#include"utils/linear_algebra.h"

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
    mat_type w_left;
    mat_type w_right;
    vec_type bias;
    vec_type u_score;
};

auto deserializeParam(std::vector<rnn::type::float_t> &param_raw){
    constexpr auto dim = rnn::config::word_dim;
    auto span2d   = gsl::as_span(param_raw.data(), gsl::dim<dim*2+2>(),gsl::dim<dim>());
    // auto w_span = gsl::as_span(span2d.data(), gsl::dim<dim*2>(),gsl::dim<dim>());
    auto wLT_span = gsl::as_span(span2d.data(),      gsl::dim<dim>(),gsl::dim<dim>());
    auto wRT_span = gsl::as_span(span2d[dim].data(), gsl::dim<dim>(),gsl::dim<dim>());
    auto wL       = util::math::transpose(wLT_span);
    auto wR       = util::math::transpose(wRT_span);
    auto bias     = util::math::Vector<rnn::type::float_t,dim>{span2d[2*dim]};
    auto u_score  = util::math::Vector<rnn::type::float_t,dim>{span2d[2*dim+1]};
    return Param{std::move(wL), std::move(wR), std::move(bias), std::move(u_score)};
}

namespace compute{
auto activation_f = [](auto x){return tanh(x);};
auto activation_df = [](auto x){
    auto fx = cosh(x);
    return decltype(x){1}/(fx*fx);
};
auto merge_to_phrase_i=[](auto const &w_left_i, auto const &w_right_i, auto const &b_i,
                          auto const &word_left, auto const &word_right){
    return activation_f(util::math::dot(w_left_i, word_left)+util::math::dot(w_right_i, word_right) + b_i);
};

template<typename T, int64_t M>
auto merge_to_phrase(util::math::Matrix<T,M,M> w_left,
                     util::math::Matrix<T,M,M> w_right,
                     util::math::Vector<T,M> bias,
                     util::math::VectorView<T,M> word_left,
                     util::math::VectorView<T,M> word_right){
    util::math::Vector<T,M> phrase{};
    for(int64_t i=0; i<M; ++i){
        phrase.span[i] = merge_to_phrase_i(w_left.span[i], w_right.span[i], bias.span[i],
                                           word_left.span, word_right.span);
    }
    return std::move(phrase);
}

}//namespace rnn::simple_model::compute
}//namespace rnn::simple_model
}//namespace rnn
