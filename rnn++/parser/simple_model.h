#pragma once
#include<vector>
#include<cmath>
#include<utility>

#include<gsl.h>

#include"parser/basic_type.h"
#include"parser/config.h"
#include"utils/string.h"
#include"utils/math.h"
#include"utils/linear_algebra.h"

#include "utils/print.h"
using util::print;

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



//Cannot do string comparison at compile time.
// constexpr auto activation_factory(const char name[]){
//     if(name=="tanh") return Activation::tanh;
//     else if(name=="sig") return Activation::sig;
// }

auto activation_f = util::math::Fun<rnn::config::activation>;//[](auto x){return tanh(x);};
auto activation_df = [](auto x){
    auto fx = cosh(x);
    return decltype(x){1}/(fx*fx);
};

template<typename T,int64_t dim>
struct WeightedSum{
private:
    using vec_type = gsl::span<T,dim>;
    using mat_type = gsl::span<T,dim,dim>;
public:
    auto operator()(int64_t i,
                    mat_type const &w_left,  mat_type const &w_right,
                    vec_type const &bias,
                    vec_type const &word_left, vec_type const &word_right) const {
        using util::math::dot;
        return dot(w_left[i], word_left)+dot(w_right[i], word_right) + bias[i];
    }
};

template<typename T,int64_t dim>
struct Tanh{
private:
    using vec_type = gsl::span<T,dim>;
public:
    // template<typename VEC>
    // auto operator()(int64_t i, VEC const &x) const {
    //     return tanh(x[i]);
    // }
    auto operator()(int64_t i, vec_type const &x) const {
        return tanh(x[i]);
    }
};
// auto Tanh=[](auto i, auto const &x) {  return tanh(x[i]);

template<template<typename ,int64_t > class OP, typename T, int64_t dim, typename... Args>
auto vectorize(OP<T,dim> const &fun, Args&&... args)
{
    util::math::Vector<T,dim> result{};
    for(int64_t i=0; i<dim; ++i){
        result.span[i] =fun(i, std::forward<Args>(args)...);
    }
    return std::move(result);
}

//TODO:change name to more approciate one and move to util::math.
template<typename T, int64_t M>
auto apply_activation(gsl::span<T,M> const &wsum){
    util::math::Vector<T,M> phrase{};
    for(decltype(M) i=0; i<M; ++i){
        phrase.span[i] = activation_f(wsum[i]);
    }
    return std::move(phrase);
}

auto weighted_sum_i=[](auto const &w_left_i, auto const &w_right_i, auto const &b_i,
                          auto const &word_left, auto const &word_right){
    using util::math::dot;
    return dot(w_left_i, word_left)+dot(w_right_i, word_right) + b_i;
};
template<typename T, int64_t M>
auto weighted_sum(util::math::Matrix<T,M,M> const &w_left,
                     util::math::Matrix<T,M,M> const &w_right,
                     util::math::Vector<T,M> const &bias,
                     gsl::span<T,Param::dim> const &word_left,
                     gsl::span<T,Param::dim> const &word_right){
    util::math::Vector<T,M> wsum{};
    for(decltype(M) i=0; i<M; ++i){
        // auto weighted_sum_i = WeightedSum<Param::value_type,Param::dim>{};
        auto weighted_sum_i = WeightedSum<float,Param::dim>{};
        wsum.span[i] = weighted_sum_i(i, w_left.span, w_right.span, bias.span,
                                      word_left, word_right);
    }
    return std::move(wsum);
}


auto merge_to_phrase_i=[](auto const &w_left_i, auto const &w_right_i, auto const &b_i,
                          auto const &word_left, auto const &word_right){
    using util::math::dot;
    return activation_f(dot(w_left_i, word_left)+dot(w_right_i, word_right) + b_i);
};
template<typename T, int64_t M>
auto merge_to_phrase(util::math::Matrix<T,M,M> const &w_left,
                     util::math::Matrix<T,M,M> const &w_right,
                     util::math::Vector<T,M> const &bias,
                     gsl::span<T,Param::dim> const &word_left,
                     gsl::span<T,Param::dim> const &word_right){
    util::math::Vector<T,M> phrase{};
    for(decltype(M) i=0; i<M; ++i){
        auto x_i = merge_to_phrase_i(w_left.span[i], w_right.span[i], bias.span[i],
                                     word_left, word_right);
        phrase.span[i] = activation_f(x_i);
    }
    return std::move(phrase);
}


// auto backward_mesg_w(grad, node, mesg, x, word){
//     mesg *= activation_df(x);
//     grad+=outer(mesg, word);
//     backward_mesg_w(grad, *node.left,  dot(mesg, w_left) , x, word_left);
//     backward_mesg_w(grad, *node.right, dot(mesg, w_right), x, word_right);
//     return;
// }
}//namespace rnn::simple_model::compute
}//namespace rnn::simple_model
}//namespace rnn
