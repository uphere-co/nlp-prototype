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
struct ActivationFun{
private:
    using vec_type = gsl::span<T,dim>;
public:
    auto operator()(int64_t i, vec_type const &x) const {
        return util::math::Fun<rnn::config::activation>(x[i]);
    }
};
template<typename T,int64_t dim>
struct ActivationDFun{
private:
    using vec_type = gsl::span<T,dim>;
public:
    auto operator()(int64_t i, vec_type const &x) const {
        return util::math::Fun<rnn::config::activation_df>(x[i]);
    }
};

template<template<typename,int64_t> class OP, typename T, int64_t dim, typename... Args>
auto vectorize(OP<T,dim> const &fun, Args&&... args)
{
    util::math::Vector<T,dim> result{};
    for(int64_t i=0; i<dim; ++i){
        result.span[i] =fun(i, std::forward<Args>(args)...);
    }
    return std::move(result);
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
