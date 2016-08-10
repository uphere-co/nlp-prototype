#pragma once
#include<vector>
#include<cmath>
#include<utility>
#include<random>

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
struct MulAssign{
private:
    using vec_type = gsl::span<T,dim>;
public:
    void operator()(int64_t i, vec_type const & out, vec_type const & factor) const {
        out[i]*=factor[i];
    }
};
template<typename T,int64_t dim>
struct AccumMesg{
private:
    using vec_type = gsl::span<T,dim>;
    using mat_type = gsl::span<T,dim,dim>;
public:
    auto operator()(int64_t i,int64_t j,
                    vec_type &out,
                    vec_type const &mesg,
                    mat_type const &w) const {
        out[j]+=mesg[i]*w[i][j];
    }
};
template<typename T,int64_t dim1,int64_t dim2>
struct BackPropGrad{
private:
    using vec_type = gsl::span<T,dim1>;
    using mat_type = gsl::span<T,dim1,dim2>;
public:
    auto operator()(int64_t i,int64_t j,
                    mat_type &grad,
                    vec_type const &mesg,
                    vec_type const &weighted_sum) const {
        grad[i][j]+=mesg[i]*weighted_sum[j];
    }
};
template<typename T,int64_t dim1,int64_t dim2>
struct MulSum{
private:
    using mat_type = gsl::span<T,dim1,dim2>;
public:
    auto operator()(int64_t i,int64_t j,
                    T & out,
                    mat_type const &a,
                    mat_type const &b) const {
        out+=a[i][j]*b[i][j];
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
auto vecloop_vec(OP<T,dim> const &fun, Args&&... args)
{
    util::math::Vector<T,dim> result{};
    for(int64_t i=0; i<dim; ++i){
        result.span[i] +=fun(i, std::forward<Args>(args)...);
    }
    return std::move(result);
}
template<template<typename,int64_t> class OP, typename T, int64_t dim, typename... Args>
void vecloop_void(OP<T,dim> const &fun, Args&&... args)
{
    for(int64_t i=0; i<dim; ++i){
        fun(i, std::forward<Args>(args)...);
    }
}

template<template<typename,int64_t,int64_t> class OP,
         typename T,int64_t dim_1,int64_t dim_2, typename... Args>
auto matloop__mat(OP<T,dim_1,dim_2> const &fun, Args&&... args)
{
    util::math::Matrix<T,dim_1,dim_2> result{};
    for(int64_t i=0; i<dim_1; ++i){
        for(int64_t j=0; j<dim_2; ++j){
            result.span[i][j] =fun(i,j, std::forward<Args>(args)...);
        }
    }
    return std::move(result);
}
template<template<typename,int64_t,int64_t> class OP,
         typename T,int64_t dim_1,int64_t dim_2, typename... Args>
auto matloop_vec1(OP<T,dim_1,dim_2> const &fun, Args&&... args)
{
    util::math::Vector<T,dim_1> result{};
    for(int64_t i=0; i<dim_1; ++i){
        for(int64_t j=0; j<dim_2; ++j){
            result.span[i]+=fun(i,j, std::forward<Args>(args)...);
        }
    }
    return std::move(result);
}
template<template<typename,int64_t,int64_t> class OP,
         typename T,int64_t dim_1,int64_t dim_2, typename... Args>
auto matloop_vec2(OP<T,dim_1,dim_2> const &fun, Args&&... args)
{
    util::math::Vector<T,dim_2> result{};
    for(int64_t i=0; i<dim_1; ++i){
        for(int64_t j=0; j<dim_2; ++j){
            result.span[j]+=fun(i,j, std::forward<Args>(args)...);
        }
    }
    return std::move(result);
}
template<template<typename,int64_t,int64_t> class OP,
         typename T,int64_t dim_1,int64_t dim_2, typename... Args>
void matloop_void(OP<T,dim_1,dim_2> const &fun, Args&&... args)
{
    for(int64_t i=0; i<dim_1; ++i){
        for(int64_t j=0; j<dim_2; ++j){
            fun(i,j, std::forward<Args>(args)...);
        }
    }
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
