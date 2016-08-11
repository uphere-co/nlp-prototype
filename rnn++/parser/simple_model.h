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
    Param() {}
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
auto randomParam(Param::value_type scale){
    std::random_device rd;
    //std::mt19937 e{rd()};
    std::mt19937 e{}; //fixed seed for testing.
    std::uniform_real_distribution<Param::value_type>  uniform_dist{-scale, scale};
    auto dim = Param::dim;
    std::vector<Param::value_type> param_raw(dim*(dim*2+2));
    for(auto &x : param_raw)
        x=uniform_dist(e);
    return deserializeParam(param_raw);
}
// namespace compute{
//Cannot do string comparison at compile time.
// constexpr auto activation_factory(const char name[]){
//     if(name=="tanh") return Activation::tanh;
//     else if(name=="sig") return Activation::sig;
// }

using val_type = rnn::type::float_t;
using vec_type = gsl::span<rnn::type::float_t,rnn::config::word_dim>;
using mat_type = gsl::span<rnn::type::float_t,rnn::config::word_dim,rnn::config::word_dim>;


auto weighted_sum=[](int64_t i,
                     mat_type const &w_left,  mat_type const &w_right,
                     vec_type const &bias,
                     vec_type const &word_left, vec_type const &word_right) {
    using util::math::dot;
    return dot(w_left[i], word_left)+dot(w_right[i], word_right) + bias[i];
};
auto activation_fun=[](int64_t i, vec_type const &x) {
    return util::math::Fun<rnn::config::activation>(x[i]);
};
auto activation_dfun=[](int64_t i, vec_type const &x) {
    return util::math::Fun<rnn::config::activation_df>(x[i]);
};

auto update_mesg_common_part=[](int64_t i, vec_type & mesg, vec_type const & weighted_sum) {
    mesg[i]*=activation_dfun(i, weighted_sum);
};
auto update_mesg_finalize=[](int64_t i,int64_t j, vec_type &out,
                   vec_type const &mesg, mat_type const &w)  {
    out[j]+=mesg[i]*w[i][j];
};
auto back_prop_grad_W=[](int64_t i,int64_t j, mat_type &grad, 
                         vec_type const &mesg, vec_type const &weighted_sum)  {
    grad[i][j]+=mesg[i]*weighted_sum[j];
};

auto add_assign_vec=[](int64_t i, vec_type const & out,vec_type const & x) {
    out[i]+=x[i];
};
auto sub_assign_vec=[](int64_t i, vec_type const & out, vec_type const & x) {
    out[i]-=x[i];
};

auto add_assign_mat=[](int64_t i,int64_t j, mat_type const & out,mat_type const & x) {
    out[i][j]+=x[i][j];
};
auto sub_assign_mat=[](int64_t i,int64_t j, mat_type const & out, mat_type const & x) {
    out[i][j]-=x[i][j];
};

auto mul_sum=[](int64_t i,int64_t j, val_type & out, mat_type const &a, mat_type const &b) {
    out+=a[i][j]*b[i][j];
};

template<typename T, int64_t dim_1>
struct VecLoop_void{
    template<typename OP, typename... Args>
    void operator()(OP const&fun, Args&&... args) const {
        for(int64_t i=0; i<dim_1; ++i){
            fun(i, std::forward<Args>(args)...);
        }
    }
};
template<typename T, int64_t dim_1>
struct VecLoop_vec{
    template<typename OP, typename... Args>
    auto operator()(OP const&fun, Args&&... args) const {
        util::math::Vector<T,dim_1> result{};
        for(int64_t i=0; i<dim_1; ++i){
            result.span[i]=fun(i, std::forward<Args>(args)...);
        }
        return result;
    }
};

template<typename T, int64_t dim_1, int64_t dim_2>
struct MatLoop_mat{
    template<typename OP, typename... Args>
    void operator()(OP const&fun, Args&&... args) const {
        util::math::Matrix<T,dim_1,dim_2> result{};
        for(int64_t i=0; i<dim_1; ++i){
            for(int64_t j=0; j<dim_2; ++j){
                result.span[i][j]=fun(i,j, std::forward<Args>(args)...);
            }
        }
    }
};
template<typename T, int64_t dim_1, int64_t dim_2>
struct MatLoop_void{
    template<typename OP, typename... Args>
    void operator()(OP const&fun, Args&&... args) const {
        for(int64_t i=0; i<dim_1; ++i){
            for(int64_t j=0; j<dim_2; ++j){
                fun(i,j, std::forward<Args>(args)...);
            }
        }
    }
};

template<typename T, int64_t M>
gsl::span<T,M>& operator +=(gsl::span<T,M>& out, const gsl::span<T,M>& x){
    auto vecloop_void=VecLoop_void<T,M>{};
    vecloop_void(add_assign_vec, out, x);
    return out;
};

template<typename T, int64_t M>
gsl::span<T,M>& operator -=(gsl::span<T,M>& out, const gsl::span<T,M>& x){
    auto vecloop_void=VecLoop_void<T,M>{};
    vecloop_void(sub_assign_vec, out, x);
    return out;
};


template<typename T, int64_t M, int64_t N>
gsl::span<T,M,N>& operator +=(gsl::span<T,M,N>& out, const gsl::span<T,M,N>& x){
    auto matloop_void=MatLoop_void<T,M,N>{};
    matloop_void(add_assign_mat, out, x);
    return out;
};
template<typename T, int64_t M, int64_t N>
gsl::span<T,M,N>& operator -=(gsl::span<T,M,N>& out, const gsl::span<T,M,N>& x){
    auto matloop_void=MatLoop_void<T,M,N>{};
    matloop_void(sub_assign_mat, out, x);
    return out;
};

Param& operator +=(Param& out, const Param& x){
    out.w_left.span  += x.w_left.span;
    out.w_right.span += x.w_right.span;
    out.bias.span    += x.bias.span;
    out.u_score.span += x.u_score.span;
    return out;
};
Param& operator -=(Param& out, const Param& x){
    out.w_left.span  -= x.w_left.span;
    out.w_right.span -= x.w_right.span;
    out.bias.span    -= x.bias.span;
    out.u_score.span -= x.u_score.span;
    return out;
};
Param operator +(const Param& x, const Param& y){
    Param out{x};
    out += y;
    return out;
};
Param operator -(const Param& x, const Param& y){
    Param out{x};
    out -= y;
    return out;
};

// }//namespace rnn::simple_model::compute
}//namespace rnn::simple_model
}//namespace rnn
