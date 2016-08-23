#pragma once
#include <vector>
#include <algorithm>

#include <gsl.h>

#include "utils/linear_algebra.h"

namespace util{
namespace math{

template<typename T, int64_t dim_1>
struct VecLoop_void{
    template<typename OP, typename... Args>
    void operator()(OP const&fun, Args&&... args) const {
        #pragma clang loop vectorize(enable)
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
        #pragma clang loop vectorize(enable)
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

}//namespace util::math
}//namespace util

namespace gsl{

auto add_assign_vec=[](int64_t i, auto const & out, auto const & x) {
    out[i]+=x[i];
};
auto sub_assign_vec=[](int64_t i, auto const & out, auto const & x) {
    out[i]-=x[i];
};
auto mul_assign_vec=[](int64_t i, auto const & out, auto x) {
    out[i]*=x;
};

auto add_assign_mat=[](int64_t i,int64_t j, auto const & out, auto const & x) {
    out[i][j]+=x[i][j];
};
auto sub_assign_mat=[](int64_t i,int64_t j, auto const & out, auto const & x) {
    out[i][j]-=x[i][j];
};
auto mul_assign_mat=[](int64_t i,int64_t j, auto const & out, auto x) {
    out[i][j]*=x;
};

template<typename T, int64_t M>
span<T,M>& operator +=(span<T,M>& out, const span<T,M>& x){
    auto vecloop_void=util::math::VecLoop_void<T,M>{};
    vecloop_void(add_assign_vec, out, x);
    return out;
};
template<typename T, int64_t M>
span<T,M>& operator -=(span<T,M>& out, const span<T,M>& x){
    auto vecloop_void=util::math::VecLoop_void<T,M>{};
    vecloop_void(sub_assign_vec, out, x);
    return out;
};
template<typename T, int64_t M>
span<T,M>& operator *=(span<T,M>& out, T x){
    auto vecloop_void=util::math::VecLoop_void<T,M>{};
    vecloop_void(mul_assign_vec, out, x);
    return out;
};


template<typename T, int64_t M, int64_t N>
span<T,M,N>& operator +=(span<T,M,N>& out, const span<T,M,N>& x){
    auto matloop_void=util::math::MatLoop_void<T,M,N>{};
    matloop_void(add_assign_mat, out, x);
    return out;
};
template<typename T, int64_t M, int64_t N>
span<T,M,N>& operator -=(span<T,M,N>& out, const span<T,M,N>& x){
    auto matloop_void=util::math::MatLoop_void<T,M,N>{};
    matloop_void(sub_assign_mat, out, x);
    return out;
};
template<typename T, int64_t M, int64_t N>
span<T,M,N>& operator *=(span<T,M,N>& out, T x){
    auto matloop_void=util::math::MatLoop_void<T,M,N>{};
    matloop_void(mul_assign_mat, out, x);
    return out;
};



auto grad_update_mat=[](int64_t i,int64_t j,
                        auto &param, auto const &grad, auto scale) {
    param[i][j] += scale*grad[i][j];
};
auto grad_update_vec=[](int64_t i,
                        auto &param, auto const &grad, auto scale) {
    param[i] += scale*grad[i];
};
template<typename T, int64_t M, int64_t N>
void grad_update(span<T,M,N>& param, span<T,M,N> const &grad, T scale){
    auto matloop_void=util::math::MatLoop_void<T,M,N>{};
    matloop_void(grad_update_mat, param, grad, scale);
};
template<typename T, int64_t M>
void grad_update(span<T,M>& param, span<T,M> const &grad, T scale){
    auto vecloop_void=util::math::VecLoop_void<T,M>{};
    vecloop_void(grad_update_vec, param, grad, scale);
};
}//namespace gsl
