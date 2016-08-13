#pragma once
#include <vector>
#include <array>
#include <iostream>
#include <stdexcept>
#include <cassert>

#include<gsl.h>

#include"utils/math.h"

namespace util{
namespace math{

template<typename T, int64_t M>
struct Vector{
    Vector(gsl::span<T, M> const vals) : _val(M, T{}) {
        // std::cerr<<M<<" Vector{gsl::span<M>}\n";
        std::copy_n(vals.cbegin(), M, _val.begin());
        assert(span[0]==_val[0]);
    }
    Vector() : _val(M, T{}) {
        // std::cerr<<"Default construct Vector{gsl::span<M>}\n";
        assert(span[0]==_val[0]);
    }
    //CAUTION: careful to correctly implement non-default constructor if necessary.
    Vector(Vector const & vec) : _val{vec._val} {
        assert(span[0]==_val[0]);
        // std::cerr<<_val.size()<<" Copy construct Vector{gsl::span<M>}\n";
    };
    Vector& operator=(const Vector& vec) {
        // std::cerr<<"Copy assignment Vector{gsl::span<M>}\n";
        //  std::cerr<<"size: " << vec._val.size() << " vs " <<_val.size() <<std::endl;
        // assert(_val.size()==vec._val.size());
        // assert(span[0]==_val[0]);
        std::copy_n(vec.span.cbegin(), M, _val.begin());
        // span=gsl::span<T, M>{_val};
        assert(span[0]==_val[0]);
        // std::cerr<<"size: " << _val.size() <<std::endl;
        return *this;
    };
    Vector(Vector&& vec) : _val{std::move(vec._val)} {
        span=gsl::span<T, M>{_val};
        //Fail if nan
        assert(span[0]==_val[0]);
        // std::cerr<<"Move Vector{gsl::span<M>}\n";
    };
    std::vector<T> _val;
    gsl::span<T, M> span=_val; //this default is critical!
};
template<typename T, int64_t M>
struct VectorView{
    VectorView(gsl::span<T, M> const vals) :span{vals} {}
    gsl::span<T, M> span;
};

template<typename T, int64_t M, int64_t N>
struct Matrix{
    Matrix(gsl::span<T, M,N> const vals) : _val(M*N, T{}) {
        std::cerr<<M<<" "<<N<<" Matrix{gsl::span<M,N>}\n";
        std::copy_n(vals.begin(), M*N, _val.begin());
    }
    Matrix() :_val(M*N, T{}) {}
    Matrix(Matrix const & mat) : _val{mat._val}{};
    Matrix& operator=(const Matrix& mat) {
        std::copy_n(mat.span.cbegin(), M*N, _val.begin());
        return *this;
    }
    Matrix(Matrix&& vec) : _val{std::move(vec._val)} {
        span=gsl::span<T, M,N>{_val};
        //Fail if nan
        assert(span[0][0]==_val[0]);
    };
    std::vector<T> _val;
    gsl::span<T, M, N> span=_val; //this default is critical!
};
template<typename T, int64_t M, int64_t N>
struct MatrixView{
    MatrixView(gsl::span<T, M,N> const vals) : span{vals} {}
    gsl::span<T, M, N> span;
};

namespace factory{

//TODO: val can be const &
template<typename T, std::size_t M>
auto Vector(std::array<T,M> &val){
    const auto span = gsl::as_span(val);
    return util::math::Vector<T,int64_t{M}>{span};
}

// template<typename T>
// auto Vector(std::vector<T> const &val){
//     constexpr auto span = gsl::span<const T>{val};
//     return util::math::Vector<T,span.size()>{span};
// }

// template<typename T>
// auto Vector(std::vector<T> const &val){
//     return gsl::span<const T>{val};
// }
//
// template<typename T>
// auto Vector(T vals){
//     auto a = util::math::Vector{to_cspan(vals)};
//     return a;
// }
//

}//namespace util::math::factory


//TODO: use bi-dimensional index, gsl::index<2>??
template<typename T, int64_t M, int64_t N>
auto transpose(gsl::span<T,M,N> mat){
    std::vector<T> tr_mat(M*N);
    auto tr = gsl::span<T,N,M>{tr_mat};
    for (int64_t i=0; i<mat.extent(0); ++i) {
       for (int64_t j=0; j<mat.extent(1); ++j){
          tr[j][i] = mat[i][j];
      }
    }
    return Matrix<T,N,M>{tr};
}

template<typename T, int64_t M>
T dot(gsl::span<T,M> const x, gsl::span<T,M> const y){
    return std::inner_product(x.cbegin(), x.cend(), y.cbegin(), T{});
}
//Following also works, without template, but less safe:
// auto dot = [](auto const &x, auto const &y){
//     typedef typename std::remove_reference<decltype(x[0])>::type A;
//     return std::inner_product(x.cbegin(), x.cend(), y.cbegin(), A{0});
// };

template<typename T, int64_t M, int64_t N>
auto dotdot(Vector<T,M> &x, Matrix<T,M,N> &m, Vector<T,N> &y){
    T sum{};
    for(decltype(M) i=0; i!=M; ++i){
        for(decltype(N) j=0; j!=N; ++j){
            sum += x.span[i]*m.span[i][j]*y.span[j];
        }
    }
    return sum;
}

}//namespace util::math
}//namespace util
