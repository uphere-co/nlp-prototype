#include<vector>
#include<array>
#include <stdexcept>

#include<gsl.h>

#include"utils/math.h"

namespace util{
namespace math{

//TODO: should support move from existing container?
template<typename T, int64_t M>
struct Vector{
    Vector(gsl::span<T, M> const vals) : _val(M) {
        std::cerr<<M<<" Vector{gsl::span<M>}\n";
        std::copy_n(vals.cbegin(), dim, _val.begin());
    }
    const int64_t dim=M;
    std::vector<T> _val;
    gsl::span<T, M> span=_val;
};
template<typename T, int64_t M, int64_t N>
struct Matrix{
    Matrix(gsl::span<T, M,N> const vals) : _val(M*N) {
        std::cerr<<M<<" "<<N<<" Matrix{gsl::span<M,N>}\n";
        std::copy_n(vals.begin(), M*N, _val.begin());
    }
    std::vector<T> _val;
    gsl::span<T, M, N> span=_val;
};

namespace factory{

//TODO: val can be const &
template<typename T, std::size_t M>
auto Vector(std::array<T,M> &val){
    const auto span = gsl::as_span(val);
    return util::math::Vector<T,M>{span};
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

}//namespace util::math
}//namespace util
