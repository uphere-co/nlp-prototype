#include<vector>
#include<array>
#include <stdexcept>

#include<gsl.h>

namespace util{
namespace math{

template<typename T, std::size_t M>
auto to_span(std::array<T,M> &val){
    return gsl::span<T,M>{val};
}
template<typename T, std::size_t M>
auto to_cspan(std::array<T,M> const &val){
    return gsl::span<const T,M>{val};
}
template<typename T>
auto to_span(std::vector<T> &val){
    return gsl::span<T>{val};
}
template<typename T>
auto to_cspan(std::vector<T> const &val){
    return gsl::span<const T>{val};
}

template<typename T, int64_t M>
struct Vector{
    Vector(gsl::span<const T, M> vals) : dim{M}, _val(M) {
        std::cerr<<M<<" Vector{gsl::span<M>}\n";
        std::copy_n(vals.cbegin(), dim, _val.begin());
    }
    Vector() : _val(M) {}
    const int64_t dim;
    std::vector<T> _val;
    gsl::span<T, M> span=_val;
    gsl::span<const T, M> cspan=_val;
};

template<int64_t M, int64_t N>
struct Matrix{
    Matrix(std::array<const float_t, M*N> vals) : _val(M*N) {
        std::copy_n(vals.begin(), M*N, _val.begin());
    }
    Matrix() : _val(M*N) {}
    std::vector<float_t> _val;
    gsl::span<const float_t, M, N> span=_val;
};

}//namespace util::math
}//namespace util
