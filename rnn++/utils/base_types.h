#pragma once
#include <functional>
#include <limits>
#include <cassert>

namespace util{

template<typename T, int64_t DEFAULT=0>
struct IntegerLike{
    using val_t = int64_t;

    IntegerLike() : val{DEFAULT} {}
    IntegerLike(val_t val) : val{val} {}
    IntegerLike(uint64_t uval) : val{static_cast<val_t>(uval)}{
        assert(uval<std::numeric_limits<val_t>::max());
    }
    val_t val;
};

template<typename T, int64_t VAL>
IntegerLike<T> operator*(IntegerLike<T,VAL> lhs, const IntegerLike<T,VAL>& rhs) {
    return IntegerLike<T>{lhs.val*rhs.val};
}

template<typename T, int64_t VAL>
bool operator==(IntegerLike<T,VAL> lhs, const IntegerLike<T,VAL>& rhs) {
    return lhs.val==rhs.val;
}
template<typename T, int64_t VAL>
bool operator!=(IntegerLike<T,VAL> lhs, const IntegerLike<T,VAL>& rhs) {
    return lhs.val!=rhs.val;
}

}//namespace util

namespace std {
template<typename T, int64_t VAL>
struct hash<util::IntegerLike<T,VAL>> {
    size_t operator()(util::IntegerLike<T,VAL> const& uid) const {
        auto val = uid.val;
        return hash<decltype(val)>{}(val);
    }
};
template<typename T, int64_t VAL>
struct equal_to<util::IntegerLike<T,VAL>> {
    constexpr bool operator()(util::IntegerLike<T,VAL> const& lhs, util::IntegerLike<T,VAL> const & rhs) const{
        return lhs.val == rhs.val;
    }
};
}//namespace std

