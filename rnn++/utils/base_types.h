#pragma once
#include <functional>
#include <vector>
#include <cassert>

#include "utils/algorithm.h"

namespace util{

template<typename T, int64_t DEFAULT=0>
struct IntegerLike{
    using val_t = int64_t;

    IntegerLike() : val{DEFAULT} {}
    IntegerLike(val_t val) : val{val} {}
    static IntegerLike from_unsigned(std::size_t uval){
        return IntegerLike{util::to_signed<val_t>(uval)};
    }
    val_t val;
};

template<typename T, int64_t VAL>
IntegerLike<T,VAL> operator*(IntegerLike<T,VAL> lhs, typename IntegerLike<T,VAL>::val_t rhs) {
    return IntegerLike<T,VAL>{lhs.val*rhs};
}
template<typename T, int64_t VAL>
IntegerLike<T,VAL> operator+(IntegerLike<T,VAL> lhs, typename IntegerLike<T,VAL>::val_t rhs) {
    return IntegerLike<T,VAL>{lhs.val+rhs};
}
template<typename T, int64_t VAL>
IntegerLike<T,VAL> operator-(IntegerLike<T,VAL> lhs, typename IntegerLike<T,VAL>::val_t rhs) {
    return IntegerLike<T,VAL>{lhs.val-rhs};
}
template<typename T, int64_t VAL>
IntegerLike<T,VAL>& operator+=(IntegerLike<T,VAL> &lhs, IntegerLike<T,VAL> rhs) {
    lhs.val+=rhs.val;
    return lhs;
}
template<typename T, int64_t VAL>
IntegerLike<T,VAL>& operator-=(IntegerLike<T,VAL> &lhs, IntegerLike<T,VAL> rhs) {
    lhs.val-=rhs.val;
    return lhs;
}

template<typename T, int64_t VAL>
bool operator<(IntegerLike<T,VAL> const& lhs, IntegerLike<T,VAL> const& rhs) {
    return lhs.val<rhs.val;
}
template<typename T, int64_t VAL>
bool operator>(IntegerLike<T,VAL> const& lhs, IntegerLike<T,VAL> const& rhs) {
    return lhs.val>rhs.val;
}
template<typename T, int64_t VAL>
bool operator==(IntegerLike<T,VAL> const& lhs, IntegerLike<T,VAL> const& rhs) {
    return lhs.val==rhs.val;
}
template<typename T, int64_t VAL>
bool operator!=(IntegerLike<T,VAL> const& lhs, IntegerLike<T,VAL> const& rhs) {
    return lhs.val!=rhs.val;
}
template<typename T, int64_t VAL>
IntegerLike<T,VAL>& operator++(IntegerLike<T,VAL> &x) {// pre increment
    ++x.val;
    return x;
}
template<typename T, int64_t VAL>
IntegerLike<T,VAL> operator++(IntegerLike<T,VAL> &x, int ){ // post increment
    IntegerLike<T,VAL> t{x.val};
    ++x.val;
    return t;
}
template<typename T, int64_t VAL>
IntegerLike<T,VAL>& operator--(IntegerLike<T,VAL> &x) {// pre increment
    --x.val;
    return x;
}
template<typename T, int64_t VAL>
IntegerLike<T,VAL> operator--(IntegerLike<T,VAL> &x, int ){ // post increment
    IntegerLike<T,VAL> t{x.val};
    --x.val;
    return t;
}

template<typename T, int64_t VAL>
int64_t diff(IntegerLike<T,VAL> lhs, IntegerLike<T,VAL> rhs) {
    return lhs.val-rhs.val;
}

template<typename T, typename TRAW>
auto deserialize(std::vector<TRAW> const& raw){
    std::vector<T> vals;
    for(auto val : raw) vals.push_back(T{val});
    return vals;
}

template<typename T, int64_t VAL>
auto serialize(std::vector<IntegerLike<T,VAL>> const& vals){
    std::vector<typename IntegerLike<T,VAL>::val_t> raw;
    for(auto val : vals) raw.push_back(val.val);
    return raw;
}
template<typename T>
auto serialize(std::vector<T> const& vals){
    std::vector<T> raw{vals};
    return raw;
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
