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

template<typename T>
std::vector<T> deserialize(std::vector<typename T::val_t> const& raw){
    std::vector<T> uids;
    for(auto uid : raw) uids.push_back(T{uid});
    return uids;
}
template<typename T>
std::vector<typename T::val_t> serialize(std::vector<T> const& uids){
    std::vector<typename T::val_t> raw;
    for(auto uid : uids) raw.push_back(uid.val);
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
