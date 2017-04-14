#pragma once

namespace util{

template<typename TIDX>
struct IndexRange{
    struct Iterator{
        Iterator(TIDX idx) : idx{idx} {}
        TIDX operator*( void ) const {return idx;}
        void operator++(void) {++idx;}
        bool operator==(Iterator rhs) const {return idx == rhs.idx;}
        bool operator!=(Iterator rhs) const {return idx != rhs.idx;}
    private:
        TIDX idx;
    };
    IndexRange(TIDX beg, TIDX end) : beg_{beg},end_{end} {}
    Iterator begin() const { return {beg_};}
    Iterator end()   const { return {end_};}
    size_t size() const {return end_.val - beg_.val;}
private:
    TIDX beg_;
    TIDX end_;
};

}//namespace util