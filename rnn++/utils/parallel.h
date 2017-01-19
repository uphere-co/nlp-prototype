#pragma once

#include <tbb/concurrent_vector.h>
#include <tbb/concurrent_hash_map.h>
#include <tbb/task_group.h>
#include <tbb/tbb.h>
#include <tbb/parallel_reduce.h>
#include <tbb/parallel_sort.h>
#include <tbb/blocked_range.h>
#include <tbb/task_group.h>

namespace util {

template<typename T>
struct TBBHashCompare {
    static size_t hash(T const& x) {return std::hash<T>{}(x);}
    static bool equal(T const& x, T const& y ) {return x==y;}
};

template<typename IT, typename OP, typename TVAL>
TVAL parallel_reducer(IT beg, IT end, OP reducer, TVAL zero){
    auto sum=tbb::parallel_reduce(
        tbb::blocked_range<IT>{beg, end},
        zero,
        //current_sum should be const & or copied by value.
        [&reducer]( tbb::blocked_range<decltype(beg)> const &r, TVAL current_sum ) {
            // std::cerr<<r.size()<< " : blocked_range"<<std::endl;
            for (auto it=r.begin(); it!=r.end(); ++it) {
                current_sum += reducer(*it); 
            }
            return current_sum; // body returns updated value of the accumulator
        },
        [](TVAL const &x,TVAL const &y){return x+y;}
    );
    return sum;
}

template<typename T>
struct ConcurrentVector{
    void push_back(T const& value ) {vec.push_back(value);}
    void push_back(T&& value ) {vec.push_back(std::move(value));}
    auto to_vector(){
        std::vector<T> vs;
        for(auto const &v : vec) vs.push_back(v);
        return vs;
    }
    tbb::concurrent_vector<T> vec;
};

}//namespace util

