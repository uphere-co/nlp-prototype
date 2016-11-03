#pragma once

#include "tbb/concurrent_vector.h"
#include "tbb/concurrent_hash_map.h"
#include "tbb/task_group.h"
#include "tbb/tbb.h"
#include "tbb/parallel_reduce.h"
#include "tbb/blocked_range.h"

#include "tbb/task_group.h"

namespace util {

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

}//namespace util