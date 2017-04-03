#pragma once

#include <tbb/concurrent_vector.h>
#include <tbb/concurrent_hash_map.h>
#include <tbb/task_group.h>
#include <tbb/tbb.h>
#include <tbb/parallel_reduce.h>
#include <tbb/parallel_sort.h>
#include <tbb/blocked_range.h>
#include <tbb/task_group.h>

#include <fmt/printf.h>

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
    auto to_vector() const {
        std::vector<T> vs;
        vs.reserve(vec.size());
        for(auto const &v : vec) vs.push_back(v);
        return vs;
    }
    tbb::concurrent_vector<T> vec;
};


template<typename T, typename F1>
void parallel_invoke_impl(T& root, F1 const& f1) {
    fmt::print("parallel_invoce_1\n");
    root.run_and_finish(f1);
}
template<typename T, typename F1, typename F2>
void parallel_invoke_impl(T& root, F1 const& f1, F2 const& f2) {
    fmt::print("parallel_invoce_2\n");
    root.add_children(f2);
    root.run_and_finish(f1);
}
template<typename T, typename F1, typename F2, typename F3>
void parallel_invoke_impl(T& root, F1 const& f1, F2 const& f2, F3 const& f3) {
    fmt::print("parallel_invoce_3\n");
    root.add_children(f3);
    root.add_children(f2);
    root.run_and_finish(f1);
}

template<typename T, typename F1, typename F2, typename F3, typename... Args>
void parallel_invoke_impl(T& root, F1 const& f1, F2 const& f2, F3 const& f3, Args&&... args) {
    fmt::print("parallel_invoce_impl\n");
    root.add_children(f1,f2,f3);
    parallel_invoke_impl(root, std::forward<Args>(args)...);
}


template<typename... Args>
void parallel_invoke(Args&&... args) {
    tbb::task_group_context context;
    constexpr int n_args = sizeof...(args);
    auto n_pack = n_args<=3? n_args : (n_args%3? n_args/3+1 : n_args/3);

    tbb::internal::parallel_invoke_cleaner cleaner(n_pack, context);
    tbb::internal::parallel_invoke_helper& root = cleaner.root;

    fmt::print("parallel_invoce\n");
    parallel_invoke_impl(root, std::forward<Args>(args)...);
}

}//namespace util

