#pragma once
#include <string>
#include <random>
#include "utils/algorithm.h"

namespace util{

int64_t uuid_gen();
std::string get_uuid_str();

template<typename KEY, typename WEIGHT>
struct Sampler{
    Sampler(std::vector<std::pair<KEY,WEIGHT>> const &counts)
            : cdf{counts} { init(); }
    Sampler(std::vector<std::pair<KEY,WEIGHT>> &&counts)
            : cdf{std::move(counts)} { init(); }
    template<typename RNG>
    KEY sample(RNG& gen) {
        auto ran = u(gen);
        //10~20 % speed up. Check if no bias.
//        if(ran<n_low) return binary_find_cell(beg,it_low, [ran](auto x){return ran<x.second;}).value()->first;
        return binary_find_cell(cdf, [ran](auto x){return ran<x.second;}).value()->first;
    }

private:
    void init(){
        std::sort(cdf.begin(), cdf.end(), [](auto x, auto y){return x.second>y.second;});
        std::partial_sum(cdf.cbegin(),cdf.cend(), cdf.begin(),
                         [](auto x, auto y){return std::make_pair(y.first, x.second+y.second);});
        auto n_total = cdf.back().second;
//        for(auto ratio : {0.1,0.2,0.5,0.8}){ //,0.9,0.99
//            auto n_cutoff = n_total*ratio;
//            beg = cdf.cbegin();
//            it_low =std::find_if_not(beg,cdf.cend(),[n_cutoff ](auto x){return x.second<n_cutoff;});
//            n_low = it_low->second;
//        }
        u = std::uniform_int_distribution<size_t>{0, n_total-1};
    }
    std::vector<std::pair<KEY,WEIGHT>> cdf;
    std::uniform_int_distribution<size_t> u;
//    decltype(cdf.cbegin()) beg;
//    decltype(cdf.cbegin()) it_low;
//    std::uniform_real_distribution<double> ran01{0.0,1.0};
//    int64_t i_low;
//    double n_low;
};
template<typename KEY>
struct Sampler<KEY,double>{
    using WEIGHT = double;
    Sampler(std::vector<std::pair<KEY,WEIGHT>> &&counts)
            : cdf{std::move(counts)} { init(); }
    Sampler(std::vector<std::pair<KEY,WEIGHT>> const &counts)
            : cdf{counts} { init(); }
    template<typename RNG>
    KEY sample(RNG& gen) {
        auto ran = u(gen);
        return binary_find_cell(cdf, [ran](auto x){return ran<x.second;}).value()->first;
    }

private:
    void init(){
        std::sort(cdf.begin(), cdf.end(), [](auto x, auto y){return x.second>y.second;});
        std::partial_sum(cdf.cbegin(),cdf.cend(), cdf.begin(),
                         [](auto x, auto y){return std::make_pair(y.first, x.second+y.second);});
        auto n_total = cdf.back().second;
        u = std::uniform_real_distribution<double>{0, n_total};
    }
    std::vector<std::pair<KEY,WEIGHT>> cdf;
    std::uniform_real_distribution<double> u;
};

//TODO: define Sampler move constructor?
//template<typename KEY, typename WEIGHT>
//auto Sampler_factory(std::vector<std::pair<KEY,WEIGHT>> &&dist){
//    return Sampler<KEY,WEIGHT>{std::move(dist)};
//};

}
