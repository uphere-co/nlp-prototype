#pragma once

#include <random>
#include <map>
#include "wordrep/indexes.h"

//TODO:Remove following header.
#include "utils/linear_algebra.h"
#include "utils/parallel.h"

namespace util{
namespace io{
struct H5file; //forward declaration
}
}

namespace wordrep{
class VocaIndexMap;
}//namespace wordrep

namespace word2vec {

struct UnigramDist {
    using WordUID = wordrep::WordUID;
    using VocaIndex = wordrep::VocaIndex;
    using float_t = float;
    UnigramDist(std::map<VocaIndex,int64_t> const& counts);

    float_t get_prob(VocaIndex idx) const;
    std::vector<std::pair<VocaIndex,float_t>> get_neg_sample_dist(float_t pow) const;
    size_t size() const {return weights.size();}

    std::vector<std::pair<VocaIndex,float_t>> weights;
};

class SubSampler{
public:
    using float_t = UnigramDist::float_t;
    using VocaIndex = wordrep::VocaIndex;

    SubSampler(float_t rate, UnigramDist const &unigram);

    bool operator() (VocaIndex word, float_t ran) const { return is_sampled(unigram.get_prob(word), ran);}

private:
    bool is_sampled(float_t p_word, float_t ran) const;

    UnigramDist const &unigram;
    float_t rate_inv;
};

struct WordContext{
    using idx_t = std::ptrdiff_t;
    using VocaIndex = wordrep::VocaIndex;
    WordContext(idx_t self, std::vector<VocaIndex> const& words,
                idx_t left, idx_t right)
    : self{self} {
        idx_t beg = 0;
        idx_t end = words.end()-words.begin();
        auto left_beg = self-left<beg? beg : self-left;
        auto right_end= self+1+right>end? end : self+1+right;
        for(auto i=left_beg; i!=self; ++i) contexts.push_back(i);
        for(auto i=self+1; i!=right_end; ++i) contexts.push_back(i);
    }
    idx_t self;
    std::vector<idx_t> contexts;
};

}//namespace word2vec


//TODO: move into right namespaces

//T is RandomNumberDistribution
template<typename T>
std::vector<typename T::result_type> random_vector(size_t len, T dist){
    std::random_device rd{};
    auto seed = rd();
    std::vector<typename T::result_type> vec(len);
    auto n=vec.size();
    tbb::parallel_for(tbb::blocked_range<decltype(n)>(0,n),
                      [dist,seed,&vec] (tbb::blocked_range<decltype(n)> const &r)  {
                          std::mt19937 gen{seed+r.begin()};
                          auto d=dist;
                          for(auto i=r.begin(); i!=r.end(); ++i)
                              vec[i]= d(gen);
                      });
    return vec;
}
template<typename T>
std::vector<typename T::result_type> random_vector_serial(size_t len, T dist){
    std::random_device rd{};
    auto seed = rd();
    std::vector<typename T::result_type> vec(len);
    std::mt19937 gen{seed};
    for(auto& x : vec) x = dist(gen);
    return vec;
}

auto symm_fma_vec = [](int64_t i,auto x, auto const &vec1, auto const &vec2){
    auto tmp = vec2[i];
    vec2[i] += x*vec1[i];
    vec1[i] += x*tmp;
};
auto sigmoid=[](auto const &x, auto const &y){
    using util::math::dot;
    auto xy = dot(x,y);
    return  1/(1+std::exp(-xy));
};
auto sigmoid_plus=[](auto const &x, auto const &y){
    using util::math::dot;
    auto xy = dot(x,y);
    return  1/(1+std::exp(xy));
};

