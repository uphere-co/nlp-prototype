#pragma once

#include <random>
#include "wordrep/voca.h"

#include "utils/persistent_vector.h"

namespace word2vec {

struct UnigramDist {
    using WordUID = wordrep::WordUID;
    using VocaIndex = wordrep::VocaIndex;
    using float_t = double;
    UnigramDist(util::io::H5file const &h5store);

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
    template<typename RNG>
    bool operator() (VocaIndex word, RNG& gen) { return is_sampled(unigram.get_prob(word), uni01(gen));}
private:
    bool is_sampled(float_t p_word, float_t ran) const;

    UnigramDist const &unigram;
    float_t rate_inv;
    std::uniform_real_distribution<float_t> uni01{0.0,1.0};
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