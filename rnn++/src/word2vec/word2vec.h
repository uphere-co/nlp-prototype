#pragma once

#include <random>
#include "wordrep/voca.h"

#include "utils/persistent_vector.h"

namespace word2vec {

struct UnigramDist {
    using WordUID = wordrep::WordUID;
    using float_t = double;
    UnigramDist(util::io::H5file const &h5store);

    float_t get_prob(wordrep::VocaIndex idx) const;

    util::PersistentVector<WordUID,WordUID::val_t>   uid;
    util::PersistentVector<size_t,size_t> count;
    wordrep::VocaIndexMap voca;
    std::vector<float_t> prob;
};

class SubSampler{
public:
    using float_t = UnigramDist::float_t;
    using VocaIndex = wordrep::VocaIndex;

    SubSampler(float_t rate, UnigramDist const &unigram);
    bool operator() (VocaIndex word) { return is_sampled(unigram.get_prob(word), uni01(gen));}
private:
    bool is_sampled(float_t p_word, float_t ran) const;

    UnigramDist const &unigram;
    float_t rate_inv;
    std::random_device rd{};
    std::mt19937 gen{rd()};
    std::uniform_real_distribution<float_t> uni01{0.0,1.0};
};

struct WordContext{
    using idx_t = std::ptrdiff_t;
    using VocaIndex = wordrep::VocaIndex;
    WordContext(idx_t self, idx_t beg, idx_t end,
                int left, int right)
    : self{self} {
        auto left_beg = self-left<beg? beg : self-left;
        auto right_end= self+1+right>end? end : self+1+right;
        for(auto i=left_beg; i!=self; ++i) contexts.push_back(i);
        for(auto i=self+1; i!=right_end; ++i) contexts.push_back(i);
    }
    idx_t self;
    std::vector<idx_t> contexts;
};
}//namespace word2vec