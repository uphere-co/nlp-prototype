#include "models/sentence2vec.h"

#include "utils/math.h"
#include "utils/base_types.h"
#include "utils/algorithm.h"

namespace sent2vec  {

UnigramDist::UnigramDist(util::io::H5file const &h5store,
                         std::string voca_file, std::string count_file)
:   voca{util::deserialize<wordrep::WordUID>(h5store.getRawData<int64_t>(util::io::H5name{voca_file}))},
    count{h5store.getRawData<count_t>(util::io::H5name{count_file})},
    prob(count.size()) {
    auto norm = 1.0 / util::math::sum(count);
    for(size_t i=0; i<count.size(); ++i) {
        prob[i]=count[i]*norm;
    }
}
val_t UnigramDist::get_prob(wordrep::VocaIndex idx) const {
    auto i=idx.val;
    if(i<0 || i >= util::to_type<int64_t>(prob.size())) return -1.0;
    return prob[idx.val];
}

/*
OccurrenceFilter::OccurrenceFilter(wcount_t cutoff, UnigramDist const &unigram)
: unigram{unigram}, cutoff{cutoff} {}
std::vector<idx_t> OccurrenceFilter::operator() (std::vector<idx_t> idxs){
    std::vector<idx_t> filtered;
    for(auto idx:idxs)
        if(unigram.get_count(idx)>=cutoff)
            filtered.push_back(idx);
    return filtered;
}
*/

WordVecContext::WordVecContext(iter_t self, widxs_t const &widxs,
                   idx_t left, idx_t right)
: widx{*self} {
    auto beg=widxs.cbegin();
    auto end=widxs.cend();
    assert(self<end);
    auto left_beg = self-left<beg? beg : self-left;
    auto right_end= self+1+right>end? end : self+1+right;
    std::copy(left_beg, self, std::back_inserter(cidxs));
    std::copy(self+1, right_end, std::back_inserter(cidxs));
}

SentVecContext::SentVecContext(idx_t sidx, iter_t self, widxs_t const &widxs, 
                   idx_t left, idx_t right)
: sidx{sidx}, widx{*self} {
    auto beg=widxs.cbegin();
    auto end=widxs.cend();
    assert(self<end);
    auto left_beg = self-left<beg? beg : self-left;
    auto right_end= self+1+right>end? end : self+1+right;
    std::copy(left_beg, self, std::back_inserter(cidxs));
    std::copy(self+1, right_end, std::back_inserter(cidxs));
}


val_t scoring_context(WordVecContext const &context, 
                     rnn::wordrep::WordBlock_base<word_dim> const &voca_vecs){
    using util::math::dot;
    auto w=voca_vecs[context.widx];
    val_t score{0};
    for(auto cidx: context.cidxs) {
        auto c=voca_vecs[cidx];
        auto xy = dot(w,c)/std::sqrt(dot(w,w)*dot(c,c));
        score += std::log( 1/(1+std::exp(-xy)));
        //score+=std::log(sigmoid(w,c));
    }
    return score;
}
val_t scoring_words(std::vector<idx_t> const &widxs,
                    rnn::wordrep::WordBlock_base<word_dim> const &voca_vecs){
    val_t score{0};
    for(auto self=widxs.cbegin(); self!=widxs.end(); ++self){
        WordVecContext context{self, widxs, 5,5};
        score+=scoring_context(context, voca_vecs);
    }
    return score;
}

}//namespace sent2vec