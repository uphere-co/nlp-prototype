#include "word2vec/word2vec.h"

#include "utils/math.h"
#include "utils/optional.h"

namespace word2vec {

UnigramDist::UnigramDist(util::io::H5file const &h5store)
        :   uid{h5store,"unigram.uid"},
            count{h5store,"unigram.count"},
            voca{uid.get()},
            prob(count.size()) {
    auto norm = 1.0 / util::math::sum(count.get());
//    util::map(count, [])
    for(size_t i=0; i<count.size(); ++i) {
        prob[i]=count[i]*norm;
    }
}

UnigramDist::float_t UnigramDist::get_prob(wordrep::VocaIndex idx) const {
    auto i=idx.val;
    if(i<0 || i >= util::to_type<int64_t>(prob.size())) return -1.0;
    return prob[idx.val];
}

SubSampler::SubSampler(float_t rate, UnigramDist const &unigram)
    : unigram{unigram}, rate_inv{1.0/rate} {}

bool SubSampler::is_sampled(float_t p_word, float_t ran) const {
    if(p_word<0.0) return false;
    //x>1 : typical word
    //x<1 : rare word
    auto x = p_word*rate_inv;
    auto p = (std::sqrt(x)+1)/x;
    if(p < ran) return false;
    return true;
};

}//namespace word2vec