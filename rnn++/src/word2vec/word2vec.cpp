#include <fmt/printf.h>

#include "word2vec/word2vec.h"

#include "utils/math.h"
#include "utils/optional.h"
#include "utils/algorithm.h"

namespace word2vec {

UnigramDist::UnigramDist(util::io::H5file const &h5store)
        :   uid{h5store,"unigram.uid"},
            count{h5store,"unigram.count"},
            voca{util::TypedPersistentVector<WordUID>{h5store,"widx2wuid"}.get()},
            prob(count.size()) {
    auto norm = 1.0 / util::math::sum(count.get());
    for(size_t i=0; i<count.size(); ++i) prob[i]=count[i]*norm;
}

UnigramDist::float_t UnigramDist::get_prob(wordrep::WordUID idx) const{
    auto it = util::binary_find(uid.get(), idx);
    if(!it) return 0.0;
    //TODO: figure out why .get() is necessary??
    auto i = it.value()-uid.get().cbegin();
    return prob[i];
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