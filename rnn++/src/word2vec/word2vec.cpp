#include "word2vec/word2vec.h"

#include <fmt/printf.h>

#include "wordrep/voca.h"

#include "utils/math.h"
#include "utils/optional.h"
#include "utils/algorithm.h"
#include "utils/persistent_vector.h"

namespace word2vec {

UnigramDist::UnigramDist(std::map<VocaIndex,int64_t> const& counts){
    int64_t sum{0};
    for(auto& elm : counts) sum+=elm.second;
    auto norm = 1.0/sum;
    weights.reserve(counts.size());
    for(auto& elm : counts) weights.push_back({elm.first, elm.second*norm});
    std::sort(weights.begin(),weights.end(), [](auto x, auto y){return x.first<y.first;});
}

UnigramDist::float_t UnigramDist::get_prob(VocaIndex idx) const{
    auto it = util::binary_find(weights,
                                [idx](auto x){return idx==x.first;},
                                [idx](auto x){return idx< x.first;});
    if(!it) return 1.0;
    return it.value()->second;
}
std::vector<std::pair<UnigramDist::VocaIndex,UnigramDist::float_t>>
UnigramDist::get_neg_sample_dist(float_t pow) const {
    return util::map(weights, [pow](auto x){return std::make_pair(x.first, std::pow(x.second, pow));});
}

SubSampler::SubSampler(float_t rate, UnigramDist const &unigram)
    : unigram{unigram}, rate_inv{1.0f/rate} {}

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