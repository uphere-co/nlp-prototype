#include "similarity/phrase_suggestion.h"

#include <functional>

#include "utils/algorithm.h"
#include "utils/linear_algebra.h"
#include "utils/parallel.h"

namespace engine{

std::pair<WordUsageInPhrase::counts_t,WordUsageInPhrase::reprs_t>
WordUsageInPhrase::usages(wordrep::WordUID word) const {
    using wordrep::Words;

    tbb::concurrent_vector<Words> phrase_usages;
    auto filter_noisy_word = [this](auto uids){
        return util::filter(uids, [this](auto uid){return importance.score(uid)>0.5;});
    };
    auto n=sents.size();
    tbb::parallel_for(decltype(n){0}, n, [&](auto i) {
//    for(auto sent : sents){
        auto sent = sents[i];
        if(!sent.isin(word)) return;    
        auto phrases = phrase_segmenter.broke_into_phrases(sent, 5.0);
        for(auto phrase : phrases){
            if(phrase.size()>10 || phrase.size()==1) continue;
            if(phrase.isin(word)) {
                phrase_usages.push_back(Words{phrase.to_word_uids()});
            }
        }
    });
    std::map<Words,int64_t> phrase_count;
    std::map<Words,std::map<Words,int64_t>> phrase_reprs;
    for(auto& words_in_phrase : phrase_usages){
        Words repr{filter_noisy_word(words_in_phrase.uids)};
        phrase_count[repr] += 1;
        phrase_reprs[repr][words_in_phrase] += 1;

    }
    auto counts = util::to_pairs(phrase_count);
    auto score_phrase_count = [this](std::pair<Words,int64_t> const& phrase_with_count){
        auto uids=phrase_with_count.first.uids;
        auto count=phrase_with_count.second;
        auto score_sum = util::math::sum(util::map(uids, [this](auto uid){
            return importance.score(uid);
        }));
        return score_sum*std::sqrt(count)*std::pow(uids.size(), -1.5);
    };
    std::sort(counts.begin(), counts.end(), [score_phrase_count](auto x, auto y){
        return score_phrase_count(x)>score_phrase_count(y);
    });
    return std::make_pair(counts, phrase_reprs);
}

} //namespace engine
