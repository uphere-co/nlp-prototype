#include "similarity/phrase_suggestion.h"

#include <functional>
#include <fmt/printf.h>

#include "utils/algorithm.h"
#include "utils/linear_algebra.h"
#include "utils/parallel.h"
#include "utils/profiling.h"
#include "utils/string.h"

namespace engine{

std::pair<WordUsageInPhrase::counts_t,WordUsageInPhrase::reprs_t>
WordUsageInPhrase::usages(wordrep::WordUID word, float_t cutoff) const {
    using wordrep::Words;
    util::Timer timer;

    tbb::concurrent_vector<Words> phrase_usages;
    auto filter_noisy_word = [this](auto uids){
        return util::filter(uids, [this](auto uid){return importance.score(uid)>0.5;});
    };
    auto n=sents.size();
    tbb::parallel_for(decltype(n){0}, n, [&](auto i) {
        auto sent = sents[i];
        //Ignore too long sentences. It matters for YGP DB.
        if(!sent.isin(word) || sent.size()>100) return;
        auto phrases = phrase_segmenter.broke_into_phrases(sent, cutoff);
        for(auto phrase : phrases){
            if(phrase.size()>10 || phrase.size()==1) continue;
            if(phrase.isin(word)) {
                phrase_usages.push_back(Words{phrase.to_word_uids()});
            }
        }
    });
    timer.here_then_reset(fmt::format("WordUsageInPhrase::usages found {} phrases among {} sents",
                                      phrase_usages.size(), sents.size()));
    std::map<Words,int64_t> phrase_count;
    std::map<Words,std::map<Words,int64_t>> phrase_reprs;
    for(auto& words_in_phrase : phrase_usages){
        Words repr{filter_noisy_word(words_in_phrase.uids)};
        phrase_count[repr] += 1;
        phrase_reprs[repr][words_in_phrase] += 1;

    }
    timer.here_then_reset("WordUsageInPhrase::usages serialize results.");
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
    timer.here_then_reset("WordUsageInPhrase::usages sort results.");
    return std::make_pair(counts, phrase_reprs);
}

util::json_t get_query_suggestion(std::vector<wordrep::WordUID> const& wuids,
                                  WordUsageInPhrase const& phrase_finder,
                                  wordrep::WordUIDindex const& wordUIDs,
                                  WordUsageInPhrase::float_t cutoff){
    util::json_t output = util::json_t::array();
    util::Timer timer;
    for(auto wuid : wuids){
        auto usage = phrase_finder.usages(wuid, cutoff);
        auto& counts = usage.first;
        auto& reprs = usage.second;
        util::json_t suggestion{};
        suggestion["idea"]=wordUIDs[wuid];
        suggestion["suggestions"] = util::json_t::array();
        auto rank=0;
        for(auto pair : counts){
            auto& phrase = pair.first;
            auto phrase_usages = reprs[phrase];
            auto max_usage_case = std::max_element(phrase_usages.cbegin(), phrase_usages.cend(),
                                                   [](auto x, auto y){return x.second<y.second;});
            auto repr = max_usage_case->first;
            //discard itself, a single word phrase.
            if(repr.uids.size()==1) continue;
            auto count = pair.second;
            if(count <2) continue;
            std::ostringstream ss;
            ss << repr.repr(wordUIDs);
            suggestion["suggestions"].push_back({util::string::strip(ss.str()), count, rank++});
            //20 is a cutoff to limit number of suggestions per word.
            if(rank>20) break;
        }
        output.push_back(suggestion);
        timer.here_then_reset(fmt::format("Process query suggestion for a word : {}", wordUIDs[wuid]));
    }
    return output;
}

} //namespace engine
