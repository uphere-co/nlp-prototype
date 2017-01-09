#pragma once

#include <vector>

#include "wordrep/sentence.h"
#include "wordrep/words.h"
#include "wordrep/word_prob.h"
#include "wordrep/dep_graph.h"

#include "utils/algorithm.h"

#include "utils/linear_algebra.h"

namespace engine{

struct PhraseUsage{

    std::vector<wordrep::WordUID> phrase;
    std::map<std::vector<wordrep::WordUID>,int> phrase_reprs;
    int64_t count;
};

struct WordUsageInPhrase{
    WordUsageInPhrase(std::vector<wordrep::Sentence> const& sents,
                      wordrep::WordImportance const& importance_)
            : sents{sents}, importance{importance_}, phrase_segmenter{importance}
    {}
    auto usages(wordrep::WordUID word) const {
        using wordrep::Words;
        std::map<Words,int> phrase_count;
        std::map<Words,std::map<Words,int>> phrase_reprs;

        auto filter_noisy_word = [this](auto uids){
            return util::filter(uids, [this](auto uid){return importance.score(uid)>0.5;});
        };
        for(auto sent : sents){
            if(!sent.isin(word)) continue;
            auto phrases = phrase_segmenter.broke_into_phrases(sent, 5.0);
            for(auto phrase : phrases){
                if(phrase.size()>10 || phrase.size()==1) continue;
                if(phrase.isin(word)) {
                    Words words_in_phrase{phrase.to_word_uids()};
                    Words repr{filter_noisy_word(words_in_phrase.uids)};
                    phrase_count[repr] += 1;
                    phrase_reprs[repr][words_in_phrase] += 1;
                }
            }
        }
        auto counts = util::to_pairs(phrase_count);
        auto score_phrase_count = [this](std::pair<Words,int> const& phrase_with_count){
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

private:
    std::vector<wordrep::Sentence> const& sents;
    wordrep::WordImportance const& importance;
    wordrep::PhraseSegmenter phrase_segmenter;
};

} //namespace engine
