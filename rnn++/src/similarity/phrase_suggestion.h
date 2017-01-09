#pragma once

#include <vector>

#include "wordrep/sentence.h"
#include "wordrep/word_prob.h"
#include "wordrep/dep_graph.h"

#include "utils/algorithm.h"

namespace engine{

struct WordUsageInPhrase{
    WordUsageInPhrase(std::vector<Sentence> const& sents,
                      WordImportance const& importance_)
            : sents{sents}, importance{importance_}, phrase_segmenter{importance}
    {}
    auto usages(WordUID word) const {
        std::map<std::vector<WordUID>,int> phrase_count;
        for(auto sent : sents){
            if(!sent.isin(word)) continue;
            auto phrases = phrase_segmenter.broke_into_phrases(sent, 5.0);
            for(auto phrase : phrases){
                if(phrase.size()>10 || phrase.size()==1) continue;
                if(phrase.isin(word))
                    phrase_count[phrase.to_word_uids()] += 1;
            }
        }
        auto counts = util::to_pairs(phrase_count);
        auto score_phrase_count = [this](std::pair<std::vector<WordUID>,int> const& phrase_with_count){
            auto uids=phrase_with_count.first;
            auto count=phrase_with_count.second;
            auto score_sum = util::math::sum(util::map(uids, [this](auto uid){
                return importance.score(uid);
            }));
            return score_sum*std::sqrt(count)*std::pow(uids.size(), -1.5);
        };
        std::sort(counts.begin(), counts.end(), [score_phrase_count](auto x, auto y){
            return score_phrase_count(x)>score_phrase_count(y);
        });
        return counts;
    }

private:
    std::vector<Sentence> const& sents;
    WordImportance const& importance;
    PhraseSegmenter phrase_segmenter;
};

} //namespace engine
