#pragma once

//TODO: move headers to .cpp
#include "wordrep/sentence.h"
#include "wordrep/words.h"
#include "wordrep/voca_info.h"
#include "wordrep/word_prob.h"
#include "wordrep/dep_parsed.h"
#include "wordrep/wikientity_repr.h"
#include "wordrep/wikientity.h"

#include "utils/linear_algebra.h" //for scoring test

namespace wordrep{

struct DepPair{
    DepPair(Sentence const& sent, DPTokenIndex idx)
            : word_gov{sent.dict->head_uid(idx)}, word_dep{sent.dict->word_uid(idx)},
              gov{sent.dict->head_word(idx)}, dep{sent.dict->word(idx)}, idx{idx}
    {}
    WordUID word_gov;
    WordUID word_dep;
    VocaIndex gov;
    VocaIndex dep;
    DPTokenIndex idx;
};

struct AngleSimilarity{
    AngleSimilarity(VocaInfo::voca_vecs_t const& wvecs)
            : wvecs{wvecs}
    {}
    auto score(VocaIndex w1, VocaIndex w2) const{
        auto v = wvecs[w1];
        auto q = wvecs[w2];
        using namespace util::math;
        return dot(v,q)/std::sqrt(dot(v,v)*dot(q,q));
    }

    VocaInfo::voca_vecs_t const& wvecs;
};

struct Scoring{
    using val_t = WordImportance::val_t;
    Scoring(WordImportance const& word_importance, AngleSimilarity const& op)
            : word_importance{word_importance}, op{op}
    {}
    auto score(DepPair query, DepPair data) const {
        auto score_gov = 1+op.score(query.gov, data.gov)*word_importance.score(query.word_gov);
        auto score_dep = op.score(query.dep, data.dep)*word_importance.score(query.word_dep);
        auto score = score_gov*score_dep;
        return score;
    }
    auto score(Words const& words) const {
        WordImportance::val_t score{0.0};
        for(auto word : words) score+=word_importance.score(word);
        return score;
    }
    auto score(wiki::Synonyms const& entity) const {
        auto it = std::max_element(entity.reprs.cbegin(),entity.reprs.cend(),[this](auto const& x, auto const& y){
            return score(x)<score(y);
        });
        return score(*it);
    }

    WordImportance const& word_importance;
    AngleSimilarity op;
};


struct ScoredEntity{
    WikidataUID uid;
    Scoring::val_t score;
    ConsecutiveTokens idxs;
};
struct ScoredAmbiguousEntity{
    struct Entity{
        WikidataUID uid;
        Scoring::val_t score;
    };
    std::vector<Entity> candidates;
    ConsecutiveTokens idxs;
};

//TODO: merge with wikidata::head_word_pos.
DPTokenIndex center_word(DepParsedTokens const& dict, ConsecutiveTokens words){
    auto positions = util::map(words,[&dict](auto idx){return dict.word_pos(idx);});
    for(auto idx : words){
        auto mh = dict.head_pos(idx);
        if(!mh) continue;
        auto head = mh.value();
        if(util::isin(positions, head)) continue;
        return idx;
    }
    return words.front();
}

Words max_score_repr(std::vector<Words> const& reprs, Scoring const& scoring){
    auto it = std::max_element(reprs.cbegin(),reprs.cend(),[&scoring](auto const& x, auto const& y){
        return scoring.score(x)<scoring.score(y);
    });
    return *it;
}

struct SentenceToScored{
    SentenceToScored(AnnotatedSentence const& sent,
                     Scoring const& scoring,
                     wiki::EntityReprs const& entity_reprs,
                     wiki::OpNamedEntity const& op)
            : orig{sent.sent} {
        for(auto& token : sent) {
            using T = AnnotatedSentence::Token::UnresolvedWikiEntity;
            token.val.match([this](DPTokenIndex idx) {
                                words.push_back({orig,idx});
                            },
                            [this,&op,&scoring,&entity_reprs](T const &entity) {
                                std::vector<WikidataUID> named_entities;
                                for (auto uid : entity.uids)
                                    if(op.is_named_entity(uid)) named_entities.push_back(uid);
                                if(named_entities.empty()){
                                    for(auto idx : entity.words)
                                        words.push_back({orig,idx});
                                    return;
                                }
                                ScoredAmbiguousEntity x{{},entity.words};
                                for (auto uid : named_entities) {
                                    auto synonyms = entity_reprs.get_synonyms(uid);
                                    auto repr = max_score_repr(synonyms.reprs, scoring);
                                    x.candidates.push_back({uid, scoring.score(repr)});
                                }
                                entities.push_back(x);
                            });
        }
    }
    Sentence const& orig;
    std::vector<ScoredAmbiguousEntity> entities;
    std::vector<DepPair> words;
};

}//namespace wordrep

