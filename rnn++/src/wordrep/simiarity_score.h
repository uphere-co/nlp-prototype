#pragma once

//TODO: move headers to .cpp
#include "wordrep/annotated_sentence.h"
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
    std::string repr(WordUIDindex const& wordUIDs) const;

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
    struct Entity{
        WikidataUID uid;
        val_t score;
        ConsecutiveTokens idxs;
        WordUID word_gov;
        VocaIndex gov;
    };
    struct AmbiguousEntity{
        struct Candidate{
            WikidataUID uid;
            val_t score;
            friend bool operator==(Candidate x, Candidate y){
                return x.uid==y.uid;
            }
        };
        std::string repr(DepParsedTokens const& dict, WordUIDindex const& wordUIDs) const;

        std::vector<Candidate> candidates;
        ConsecutiveTokens idxs;
        WordUID word_gov;
        VocaIndex gov;
    };
    struct Preprocess {
        struct SentenceToScored{
            Sentence const& orig;
            std::vector<Scoring::AmbiguousEntity> entities;
            std::vector<DepPair> words;
        };
        SentenceToScored sentence(AnnotatedSentence const& orig) const {
            SentenceToScored sent{orig.sent,{},{}};
            for(auto& token : orig) {
                using T = AnnotatedSentence::Token::UnresolvedWikiEntity;
                token.val.match([&sent,&orig](DPTokenIndex idx) {
                                    sent.words.push_back({orig.sent,idx});
                                },
                                [this,&sent,&orig](T const &entity) {
                                    std::vector<WikidataUID> named_entities;
                                    for (auto uid : entity.uids)
                                        if(op.is_named_entity(uid)) named_entities.push_back(uid);
                                    if(named_entities.empty()){
                                        for(auto idx : entity.words)
                                            sent.words.push_back({orig.sent,idx});
                                        return;
                                    }
                                    auto& dict       = *orig.sent.dict;
                                    auto idx         = entity.words.dep_token_idx(dict);
                                    WordUID word_gov = dict.head_uid(idx);
                                    VocaIndex gov    = dict.head_word(idx);
                                    Scoring::AmbiguousEntity x{{},entity.words, word_gov,gov};
                                    for (auto uid : named_entities) {
                                        auto synonyms = entity_reprs.get_synonyms(uid);
                                        auto repr = scoring.max_score_repr(synonyms);
                                        x.candidates.push_back({uid, scoring.phrase(repr)});
                                    }
                                    sent.entities.push_back(x);
                                });
            }
            return sent;
        }
        Scoring const &scoring;
        wiki::EntityReprs const &entity_reprs;
        wiki::OpNamedEntity const &op;
    };


    Words max_score_repr(wiki::Synonyms const& synonym) const {
        auto it = std::max_element(synonym.reprs.cbegin(),synonym.reprs.cend(),[this](auto const& x, auto const& y){
            return phrase(x)<phrase(y);
        });
        return *it;
    }

    val_t phrase(Words const &words) const {
        WordImportance::val_t score{0.0};
        for(auto word : words) score+=word_importance.score(word);
        return score;
    }
    val_t phrase(wiki::Synonyms const& entity) const {
        auto it = std::max_element(entity.reprs.cbegin(),entity.reprs.cend(),[this](auto const& x, auto const& y){
            return phrase(x)<phrase(y);
        });
        return phrase(*it);
    }

    val_t similarity(DepPair query, DepPair data) const {
        auto score_gov = 1+op.score(query.gov, data.gov)*word_importance.score(query.word_gov);
        auto score_dep = op.score(query.dep, data.dep)*word_importance.score(query.word_dep);
        auto score = score_gov*score_dep;
        return score;
    }
    val_t similarity(AmbiguousEntity const& query, AmbiguousEntity const& data) const {
        val_t score_dep = 0.0;
        for(auto e1 : query.candidates){
            if(!util::isin(data.candidates, e1)) continue;
            score_dep = std::max(e1.score,score_dep);
        }
        auto score_gov = 1+op.score(query.gov, data.gov)*word_importance.score(query.word_gov);
        auto score = score_gov*score_dep;
        return score;
    }
    val_t similarity(DepPair query, Preprocess::SentenceToScored const& data) const{
        
        return 0.0;
    }
    WordImportance const& word_importance;
    AngleSimilarity op;
};


}//namespace wordrep
