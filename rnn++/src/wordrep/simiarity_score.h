#pragma once

#include <utility>

//TODO: move headers to .cpp
#include "wordrep/annotated_sentence.h"
#include "wordrep/sentence.h"
#include "wordrep/words.h"
#include "wordrep/voca_info.h"
#include "wordrep/word_prob.h"
#include "wordrep/dep_parsed.h"
#include "wordrep/wikientity_repr.h"
#include "wordrep/wikientity.h"
#include "wordrep/wordsim_cache.h"

#include "utils/linear_algebra.h" //for scoring test
#include "utils/optional.h"

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
        wiki::AmbiguousUID uid;
        ConsecutiveTokens idxs;
        WordUID word_gov;
        VocaIndex gov;
    };
    struct SentenceToScored{
        SentenceToScored(SentenceToScored const& src)
        : orig{src.orig}, entities{src.entities}, words{src.words}
        {}
        SentenceToScored(Sentence const& sent)
                : orig{sent}, entities{}, words{}
        {}
        std::vector<wiki::AmbiguousUID> all_named_entities() const;
        void filter_false_named_entity(wiki::OpNamedEntity const &op,
                                       POSUIDindex const& posUIDs);
        Sentence const& orig;
        std::vector<AmbiguousEntity> entities;
        std::vector<DepPair> words;
    };
    struct Score{
        ConsecutiveTokens data;
        val_t score;
    };
    struct ScoredSentence{
        Sentence const& orig;
        std::vector<std::pair<AmbiguousEntity,std::optional<Score>>> entities;
        std::vector<std::pair<DepPair,std::optional<Score>>>         words;
    };
    struct Preprocess {
        SentenceToScored sentence(AnnotatedSentence const& orig) const {
            SentenceToScored sent{orig.sent};
            for(auto& token : orig) {
                using T = AnnotatedSentence::Token::UnresolvedWikiEntity;
                token.val.match([&sent,&orig](DPTokenIndex idx) {
                                    sent.words.push_back({orig.sent,idx});
                                },
                                [this,&sent,&orig](T const &entity) {
                                    auto& dict       = *orig.sent.dict;
                                    auto idx         = entity.words.dep_token_idx(dict);
                                    WordUID word_gov = dict.head_uid(idx);
                                    VocaIndex gov    = dict.head_word(idx);
                                    Scoring::AmbiguousEntity x{{},entity.uid, entity.words, word_gov,gov};
                                    for (auto uid : x.uid.candidates) {
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
    };

    struct OpSimilarity{
        struct OpWordSimilarity{
            val_t score(VocaIndex w1, VocaIndex w2) const {
                return op.match([w1,w2](auto const& op){return op.score(w1, w2);});
            }
            mapbox::util::variant<WordSimCache::WordSimOp,AngleSimilarity> op;
        };
        struct OpWordImportance{
            val_t score(WordUID word) const{
                return op.match([word](WordImportance const* wm){
                    return wm->score(word);
                    }, [word](std::map<WordUID,val_t> const& cache){
                    return cache.find(word)->second;
                    });
            }
            mapbox::util::variant<WordImportance const*,std::map<WordUID,val_t>> op;
        };
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

        std::optional<Score> similarity(DepPair query, SentenceToScored const& data) const{
            val_t max_score = 0.0;
            std::optional<ConsecutiveTokens> best_match={};
            for(auto word : data.words) {
                auto score = similarity(query, word);
                if (max_score > score) continue;
                max_score = score;
                best_match = ConsecutiveTokens{word.idx};
            }
            if(best_match)
                return Score{best_match.value(), max_score};
            return {};
        }
        std::optional<Score> similarity(AmbiguousEntity const& query, SentenceToScored const& data) const{
            val_t max_score = 0.0;
            std::optional<ConsecutiveTokens> best_match={};
            for(auto& entity : data.entities) {
                auto score = similarity(query, entity);
                if(max_score>=score) continue;
                max_score = score;
                best_match = entity.idxs;
            }
            if(best_match)
                return Score{best_match.value(), max_score};
            return {};
        }
        std::optional<ScoredSentence> similarity(SentenceToScored const& query, SentenceToScored const& data) const{
            ScoredSentence scored_sent{data.orig,{},{}};
            for(auto& entity : query.entities) {
                auto m_score = similarity(entity, data);
                if(!m_score) return {};
                scored_sent.entities.push_back(std::make_pair(entity, m_score));
            }
            for(auto& word : query.words)
                scored_sent.words.push_back(std::make_pair(word, similarity(word, data)));
            return {scored_sent};
        }
        OpWordImportance word_importance;
        OpWordSimilarity op;
    };
    OpSimilarity op_similarity() const{
        return {{&word_importance}, {op}};
    }
    //TODO: make a cached version.
    struct OpSentenceSimilarity{
        OpSimilarity op;
        SentenceToScored query;
        mutable std::optional<WordSimCache::WordSimOp> op_cached={};
        std::optional<ScoredSentence> score(SentenceToScored const&data) const{
            return op.similarity(query, data);
        }
    };
    OpSentenceSimilarity op_sentence_similarity(SentenceToScored const& query) const {
        std::map<WordUID,val_t> word_importance_cache;
        for(auto word : query.words){
            word_importance_cache[word.word_dep]=word_importance.score(word.word_dep);
            word_importance_cache[word.word_gov]=word_importance.score(word.word_gov);
        }
        for(auto e : query.entities){
            word_importance_cache[e.word_gov]=word_importance.score(e.word_gov);
        }
        return {{{word_importance_cache}, {op}}, query};
    }
    OpSentenceSimilarity op_sentence_similarity(SentenceToScored const& query,
                                                WordSimCache::WordSimOp const& op_cached) const {
        std::map<WordUID,val_t> word_importance_cache;
        for(auto word : query.words){
            word_importance_cache[word.word_dep]=word_importance.score(word.word_dep);
            word_importance_cache[word.word_gov]=word_importance.score(word.word_gov);
        }
        for(auto e : query.entities){
            word_importance_cache[e.word_gov]=word_importance.score(e.word_gov);
        }
        return {{{word_importance_cache}, {op_cached}}, query};
    }

    Words max_score_repr(wiki::Synonyms const& synonym) const {
        auto it = std::max_element(synonym.reprs.cbegin(),synonym.reprs.cend(),[this](auto const& x, auto const& y){
            return this->phrase(x)<this->phrase(y);
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
            return this->phrase(x)<this->phrase(y);
        });
        return phrase(*it);
    }
    WordImportance const& word_importance;
    AngleSimilarity op;
};


}//namespace wordrep
