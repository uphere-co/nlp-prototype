#pragma once
#include <vector>

#include "wordrep/word_uid.h"
#include "wordrep/sentence.h"
#include "wordrep/words.h"
#include "wordrep/annotated_sentence.h"
#include "wordrep/wikientity_repr.h"
#include "wordrep/wikientity.h"

#include "utils/variant.h"

namespace wordrep{
struct DepParsedTokens;
}

namespace wikidata{

struct SortedEntities{
    std::vector<wordrep::wiki::Entity> entities;
};

struct TaggedEntity{
    size_t offset;
    size_t len;
    wordrep::WikidataUID uid;

    wordrep::ConsecutiveTokens map_to_sent(wordrep::Sentence const& sent) const {
        return {sent.front()+offset, len};
    }
    friend bool operator==(TaggedEntity x, TaggedEntity y){
        return x.uid==y.uid;
    }
};

struct WordWithOffset{
    size_t offset;
    wordrep::WordUID uid;
    friend bool operator==(WordWithOffset x, WordWithOffset y){
        return x.uid==y.uid;
    }
};

struct AnnotatedToken{
    mapbox::util::variant<WordWithOffset,wordrep::wiki::AmbiguousEntity> val;
    std::string repr(wordrep::wiki::EntityReprs const& entity_reprs,
                     wordrep::WikidataUIDindex const& wikidataUIDs,
                     wordrep::WordUIDindex const& wordUIDs) const;
    wordrep::AnnotatedSentence::Token to_sent_token(wordrep::Sentence const& sent) const {
        return val.match([&sent](WordWithOffset const& w)->wordrep::AnnotatedSentence::Token{
                             return {sent.front()+w.offset};},
                         [&sent](wordrep::wiki::AmbiguousEntity const& e)->wordrep::AnnotatedSentence::Token{
                             using T = wordrep::AnnotatedSentence::Token::UnresolvedWikiEntity;
                             return {T{e.map_to_sent(sent),e.uid}};});
    }
};

SortedEntities read_wikidata_entities(wordrep::WordUIDindex const& wordUIDs, std::istream&& is);
SortedEntities read_wikidata_entities(wordrep::WordUIDindex const& wordUIDs, std::string entity_file);



struct GreedyAnnotator{
    GreedyAnnotator(SortedEntities&& entities)
            : entities{std::move(entities.entities)} {
    }
    GreedyAnnotator(SortedEntities const& entities)
            : entities{entities.entities} {
    }

    std::vector<TaggedEntity> annotate(std::vector<wordrep::WordUID> const& text) const;
    wordrep::AnnotatedSentence annotate(wordrep::Sentence const& sent) const;

    std::vector<wordrep::wiki::Entity> entities;
};

//OP is one of OpCompare, OpEntityCompare and OpAmbiguousEntityCompare.
template<typename OP>
std::vector<wordrep::ConsecutiveTokens> is_contain(wordrep::Sentence const& sent,
                                          OP const& op){
    std::vector<wordrep::ConsecutiveTokens> offsets;
    auto iter_words = sent.iter_words();
    auto beg = iter_words.begin();
    auto end = iter_words.end();
    auto idx_beg=sent.front();
    for(auto it=beg; it!=end; ){
        auto n = op.exact_match(it,end);
        if(n){
            offsets.push_back({idx_beg+(it-beg),n});
            it = it + n;
        } else{
            ++it;
        }
    }
    return offsets;
}

std::vector<wordrep::WordPosition> head_word_pos(wordrep::DepParsedTokens const& dict,
                                                 wordrep::ConsecutiveTokens words);


struct EntityModule{
    EntityModule(std::string word_uids,
                 std::string pos_uids,
                 std::string wikidata_entities,
                 std::string named_entity_wikidata_uids,
                 std::string wikidata_uids)
            : wordUIDs{word_uids},
              posUIDs{pos_uids},
              entities{read_wikidata_entities(wordUIDs, wikidata_entities)},
              annotator{entities},
              entity_reprs{entities.entities},
              op_named_entity{named_entity_wikidata_uids, wordUIDs, entity_reprs},
              entityUIDs{wikidata_uids}
    {}
    EntityModule(EntityModule &&orig)
            : wordUIDs{std::move(orig.wordUIDs)},
              posUIDs{std::move(orig.posUIDs)},
              entities{std::move(orig.entities)},
              annotator{entities},
              entity_reprs{entities.entities},
              op_named_entity{std::move(orig.op_named_entity.named_entities),wordUIDs, entity_reprs},
              entityUIDs{std::move(orig.entityUIDs)}
    {}
    wordrep::WordUIDindex wordUIDs;
    wordrep::POSUIDindex posUIDs;
    SortedEntities entities;
    GreedyAnnotator annotator;
    wordrep::wiki::EntityReprs entity_reprs;
    wordrep::wiki::OpNamedEntity op_named_entity;
    wordrep::WikidataUIDindex entityUIDs;
//    wordrep::Scoring scoring{word_importance, voca.wvecs}
//    wordrep::Scoring::Preprocess scoring_preprocessor{scoring, entity_reprs, op_named_entity};
};
}//namespace wikidata
