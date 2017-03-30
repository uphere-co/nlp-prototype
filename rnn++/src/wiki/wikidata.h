#pragma once
#include <vector>
#include <memory>

#include "wiki/property_triple.h"

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

wordrep::wiki::SortedEntities read_wikidata_entities(wordrep::WordUIDindex const& wordUIDs, std::istream&& is);
wordrep::wiki::SortedEntities read_wikidata_entities(wordrep::WordUIDindex const& wordUIDs, std::string entity_file);



struct GreedyAnnotator{
    GreedyAnnotator(wordrep::wiki::SortedEntities const& entities)
            : entities{entities} {
    }

    std::vector<TaggedEntity> annotate(std::vector<wordrep::WordUID> const& text) const;
    wordrep::AnnotatedSentence annotate(wordrep::Sentence const& sent) const;
private:
    wordrep::wiki::SortedEntities const& entities;
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
                 std::string wikidata_properties,
                 std::string named_entity_wikidata_uids,
                 std::string wikidata_uids)
            : wordUIDs{std::make_unique<wordrep::WordUIDindex>(word_uids)},
              posUIDs{std::make_unique<wordrep::POSUIDindex>(pos_uids)},
              entities{std::make_unique<wordrep::wiki::SortedEntities>(read_wikidata_entities(*wordUIDs, wikidata_entities))},
              entities_by_uid{std::make_unique<wordrep::wiki::UIDSortedEntities>(entities->to_uid_sorted())},
              annotator{*entities},
              prop_dict{wikidata_properties},
              entity_reprs{*entities_by_uid},
              op_named_entity{named_entity_wikidata_uids, *wordUIDs, entity_reprs},
              entityUIDs{wikidata_uids}
    {}
    EntityModule(EntityModule &&orig)
            : wordUIDs{std::move(orig.wordUIDs)},
              posUIDs{std::move(orig.posUIDs)},
              entities{std::move(orig.entities)},
              entities_by_uid{std::move(orig.entities_by_uid)},
              annotator{*entities},
              prop_dict{std::move(orig.prop_dict)},
              entity_reprs{*entities_by_uid},
              op_named_entity{std::move(orig.op_named_entity.named_entities),*wordUIDs, entity_reprs},
              entityUIDs{std::move(orig.entityUIDs)}
    {}
    wordrep::WordUIDindex const& word_uid() const {return *wordUIDs;}
    wordrep::POSUIDindex const& pos_uid() const {return *posUIDs;}
//    wordrep::wiki::SortedEntities const& entities_sorted() const {return *entities;}

    std::unique_ptr<wordrep::WordUIDindex> wordUIDs;
    std::unique_ptr<wordrep::POSUIDindex> posUIDs;
    std::unique_ptr<wordrep::wiki::SortedEntities> entities;
    std::unique_ptr<wordrep::wiki::UIDSortedEntities> entities_by_uid;
    GreedyAnnotator annotator;
    PropertyTable  prop_dict;
    wordrep::wiki::EntityReprs entity_reprs;
    wordrep::wiki::OpNamedEntity op_named_entity;
    wordrep::WikidataUIDindex entityUIDs;
//    wordrep::Scoring scoring{word_importance, voca.wvecs}
//    wordrep::Scoring::Preprocess scoring_preprocessor{scoring, entity_reprs, op_named_entity};
};
}//namespace wikidata
