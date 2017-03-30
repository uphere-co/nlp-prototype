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
#include "utils/flatbuffers/io.h"

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

struct EntityModuleBuilder;

struct EntityModule{
    EntityModule(std::string word_uids,
                 std::string pos_uids,
                 std::string wikidata_entities,
                 std::string wikidata_properties,
                 std::string named_entity_wikidata_uids,
                 std::string wikidata_uids)
            : wordUIDs{std::make_unique<wordrep::WordUIDindex>(word_uids)},
              posUIDs{std::make_unique<wordrep::POSUIDindex>(pos_uids)},
              wikiUIDs{std::make_unique<wordrep::WikidataUIDindex>(wikidata_uids)},
              wiki_ne_UIDs{std::make_unique<wordrep::WikidataUIDindex>(named_entity_wikidata_uids)},
              prop_dict{std::make_unique<PropertyTable>(wikidata_properties)},
              entities{std::make_unique<wordrep::wiki::SortedEntities>(read_wikidata_entities(*wordUIDs, wikidata_entities))},
              entities_by_uid{std::make_unique<wordrep::wiki::UIDSortedEntities>(entities->to_uid_sorted())},
              greedy_annotator{std::make_unique<GreedyAnnotator>(*entities)},
              entity_reprs{std::make_unique<wordrep::wiki::EntityReprs>(*entities_by_uid)},
              op_named_entity{std::make_unique<wordrep::wiki::OpNamedEntity>(*wiki_ne_UIDs,*wordUIDs, *entity_reprs)}
    {}
    EntityModule(EntityModule &&orig)
            : wordUIDs{std::move(orig.wordUIDs)},
              posUIDs{std::move(orig.posUIDs)},
              wikiUIDs{std::move(orig.wikiUIDs)},
              wiki_ne_UIDs{std::move(orig.wiki_ne_UIDs)},
              prop_dict{std::move(orig.prop_dict)},
              entities{std::move(orig.entities)},
              entities_by_uid{std::move(orig.entities_by_uid)},
              greedy_annotator{std::make_unique<GreedyAnnotator>(*entities)},
              entity_reprs{std::make_unique<wordrep::wiki::EntityReprs>(*entities_by_uid)},
              op_named_entity{std::make_unique<wordrep::wiki::OpNamedEntity>(*wiki_ne_UIDs,*wordUIDs,*entity_reprs)}
    {}
    wordrep::WordUIDindex const& word_uid() const {return *wordUIDs;}
    wordrep::POSUIDindex const& pos_uid() const {return *posUIDs;}
    wordrep::WikidataUIDindex const& wiki_uid() const {return *wikiUIDs;}
    GreedyAnnotator const& annotator() const{return *greedy_annotator;}
    PropertyTable const& properties() const{return *prop_dict;}
    wordrep::wiki::EntityReprs const& entity_repr() const {return *entity_reprs;}
    wordrep::wiki::OpNamedEntity const& get_op_named_entity() const{return *op_named_entity;}

    friend struct EntityModuleBuilder;
protected:
    EntityModule(){}
private:
    std::unique_ptr<wordrep::WordUIDindex> wordUIDs;
    std::unique_ptr<wordrep::POSUIDindex> posUIDs;
    std::unique_ptr<wordrep::WikidataUIDindex> wikiUIDs;
    std::unique_ptr<wordrep::WikidataUIDindex> wiki_ne_UIDs;
    std::unique_ptr<PropertyTable> prop_dict;
    std::unique_ptr<wordrep::wiki::SortedEntities> entities;
    std::unique_ptr<wordrep::wiki::UIDSortedEntities> entities_by_uid;
    std::unique_ptr<GreedyAnnotator> greedy_annotator;
    std::unique_ptr<wordrep::wiki::EntityReprs> entity_reprs;
    std::unique_ptr<wordrep::wiki::OpNamedEntity> op_named_entity;
};

struct EntityModuleBuilder {
    EntityModule build(wordrep::UIDIndexBinary word_uids,
                        wordrep::UIDIndexBinary pos_uids,
                        wordrep::wiki::SortedEntities::Binary wikidata_entities,
                        wordrep::wiki::UIDSortedEntities::Binary wikidata_entities_by_uid,
                        util::io::fb::PairsBinary wikidata_properties,
                        util::io::fb::PairsBinary wikidata_instances,
                        wordrep::UIDIndexBinary named_entity_wikidata_uids,
                        wordrep::UIDIndexBinary wikidata_uids) const {
        EntityModule f{};
        tbb::task_group g;
        g.run([&f,&word_uids](){f.wordUIDs = std::make_unique<wordrep::WordUIDindex>(word_uids);});
        g.run([&f,&pos_uids](){f.posUIDs = std::make_unique<wordrep::POSUIDindex>(pos_uids);});
        g.run([&f,&wikidata_uids](){f.wikiUIDs = std::make_unique<wordrep::WikidataUIDindex>(wikidata_uids);});
        g.run([&f,&named_entity_wikidata_uids](){f.wiki_ne_UIDs = std::make_unique<wordrep::WikidataUIDindex>(named_entity_wikidata_uids);});
        g.run([&f,&wikidata_properties,&wikidata_instances](){
            using util::io::fb::deserialize_pairs;
            using util::io::fb::load_binary_file;
            auto properties = deserialize_pairs<wikidata::PropertyOfEntity>(load_binary_file(wikidata_properties));
            auto instances  = deserialize_pairs<wikidata::EntityOfProperty>(load_binary_file(wikidata_instances));
            f.prop_dict = std::make_unique<wikidata::PropertyTable>(std::move(properties),std::move(instances));
        });
        g.run([&f,&wikidata_entities](){f.entities = std::make_unique<wordrep::wiki::SortedEntities>(wikidata_entities);});
        g.run([&f,&wikidata_entities_by_uid](){f.entities_by_uid  = std::make_unique<wordrep::wiki::UIDSortedEntities>(wikidata_entities_by_uid);});
        g.wait();
        f.greedy_annotator = std::make_unique<GreedyAnnotator>(*f.entities);
        f.entity_reprs     = std::make_unique<wordrep::wiki::EntityReprs>(*f.entities_by_uid);
        f.op_named_entity  = std::make_unique<wordrep::wiki::OpNamedEntity>(*f.wiki_ne_UIDs, *f.wordUIDs, *f.entity_reprs);
        return f;
    }
};

}//namespace wikidata
