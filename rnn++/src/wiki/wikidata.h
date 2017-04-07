#pragma once
#include <vector>
#include <memory>

#include "wiki/property_triple.h"

#include "wordrep/file_formats.h"
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

struct EntityModule{
    struct InputParam{
        wordrep::UIDIndexFile word_uids;
        wordrep::UIDIndexFile pos_uids;
        wordrep::WikiEntityByNameFile wikidata_entities;
        wordrep::WikiEntityByUIDFile wikidata_entities_by_uid;
        util::io::fb::PairsBinary wikidata_properties;
        util::io::fb::PairsBinary wikidata_instances;
        wordrep::UIDIndexFile named_entity_wikidata_uids;
        wordrep::UIDIndexFile wikidata_uids;
    };
    static EntityModule factory(InputParam const& param) {
        EntityModule f{};
        util::parallel_invoke(
                [&f,param](){f.wordUIDs = std::make_unique<wordrep::WordUIDindex>(param.word_uids);},
                [&f,param](){f.posUIDs = std::make_unique<wordrep::POSUIDindex>(param.pos_uids);},
                [&f,param](){f.wikiUIDs = std::make_unique<wordrep::WikidataUIDindex>(param.wikidata_uids);},
                [&f,param](){f.wiki_ne_UIDs = std::make_unique<wordrep::WikidataUIDindex>(param.named_entity_wikidata_uids);},
                [&f,param]() {
                    using util::io::fb::deserialize_pairs;
                    using util::io::fb::load_binary_file;
                    auto properties = deserialize_pairs<wikidata::PropertyOfEntity>(load_binary_file(param.wikidata_properties));
                    auto instances = deserialize_pairs<wikidata::EntityOfProperty>(load_binary_file(param.wikidata_instances));
                    f.prop_dict = std::make_unique<wikidata::PropertyTable>(std::move(properties), std::move(instances));
                },
                [&f,param](){f.entities = std::make_unique<wordrep::wiki::SortedEntities>(param.wikidata_entities);},
                [&f,param]() {
                    f.entities_by_uid = std::make_unique<wordrep::wiki::UIDSortedEntities>(
                            wordrep::wiki::read_binary_file(param.wikidata_entities_by_uid));
                }
        );
        f.greedy_annotator = std::make_unique<GreedyAnnotator>(*f.entities);
        f.entity_reprs     = std::make_unique<wordrep::wiki::EntityReprs>(*f.entities_by_uid);
        f.op_named_entity  = std::make_unique<wordrep::wiki::OpNamedEntity>(*f.wiki_ne_UIDs, *f.wordUIDs, *f.entity_reprs);
        return f;
    }
    EntityModule(EntityModule&& orig)
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
    EntityModule& operator=(EntityModule&& orig){
        wordUIDs     = std::move(orig.wordUIDs);
        posUIDs      = std::move(orig.posUIDs);
        wikiUIDs     = std::move(orig.wikiUIDs);
        wiki_ne_UIDs = std::move(orig.wiki_ne_UIDs);
        prop_dict    = std::move(orig.prop_dict);
        entities     = std::move(orig.entities);
        entities_by_uid  = std::move(orig.entities_by_uid);
        greedy_annotator = std::make_unique<GreedyAnnotator>(*entities);
        entity_reprs     = std::make_unique<wordrep::wiki::EntityReprs>(*entities_by_uid);
        op_named_entity  = std::make_unique<wordrep::wiki::OpNamedEntity>(*wiki_ne_UIDs,*wordUIDs,*entity_reprs);
        return *this;
    }
    wordrep::WordUIDindex const& word_uid() const {return *wordUIDs;}
    wordrep::POSUIDindex const& pos_uid() const {return *posUIDs;}
    wordrep::WikidataUIDindex const& wiki_uid() const {return *wikiUIDs;}
    GreedyAnnotator const& annotator() const{return *greedy_annotator;}
    PropertyTable const& properties() const{return *prop_dict;}
    wordrep::wiki::EntityReprs const& entity_repr() const {return *entity_reprs;}
    wordrep::wiki::OpNamedEntity const& get_op_named_entity() const{return *op_named_entity;}

private:
    EntityModule(){}

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

}//namespace wikidata
