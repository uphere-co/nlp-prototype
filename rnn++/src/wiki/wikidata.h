#pragma once

#include <sstream>
#include <iostream>
#include <vector>

#include "wordrep/word_uid.h"
#include "wordrep/sentence.h"

#include "utils/variant.h"

namespace wikidata{

struct Entity{
    Entity(wordrep::WikidataUID uid, std::vector<wordrep::WordUID> words)
            : uid{uid}, words{words}
    {}
    Entity(wordrep::WordUIDindex const& wordUIDs, std::string line);

    std::string repr(wordrep::WikidataUIDindex const& wikidataUIDs,
                     wordrep::WordUIDindex const& wordUIDs) const;
    size_t size() const{return words.size();}

    friend bool operator< (Entity const& a, Entity const& b){
        return a.words > b.words;
    }
    friend std::ostream& operator<< (std::ostream& os, Entity const& a);
    
    wordrep::WikidataUID uid;
    std::vector<wordrep::WordUID> words;
};
std::ostream& operator<< (std::ostream& os, Entity const& a);

struct SortedEntities{
    std::vector<Entity> entities;
};

SortedEntities read_wikidata_entities(wordrep::WordUIDindex const& wordUIDs, std::istream&& is);
SortedEntities read_wikidata_entities(wordrep::WordUIDindex const& wordUIDs, std::string entity_file);

struct TaggedEntity{
    size_t offset;
    Entity const& entity;
};
struct EntityReprs{
    struct OpExactMatch{
        OpExactMatch(EntityReprs const& self) : dict{self} {}
        bool operator() (wordrep::WikidataUID uid, std::vector<wordrep::WordUID> qwords) const {
            auto it = dict.reprs.find(uid);
            if(it==dict.reprs.cend()) return false;
            for(auto words : it->second) if(words == qwords) return true;
            return false;
        }
        template<typename TI>
        bool operator() (wordrep::WikidataUID uid, TI beg, TI end) const {
            auto elm = dict.reprs.find(uid);
            if(elm==dict.reprs.cend()) return false;
            for(auto words : elm->second){
                auto match=true;
                auto q=beg;
                for(auto it=words.begin(); it!=words.end(); ++it){
                    if(*q != *it) {
                        match=false;
                        break;
                    }
                    ++q;
                }
                if(match) return true;
            }
            return false;
        }
        EntityReprs const& dict;
    };
    EntityReprs(std::vector<Entity> const& entities){
        for(auto& entity : entities)
            reprs[entity.uid].push_back(entity.words);
    }
    OpExactMatch get_exact_match_operator() const{
        return {*this};
    }
    std::map<wordrep::WikidataUID, std::vector<std::vector<wordrep::WordUID>>> reprs;
};

struct AmbiguousEntity{
    std::vector<wordrep::WikidataUID> uids;
};
struct AnnotatedSentence{
    struct Token{
        mapbox::util::variant<wordrep::WordUID,AmbiguousEntity> token;
    };
    std::vector<Token> tokens;
};

struct GreedyAnnotator{
    GreedyAnnotator(SortedEntities&& entities)
            : entities{std::move(entities.entities)} {
    }
    GreedyAnnotator(SortedEntities entities)
            : entities{std::move(entities.entities)} {
    }

    std::vector<TaggedEntity> annotate(std::vector<wordrep::WordUID> const& text) const;
    AnnotatedSentence annotate(wordrep::Sentence const& sent) const;

    std::vector<Entity> entities;
};

}//namespace wikidata
