#pragma once

#include <vector>
#include <string>

#include "wordrep/word_uid.h"
#include "wordrep/wikientity_repr.h"

namespace wordrep{
namespace wiki{

struct OpAcronym{
    std::string to_acronym(Words const& words) const {
        std::string acronym;
        for(auto& word : words){
            auto str = wordUIDs[word];
            if(str=="and"||str=="the"||str=="of"||!std::isalpha(str.front())) continue;
            acronym.push_back(std::toupper(str.front()));
        }
        return acronym;
    }
    WordUID to_acronyms(wiki::Synonyms const& entity) const {
        for(auto& words : entity.reprs){
            auto acronym = wordUIDs[to_acronym(words)];
            for(auto& repr : entity.reprs){
                if(repr.size()!=1) continue;
                if(repr.front()==acronym) return acronym;
            }
        }
        return the_unknown_word_uid();
    }
    bool is_acronyms(wiki::Synonyms const& entity) const {
        for(auto& words : entity.reprs){
            auto acronym = wordUIDs[to_acronym(words)];
            for(auto& repr : entity.reprs){
                if(repr.size()!=1) continue;
                if(repr.front()==acronym) return true;
            }
        }
        return false;
    }

    WordUIDindex const& wordUIDs;
};

struct OpNamedEntity{
    OpNamedEntity(WikidataUIDindex const& named_entities, WordUIDindex const& wordUIDs, EntityReprs const& entity_reprs)
            : named_entities{named_entities}, op_acronym{wordUIDs}, entity_reprs{entity_reprs}
    {}
    bool is_named_entity(wordrep::WikidataUID uid) const {
        return named_entities.isin(uid)
               || op_acronym.is_acronyms(entity_reprs.get_synonyms(uid));
    }
    WikidataUIDindex const& named_entities;
    OpAcronym op_acronym;
    EntityReprs const& entity_reprs;
};

}//namespace wordrep::wiki
}//namespace wordrep

