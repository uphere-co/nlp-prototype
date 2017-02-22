#pragma once

#include <sstream>
#include <iostream>
#include <vector>

#include "wordrep/word_uid.h"
#include "wordrep/sentence.h"

#include "utils/variant.h"

namespace wordrep{
struct DepParsedTokens;
}
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
    size_t len;
    wordrep::WikidataUID uid;
};

struct EntityReprs{
    using dict_type = std::map<wordrep::WikidataUID, std::vector<std::vector<wordrep::WordUID>>>;
    using value_type = dict_type::value_type;
    struct OpCompare{
        OpCompare(EntityReprs const& self) : dict{self} {}
        bool exact_match(wordrep::WikidataUID uid, std::vector<wordrep::WordUID> qwords) const {
            auto it = dict.reprs.find(uid);
            if(it==dict.reprs.cend()) return false;
            OpEntityCompare op{*it};
            return op.exact_match(qwords);
        }
        template<typename TI>
        size_t exact_match(wordrep::WikidataUID uid, TI beg, TI end) const {
            auto it = dict.reprs.find(uid);
            if(it ==dict.reprs.cend()) return 0;
            OpEntityCompare op{*it};
            return op.exact_match(beg,end);
        }
        EntityReprs const& dict;
    };
    struct OpEntityCompare{
        OpEntityCompare(value_type const& reprs) : reprs{reprs} {}
        bool exact_match(std::vector<wordrep::WordUID> qwords) const {
            for(auto words : reprs.second) if(words == qwords) return true;
            return false;
        }
        template<typename TI>
        size_t exact_match(TI beg, TI end) const {
            for(auto words : reprs.second){
                auto match=words.size();
                auto q=beg;
                for(auto it=words.begin(); it!=words.end(); ++it){
                    if(*q != *it || q==end) {
                        match=0;
                        break;
                    }
                    ++q;
                }
                if(match) return match;
            }
            return 0;
        }
        value_type const& reprs;
    };
    EntityReprs(std::vector<Entity> const& entities){
        for(auto& entity : entities)
            reprs[entity.uid].push_back(entity.words);
    }
    OpCompare get_comparison_operator() const{
        return {*this};
    }
    OpEntityCompare get_exact_match_operator(wordrep::WikidataUID uid) const{
        auto it = reprs.find(uid);
        if(it==reprs.cend()) assert(0);
        return {*it};
    }
    Entity operator[](wordrep::WikidataUID uid) const;

    dict_type reprs;
};

struct AmbiguousEntity{
    std::vector<TaggedEntity> entities;
};
struct WordWithOffset{
    size_t offset;
    size_t len;
    wordrep::WordUID uid;
};
struct AnnotatedSentence{
    struct Token{
        mapbox::util::variant<WordWithOffset,AmbiguousEntity> token;
    };
    std::vector<Token> tokens;
};

struct GreedyAnnotator{
    GreedyAnnotator(SortedEntities&& entities)
            : entities{std::move(entities.entities)} {
    }
    GreedyAnnotator(SortedEntities const& entities)
            : entities{entities.entities} {
    }

    std::vector<TaggedEntity> annotate(std::vector<wordrep::WordUID> const& text) const;
    AnnotatedSentence annotate(wordrep::Sentence const& sent) const;

    std::vector<Entity> entities;
};


struct ConsecutiveTokens{
    struct Iterator{
        Iterator(wordrep::DPTokenIndex idx) : idx{idx} {}
        wordrep::DPTokenIndex operator*( void ) const {return idx;}
        void operator++(void)                {++idx;}
        bool operator!=(Iterator rhs ) const {return idx != rhs.idx;}
    private:
        wordrep::DPTokenIndex idx;
    };
    Iterator begin() const { return {idx};}
    Iterator end() const { return {idx+len};}
    size_t size() const {return len;}

    wordrep::DPTokenIndex idx;
    size_t len;
};

std::vector<ConsecutiveTokens> is_contain(wordrep::Sentence const& sent,
                                          EntityReprs::OpEntityCompare const& op);
std::vector<wordrep::WordPosition> head_word(wordrep::DepParsedTokens const& dict, ConsecutiveTokens words);

}//namespace wikidata
