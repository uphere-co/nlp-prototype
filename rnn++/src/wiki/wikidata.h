#pragma once

#include <sstream>
#include <iostream>
#include <vector>

#include "wordrep/word_uid.h"
#include "wordrep/sentence.h"
#include "wordrep/words.h"

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

struct Synonyms{
    wordrep::WikidataUID uid;
    std::vector<wordrep::Words> reprs;
};

struct TaggedEntity{
    size_t offset;
    size_t len;
    wordrep::WikidataUID uid;
    friend bool operator==(TaggedEntity x, TaggedEntity y){
        return x.uid==y.uid;
    }
};

struct AmbiguousEntity{
    std::vector<TaggedEntity> entities;
    friend bool operator==(AmbiguousEntity const& x, AmbiguousEntity const& y){
        for(auto& xx : x.entities )
            for(auto& yy : y.entities )
                if(xx.uid==yy.uid) return true;
        return false;
    }
    friend bool operator!=(AmbiguousEntity const& x, AmbiguousEntity const& y){
        return !(x==y);
    }
};

struct WordWithOffset{
    size_t offset;
    wordrep::WordUID uid;
    friend bool operator==(WordWithOffset x, WordWithOffset y){
        return x.uid==y.uid;
    }
};

struct EntityReprs;
struct AnnotatedSentence{
    struct Token{
        mapbox::util::variant<WordWithOffset,AmbiguousEntity> val;
        std::string repr(EntityReprs const& entity_reprs,
                         wordrep::WikidataUIDindex const& wikidataUIDs,
                         wordrep::WordUIDindex const& wordUIDs) const;
    };
    struct Iterator{
        Iterator(AnnotatedSentence const& sent, size_t idx) : idx{idx}, sent{sent}{}
        auto const& operator*( void ) const {return sent.tokens[idx];}
        void operator++(void)                {++idx;}
        bool operator==(Iterator rhs ) const {return idx == rhs.idx;}
        bool operator!=(Iterator rhs ) const {return idx != rhs.idx;}
    private:
        size_t idx;
        AnnotatedSentence const& sent;
    };
    auto get_entities() const{
        std::vector<AmbiguousEntity> entities;
        for(auto token : tokens)
            token.val.match([&entities](AmbiguousEntity x){entities.push_back(x);},
                              [](auto ){});
        return entities;
    }
    auto begin() const { return Iterator{*this,0};}
    auto end() const { return Iterator{*this,tokens.size()};}
    std::vector<Token> tokens;
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


SortedEntities read_wikidata_entities(wordrep::WordUIDindex const& wordUIDs, std::istream&& is);
SortedEntities read_wikidata_entities(wordrep::WordUIDindex const& wordUIDs, std::string entity_file);


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
        value_type reprs;
    };
    struct OpAmbiguousEntityCompare{
        bool exact_match(std::vector<wordrep::WordUID> qwords) const {
            for(auto& op : ops)
                if(op.exact_match(qwords)) return true;
            return false;
        }
        template<typename TI>
        size_t exact_match(TI beg, TI end) const {
            for(auto& op : ops) {
                auto n = op.exact_match(beg, end);
                if (n) return n;
            }
            return 0;
        }
        std::vector<OpEntityCompare> ops;
    };
    EntityReprs(std::vector<Entity> const& entities){
        for(auto& entity : entities)
            reprs[entity.uid].push_back(entity.words);
    }
    OpCompare get_comparison_operator() const{
        return {*this};
    }
    OpEntityCompare get_comparison_operator(wordrep::WikidataUID uid) const{
        auto it = reprs.find(uid);
        if(it==reprs.cend()) assert(0);
        return {*it};
    }
    OpAmbiguousEntityCompare get_comparison_operator(AmbiguousEntity entity) const{
        OpAmbiguousEntityCompare op{};
        for(auto x : entity.entities) op.ops.push_back(get_comparison_operator(x.uid));
        return op;
    }
    Entity operator[](wordrep::WikidataUID uid) const;
    Synonyms get_synonyms(wordrep::WikidataUID uid) const;

    dict_type reprs;
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

template<typename OP>
std::vector<ConsecutiveTokens> is_contain(wordrep::Sentence const& sent,
                                          OP const& op){
    std::vector<ConsecutiveTokens> offsets;
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

std::vector<wordrep::WordPosition> head_word(wordrep::DepParsedTokens const& dict, ConsecutiveTokens words);

}//namespace wikidata
