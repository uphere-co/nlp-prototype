#pragma once

#include "wordrep/wikientity_base.h"

#include "utils/variant.h"

namespace wordrep{
namespace wiki {
struct EntityReprs;
}//namespace wordrep::wiki

struct AnnotatedSentence{
    struct Token{
        struct UnresolvedWikiEntity{
            ConsecutiveTokens words;
            wiki::AmbiguousUID uid;

            friend bool operator==(UnresolvedWikiEntity const& x, UnresolvedWikiEntity const& y){
                for(auto& xx : x.uid.candidates )
                    for(auto& yy : y.uid.candidates )
                        if(xx==yy) return true;
                return false;
            }
            friend bool operator!=(UnresolvedWikiEntity const& x, UnresolvedWikiEntity const& y){
                return !(x==y);
            }
        };
        mapbox::util::variant<DPTokenIndex,UnresolvedWikiEntity> val;
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
    std::vector<Token::UnresolvedWikiEntity> get_entities() const{
        std::vector<Token::UnresolvedWikiEntity> entities;
        for(auto token : tokens)
            token.val.match([&entities](Token::UnresolvedWikiEntity& x){entities.push_back(x);},
                            [](auto ){});
        return entities;
    }
    Iterator begin() const { return {*this,0};}
    Iterator end() const { return {*this,tokens.size()};}
    std::string repr(wiki::EntityReprs const& entity_reprs,
                     WikidataUIDindex const& wikidataUIDs,
                     WordUIDindex const& wordUIDs) const;

    Sentence const& sent;
    std::vector<Token> tokens;
};

}//namespace wordrep
