#pragma once

#include <vector>

#include "wordrep/indexes.h"
#include "wordrep/words.h"
#include "wordrep/sentence.h"

#include "utils/variant.h"

namespace wordrep{

struct WikidataEntityReprs;

struct AnnotatedSentence{
    struct Token{
        struct Word{
            DPTokenIndex idx;
            DepParsedTokens const* dict;
        };
        struct UnresolvedWikiEntity{
            ConsecutiveTokens words;
            std::vector<WikidataUID> uids;

            friend bool operator==(UnresolvedWikiEntity const& x, UnresolvedWikiEntity const& y){
                for(auto& xx : x.uids )
                    for(auto& yy : y.uids )
                        if(xx==yy) return true;
                return false;
            }
            friend bool operator!=(UnresolvedWikiEntity const& x, UnresolvedWikiEntity const& y){
                return !(x==y);
            }
        };
        std::string repr(WikidataEntityReprs const& entity_reprs,
                         WikidataUIDindex const& wikidataUIDs,
                         WordUIDindex const& wordUIDs) const;
        mapbox::util::variant<Word,UnresolvedWikiEntity> val;
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

    Sentence const& sent;
    std::vector<Token> tokens;
};

}//namespace wordrep
