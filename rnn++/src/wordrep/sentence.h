#pragma once

#include "wordrep/indexes.h"
#include "wordrep/word_uid.h"

namespace wordrep {

struct DepParsedTokens; //forward declaration.

struct Sentence;

struct SentenceRepr{
    SentenceRepr(Sentence const& sent, WordUIDindex const& wordUIDs);
    friend std::ostream& operator<<(std::ostream& os, SentenceRepr const& self);
private:
    Sentence const& sent;
    WordUIDindex const& wordUIDs;
};
std::ostream& operator<<(std::ostream& os, SentenceRepr const& self);

struct Sentence{
    struct Iterator{
        Iterator(DPTokenIndex idx) : idx{idx} {}

        DPTokenIndex operator*( void ) const {return idx;}
        void operator++(void)                {++idx;}
        bool operator!=(Iterator rhs ) const {return idx != rhs.idx;}
    private:
        DPTokenIndex idx;
    };
    Sentence();
    Sentence(SentUID uid, DPTokenIndex beg, DPTokenIndex end, DepParsedTokens const *tokens);

    DPTokenIndex front() const {return beg_token;}
    DPTokenIndex back() const {return end_token-1;}
    auto size() const {return util::diff(end_token, beg_token);}
    CharOffset beg_offset() const;
    CharOffset end_offset() const;
    SentUID::val_t chrlen() const;

    auto begin() const { return Iterator{beg_token};}
    auto end() const { return Iterator{end_token};}

    bool isin(WordUID idx) const;
    SentenceRepr repr(WordUIDindex const& wordUIDs) const;

    SentUID uid;
    DPTokenIndex beg_token;
    DPTokenIndex end_token;
    DepParsedTokens const *tokens;
};

}//namespace wordrep
