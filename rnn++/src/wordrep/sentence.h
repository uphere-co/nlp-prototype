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
    Sentence();
    Sentence(SentUID uid, DPTokenIndex beg, DPTokenIndex end, DepParsedTokens const *tokens);

    DPTokenIndex front() const {return beg;}
    DPTokenIndex back() const {return end-1;}
    auto size() const {return util::diff(end, beg);}
    CharOffset beg_offset() const;
    CharOffset end_offset() const;
    SentUID::val_t chrlen() const;

    bool isin(WordUID idx) const;
    SentenceRepr repr(WordUIDindex const& wordUIDs) const;

    SentUID uid;
    DPTokenIndex beg;
    DPTokenIndex end;
    DepParsedTokens const *tokens;
};

}//namespace wordrep
