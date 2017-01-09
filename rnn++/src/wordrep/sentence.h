#pragma once

#include <string>

#include "wordrep/indexes.h"

namespace wordrep {

struct DepParsedTokens; //forward declaration.
struct Sentence{
    Sentence();
    Sentence(SentUID uid, DPTokenIndex beg, DPTokenIndex end, DepParsedTokens const *tokens);

    bool isin(WordUID idx) const;
    DPTokenIndex front() const {return beg;}
    DPTokenIndex back() const {return end-1;}
    auto size() const {return util::diff(end, beg);}
    CharOffset beg_offset() const;
    CharOffset end_offset() const;
    SentUID::val_t chrlen() const;
    //TODO: Implement this
    //std::string to_str() const;
    SentUID uid;
    DPTokenIndex beg;
    DPTokenIndex end;
    DepParsedTokens const *tokens;
};

}
