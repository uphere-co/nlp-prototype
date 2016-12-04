#pragma once

#include "wordrep/indexes.h"

namespace wordrep {

struct DepParsedTokens; //forward declaration.
struct Sentence{
    Sentence() : tokens{nullptr} {}
    Sentence(SentUID uid, DPTokenIndex beg, DPTokenIndex end, DepParsedTokens const *tokens)
            : uid{uid}, beg{beg}, end{end}, tokens{tokens} {}
    DPTokenIndex front() const {return beg;}
    DPTokenIndex back() const {return end-1;}
    CharOffset beg_offset() const;
    CharOffset end_offset() const;
    SentUID::val_t chrlen() const;
    SentUID uid;
    DPTokenIndex beg;
    DPTokenIndex end;
    DepParsedTokens const *tokens;
};

}
