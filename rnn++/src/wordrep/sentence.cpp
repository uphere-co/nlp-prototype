#include "wordrep/sentence.h"

#include <fmt/printf.h>

#include "wordrep/dep_parsed.h"

namespace wordrep {

Sentence::Sentence() : tokens{nullptr} {}
Sentence::Sentence(SentUID uid, DPTokenIndex beg, DPTokenIndex end, DepParsedTokens const *tokens)
: uid{uid}, beg{beg}, end{end}, tokens{tokens}
{}

CharOffset Sentence::beg_offset() const {return tokens->word_beg(front());}
CharOffset Sentence::end_offset() const {return tokens->word_end(back());}
SentUID::val_t Sentence::chrlen() const{ return util::diff(end_offset(), beg_offset());}
//std::string to_str() const{
//}
}//namespace wordrep
