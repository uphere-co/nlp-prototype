#include "wordrep/sentence.h"

#include <ostream>

#include "wordrep/dep_parsed.h"

namespace wordrep {

SentenceRepr::SentenceRepr(Sentence const& sent, WordUIDindex const& wordUIDs)
        : sent{sent}, wordUIDs{wordUIDs}
{}
std::ostream& operator<<(std::ostream& os, SentenceRepr const& self) {
    for(auto idx : self.sent){
        os << self.wordUIDs[self.sent.tokens->word_uid(idx)]<< " ";
    }
    return os;
}


Sentence::Sentence() : tokens{nullptr} {}
Sentence::Sentence(SentUID uid, DPTokenIndex beg, DPTokenIndex end, DepParsedTokens const *tokens)
: uid{uid}, tokens{tokens}, beg_token{beg}, end_token{end}
{}

CharOffset Sentence::beg_offset() const {return tokens->word_beg(front());}
CharOffset Sentence::end_offset() const {return tokens->word_end(back());}
SentUID::val_t Sentence::chrlen() const{ return util::diff(end_offset(), beg_offset());}
//std::string to_str() const{
//}

bool Sentence::isin(WordUID word) const {
    for(auto idx=beg_token; idx!=end_token; ++idx)
        if(tokens->word_uid(idx)==word) return true;
    return false;
}
SentenceRepr Sentence::repr(WordUIDindex const& wordUIDs) const {
    return SentenceRepr{*this, wordUIDs};
}

}//namespace wordrep
