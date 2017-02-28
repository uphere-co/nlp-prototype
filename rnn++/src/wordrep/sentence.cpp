#include "wordrep/sentence.h"

#include <ostream>

#include "wordrep/dep_parsed.h"

namespace wordrep {

SentenceRepr::SentenceRepr(Sentence const& sent, WordUIDindex const& wordUIDs)
        : sent{sent}, wordUIDs{wordUIDs}
{}
std::ostream& operator<<(std::ostream& os, SentenceRepr const& self) {
    for(auto idx : self.sent){
        os << self.wordUIDs[self.sent.dict->word_uid(idx)]<< " ";
    }
    return os;
}

WordUID Sentence::WordIterator::Iterator::operator*( void ) const {return dict->word_uid(idx);}

Sentence::Sentence() : dict{nullptr} {}
Sentence::Sentence(SentUID uid, DPTokenIndex beg, DPTokenIndex end, DepParsedTokens const *dict)
: uid{uid}, dict{dict}, beg_token{beg}, end_token{end}
{}

CharOffset Sentence::beg_offset() const {return dict->word_beg(front());}
CharOffset Sentence::end_offset() const {return dict->word_end(back());}
SentUID::val_t Sentence::chrlen() const{ return util::diff(end_offset(), beg_offset());}
//std::string to_str() const{
//}

bool Sentence::isin(WordUID word) const {
    for(auto idx=beg_token; idx!=end_token; ++idx)
        if(dict->word_uid(idx)==word) return true;
    return false;
}
SentenceRepr Sentence::repr(WordUIDindex const& wordUIDs) const {
    return SentenceRepr{*this, wordUIDs};
}

}//namespace wordrep
