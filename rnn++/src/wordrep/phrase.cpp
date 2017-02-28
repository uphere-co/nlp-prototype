#include "wordrep/phrase.h"

#include <ostream>

#include "wordrep/sentence.h"
#include "wordrep/dep_parsed.h"

namespace wordrep{

PhraseRepr::PhraseRepr(Phrase const& phrase, WordUIDindex const& wordUIDs)
        : phrase{phrase}, wordUIDs{wordUIDs}
{}

std::ostream& operator<<(std::ostream& os, PhraseRepr const& self) {
    for(auto uid : self.phrase.to_word_uids()){
        os << self.wordUIDs[uid]<< " ";
    }
    return os;
}

Phrase::Phrase(Sentence const& sent, std::vector<DPTokenIndex> &&tokens)
: idxs{std::move(tokens)}, sent{sent} {
    std::sort(idxs.begin(), idxs.end());
}

std::vector<WordUID> Phrase::to_word_uids() const {
    return util::map(idxs, [this](auto idx){
        return sent.dict->word_uid(idx);
    });
}
bool Phrase::isin(WordUID word) const {
    for(auto idx : idxs)
        if(sent.dict->word_uid(idx)==word) return true;
    return false;
};
PhraseRepr Phrase::repr(WordUIDindex const& wordUIDs) const{
    return PhraseRepr{*this, wordUIDs};
}

}//namespace wordrep
