#pragma once

#include "wordrep/indexes.h"
#include "wordrep/word_uid.h"

namespace wordrep {

struct Sentence;
struct Phrase;

struct PhraseRepr{
    PhraseRepr(Phrase const& phrase, WordUIDindex const& wordUIDs);
    friend std::ostream& operator<<(std::ostream& os, PhraseRepr const& self);
private:
    Phrase const& phrase;
    WordUIDindex const& wordUIDs;
};
std::ostream& operator<<(std::ostream& os, PhraseRepr const& self);

struct Phrase{
    Phrase(Sentence const& sent, std::vector<DPTokenIndex> &&tokens);

    std::vector<WordUID> to_word_uids() const;
    bool isin(WordUID uid) const;
    auto size() const {return idxs.size();}
    PhraseRepr repr(WordUIDindex const& wordUIDs) const;

private:
    std::vector<DPTokenIndex> idxs;
    Sentence const& sent;
};

}//namespace wordrep
