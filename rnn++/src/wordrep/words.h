#pragma once

#include <functional>

#include "wordrep/indexes.h"
#include "wordrep/word_uid.h"

namespace wordrep {

struct Words;
//TODO: remove duplicated logic of *Repr.
struct WordsRepr{
    WordsRepr(Words const& words, WordUIDindex const& wordUIDs);
    friend std::ostream& operator<<(std::ostream& os, WordsRepr const& self);
private:
    Words const& words;
    WordUIDindex const& wordUIDs;
};
std::ostream& operator<<(std::ostream& os, WordsRepr const& self);

struct Words{
    Words(std::vector<WordUID> &&words)
            : uids{words}
    {}
    WordsRepr repr(WordUIDindex const& wordUIDs) const{
        return WordsRepr{*this, wordUIDs};
    }

    std::vector<WordUID> uids;
};

}//namespace wordrep

namespace std {
template<>
struct less<wordrep::Words> {
    bool operator()(wordrep::Words const &x, wordrep::Words const &y) const {
        return std::less<std::vector<wordrep::WordUID>>{}(x.uids, y.uids);
    }
};

}//namespace std
