#pragma once

#include <functional>

#include "wordrep/indexes.h"
#include "wordrep/word_uid.h"
#include "wordrep/dep_parsed.h"

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
    size_t size() const {return uids.size();}
    friend bool operator==(wordrep::Words const &x, wordrep::Words const &y) {
        return x.uids==y.uids;
    }
    friend bool operator< (wordrep::Words const &x, wordrep::Words const &y) {
        return std::less<std::vector<wordrep::WordUID>>{}(x.uids, y.uids);
    }

    std::vector<WordUID> uids;
};

struct DepParsedTokens;
struct ConsecutiveTokens{
    struct Iterator{
        Iterator(DPTokenIndex idx) : idx{idx} {}
        DPTokenIndex operator*( void ) const {return idx;}
        void operator++(void)                {++idx;}
        bool operator!=(Iterator rhs ) const {return idx != rhs.idx;}
    private:
        DPTokenIndex idx;
    };
    Iterator begin() const { return {idx};}
    Iterator end() const { return {idx+len};}
    size_t size() const {return len;}
    Words to_words() const;

    DPTokenIndex idx;
    size_t len;
    DepParsedTokens const* tokens;
};

}//namespace wordrep
