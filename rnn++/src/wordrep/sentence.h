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
    struct Iterator{
        Iterator(DPTokenIndex idx) : idx{idx} {}

        DPTokenIndex operator*( void ) const {return idx;}
        void operator++(void)                {++idx;}
        bool operator!=(Iterator rhs ) const {return idx != rhs.idx;}
    private:
        DPTokenIndex idx;
    };
    Sentence();
    Sentence(SentUID uid, DPTokenIndex beg, DPTokenIndex end, DepParsedTokens const *tokens);

    DPTokenIndex front() const {return beg;}
    DPTokenIndex back() const {return end-1;}
    auto size() const {return util::diff(end, beg);}
    CharOffset beg_offset() const;
    CharOffset end_offset() const;
    SentUID::val_t chrlen() const;

    //auto begin() const { return Iterator{sent.beg};}
    //auto end() const { return Iterator{sent.end};}

    bool isin(WordUID idx) const;
    SentenceRepr repr(WordUIDindex const& wordUIDs) const;

    SentUID uid;
    DPTokenIndex beg;
    DPTokenIndex end;
    DepParsedTokens const *tokens;
};

}//namespace wordrep


inline auto begin(wordrep::Sentence& sent) {
    return wordrep::Sentence::Iterator{sent.beg};
}
inline auto end(wordrep::Sentence& sent) {
    return wordrep::Sentence::Iterator{sent.end};
}
//namespace std{
//
//template<>
//wordrep::Sentence::Iterator begin<wordrep::Sentence>(wordrep::Sentence const& sent) {
//    return wordrep::Sentence::Iterator{sent.beg};
//}
//template<>
//wordrep::Sentence::Iterator end<wordrep::Sentence>(wordrep::Sentence const& sent) {
//    return wordrep::Sentence::Iterator{sent.end};
//}
//
//}//namespace std
