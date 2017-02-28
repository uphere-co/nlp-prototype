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
        friend Iterator operator+(Iterator x, size_t i) {return {x.idx+i};}
        friend auto operator-(Iterator x, Iterator y) {return util::diff(x.idx, y.idx);}
        friend bool operator<(Iterator x, Iterator y) {return x.idx < y.idx;}
        bool operator==(Iterator rhs ) const {return idx == rhs.idx;}
        bool operator!=(Iterator rhs ) const {return idx != rhs.idx;}
    private:
        DPTokenIndex idx;
    };
    struct WordIterator{
        struct Iterator{
            Iterator(DPTokenIndex idx, wordrep::DepParsedTokens const *dict)
                    : idx{idx}, dict{dict}
            {}

            WordUID operator*( void ) const;
            void operator++(void)                {++idx;}
            friend Iterator operator+(Iterator x, size_t i) {return {x.idx+i,x.dict};}
            friend auto operator-(Iterator x, Iterator y) {return util::diff(x.idx, y.idx);}
            friend bool operator<(Iterator x, Iterator y) {return x.idx < y.idx;}
            bool operator==(Iterator rhs ) const {return idx == rhs.idx;}
            bool operator!=(Iterator rhs ) const {return idx != rhs.idx;}
        private:
            DPTokenIndex idx;
            wordrep::DepParsedTokens const *dict;
        };
        WordIterator(DepParsedTokens const *dict, DPTokenIndex beg, DPTokenIndex end)
        : dict{dict}, beg_token{beg}, end_token{end}
        {}
        Iterator begin() const { return {beg_token, dict};}
        Iterator end() const   { return {end_token, dict};}
        size_t size() const {return util::diff(end_token,beg_token);}
    private:
        DepParsedTokens const *dict;
        DPTokenIndex beg_token;
        DPTokenIndex end_token;
    };

    Sentence();
    Sentence(SentUID uid, DPTokenIndex beg, DPTokenIndex end, DepParsedTokens const *tokens);

    DPTokenIndex front() const {return beg_token;}
    DPTokenIndex back() const {return end_token-1;}
    auto size() const {return util::diff(end_token, beg_token);}
    auto empty() const {return end_token==beg_token;}
    CharOffset beg_offset() const;
    CharOffset end_offset() const;
    SentUID::val_t chrlen() const;

    auto begin() const { return Iterator{beg_token};}
    auto end() const { return Iterator{end_token};}

    bool isin(WordUID idx) const;
    SentenceRepr repr(WordUIDindex const& wordUIDs) const;
    WordIterator iter_words() const{ return WordIterator{dict, beg_token, end_token};}

    SentUID uid;
    DepParsedTokens const *dict;
private:
    DPTokenIndex beg_token;
    DPTokenIndex end_token;
};

}//namespace wordrep
