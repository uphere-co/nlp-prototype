#pragma once
#include <ostream>
#include <string>
#include <vector>
#include <map>
#include <unordered_map>
#include <chrono>
#include <limits>

#include "parser/basic_type.h"

#include "utils/print.h"
#include "utils/span.h"
#include "utils/string.h"
#include "utils/math.h"

namespace rnn{
namespace wordrep{
struct Word{
    typedef size_t idx_t;
    // Word(util::cstring_span<> word) : val{word.data()}, span{val} {}
    Word(std::string word) : val{word}, span{val} {}
    bool operator<(const Word &word)  const { return this->span < word.span;}
    std::string val;
    util::cstring_span<> span;
};
struct IndexedWord{
    typedef size_t idx_t;
    IndexedWord(std::string word, idx_t idx) : val{word}, span{val}, idx{idx} {}
    IndexedWord(util::cstring_span<> word, idx_t idx) : val{word.data()}, span{val},  idx{idx} {}
    static auto blank_word() {return IndexedWord{std::string{}, std::numeric_limits<idx_t>::max()};}
    bool operator<(const IndexedWord &word)  const { return this->span < word.span;}
    std::string val;
    util::cstring_span<> span;
    idx_t idx;
};
std::ostream& operator<<(std::ostream& os, const Word& obj);

struct cmp_str {
   bool operator()(char const *a, char const *b) const {
      return std::strcmp(a, b) < 0;
   }
};

class [[deprecated("Replaced by src/wordrep/voca.h")]] VocaIndexMap{
public:
    typedef size_t idx_t;
    // typedef std::unordered_map<Word, idx_t> data_t;
    // typedef std::map<Word, idx_t> data_t;
    //typedef std::map<std::string, size_t> data_t;
    typedef std::unordered_map<std::string, idx_t> data_t;
    // typedef std::map<char const *, size_t, cmp_str> data_t;
    // typedef std::unordered_map<char const *, size_t, cmp_str> data_t;
    VocaIndexMap(data_t const &word2idxs) : val{word2idxs}{}
    //auto getIndex(Word word) const {return val.find(word.span.data())->second;}//return val[word];}
    auto getIndex(Word word) const {
        auto it = val.find(word.val);
        //TODO: remove ad-hoc rule for unknown words.
        if(it==val.end()) return idx_t{0};
        return it->second;
    }
    auto getIndex(std::string sentence) const {
        auto tokens = util::string::split(sentence);
        std::vector<idx_t> idxs;
        for(auto const &word : tokens){
            // if(word==std::string{"-LRB-"}) {
            //     idxs.push_back(getIndex(Word{std::string{"("}}));
            // }
            // else if(word==std::string{"-RRB-"}) idxs.push_back(getIndex(Word{std::string{")"}}));
            // else idxs.push_back(getIndex(Word{word}));
            idxs.push_back(getIndex(Word{word}));
        }
        return idxs;
    }
private:
    const data_t val;
};

class [[deprecated("Replaced by src/wordrep/voca.h")]] Voca{
    using idx_t = VocaIndexMap::idx_t;
public:
    typedef std::vector<rnn::type::char_t> data_t;
    Voca(data_t raw_data)
    : _val{raw_data}, span{_val},word_views{util::string::unpack_word_views(_val)}
    {}
    IndexedWord getWord(data_t::size_type idx) const {
        util::cstring_span<> word = gsl::ensure_z(word_views[idx]);
        return IndexedWord{word,idx};
    }
    util::cstring_span<> getWordSpan(data_t::size_type idx) const {
        return gsl::ensure_z(word_views[idx]);
    }
    std::string operator[](idx_t i) const {return word_views[i];}
    auto size() const {return word_views.size();}
    VocaIndexMap indexing() const{
        auto word_to_idx = VocaIndexMap::data_t{};
        for(auto i=data_t::size_type{0}; i<size(); ++i){
            word_to_idx[getWord(i).val]=i;
        }
        return VocaIndexMap{word_to_idx};
    }
private:
    const data_t _val;
    util::cstring_span<> span;
    std::vector<const char*> word_views;
};

Voca load_voca(std::string filename, std::string dataset);
void print_words(Voca const &voca);

}//namespace rnn::wordrep
}//namespace rnn
