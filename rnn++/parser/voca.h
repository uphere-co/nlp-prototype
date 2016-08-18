#pragma once
#include <ostream>
#include <string>
#include <vector>
#include <map>
#include <unordered_map>
#include <chrono>

#include "parser/basic_type.h"

#include "utils/span.h"
#include "utils/string.h"
#include "utils/math.h"

namespace rnn{
namespace wordrep{
struct Word{
    Word(util::cstring_span<> word) : span{word}, val{span.data()}{}
    Word(std::string word) : span{word}, val{word}{}
    bool operator<(const Word &word)  const { return this->span < word.span;}
    util::cstring_span<> span;
    std::string val;
};
std::ostream& operator<<(std::ostream& os, const Word& obj);

struct cmp_str {
   bool operator()(char const *a, char const *b) const {
      return std::strcmp(a, b) < 0;
   }
};

class VocaIndexMap{
public:
    typedef size_t idx_t;
    // typedef std::unordered_map<Word, idx_t> data_t;
    // typedef std::map<Word, idx_t> data_t;
    //typedef std::map<std::string, size_t> data_t;
    typedef std::unordered_map<std::string, size_t> data_t;
    // typedef std::map<char const *, size_t, cmp_str> data_t;
    // typedef std::unordered_map<char const *, size_t, cmp_str> data_t;
    VocaIndexMap(data_t const &word2idxs) : val{word2idxs}{}
    //auto getIndex(Word word) const {return val.find(word.span.data())->second;}//return val[word];}
    auto getIndex(Word word) const {return val.find(word.val)->second;}
    auto getIndex(std::string sentence) const {
        auto tokens = util::string::split(sentence);
        std::vector<idx_t> idxs;
        for(auto const &word : tokens){
            idxs.push_back(getIndex(Word{word}));
        }
        return idxs;
    }
private:
    const data_t val;
};

class Voca{
public:
    typedef std::vector<rnn::type::char_t> data_t;
    Voca(data_t raw_data, int max_word_len)
    : _val{raw_data}, span{_val},
      max_word_len{max_word_len},
      voca_size{raw_data.size()/max_word_len}{}
    Voca(data_t raw_data)
    : _val{raw_data}, span{_val},words{util::string::unpack_word_views(_val)},
      max_word_len{-1}, voca_size{words.size()} {}
    Word getWord(data_t::size_type idx) const {
        auto beg=std::cbegin(_val)+idx*max_word_len;
        auto end=std::find(beg, beg+max_word_len, '\0');
        util::cstring_span<> word = span.subspan(idx*max_word_len, end-beg);
        return Word{word};
    }
    auto size() const {return voca_size;}
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
    const std::vector<const char*> words;
    const int max_word_len;
    const data_t::size_type voca_size;
};

Voca load_voca(std::string filename, std::string dataset);

}//namespace rnn::wordrep
}//namespace rnn
