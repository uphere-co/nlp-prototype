#ifndef RNN_PARSER_VOCA
#define RNN_PARSER_VOCA

#include<ostream>
#include<string>
#include<vector>
#include<map>

#include<gsl.h>

#include"parser/basic_type.h"
#include"utils/string.h"
#include"utils/math.h"

namespace rnn{
namespace parser{
namespace wordrep{
using char_t = rnn::type::char_t;
using float_t = rnn::type::float_t;

struct Word{
    Word(gsl::cstring_span<> word) : span{word}{}
    bool operator<(const Word &word)  const { return this->span < word.span;}
    gsl::cstring_span<> span;
};
std::ostream& operator<<(std::ostream& os, const Word& obj) {
    os<<gsl::to_string(obj.span);
    return os;
}

class VocaIndexMap{
public:
    typedef size_t idx_t;
    typedef std::map<Word, idx_t> data_t;
    VocaIndexMap(data_t const &word2idxs) : val{word2idxs}{}
    auto getIndex(Word word) const {return val.find(word)->second;}
    auto getIndex(std::string sentence) const {
        auto tokens = util::string::split(sentence);
        std::vector<idx_t> idxs;
        for(auto const &word : tokens){
            idxs.push_back(this->getIndex(Word{word}));
        }
        return idxs;
    }
private:
    const data_t val;
};

class Voca{
public:
    typedef std::vector<char_t> data_t;
    Voca(data_t raw_data, int max_word_len)
    : _val{raw_data}, span{_val},
      max_word_len{max_word_len},
      voca_size{raw_data.size()/max_word_len}{}
    Word getWord(data_t::size_type idx) const {
        auto beg=std::cbegin(_val)+idx*max_word_len;
        auto end=std::find(beg, beg+max_word_len, '\0');
        gsl::cstring_span<> word = span.subspan(idx*max_word_len, end-beg);
        return Word{word};
    }
    auto size() const {return voca_size;}
    VocaIndexMap indexing() const{
        auto word_to_idx = VocaIndexMap::data_t{};
        for(auto i=data_t::size_type{0}; i<this->size(); ++i){
            word_to_idx[this->getWord(i)]=i;
        }
        return VocaIndexMap{word_to_idx};
    }
private:
    const data_t _val;
    gsl::cstring_span<> span;
    const int max_word_len;
    const data_t::size_type voca_size;
};

}//namespace rnn::wordrep
}//namespace rnn::parser
}//namespace rnn


#endif //RNN_PARSER_VOCA
