#ifndef RNN_PARSER_VOCA
#define RNN_PARSER_VOCA

#include<ostream>
#include<string>
#include<vector>
#include<map>
#include<algorithm>

#include<gsl.h>

namespace rnn{
namespace type{
using float_t = float;
using char_t = char;
}//namespace rnn::type

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
    typedef std::map<Word, size_t> data_t;
    VocaIndexMap(data_t const &word2idxs) : val{word2idxs}{}
    auto getIndex(Word word) const {return val.find(word)->second;}
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

class WordVec{
public:
    WordVec(gsl::span<const float_t> vec) : val{vec}{}
    gsl::span<const float_t> val;
};

class WordBlock{
public:
    typedef std::vector<float_t>     data_t;
    typedef gsl::span<const float_t> span_t;
    WordBlock(data_t raw_data, int voca_size, int word_dim)
    : _val{raw_data},span{_val}, voca_size{voca_size}, word_dim{word_dim} {}
    WordVec getWordVec(int idx) const{
        span_t vec = span.subspan(idx*word_dim, word_dim);
        return WordVec{vec};
   }
//private:
    data_t _val;
    span_t span;
    int voca_size;
    int word_dim;
};


}//namespace rnn::wordrep
}//namespace rnn::parser
}//namespace rnn


#endif //RNN_PARSER_VOCA