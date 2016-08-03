#ifndef RNN_PARSER_WORDVEC
#define RNN_PARSER_WORDVEC

#include<vector>

#include<gsl.h>

#include"parser/basic_type.h"
#include"utils/string.h"
#include"utils/math.h"

namespace rnn{
namespace parser{
namespace wordrep{
using char_t = rnn::type::char_t;
using float_t = rnn::type::float_t;

class WordVec{
public:
    WordVec(gsl::span<const float_t> vec) : val{vec}{}
    gsl::span<const float_t> val;
};

class WordBlock{
public:
    typedef std::vector<float_t>     data_t;
    typedef gsl::span<const float_t> span_t;
    typedef data_t::size_type idx_t;
    WordBlock(data_t raw_data, int word_dim)
    : _val{raw_data},span{_val}, word_dim{word_dim} {}
    WordBlock(WordBlock&& )= default;
    WordBlock(WordBlock& )= delete;
    WordBlock& operator=(const WordBlock& )= delete;
    WordVec getWordVec(idx_t idx) const{
        span_t vec = span.subspan(idx*word_dim, word_dim);
        return WordVec{vec};
    }
    WordBlock getWordVec(gsl::span<idx_t> idxs) const {
        data_t new_block;
        for(auto idx : idxs){
            span_t vec = span.subspan(idx*word_dim, word_dim);
            std::copy_n(std::cbegin(vec), word_dim, std::back_inserter(new_block));
        }
        return WordBlock{new_block, word_dim};
   }
   void push_back(span_t word_vec){
       std::copy_n(std::cbegin(word_vec), word_dim, std::back_inserter(this->_val));
   }
   idx_t size() const {return _val.size()/word_dim;};

// private:
    data_t _val;
    span_t span;
    const int word_dim;
};


}//namespace rnn::wordrep
}//namespace rnn::parser
}//namespace rnn


#endif //RNN_PARSER_WORDVEC
