#pragma once
#include<vector>

#include<gsl.h>

#include"parser/basic_type.h"
#include"parser/config.h"

#include"utils/string.h"
#include"utils/math.h"

namespace rnn{
namespace parser{
namespace wordrep{

using char_t = rnn::type::char_t;
using float_t = rnn::type::float_t;

using WordVec=const util::math::VectorView<float_t, rnn::config::word_dim>;

class WordBlock{
public:
    typedef std::vector<float_t>     data_t;
    typedef gsl::span<float_t> span_t;
    typedef gsl::span<float_t,rnn::config::word_dim> wordvec_t;
    typedef data_t::size_type idx_t;
    WordBlock(data_t raw_data, int word_dim)
    : _val{raw_data},span{_val}, word_dim{word_dim} {}
    WordBlock(WordBlock&& )= default;
    WordBlock(WordBlock& )= delete;
    WordBlock& operator=(const WordBlock& )= delete;
    wordvec_t operator[](idx_t idx) const{
        return span.subspan(idx*word_dim, word_dim);
    }
    WordVec getWordVec(idx_t idx) const{
        return WordVec{(*this)[idx]};
    }
    WordBlock getWordVec(gsl::span<idx_t> idxs) const {
        data_t new_block;
        for(auto idx : idxs){
            span_t vec = (*this)[idx];
            std::copy_n(std::cbegin(vec), word_dim, std::back_inserter(new_block));
        }
        return WordBlock{new_block, word_dim};
   }
   void push_back(wordvec_t word_vec){
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
