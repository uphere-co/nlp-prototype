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
    typedef gsl::span<float_t,rnn::config::word_dim> wordspan_t;
    typedef data_t::size_type idx_t;
    WordBlock(int word_dim)
    : _val{},span{_val}, word_dim{word_dim} {}
    WordBlock(data_t raw_data, int word_dim)
    : _val{raw_data},span{_val}, word_dim{word_dim} {}
    WordBlock(WordBlock&& )= default;
    WordBlock(WordBlock& )= delete;
    WordBlock& operator=(const WordBlock& )= delete;
    WordBlock copy() const {
        return WordBlock{_val, word_dim};
    }
    wordspan_t operator[](idx_t idx) const{
        return span.subspan(idx*word_dim, word_dim);
    }
    WordVec getWordVec(idx_t idx) const{
        return WordVec{(*this)[idx]};
    }
    WordBlock getWordVec(gsl::span<idx_t> idxs) const {
        auto t_start = std::chrono::high_resolution_clock::now();
        // data_t new_block(2*idxs.size()*word_dim);
        data_t new_block;
        new_block.reserve(2*idxs.size()*word_dim);
        auto t_alloc = std::chrono::high_resolution_clock::now();
        for(auto idx : idxs){
            span_t vec = (*this)[idx];
            auto t_start = std::chrono::high_resolution_clock::now();
            std::copy_n(std::cbegin(vec), word_dim, std::back_inserter(new_block));
            // std::copy_n(std::cbegin(vec), word_dim, new_block.begin());
            auto t_end = std::chrono::high_resolution_clock::now();
            std::cerr << "WordBlock::getWordVec loop #"<<idx<<" : "<< std::chrono::duration<double, std::milli>(t_end-t_start).count() << std::endl;
        }
        auto t_end = std::chrono::high_resolution_clock::now();
        std::cerr << "WordBlock::getWordVec alloc #"<<idxs.size()*word_dim<<" : "<< std::chrono::duration<double, std::milli>(t_alloc-t_start).count() << std::endl;
        std::cerr << "WordBlock::getWordVec : "<< std::chrono::duration<double, std::milli>(t_end-t_start).count() << std::endl;
        return WordBlock{new_block, word_dim};
   }
   void push_back(wordspan_t word_vec){
       std::copy_n(std::cbegin(word_vec), word_dim, std::back_inserter(_val));
       span=span_t{_val};
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
