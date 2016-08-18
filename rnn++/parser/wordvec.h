#pragma once
#include<vector>

#include "parser/basic_type.h"

#include "utils/span.h"
#include "utils/string.h"
#include "utils/linear_algebra.h"
#include "utils/type_param.h"

namespace rnn{
namespace wordrep{



class WordBlock{
public:
    using float_t    = rnn::type::float_t;
    using data_t     = std::vector<float_t>;
    using span_t     = util::span_dyn<float_t>;
    using idx_t      = data_t::size_type ;
    //TODO: fix bug! WordVec should be defined by WordBlock::word_dim.
    // using WordVec=const util::math::VectorView<rnn::type::float_t, rnn::config::word_dim>;
    WordBlock(int word_dim)
    : _val{},span{_val}, word_dim{word_dim} {}
    WordBlock(data_t raw_data, int word_dim)
    : _val{raw_data},span{_val}, word_dim{word_dim} {}
    WordBlock(WordBlock&& )= default;
    WordBlock(const WordBlock& )= delete;
    WordBlock& operator=(const WordBlock& )= delete;
    WordBlock copy() const {
        return WordBlock{_val, word_dim};
    }
    span_t operator[](idx_t idx) const{
        return span.subspan(idx*word_dim, word_dim);
    }
    // WordVec getWordVec(idx_t idx) const{
    //     return WordVec{(*this)[idx]};
    // }
    WordBlock getWordVec(util::span_dyn<idx_t> idxs) const {
        // auto t_start = std::chrono::high_resolution_clock::now();
        // data_t new_block(2*idxs.size()*word_dim);
        data_t new_block;
        new_block.reserve(2*idxs.size()*word_dim);
        // auto t_alloc = std::chrono::high_resolution_clock::now();
        for(auto idx : idxs){
            span_t vec = (*this)[idx];
            std::copy_n(std::cbegin(vec), word_dim, std::back_inserter(new_block));
            // std::copy_n(std::cbegin(vec), word_dim, new_block.begin());
        }
        // auto t_end = std::chrono::high_resolution_clock::now();
        // std::cerr << "WordBlock::getWordVec alloc #"<<idxs.size()*word_dim<<" : "<< std::chrono::duration<double, std::milli>(t_alloc-t_start).count() << std::endl;
        // std::cerr << "WordBlock::getWordVec : "<< std::chrono::duration<double, std::milli>(t_end-t_start).count() << std::endl;
        return WordBlock{new_block, word_dim};
   }
   void push_back(span_t word_vec){
       std::copy_n(std::cbegin(word_vec), word_dim, std::back_inserter(_val));
       span=span_t{_val};
   }
   idx_t size() const {return _val.size()/word_dim;};

// private:
    data_t _val;
    span_t span;
    const int word_dim;
};

WordBlock load_voca_vecs(std::string filename, std::string dataset, 
                         int word_dim, util::DataType param_type);


}//namespace rnn::wordrep
}//namespace rnn
