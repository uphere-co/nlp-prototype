#pragma once
#include<vector>

#include "parser/basic_type.h"

#include "utils/span.h"
#include "utils/string.h"
#include "utils/linear_algebra.h"
#include "utils/type_param.h"
#include "utils/hdf5.h"

namespace rnn{
namespace wordrep{


template<int32_t word_dim>
class WordBlock_base{
public:
    using float_t    = rnn::type::float_t;
    using data_t     = std::vector<float_t>;
    using raw_span_t = util::span_dyn<float_t>;
    using span_t     = util::span_1d<float_t,word_dim>;
    using idx_t      = data_t::size_type;
    //TODO: fix bug! WordVec should be defined by WordBlock::word_dim.
    // using WordVec=const util::math::VectorView<rnn::type::float_t, rnn::config::word_dim>;
    WordBlock_base()
    : _val{},span{_val} {}
    WordBlock_base(data_t const &raw_data)
    : _val{raw_data},span{_val} {}
    WordBlock_base(WordBlock_base&& )= default;
    WordBlock_base(const WordBlock_base& )= delete;
    WordBlock_base& operator=(const WordBlock_base& )= delete;
    WordBlock_base copy() const {
        return WordBlock_base<word_dim>{_val};
    }
    span_t operator[](idx_t idx) const{
        return span.subspan(idx*word_dim, word_dim);
    }
    // WordVec getWordVec(idx_t idx) const{
    //     return WordVec{(*this)[idx]};
    // }
    WordBlock_base getWordVec(util::span_dyn<idx_t> idxs) const {
        // auto t_start = std::chrono::high_resolution_clock::now();
        // data_t new_block(2*idxs.size()*word_dim);
        data_t new_block;
        new_block.reserve(2*idxs.size()*word_dim);
        // auto t_alloc = std::chrono::high_resolution_clock::now();
        for(auto idx : idxs){
            auto vec = (*this)[idx];
            std::copy_n(std::cbegin(vec), word_dim, std::back_inserter(new_block));
            // std::copy_n(std::cbegin(vec), word_dim, new_block.begin());
        }
        // auto t_end = std::chrono::high_resolution_clock::now();
        // std::cerr << "WordBlock::getWordVec alloc #"<<idxs.size()*word_dim<<" : "<< std::chrono::duration<double, std::milli>(t_alloc-t_start).count() << std::endl;
        // std::cerr << "WordBlock::getWordVec : "<< std::chrono::duration<double, std::milli>(t_end-t_start).count() << std::endl;
        return WordBlock_base<word_dim>{new_block};
   }
   
   void push_back(span_t word_vec){
       std::copy_n(std::cbegin(word_vec), word_dim, std::back_inserter(_val));
       span=raw_span_t{_val};
   }
   idx_t size() const {return _val.size()/word_dim;};

// private:
    data_t _val;
    raw_span_t span;
};

template<int32_t word_dim>
WordBlock_base<word_dim> load_voca_vecs(std::string filename, std::string dataset, 
                                        util::DataType param_type){
    using namespace util::io;
    H5file file{H5name{filename}, hdf5::FileMode::read_exist};
    typename WordBlock_base<word_dim>::data_t vocavec;
    if(param_type == util::DataType::sp){
        auto raw0 = file.getRawData<float>(H5name{dataset});
        for(auto x: raw0) vocavec.push_back(x);
    } else if(param_type == util::DataType::dp){
        auto raw0 = file.getRawData<double>(H5name{dataset});
        for(auto x: raw0) vocavec.push_back(x);
    }
    return WordBlock_base<word_dim>{vocavec};
}

}//namespace rnn::wordrep
}//namespace rnn
