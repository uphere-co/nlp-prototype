#pragma once

#include "utils/span.h"

#include "wordrep/voca.h"

namespace wordrep{

template<typename T, int32_t word_dim>
class WordBlock_base{
public:
    static constexpr int32_t dim = word_dim;
    using val_t      = T;
    using data_t     = std::vector<val_t>;
    using raw_span_t = util::span_dyn<val_t>;
    using span_t     = util::span_1d<val_t,dim>;
    using idx_t      = VocaIndex;
//    WordBlock_base()
//    : _val{},span{_val} {}
//    WordBlock_base(idx_t voca_size)
//    : _val(voca_size*word_dim),span{_val} {}
    template<typename TV>
    WordBlock_base(std::vector<TV> const &raw_data)
    : _val{},span{} {
        for(auto x : raw_data) _val.push_back(static_cast<val_t>(x));
        span = _val;
    }
    WordBlock_base(std::vector<val_t> &&raw_data)
            : _val{std::move(raw_data)},span{_val}
    {}

    WordBlock_base(WordBlock_base&& )= default;
    WordBlock_base(const WordBlock_base& )= delete;
    WordBlock_base& operator=(const WordBlock_base& )= delete;
    WordBlock_base copy() const {
        return WordBlock_base{_val};
    }
    span_t operator[](idx_t idx) const{
        return span.subspan(idx.val*word_dim, word_dim);
    }
    WordBlock_base getWordVec(util::span_dyn<idx_t> idxs) const {
        data_t new_block;
        new_block.reserve(2*idxs.size()*word_dim); //2x size for phrases that will be generated from words.
        for(auto idx : idxs){
            auto vec = (*this)[idx];
            std::copy_n(std::cbegin(vec), word_dim, std::back_inserter(new_block));
        }
        return WordBlock_base{new_block};
    }
    auto size() const {return _val.size()/word_dim;};
    data_t serialize() const { return _val;}

private:
    data_t _val;
    raw_span_t span;
};

std::vector<double> load_raw_wvec(std::string h5name, std::string wvec_name, std::string float_type);
}//namespace wordrep
