#pragma once

#include "Matrix.h"
#include "Vocab.h"

namespace tfkld{

type::real_t val_idf(type::int64_t D, type::int_t Dt);
    
void MakeTFIDF(std::vector<type::real_t> &idf, std::vector<SpValue> &values, type::int64_t &count, vocab_t const &vocab, doc_t const &docs);

 void MakeTFIDF(std::vector<type::real_t> &idf, std::vector<SpValue> &values);

}//namespace tfkld
