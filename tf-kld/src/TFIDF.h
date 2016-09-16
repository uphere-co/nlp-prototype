#pragma once

#include "Matrix.h"
#include "Vocab.h"

namespace tfkld{

float_t val_idf(tfkld::type::int64_t D, tfkld::type::int_t Dt);
    
void MakeTFIDF(std::vector<tfkld::type::float_t> &idf, std::vector<tfkld::SpValue> &values, tfkld::type::int64_t &count, vocab_t const &vocab, doc_t const &docs);

 void MakeTFIDF(std::vector<tfkld::type::float_t> &idf, std::vector<tfkld::SpValue> &values);

}//namespace tfkld
