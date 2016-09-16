#pragma once

#include "src/Matrix.h"

namespace tfkld{

void MakeTFKLD(std::vector<float_t> &kld, std::vector<std::string> &tag, std::vector<SpValue> &values, int64_t &count, vocab_t const &vocab, doc_t const &docs);

void MakeTFKLD(std::vector<float_t> &kld, std::vector<SpValue> &values);
    
}//namespace tfkld
