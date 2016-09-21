#pragma once

#include "src/Matrix.h"
#include "src/Vocab.h"

namespace tfkld{

void MakeTFKLD(Param const &params, std::vector<type::real_t> &kld, std::vector<std::string> &tag, std::vector<SpValue> &values, vocab_t const &vocab, doc_t const &docs);

void MakeTFKLD(Param const &params, std::vector<type::real_t> &kld, std::vector<SpValue> &values);

void runTFKLD(Param const &params);
 
}//namespace tfkld
