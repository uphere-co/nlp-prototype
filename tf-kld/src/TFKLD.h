#pragma once

#include "src/Matrix.h"
#include "src/Vocab.h"

namespace tfkld{

void MakeTFKLD(Param const &params, Documents &document);
void MakeTFKLD(Param const &params, std::vector<type::real_t> &kld, std::vector<SpValue> &values);
void MakeTFKLD_without_calculating_KLD(Param const &params, Documents &document);
void runTFKLD(Param const &params, Documents &doc_train, Documents &doc_test);
 
}//namespace tfkld
