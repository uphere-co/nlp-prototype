#pragma once

#include "src/Matrix.h"
#include "src/Vocab.h"

namespace tfkld{

void MakeTFKLD(Param const &params, Documents &document);

void MakeTFKLD(Param const &params, std::vector<type::real_t> &kld, std::vector<SpValue> &values);

void runTFKLD(Param const &params);
 
}//namespace tfkld
