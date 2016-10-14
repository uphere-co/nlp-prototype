#pragma once

#include "Matrix.h"
#include "Vocab.h"

namespace tfkld{

type::real_t val_idf(type::int64_t D, type::int_t Dt);
void MakeTFIDF(Param const &params, Documents &document);
void runTFIDF(Param const &params, Documents &document);

}//namespace tfkld
