#pragma once

#include "Matrix.h"
#include "Vocab.h"

namespace tfkld{

type::real_t val_idf(type::int64_t D, type::int_t Dt);
void MakeTFIDF(Param const &params, Documents &document);
std::vector<int> findDocbyTopicThreshold(arma::mat &V, int topic, double threshold);
std::vector<int> findDocbyTopicRank(arma::mat &V, int topic, int n);
void runTFIDF(Param const &params, Documents &document);

}//namespace tfkld
