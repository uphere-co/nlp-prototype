#pragma once

#include "src/Vocab.h"
#include "src/Matrix.h"
#include "utils/hdf5.h"

namespace tfkld{
namespace util{

auto Concat(std::vector<std::string> const &words);

void writeVocabH5(vocab_t &vocab);
void writeDocsH5(doc_t &docs);
void writeTFH5(tfmat_t &tfmat); 

}//namespace util
}//namespace tfkld
