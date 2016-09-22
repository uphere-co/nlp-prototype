#pragma once

#include "src/Vocab.h"
#include "utils/hdf5.h"

namespace tfkld{
namespace util{

auto Concat(std::vector<std::string> const &words);

void writeVocabH5(vocab_t &vocab);
    
}//namespace util
}//namespace tfkld
