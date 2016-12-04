#pragma once

#include "utils/json.h"

namespace sent2vec{
namespace test{

void word_count(std::string corenlp_outputs);
void io_unigram_dist(util::json_t const &config, std::string corenlp_outputs);
void sampler();
void negative_sampling();
void context_words();

}//namespace sent2vec::test
}//namespace sent2vec
