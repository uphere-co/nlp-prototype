#pragma once

#include "utils/json.h"

namespace sent2vec{
namespace test{

void word_count(util::json_t const &config);
void sampler();
void negative_sampling();
void context_words();

}//namespace sent2vec::test
}//namespace sent2vec
