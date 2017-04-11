#pragma once

#include "utils/json.h"

namespace rnn{
namespace test{

void test_context_node();
void test_crnn_backward();
void test_crnn_directed_backward();
void test_grad_parallel_reduce();
void test_minibatch_crnn();

void test_all();

void train_crnn(util::json_t const &config);
void crnn_parser(char** argv);

}//namespace rnn::test
}//namespace rnn
