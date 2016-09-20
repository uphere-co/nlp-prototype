#pragma once

namespace rnn{
namespace test{

void test_context_node();
void test_crnn_backward();
void test_crnn_directed_backward();
void test_grad_parallel_reduce();
void test_minibatch_crnn();

void train_crnn();
void crnn_parser(char** argv);

}//namespace rnn::test
}//namespace rnn
