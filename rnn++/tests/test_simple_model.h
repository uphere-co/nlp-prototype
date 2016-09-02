#pragma once

namespace rnn{
namespace simple_model{
namespace test{

void test_init_rnn();
void test_read_voca();
void test_read_voca_config();
void test_read_word2vec_output();
void test_forward_backward();
void test_backward_wordvec();
void test_parallel_reduce();
void test_rnn_full_step();
void test_supervised_rnn_full_step();

}//namespace rnn::simple_model::test
}//namespace rnn::simple_model
}//namespace rnn
