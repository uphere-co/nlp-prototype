#include "tests/test_context_rnn.h"

int main(){
    rnn::test::test_context_node();
    rnn::test::test_crnn_backward();
    rnn::test::test_crnn_directed_backward();
    rnn::test::test_grad_parallel_reduce();
    rnn::test::test_minibatch_crnn();

    return 0;
}