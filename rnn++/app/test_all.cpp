#include "tests/test_context_rnn.h"
#include "tests/query_processing.h"

int main(int argc, char** argv){
//    rnn::test::test_all();
    engine::test::query_sent_processing(argc,argv);
    return 0;
}

