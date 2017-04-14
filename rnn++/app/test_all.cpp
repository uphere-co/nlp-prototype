#include "tests/test_context_rnn.h"
#include "tests/query_processing.h"

int main(int argc, char** argv){
    rnn::test::test_all();
    engine::test::load_query_engine_data(argc,argv);
    return 0;
}

