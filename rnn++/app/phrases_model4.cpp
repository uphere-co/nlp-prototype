#include "parser/parser.h"
#include "utils/binary_tree.h"
#include "utils/parallel.h"

#include "tests/test_voca_wordvec.h"

using namespace rnn::config;
using namespace rnn::simple_model;
using namespace rnn::simple_model::detail;

int main(int /*argc*/, char** argv){
    using namespace test;
    test_collecting_new_voca();
    return 0;
}
