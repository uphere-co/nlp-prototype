#include <iostream>
#include <sstream>
#include <cassert>
#include <cmath>


#include "utils/hdf5.h"
#include "utils/math.h"
#include "utils/linear_algebra.h"
#include "utils/print.h"
#include "utils/profiling.h"
#include "utils/parallel.h"
#include "utils/logger.h"

#include "parser/optimizers.h"
#include "parser/parser.h"

#include "tests/test_context_rnn.h"

using namespace util;
using namespace util::io;
using namespace util::math;

using namespace rnn;
using namespace rnn::test;


int main(int /*argc*/, char** argv){
    try {
//        test_context_node();
//        test_crnn_backward();
//        test_crnn_directed_backward();
//        test_grad_parallel_reduce();
//        test_minibatch_crnn();
//        return 0;
        auto config = util::load_json(argv[1]);
        train_crnn(config);
    } catch (H5::Exception &ex) {
        std::cerr << ex.getCDetailMsg() << std::endl;
    } catch (std::exception &e) {
        std::cerr<<"Got "<<e.what()<<std::endl;
    } catch (...) {
        std::cerr << "Unknown exception" << std::endl;
    }

    return 0;
}
