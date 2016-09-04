#include "utils/hdf5.h"

#include "tests/test_dp_merging.h"

// using namespace util;
using namespace util::io;
// using namespace util::math;
// using namespace rnn::wordrep;
// using namespace rnn::config;
using namespace rnn::simple_model::test;
// using namespace rnn::simple_model;
// namespace rnn_t = rnn::type;

int main(){
    try {
        test_DPtable();
        test_dp_merging();
    } catch (H5::Exception &ex) {
        std::cerr << ex.getCDetailMsg() << std::endl;
    } catch (std::exception &e) {
        std::cerr<<"Got "<<e.what()<<std::endl;
    } catch (...) {
        std::cerr << "Unknown exception" << std::endl;
    }

    return 0;
}
