#include "utils/hdf5.h"
#include "tests/test_context_rnn.h"

using namespace util::io;

using namespace rnn;
using namespace rnn::test;


int main(int /*argc*/, char** argv){
    try {
        crnn_parser(argv);

    } catch (H5::Exception &ex) {
        std::cerr << ex.getCDetailMsg() << std::endl;
    } catch (std::exception &e) {
        std::cerr<<"Got "<<e.what()<<std::endl;
    } catch (...) {
        std::cerr << "Unknown exception" << std::endl;
    }

    return 0;
}
