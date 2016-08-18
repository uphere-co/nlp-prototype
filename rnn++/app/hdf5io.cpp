#include <iostream>
#include <cassert>

#include "utils/print.h"
#include "parser/config.h"
#include "parser/param.h"

using namespace util;
using namespace util::io;
using namespace rnn::simple_model;
using namespace rnn::config;

int main(){
    try {
        auto param = load_param(rnn_param_store_name, rnn_param_name, DataType::sp);
        auto param_raw = param.serialize();
        // H5file h5store{H5name{"iotest.h5"}, hdf5::FileMode::create};
        H5file h5store{H5name{"iotest.h5"}, hdf5::FileMode::rw_exist};
        h5store.overwriteRawData(H5name{"param.0"}, param_raw);
        h5store.overwriteRawData(H5name{"param.1"}, param_raw);
        auto param1 = load_param(H5name{"iotest.h5"}, H5name{"param.0"}, DataType::dp);
        auto param2 = load_param(H5name{"iotest.h5"}, H5name{"param.1"}, DataType::dp);

        assert(param.bias.span==param1.bias.span);
        assert(param.bias.span==param2.bias.span);
        param2.bias.span[0]+=1.0;
        assert(param.bias.span!=param2.bias.span);
    } catch (H5::Exception &ex) {
        std::cerr << ex.getCDetailMsg() << std::endl;
    } catch (std::exception &e) {
        std::cerr<<"Got "<<e.what()<<std::endl;
    } catch (...) {
        std::cerr << "Unknown exception" << std::endl;
    }

    return 0;
}

