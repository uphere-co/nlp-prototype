#include <iostream>
#include <cassert>

#include "utils/print.h"
#include "utils/string.h"
#include "utils/hdf5.h"

#include "parser/config.h"
#include "parser/param.h"

using namespace util;
using namespace util::io;
using namespace util::string;
using namespace rnn::simple_model;
using namespace rnn::config;

void test_param_serialization(){
    auto param = load_param(rnn_param_store_name, rnn_param_name, DataType::sp);
    auto param_raw = param.serialize();
    // H5file h5store{H5name{"iotest.h5"}, hdf5::FileMode::create};
    H5file h5store{H5name{"iotest.h5"}, hdf5::FileMode::rw_exist};
    h5store.overwriteRawData(H5name{"param.0"}, param_raw);
    h5store.overwriteRawData(H5name{"param.1"}, param_raw);
    auto param1 = load_param("iotest.h5", "param.0", DataType::dp);
    auto param2 = load_param("iotest.h5", "param.1", DataType::dp);

    assert(param.bias.span==param1.bias.span);
    assert(param.bias.span==param2.bias.span);
    param2.bias.span[0]+=1.0;
    assert(param.bias.span!=param2.bias.span);
}


void test_variable_length_word_packing(){
    std::vector<std::string> words={"a", "bc", "edf", "g"};
    auto words_concat=pack_words(words);
    H5file h5store{H5name{"iotest.h5"}, hdf5::FileMode::rw_exist};
    h5store.overwriteRawData(H5name{"voca.0"}, words_concat);
    auto raw_data = h5store.getRawData<char>(H5name{"voca.0"});
    auto bar=unpack_words(raw_data);
    auto foo_span = span_dyn<std::string>{words};
    auto bar_span = span_dyn<std::string>{bar};
    assert(foo_span==bar_span);
    bar_span[0]="aa";
    assert(foo_span!=bar_span);

    auto views=unpack_word_views(raw_data);
    for(auto x : views) print(x);
    print(": variable length encoding\n");
    
}
void test_fixed_length_word_packing(){
    std::vector<char> words_concat={'a','\0','\0','b','c','\0','d','e','\0','f','\0','\0','g','h','\0'};
    auto views=unpack_word_views(words_concat);
    for(auto x : views) print(x);
    print(": fixed length encoding\n");
    
}

int main(){
    try {
        test_param_serialization();
        test_fixed_length_word_packing();
        test_variable_length_word_packing();
    } catch (H5::Exception &ex) {
        std::cerr << ex.getCDetailMsg() << std::endl;
    } catch (std::exception &e) {
        std::cerr<<"Got "<<e.what()<<std::endl;
    } catch (...) {
        std::cerr << "Unknown exception" << std::endl;
    }

    return 0;
}

