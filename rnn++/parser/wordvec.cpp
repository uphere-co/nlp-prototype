#include "parser/wordvec.h"

#include "utils/hdf5.h"

namespace rnn{
namespace wordrep{

WordBlock load_voca_vecs(std::string filename, std::string dataset, util::DataType param_type){
    using namespace rnn::config;
    using namespace util::io;
    H5file file{H5name{filename}, hdf5::FileMode::read_exist};
    std::vector<WordBlock::float_t> vocavec;
    if(param_type == util::DataType::sp){
        auto raw0 = file.getRawData<float>(H5name{dataset});
        for(auto x: raw0) vocavec.push_back(x);
    } else if(param_type == util::DataType::dp){
        auto raw0 = file.getRawData<double>(H5name{dataset});
        for(auto x: raw0) vocavec.push_back(x);
    }
    return WordBlock{vocavec, word_dim};
}

}//namespace rnn::wordrep
}//namespace rnn
