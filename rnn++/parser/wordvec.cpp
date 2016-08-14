#include "parser/wordvec.h"

#include "utils/hdf5.h"

namespace rnn{
namespace wordrep{

WordBlock load_voca_vecs(){
    using namespace rnn::config;
    using namespace util::io;
    H5file file{file_name, hdf5::FileMode::read_exist};
    auto vocavec_tmp=file.getRawData<float>(w2vmodel_name);
    std::vector<WordBlock::float_t> vocavec;
    for(auto x: vocavec_tmp) vocavec.push_back(x);
    return WordBlock{vocavec, word_dim};
}

}//namespace rnn::wordrep
}//namespace rnn
