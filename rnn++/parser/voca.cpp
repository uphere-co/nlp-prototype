#include "parser/voca.h"
#include "parser/config.h"

#include "utils/hdf5.h"

namespace rnn{
namespace wordrep{

std::ostream& operator<<(std::ostream& os, const Word& obj) {
    os<<gsl::to_string(obj.span);
    return os;
}

Voca load_voca(std::string filename, std::string dataset){
    using namespace rnn::config;
    using namespace util::io;
    H5file file{H5name{file_name}, hdf5::FileMode::read_exist};
    return Voca{file.getRawData<rnn::type::char_t>(H5name{voca_name}), voca_max_word_len};
}

}//namespace rnn::wordrep
}//namespace rnn
