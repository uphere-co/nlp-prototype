#include "parser/voca.h"

#include "utils/hdf5.h"

namespace rnn{
namespace wordrep{

std::ostream& operator<<(std::ostream& os, const Word& obj) {
    os<<gsl::to_string(obj.span);
    return os;
}

Voca load_voca(std::string filename, std::string dataset){
    using namespace util::io;
    H5file file{H5name{filename}, hdf5::FileMode::read_exist};
    return Voca{file.getRawData<rnn::type::char_t>(H5name{dataset})};
}

}//namespace rnn::wordrep
}//namespace rnn
