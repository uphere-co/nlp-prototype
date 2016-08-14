#include "parser/voca.h"
#include "parser/config.h"

#include "utils/hdf5.h"

namespace rnn{
namespace wordrep{

std::ostream& operator<<(std::ostream& os, const Word& obj) {
    os<<gsl::to_string(obj.span);
    return os;
}

Voca load_voca(){
    using namespace rnn::config;
    using namespace util::io;
    H5file file{file_name, hdf5::FileMode::read_exist};
    return Voca{file.getRawData<rnn::type::char_t>(voca_name), voca_max_word_len};
}

}//namespace rnn::wordrep
}//namespace rnn
