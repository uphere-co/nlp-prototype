#include <unordered_map>

#include "wordrep/voca.h"

#include "utils/hdf5.h"

using namespace util::io;

namespace wordrep{

std::vector<WordUID::val_t> load_voca(std::string h5name, std::string voca_name){
    H5file file{H5name{h5name}, hdf5::FileMode::read_exist};
    return file.getRawData<WordUID::val_t>(H5name{voca_name});
}

}//namespace wordrep
