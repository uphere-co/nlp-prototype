#include "wordrep/wordvec.h"

#include "utils/hdf5.h"
#include "utils/type_param.h"

namespace wordrep{

//TODO: Update load_raw_wvec to use PersistentVector
std::vector<double> load_raw_wvec(std::string h5name, std::string wvec_name, std::string float_type){
    using namespace util::io;
    H5file file{H5name{h5name}, hdf5::FileMode::read_exist};
    std::vector<double> vocavec;
    auto param_type = util::datatype_from_string(float_type);
    if(param_type == util::DataType::sp){
        auto raw0 = file.getRawData<float>(H5name{wvec_name});
        for(auto x: raw0) vocavec.push_back(x);
    } else if(param_type == util::DataType::dp){
        auto raw0 = file.getRawData<double>(H5name{wvec_name});
        for(auto x: raw0) vocavec.push_back(x);
    }
    return vocavec;
}

}//namespace wordrep
