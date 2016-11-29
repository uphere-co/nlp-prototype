#include <string>
#include <fstream>

#include <H5Cpp.h>

#include "hdf5.h"

namespace{
using util::io::hdf5::FileMode;

unsigned int OpenWith(FileMode mode){
    switch(mode) {
        // the enum is unsigned int 
        // https://www.hdfgroup.org/HDF5/doc/cpplus_RM/class_h5_1_1_h5_file.html
        case FileMode::create : return H5F_ACC_EXCL; 
        case FileMode::replace : return H5F_ACC_TRUNC;
        case FileMode::read_exist : return H5F_ACC_RDONLY;
        case FileMode::rw_exist : return H5F_ACC_RDWR;
    }
}

bool is_exist(std::string filename){
    std::ifstream f{filename};
    return f.good();
}
}//nameless namespace


namespace util{
namespace io{


// class H5dataset::impl{
// public:
//     impl(H5::H5File const & file, H5name dataset)
//     : val{file.openDataSet(dataset.val)}, name{dataset.val} {}
//     ~impl() {
//         try {
//             val.close();
//             std::cerr << "Close dataset " << name.val << "\n";
//         } catch (H5::Exception ex) {
//             std::cerr << ex.getCDetailMsg() << std::endl;
//         }
//     }
//     H5::DataSet val;
//     H5name name;
// };

// H5dataset::H5dataset(H5::H5File const & file, H5name dataset)
//     : pimpl{std::make_unique<impl>(file, dataset)} {}


H5file::H5file(H5name path, hdf5::FileMode mode)
: val{path.val, OpenWith(mode)}, name{path.val} 
{}

H5file::~H5file(){
    try {
        val.close();
        std::cerr << "Close H5File " << name.val << "\n";
    } catch (H5::Exception ex) {
        std::cerr << ex.getCDetailMsg() << std::endl;
    }
};


H5file read(std::string path){
    return H5file{H5name{path}, hdf5::FileMode::read_exist};
}


}//namespace util::io
}//namespace util

namespace util{
namespace io{
namespace hdf5{

template<>
H5::PredType ToH5PredType<int>()     {return H5::PredType::NATIVE_INT;}
template<>
H5::PredType ToH5PredType<int64_t>() {return H5::PredType::NATIVE_INT64;}
template<>
H5::PredType ToH5PredType<uint64_t>(){return H5::PredType::NATIVE_UINT64;}
template<>
H5::PredType ToH5PredType<float>()   {return H5::PredType::NATIVE_FLOAT;}
template<>
H5::PredType ToH5PredType<double>()   {return H5::PredType::NATIVE_DOUBLE;}
template<>
H5::PredType ToH5PredType<char>()    {return H5::PredType::NATIVE_CHAR;}


}// namespace util::io::hdf5
}//namespace util::io
}//namespace util
