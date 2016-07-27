#ifndef RNN_IO_HDF5
#define RNN_IO_HDF5

#include<vector>

#include<H5Cpp.h>

namespace util{
namespace io{

struct H5name{
    H5name(std::string filename): val{filename} {}
    const H5std_string val;
};

struct H5dataset{
    H5dataset(H5::H5File const & file, H5name dataset)
    : val{file.openDataSet(dataset.val)}, name{dataset.val} {}
    ~H5dataset(){
        try {
            val.close();
            std::cerr << "Close dataset " << name.val << "\n";
        } catch (H5::Exception ex) {
            std::cerr << ex.getCDetailMsg() << std::endl;
        }
    }
    H5::DataSet val;
    H5name name;
};

struct H5file {
    H5file(H5name path): val{path.val, H5F_ACC_RDONLY}, name{path.val} {}
    ~H5file(){
        try {
            val.close();
            std::cerr << "Close H5File " << name.val << "\n";
        } catch (H5::Exception ex) {
            std::cerr << ex.getCDetailMsg() << std::endl;
        }
    };
    template<typename T_RAW_ELM>
    std::vector<T_RAW_ELM> getRawData(H5name dataset){
        H5dataset data{val, dataset};
        //TODO: should use getStorageSize instead of getInMemDataSize?
        std::vector<T_RAW_ELM> data_raw(data.val.getInMemDataSize());
        std::cerr << "Read "<< dataset.val << ". Memory size:  " << data_raw.size() << std::endl;
        data.val.read(data_raw.data(), data.val.getDataType());
        return data_raw;
    }
    H5::H5File val;
    H5name name;
};

}//namespace util::io
}//namespace util

#endif //RNN_IO_HDF5
