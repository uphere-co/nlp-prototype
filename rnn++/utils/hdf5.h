#pragma once
#include<vector>
#include<iostream>

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
namespace {
  
template<typename T>
struct ToH5PredType;
template<>
struct ToH5PredType<int>{
    const H5::PredType val = H5::PredType::NATIVE_INT;
};
template<>
struct ToH5PredType<int64_t>{
    const H5::PredType val = H5::PredType::NATIVE_INT64;
};
template<>
struct ToH5PredType<uint64_t>{
    const H5::PredType val = H5::PredType::NATIVE_UINT64;
};
template<>
struct ToH5PredType<float>{
    const H5::PredType val = H5::PredType::NATIVE_FLOAT;
};
template<>
struct ToH5PredType<char>{
    const H5::PredType val = H5::PredType::NATIVE_CHAR;
};
}

 
struct H5file {
H5file(H5name path): val{path.val, H5F_ACC_TRUNC}, name{path.val} {}
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
        std::vector<T_RAW_ELM> data_raw(data.val.getInMemDataSize()/sizeof(T_RAW_ELM));
        std::cerr << "Read "<< dataset.val << ". # of elements:  " << data_raw.size() << std::endl;
        data.val.read(data_raw.data(), data.val.getDataType());
        return data_raw;
    }
    template<typename T>
    void writeRawData(H5name dataset_name, std::vector<T> const &data_raw){
        T fillvalue{0};
        hsize_t fdim[] = {data_raw.size()};
        H5::DataSpace space(1, fdim);
        H5::DSetCreatPropList plist;
        //plist.setFillValue(H5::PredType::NATIVE_INT, &fillvalue);
        plist.setFillValue(ToH5PredType<T>{}.val, &fillvalue);
        val.createDataSet(dataset_name.val, ToH5PredType<T>{}.val, space, plist);
        H5dataset data{val, dataset_name};
        data.val.write(data_raw.data(), ToH5PredType<T>{}.val, space);
    }
    H5::H5File val;
    H5name name;
};
 
}//namespace util::io
}//namespace util
