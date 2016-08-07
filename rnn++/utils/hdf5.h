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
namespace hdf5{

template<typename T>
H5::PredType ToH5PredType();
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

/*
Excerpted from HDF5 document:
H5F_ACC_EXCL
If file already exists, H5Fcreate fails. If file does not exist, it is created and opened with read-write access.
H5F_ACC_TRUNC
If file already exists, file is opened with read-write access and new data overwrites existing data, destroying all prior content,
i.e., file content is truncated upon opening. If file does not exist, it is created and opened with read-write access.
H5F_ACC_RDONLY
Existing file is opened with read-only access. If file does not exist, H5Fopen fails.
H5F_ACC_RDWR
Existing file is opened with read-write access. If file does not exist, H5Fopen fails.
*/
enum class FileMode {
    create,
    replace,
    read_exist,
    rw_exist,
};

auto OpenWith(FileMode mode){
    switch(mode) {
        case FileMode::create : return H5F_ACC_EXCL;
        case FileMode::replace : return H5F_ACC_TRUNC;
        case FileMode::read_exist : return H5F_ACC_RDONLY;
        case FileMode::rw_exist : return H5F_ACC_RDWR;
    }
}

}// namespace util::io::hdf5

struct H5file {
    H5file(H5name path, hdf5::FileMode mode): val{path.val, OpenWith(mode)}, name{path.val} {}
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
        auto h5PredType = hdf5::ToH5PredType<T>();
    	plist.setFillValue(h5PredType, &fillvalue);
    	val.createDataSet(dataset_name.val, h5PredType, space, plist);
    	H5dataset data{val, dataset_name};
        data.val.write(data_raw.data(), h5PredType, space);
    }
    H5::H5File val;
    H5name name;
};

}//namespace util::io
}//namespace util
