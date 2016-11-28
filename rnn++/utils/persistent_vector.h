#pragma once
#include <vector>

#include "utils/base_types.h"
#include "utils/hdf5.h"
#include "utils/string.h"
#include "utils/type_param.h"

namespace util{

template<typename T, typename TRAW>
struct PersistentVector{
    PersistentVector(std::vector<TRAW> const &raw, std::string name)
            : fullname{append_postfix(name)}, vals{util::deserialize<T>(raw)}
    {}
    PersistentVector(io::H5file const &store, std::string name)
            : fullname{append_postfix(name)},
              vals{util::deserialize<T>(store.getRawData<TRAW>(io::H5name{fullname}))}

    {}

    void write(io::H5file &store) const {
        store.writeRawData(io::H5name{fullname}, util::serialize(vals));
    };
    void overwrite(io::H5file &store) const{
        store.overwriteRawData(io::H5name{fullname}, util::serialize(vals));
    };

    std::string fullname;
    std::vector<T> vals;

private:
    static std::string append_postfix(std::string name){
        return name+ "." + util::datatype_to_string(util::to_datatype<TRAW>());
    }
};


}//namespace util