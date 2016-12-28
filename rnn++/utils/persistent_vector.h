#pragma once
#include <vector>

#include "utils/base_types.h"
#include "utils/hdf5.h"
#include "utils/string.h"
#include "utils/type_param.h"
#include "utils/algorithm.h"

namespace util{

template<typename T, typename TRAW>
struct PersistentVector;
template<typename T, typename TRAW>
void append(PersistentVector<T,TRAW> &orig, PersistentVector<T,TRAW> const &elms);
template<typename T, typename TRAW>
std::vector<std::pair<T,T>> zip(PersistentVector<T,TRAW> &orig, PersistentVector<T,TRAW> const &elms);

template<typename T, typename TRAW>
struct PersistentVector{
    PersistentVector()
            : name{}, vals{}
    {}
    PersistentVector(std::string name)
            : name{name}, vals{}
    {}
    PersistentVector(std::vector<TRAW> const &raw, std::string name)
            : name{name}, vals{util::deserialize<T>(raw)}
    {}
    PersistentVector(io::H5file const &store, std::string name)
            : name{name},
              vals{util::deserialize<T>(store.getRawData<TRAW>(fullname()))}
    {}

    T operator[] (size_t idx) const {return vals[idx];}
    T& operator[] (size_t idx) {return vals[idx];}
    size_t size() const {return vals.size();}
    auto cbegin() const {return vals.cbegin();};
    auto cend() const {return vals.cend();};
    auto begin() const {return vals.begin();};
    auto end() const {return vals.end();};
    T& front() {return vals.front();}
    T& back()  {return vals.back();}
    T front() const {return vals.front();}
    T back()  const {return vals.back();}
    void push_back(T const& val){vals.push_back(val);}
    void push_back(T&& val){vals.push_back(val);}
    std::vector<T> get() const{ return vals;}
    std::vector<T>& get() { return vals;}
    std::string get_name() const {return name;}

    void write(io::H5file &store, std::string prefix="") const {
        store.writeRawData(fullname(prefix), util::serialize(vals));
    };
    void overwrite(io::H5file &store, std::string prefix="") const{
        store.overwriteRawData(fullname(prefix), util::serialize(vals));
    };
    friend void append<T,TRAW>(PersistentVector &orig, PersistentVector const &elms);
    friend std::vector<std::pair<T,T>> zip<T,TRAW>(PersistentVector &orig, PersistentVector const &elms);

private:
    std::string name;
    std::vector<T> vals;

    io::H5name fullname(std::string prefix="") const {
        return io::H5name{prefix+name+ "." + util::datatype_to_string(util::to_datatype<TRAW>())};
    }
};

template<typename T>
using TypedPersistentVector = PersistentVector<T, typename T::val_t>;

template<typename T, typename TRAW>
void append(PersistentVector<T,TRAW> &orig, PersistentVector<T,TRAW> const &elms) {
     util::append(orig.vals, elms.vals);
}
template<typename T, typename TRAW>
std::vector<std::pair<T,T>> zip(PersistentVector<T,TRAW> &orig, PersistentVector<T,TRAW> const &elms){
    return util::zip(orig.vals, elms.vals);
}
}//namespace util
