#include <unordered_map>

#include "wordrep/voca.h"

#include "utils/hdf5.h"
#include "utils/string.h"

using namespace util::io;

namespace wordrep{

VocaIndexMap::VocaIndexMap(std::vector<idx_t::val_t> const &uids_val) {
    auto n = uids_val.size();
    for(decltype(n)i=0; i!=n; ++i){
        idx_t uid{uids_val[i]};
        VocaIndex idx{i};
        uid2idx[uid]=idx;
        idx2uid[idx]=uid;
    }
}
VocaIndex VocaIndexMap::operator[](idx_t uid) const {
    //return uid2idx[uid];
    auto it=uid2idx.find(uid);
    if(it==uid2idx.cend()) return VocaIndex{};
    return it->second;
}
VocaIndexMap::idx_t VocaIndexMap::operator[](VocaIndex idx) const {
    //return idx2uid[idx];
    auto it=idx2uid.find(idx);
    if(it==idx2uid.cend()) return idx_t{};
    return it->second;
}


std::vector<WordUID::val_t> load_voca(std::string h5name, std::string voca_name){
    H5file file{H5name{h5name}, hdf5::FileMode::read_exist};
    return file.getRawData<WordUID::val_t>(H5name{voca_name});
}

}//namespace wordrep