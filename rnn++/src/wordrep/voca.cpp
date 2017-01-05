#include <unordered_map>

#include "wordrep/voca.h"

#include "utils/hdf5.h"
#include "utils/string.h"
#include "utils/persistent_vector.h"

using namespace util::io;

namespace wordrep{

VocaIndexMap::VocaIndexMap(std::vector<WordUID> const &uids) {
    auto n = uids.size();
    for(decltype(n)i=0; i!=n; ++i){
        auto uid = uids[i];
        auto idx=VocaIndex::from_unsigned(i);
        uid2idx[uid]=idx;
        idx2uid[idx]=uid;
    }
    uid2idx[WordUID{}]=VocaIndex{0};
    idx2uid[VocaIndex{0}]=WordUID{};
}
VocaIndex VocaIndexMap::operator[](WordUID uid) const {
    //return uid2idx[uid];
    auto it=uid2idx.find(uid);
    if(it==uid2idx.cend()) return VocaIndex{0};
    return it->second;
}
WordUID VocaIndexMap::operator[](VocaIndex idx) const {
    //return idx2uid[idx];
    auto it=idx2uid.find(idx);
    if(it==idx2uid.cend()) return WordUID{};
    return it->second;
}

std::vector<WordUID> load_voca(std::string h5name, std::string voca_name){
    auto file = h5read(h5name);
    util::TypedPersistentVector<WordUID> words_uids{file,voca_name};
    std::vector<WordUID> vec = words_uids.get();
    return vec;
}

}//namespace wordrep
