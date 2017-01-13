#include <unordered_map>

#include "wordrep/voca.h"

#include "utils/hdf5.h"
#include "utils/string.h"
#include "utils/persistent_vector.h"

using namespace util::io;

namespace wordrep{

VocaIndexMap::VocaIndexMap(std::vector<WordUID> const &uids)
: uids{uids} {
    VocaIndex idx{0};
    for(auto uid: uids) uid2idx[uid]=idx++;
    //the uids must contain a UID for unknown words
    auto unknown_word_uid = wordrep::the_unknown_word_uid();
    assert(uid2idx.find(unknown_word_uid)!=uid2idx.end());
    unknown_word = uid2idx[unknown_word_uid];
}
VocaIndex VocaIndexMap::operator[](WordUID uid) const {
    auto it=uid2idx.find(uid);
    if(it==uid2idx.cend()) return unknown_word;
    return it->second;
}
WordUID VocaIndexMap::operator[](VocaIndex idx) const {
    return uids[idx.val];
}

std::vector<WordUID> load_voca(std::string h5name, std::string voca_name){
    auto file = h5read(h5name);
    util::TypedPersistentVector<WordUID> words_uids{file,voca_name};
    std::vector<WordUID> vec = words_uids.get();
    return vec;
}

bool VocaIndexMap::is_known(VocaIndex idx) const{
    return (*this)[idx] == wordrep::the_unknown_word_uid();
}
}//namespace wordrep
