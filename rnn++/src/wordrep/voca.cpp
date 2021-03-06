#include <unordered_map>

#include "wordrep/voca.h"
#include "wordrep/io.h"
#include "utils/string.h"

namespace wordrep{

VocaIndexMap VocaIndexMap::factory(Binary file){
    namespace fb = util::io;
    std::vector<WordUID> vidx_wuids;
    fb::deserialize_i64vector(fb::load_binary_file(file.name), vidx_wuids);
    return {vidx_wuids};
}

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


}//namespace wordrep
