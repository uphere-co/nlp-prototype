#include <fstream>

#include "wordrep/word_uid.h"
#include "wordrep/word_hash.h"

#include "utils/string.h"
namespace wordrep{

template<typename TUID>
UIDIndex<TUID>::UIDIndex(std::string file) : current_idx{typename TUID::val_t{0}} {
    auto words = util::string::readlines(file);
    for(auto &word : words) insert(word);
}

template<typename TUID>
typename UIDIndex<TUID>::uid_t UIDIndex<TUID>::get_uid(std::string const &word) const {
    return uid_t::from_unsigned(hash(word));
}

//using WordUIDindex = UIDIndex<WordUID>;
template<typename TUID>
typename UIDIndex<TUID>::uid_t UIDIndex<TUID>::operator[] (std::string const &word) const {
    return get_uid(word);
}
template<typename TUID>
typename UIDIndex<TUID>::uid_t UIDIndex<TUID>::insert(std::string const &word) {
    auto uid = get_uid(word);
    uid2word[uid]=word;
    return uid;
}
template<typename TUID>
std::string UIDIndex<TUID>::operator[](uid_t uid) const {
//    return uid2word[uid];
    auto it=uid2word.find(uid);
    if(it==uid2word.cend()) return the_unknown_word();
    return it->second;
}
template<typename TUID>
void UIDIndex<TUID>::write_to_disk(std::string filename) const {
    std::ofstream file;
    file.open(filename);
    uid_t idx{0};
    for(auto pair : uid2word){
        auto word= pair.second;
        file<<word<<'\n';
    }
    file.close();
}

std::string the_unknown_word(){
    return "-UNKNOWN-";
}
WordUID the_unknown_word_uid(){
    return WordUID::from_unsigned(hash(the_unknown_word()));
}
//Explicit instantiations;
template class UIDIndex<WordUID>;
template class UIDIndex<POSUID>;
template class UIDIndex<ArcLabelUID>;

}//namespace wordrep
