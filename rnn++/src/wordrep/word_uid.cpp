#include <fstream>

#include "wordrep/word_uid.h"

#include "utils/string.h"

namespace wordrep{

template<typename TUID>
UIDIndex<TUID>::UIDIndex(std::string file) : current_idx{typename TUID::val_t{0}} {
    auto words = util::string::readlines(file);
    for(auto &word : words) insert(word);
    uid2word[uid_t{typename uid_t::val_t{-1}}]="-UNKNOWN-";
}
//using WordUIDindex = UIDIndex<WordUID>;

template<typename TUID>
void UIDIndex<TUID>::write_to_disk(std::string filename){
    std::ofstream file;
    file.open(filename);
    uid_t idx{typename uid_t::val_t{0}};
    for(auto uid=idx; uid!=current_idx; ++uid){
        auto word=uid2word[uid];
        file<<word<<'\n';
    }
    file.close();
}

//Explicit instantiations;
template class UIDIndex<WordUID>;
template class UIDIndex<POSUID>;
template class UIDIndex<ArcLabelUID>;

}//namespace wordrep
