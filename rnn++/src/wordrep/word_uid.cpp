#include <fstream>
#include <fmt/printf.h>
#include "wordrep/word_uid.h"
#include "wordrep/word_hash.h"
#include "wordrep/word_iter.h"

#include "utils/string.h"
#include "utils/parallel.h"
#include "utils/profiling.h"
namespace wordrep{

template<typename TUID>
UIDIndex<TUID>::UIDIndex(std::string file) : current_idx{typename TUID::val_t{0}} {
    tbb::concurrent_vector<std::pair<TUID,std::string>> uids;
    std::ifstream is{file};
    tbb::task_group g;
    while (auto buffer=util::string::read_chunk(is, 2000000)) {
        std::string str{buffer.value().data()};
        g.run([this,&uids,str{std::move(str)}]() { //important to copy the str variable.
            WordIterBase<std::string> text{std::move(str)};
            text.iter([this,&uids](auto const& word){uids.push_back({get_uid(word),word});});
        });
    }
    g.wait();
    tbb::parallel_sort(uids.begin(), uids.end());
    std::copy(uids.cbegin(), uids.cend(), std::back_inserter(uid2word));
//    auto words = util::string::readlines(file);
//    for(auto& word : words) uid2word[get_uid(word)] = word;
}

template<typename TUID>
bool UIDIndex<TUID>::isin(uid_t uid) const {
    auto eq   = [uid](auto x){return uid==x.first;};
    auto less = [uid](auto x){return uid<x.first;};
    auto mit = util::binary_find(uid2word, eq, less);
    if(!mit) return false;
    return true;
}

template<typename TUID>
typename UIDIndex<TUID>::uid_t UIDIndex<TUID>::get_uid(std::string const &word) {
    return uid_t::from_unsigned(hash(word));
}

template<typename TUID>
typename UIDIndex<TUID>::uid_t UIDIndex<TUID>::uid(std::string const &word) const {
    return get_uid(word);
}
template<typename TUID>
std::string UIDIndex<TUID>::str(uid_t uid) const {
    auto eq   = [uid](auto x){return uid==x.first;};
    auto less = [uid](auto x){return uid<x.first;};
    auto mit = util::binary_find(uid2word, eq, less);
    if(!mit) return the_unknown_word();
    auto it = mit.value();
    return it->second;
}

template<typename TUID>
typename UIDIndex<TUID>::uid_t UIDIndex<TUID>::operator[] (std::string const &word) const {
    return this->uid(word);
}
template<typename TUID>
std::string UIDIndex<TUID>::operator[](uid_t uid) const {
    return this->str(uid);
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

template<typename TUID>
std::vector<typename UIDIndex<TUID>::uid_t> UIDIndex<TUID>::get_uids() const{
    return util::map(uid2word, [](auto elm){return elm.first;});
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
template class UIDIndex<WikidataUID>;

}//namespace wordrep
