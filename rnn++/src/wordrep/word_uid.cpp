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
    util::Timer timer;
    tbb::concurrent_vector<std::pair<TUID,std::string>> uids;
    std::vector<std::pair<TUID,std::string>> suids;
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
    timer.here_then_reset("Finish reading.");
    std::copy(uids.cbegin(), uids.cend(), std::back_inserter(suids));
    uids.clear();
    timer.here_then_reset("Finish copy.");
    auto words = util::string::readlines(file);
    auto n = words.size();
    tbb::parallel_for(decltype(n){0}, n, [&words,&uids,this](auto i) {
        auto const& word = words[i];
        uids.push_back({get_uid(word),word});
    });
    timer.here_then_reset("Finish reading.");
    for(auto elm : uids) uid2word[elm.first]=elm.second;
    timer.here_then_reset("Finish indexing.");
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
std::string UIDIndex<TUID>::operator[](uid_t uid) const {
//    return uid2word[uid];
    auto it=uid2word.find(uid.val);
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

}//namespace wordrep
