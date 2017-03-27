#include <fstream>
#include <fmt/printf.h>
#include "wordrep/word_uid.h"
#include "wordrep/word_hash.h"
#include "wordrep/word_iter.h"

#include "wordrep/flatbuffers/uids_generated.h"

#include "utils/string.h"
#include "utils/parallel.h"
#include "utils/profiling.h"
namespace wordrep{

template<typename TUID>
UIDIndex<TUID>::UIDIndex(std::string file) {
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
UIDIndex<TUID>::UIDIndex(std::vector<std::pair<uid_t,std::string>> &&pairs)
: uid2word{std::move(pairs)}
{}

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
void UIDIndex<TUID>::write_to_text(std::string filename) const {
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

template<typename TUID>
UIDIndex<TUID> UIDIndex<TUID>::from_file(std::string filename){
    namespace fb = wordrep::io;
    static_assert(std::is_same<typename TUID::val_t, int64_t>::value, "");

    util::Timer timer;
    std::ifstream input_file (filename, std::ios::binary);
    flatbuffers::uoffset_t read_size;
    input_file.read(reinterpret_cast<char*>(&read_size), sizeof(read_size));
    auto data = std::make_unique<char[]>(read_size);
    input_file.read(data.get(), read_size);
    timer.here_then_reset(fmt::format("wordrep::UIDIndex<TUID>: Read file {}.", filename));

    flatbuffers::FlatBufferBuilder builder;
    std::vector<fb::UID> es;

    auto rbuf = fb::GetSortedUIDs(data.get());
    auto n = rbuf->uids()->size();
    auto& uids_buf = *rbuf->uids();
    auto& chars_buf = *rbuf->chars();
    tbb::concurrent_vector<std::pair<uid_t,std::string>> pairs(n,{-1,""});
    tbb::parallel_for(decltype(n){0}, n, [&uids_buf,&chars_buf,&pairs](auto i){
        auto it=uids_buf[i];
        std::string word;
        auto end = it->name_end();
        auto beg = it->name_beg();
        word.reserve(end-beg);
        for(auto j=beg; j!=end;++j) word.push_back(chars_buf[j]);
        pairs[i]={it->uid(), word};
    });
    timer.here_then_reset("wordrep::UIDIndex<TUID>: Construct temporal UIDs");
    std::vector<std::pair<uid_t,std::string>> uids;
    uids.reserve(pairs.size());
    for(auto&& e : pairs) uids.push_back(std::move(e));
    timer.here_then_reset("wordrep::UIDIndex<TUID>: Construct SortedUIDs");
    return {std::move(uids)};
};

template<typename TUID>
void UIDIndex<TUID>::to_file(std::string filename) const {
    namespace fb = wordrep::io;
    static_assert(std::is_same<typename TUID::val_t, int64_t>::value, "");

    flatbuffers::FlatBufferBuilder builder;
    std::vector<uint8_t> chars;
    std::vector<fb::UID> es;

    uint32_t name_beg=0;
    uint32_t name_end=0;
    for(auto& elm : uid2word){
        name_end = name_beg+elm.second.size();
        for(auto c : elm.second) chars.push_back(c);
        es.push_back({elm.first.val, name_beg, name_end});
        name_beg = name_end;
    }

    auto words_serialized = builder.CreateVector(chars);
    auto es_serialized = builder.CreateVectorOfStructs(es);
    auto entities = fb::CreateSortedUIDs(builder, es_serialized, words_serialized);
    builder.Finish(entities);

    auto *buf = builder.GetBufferPointer();
    auto size = builder.GetSize();

    std::ofstream outfile(filename, std::ios::binary);
    outfile.write(reinterpret_cast<const char *>(&size), sizeof(size));
    outfile.write(reinterpret_cast<const char *>(buf), size);
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
