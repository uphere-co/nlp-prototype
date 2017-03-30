#include "wordrep/wikientity_base.h"
#include "wordrep/io.h"

#include <sstream>

#include <fmt/printf.h>

#include "wordrep/word_iter.h"

#include "utils/string.h"
#include "utils/profiling.h"

namespace wordrep{
namespace wiki {

Entity::Entity(WordUIDindex const &wordUIDs, std::string line)
        : words{{}} {
    auto tokens = util::string::split(line, "\t");
    if (tokens.size() != 2) {
        fmt::print("{}\n", line);
        assert(0);
    }
    uid = WikidataUIDindex::get_uid(tokens[0]);
    WordIterBase<std::string> word_iter{tokens[1]};
    word_iter.iter([this, &wordUIDs](auto w) { words.uids.push_back(wordUIDs[w]); });
}

std::string Entity::repr(WikidataUIDindex const &wikidataUIDs,
                         WordUIDindex const &wordUIDs) const {
    std::stringstream ss;
    ss << wikidataUIDs[uid] << " " << words.repr(wordUIDs);
    return ss.str();
}

std::ostream &operator<<(std::ostream &os, Entity const &a) {
    fmt::print(os, "{}\t", a.uid);
    for (auto word: a.words.uids) fmt::print(os, " {}", word);
    return os;
}

UIDSortedEntities::UIDSortedEntities(Binary file){
    util::Timer timer;
    std::ifstream input_file (file.filename, std::ios::binary);
    namespace fb = wordrep::wiki::io;
    flatbuffers::uoffset_t read_size;
    input_file.read(reinterpret_cast<char*>(&read_size), sizeof(read_size));
    auto data = std::make_unique<char[]>(read_size);
    input_file.read(data.get(), read_size);
    timer.here_then_reset(fmt::format("wiki::UIDSortedEntities::UIDSortedEntities: Read file. {}", file.filename));

    auto rbuf = fb::GetSortedEntities(data.get());
    auto n = rbuf->entities()->size();
    entities.resize(n,{-1,{}});
    timer.here_then_reset("wiki::UIDSortedEntities::UIDSortedEntities: Prepare construction.");

    auto& entities_buf = *rbuf->entities();
    auto& names_buf = *rbuf->names();
    tbb::parallel_for(decltype(n){0}, n, [&names_buf,&entities_buf,this](auto i){
        auto it=entities_buf[i];
        std::vector<wordrep::WordUID> words;
        for(auto j=it->name_beg(); j!=it->name_end();++j) words.push_back(names_buf[j]);
        this->entities[i]={it->uid(), std::move(words)};
    });
    timer.here_then_reset("wiki::UIDSortedEntities::UIDSortedEntities: Construct temporal entities");
}


void UIDSortedEntities::to_file(Binary file) const{
    flatbuffers::FlatBufferBuilder builder;
    namespace fb = wordrep::wiki::io;
    std::vector<fb::Entity> es;
    std::vector<int64_t> names;
    names.reserve(entities.size()*5);
    uint32_t name_beg=0;
    uint32_t name_end=0;
    for(auto& e : entities){
        name_end = name_beg+e.words.size();
        es.push_back({e.uid.val, name_beg,name_end});
        for(auto w : e.words) names.push_back(w.val);
        name_beg = name_end;
    }

    auto names_serialized = builder.CreateVector(names);
    auto es_serialized = builder.CreateVectorOfStructs(es);
    auto entities = fb::CreateSortedEntities(builder, es_serialized, names_serialized);
    builder.Finish(entities);

    auto *buf = builder.GetBufferPointer();
    auto size = builder.GetSize();

    std::ofstream outfile(file.filename, std::ios::binary);
    outfile.write(reinterpret_cast<const char *>(&size), sizeof(size));
    outfile.write(reinterpret_cast<const char *>(buf), size);
}

void SortedEntities::to_file(Binary file) const{
    flatbuffers::FlatBufferBuilder builder;
    namespace fb = wordrep::wiki::io;
    std::vector<fb::Entity> es;
    std::vector<int64_t> names;
    names.reserve(entities.size()*5);
    uint32_t name_beg=0;
    uint32_t name_end=0;
    for(auto& e : entities){
        name_end = name_beg+e.words.size();
        es.push_back({e.uid.val, name_beg,name_end});
        for(auto w : e.words) names.push_back(w.val);
        name_beg = name_end;
    }

    auto names_serialized = builder.CreateVector(names);
    auto es_serialized = builder.CreateVectorOfStructs(es);
    auto entities = fb::CreateSortedEntities(builder, es_serialized, names_serialized);
    builder.Finish(entities);

    auto *buf = builder.GetBufferPointer();
    auto size = builder.GetSize();

    std::ofstream outfile(file.filename, std::ios::binary);
    outfile.write(reinterpret_cast<const char *>(&size), sizeof(size));
    outfile.write(reinterpret_cast<const char *>(buf), size);
}

SortedEntities::SortedEntities(Binary file){
    util::Timer timer;
    std::ifstream input_file (file.filename, std::ios::binary);
    namespace fb = wordrep::wiki::io;
    flatbuffers::uoffset_t read_size;
    input_file.read(reinterpret_cast<char*>(&read_size), sizeof(read_size));
    auto data = std::make_unique<char[]>(read_size);
    input_file.read(data.get(), read_size);
    timer.here_then_reset(fmt::format("wiki::SortedEntities::SortedEntities: Read file. {}", file.filename));

    auto rbuf = fb::GetSortedEntities(data.get());
    auto n = rbuf->entities()->size();
    entities.resize(n,{-1,{}});
    timer.here_then_reset("wiki::SortedEntities::SortedEntities: Prepare construction.");

    auto& entities_buf = *rbuf->entities();
    auto& names_buf = *rbuf->names();
    tbb::parallel_for(decltype(n){0}, n, [&names_buf,&entities_buf,this](auto i){
        auto it=entities_buf[i];
        std::vector<wordrep::WordUID> words;
        for(auto j=it->name_beg(); j!=it->name_end();++j) words.push_back(names_buf[j]);
        this->entities[i]={it->uid(), std::move(words)};
    });
    timer.here_then_reset("wiki::SortedEntities::SortedEntities: Construct temporal entities");
}


}//namespace wordrep::wiki
}//namespace wordrep
