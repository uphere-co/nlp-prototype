#include <vector>
#include <fstream>
#include <cassert>

#include <fmt/printf.h>
#include <tbb/task_group.h>

#include "wordrep/indexed_text.h"

#include "utils/flatbuffers/i64vec_generated.h"
#include "utils/profiling.h"

namespace fb = util::flatbuffer;

template<typename T>
void write_to_binary_file(std::vector<T> const& vec, std::string filename){
    flatbuffers::FlatBufferBuilder builder;
    auto serialized_vec = builder.CreateVector(util::serialize(vec));
    auto vec1 = fb::CreateI64Vector(builder, serialized_vec);
    builder.Finish(vec1);

    auto *buf = builder.GetBufferPointer();
    auto size = builder.GetSize();

    std::ofstream myFile (filename, std::ios::binary);
    myFile.write(reinterpret_cast<const char*>(&size), sizeof(size));
    myFile.write(reinterpret_cast<const char*>(buf), size);
}
void hdf5_to_flatbuffers(){
    util::Timer timer;
    auto hdf5_file=util::io::h5read("/home/jihuni/word2vec/rss/nyt.h5.5.3");
    wordrep::IndexedTexts texts{hdf5_file, "nyt"};
    timer.here_then_reset("Load nyt.h5");

    write_to_binary_file(texts.chunks_idx.get(),"nyt.chunks_idx.i64v");
    write_to_binary_file(texts.sents_uid.get(), "nyt.sents_uid.i64v");
    write_to_binary_file(texts.words.get(),     "nyt.words.i64v");
    write_to_binary_file(texts.words_uid.get(), "nyt.words_uid.i64v");
    timer.here_then_reset("Write to the binary file.");
}

template<typename T>
void load_binary_file(std::string filename, T& vec){
    std::ifstream input_file (filename, std::ios::binary);
    flatbuffers::uoffset_t read_size;
    input_file.read(reinterpret_cast<char*>(&read_size), sizeof(read_size));
    auto data = std::make_unique<char[]>(read_size);
    input_file.read(data.get(), read_size);

    auto rbuf = fb::GetI64Vector(data.get());
    vec.reserve(rbuf->vals()->size());
    for(auto v : *rbuf->vals()) vec.push_back(v);
}

template<typename T, typename T2>
bool vector_equal(T const& lhs, T2 const& rhs){
    if(lhs.size()!=rhs.size()) return false;
    auto n = lhs.size();
    for(decltype(n)i=0;i!=n;++i)
        if(lhs[i]!=rhs[i]) return false;
    return true;
}

void hdf5_vs_flatbuffer_benchmark(){
    util::Timer timer;
    auto hdf5_file=util::io::h5read("/home/jihuni/word2vec/rss/nyt.h5.5.3");
    wordrep::IndexedTexts texts{hdf5_file, "nyt"};
    timer.here_then_reset("Load nyt.h5");

    tbb::task_group g;
    std::vector<wordrep::ChunkIndex> chunks_idx;
    std::vector<wordrep::SentUID> sents_uid;
    std::vector<wordrep::VocaIndex> words;
    std::vector<wordrep::WordUID> words_uid;
    g.run([&chunks_idx](){load_binary_file("nyt.chunks_idx.i64v", chunks_idx);});
    g.run([&sents_uid](){load_binary_file("nyt.sents_uid.i64v", sents_uid);});
    g.run([&words](){load_binary_file("nyt.words.i64v", words);});
    g.run([&words_uid](){load_binary_file("nyt.words_uid.i64v", words_uid);});
    g.wait();
    timer.here_then_reset("Complete to load data structure from .i64v files");
    assert(vector_equal(texts.chunks_idx,chunks_idx));
    assert(vector_equal(texts.sents_uid, sents_uid));
    assert(vector_equal(texts.words,     words));
    assert(vector_equal(texts.words_uid, words_uid));
    timer.here_then_reset("Test success.");
}

void binary_file_io(){
    std::vector<int64_t> vec0;
    vec0.reserve(1000000);
    for(int i=0; i<1000000; ++i) vec0.push_back(i%10);

    flatbuffers::FlatBufferBuilder builder{1000000};
    auto vec_serialized = builder.CreateVector(vec0);
    auto vec1 = fb::CreateI64Vector(builder, vec_serialized);
    builder.Finish(vec1);

    auto *buf = builder.GetBufferPointer();
    auto size = builder.GetSize();
    fmt::print("{} bytes\n", size);

    std::ofstream myFile ("data.bin", std::ios::binary);
    myFile.write(reinterpret_cast<const char*>(&size), sizeof(size));
    myFile.write(reinterpret_cast<const char*>(buf), size);


    std::ifstream input_file ("data.bin", std::ios::binary);
    flatbuffers::uoffset_t read_size;
    input_file.read(reinterpret_cast<char*>(&read_size), sizeof(read_size));
    auto data = std::make_unique<char[]>(read_size);
    input_file.read(data.get(), read_size);
    auto rbuf = fb::GetI64Vector(data.get());
    std::vector<int64_t> vec2;
    auto beg = rbuf->vals()->begin();
    auto end = rbuf->vals()->end();
    vec2.reserve(end-beg);
    std::copy(beg,end,std::back_inserter(vec2));
    assert(vector_equal(vec0, vec2));
}
int main(){
//    binary_file_io();
    //hdf5_to_flatbuffers();
    hdf5_vs_flatbuffer_benchmark();
    return 0;
}
