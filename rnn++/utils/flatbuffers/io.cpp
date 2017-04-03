#include "utils/flatbuffers/io.h"

#include <fstream>

namespace {

void write_to_file(flatbuffers::FlatBufferBuilder & builder, std::string filename){
    auto *buf = builder.GetBufferPointer();
    auto size = builder.GetSize();

    std::ofstream outfile(filename, std::ios::binary);
    outfile.write(reinterpret_cast<const char *>(&size), sizeof(size));
    outfile.write(reinterpret_cast<const char *>(buf), size);
}

}//nameless namespace

namespace util {
namespace io {
namespace fb {

void to_file(std::vector<Pair> const& vals, PairsBinary file){
    flatbuffers::FlatBufferBuilder builder;
    auto vals_serialized = builder.CreateVectorOfStructs(vals);
    auto properties = CreatePairs(builder, vals_serialized);
    builder.Finish(properties);
    write_to_file(builder, file.name);
}

void to_file(std::vector<int64_t> const& vals, I64Binary file){
    flatbuffers::FlatBufferBuilder builder;
    auto vals_serialized = builder.CreateVector(vals);
    auto properties = CreateI64Vector(builder, vals_serialized);
    builder.Finish(properties);
    write_to_file(builder, file.name);
}

void to_file(std::vector<float> const& vals, F32Binary file){
    flatbuffers::FlatBufferBuilder builder;
    auto vals_serialized = builder.CreateVector(vals);
    auto properties = CreateF32Vector(builder, vals_serialized);
    builder.Finish(properties);
    write_to_file(builder, file.name);
}

std::unique_ptr<char[]> load_binary_file(std::string filename){
    std::ifstream input_file{filename, std::ios::binary};
    flatbuffers::uoffset_t read_size;
    input_file.read(reinterpret_cast<char*>(&read_size), sizeof(read_size));
    auto data = std::make_unique<char[]>(read_size);
    input_file.read(data.get(), read_size);
    return data;
}


}//namespace util::io::fb
}//namespace util::io
}//namesapce util
