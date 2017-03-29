#include "utils/flatbuffers/io.h"

#include <fstream>

namespace util {
namespace io {
namespace fb {

void to_file(std::vector<Pair> const& vals, PairsBinary file){
    flatbuffers::FlatBufferBuilder builder;
    auto vals_serialized = builder.CreateVectorOfStructs(vals);
    auto properties = CreatePairs(builder, vals_serialized);
    builder.Finish(properties);

    auto *buf = builder.GetBufferPointer();
    auto size = builder.GetSize();

    std::ofstream outfile(file.name, std::ios::binary);
    outfile.write(reinterpret_cast<const char *>(&size), sizeof(size));
    outfile.write(reinterpret_cast<const char *>(buf), size);
}

std::unique_ptr<char[]> load_binary_file(PairsBinary file){
    std::ifstream input_file{file.name, std::ios::binary};
    flatbuffers::uoffset_t read_size;
    input_file.read(reinterpret_cast<char*>(&read_size), sizeof(read_size));
    auto data = std::make_unique<char[]>(read_size);
    input_file.read(data.get(), read_size);
    return data;
}


}//namespace util::io::fb
}//namespace util::io
}//namesapce util
