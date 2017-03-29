#include "utils/flatbuffers/io.h"

#include <fstream>

namespace util {
namespace io {
namespace fb {

void to_file(std::vector<Pair> const& vals, std::string filename){
    flatbuffers::FlatBufferBuilder builder;
    auto vals_serialized = builder.CreateVectorOfStructs(vals);
    auto properties = CreatePairs(builder, vals_serialized);
    builder.Finish(properties);

    auto *buf = builder.GetBufferPointer();
    auto size = builder.GetSize();

    std::ofstream outfile(filename, std::ios::binary);
    outfile.write(reinterpret_cast<const char *>(&size), sizeof(size));
    outfile.write(reinterpret_cast<const char *>(buf), size);
}

}//namespace util::io::fb
}//namespace util::io
}//namesapce util
