#include <vector>
#include <fstream>
#include <cassert>

#include <fmt/printf.h>

#include "utils/flatbuffers/i64vec_generated.h"

namespace fb = util::flatbuffer;
int main(){
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
    myFile.write((const char *)buf, size);

    std::ifstream input_file ("data.bin", std::ios::binary);
    auto data = std::make_unique<char[]>(size);
    input_file.read(data.get(), size);

    auto rbuf = fb::GetI64Vector(data.get());
    std::vector<int64_t> vec2;
    auto beg = rbuf->vals()->begin();
    auto end = rbuf->vals()->end();
    vec2.reserve(end-beg);
    assert(end-beg==vec0.size());
    std::copy(beg,end,std::back_inserter(vec2));
    auto n = vec0.size();
    for(decltype(n)i=0;i!=n;++i)
        assert(vec0[i]==vec2[i]);
    return 0;
}
