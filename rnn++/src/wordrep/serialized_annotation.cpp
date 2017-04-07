#include "wordrep/serialized_annotation.h"

#include <fmt/printf.h>

#include "utils/parallel.h"

namespace wordrep{

void SerializedAnnotation::sort(){
    std::sort(candidates.begin(),candidates.end(), [](auto const& x, auto const& y){
        return x.token_idx() < y.token_idx();
    });
    std::sort(tagged_tokens.begin(),tagged_tokens.end(), [](auto const& x, auto const& y){
        return x.token_idx() < y.token_idx();
    });
    is_sorted=true;
}
void SerializedAnnotation::to_file(Binary file) const {
    assert(is_sorted);
    namespace fb = wordrep::io;

    flatbuffers::FlatBufferBuilder builder;
    auto candidates_serialized = builder.CreateVectorOfStructs(candidates);
    auto tokens_serialized = builder.CreateVectorOfStructs(tagged_tokens);
    auto entities = fb::CreateTaggedSentences(builder, candidates_serialized, tokens_serialized);
    builder.Finish(entities);

    auto *buf = builder.GetBufferPointer();
    auto size = builder.GetSize();
    std::ofstream outfile(file.name, std::ios::binary);
    outfile.write(reinterpret_cast<const char *>(&size), sizeof(size));
    outfile.write(reinterpret_cast<const char *>(buf), size);
}

std::unique_ptr<SerializedAnnotation> load_binary_file(SerializedAnnotation::Binary const& file){
    auto data = util::io::fb::load_binary_file(file.name);
    auto block = std::make_unique<SerializedAnnotation>();
    auto rbuf = wordrep::io::GetTaggedSentences(data.get());
    block->candidates.reserve(rbuf->candidates()->size());
    block->tagged_tokens.reserve(rbuf->tagged_tokens()->size());

    for(auto v : *rbuf->candidates())
        block->candidates.push_back(*v);
    for(auto v : *rbuf->tagged_tokens())
        block->tagged_tokens.push_back(*v);
    return block;
}

AnnotationData AnnotationData::factory(AnnotatedTokenFile const& param){
    AnnotationData root;
    root.blocks.resize(param.n_block);

    tbb::parallel_for(int{0},param.n_block, [&root,&param](auto i){
        auto& block = root.blocks[i];
        block = wordrep::load_binary_file(wordrep::SerializedAnnotation::Binary{fmt::format("{}.{}",param.name, i)});
    });
    return root;
}
}//namespace wordrep;
