#include "wordrep/similar_words.h"

#include "wordrep/io.h"

#include "utils/parallel_algorithm.h"

namespace wordrep {

SimilarWords SimilarWords::factory(SimilarWordsFile const& file){
    auto data = util::io::fb::load_binary_file(file.name);
    auto rbuf = wordrep::io::GetSimilarWords(data.get());
    tbb::concurrent_vector<wordrep::io::SimilarWordPair> similar_words;
    similar_words.reserve(rbuf->pairs()->size());
    for(auto v : *rbuf->pairs())
        similar_words.push_back(*v);
    return {std::move(similar_words)};
}

SimilarWords::SimilarWords(tbb::concurrent_vector<io::SimilarWordPair>&& may_not_sorted)
: similar_words{std::move(may_not_sorted)} {
    tbb::parallel_sort(similar_words.begin(), similar_words.end());
}

void SimilarWords::to_file(SimilarWordsFile&& file) const {
    auto tokens = util::to_vector(similar_words);
    flatbuffers::FlatBufferBuilder builder;
    auto tokens_serialized = builder.CreateVectorOfStructs(tokens);
    auto entities = wordrep::io::CreateSimilarWords(builder, tokens_serialized);
    builder.Finish(entities);
    util::io::fb::to_file(builder, file.name);
}

}//namespace wordrep
