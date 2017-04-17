#include "wordrep/similar_words.h"

#include "wordrep/io.h"

#include "utils/parallel_algorithm.h"

namespace wordrep {

SimilarWords SimilarWords::factory(SimilarWordsFile const& file){
    auto data = util::io::load_binary_file(file.name);
    auto rbuf = wordrep::io::GetSimilarWords(data.get());
    tbb::concurrent_vector<value_type> similar_words;
    similar_words.reserve(rbuf->pairs()->size());
    for(auto v : *rbuf->pairs())
        similar_words.push_back(*v);
    return {std::move(similar_words)};
}

SimilarWords::SimilarWords(tbb::concurrent_vector<value_type>&& may_not_sorted)
: similar_words{std::move(may_not_sorted)} {
    tbb::parallel_sort(similar_words.begin(),
                       similar_words.end(),
                       [](auto &x, auto &y){return to_key(x) < to_key(y);});
}

void SimilarWords::to_file(SimilarWordsFile&& file) const {
    auto tokens = util::to_vector(similar_words);
    flatbuffers::FlatBufferBuilder builder;
    auto tokens_serialized = builder.CreateVectorOfStructs(tokens);
    auto entities = wordrep::io::CreateSimilarWords(builder, tokens_serialized);
    builder.Finish(entities);
    util::io::to_file(builder, file.name);
}

SimilarWords::Range SimilarWords::find(key_type word) const {
    auto eq   = [word](auto& x){return word==to_key(x);};
    auto less = [word](auto& x){return word< to_key(x);};
    auto m_pair = util::binary_find_block(similar_words, eq, less);
    if(!m_pair) return {0,0};
    auto beg = m_pair->first  - similar_words.cbegin();
    auto end = m_pair->second - similar_words.cbegin();
    return {beg,end};
}
}//namespace wordrep
