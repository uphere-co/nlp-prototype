#pragma once

#include <fstream>

#include "wordrep/flatbuffers/sorted_entity_generated.h"
#include "wordrep/flatbuffers/uids_generated.h"
#include "wordrep/flatbuffers/tagged_sentences_generated.h"
#include "wordrep/flatbuffers/similar_word_generated.h"

#include "utils/flatbuffers/io.h"

namespace wordrep{
namespace io{

inline bool operator<(SimilarWordPair const& x, SimilarWordPair const& y) {
    return x.word() < y.word();
}
inline bool operator==(SimilarWordPair const& x, SimilarWordPair const& y) {
    return x.word() == y.word();
}

inline bool operator<(EntityCandidate const& x, EntityCandidate const& y) {
    return x.wiki_uid() < y.wiki_uid();
}
inline bool operator==(EntityCandidate const& x, EntityCandidate const& y) {
    return x.wiki_uid() == y.wiki_uid();
}

}//namespace wordrep::io
}//namespace wordrep