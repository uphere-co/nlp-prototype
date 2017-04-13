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

}//namespace wordrep::io
}//namespace wordrep