#pragma once

#include "wordrep/indexes.h"

#include "utils/persistent_vector.h"

namespace wordrep {

struct IndexedTexts{
    IndexedTexts(util::io::H5file const &file, std::string prefix)
            : chunks_idx{file,prefix+".chunk_idx"},
              sents_uid {file,prefix+".sent_uid"},
              words_uid {file,prefix+".word_uid"},
              words {file,prefix+".word"}
    {}
    WordUID   word_uid(size_t n) const {return words_uid[n];}
    VocaIndex word(size_t n) const {return words[n];}

    util::TypedPersistentVector<ChunkIndex> chunks_idx;
    util::TypedPersistentVector<SentUID> sents_uid;
    util::TypedPersistentVector<WordUID> words_uid;
    util::TypedPersistentVector<VocaIndex> words;
};

}//namespace wordrep
