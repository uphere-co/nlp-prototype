#pragma once

#include "wordrep/indexes.h"

#include "utils/persistent_vector.h"

namespace wordrep {
struct DummyIndexedTextsIndex{};

struct IndexedTexts{
    using Index = util::IntegerLike<DummyIndexedTextsIndex,-1>; //UID -1 for unknown words.
    IndexedTexts(util::io::H5file const &file, std::string prefix)
            : chunks_idx{file,prefix+".chunk_idx"},
              sents_uid {file,prefix+".sent_uid"},
              words_uid {file,prefix+".word_uid"},
              words {file,prefix+".word"}
    {}
    WordUID   word_uid(Index n) const {return words_uid[n.val];}
    VocaIndex word(Index n) const {return words[n.val];}
    ChunkIndex chunk_idx(Index n) const {return chunks_idx[n.val];}
    SentUID   sent_uid(Index n) const {return sents_uid[n.val];}
    size_t   size() const {return words.size();}

    util::TypedPersistentVector<ChunkIndex> chunks_idx;
    util::TypedPersistentVector<SentUID> sents_uid;
    util::TypedPersistentVector<WordUID> words_uid;
    util::TypedPersistentVector<VocaIndex> words;
};

}//namespace wordrep
