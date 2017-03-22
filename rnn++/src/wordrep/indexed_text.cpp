#include "wordrep/indexed_text.h"

#include "utils/profiling.h"

namespace wordrep {

IndexedTexts::IndexedTexts(util::io::H5file const &file, std::string prefix, VocaIndexMap const& vmap)
        : chunks_idx{file,prefix+".chunk_idx"},
          sents_uid {file,prefix+".sent_uid"},
          words_uid {file,prefix+".word_uid"},
          words{}    {
    util::Timer timer;
    words.reserve(words_uid.size());
    for(auto w : words_uid) words.push_back(vmap[w]);
    timer.here_then_reset("Get VocaIndex.");
}

}//namespace wordrep
