#pragma once
#include <memory>

#include "wordrep/indexes.h"
#include "wordrep/file_formats.h"
#include "wordrep/io.h"

#include "utils/parallel.h"
#include "utils/index_range.h"

namespace wordrep {

namespace io{
//struct SimilarWordPair;
}//namespace wordrep::io

struct SimilarWordsIndexDummy{};

struct SimilarWords {
    using Index = util::IntegerLike<SimilarWordsIndexDummy>;
    using Range = util::IndexRange<Index>;

    static SimilarWords factory(SimilarWordsFile const& file);

    SimilarWords(tbb::concurrent_vector<io::SimilarWordPair>&& may_not_sorted);

    void to_file(SimilarWordsFile&& file) const;
    auto begin() const {return similar_words.begin();}
    auto end()   const {return similar_words.end();}
    Range find(WordUID uid) const;
    auto& at(Index idx) const {return similar_words.at(idx.val);}
    auto word(Index idx)       const {return at(idx).word();}
    auto sim_word(Index idx)   const {return at(idx).sim();}
    auto similarity(Index idx) const {return at(idx).similarity();}
private:
    tbb::concurrent_vector<io::SimilarWordPair> similar_words;
};

}//namespace wordrep
