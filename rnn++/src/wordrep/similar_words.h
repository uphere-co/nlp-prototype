#pragma once
#include <memory>

#include "wordrep/indexes.h"
#include "wordrep/file_formats.h"
#include "wordrep/io.h"

#include "utils/parallel.h"

namespace wordrep {

namespace io{
//struct SimilarWordPair;
}//namespace wordrep::io

struct SimilarWordsIndexDummy{};

struct SimilarWords {
    using Index = util::IntegerLike<SimilarWordsIndexDummy>;
    struct Range{
        struct Iterator{
            Iterator(Index idx) : idx{idx} {}
            Index operator*( void ) const {return idx;}
            void operator++(void) {++idx;}
            bool operator==(Iterator rhs) const {return idx == rhs.idx;}
            bool operator!=(Iterator rhs) const {return idx != rhs.idx;}
        private:
            Index idx;
        };
        Range(Index beg, Index end) : beg_{beg},end_{end} {}
        auto begin() const { return Iterator{beg_};}
        auto end() const { return Iterator{end_};}
        size_t size() const {return end_.val - beg_.val;}
    private:
        Index beg_;
        Index end_;
    };

    static SimilarWords factory(SimilarWordsFile const& file);

    SimilarWords(tbb::concurrent_vector<io::SimilarWordPair>&& may_not_sorted);

    void to_file(SimilarWordsFile&& file) const;
    auto begin() const {return similar_words.begin();}
    auto end()   const {return similar_words.end();}
    Range find(WordUID uid) const;
    auto& at(Index idx) const {return similar_words.at(idx.val);}
    WordUID word(Index idx)     const {return at(idx).word();}
    WordUID sim_word(Index idx) const {return at(idx).sim();}
    auto similarity(Index idx)  const {return at(idx).similarity();}
private:
    tbb::concurrent_vector<io::SimilarWordPair> similar_words;
};

}//namespace wordrep
