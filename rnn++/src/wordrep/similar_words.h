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

    using key_type   = WordUID;
    using value_type = io::SimilarWordPair;
    static key_type to_key(value_type const& x) {return x.word();};

    static SimilarWords factory(SimilarWordsFile const& file);
    SimilarWords(tbb::concurrent_vector<value_type>&& may_not_sorted);

    void to_file(SimilarWordsFile&& file) const;
    auto begin() const {return similar_words.begin();}
    auto end()   const {return similar_words.end();}
    Range find(key_type word) const;
    auto& at(Index idx) const {return similar_words.at(idx.val);}
    WordUID word(Index idx)     const {return at(idx).word();}
    WordUID sim_word(Index idx) const {return at(idx).sim();}
    auto similarity(Index idx)  const {return at(idx).similarity();}

    struct OpSimilarity{
        auto similarity(WordUID word) const {
            for(auto idx : range){
                if(word==self.sim_word(idx))
                    return self.similarity(idx);
            }
            return decltype(self.similarity(0)){0.0};
        }
        Range range;
        SimilarWords const& self;
    };

    OpSimilarity get_op_sim(WordUID word) const {
        return {find(word), *this};
    }

private:
    tbb::concurrent_vector<value_type> similar_words;
};

}//namespace wordrep
