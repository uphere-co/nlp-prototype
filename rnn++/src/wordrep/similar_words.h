#include <memory>

#include "wordrep/file_formats.h"
#include "utils/parallel.h"

namespace wordrep {

namespace io{
struct SimilarWordPair;
}//namespace wordrep::io

struct SimilarWords {
    static SimilarWords factory(SimilarWordsFile const& file);

    SimilarWords(tbb::concurrent_vector<io::SimilarWordPair> && may_not_sorted);

    void to_file(SimilarWordsFile&& file) const;
    auto begin() const {return similar_words->begin();}
    auto end()   const {return similar_words->end();}
private:
    std::unique_ptr<tbb::concurrent_vector<io::SimilarWordPair>> similar_words;
};

}//namespace wordrep
