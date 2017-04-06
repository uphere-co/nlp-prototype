#pragma once

#include <vector>
#include <memory>

#include "utils/parallel.h"

#include "wordrep/serialized_annotation.h"
#include "wordrep/simiarity_score.h"

namespace wordrep{

struct PreprocessedSentences{
    static PreprocessedSentences factory(std::vector<Sentence> const& sents,
                                         AnnotationFile const& annotation);

    auto begin() const {return data->cbegin();}
    auto end()   const {return data->cend();}
    size_t size() const {return data->size();}
    Scoring::SentenceToScored const& at(size_t  i)   const {return data->at(i);}
    Scoring::SentenceToScored const& at(SentUID idx) const {return at(idx.val);}
    using T = tbb::concurrent_vector<Scoring::SentenceToScored>;
    std::unique_ptr<T> data = std::make_unique<T>();
};

}//namespace wordrep
