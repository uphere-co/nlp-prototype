#pragma once

#include "wordrep/voca.h"
#include "wordrep/wordvec.h"

namespace wordrep{
struct VocaInfo{
    static constexpr int64_t dim = 100;
    using val_t = float;
    using voca_vecs_t = WordBlock_base<val_t,dim>;

    VocaInfo(VocaIndexMap&& indexmap, voca_vecs_t&& wvecs)
            : indexmap{std::move(indexmap)},
              wvecs{std::move(wvecs)}
    {}
    VocaIndexMap indexmap;
    voca_vecs_t wvecs;
};

}//namespace wordrep