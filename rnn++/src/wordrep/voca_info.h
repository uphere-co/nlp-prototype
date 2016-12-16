#pragma once

#include "wordrep/voca.h"
#include "wordrep/wordvec.h"

namespace wordrep{
struct VocaInfo{
    using val_t = float;
    using voca_vecs_t = WordBlock_base<val_t,100>;
    VocaInfo(std::string h5file, std::string voca_name,
             std::string wvec_name, std::string w2v_float_t)
            : indexmap{load_voca(h5file, voca_name)},
              wvecs{load_raw_wvec(h5file, wvec_name, w2v_float_t)}
    {}
    VocaIndexMap indexmap;
    voca_vecs_t wvecs;
};

}//namespace wordrep