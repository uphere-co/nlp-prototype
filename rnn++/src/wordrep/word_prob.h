#pragma once
#include "wordrep/word_uid.h"

namespace util{
namespace io{
struct H5file;//forward declaration
}; //namespace util::io
}; //namespace util


namespace wordrep{

struct WordImportance{
    using val_t = double;
    WordImportance(util::io::H5file const& stats);
    val_t ratio(WordUID uid)  const {return ratios[uid.val];}
    val_t cutoff(WordUID uid) const {
        auto factor = ratio(uid)+0.001;
        factor = factor<1.0? 1.0: factor;
        return 0.9*(1- 1/(factor));
    }
    std::vector<val_t> ratios;
};

}//namespace wordrep
