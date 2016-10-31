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
    val_t ratio(WordUID uid)  const {
        if(uid.val<0) return 0.0;
        return ratios[uid.val];
    }
    val_t cutoff(WordUID uid) const {
        if(uid.val<0) return 0.0;
        return cutoffs[uid.val];
    }
    std::vector<val_t> ratios;
    std::vector<val_t> cutoffs;
};

}//namespace wordrep
