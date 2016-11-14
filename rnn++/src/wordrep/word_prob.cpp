#include "wordrep/word_prob.h"

#include "utils/hdf5.h"
#include "utils/algorithm.h"

using namespace util::io;

namespace wordrep{

WordImportance::WordImportance(H5file const& stats)
        : ratios{stats.getRawData<double>(H5name{"prob.ratio"})},
          cutoffs{}
{
    for(auto ratio : ratios){
        auto factor = ratio+0.001;
        factor = factor<1.0? 1.0: factor;
        cutoffs.push_back(0.9*(1- 1/(factor)));
    }
}

WordImportance::val_t WordImportance::ratio(WordUID uid)  const {
    if(uid.val<0 || uid.val>=util::to_signed<val_t>(ratios.size())) return 0.0;
    return ratios[uid.val];
}
WordImportance::val_t WordImportance::cutoff(WordUID uid) const {
    if(uid.val<0 || uid.val>=cutoffs.size()) return 0.0;
    return cutoffs[uid.val];
}
}//namespace wordrep
