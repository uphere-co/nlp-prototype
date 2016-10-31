#include "wordrep/word_prob.h"

#include "utils/hdf5.h"

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

}//namespace wordrep
