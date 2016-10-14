#include "wordrep/word_prob.h"

#include "utils/hdf5.h"

using namespace util::io;

namespace wordrep{

WordImportance::WordImportance(H5file const& stats)
        : ratios{stats.getRawData<double>(H5name{"prob.ratio"})}
{}

}//namespace wordrep
