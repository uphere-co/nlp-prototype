#include "wordrep/word_prob.h"
#include <iostream>
#include "utils/hdf5.h"
#include "utils/string.h"
#include "utils/algorithm.h"

using util::io::H5file;
using util::io::H5name;
using util::string::readlines;
using util::map;

namespace{
auto ratio_to_score = [](auto ratio){
    auto factor = ratio+0.001;
    factor = factor<1.0? 1.0: factor;
    return 0.9*(1- 1/(factor));
};
}//nameless namespace

namespace wordrep{

WordImportance::WordImportance(H5file const& stats)
        : scores{map(stats.getRawData<double>(H5name{"prob.ratio"}), ratio_to_score)} {
}

WordImportance::WordImportance(std::string filename)
        : scores{map(readlines(filename),[](auto x){return std::stod(x);})} {
}

WordImportance::val_t WordImportance::score(WordUID uid) const {
    if(uid.val<0 || uid.val>= util::to_type<val_t>(scores.size())) return 0.0;
    return scores[uid.val];
}
}//namespace wordrep
