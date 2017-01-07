#include <iostream>

#include <fmt/printf.h>

#include "wordrep/word_prob.h"
#include "wordrep/word_hash.h"

#include "utils/hdf5.h"
#include "utils/persistent_vector.h"
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

WordImportance::WordImportance(H5file const& stats) {
    auto scores = map(stats.getRawData<double>(H5name{"prob.ratio"}), ratio_to_score);
    util::TypedPersistentVector<WordUID> uids{stats, "prob.word_uid"};
    auto n = scores.size();
    for(decltype(n)i=0; i!=n; ++i) uid2score[uids[i]]=scores[i];
}

WordImportance::WordImportance(std::string probfile, std::string wordfile) {
    auto scores = map(readlines(probfile),[](auto x){return std::stod(x);});
    auto words = readlines(wordfile);
    WordUIDindex  wordUIDs{wordfile};
    auto n = scores.size();
    for(decltype(n)i=0; i!=n; ++i) uid2score[wordUIDs[words[i]]]=scores[i];
}

WordImportance::val_t WordImportance::score(WordUID uid) const {
    auto it=uid2score.find(uid);
    if(it==uid2score.cend()) return 0.0;
    return it->second;
}

}//namespace wordrep
