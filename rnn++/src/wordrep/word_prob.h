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
    WordImportance(std::string filename);
    val_t score(WordUID uid) const;

private:
    std::vector<val_t> scores;
};

}//namespace wordrep
