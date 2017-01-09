#pragma once

#include <map>

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
    WordImportance(std::string probfile, std::string wordfile);
    val_t score(WordUID uid) const;

private:
    std::map<WordUID,val_t> uid2score;
};

}//namespace wordrep
