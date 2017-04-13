#pragma once

#include <map>

#include "wordrep/word_uid.h"
#include "wordrep/file_formats.h"

namespace util{
namespace io{
struct H5file;//forward declaration
}; //namespace util::io
}; //namespace util


namespace wordrep{

struct Words;
struct WordImportance{
    struct IterToken{
        auto begin() const{return self.uid2score.cbegin();}
        auto end()   const{return self.uid2score.cend();}
        WordImportance const& self;
    };
    static WordImportance factory(WordImportanceFile const& file);
    using val_t = double;
    //words with their score below low_cutoff are considered noisy words.
    static constexpr val_t low_cutoff = 0.6;
    //low_cutoff_ratio is a functio of low_cutoff and the ratio_to_score function.
    static constexpr val_t low_cutoff_ratio = 3.0;
    WordImportance(std::map<WordUID,val_t>&& uid2score);
    WordImportance(util::io::H5file const& stats);
    WordImportance(std::string probfile, std::string wordfile);
    val_t score(WordUID uid) const;
    bool is_noisy_word(WordUID uid) const { return score(uid)<low_cutoff;}
    bool is_unknown(WordUID uid) const {return score(uid)==val_t{0.0};}

    auto const& all_scores() const {return uid2score;};
    IterToken iter_token() const{return {*this};}

private:
    std::map<WordUID,val_t> uid2score;
};

}//namespace wordrep
