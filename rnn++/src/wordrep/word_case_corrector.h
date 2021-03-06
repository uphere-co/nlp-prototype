#pragma once

#include "wordrep/word_uid.h"
#include "wordrep/word_prob.h"

namespace wordrep{

struct WordNormalizedForm{
    WordNormalizedForm(std::string word);
    bool operator<(WordNormalizedForm const& right) const{
        return val<right.val;
    }
    std::string val;
};

struct WordCaseCorrector{
    WordCaseCorrector(std::string wordfile,
                      WordImportance const& importance);
    WordCaseCorrector(WordCaseCorrector&& orig)
            : to_original_form{std::move(orig.to_original_form)},
              wordUIDs{std::move(orig.wordUIDs)},
              importance(orig.importance)
    {}
    std::string try_correct(std::string word) const;

    std::map<WordNormalizedForm,std::string> to_original_form;
    WordUIDindex wordUIDs;
    WordImportance importance;
};

}
