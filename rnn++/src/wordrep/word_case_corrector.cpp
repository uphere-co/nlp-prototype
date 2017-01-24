#include "wordrep/word_case_corrector.h"

#include "utils/string.h"

namespace wordrep{

WordNormalizedForm::WordNormalizedForm(std::string word)
        : val{util::string::tolower(word)}
{}

WordCaseCorrector::WordCaseCorrector(std::string wordfile,
                                     WordImportance const& importance)
: wordUIDs{wordfile}, importance{importance}{
    auto words = util::string::readlines(wordfile);
    for(auto word: words){
        auto uid = wordUIDs[word];
        if(importance.is_noisy_word(uid)) continue;
        to_original_form[WordNormalizedForm{word}]=word;
    }
}
std::string WordCaseCorrector::try_correct(std::string word) const{
    WordNormalizedForm nword{word};
    auto wuid = wordUIDs[word];
    auto it=to_original_form.find(nword);
    if(importance.is_unknown(wuid)&&it!=to_original_form.cend())
        return it->second;
    return word;
}

}
