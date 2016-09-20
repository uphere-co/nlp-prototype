#include "utils/h5io.h"

namespace tfkld{
namespace util{
    
auto Concat(std::vector<std::string> const &words){
    std::vector<char> vec;
    for(auto const &x:words){
        std::copy(x.cbegin(),x.cend(),std::back_inserter(vec));
        vec.push_back('\0');
    }
    return vec;
}

void writeH5File() {
    
    H5file file{H5name{"data.h5"}, hdf5::FileMode::create};
    auto vocab_word = getVocabWord(vocab);
    auto vocab_index = getVocabIndex(vocab);
    std::vector<char> concat_words = Concat(vocab_word);
    file.writeRawData(H5name{"MSR.training.vocab.word"},concat_words);
    file.writeRawData(H5name{"MSR.training.vocab.index"},vocab_index);

}

}//namespace util
}//namespace tfkld
