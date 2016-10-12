#include "utils/h5io.h"

namespace tfkld{
namespace util{

using namespace ::util::io;
 
auto Concat(std::vector<std::string> const &words){
    std::vector<char> vec;
    for(auto const &x:words){
        std::copy(x.cbegin(),x.cend(),std::back_inserter(vec));
        vec.push_back('\0');
    }
    return vec;
}

void writeVocabH5(vocab_t &vocab) {

    std::ifstream f{"data.h5"};

    if(!f.good()) {
        H5file file{H5name{"data.h5"}, hdf5::FileMode::create};
    } // Repeat use of file as H5file can be dangerous?

    H5file file{H5name{"data.h5"}, hdf5::FileMode::rw_exist};
    auto vocab_word = getVocabWord(vocab);
    auto vocab_index = getVocabIndex(vocab);
    std::vector<char> concat_words = Concat(vocab_word);
    
    file.overwriteRawData(H5name{"MSR.training.vocab.word"},concat_words);
    file.overwriteRawData(H5name{"MSR.training.vocab.index"},vocab_index);

    /*
    auto dataset = file.val;

    auto x = dataset.openDataSet("MSR.training.vocab.worda");

    auto vocab_word_H5 = file.getRawData<char>(H5name{"MSR.training.vocab.word"});
    auto vocab_index_H5 = file.getRawData<type::int64_t>(H5name{"MSR.training.vocab.index"});
    */
}

void writeDocsH5(doc_t &docs) {
    std::ifstream f{"data.h5"};

    if(!f.good()) {
        H5file file{H5name{"data.h5"}, hdf5::FileMode::create};
    }

    H5file file{H5name{"data.h5"}, hdf5::FileMode::rw_exist};
    
    auto docs_word_idx = getDocsIndex(docs);
    auto docs_word_count = getDocsCount(docs);
    //std::vector<char> concat_words = Concat(vocab_word);
    
    file.writeRawData(H5name{"MSR.training.docs.index"},docs_word_idx);
    file.writeRawData(H5name{"MSR.training.docs.count"},docs_word_count);

}

void writeTFH5(tfmat_t &tfmat) {
    std::ifstream f{"tfmat.h5"};

    if(!f.good()) {
        H5file file{H5name{"tfmat.h5"}, hdf5::FileMode::create};
    }

    H5file file{H5name{"tfmat.h5"}, hdf5::FileMode::rw_exist};

    auto doc_idx = getDocIndex(tfmat);
    auto word = getWord(tfmat);
    auto word_count = getWordCount(tfmat);
    //std::vector<char> concat_doc_idx = Concat(doc_idx);
    std::vector<char> concat_word = Concat(word);

    file.writeRawData(H5name{"YGP.Amendment.DocIndex"},doc_idx);
    file.writeRawData(H5name{"YGP.Amendment.Word"},concat_word);
    file.writeRawData(H5name{"YGP.Amendment.WordCount"},word_count);
    
}
}//namespace util
}//namespace tfkld
