#include "src/Common.h"
#include "src/Vocab.h"
#include "src/Matrix.h"

#include "utils/hdf5.h"
#include "utils/h5io.h"

int main(){

    using namespace util;
    using namespace util::io;
    using namespace tfkld;
    using namespace tfkld::type;
    using namespace tfkld::util;
    using namespace arma;
    
    auto timer = Timer{};

    Documents document;
    std::string fin_name = "YGP_text.txt.short";
    MSParaFile fin{fin_name};

    document.LearnVocab(fin);
    document.LearnSentence(fin);

    tfmat_t tfmat;

    transformDocsToTF(document.vocab, document.docs, tfmat);

    writeTFH5(tfmat);

    return 0;
}
