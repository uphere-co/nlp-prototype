#include "src/Matrix.h"
#include "src/Vocab.h"
#include "src/TFKLD.h"
#include "src/Type.h"
#include "src/SVM.h"

#include "utils/h5io.h"
#include "utils/help.h"

int main(int argc, char **argv){

    using namespace tfkld;
    using namespace tfkld::type;
    using namespace tfkld::util;
    using namespace arma;
    using namespace tfkld::svm;
    using namespace tfkld::svm::training;
    using namespace tfkld::svm::predicting;

    Param params;

    if(argc < 2) {
        printHelp();
        return 1;
    } else {
        ArgPass(argc, argv, params);
    }


    MSParaFile trainFile{params.trainFile};
    
    int K_dim = params.kdim;
    
    auto vocab = LearnVocab(trainFile);
    trainFile.setBegin();
    auto docs = LearnDocs(vocab,trainFile);
    trainFile.setBegin();
    auto tag = LearnTag(trainFile);
    
    std::vector<SpValue> values;
    std::vector<real_t> kld;

    fillValue(values, vocab, docs);
    MakeTFKLD(params, kld, tag, values, vocab, docs);

    int64_t n_rows, n_cols;
    n_rows = vocab.size();
    n_cols = docs.size();

    sp_mat inMat(n_rows, n_cols);

    fillMat(values, vocab, docs, inMat);

    mat U;
    vec s;
    mat V;

    svds(U,s,V,inMat,K_dim);

    auto svec = makeSimMat(V);



    MSParaFile testFile{params.testFile};
    auto tag2 = LearnTag(testFile);

    std::vector<SpValue> values2;
    std::vector<real_t> kld2;

    fillValue(values2, vocab, docs);
    MakeTFKLD(params, kld, tag2, values2, vocab, docs);

    n_rows = vocab.size();
    n_cols = docs.size();

    sp_mat inMat2(n_rows, n_cols);

    fillMat(values2, vocab, docs, inMat2);

    mat U2;
    vec s2;
    mat V2;

    svds(U2,s2,V2,inMat2,K_dim);

    auto svec2 = makeSimMat(V);

    mParam *mparams;
    mparams = Do_Train(tag,svec);
    
    mainPredict(tag2, svec2, mparams);
    return 0;
}
