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
    testFile.setBegin();
    auto docs2 = LearnDocs(vocab, testFile);
    
    std::vector<SpValue> values2;
    std::vector<real_t> kld2;

    fillValue(values2, vocab, docs2);
    MakeTFKLD(params, kld, tag2, values2, vocab, docs2);

    n_rows = vocab.size();
    n_cols = docs2.size();

    sp_mat inMat2(n_rows, n_cols);

    fillMat(values2, vocab, docs2, inMat2);

    mat U2;
    vec s2;
    mat V2;

    svds(U2,s2,V2,inMat2,K_dim);

    auto svec2 = makeSimMat(V2);

    mParam *mparams;
    mparams = Do_Train(tag,svec);

    int sum = 0;
    int correct = 0;
    int q;
    
    std::vector<std::string> tag3;
    std::vector<std::vector<float>> svec3;
    for(int j=0;j<10;j++) {
    for(int i=0;i<tag2.size();i++) {
    tag3.push_back(tag2[i]);
    svec3.push_back(svec2[i]);
    //mainPredict(tag2, svec2, mparams);
    q = onePredict(tag3, svec3, mparams);

    if(q!=0) correct++;
    sum++;

    std::cout << i << std::endl;
    tag3.clear();
    svec3.clear();
    }
    }

    std::cout << "Accuracy is " << correct/(double)sum << std::endl;
    free(mparams);
    
    return 0;
}
