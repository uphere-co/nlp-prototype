#include "src/Matrix.h"
#include "src/Vocab.h"
#include "src/TFKLD.h"
#include "src/Type.h"
#include "src/Query.h"

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

    
    Documents document;
    MSParaFile trainFile{params.trainFile};
    int K_dim = params.kdim;

    sp_mat inMat;
    mat U;
    vec s;
    mat V;

    document.LearnVocab(trainFile);
    document.LearnPairSentence(trainFile);
    document.LearnTag(trainFile);
    
    fillValue(document);
    MakeTFKLD(params, document);
    fillMat(document, inMat);
    
    svds(U,s,V,inMat,K_dim);
    auto svec = makeSimMat(V);



    Documents document2;
    MSParaFile testFile{params.testFile};
    sp_mat inMat2;
    mat U2;
    vec s2;
    mat V2;

    document2.vocab = document.vocab;
    document2.LearnPairSentence(testFile);
    document2.LearnTag(testFile);
    
    fillValue(document2);
    document2.kld = document.kld; // This should be preceded.
    MakeTFKLD_without_calculating_KLD(params, document2);
    // Note that this function does not check that KLD comes from other document set.
    // Name is just for documentation purpose.
    fillMat(document2,inMat2);

    //////////////svds(U2,s2,V2,inMat2,K_dim);
    //////////////auto svec2 = makeSimMat(V2);

    mParam *mparams;
    mparams = Do_Train(document.tag,svec);


    std::cout << "train is over.\n";
    fflush(stdout);

    
    int sum = 0;
    int correct = 0;
    int q;

    //auto k_word = mapSentoLatent("hello my world", document);

    searchSentence(mparams, document, "the stock rose or about percent to close friday at on the new york stock exchange");

    //mainPredict(document2.tag, svec2, mparams);

    /*
    std::vector<std::string> tag3;
    std::vector<std::vector<float>> svec3;
    for(int j=0;j<1;j++) {
    for(int i=0;i<document2.tag.size();i++) {
    tag3.push_back(document2.tag[i]);
    svec3.push_back(svec2[i]);
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
    */
    return 0;
}
