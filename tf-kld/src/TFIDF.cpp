#include "TFIDF.h"

using namespace util;
using namespace util::io;

using namespace arma;
using namespace tfkld::type;

namespace tfkld{

real_t val_idf(int64_t D, int_t Dt) {
    return log(D/(real_t)Dt);
}
    
void MakeTFIDF(Param const &params, Documents &document) {

    hashmap_t df;
    int64_t D = document.docs.size();
    
    for(auto x : document.values) df[x.row] += 1;

    if(df.size() != document.vocab.size()) {
        std::cout << "Sanity check failed!\n";
        exit(1);
    }
    
    for(auto x : df) document.idf.push_back(val_idf(D,x.second));

    for(auto &x : document.values) x.val *= document.idf[x.row];

}

void runTFIDF(Param const &params, Documents &document) {

    auto timer = Timer{};

    MSParaFile trainFile{params.trainFile};
    int K_dim = params.kdim;

    sp_mat inMat;
    mat U;
    vec s;
    mat V;

    document.LearnVocab(trainFile);
    // document.LearnSentence(trainFile);
    document.LearnYGPDocs(trainFile); // YGP specific procedure
    fillValue(document);
    MakeTFIDF(params, document);
    fillMat(document, inMat);
    svds(U,s,V,inMat,K_dim);

    s.print("s = ");
}


}//namespace tfkld
