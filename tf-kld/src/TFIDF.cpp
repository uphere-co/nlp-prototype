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

std::vector<int> findDocbyTopicThreshold(mat &V, int topic, double threshold) {
    std::vector<int> result;
    
    for(int64_t i = 0; i<V.n_rows; i++) {
        if(std::abs(V.row(i)[topic]) > threshold) result.push_back(i);
    }

    return result;
}

std::vector<int> findDocbyTopicRank(mat &V, int topic, int n) {
    std::vector<std::pair<int64_t, double>> rank;
    std::vector<int> result;

    for(int64_t i = 0; i<V.n_rows; i++) {
        rank.push_back(std::make_pair(i,std::abs(V.row(i)[topic])));
    }

    std::sort(rank.begin(), rank.end(), [](auto &left, auto &right) {
      return left.second  < right.second;
    });

    for(int i = 0; i < n; i++) {
        result.push_back(std::get<0>(rank[i]));
    }

    return result;
    
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

    //auto target = findDocbyTopicThreshold(V, 0, 1e-4);
    auto target = findDocbyTopicRank(V, 5, 10);
    for(auto x : target) {
        printYGPDocs(trainFile, x);
        std::cout << "\n\n";
    }
    
}


}//namespace tfkld
