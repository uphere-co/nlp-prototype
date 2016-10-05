#include "src/Query.h"

namespace tfkld{

double calculateDistance(arma::mat a, arma::mat b) {

    double sum = 0;
    for(int i = 0; i < a.n_cols; i++) {
        sum += a(i)*b(i);
    }
    return sum;
}
   
void searchSentence(Documents &document, std::string sen) {

    Documents resultDocs;
    resultDocs = document;

    hashmap_t doc = document.makeSentoDoc(sen);

    resultDocs.docs.push_back(doc);
    resultDocs.values.clear();
    
    fillValue(resultDocs);
    arma::sp_mat inMat;
    fillMat(resultDocs,inMat);
    arma::mat U;
    arma::vec s;
    arma::mat V;

    arma::svds(U,s,V,inMat,resultDocs.K_dim);

    int V_last_row = V.n_rows - 1;

    for(int i=0;i<V.n_rows - 1; i++) {
        double distance = dot(V.row(i),V.row(V_last_row));
        if(distance > 0.8) V.row(i).print("V is ");
    }

    
}

}
