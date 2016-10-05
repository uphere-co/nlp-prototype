#include "src/Matrix.h"
#include "src/Vocab.h"

using namespace util;
using namespace util::io;

using namespace tfkld::type;

namespace tfkld{

void fillValue(Documents &document) {
    SpValue value;
    doc_t docs = document.docs;
    for(auto it = docs.begin(); it != docs.end(); ++it) {
        value.reset();
        for(auto itt = it -> begin(); itt != it -> end(); ++itt) {
            value.row = (*itt).first;
            value.col = std::distance(docs.begin(),it);
            value.val = (*itt).second;
            document.values.push_back(value);
        }
    }   
}

void fillMat(Documents &document, arma::sp_mat &mat) {

    int64_t count{static_cast<int64_t>(document.values.size())};

    int64_t n_rows, n_cols;
    n_rows = document.vocab.size();
    n_cols = document.docs.size();

    mat.set_size(n_rows, n_cols);
    
    arma::umat location(2,count);
    arma::vec value(count);

    for(int64_t a = 0; a < count; a++) {
        location(0,a) = document.values[a].row;
        location(1,a) = document.values[a].col;
        value(a) = document.values[a].val;
    }

    mat = std::move( arma::sp_mat(location, value) );
}

arma::mat mapSentoLatent(std::string sen, Documents &document) {
    Documents resultDocs;
    hashmap_t doc = document.makeSentoDoc(sen);

    resultDocs.K_dim = document.K_dim;
    resultDocs.vocab = document.vocab;
    resultDocs.docs.push_back(doc);

    fillValue(resultDocs);
    arma::sp_mat inMat;
    fillMat(resultDocs,inMat);
    arma::mat U;
    arma::vec s;
    arma::mat V;

    arma::svds(U,s,V,inMat,resultDocs.K_dim);

    return V;
}

    
std::vector<std::vector<real_t> > makeSimMat(arma::mat const &V) {

    std::vector<real_t> svec;
    std::vector< std::vector<real_t> > result;
    real_t minus = 0;

    // transpose V
    for(int a = 0; a < V.n_rows; a=a+2) {
        for(int b = 0; b < V.n_cols; b++) {
            svec.push_back(V(a,b) + V(a+1,b));
            minus = pow(V(a,b) - V(a+1,b),2.0);
        }
        svec.push_back(sqrt(minus));

        result.push_back(svec);
        minus = 0;
        svec.clear();
        
    }

    return result;
}


void normalizeSimMat(std::vector<std::vector<real_t> > &svec) {

    real_t sum = 0;
    
    for(auto &x : svec) {
        for(auto &y : x) {
            sum += y*y;
        }

        for(auto &y : x) {
            y = y / sqrt(sum);
        }

        sum = 0;
    }
}

}//namespace tfkld
