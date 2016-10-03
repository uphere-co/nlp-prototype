#include "src/Matrix.h"

using namespace util;
using namespace util::io;

using namespace tfkld::type;

namespace tfkld{

void fillValue(std::vector<SpValue> &values, vocab_t const &vocab, doc_t const &docs) {    
    SpValue value;
    for(auto it = docs.begin(); it != docs.end(); ++it) {
        value.reset();
        for(auto itt = it -> begin(); itt != it -> end(); ++itt) {
            value.row = (*itt).first;
            value.col = std::distance(docs.begin(),it);
            value.val = (*itt).second;
            values.push_back(value);
        }
    }   
}

void fillMat(std::vector<SpValue> &values, vocab_t const &vocab, doc_t const &docs, arma::sp_mat &mat) {

    int64_t count{static_cast<int64_t>(values.size())};
    
    arma::umat location(2,count);
    arma::vec value(count);

    for(int64_t a = 0; a < count; a++) {
        location(0,a) = values[a].row;
        location(1,a) = values[a].col;
        value(a) = values[a].val;
    }

    mat = std::move( arma::sp_mat(location, value) );
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
