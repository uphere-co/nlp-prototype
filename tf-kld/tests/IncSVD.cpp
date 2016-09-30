#include <algorithm>
#include <fstream>
#include <iterator>
#include <map>
#include <set>
#include <sstream>
#include <string>
#include <utility>

#include <armadillo>

int main() {
    using namespace arma;
    
    mat inMat = { {1,2,3,4}, {5,6,7,8}, {9,10,11,12}, {13,14,15,16} };

    mat U;
    vec s;
    mat V;

    svd(U,s,V,inMat);

    std::cout << "SVD for Dense Matrix:" << std::endl;
    inMat.print();
    U.print();
    s.print();
    V.print();

    
    int K_dim = 3;
    sp_mat insMat(4,4);

    for(int i = 0; i < 4; i++) {
        for(int j = 0; j < 4; j++) {
            insMat(i,j) = 4*i + j + 1;
        }
    }

    mat sU;
    vec ss;
    mat sV;

    svds(sU,ss,sV,insMat,K_dim);

    std::cout << "Truncated SVD for Sparse Matrix:" << std::endl;
    insMat.print();
    sU.print();
    ss.print();
    sV.print();
    
    mat X = { {1,2,3,4}, {5,6,7,8}, {9,10,11,12}, {13,14,15,16} };
    mat Q, R;

    qr(Q,R,X);

    std::cout << "QR Decomposition for Dense Matrix:" << std::endl;
    X.print();
    Q.print();
    R.print();
    
    return 0;
}
