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

    int K_dim = 3;
    
    mat inMat = { {1,2,3,4}, {5,6,7,8}, {9,10,11,12}, {13,14,15,16} };
    sp_mat insMat(4,4);

    for(int i = 0; i < 4; i++) {
        for(int j = 0; j < 4; j++) {
            insMat(i,j) = 4*i + j + 1;
        }
    }

    mat U;
    vec s;
    mat V;

    svd(U,s,V,inMat);


    std::cout << "Dense Matrix:" << std::endl;
    inMat.print();
    U.print();
    s.print();
    V.print();
    
    svds(U,s,V,insMat,K_dim);

    std::cout << "Sparse Matrix:" << std::endl;
    insMat.print();
    U.print();
    s.print();
    V.print();
    
    
    return 0;
}
