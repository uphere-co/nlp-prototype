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

    int K_dim = 3; // Reduced rank

    // TF matrix for the test.
    // n_dim by m_dim .
    int n_dim = 4;
    int m_dim = 4;
    
    sp_mat inM(n_dim,m_dim);
    for(int i = 0; i < n_dim; i++) {
        for(int j = 0; j < m_dim; j++) {
            inM(i,j) = n_dim*i + j + 1;
        }
    }

    mat inU;
    vec ins;
    mat inV;

    svds(inU,ins,inV,inM,K_dim);

    // incremental n_dim by c_dim matrix
    int c_dim = 1;
    mat inC(n_dim,c_dim);
    for(int i = 0; i < n_dim; i++) {
        for(int j = 0; j < c_dim; j++) {
            inC(i,j) = i*i+j*(j+1)+1;
        }
    }

    mat inL = trans(inU)*inC;

    mat inH;
    inH = inC - inU*inL;

    mat inJ;
    mat inK;

    qr(inJ,inK,inH);

    mat inQ(K_dim+n_dim,K_dim+c_dim,fill::zeros);
    
    for(int r=0;r<K_dim;r++)
        inQ(r,r) = ins(r);

    for(int i=0;i<K_dim;i++) {
        for(int j=0;j<c_dim;j++) {
            inQ(i,K_dim+j) = inL(i,j);
        }
    }

    for(int i=0;i<n_dim;i++) {
        for(int j=0;j<c_dim;j++) {
            inQ(K_dim+i,K_dim+j)=inK(i,j);
        }
    }
    inL.print("inL = ");
    inK.print("inK = ");
    inQ.print("inQ = ");
    /*
    mat inUp;
    vec insp;
    mat inVp;
    
    svd(inUp,insp,inVp,inQ);

    mat inUpp;
    vec inspp;
    mat inVpp;

    mat mapU = join_rows(inU,inJ);

    mapU.print("mapU = ");
    mat mapVzeros;
    mapVzeros << 0 << endr << 0 << endr << 0 << endr << 0 << endr;
    mat mapV = join_rows(inV,mapVzeros); // Accidental. Very temporary.

    inUpp = mapU * inUp;
    inspp = insp;
    inVpp = mapV * inVp;

    mat diag_inspp = diagmat(inspp);

    */
    //mat resMat = inUpp*diag_inspp*trans(inVpp);



    /*
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

    std::cout << "Matrix Multiplication:" << std::endl;
    mat X1 = {{1,2,3},{4,5,6},{7,8,9}};
    mat X2 = {{1,2,3},{4,5,6},{7,8,9}};

    mat X3 = X1*X2;

    X3.print();


    */
    
    return 0;
}
