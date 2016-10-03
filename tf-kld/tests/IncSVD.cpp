#include <algorithm>
#include <fstream>
#include <iterator>
#include <map>
#include <set>
#include <sstream>
#include <string>
#include <utility>

#include <armadillo>

using namespace arma;

bool checkMat(int row, int col, mat const &mat) {
    if(mat.n_rows == row && mat.n_cols == col) return true;
    return false;
}
bool checkVec(int dim, vec const &vec) {
    if(vec.size() == dim) return true;
    return false;
}

void exit_with_error(std::string error_message) {
    std::cout << error_message << std::endl;
    exit(1);
}

int main() {

    int r_dim = 5; // Reduced rank r

    // TF matrix for the test.
    // n_dim by m_dim .
    int n_dim = 100;
    int m_dim = 100;
    
    sp_mat inM(n_dim,m_dim);
    for(int i = 0; i < n_dim; i++) {
        for(int j = 0; j < m_dim; j++) {
            inM(i,j) = n_dim*i + j + 1;
        }
    }

    mat inU;
    vec ins;
    mat inV;

    svds(inU,ins,inV,inM,r_dim);

    if(checkMat(n_dim,r_dim,inU) == false) exit_with_error("U matrix is incorrect.");
    if(checkVec(r_dim,ins) == false) exit_with_error("s diagonal matrix is incorrect.");
    if(checkMat(m_dim,r_dim,inV) == false) exit_with_error("V matrix is incorrect.");
    
    // incremental n_dim by c_dim matrix
    int c_dim = 1;
    mat inC(n_dim,c_dim);
    for(int i = 0; i < n_dim; i++) {
        for(int j = 0; j < c_dim; j++) {
            inC(i,j) = i*i+j*(j+1)+1;
        }
    }

    if(checkMat(n_dim,c_dim,inC) == false) exit_with_error("C matrix is incorrect.");
    
    mat inL = trans(inU)*inC;
    if(checkMat(r_dim,c_dim,inL) == false) exit_with_error("L matrix is incorrect.");
    
    mat inH;
    inH = inC - inU*inL;
    if(checkMat(n_dim,c_dim,inH) == false) exit_with_error("H matrix is incorrect.");

    mat inJ;
    mat inK;

    qr(inJ,inK,inH);

    if(checkMat(n_dim,n_dim,inJ) == false) exit_with_error("J matrix is incorrect.");
    if(checkMat(n_dim,c_dim,inK) == false) exit_with_error("K matrix is incorrect.");
    
    mat inQ(r_dim+n_dim,r_dim+c_dim,fill::zeros);
    
    for(int r=0;r<r_dim;r++)
        inQ(r,r) = ins(r);

    for(int i=0;i<r_dim;i++) {
        for(int j=0;j<c_dim;j++) {
            inQ(i,r_dim+j) = inL(i,j);
        }
    }

    for(int i=0;i<n_dim;i++) {
        for(int j=0;j<c_dim;j++) {
            inQ(r_dim+i,r_dim+j)=inK(i,j);
        }
    }

    if(checkMat(r_dim+n_dim,r_dim+c_dim,inQ) == false) exit_with_error("Q matrix is incorrect.");
    
    mat inUp;
    vec insp;
    mat inVp;
    
    svd(inUp,insp,inVp,inQ);

    if(checkMat(r_dim+n_dim,r_dim+n_dim,inUp) == false) exit_with_error("Up matrix is incorrect.");
    if(checkVec(r_dim+c_dim,insp) == false) exit_with_error("sp diagonal matrix is incorrect.");
    // Assuming r_dim+c_dim <= r_dim+n_dim
    if(checkMat(r_dim+c_dim,r_dim+c_dim,inVp) == false) exit_with_error("Vp matrix is incorrect.");

    
    mat inUpp;
    vec inspp;
    mat inVpp;

    mat mapU;
    mapU = join_rows(inU,inJ);
    if(checkMat(n_dim,r_dim+n_dim,mapU) == false) exit_with_error("mapU matrix is incorrect.");
    
    mat mapV(m_dim+c_dim,r_dim+c_dim,fill::zeros);
    for(int i=0;i<m_dim;i++) {
        for(int j=0;j<r_dim;j++) {
            mapV(i,j) = inV(i,j);
        }
    }

    for(int i=0;i<c_dim;i++) {
        mapV(m_dim+i,r_dim+i) = 1; 
    }
    if(checkMat(m_dim+c_dim,r_dim+c_dim,mapV) == false) exit_with_error("mapV matrix is incorrect.");    
    
    inUpp = mapU * inUp;
    inspp = insp;
    inVpp = mapV * inVp;

    mat inspp_diag(r_dim+n_dim,r_dim+c_dim,fill::zeros);

    for(int i=0;i<r_dim+c_dim;i++) {
        inspp_diag(i,i) = insp(i);
    }

    if(checkMat(n_dim,n_dim+r_dim,inUpp) == false) exit_with_error("Upp matrix is incorrect.");
    if(checkMat(n_dim+r_dim,r_dim+c_dim,inspp_diag) == false) exit_with_error("inspp_diag matrix is incorrect.");
    if(checkMat(m_dim+c_dim,r_dim+c_dim,inVpp) == false) exit_with_error("Vpp matrix is incorrect.");


    
    mat resMat = inUpp*inspp_diag*trans(inVpp);

    resMat.print("resMat = ");
    // End of updating SVD (slow way)

    
    return 0;
}
