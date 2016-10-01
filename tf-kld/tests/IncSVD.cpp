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

    int r_dim = 3; // Reduced rank r

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

    svds(inU,ins,inV,inM,r_dim);

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
    
    mat inUp;
    vec insp;
    mat inVp;
    
    svd(inUp,insp,inVp,inQ);

    mat inUpp;
    vec inspp;
    mat inVpp;

    mat mapU = join_rows(inU,inJ);

    mat mapV(m_dim+c_dim,r_dim+c_dim,fill::zeros);
    for(int i=0;i<m_dim;i++) {
        for(int j=0;j<r_dim;j++) {
            mapV(i,j) = inV(i,j);
        }
    }

    for(int i=0;i<c_dim;i++) {
        mapV(m_dim+i,r_dim+i) = 1; 
    }

    inUpp = mapU * inUp;
    inspp = insp;
    inVpp = mapV * inVp;

    mat inspp_diag(r_dim+n_dim,r_dim+c_dim,fill::zeros);

    for(int i=0;i<r_dim+c_dim;i++) {
        inspp_diag(i,i) = insp(i);
    }

    
    mat resMat = inUpp*inspp_diag*trans(inVpp);

    inM.print("inM = ");
    inC.print("inC = ");
    resMat.print("resMat = ");

    return 0;
}
