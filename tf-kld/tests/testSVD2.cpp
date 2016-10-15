#include <algorithm>
#include <fstream>
#include <iterator>
#include <map>
#include <set>
#include <sstream>
#include <string>
#include <utility>

#include "utils/profiling.h"

#include <armadillo>

using namespace util;
using namespace arma;

bool checkMat(int row, int col, mat const &mat) {
    if(mat.n_rows == row && mat.n_cols == col) return true;
    return false;
}
bool checkVec(int dim, vec const &vec) {
    if(vec.size() == dim) return true;
    return false;
}

int main() {

    auto timer = Timer{};
    timer.here_then_reset("");
    
    int r_dim = 3;

    sp_mat M = sprandu<sp_mat>(1000,1000, 0.1);

    M(0,0) = 10;
    M(10,10)= 10;
    M(100,100) = 10;
    M(102,102) = 100;
    //M(105,105) = 100;
    mat U;
    vec s;
    mat V;

    svds(U,s,V,M,r_dim);


    s.print("s = ");
    return 0;
}
