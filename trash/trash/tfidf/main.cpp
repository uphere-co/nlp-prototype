#include <armadillo>
//#include <fstream>
//#include <set>
//#include <sstream>
//#include <string>
//#include <algorithm>
//#include <map>
//#include <iterator>
//#include <unordered_set>
//#include <utility>

// #include "utils/hdf5.h"
//#include "utils/string.h"
#include "utils/profiling.h"

extern "C" {
  int mymain( int, arma::uword*, arma::uword*, double* );
    void (*callhaskell)( void );
    void registerfun( void(*)(void) );
}


using namespace util;

extern void (*callhaskell)( void ); 
void registerfun( void (*f)(void) ) {
    callhaskell=  f;
};

// armadillo: column-major ordering

int mymain( int count, arma::uword* prow, arma::uword* pcol, double* pval ){
    auto timer = Timer{};
    std::vector<uint64_t> vloc(2*count);
    vloc.insert(vloc.begin(),prow,prow+count);
    vloc.insert(vloc.begin()+count,pcol,pcol+count);
    std::vector<double> vval(count);
    vval.assign(pval,pval+count);
    arma::vec value(vval);

    
    arma::umat locationt ((arma::uword*)vloc.data(), (arma::uword)count, (arma::uword)2);
    arma::umat location = locationt.t();

    
    arma::sp_mat mat(location, value) ;
    
    timer.here_then_reset("Move matrix done!\n");

    
    arma::mat U;
    arma::vec s;
    arma::mat V;

    arma::svds(U, s, V, mat, 100);

    timer.here_then_reset("Completed SVD calculation!\n");

    for( auto d : s ) {
      std::cout << d << "," ;
    }
    std::cout << std::endl;
    
    std::cout << "Finished!" << std::endl;  

    callhaskell();

    std::cout << "After callhaskell()" << std::endl;
    
    
    return 0;
}

