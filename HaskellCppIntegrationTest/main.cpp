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
    int mymain( int, int*, int*, double* );
    void (*callhaskell)( void );
    void registerfun( void(*)(void) );
}


using namespace util;

extern void (*callhaskell)( void ); 
void registerfun( void (*f)(void) ) {
    callhaskell=  f;
};

int mymain( int count,  int* prow, int* pcol, double* pval ){
    auto timer = Timer{};
    arma::umat location(2,count);
    arma::vec value(count);
    for(size_t a = 0 ; a < count ; a++ ) {
    	location(0,a) = prow[a];
    	location(1,a) = pcol[a];
	value(a) = pval[a];
    } 

    arma::sp_mat mat(location, value) ;
    
    // mat = std::move( arma::sp_mat(location, value) );
    
    
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

