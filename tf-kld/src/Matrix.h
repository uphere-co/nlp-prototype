#pragma once

#include <algorithm>
#include <fstream>
#include <iterator>
#include <sstream>
#include <utility>

#include "src/Type.h"

#include "utils/hdf5.h"
#include "utils/profiling.h"
#include "utils/string.h"

#include <armadillo>
#include <boost/algorithm/string.hpp>

namespace tfkld{

class Documents;
    
struct SpValue{
    SpValue() :
        row(0), col(0), val(0.0) {}

    void reset() {
        row = 0;
        col = 0;
        val = 0.0;
    }
    
    int64_t row, col;
    type::real_t val;
};

struct Param{
    Param() :
        trainFile(""),
        testFile(""),
        kdim(100),
        power(1.0),
        mode(1),
        verbose(0) {}
    
    Param(std::string p_trainFile, std::string p_testFile, int p_kdim, type::real_t p_power, int p_mode, int p_verbose) :
        trainFile{p_trainFile},
        testFile{p_testFile},
        kdim{p_kdim},
        power{p_power},
        mode{p_mode},
        verbose(p_verbose) {}
    
    std::string trainFile, testFile;
    int kdim;
    type::real_t power;
    int mode;
    int verbose;
};

 
void fillValue(Documents &document);
void fillMat(Documents &document, arma::sp_mat &mat);
std::vector<std::vector<type::real_t>> makeSimMat(arma::mat const &V);
void normalizeSimMat(std::vector<std::vector<type::real_t>> &svec);

}//namespace tfkld
