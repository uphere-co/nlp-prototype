#pragma once

#include <algorithm>
#include <fstream>
#include <iterator>
#include <map>
#include <set>
#include <sstream>
#include <string>
#include <unordered_set>
#include <utility>

#include "utils/hdf5.h"
#include "utils/profiling.h"
#include "utils/string.h"

#include <armadillo>
#include <boost/algorithm/string.hpp>

namespace tfkld{

namespace type{  
using int_t = int;
using int64_t = int64_t;
using float_t = float;
using char_t = char;
}//namespace tfkld::type

using hashmap_t = std::map<tfkld::type::int64_t, tfkld::type::int_t>;
using vocab_t = std::map<std::string, tfkld::type::int64_t>;
using doc_t = std::vector<hashmap_t>;

struct SpValue{
    SpValue() :
        row(0), col(0), val(0.0) {}

    void reset() {
        row = 0;
        col = 0;
        val = 0.0;
    }
    
    int64_t row, col;
    float_t val;
};

struct Param{
    Param() :
        trainFile(""),
        testFile(""),
        kdim(100),
        power(1.0),
        mode(1),
        verbose(0) {}
    
    Param(std::string p_trainFile, std::string p_testFile, int p_kdim, float_t p_power, int p_mode, int p_verbose) :
        trainFile{p_trainFile},
        testFile{p_testFile},
        kdim{p_kdim},
        power{p_power},
        mode{p_mode},
        verbose(p_verbose) {}
    
    std::string trainFile, testFile;
    int kdim;
    float_t power;
    int mode;
    int verbose;
};

 
void fillValue(std::vector<SpValue> &values, vocab_t const &vocab, doc_t const &docs);

void fillMat(std::vector<SpValue> &values, vocab_t const &vocab, doc_t const &docs, arma::sp_mat &mat);
    
std::vector<std::vector<tfkld::type::float_t>> makeSimMat(arma::mat const &V);

void normalizeSimMat(std::vector<std::vector<tfkld::type::float_t>> &svec);

}//namespace tfkld
