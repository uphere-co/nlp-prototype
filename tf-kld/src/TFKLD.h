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

using namespace util;
using namespace util::io;

namespace tfkld{
namespace type{
  
using int_t = int;
//using int64_t = int64_t;
using float_t = float;
using char_t = char;
}//namespace tfkld::type
}//namespace tfkld

using namespace tfkld::type;

using hashmap_t = std::map<int64_t, int_t>;
using vocab_t = std::map<std::string, int64_t>;
//using vocab_t = std::unordered_set<std::string>;
using doc_t = std::vector<hashmap_t>;

namespace tfkld{

struct MSParaFile{
    MSParaFile(std::string train_file) {
        val.open(train_file, std::ifstream::in);

        if(val.fail()) {
            std::cout << "ERROR: training data file not found!\n";
            exit(1);
        }

    }

    void setBegin() {
        val.clear();
        val.seekg(0);
    }
    std::ifstream val;
};
    
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

vocab_t LearnVocab(MSParaFile &file);
    
doc_t LearnPara(vocab_t &vocab, MSParaFile &file);

std::vector<std::string> LearnTag(MSParaFile &file);

vocab_t ReadVocab(std::ifstream &vocab_file);
    
void fillValue(std::vector<SpValue> &values, int64_t &count, vocab_t const &vocab, doc_t const &docs);

float_t val_idf(int64_t D, int_t Dt);
    
void MakeTFIDF(std::vector<float_t> &idf, std::vector<SpValue> &values, int64_t &count, vocab_t const &vocab, doc_t const &docs);

void MakeTFIDF(std::vector<float_t> &idf, std::vector<SpValue> &values);

void MakeTFKLD(std::vector<float_t> &kld, std::vector<std::string> &tag, std::vector<SpValue> &values, int64_t &count, vocab_t const &vocab, doc_t const &docs);

void MakeTFKLD(std::vector<float_t> &kld, std::vector<SpValue> &values);
    
void fillMat(std::vector<SpValue> &values, int64_t &count, vocab_t const &vocab, doc_t const &docs, arma::sp_mat &mat);
    
void PrintVocab(vocab_t &vocab);

std::vector<std::vector<float_t>> makeSimMat(arma::mat const &V);

void normalizeSimMat(std::vector<std::vector<float_t>> &svec);

auto getVocabWord(vocab_t &vocab);

auto getVocabIndex(vocab_t &vocab);

auto Concat(std::vector<std::string> const &words);
    
}//namespace tfkld
