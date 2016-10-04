#pragma once

#include "src/Matrix.h"

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

class Documents {
    vocab_t vocab;
    doc_t docs;
    tag_t LearnTag;
    std::vector<type::float_t> kld;
    std::vector<SpValue> values;
 public:
    int K_dim;
};

 
std::vector<std::string> MakeNGrams(std::vector<std::string> &words, int n);
 
vocab_t LearnVocab(MSParaFile &file);
    
doc_t LearnPairSentence(MSParaFile &file);

tag_t LearnTag(MSParaFile &file);

vocab_t ReadVocab(std::ifstream &vocab_file);
    
void PrintVocab();

std::vector<std::string> getVocabWord();

std::vector<type::int64_t> getVocabIndex();

std::vector<std::vector<type::int64_t>> getDocsIndex();

std::vector<std::vector<int>> getDocsCount(doc_t &docs);
 
}//namespace tfkld
