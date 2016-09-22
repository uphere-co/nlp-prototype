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

std::vector<std::string> MakeNGrams(std::vector<std::string> &words, int n);
 
vocab_t LearnVocab(MSParaFile &file);
    
doc_t LearnPara(vocab_t &vocab, MSParaFile &file);

std::vector<std::string> LearnTag(MSParaFile &file);

vocab_t ReadVocab(std::ifstream &vocab_file);
    
void PrintVocab(vocab_t &vocab);

std::vector<std::string> getVocabWord(vocab_t &vocab);

std::vector<type::int64_t> getVocabIndex(vocab_t &vocab);

std::vector<std::vector<type::int64_t>> getDocsIndex(doc_t &docs);

std::vector<std::vector<int>> getDocsCount(doc_t &docs);
 
}//namespace tfkld
