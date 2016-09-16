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
    
vocab_t LearnVocab(MSParaFile &file);
    
doc_t LearnPara(vocab_t &vocab, MSParaFile &file);

std::vector<std::string> LearnTag(MSParaFile &file);

vocab_t ReadVocab(std::ifstream &vocab_file);
    
void PrintVocab(vocab_t &vocab);

auto getVocabWord(vocab_t &vocab);

auto getVocabIndex(vocab_t &vocab);

auto Concat(std::vector<std::string> const &words);
    
}//namespace tfkld
