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

public:
  Documents():
    K_dim(100) {};

    
    vocab_t vocab;
    doc_t docs;
    tag_t tag;
    std::vector<type::real_t> kld;
    std::vector<SpValue> values;
    int K_dim;


std::vector<std::string> MakeNGrams(std::vector<std::string> &words, int n);
 
void LearnVocab(MSParaFile &file);
void LearnPairSentence(MSParaFile &file);
void LearnSentence(MSParaFile &file);
void LearnTag(MSParaFile &file);
void ReadVocab(std::ifstream &vocab_file);
hashmap_t makeSentoDoc(std::string sen);
void PrintVocab();
};

std::vector<std::string> get_raw_sentence(MSParaFile &file);
std::vector<std::string> getVocabWord(vocab_t &vocab);
std::vector<type::int64_t> getVocabIndex(vocab_t &vocab);
std::vector<std::vector<type::int64_t>> getDocsIndex(doc_t &docs);
std::vector<std::vector<int>> getDocsCount(doc_t &docs);

}//namespace tfkld
