#include <armadillo>
#include <fstream>
#include <set>
#include <sstream>
#include <string>
#include <algorithm>
#include <map>
#include <iterator>
#include <unordered_set>
#include <utility>

#include "utils/hdf5.h"
#include "utils/string.h"
#include "utils/profiling.h"

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

struct TokenizedFile{
    TokenizedFile(std::string train_file) {
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
    SpValue() {
        row = 0;
        col = 0;
        val = 0.0;
    }

    void reset() {
        row = 0;
        col = 0;
        val = 0.0;
    }
    
    int64_t row;
    int64_t col;
    float_t val;
};

vocab_t LearnVocab(TokenizedFile &file) {
  std::string line;
  vocab_t vocab;
  int64_t word_idx = 0;
  int64_t count = 0;
  while(std::getline(file.val, line)){
    count++;
    if(count % 1000 == 0) std::cout << "\r" << count << " lines.";

    std::istringstream iss{line};
    auto words = util::string::split(line);
    for(auto x : words) {
        auto isin = vocab.find(x);
        if(isin == vocab.end()) {
            vocab[x] = word_idx;
            word_idx++;
        }
    }
  }
  // return sorted vocabulary with ascending order
  return vocab;
}

doc_t LearnDoc(vocab_t &vocab, TokenizedFile &file) {
    std::string line;
    doc_t docs;
    hashmap_t doc;
    int64_t count = 0;
    while (std::getline(file.val, line)) {
        count++;
        if(count % 1000 == 0) std::cout << "\r" << count << " lines.";
        
        std::istringstream iss{line};
        auto words = util::string::split(line);
        for(auto x : words) {
            auto word_idx = vocab.find(x) -> second;
            auto isin = doc.find(word_idx);
            if(isin != doc.end()) {
                doc[word_idx] += 1;
            } else {
                doc[word_idx] = 1;
            }
        }
        docs.push_back(doc);
        doc.clear();
    }

    return docs;
}

std::vector<std::vector<float_t>> makeSim(vocab_t &vocab, doc_t &docs) {
    std::vector<std::vector<float_t>> val;
    for(int64_t a = 0; a < docs.size(); a++) {
        // doc_a + doc_(a+1)
        // |doc_a - doc_(a+1)|
        std::vector<float_t> vec1(vocab.size()+1,0.0);
        std::vector<float_t> vec2(vocab.size()+1,0.0);
        std::vector<float_t> vec(vocab.size()+1,0.0);
        float_t sum = 0;
        for(auto x : docs[a]) {
            vec1[x.first] = x.second;
        }
        for(auto x : docs[a+1]) {
            vec2[x.first] = x.second;
        }

        for(int64_t a = 0; a < vec.size() - 1; a++) {
            vec[a] = vec1[a] + vec2[a];
            sum += abs(vec1[a] - vec2[a]);
        }
        vec[vec.size()-1] = sum;
        
        val.push_back(vec);
    }
    
    return val;

}
    
    
}//namespace tfkld





int main(){
    using namespace tfkld;
    using namespace arma;

    auto timer = Timer{};
    
    std::string train_file = "1M.training";
    TokenizedFile infile{train_file};

    auto vocab = LearnVocab(infile);
    timer.here_then_reset("\nConstructed Vocabulary.\n");
    infile.setBegin();
    auto docs = LearnDoc(vocab, infile);
    timer.here_then_reset("\nConstructed Documents.\n");

    std::vector<std::vector<float_t>> val;
    val = makeSim(vocab,docs);
    timer.here_then_reset("Make similarity.\n");
    /*
    int64_t n_rows, n_cols;

    n_rows = vocab.size();
    n_cols = docs.size();
    
    mat inMat(n_rows+1,n_cols*(n_cols-1)/2);
    */
    
    return 0;
}
