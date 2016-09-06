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

vocab_t LearnVocab(MSParaFile &file) {
  std::string line;
  vocab_t vocab;
  int64_t word_idx = 0;
  int64_t count = 0;
  std::vector<std::string> items;
  
  std::getline(file.val, line);
  while(std::getline(file.val, line)){
    count++;
    //if(count % 1000 == 0) std::cout << "\r" << count << " lines.";

    std::istringstream iss{line};
    boost::split(items, line, boost::is_any_of("\t"));

    auto words = util::string::split(items[3]);
    for(auto x : words) {
        auto isin = vocab.find(x);
        if(isin == vocab.end()) {
            vocab[x] = word_idx;
            word_idx++;
        }
    }

    words = util::string::split(items[4]);
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
    
doc_t LearnPara(vocab_t &vocab, MSParaFile &file) {
    std::string line;
    doc_t docs;
    hashmap_t doc;
    int64_t count = 0;
    std::vector<std::string> items;

    std::getline(file.val, line);
    while (std::getline(file.val, line)) {
        count++;
        if(count % 1000 == 0) std::cout << "\r" << count << " lines.";
        
        std::istringstream iss{line};
        boost::split(items, line, boost::is_any_of("\t"));
        
        auto words = util::string::split(items[3]);
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

        words = util::string::split(items[4]);
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

std::vector<std::string> LearnTag(MSParaFile &file) {
    std::string line;
    int64_t count = 0;
    std::vector<std::string> items;
    std::vector<std::string> tag;
    
    std::getline(file.val, line);
    while (std::getline(file.val, line)) {
        count++;
        if(count % 1000 == 0) std::cout << "\r" << count << " lines.";
        
        std::istringstream iss{line};
        boost::split(items, line, boost::is_any_of("\t"));

        auto words = items[0];
        std::cout << items[0];
        tag.push_back(words);
    }

    return tag;
}


void fillValue(std::vector<SpValue> &values, int64_t &count, vocab_t const &vocab, doc_t const &docs) {    
    SpValue value;
    for(auto it = docs.begin(); it != docs.end(); ++it) {
        value.reset();
        for(auto itt = it -> begin(); itt != it -> end(); ++itt) {
            value.row = (*itt).first;
            value.col = std::distance(docs.begin(),it);
            value.val = (*itt).second;
            values.push_back(value);
            count++;
        }
    }   
}

float_t val_idf(int64_t D, int_t Dt) {
    return log(D/(float_t)Dt);
}
    
void MakeTFIDF(std::vector<SpValue> &values, int64_t &count, vocab_t const &vocab, doc_t const &docs) {
    hashmap_t df;
    int64_t D = docs.size();
    
    for(auto x : values) {
        auto isin = df.find(x.row);
        if(isin != df.end()) {
            df[x.row] += 1;
        } else {
            df[x.row] = 1;
        }
    }

    std::vector<float_t> idf;
    for(auto x : df) {
        idf.push_back(val_idf(D,x.second));
    }

    for(auto &x : values) {
        x.val *= idf[x.row];
    }

}

void fillMat(std::vector<SpValue> &values, int64_t &count, vocab_t const &vocab, doc_t const &docs, arma::sp_mat &mat) {

    
    arma::umat location(2,count);
    arma::vec value(count);

    for(int64_t a = 0; a < count; a++) {
        location(0,a) = values[a].row;
        location(1,a) = values[a].col;
        value(a) = values[a].val;
    }

    mat = std::move( arma::sp_mat(location, value) );
}
    
void PrintVocab(vocab_t &vocab){
    for(auto x : vocab){
        std::cout << x.first << std::endl;
    }
}

}//namespace tfkld





int main(){
    using namespace tfkld;
    using namespace arma;

    auto timer = Timer{};
    
    std::string fin_name = "msr_paraphrase_train.txt";
    MSParaFile fin{fin_name};

    auto vocab = LearnVocab(fin);
    fin.setBegin();
    auto docs = LearnPara(vocab,fin);
    fin.setBegin();
    auto tag = LearnTag(fin);

    mat U;
    vec s;
    mat V;

    //svds(U,s,V,inMat,100);
    
    return 0;
}
