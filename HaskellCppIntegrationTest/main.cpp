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

// #include "utils/hdf5.h"
#include "utils/string.h"
#include "utils/profiling.h"

extern "C" {
    int mymain( int, int*, int*, double* );
    void (*callhaskell)( void );
    void registerfun( void(*)(void) );
}


using namespace util;
// using namespace util::io;

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

auto Concat(std::vector<std::string> const &words){
    std::vector<char> vec;
    for(auto const &x:words){
        std::copy(x.cbegin(),x.cend(),std::back_inserter(vec));
        vec.push_back('\0');
    }
    return vec;
}
auto ToStrings(std::vector<char> const &concat_words){
    std::vector<std::string> words;
    auto it =concat_words.cbegin();
    auto end=concat_words.cend();
    while(it!=end){
        words.push_back(std::string{&(*it)});
        //std::cout<<std::string{&(*it)}<<std::endl;
        it=std::find(it, end, '\0');
        ++it;
    }
    return words;
}
}//namespace tfkld


extern void (*callhaskell)( void ); 
void registerfun( void (*f)(void) ) {
    callhaskell=  f;
};

int mymain( int count,  int* vrow, int* vcol, double* vval ){
    auto timer = Timer{};
    
    arma::umat location(2,count);
    arma::vec value(count);
    arma::sp_mat mat;
    
    for(int64_t a = 0 ; a < count ; a++ ) {
	location(0,a) = vrow[a];
	location(1,a) = vcol[a];
	value(a) = vval[a];
    }
    mat = std::move( arma::sp_mat(location, value) );
    timer.here_then_reset("Move matrix done!\n");

    
    arma::mat U;
    arma::vec s;
    arma::mat V;

    arma::svds(U, s, V, mat, 100);

    timer.here_then_reset("Completed SVD calculation!\n");
    
    std::cout << "Finished!" << std::endl;  

    callhaskell();

    std::cout << "After callhaskell()" << std::endl;
    
    /*    using namespace tfkld;
    using namespace arma;

    auto timer = Timer{};
    
    std::string train_file = "1K.training";
    TokenizedFile infile{train_file};

    auto vocab = LearnVocab(infile);
    timer.here_then_reset("\nConstructed Vocabulary.\n");
    infile.setBegin();
    auto docs = LearnDoc(vocab, infile);
    timer.here_then_reset("\nConstructed Documents.\n");
    
    int64_t n_rows, n_cols;
    n_rows = vocab.size();
    n_cols = docs.size();
    //(n_rows, n_cols)
    sp_mat inMat(n_rows,n_cols);

    //int64_t pos = std::distance(vocab.begin(), vocab.find("that"));
    //std::cout << pos << std::endl;
    
    std::vector<SpValue> values;
    
    int64_t count = 0;    
    fillValue(values, count, vocab, docs);
    MakeTFIDF(values, count, vocab, docs);
    fillMat(values, count, vocab, docs, inMat);

    timer.here_then_reset("Filled the Matrix.\n");
    
    //inMat.print("inMat = ");

    mat U;
    vec s;
    mat V;

    svds(U, s, V, inMat, 100);

    timer.here_then_reset("Completed SVD calculation!\n");
    
    std::cout << "Finished!" << std::endl;  */
    
    return 0;
}
