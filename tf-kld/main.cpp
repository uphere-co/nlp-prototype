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

struct docs{
    doc_t docs;
};

vocab_t LearnVocab(TokenizedFile &file) {
  std::string line;
  vocab_t vocab;
  int64_t word_idx = 0;
  while(std::getline(file.val, line)){
    std::istringstream iss{line};
    auto words = util::string::split(line);
    for(auto x : words) {
        auto isin = vocab.find(x);
        if(isin != vocab.end()) {
                continue;
            } else {
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
    while (std::getline(file.val, line)) {
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


void fillMat(vocab_t const &vocab, doc_t const &docs, arma::sp_mat &mat) {
    int64_t row, col;
    int64_t count = 0;
    
    std::vector<int64_t> locations_row;
    std::vector<int64_t> locations_col;
    std::vector<float_t> values;
        
    for(auto it = docs.begin(); it != docs.end(); ++it) {
        for(auto itt = it -> begin(); itt != it -> end(); ++itt) {
            row = (*itt).first;
            col = std::distance(docs.begin(),it);
            locations_row.push_back(row);
            locations_col.push_back(col);
            values.push_back((*itt).second);
            count++;
        }
    }

    arma::umat location(2,count);
    arma::vec value(count);

    for(int64_t a = 0; a < count; a++) {
        location(0,a) = locations_row[a];
        location(1,a) = locations_col[a];
        value(a) = values[a];
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

void testKLD() {
    
}



int main(){
    using namespace tfkld;
    using namespace arma;

    auto timer = Timer{};
    
    std::string train_file = "1b.training";
    TokenizedFile infile{train_file};
    auto vocab = LearnVocab(infile);
    infile.setBegin();
    auto docs = LearnDoc(vocab, infile);

    timer.here_then_reset("Constructed Vocabulary and Documents.\n");
    
    int64_t n_rows, n_cols;
    n_rows = vocab.size();
    n_cols = docs.size();
    //(n_rows, n_cols)
    sp_mat inMat(n_rows,n_cols);

    //int64_t pos = std::distance(vocab.begin(), vocab.find("that"));
    //std::cout << pos << std::endl;
    
    fillMat(vocab, docs, inMat);

    timer.here_then_reset("Filled the Matrix.\n");
    
    //inMat.print("inMat = ");
    std::cout << "Finished!" << std::endl;




    
    //arma::mat inMat = arma::randu<arma::mat>(5,5);



    /*
    sp_mat X = sprandu<sp_mat>(50000, 200000, 0.001);

    mat U;
    vec s;
    mat V;

    svds(U, s, V, X, 200);




    //X.print("X = ");
    //U.print("U = ");
    s.print("S = ");
    //V.print("V = ");
    


    */
    /*
    arma::mat inMat = arma::randu<arma::mat>(5,5);

    arma::mat U;
    arma::vec s;
    arma::mat V;

    arma::svd(U,s,V,inMat);


    inMat.print("inMat = ");                                                                                                                                                  U.print("U = ");                                                                                                                                                          s.print("S = ");                                                                                                                                                          V.print("V = ");
    */


    //PrintVocab(vocab);
    //std::vector<char> concat_words = Concat(word_count_keys);
    
    //H5file file{H5name{"data.h5"}, hdf5::FileMode::replace};
    //file.writeRawData(H5name{"1b.training.1M.count"},word_count_values);
    //file.writeRawData(H5name{"1b.training.1M.word"},concat_words);
    
    //concat_read = file.readRawData(H5name{"bar.word_key"},concat_words); 
    //auto words = ToStrings(concat_read);
    //assert(words=word_count_keys);
    /*

  //fvec column vector                                                                                                                                                     

  arma::fmat inMat;
  arma::fvec v1;
  arma::fvec v2;
  arma::fvec v3;

  v1 = {1, 2, 3};
  v2 = {2, 3, 4};
  v3 = {3, 4, 9};

  std::vector<arma::fvec> v;
  std::vector<arma::fvec> rv;
  v.push_back(v1);
  v.push_back(v2);
  v.push_back(v3);

  rv = makeBasis(v);
  makeNormal(rv);
  for(int i = 0; i < rv.size(); i++) {
    rv[i].print();
  }

  inMat = {{1,2,3},{2,3,4},{3,4,9}};
  std::cout << rank(inMat) << std::endl;

  //arma::fmat U;                                                                                                                                                          
  //arma::fvec S;                                                                                                                                                          
  //arma::fmat V;                                                                                                                                                          


  //arma::svd(U, S, V, inMat);                                                                                                                                             

  //inMat.print("inMat = ");                                                                                                                                               
  //U.print("U = ");                                                                                                                                                       
  //S.print("S = ");                                                                                                                                                       
  //V.print("V = ");                    





     */

    
    return 0;
}
