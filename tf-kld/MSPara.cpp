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

vocab_t LearnVocab(MSParaFile &file) {
  std::string line;
  vocab_t vocab;
  int64_t word_idx = 0;
  int64_t count = 0;
  std::vector<std::string> items;
  
  std::getline(file.val, line);
  while(std::getline(file.val, line)){
    count++;
    if(count % 1000 == 0) std::cout << "\r" << count << " lines.";

    std::istringstream iss{line};
    boost::split(items, line, boost::is_any_of("\t"));

    auto words = util::string::split(items[3]+" "+items[4]);
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
            doc[word_idx] += 1;
        }
        docs.push_back(doc);
        doc.clear();

        words = util::string::split(items[4]);
        for(auto x : words) {
            auto word_idx = vocab.find(x) -> second;
            doc[word_idx] += 1;

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

        if(words == "0") words = "-1";
        
        tag.push_back(words);
    }

    return tag;
}

vocab_t ReadVocab(std::ifstream &vocab_file) {
    std::string word;
    int64_t index;
    vocab_t vocab;
    
    while(vocab_file >> word >> index) {
        vocab[word] = index;
    }

    return vocab;
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
    std::vector<float_t> idf;
    int64_t D = docs.size();
    
    for(auto x : values) df[x.row] += 1;

    for(auto x : df) idf.push_back(val_idf(D,x.second));

    for(auto &x : values) x.val *= idf[x.row];

}

void MakeTFKLD(std::vector<float_t> &kld, std::vector<std::string> &tag, std::vector<SpValue> &values, int64_t &count, vocab_t const &vocab, doc_t const &docs) {

    float_t ep=0.05;
    float_t count_p=2*ep;
    float_t count_q=2*ep;
    float_t p=ep;
    float_t q=ep;

    float_t div;
    for(int64_t a = 0; a < vocab.size(); ++a) {
        for(int i = 0; i < tag.size(); ++i) {
            
            auto isin1 = docs[i*2].find(a);
            auto isin2 = docs[i*2+1].find(a);

            if(tag[i] == "-1") {
                if(isin1 != docs[i*2].end() && isin2 != docs[i*2+1].end()) {
                    q++;
                    count_q++;
                }
                if(isin1 == docs[i*2].end() && isin2 != docs[i*2+1].end()) {
                    count_q++;
                }
            } else { // tag[i] == 1;
                if(isin1 != docs[i*2].end() && isin2 != docs[i*2+1].end()) {
                    p++;
                    count_p++;
                }
                if(isin1 == docs[i*2].end() && isin2 != docs[i*2+1].end()) {
                    count_p++;
                }
            }
        }
        
        div = (p/count_p)*log((p/count_p)/(q/count_q)) + (1-p/count_p)*log((1-p/count_p)/(1-q/count_q));        
        kld.push_back(div);
        count_p = 2*ep;
        count_q = 2*ep;
        p = ep;
        q = ep;
        
    }

    for(auto &x : values) x.val *= kld[x.row];
        
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
    for(auto x : vocab) std::cout << x.first << std::endl;
}

std::vector<std::vector<float_t> > makeSimMat(arma::mat const &V) {

    std::vector<float_t> svec;
    std::vector< std::vector<float_t> > result;
    float_t minus = 0;

    // transpose V
    for(int a = 0; a < V.n_rows; a=a+2) {
        for(int b = 0; b < V.n_cols; b++) {
            svec.push_back(V(a,b) + V(a+1,b));
            minus = pow(V(a,b) - V(a+1,b),2.0);
        }
        svec.push_back(sqrt(minus));

        result.push_back(svec);
        minus = 0;
        svec.clear();
        
    }

    return result;
}


auto getVocabWord(vocab_t &vocab) {
    std::vector<std::string> result;
    for(auto x : vocab) result.push_back(x.first);
    return result;
}

auto getVocabIndex(vocab_t &vocab) {
    std::vector<int64_t> result;
    for(auto x : vocab) result.push_back(x.second);
    return result;
}

auto Concat(std::vector<std::string> const &words){
    std::vector<char> vec;
    for(auto const &x:words){
        std::copy(x.cbegin(),x.cend(),std::back_inserter(vec));
        vec.push_back('\0');
    }
    return vec;
}

    
}//namespace tfkld





int main(){
    
    using namespace tfkld;
    using namespace arma;

    auto timer = Timer{};
    
    std::string fin_name = "Tk_msr_paraphrase_train.txt";
    MSParaFile fin{fin_name};

    auto vocab = LearnVocab(fin);
    timer.here_then_reset("\nConstructed Vocabulary.\n");
    fin.setBegin();
    auto docs = LearnPara(vocab,fin);
    timer.here_then_reset("\nConstructed Paragraphs.\n");
    fin.setBegin();
    auto tag = LearnTag(fin);
    timer.here_then_reset("\nConstructed Tag.\n");
    
    int64_t n_rows, n_cols;
    n_rows = vocab.size();
    n_cols = docs.size();

    sp_mat inMat(n_rows, n_cols);

    std::vector<SpValue> values;
    std::vector<float_t> kld;
    int64_t count = 0;

    fillValue(values, count, vocab, docs);
    //MakeTFIDF(values, count, vocab, docs);
    MakeTFKLD(kld, tag, values, count, vocab, docs);
    fillMat(values, count, vocab, docs, inMat);

    timer.here_then_reset("Filled the Matrix.\n");
    
    mat U;
    vec s;
    mat V;

    svds(U,s,V,inMat,100);

    timer.here_then_reset("SVD is complete.\n");

    auto svec = makeSimMat(V);

    timer.here_then_reset("Made Similarity Vectors.\n");
    
    std::ofstream fout{"train_KLD.dat"};

    count = 0;
    int lcount = 1;
    for(auto x : svec) {
        fout << tag[count] << " ";
        for(auto y : x) {
            fout << lcount << ":" << y << " ";
            lcount++;
        }
        fout << "\n";
        lcount = 1;
        count++;
    }

    timer.here_then_reset("Writing Similarity Vectors is Done.\n");

    std::string vocab_filename = "vocab.dat";
    std::ofstream vocab_out{vocab_filename};

    std::string kld_filename = "kld.dat";
    std::ofstream kld_out{kld_filename};
    
    for(auto x : vocab) {
        vocab_out << x.first << " " << x.second << std::endl;
    }

    for(auto x : kld) {
        kld_out << x << std::endl;
    }

    
    /*
    H5file file{H5name{"data.h5"}, hdf5::FileMode::create};
    auto vocab_word = getVocabWord(vocab);
    auto vocab_index = getVocabIndex(vocab);
    std::vector<char> concat_words = Concat(vocab_word);
    file.writeRawData(H5name{"MSR.training.vocab.word"},concat_words);
    file.writeRawData(H5name{"MSR.training.vocab.index"},vocab_index);
    */

    
    

    vocab_out.close();
    kld_out.close();

    std::string vocabread_filename = "vocab.dat";
    std::ifstream vocabread_in{vocabread_filename};

    auto vocab2 = ReadVocab(vocabread_in);

    return 0;
}
