#include "src/TFKLD.h"

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
using doc_t = std::vector<hashmap_t>;

namespace tfkld{

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
            auto it = vocab.find(x);
            if(it != vocab.end()) {   
                auto word_idx = it -> second;
                doc[word_idx] += 1;
            }
        }
        docs.push_back(doc);
        doc.clear();

        words = util::string::split(items[4]);
        for(auto x : words) {
            auto it = vocab.find(x);
            if(it != vocab.end()) {   
                auto word_idx = it -> second;
                doc[word_idx] += 1;
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
    
void MakeTFIDF(std::vector<float_t> &idf, std::vector<SpValue> &values, int64_t &count, vocab_t const &vocab, doc_t const &docs) {

    hashmap_t df;
    int64_t D = docs.size();
    
    for(auto x : values) df[x.row] += 1;

    if(df.size() != vocab.size()) {
        std::cout << "Sanity check failed!\n";
        exit(1);
    }
    
    for(auto x : df) idf.push_back(val_idf(D,x.second));

    for(auto &x : values) x.val *= idf[x.row];

}

void MakeTFIDF(std::vector<float_t> &idf, std::vector<SpValue> &values) {

    for(auto &x : values) x.val *= idf[x.row];

}



void MakeTFKLD(std::vector<float_t> &kld, std::vector<std::string> &tag, std::vector<SpValue> &values, int64_t &count, vocab_t const &vocab, doc_t const &docs) {

    float_t ep=0.05;
    float_t p=ep;
    float_t q=ep;
    float_t np=ep;
    float_t nq=ep;
    
    float_t div;
    for(int64_t a = 0; a < vocab.size(); ++a) {
        for(int i = 0; i < tag.size(); ++i) {
            
            auto isin1 = docs[i*2].find(a);
            auto isin2 = docs[i*2+1].find(a);

            if(tag[i] == "-1") {
                if(isin1 != docs[i*2].end() && isin2 != docs[i*2+1].end()) {
                    q++;
                }
                if(isin1 != docs[i*2].end() && isin2 == docs[i*2+1].end()) {
                    nq++;
                }
                if(isin1 == docs[i*2].end() && isin2 != docs[i*2+1].end()) {
                    nq++;
                }
            } else { // tag[i] == 1;
                if(isin1 != docs[i*2].end() && isin2 != docs[i*2+1].end()) {
                    p++;
                }
                if(isin1 != docs[i*2].end() && isin2 == docs[i*2+1].end()) {
                    np++;
                }
                if(isin1 == docs[i*2].end() && isin2 != docs[i*2+1].end()) {
                    np++;
                }
            }
        }

        div = (p/(p+np))*log((p/(p+np))/(q/(q+nq)) + 1e-7) + (np/(p+np))*log((np/(p+np))/(nq/(q+nq)) + 1e-7);        
        kld.push_back(div);
        p = ep;
        q = ep;
        np = ep;
        nq = ep;
        
    }

    for(auto &x : values) x.val *= kld[x.row];
        
}

void MakeTFKLD(std::vector<float_t> &kld, std::vector<SpValue> &values) {

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


void normalizeSimMat(std::vector<std::vector<float_t> > &svec) {

    float_t sum = 0;
    
    for(auto &x : svec) {
        for(auto &y : x) {
            sum += y*y;
        }

        for(auto &y : x) {
            y = y / sqrt(sum);
        }

        sum = 0;
    }
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
