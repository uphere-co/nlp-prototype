#include "src/Vocab.h"

using namespace util;
using namespace util::io;

using namespace tfkld::type;

namespace tfkld{

std::vector<std::string> MakeNGrams(std::vector<std::string> &words, int n) {
    std::vector<std::string> result;
    std::string word{""};
    if(words.size() < n) {
        std::cout << "ERROR! " << n << "-gram model is not valid here.\n";
        exit(1);
    }

    for(int i = 0; i < words.size() - n + 1; i++) {
        for(int j = 0; j < n; j++) {
            if(j == 0) {
                word = words[i+j];
            } else {
                word = word + " " + words[i+j];
            }
        }
        result.push_back(word);
        word = "";
    }

    return result;
}
    
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

    std::vector<std::string> words = util::string::split(items[3]);
    std::vector<std::string> unigram_words = MakeNGrams(words,1);
    std::vector<std::string> bigram_words = MakeNGrams(words,2);
    
    for(auto x : unigram_words) {
        auto isin = vocab.find(x);
        if(isin == vocab.end()) {
            vocab[x] = word_idx;
            word_idx++;
        }
    }

    for(auto x : bigram_words) {
        auto isin = vocab.find(x);
        if(isin == vocab.end()) {
            vocab[x] = word_idx;
            word_idx++;
        }
    }

    
    words = util::string::split(items[4]);
    unigram_words = MakeNGrams(words,1);
    bigram_words = MakeNGrams(words,2);
    
    for(auto x : unigram_words) {
        auto isin = vocab.find(x);
        if(isin == vocab.end()) {
            vocab[x] = word_idx;
            word_idx++;
        }
    }
    
    for(auto x : bigram_words) {
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
        
        std::vector<std::string> words = util::string::split(items[3]);
        std::vector<std::string> unigram_words = MakeNGrams(words,1);
        std::vector<std::string> bigram_words = MakeNGrams(words,2);
        
        for(auto x : unigram_words) {
            auto it = vocab.find(x);
            if(it != vocab.end()) {   
                auto word_idx = it -> second;
                doc[word_idx] += 1;
            }
        }
        
        for(auto x : bigram_words) {
            auto it = vocab.find(x);
            if(it != vocab.end()) {   
                auto word_idx = it -> second;
                doc[word_idx] += 1;
            }
        }

        docs.push_back(doc);
        doc.clear();

        words = util::string::split(items[4]);
        unigram_words = MakeNGrams(words,1);
        bigram_words = MakeNGrams(words,2);
        
        for(auto x : unigram_words) {
            auto it = vocab.find(x);
            if(it != vocab.end()) {   
                auto word_idx = it -> second;
                doc[word_idx] += 1;
            }
        }
        
        for(auto x : bigram_words) {
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
    
void PrintVocab(vocab_t &vocab){
    for(auto x : vocab) std::cout << x.first << std::endl;
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
    
}//namespace tfkld
