#include <array>
#include <cmath>
#include <iostream>
#include <fstream>
#include <string>

#include "utils.h"

typedef float real;

std::string wordvector_file;

int layer1_size = 100;

std::vector<std::string> word_vector_label;
std::vector<real> word_vector;

const long long vocab_hash_size = 30000000;

std::array<unsigned long long, vocab_hash_size> vocab_hash;

std::vector<double> mine;

int findPosition(std::string& word) {
  for(int i = 0 ; i < word_vector_label.size(); i++) {
    if(word == word_vector_label[i]) return i;
  }
  return -1;
}

void initVocabHash() {
  vocab_hash.fill(-1);
}

int GetWordHash(std::string& word) {
  unsigned long long a;
  unsigned long long hash = 0;
  for(a = 0; a < word.length(); a++) hash = hash * 257 + word.at(a);
  hash = hash % vocab_hash_size;
  return hash;
}


// Begin of Distance Measurement
void ReadWordVector() {
  std::ifstream inFile;
  std::vector<std::string> word;
  std::string line;
  unsigned long long hash;
  long long pos = 0;
  inFile.open(wordvector_file, std::ifstream::in | std::ifstream::binary);
  if(inFile.fail()) {
    std::cout << "Word vector file not found!\n";
    exit(1);
  }
  std::getline(inFile,line); // The first line : ( vocab_size, layer1_size)
  split(line, word);
  layer1_size = atoi(word[1].c_str());
  while(1) {
    word.clear();
    std::getline(inFile,line);
    if(inFile.eof()) break;
    split(line, word);
    word_vector_label.push_back(word[0]);
    hash = GetWordHash(word[0]);
    while(vocab_hash[hash] != -1) hash = (hash + 1) % vocab_hash_size;
    vocab_hash[hash] = pos;
    pos++;
    for(int i = 0; i < layer1_size; i++) {
      word_vector.push_back(atof(word[i+1].c_str()));
    }  
  }

  inFile.close();
}

std::vector<real> getWordVector(std::string& word) {

  std::vector<real> wordvector;
  int position;
  position = vocab_hash[GetWordHash(word)];
  for(int i = 0; i < layer1_size; i++) {
    wordvector.push_back(word_vector[position*layer1_size + i]);
  }
  return wordvector;
}

double cosDistBetweenWords(std::string& str1, std::string& str2)
{
  double str1mag = 0, str2mag = 0;
  double dist = 0;

  long long str1pos, str2pos;

  str1pos = vocab_hash[GetWordHash(str1)];
  str2pos = vocab_hash[GetWordHash(str2)];
  
  for(int a = 0; a < layer1_size; a++) {
    str1mag += word_vector[layer1_size*str1pos + a]*word_vector[layer1_size*str1pos + a];
    str2mag += word_vector[layer1_size*str2pos + a]*word_vector[layer1_size*str2pos + a];
  }

  str1mag = sqrt(str1mag);
  str2mag = sqrt(str2mag);
  
  for(int a = 0; a < layer1_size; a++) {
    dist += (word_vector[layer1_size*str1pos + a]/str1mag) * (word_vector[layer1_size*str2pos + a]/str2mag);
  }

  return dist;
}

std::vector<std::string> GetSimilarWord(std::string& word, int n) {
  std::string top[n];
  int count = 0;
  for(int i = 0; i < word_vector_label.size(); i++) {
    if(word != word_vector_label[i]) {
      if(count < n) {
	top[count] = word_vector_label[i];
	count++;
      }
      else {
	std::string temp;
	//sort top[n]
	for(int q = 0; q < n - 1; q++) {
	  for(int p = q + 1 ; p < n; p++) {
	    if(cosDistBetweenWords(word, top[q])
	       < cosDistBetweenWords(word, top[p])) {
	      temp = top[q]; top[q] = top[p]; top[p] = temp;
	    }
	  }
	}
	if(i % 1000 == 0) {std::cout << i << "  ";fflush(stdout);}
	//sort top[n]
	if(cosDistBetweenWords(word, top[n-1]) < cosDistBetweenWords(word, word_vector_label[i])) {
	  top[n-1] = word_vector_label[i];
	}
      }
    }
    
  }

  std::vector<std::string> result;
  for(int i = 0; i < n; i++) {
    result.push_back(top[i]);
  }

  return result;
  
}
// End of Distance Measurement


int main(int argc, char **argv) {
  initVocabHash();
  wordvector_file = "vec.txt";
  ReadWordVector();
  std::vector<std::string> testa;
  std::string aaa;
  aaa = "witten";
  testa = GetSimilarWord(aaa,10);
  for(int i = 0; i < 10; i++) std::cout << testa[i] << " ";
  return 0;
}
