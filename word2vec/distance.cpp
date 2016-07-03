#include <array>
#include <cmath>
#include <iostream>
#include <fstream>
#include <string>

#include "utils.h"

typedef float real;

std::string word_vector_file;

int layer1_size = 100;

std::vector<std::string> word_vector_label;
std::vector<real> word_vector;

const long long vocab_hash_size = 30000000;

std::array<unsigned long long, vocab_hash_size> vocab_hash;

std::vector<double> mine;

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
  inFile.open(word_vector_file, std::ifstream::in | std::ifstream::binary);
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

std::vector<real> GetWordVector(std::string& word) {

  std::vector<real> wordvector;
  int position;
  position = vocab_hash[GetWordHash(word)];
  for(int i = 0; i < layer1_size; i++) {
    wordvector.push_back(word_vector[position*layer1_size + i]);
  }
  return wordvector;
}

std::vector<real> AddVectors(std::string& word1, std::string& word2) {
  std::vector<real> result;
  std::vector<real> wordvec1;
  std::vector<real> wordvec2;

  wordvec1 = GetWordVector(word1);
  wordvec2 = GetWordVector(word2);
  for(int i = 0; i < layer1_size; i++) result.push_back(wordvec1[i] + wordvec2[i]);

  return result;
}

std::vector<real> AddVectors(std::string& word1, std::vector<real> wordvec2) {
  std::vector<real> result;
  std::vector<real> wordvec1;

  wordvec1 = GetWordVector(word1);
  for(int i = 0; i < layer1_size; i++) result.push_back(wordvec1[i] + wordvec2[i]);

  return result;
}

std::vector<real> AddVectors(std::vector<real> wordvec1, std::string& word2) {
  std::vector<real> result;
  std::vector<real> wordvec2;

  wordvec2 = GetWordVector(word2);
  for(int i = 0; i < layer1_size; i++) result.push_back(wordvec1[i] + wordvec2[i]);

  return result;
}

std::vector<real> AddVectors(std::vector<real> wordvec1, std::vector<real> wordvec2) {
  std::vector<real> result;

  for(int i = 0; i < layer1_size; i++) result.push_back(wordvec1[i] + wordvec2[i]);

  return result;
}

std::vector<real> SubtractVectors(std::string& word1, std::string& word2) {
  std::vector<real> result;
  std::vector<real> wordvec1;
  std::vector<real> wordvec2;

  wordvec1 = GetWordVector(word1);
  wordvec2 = GetWordVector(word2);
  for(int i = 0; i < layer1_size; i++) result.push_back(wordvec1[i] - wordvec2[i]);

  return result;
}

std::vector<real> SubtractVectors(std::string& word1, std::vector<real> wordvec2) {
  std::vector<real> result;
  std::vector<real> wordvec1;

  wordvec1 = GetWordVector(word1);
  for(int i = 0; i < layer1_size; i++) result.push_back(wordvec1[i] - wordvec2[i]);

  return result;
}

std::vector<real> SubtractVectors(std::vector<real> wordvec1, std::string& word2) {
  std::vector<real> result;
  std::vector<real> wordvec2;

  wordvec2 = GetWordVector(word2);
  for(int i = 0; i < layer1_size; i++) result.push_back(wordvec1[i] - wordvec2[i]);

  return result;
}

std::vector<real> SubtractVectors(std::vector<real> wordvec1, std::vector<real> wordvec2) {
  std::vector<real> result;
  for(int i = 0; i < layer1_size; i++) result.push_back(wordvec1[i] - wordvec2[i]);

  return result;
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

double cosDistBetweenWords(std::vector<real>& vec1, std::string& str2)
{
  double str1mag = 0, str2mag = 0;
  double dist = 0;

  long long str2pos;

  str2pos = vocab_hash[GetWordHash(str2)];
  
  for(int a = 0; a < layer1_size; a++) {
    str1mag += vec1[a]*vec1[a];
    str2mag += word_vector[layer1_size*str2pos + a]*word_vector[layer1_size*str2pos + a];
  }

  str1mag = sqrt(str1mag);
  str2mag = sqrt(str2mag);
  
  for(int a = 0; a < layer1_size; a++) {
    dist += (vec1[a]/str1mag) * (word_vector[layer1_size*str2pos + a]/str2mag);
  }

  return dist;
}

double cosDistBetweenWords(std::string& str1, std::vector<real>& vec2)
{
  double str1mag = 0, str2mag = 0;
  double dist = 0;

  long long str1pos;

  str1pos = vocab_hash[GetWordHash(str1)];
  
  for(int a = 0; a < layer1_size; a++) {
    str1mag += word_vector[layer1_size*str1pos + a]*word_vector[layer1_size*str1pos + a];
    str2mag += vec2[a]*vec2[a];
  }

  str1mag = sqrt(str1mag);
  str2mag = sqrt(str2mag);
  
  for(int a = 0; a < layer1_size; a++) {
    dist += (word_vector[layer1_size*str1pos + a]/str1mag) * (vec2[a]/str2mag);
  }

  return dist;
}

double cosDistBetweenWords(std::vector<real>& vec1, std::vector<real>& vec2)
{
  double str1mag = 0, str2mag = 0;
  double dist = 0;

  for(int a = 0; a < layer1_size; a++) {
    str1mag += vec1[a]*vec1[a];
    str2mag += vec2[a]*vec2[a];
  }

  str1mag = sqrt(str1mag);
  str2mag = sqrt(str2mag);
  
  for(int a = 0; a < layer1_size; a++) {
    dist += (vec1[a]/str1mag) * (vec2[a]/str2mag);
  }

  return dist;
}

std::vector<std::string> GetSimilarWords(std::string& word, int n) {
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

std::vector<std::string> GetSimilarWords(std::vector<real>& wordvector, int n) {
  std::string top[n];
  int count = 0;
  for(int i = 0; i < word_vector_label.size(); i++) {
      if(count < n) {
	top[count] = word_vector_label[i];
	count++;
      }
      else {
	std::string temp;
	//sort top[n]
	for(int q = 0; q < n - 1; q++) {
	  for(int p = q + 1 ; p < n; p++) {
	    if(cosDistBetweenWords(wordvector, top[q])
	       < cosDistBetweenWords(wordvector, top[p])) {
	      temp = top[q]; top[q] = top[p]; top[p] = temp;
	    }
	  }
	}
	//sort top[n]
	std::vector<real> topn1;
	std::vector<real> topi;
	if(cosDistBetweenWords(wordvector, top[n-1]) < cosDistBetweenWords(wordvector, word_vector_label[i])) {
	  top[n-1] = word_vector_label[i];
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

//main function arguments

void printHelp() {
  std::cout << "c++ distance implementation \n\n";
  std::cout << "Options:\n";
  std::cout << "Parameters for calculating distance:\n";
  std::cout << "\t-word-vector <file>\n";
  std::cout << "\t\tUse word vector stored in <file>\n";
  std::cout << "\nExamples:\n";
  std::cout << "./distance -word-vector data.txt";
  
}

int ArgPos(char *str, int argc, char **argv) {
  int i;
  std::string s_str;
  std::string s_argv[argc];
  
  for(i = 1; i < argc; i++) s_argv[i] = argv[i];
  s_str = str;
  
  for(i = 1; i < argc; i++) if(s_str == s_argv[i]) {
      if(i == argc - 1) {
	std::cout << "Argument missing for " << s_str << std::endl;;
	exit(1);
      }
      return i;
  }
  return -1;
}

void ArgPass(int argc, char **argv) {
  int i;
  if ((i = ArgPos((char *)"-word-vector", argc, argv)) > 0) word_vector_file = argv[i + 1];
}

// main function arguments





int main(int argc, char **argv) {

  if(argc == 1) {
    printHelp();
    return 0;
  }

  ArgPass(argc, argv);
  
  initVocabHash();
  ReadWordVector();
  std::vector<std::string> test_list;
  std::vector<real> test_vector;
  std::string aaa;
  std::string bbb;
  std::string ccc;
  aaa = "king";
  bbb = "man";
  ccc = "woman";
  test_vector = AddVectors(SubtractVectors(aaa,bbb),ccc);
  //testa = GetSimilarWords(aaa,10);
  
  test_list = GetSimilarWords(test_vector, 20);
  //for(int i = 0; i < 10; i++) std::cout << testa[i] << " ";
  for(int i = 0; i < 20; i++) std::cout << test_list[i] << " ";
  return 0;
}
