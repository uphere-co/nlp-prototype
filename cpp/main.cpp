#include <algorithm>
#include <cmath>
#include <cstdlib>
#include <iostream>
#include <iterator>
#include <fstream>
#include <string>
#include <vector>

#include <stdlib.h>
#include <string.h>

#define MAX_STRING 1000
#define MAX_CODE_LENGTH 40

typedef float real;

struct vocab_word {
  long long cn;
  int *point;
  std::string word, code;
  char codelen;
};

// Constants
int min_count = 5;
long long vocab_hash_size = 30000000;

// Variables
long long vocab_max_size = 1000, vocab_size = 0;
long long train_words = 0;

// Struct
struct vocab_word *vocab;


// Tables
int *vocab_hash = (int *)calloc(vocab_hash_size, sizeof(struct vocab_word)); // HashMap for words. vocab_hash[WORD_HASH] = WORD_HASH





// Begin of Tokenizer

void makeLower(std::string& str) {
  std::transform(str.begin(), str.end(), str.begin(), ::tolower);
}

void remove_punctuations(std::string& str) {
  char punctuations[5] = {'.',',','?',':',';'};
  for(int i=0;i<5;i++)
    str.erase(std::remove(str.begin(),str.end(), punctuations[i]), str.end());
}

void remove_stopwords(std::vector<std::string>& tokens) {
  std::vector<std::string> stopwords {"a","able","about","across","after","all","almost","also","am","among","an","and","any","are","as","at","be","because","been","but","by","can","cannot","could","dear","did","do","does","either","else","ever","every","for","from","get","got","had","has","have","he","her","hers","him","his","how","however","i","if","in","into","is","it","its","just","least","let","like","likely","may","me","might","most","must","my","neither","no","nor","not","of","off","often","on","only","or","other","our","own","rather","said","say","says","she","should","since","so","some","than","that","the","their","them","then","there","these","they","this","tis","to","too","twas","us","wants","was","we","were","what","when","where","which","while","who","whom","why","will","with","would","yet","you","your"}; // 119 stopwords.

  for(std::vector<std::string>::const_iterator iter = stopwords.begin(); iter != stopwords.end(); ++iter) {
    if(std::find(tokens.begin(), tokens.end(), (*iter)) != tokens.end())
      tokens.erase(std::remove(tokens.begin(), tokens.end(), (*iter)), tokens.end());
  
  }

}

void Tokenize(const std::string& str, std::vector<std::string>& tokens, const std::string& delimiters = " ") {
  std::string::size_type lastPos = str.find_first_not_of(delimiters, 0);
  std::string::size_type pos = str.find_first_of(delimiters, lastPos);

  while (std::string::npos != pos || std::string::npos != lastPos) {
    std::string a = str.substr(lastPos, pos-lastPos);
    makeLower(a); // Converting into lowercase characters.
    remove_punctuations(a);
    if(std::find(tokens.begin(), tokens.end(), a) == tokens.end()) // find() returns tokens.end if tarket is not found. 
      tokens.push_back(a);
    lastPos = str.find_first_not_of(delimiters, pos);
    pos = str.find_first_of(delimiters, lastPos);
  }
  remove_stopwords(tokens);

}

// End of Tokenizer





// Begin of HashMap for dictionary


// Reads a single word from a file, assuming space + tab + EOL to be word boundaries
void ReadWord(std::string& word, std::ifstream& inFile) {
  int a = 0, ch;
  while (!inFile.eof()) {
    ch = inFile.get();
    if (ch == 13) continue;
    if ((ch == ' ') || (ch == '\t') || (ch == '\n')) {
      if (a > 0) {
	if (ch == '\n') inFile.unget();
	break;
      }
      if (ch == '\n') {
	word = "</s>";
	return;
      } else continue;
    }
    word += ch;
    a++;
    if (a >= MAX_STRING - 1) a--;   // Truncate too long words
  }
  word += "";
}

// Initiate vocab_hash array to -1, vocab_hash[hash] = position of a word
void initVocabHash() {
  long long a;
  for(a = 0; a < vocab_hash_size; a++) vocab_hash[a] = -1;
}

// Returns hash value of a word
int GetWordHash(std::string& word) {
  unsigned long long a, hash = 0;
  for(a = 0; a < word.length(); a++) hash = hash * 257 + word.at(a);
  hash = hash % vocab_hash_size;
  return hash;
}

// Returns position of a word in the vocabulary; if the word is not found, returns -1
int SearchVocab(std::string& word) {
  unsigned long long hash = GetWordHash(word);
  while(1) {
    if (vocab_hash[hash] == -1) return -1;
    if (word == vocab[vocab_hash[hash]].word) return vocab_hash[hash];
    hash = (hash + 1) % vocab_hash_size;
  }
  return -1;
}

// Reads a word and returns its index in the vocabulary
int ReadWordIndex(std::ifstream& inFile) {
  std::string word;
  ReadWord(word, inFile);
  if(inFile.eof()) return -1;
  return SearchVocab(word);
}

// Adds a word to the vocabulary, and returns an index of a word in the vocabulary
int AddWordToVocab(std::string word) {
  long long hash, length = word.length() + 1;
  if(length > MAX_STRING) length = MAX_STRING;
  vocab[vocab_size].word = (char *)calloc(length, sizeof(char));
  vocab[vocab_size].word = word;
  vocab[vocab_size].cn = 0;
  vocab_size++;
  // Reallocate memory if needed, in other words, increase vocab_max_size if needed
  if(vocab_size + 2 >=vocab_max_size) {
    vocab_max_size += 1000;
    vocab = (struct vocab_word *)realloc(vocab, vocab_max_size * sizeof(struct vocab_word));
  }
  hash = GetWordHash(word);
  while(vocab_hash[hash] != -1) hash = (hash + 1) % vocab_hash_size;
  vocab_hash[hash] = vocab_size - 1;
  return vocab_size -1;
}

// Compares words based on word counts, used later for sorting by word counts
int VocabCompare(const void *a, const void *b) {
  return ((struct vocab_word *)b) -> cn - ((struct vocab_word *)a) -> cn;
}

// Sorts the vocabulary by frequency using word counts
// (Maybe) Assumed that SortVocab() is always excuted right after learning vocab from training file
void SortVocab() {
  long long a, size;
  long long hash;
  // Sort the vocabulary and keep </s> at the first position
  qsort(&vocab[1], vocab_size -1, sizeof(struct vocab_word), VocabCompare);
  for (a = 0; a < vocab_hash_size; a++) vocab_hash[a] = -1; // This initialization is really necessary?
  size = vocab_size;
  train_words = 0;
  for (a = 0; a < size; a++) {
    // Words occuring less than min_count times will be discarded from the vocabulary
    if((vocab[a].cn < min_count) && ( a != 0)) {
      vocab_size--;
    } else {
      // Hash will be re-computed, as after the sorting it is not actual
      hash = GetWordHash(vocab[a].word);
      while(vocab_hash[hash] != -1) hash = (hash + 1) % vocab_hash_size;
      vocab_hash[hash] = a;
      train_words += vocab[a].cn;
    }
  }
  vocab = (struct vocab_word *)realloc(vocab, (vocab_size + 1) * sizeof(struct vocab_word));
  // Allocate memory for the binary tree construction
  for(a = 0; a < vocab_size; a++) {
    vocab[a].code = (char *)calloc(MAX_CODE_LENGTH, sizeof(char));
    vocab[a].point = (int *)calloc(MAX_CODE_LENGTH, sizeof(int));
  }
}


// End of HashMap for dictionary

int main()
{
  char inputString[MAX_STRING];

  real eta = 0.025; // Learning rate ( > 0)
  
  std::vector<std::string> tokens;

  std::ifstream inFile("input.txt");
  std::ofstream outFile("outfile.txt");

  // Initializaiton of Data Structures
  initVocabHash();
  
  while(!inFile.eof()) {
    inFile.getline(inputString,MAX_STRING);
    Tokenize(inputString, tokens);
  }

  for(std::vector<std::string>::size_type i = 0; i < tokens.size(); ++i)
    outFile << tokens[i] << std::endl;
  
  // Feedforwarding
  // Backpropagation
  std::string test = "Hello!";
  unsigned int hash = GetWordHash(test);
  
  std::cout << vocab_hash[hash] << std::endl;
  
  inFile.close();
  outFile.close();

}
