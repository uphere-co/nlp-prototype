#include <array>
#include <cmath>
#include <cstdlib>
#include <ctime>
#include <iostream>
#include <fstream>

#include <boost/tuple/tuple.hpp>

#include <stdlib.h>
#include <string.h>

#include "gnuplot-iostream.h"

#include "utils.h"

#define EXP_TABLE_SIZE 1000
#define MAX_EXP 6
#define MAX_STRING 1000
#define MAX_CODE_LENGTH 40
#define MAX_SENTENCE_LENGTH 1000

typedef float real;

struct vocab_word {
  long long cn;
  std::vector<int> point;
  std::string word, code;
  char codelen;
};

// Constants
const int table_size = 1e8;

int layer1_size = 100;
int hs = 0, negative = 5;
int cbow = 0, binary = 0, min_count = 5, min_reduce = 1, debug_mode = 2, window = 5;
int num_threads = 12;
const long long vocab_hash_size = 30000000;

// Variables
long long vocab_size = 0;
long long train_words = 0, word_count_actual = 0, file_size = 0, iter = 5, classes = 0;//, iter = 5; Threaded

real alpha = 0.025, starting_alpha, sample = 1e-3; // 1e-5;

std::array<real,(EXP_TABLE_SIZE + 1)> expTable;

clock_t start;


std::vector<real> syn0;
std::vector<real> syn1;
std::vector<real> syn1neg;


std::string train_file, output_file;
std::string save_vocab_file = "", read_vocab_file = "";

// Struct
std::vector<vocab_word> vocab;

// Tables
std::array<unsigned long long, vocab_hash_size> vocab_hash; // HashMap for words. vocab_hash[WORD_HASH] = WORD_POSITION
std::array<int, table_size> table;

std::vector<double> mine;






// Begin of HashMap for dictionary


// Reads a single word from a file, assuming space + tab + EOL to be word boundaries
void ReadWord(std::string& word, std::ifstream& inFile) {
  int a = 0, ch;
  word.clear();
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
  }
}

// Initiate vocab_hash array to -1, vocab_hash[hash] = position of a word
void initVocabHash() {
  vocab_hash.fill(-1);
}

// Returns hash value of a word
int GetWordHash(std::string& word) {
  unsigned long long a;
  unsigned long long hash = 0;
  for(a = 0; a < word.length(); a++) hash = hash * 257 + word.at(a);
  hash = hash % vocab_hash_size;
  return hash;
}

// Returns position of a word in the vocabulary; if the word is not found, returns -1
int SearchVocab(std::string& word) {
  unsigned long long hash = GetWordHash(word);
  while(1) {
    if(vocab_hash[hash] == -1) return -1;
    if(word == vocab[vocab_hash[hash]].word) return vocab_hash[hash];
    hash = (hash + 1) % vocab_hash_size;
  }
  return -1;
}

// Reads a word and returns its index in the vocabulary
int ReadWordIndex (std::ifstream& inFile) {
  std::string word;
  ReadWord(word, inFile);
  if(inFile.eof()) return -1;
  return SearchVocab(word);
}

// Adds a word to the vocabulary, and returns an index of a word in the vocabulary
int AddWordToVocab(std::string word) {
  unsigned long long hash;
  vocab_word s_word;

  s_word.word = word;
  s_word.cn = 0;

  vocab.push_back(s_word);
  vocab_size++;

  hash = GetWordHash(word);
  while(vocab_hash[hash] != -1) hash = (hash + 1) % vocab_hash_size;
  vocab_hash[hash] = vocab_size - 1;
  return vocab_size - 1;
}


// Compares words based on word counts, used later for sorting by word counts
bool VocabFreqCompare(const vocab_word &a, const vocab_word &b) {
  return (a.cn > b.cn);
}

// Sorts the vocabulary by frequency using word counts
// Assumed that SortVocab() is always excuted right after learning vocab from training file
void SortVocab() {
  long long a, size;
  unsigned long long hash;
  // Sort the vocabulary and keep </s> at the first position
  std::sort(vocab.begin()+1, vocab.end(), VocabFreqCompare);

  initVocabHash(); 

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
      train_words += vocab[a].cn; // Total number of words. Same words are counted many times.
    }
  }
  // Allocate memory for the binary tree construction
  for(a = 0; a < vocab_size; a++)
    vocab[a].point.reserve(MAX_CODE_LENGTH);
  
}

// Reduces the vocabulary by removing infrequent tokens. Neutralized by min_reduce = 0
void ReduceVocab() {
  long long a, b = 0;
  unsigned long long hash;
  for(a = 0; a < vocab_size; a++) if (vocab[a].cn > min_reduce) {
      vocab[b].cn = vocab[a].cn;
      vocab[b].word = vocab[a].word;
      b++;
  }
  vocab_size = b;
  // Hash will be re-computed, as it is not actual
  initVocabHash(); // Re-initialization
  for(a = 0; a < vocab_size; a++) {
    hash = GetWordHash(vocab[a].word);
    while(vocab_hash[hash] != -1) hash = (hash + 1) % vocab_hash_size;
    vocab_hash[hash] = a; // The value is the index(position) of the word
  }
  fflush(stdout); // print first
  min_reduce++; // As ReduceVocab() is called, more words will be removed in the vocabulary
}


// Create binary Huffman tree using the word counts
// Frequent words will have short unique binary codes
void CreateBinaryTree() {
  // Words are not sorted at the beginning
  long long a, b, i, min1i, min2i, pos1, pos2;
  long long point[MAX_CODE_LENGTH];
  char code[MAX_CODE_LENGTH];

  std::vector<long long> count;
  std::vector<long long> binary;
  std::vector<long long> parent_node;

  count.reserve(vocab_size * 2 + 1);
  binary.reserve(vocab_size * 2 + 1);
  parent_node.reserve(vocab_size * 2 +1);
  
  for(a = 0; a < vocab_size; a++) count[a] = vocab[a].cn; // count word frequency
  for(a = vocab_size; a < 2 * vocab_size; a++) count[a] = 1e15; // should be larger than any number in the vocabulary count 
  pos1 = vocab_size - 1;
  pos2 = vocab_size; //

  // following algorithm constructs the Huffman tree by adding one node at a time
  for(a = 0; a < vocab_size - 1; a++) {
    // First, find two smallest nodes 'min1, min2'
    if(pos1 >= 0) {
      if(count[pos1] < count[pos2]) {
	min1i = pos1;
	pos1--;
      } else {
	min1i = pos2;
	pos2++;
      }
    } else {
      min1i = pos2;
      pos2++;
    }
    if(pos1 >= 0) {
      if(count[pos1] < count[pos2]) {
	min2i = pos1;
	pos1--;
      } else {
	min2i = pos2;
	pos2++;
      }
    } else {
      min2i = pos2;
      pos2++;
    }
    count[vocab_size + a] = count[min1i] + count[min2i]; // Creating parent node with summed frequency
    parent_node[min1i] = vocab_size + a;
    parent_node[min2i] = vocab_size + a; // Note that having a common parent node
    binary[min2i] = 1; // min2i will point to root node at the final state
  }
  // Now assign binary code to each vocabulary word
  for(a = 0; a < vocab_size; a++) {
    b = a;
    i = 0;
    while(1) {
      code[i] = binary[b];
      point[i] = b;
      i++;
      b = parent_node[b];
      if(b == vocab_size * 2 - 2) break;
    }
    vocab[a].codelen = i;
    vocab[a].point[0] = vocab_size - 2;
    for(b = 0; b < i; b++) {
      vocab[a].code[i - b - 1] = code[b];
      vocab[a].point[i - b] = point[b] - vocab_size;
    }
  }
}



void LearnVocabFromTrainFile() {
  std::string word;
  std::ifstream inFile;
  long long a, i;

  initVocabHash(); // Initialization
  
  inFile.open(train_file, std::ifstream::in | std::ifstream::binary );

  if(inFile.fail()) {
    std::cout << "ERROR: training data file not found!\n";
    exit(1);
  }
  vocab_size = 0;
  AddWordToVocab("</s>");
  
  while(1) {
    ReadWord(word, inFile);
    if(inFile.eof()) break;
    train_words++;
    if((debug_mode > 1) && (train_words % 100000 == 0)) {
      printf("%lldK%c", train_words / 1000, 13);
      fflush(stdout);
    }
    i = SearchVocab(word);
    if(i == -1) {
      a = AddWordToVocab(word);
      vocab[a].cn = 1;
    } else vocab[i].cn++;
    if(vocab_size > vocab_hash_size * 0.7) ReduceVocab(); // Limiting vocabulary size
  }
  SortVocab(); // Necessary before making Huffman tree
    if(debug_mode > 0) {
      printf("Vocab size: %lld\n", vocab_size);
      printf("Words in the train file: %lld\n", train_words);
    }
    file_size = inFile.tellg();
    inFile.close();
}


void SaveVocab() {
  long long i;
  std::ofstream outFile;
  outFile.open(save_vocab_file, std::ofstream::out | std::ofstream::binary);
  for(i = 0; i < vocab_size; i++) {
    outFile << vocab[i].word << " ";
    outFile << vocab[i].cn << "\n";
  }
  outFile.close();
}

void ReadVocab() {
  long long a, i = 0;
  std::string line;
  std::vector<std::string> word;
  std::ifstream inFile;
  inFile.open(read_vocab_file, std::ifstream::in);// | std::ifstream::binary);
  if(inFile.fail()) {
    std::cout << "Vocabulary file not found!\n";
    exit(1);
  }
  initVocabHash();
  vocab_size = 0;
  while(1) {
    word.clear();
    std::getline(inFile,line);
    if(inFile.eof()) break;
    split(line, word);
    a = AddWordToVocab(word[0]);
    std::cout << word[0] << " ";
    std::cout << word[1] << "\n";
    vocab[a].cn = atof(word[1].c_str());
  }
  SortVocab();
  if(debug_mode > 0) {
    std::cout << "Vocab size : " << vocab_size << std::endl;
    std::cout << "Words in train file : " << train_words << std::endl;
  }
  inFile.close();
  inFile.open(train_file, std::ifstream::in | std::ifstream::binary);
  if(inFile.fail()) {
    std::cout << "Training data file not found!\n";
    exit(1);
  }
  
  file_size = inFile.tellg();
  inFile.close();
}


// End of HashMap for dictionary


// Begin of Learning Net

void InitUnigramTable() {
  long long a, i;
  double train_words_pow = 0;
  double d1, power = 0.75;
  for (a = 0; a < vocab_size; a++) train_words_pow += pow(vocab[a].cn, power);
  i = 0;
  d1 = pow(vocab[i].cn, power) / train_words_pow;
  for (a = 0; a < table_size; a++) {
    table[a] = i;
    if (a / (double)table_size > d1) {
      i++;
      d1 += pow(vocab[i].cn, power) / train_words_pow;
    }
    if (i >= vocab_size) i = vocab_size - 1;
  }
}


void InitNet() {
  long long a, b;

  // Precompute the exp() and f(x) = x / (x + 1) tables
  for (int i = 0; i < EXP_TABLE_SIZE; i++) {
    expTable[i] = exp((i / (real)EXP_TABLE_SIZE * 2 - 1) * MAX_EXP);
    expTable[i] = expTable[i] / (expTable[i] + 1);
  }
  
  
  syn0.reserve((long long)vocab_size * layer1_size);
  if(hs) {
    syn1.reserve((long long)vocab_size * layer1_size);
    for(a = 0; a < vocab_size; a++)
      for(b = 0; b < layer1_size; b++)
	syn1[a * layer1_size + b] = 0;
  }
  if(negative > 0) {
    syn1neg.reserve((long long)vocab_size * layer1_size);
    for(a = 0; a < vocab_size; a++)
      for(b = 0; b < layer1_size; b++)
	syn1neg[a * layer1_size + b] = 0;
  }
  
  for(a = 0; a < vocab_size; a++)
    for(b = 0; b < layer1_size; b++)
      syn0[a * layer1_size + b] = (rand()/(double)RAND_MAX - 0.5) / layer1_size;

  CreateBinaryTree(); // Why this position in the code?

}

void TrainModelThread(){
  long long a, b, d, cw, word, last_word, sentence_length = 0, sentence_position = 0;
  long long word_count = 0, last_word_count = 0, sen[MAX_SENTENCE_LENGTH + 1];
  long long l1, l2, c, target, label, local_iter = iter;
  unsigned long long next_random;
  real f, g;
  clock_t now;

  std::vector<real> neu1;
  std::vector<real> neu1e;

  neu1.reserve(layer1_size);
  neu1e.reserve(layer1_size);
  
  std::ifstream inFile(train_file, std::ifstream::in | std::ifstream::binary);
  while(1) {
    if(word_count - last_word_count > 10000) {
      word_count_actual += word_count - last_word_count;
      last_word_count = word_count;
      if((debug_mode > 1)) {
	now = clock();
	printf("%cAlpha: %f  Progress: %.2f%%  Words/thread/sec: %.2fk   ", 13, alpha,
	       word_count_actual / (real)(iter * train_words + 1) * 100,
	       word_count_actual / ((real)(now - start + 1) / (real)CLOCKS_PER_SEC * 1000));
	fflush(stdout);
      }
      alpha = starting_alpha * (1 - word_count_actual / (real)(iter * train_words + 1));
      if(alpha < starting_alpha * 0.0001) alpha = starting_alpha * 0.0001;
    }
    if(sentence_length == 0) {
      while(1) {
	word = ReadWordIndex(inFile);
	if(inFile.eof()) break;
	if(word == -1) continue;
	word_count++;
	if(word == 0) break;
	// The subsampling randomly discards frequent words while keeping the ranking same
	if(sample > 0) {
	  real ran = (sqrt(vocab[word].cn / (sample * train_words)) + 1) * (sample * train_words) / vocab[word].cn;
	  next_random = rand();
	  if(ran < (next_random / (real)RAND_MAX)) continue;
	}
	sen[sentence_length] = word;
	sentence_length++;
	if(sentence_length >= MAX_SENTENCE_LENGTH) break;
      }
      sentence_position = 0;
    }
    if(inFile.eof() || (word_count > train_words )) { /// num_threads)) {
    word_count_actual += word_count - last_word_count;
    local_iter--;
    if(local_iter == 0) break;
    word_count = 0;
    last_word_count = 0;
    sentence_length = 0;
    //inFile.seekg(file_size / (long long)num_threads * (long long)id, SEEK_SET);
    continue;
    }
    word = sen[sentence_position];
    if(word == -1) continue;
    // Network Initialization
    for(c = 0; c < layer1_size; c++) neu1[c] = 0;
    for(c = 0; c < layer1_size; c++) neu1e[c] = 0;
    next_random = rand();
    b = next_random % window;
    // Skip-gram
    {
      for(a = b; a < window * 2 + 1 - b; a++) if(a != window) {
	  c = sentence_position - window + a;
	  if(c < 0) continue;
	  if(c >= sentence_length) continue;
	  last_word = sen[c];
	  if(last_word == -1) continue;
	  l1 = last_word * layer1_size;
	  // Hierarchical Softmax
	  if(hs) for(d = 0; d < vocab[word].codelen; d++) {
	      f = 0;
	      l2 = vocab[word].point[d] * layer1_size;
	      // Propagate hidden -> output
	      for(c = 0; c < layer1_size; c++) f += syn0[c + l1] * syn1[c + l2];
	      if(f <= -MAX_EXP) continue;
	      else if(f >= MAX_EXP) continue;
	      else f = expTable[(int)((f + MAX_EXP) * (EXP_TABLE_SIZE / MAX_EXP / 2))];
	      // g is the gradient multiplied by the learning rate
	      g = (1 - vocab[word].code[d] - f) * alpha;
	      // Backpropagate errors output -> hidden
	      for(c = 0; c < layer1_size; c++) neu1e[c] += g * syn1[c + l2];
	      // Learn weights hidden -> output
	      for(c = 0; c < layer1_size; c++) syn1[c + l2] += g * syn0[c +l1];
	    }
	  // Negative Sampling
	  if(negative > 0) for (d = 0; d < negative + 1 ; d++) {
	      if(d == 0) {
		target = word;
		label = 1;
	      } else {
		next_random = rand();
		target = table[next_random % table_size];
		if(target == 0) target = next_random % (vocab_size - 1) + 1;
		if(target == word) continue;
		label = 0;
	      }
	      l2 = target * layer1_size;
	      f = 0;
	      for(c = 0; c < layer1_size; c++) f += syn0[c + l1] * syn1neg[c + l2];
	      if(f > MAX_EXP) g = (label - 1) * alpha;
	      else if(f < -MAX_EXP) g = (label - 0) * alpha;
	      else g = (label - expTable[(int)((f + MAX_EXP) * (EXP_TABLE_SIZE / MAX_EXP / 2))]) * alpha;
	      for (c = 0; c < layer1_size; c++) neu1e[c] += g * syn1neg[c + l2];
	      for (c = 0; c < layer1_size; c++) syn1neg[c + l2] += g * syn0[c + l1];
	    }
	  // Learn weights input -> hidden
	  for(c = 0; c < layer1_size; c++) syn0[c + l1] += neu1e[c];
	}
    }
    sentence_position++;
    if(sentence_position >= sentence_length) {
      sentence_length = 0;
      continue;
    }
  }
  inFile.close();
}

void TrainModel() {
  long a, b, c, d;
  std::ofstream outFile;
  std::cout << "Starting training using file " << train_file << std::endl;
  starting_alpha = alpha;
  if(read_vocab_file != "") ReadVocab(); else LearnVocabFromTrainFile();
  if(save_vocab_file != "") SaveVocab();
  //LearnVocabFromTrainFile();
  if(output_file[0] == 0) return;
  InitNet();
  if(negative > 0) InitUnigramTable();
  start = clock();
  TrainModelThread();
  outFile.open(output_file, std::ofstream::out | std::ofstream::binary);
  if(classes == 0) {
    outFile << vocab_size << " " << layer1_size << std::endl;
    for(a = 0; a < vocab_size; a++) {
      outFile << vocab[a].word << " ";
      if(binary) for(b = 0; b < layer1_size; b++) outFile << syn0[a * layer1_size + b] << " ";
      else for(b = 0; b < layer1_size; b++) outFile << syn0[a * layer1_size + b] << " ";
      outFile << std::endl;
    }
  } else {
        // Run K-means on the word vectors
    int clcn = classes, iter = 10, closeid;
    real closev, x;
    
    std::vector<int> centcn, cl;
    std::vector<real> cent;

    centcn.reserve(classes);
    cl.reserve(vocab_size);
    cent.reserve(classes * layer1_size);
    
    for (a = 0; a < vocab_size; a++) cl[a] = a % clcn;
    for (a = 0; a < iter; a++) {
      for (b = 0; b < clcn * layer1_size; b++) cent[b] = 0;
      for (b = 0; b < clcn; b++) centcn[b] = 1;
      for (c = 0; c < vocab_size; c++) {
        for (d = 0; d < layer1_size; d++) cent[layer1_size * cl[c] + d] += syn0[c * layer1_size + d];
        centcn[cl[c]]++;
      }
      for (b = 0; b < clcn; b++) {
        closev = 0;
        for (c = 0; c < layer1_size; c++) {
          cent[layer1_size * b + c] /= centcn[b];
          closev += cent[layer1_size * b + c] * cent[layer1_size * b + c];
        }
        closev = sqrt(closev);
        for (c = 0; c < layer1_size; c++) cent[layer1_size * b + c] /= closev;
      }
      for (c = 0; c < vocab_size; c++) {
        closev = -10;
        closeid = 0;
        for (d = 0; d < clcn; d++) {
          x = 0;
          for (b = 0; b < layer1_size; b++) x += cent[layer1_size * d + b] * syn0[c * layer1_size + b];
          if (x > closev) {
            closev = x;
            closeid = d;
          }
        }
        cl[c] = closeid;
      }
    }
    // Save the K-means classes
    for (a = 0; a < vocab_size; a++) outFile << vocab[a].word << "    " << cl[a] << std::endl;

  }
  outFile.close();
}

// End of Learning Net


// Begin of Distance Measurement

double cosDistBetweenWords(std::string& str1, std::string& str2)
{
  double str1mag = 0, str2mag = 0;
  double dist = 0;

  long long str1pos, str2pos;

  str1pos = vocab_hash[GetWordHash(str1)];
  str2pos = vocab_hash[GetWordHash(str2)];
  
  for(int a = 0; a < layer1_size; a++) {
    str1mag += mine[layer1_size*str1pos + a]*mine[layer1_size*str1pos + a];
    str2mag += mine[layer1_size*str2pos + a]*mine[layer1_size*str2pos + a];
  }

  str1mag = sqrt(str1mag);
  str2mag = sqrt(str2mag);
  
  for(int a = 0; a < layer1_size; a++) {
    dist += (mine[layer1_size*str1pos + a]/str1mag) * (mine[layer1_size*str2pos + a]/str2mag);
  }
}

// End of Distance Measurement




// main function arguments

void printHelp() {
  std::cout << "c++ word2vector implementation \n\n";
  std::cout << "Options:\n";
  std::cout << "Parameters for training:\n";
  std::cout << "\t-train <file>\n";
  std::cout << "\t\tUse text data from <file> to train the model\n";
  std::cout << "\t-output <file>\n";
  std::cout << "\t\tUse <file> to save the resulting word vectors / word clusters\n";
  std::cout << "\t-size <int>\n";
  std::cout << "\t\tSet size of word vectors; default is 100\n";
  std::cout << "\t-window <int>\n";
  std::cout << "\t\tSet max skip length between words; default is 5\n";
  std::cout << "\t-sample <float>\n";
  std::cout << "\t\tSet threshold for occurrence of words. Those that appear with higher frequency in the training data\n";
  std::cout << "\t\twill be randomly down-sampled; default is 1e-3, useful range is (0, 1e-5)\n";
  std::cout << "\t-hs <int>\n";
  std::cout << "\t\tUse Hierarchical Softmax; default is 0 (not used)\n";
  std::cout << "\t-negative <int>\n";
  std::cout << "\t\tNumber of negative examples; default is 5, common values are 3 - 10 (0 = not used)\n";
  std::cout << "\t-threads <int>\n";
  std::cout << "\t\tUse <int> threads (default 12)\n";
  std::cout << "\t-iter <int>\n";
  std::cout << "\t\tRun more training iterations (default 5)\n";
  std::cout << "\t-min-count <int>\n";
  std::cout << "\t\tThis will discard words that appear less than <int> times; default is 5\n";
  std::cout << "\t-alpha <float>\n";
  std::cout << "\t\tSet the starting learning rate; default is 0.025 for skip-gram and 0.05 for CBOW\n";
  std::cout << "\t-classes <int>\n";
  std::cout << "\t\tOutput word classes rather than word vectors; default number of classes is 0 (vectors are written)\n";
  std::cout << "\t-debug <int>\n";
  std::cout << "\t\tSet the debug mode (default = 2 = more info during training)\n";
  std::cout << "\t-binary <int>\n";
  std::cout << "\t\tSave the resulting vectors in binary moded; default is 0 (off)\n";
  std::cout << "\t-save-vocab <file>\n";
  std::cout << "\t\tThe vocabulary will be saved to <file>\n";
  std::cout << "\t-read-vocab <file>\n";
  std::cout << "\t\tThe vocabulary will be read from <file>, not constructed from the training data\n";
  std::cout << "\t-cbow <int>\n";
  std::cout << "\t\tUse the continuous bag of words model; default is 1 (use 0 for skip-gram model)\n";
  std::cout << "\nExamples:\n";
  std::cout << "./word2vec -train data.txt -output vec.txt -size 200 -window 5 -sample 1e-4 -negative 5 -hs 0 -binary 0 -cbow 1 -iter 3\n\n";
  
}

int ArgPos(char *str, int argc, char **argv) {
  int i;
  std::string s_str;
  std::string s_argv[argc];
  
  for(i = 1; i < argc; i++) s_argv[i] = argv[i];
  s_str = str;
  
  for(i = 1; i < argc; i++) if(s_str == s_argv[i]) {
      if(i == argc - 1) {
	std::cout << "Argument missing for " << s_str << std::endl;
	exit(1);
      }
      return i;
  }
  return -1;
}

void ArgPass(int argc, char **argv) {
  int i;
  
  if ((i = ArgPos((char *)"-size", argc, argv)) > 0) layer1_size = atoi(argv[i + 1]);
  if ((i = ArgPos((char *)"-train", argc, argv)) > 0) train_file = argv[i + 1];
  if ((i = ArgPos((char *)"-save-vocab", argc, argv)) > 0) save_vocab_file = argv[i + 1];
  if ((i = ArgPos((char *)"-read-vocab", argc, argv)) > 0) read_vocab_file = argv[i + 1];
  if ((i = ArgPos((char *)"-debug", argc, argv)) > 0) debug_mode = atoi(argv[i + 1]);
  if ((i = ArgPos((char *)"-binary", argc, argv)) > 0) binary = atoi(argv[i + 1]);
  if ((i = ArgPos((char *)"-cbow", argc, argv)) > 0) cbow = atoi(argv[i + 1]);
  if (cbow) alpha = 0.05;
  if ((i = ArgPos((char *)"-alpha", argc, argv)) > 0) alpha = atof(argv[i + 1]);
  if ((i = ArgPos((char *)"-output", argc, argv)) > 0) output_file = argv[i + 1];
  if ((i = ArgPos((char *)"-window", argc, argv)) > 0) window = atoi(argv[i + 1]);
  if ((i = ArgPos((char *)"-sample", argc, argv)) > 0) sample = atof(argv[i + 1]);
  if ((i = ArgPos((char *)"-hs", argc, argv)) > 0) hs = atoi(argv[i + 1]);
  if ((i = ArgPos((char *)"-negative", argc, argv)) > 0) negative = atoi(argv[i + 1]);
  if ((i = ArgPos((char *)"-threads", argc, argv)) > 0) num_threads = atoi(argv[i + 1]);
  if ((i = ArgPos((char *)"-iter", argc, argv)) > 0) iter = atoi(argv[i + 1]);
  if ((i = ArgPos((char *)"-min-count", argc, argv)) > 0) min_count = atoi(argv[i + 1]);
  if ((i = ArgPos((char *)"-classes", argc, argv)) > 0) classes = atoi(argv[i + 1]);
}

// main function arguments

void ReadValue(std::string& word, std::ifstream& fin) {
  int a = 0, ch;
  word.clear();
  while(!fin.eof()) {
    ch = fin.get();
    if( ch == ' ' || ch == '\n') {
      if(a > 0) {
	if(ch == '\n') fin.unget();
	break;
      } else continue;
    }
    word += ch;
    a++;
  }
}

int main(int argc, char **argv) {

  // Seeding random function
  std::srand((unsigned int)time(NULL)); // This is enought for subroutines

  if(argc == 1) {
    printHelp();
    return 0;
  }

  ArgPass(argc, argv);

  
  TrainModel();




  /*
  long long a, b, c;
  
  vocab_size = 71291;
  std::ifstream fin;
  std::string value;
  fin.open("vectors.bin", std::ifstream::in);
  std::vector<std::string> my;
  std::vector<std::string> smine;

  my.reserve(vocab_size);
  mine.reserve(vocab_size*layer1_size);
  smine.reserve(vocab_size*layer1_size);
  int number = 0;
  while(!fin.eof()) {
      ReadValue(value, fin);
      my.push_back(value);

      for(number = 0; number < 100; number++) {
	ReadValue(value, fin);
	smine.push_back(value);
      }
  }


  for(a = 0; a < vocab_size*layer1_size; a++) {
    mine[a] = std::atof(smine[a].c_str());
  }
  std::string qword;
  std::vector< std::pair<std::string, double> > distM;
  int p = 0;
  double dist;

  fin.close();
  
  while(1) {
    std::cout << "Enter word (EXIT to break): ";
    std::cin >> qword;

    if(qword == "EXIT") {
      std::cout << "Finishing the program!\n";
      break;
    }
    
    p = 0;
    distM.clear();
    
    for(a = 0; a < vocab_size; a++) {
      if(qword != my[a]) {
	dist = cosDistBetweenWords(qword, my[a]);
	//dist = sqrt(dist);
	if(p<50) {
	  distM.push_back(make_pair(my[a], dist));
	  p++;
	  continue;
	}
	if(p>=50) {
	  double smallest = 0;
	  int smallposition = 0;
	  for(int w = 0; w < p; w++) {
	    if(w == 0) {
	      smallest = distM[w].second;
	      smallposition = 0;
	    }
	    if(w != 0) {
	      if(distM[w].second < smallest) {
		smallest = distM[w].second;
		smallposition = w;
	      }
	    }
	  }
	  
	  if(smallest < dist) {
	    distM.erase(distM.begin() + smallposition);
	    distM.push_back(make_pair(my[a], dist));  
	  }
	}
      }
    }

    // Print
    for(int w = 0; w < 50; w++) {
      std::cout << distM[w].first << "    " << distM[w].second << std::endl;
    }
    std::cout << std::endl;
  }*/

  
  return 0;
}
