#include <fstream>
#include <sstream>
#include <iostream>
#include <thread>
#include <string>
#include <cmath>
#include <map>
#include <utility>

//#include <inttypes.h>

//#include <boost/tr1/random.hpp>

#include "utils.h"
#include "utils/hdf5.h"
#include "utils/string.h"

//#include "WordEmbed.h"


const int exp_table_size = 1000;
const int64_t table_size = 100000000;

using namespace util::io;



///// Data Structure
namespace word2vec{
namespace type{
  
using int_t = int;
using float_t = float;
using char_t = char;
}//namespace word2vec::type
}//namespace word2vec

namespace word2vec{

struct TokenizedFile{
    TokenizedFile(std::string train_file) {
        val.open(train_file, std::ifstream::in);

        if(val.fail()) {
            std::cout << "ERROR: training data file not found!\n";
            exit(1);
        }
    }
    std::ifstream val; 
};

//CAUTION!! Do not include a following line in a header.
using namespace word2vec::type;

using hashmap_t = std::map<std::string, int_t>;
    
void VocabLearn(TokenizedFile & file, hashmap_t &word_count, std::vector<std::pair<int64_t,std::string>> &word_position){
    std::string line;
    int64_t pos = 0;
    while (std::getline(file.val, line)){  
        std::istringstream iss{line};
        auto words = util::string::split(line);
        for(auto x : words) {
            word_position.push_back(std::make_pair(pos,x));
            pos++;
            auto isin = word_count.find(x);
            if(isin != word_count.end()) {
                word_count[x]+=1;
            } else {
                word_count[x]= 1;
            }
        }
    }
}

    
void PrintWordCount(hashmap_t const &word_count){
    for(auto x : word_count){
        std::cout << x.first << " " <<x.second<< std::endl;
    }
}

void InsertSpecialTag(hashmap_t &word_count){
    auto word2vec_sentence_delim = "</s>";
    word_count[word2vec_sentence_delim]=1;
}

auto MapValues(hashmap_t const &map){
    std::vector<hashmap_t::mapped_type> values;
    for(auto x : map){
        values.push_back(x.second);
    }
    return values;
}
auto MapKeys(hashmap_t const &map){
    std::vector<hashmap_t::key_type> values;
    for(auto x : map){
        values.push_back(x.first);
    }
    return values;
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
}//namespace word2vec

///// Data Structure


// main function arguments

using namespace word2vec;

class Word2Vec {
private:
    unsigned int layer1_size;
    unsigned int window;
    unsigned int num_threads;
    unsigned int iter;
    int hs, negative;
    int cbow;

    unsigned int min_reduce;
    unsigned int min_count;
    unsigned int vocab_size;
    int64_t train_words;
    int64_t word_count_actual;
    int64_t file_size;

    double sample;
    double alpha, starting_alpha;
    
    std::string train_file, output_file;
    std::string save_vocab_file, read_vocab_file;

    std::array<double,(exp_table_size + 1)> expTable;
    std::array<int, table_size> table;

    clock_t start;

    std::vector<hashmap_t::key_type> vocab;
    std::vector<hashmap_t::mapped_type> vocabcn;

    hashmap_t word_cn;
    std::vector<std::pair<int64_t,std::string>> word_pos;
    
    std::vector<double> syn0, syn1, syn1neg;

public:
    Word2Vec (             
        std::string train_file, //Use text data from <file> to train the model
        std::string output_file, //Use <file> to save the resulting word vectors / word clusters
        unsigned int min_count, //This will discard words that appear less than <int> times; default is 5
        std::string save_vocab_file, //The vocabulary will be saved to <file>
        std::string read_vocab_file, //The vocabulary will be read from <file>, not constructed from the training data
        unsigned int layer1_size, //Set size of word vectors; default is 100
        unsigned int window, //Set max skip length between words; default is 5
        double sample, //Set threshold for occurrence of words. default is 1e-3, useful range is (0, 1e-5) 
        int hs, //Use Hierarchical Softmax; default is 0 (not used)
        int negative, //Number of negative examples; default is 5, common values are 3 - 10 (0 = not used)
        unsigned int num_threads, //Use <int> threads (default 12)
        int iter, //Run more training iterations (default 5)
        double alpha, //Set the starting learning rate; default is 0.025 for skip-gram and 0.05 for CBOW
        int cbow = 0 //Use the continuous bag of words model; default is 1 (use 0 for skip-gram model)
        ):
        train_file (train_file),
        output_file (output_file),
        save_vocab_file (save_vocab_file),
        read_vocab_file (read_vocab_file),
        min_count (min_count),
        layer1_size (layer1_size),
        window (window),
        sample (sample),
        hs (hs),
        negative (negative),
        num_threads (num_threads),
        iter (iter),
        alpha (alpha),
        cbow (cbow)
    {
        min_reduce = 1;
        vocab_size = 0;
        train_words = 0;
        word_count_actual = 0;
        file_size = 0;
        
        //infile = TokenizedFile sinfile{train_file};
        //auto word_count = WordCount(infile);
            
        //auto word_count_values = MapValues(word_count);
        //auto word_count_keys = MapKeys(word_count);

    };
    void LearnVocab();
    //void CreateBinaryTree();
    void InitUnigramTable(auto const &word_cn);
    void testFunction();
    //void InitNet();
    //void TrainModelThread(int tid);
    //void TrainModel();
};

void Word2Vec::LearnVocab(){  
    TokenizedFile infile{train_file};
    VocabLearn(infile, word_cn, word_pos);
    vocab = MapKeys(word_cn);
    vocabcn = MapValues(word_cn);
}

void Word2Vec::InitUnigramTable(auto const &word_cn) {
    unsigned int i;
    double train_words_pow = 0;
    double d1;
    double power = 0.75;
    
    for(auto w : word_cn) train_words_pow += pow(w, power);
    i = 0;
    d1 = pow(word_cn[0], power)/train_words_pow;
    for(unsigned int a = 0; a < table_size; a++){
        table[a] = i;
        if(a / (double)table_size > d1){
            i++;
            d1 += pow(word_cn[i],power)/train_words_pow;
        }
        if(i >= vocab_size) i = vocab_size -1;
    }
}

void Word2Vec::testFunction(){
    //    for(auto x : vocab) std::cout << x << std::endl;
    for(auto x : word_pos) std::cout << x.first << "  " << x.second << std::endl;
}

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
    std::cout << "\t-save-vocab <file>\n";
    std::cout << "\t\tThe vocabulary will be saved to <file>\n";
    std::cout << "\t-read-vocab <file>\n";
    std::cout << "\t\tThe vocabulary will be read from <file>, not constructed from the training data\n";
    std::cout << "\t-cbow <int>\n";
    std::cout << "\t\tUse the continuous bag of words model; default is 1 (use 0 for skip-gram model)\n";
    std::cout << "\nExamples:\n";
    std::cout << "./word2vec -train data.txt -output vec.txt -size 200 -window 5 -sample 1e-4 -negative 5 -hs 0 -binary 0 -cbow 1 -iter 3\n\n";

}

int argpos(const char *str, int argc, char **argv) {
    std::string s_str(str);
    std::vector<std::string> s_argv(argv,argv+argc);

    int i=0;
    while (i<argc) {
        if(s_str == s_argv[i]) break;
        i++;
    }
    if(i == argc) i=0;

    return i;
}

Word2Vec *arg_to_w2v(int argc, char **argv) {

    int i;
    int layer1_size = 100;
    std::string train_file ="", save_vocab_file = "", read_vocab_file = "";
    int debug_mode =2, binary = 0, cbow = 0;
    double alpha = 0.05;
    std::string output_file = "";
    int window = 5;
    double sample = 1e-3;
    int hs = 0, negative = 5, num_threads = 12;
    int64_t iter = 5;
    int min_count = 5;
    int64_t classes = 0;
    std::string wordvector_file = "";
    
    
    if ((i = argpos("-size", argc, argv)) > 0) layer1_size = atoi(argv[i + 1]);
    if ((i = argpos("-train", argc, argv)) > 0) train_file = argv[i + 1];
    if ((i = argpos("-save-vocab", argc, argv)) > 0) save_vocab_file = argv[i + 1];
    if ((i = argpos("-read-vocab", argc, argv)) > 0) read_vocab_file = argv[i + 1];
    if ((i = argpos("-cbow", argc, argv)) > 0) cbow = atoi(argv[i + 1]);
    if (cbow) alpha = 0.05; else alpha = 0.025;
    if ((i = argpos("-alpha", argc, argv)) > 0) alpha = atof(argv[i + 1]);
    if ((i = argpos("-output", argc, argv)) > 0) output_file = argv[i + 1];
    if ((i = argpos("-window", argc, argv)) > 0) window = atoi(argv[i + 1]);
    if ((i = argpos("-sample", argc, argv)) > 0) sample = atof(argv[i + 1]);
    if ((i = argpos("-hs", argc, argv)) > 0) hs = atoi(argv[i + 1]);
    if ((i = argpos("-negative", argc, argv)) > 0) negative = atoi(argv[i + 1]);
    if ((i = argpos("-threads", argc, argv)) > 0) num_threads = atoi(argv[i + 1]);
    if ((i = argpos("-iter", argc, argv)) > 0) iter = atoi(argv[i + 1]);
    if ((i = argpos("-min-count", argc, argv)) > 0) min_count = atoi(argv[i + 1]);

    Word2Vec *w2v = new Word2Vec (             
                train_file, 
                output_file, 
                min_count, 
                save_vocab_file, 
                read_vocab_file, 
                layer1_size, 
                window, 
                sample, 
                hs, 
                negative, 
                num_threads, 
                iter, 
                alpha,
                cbow );

    return w2v;
}

int main(int argc, char **argv) {


    
    //std::vector<char> concat_words = Concat(word_count_keys);
    
    //PrintWordCount(word_count);

    
    //H5file file{H5name{"data.h5"}, hdf5::FileMode::read_exist};
    //file.writeRawData(H5name{"bar.word_count"},word_count_values);
    //file.writeRawData(H5name{"bar.word_key"},concat_words);
    
    //concat_read = file.readRawData(H5name{"bar.word_key"},concat_words); 
    //auto words = ToStrings(concat_read);
    //assert(words=word_count_keys);
    
    Word2Vec *w2v;
    
    if(argc < 2) { printHelp(); return 1; }
    else {
        w2v = arg_to_w2v(argc, argv);
    }
    w2v -> LearnVocab();
    //w2v -> testFunction();
    //w2v->TrainModel();

    return 0;
}
