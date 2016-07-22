#include <stdlib.h>
#include <iostream>
#include <thread>
#include <string>

#include <inttypes.h>

#include <boost/tr1/random.hpp>
#include "utils.h"

#include "WordEmbed.h"

#define EXP_TABLE_SIZE 1000
#define MAX_EXP 6
#define MAX_STRING 1000
#define MAX_CODE_LENGTH 40
#define MAX_SENTENCE_LENGTH 1000

class Word2Vec : public WordEmbed {
    private:
        unsigned int layer1_size;
        int window;
        real sample;
        int hs, negative;
        unsigned int num_threads;
        unsigned int iter;
        real alpha;
        unsigned int classes;
        int  cbow;
        std::string wordvector_file;

        int64_t word_count_actual;

        real starting_alpha;

        std::array<real,(EXP_TABLE_SIZE + 1)> expTable;

        clock_t start;

        std::vector<real> syn0, syn1, syn1neg;

        // Tables
        std::array<int, table_size> table;
    public:
        Word2Vec (             
                std::string train_file, //Use text data from <file> to train the model
                std::string output_file = "", //Use <file> to save the resulting word vectors / word clusters
                unsigned int min_count = 5, //This will discard words that appear less than <int> times; default is 5
                int debug_mode = 2, //Set the debug mode (default = 2 = more info during training)
                int binary = 0, //Save the resulting vectors in binary moded; default is 0 (off)
                std::string save_vocab_file = "", //The vocabulary will be saved to <file>
                std::string read_vocab_file = "", //The vocabulary will be read from <file>, not constructed from the training data
                unsigned int layer1_size = 100, //Set size of word vectors; default is 100
                unsigned int window = 5, //Set max skip length between words; default is 5
                real sample = 1e-3, //Set threshold for occurrence of words. Those that appear with higher frequency in the training data will be randomly down-sampled; default is 1e-3, useful range is (0, 1e-5) 
                int hs = 0, //Use Hierarchical Softmax; default is 0 (not used)
                int negative = 5, //Number of negative examples; default is 5, common values are 3 - 10 (0 = not used)
                unsigned int num_threads = 12, //Use <int> threads (default 12)
                int iter = 5, //Run more training iterations (default 5)
                real alpha = 0.05, //Set the starting learning rate; default is 0.025 for skip-gram and 0.05 for CBOW
                unsigned int classes = 0,//Output word classes rather than word vectors; default number of classes is 0 (vectors are written)
                int cbow = 0, //Use the continuous bag of words model; default is 1 (use 0 for skip-gram model)
                std::string wordvector_file = "" //Read the trained word vector file
                ):
            WordEmbed (train_file, output_file, min_count, debug_mode, binary,save_vocab_file,read_vocab_file),
            layer1_size (layer1_size),
            window (window),
            sample (sample),
            hs (hs),
            negative (negative),
            num_threads (num_threads),
            iter (iter),
            alpha (alpha),
            classes (classes),
            cbow (cbow),
            wordvector_file (wordvector_file)
        {
            min_reduce = 1, 
            vocab_size = 0;
            train_words = 0;
            word_count_actual=0;
            file_size = 0;

            ready_to_train = false;
            if( CheckReady() ) ready_to_train = true;
        };
        
        void TrainModel ();

        void CreateBinaryTree ();

        void InitUnigramTable ();
        void InitNet ();
        void TrainModelThread (int tid);
};

// Create binary Huffman tree using the word counts
// Frequent words will have short unique binary codes
void Word2Vec::CreateBinaryTree() {
    // Words are not sorted at the beginning
    int64_t a, b, i, min1i, min2i, pos1, pos2;
    int64_t point[MAX_CODE_LENGTH];
    char code[MAX_CODE_LENGTH];

    std::vector<int64_t> count;
    std::vector<int64_t> binary;
    std::vector<int64_t> parent_node;

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

// Begin of Learning Net

void Word2Vec::InitUnigramTable() {
    unsigned int i;
    double train_words_pow = 0;
    double d1, power = 0.75;
    for (unsigned int a = 0; a < vocab_size; a++) train_words_pow += pow(vocab[a].cn, power);
    i = 0;
    d1 = pow(vocab[i].cn, power) / train_words_pow;
    for (unsigned int a = 0; a < table_size; a++) {
        table[a] = i;
        if (a / (double)table_size > d1) {
            i++;
            d1 += pow(vocab[i].cn, power) / train_words_pow;
        }
        if (i >= vocab_size) i = vocab_size - 1;
    }
}

void Word2Vec::InitNet() {
    // The name EXP_TABLE is totally mis-guiding.
    for (int i = 0; i < EXP_TABLE_SIZE; i++) {
        expTable[i] = exp((i / (real)EXP_TABLE_SIZE * 2 - 1) * MAX_EXP);
        expTable[i] = expTable[i] / (expTable[i] + 1);
    }

    syn0.reserve((int64_t)vocab_size * layer1_size);
    if(hs) {
        syn1.reserve((int64_t)vocab_size * layer1_size);
        for(unsigned int a = 0; a < vocab_size; a++)
            for(unsigned int b = 0; b < layer1_size; b++)
                syn1[a * layer1_size + b] = 0;
    }
    if(negative > 0) {
        syn1neg.reserve((int64_t)vocab_size * layer1_size);
        for(unsigned int a = 0; a < vocab_size; a++)
            for(unsigned int b = 0; b < layer1_size; b++)
                syn1neg[a * layer1_size + b] = 0;
    }

    for(unsigned int a = 0; a < vocab_size; a++)
        for(unsigned int b = 0; b < layer1_size; b++)
            syn0[a * layer1_size + b] = (rand()/(double)RAND_MAX - 0.5) / layer1_size;

    CreateBinaryTree();
}

void Word2Vec::TrainModelThread(int tid){
    int64_t a, b, d, word, last_word, sentence_length = 0, sentence_position = 0;
    int64_t word_count = 0, last_word_count = 0, sen[MAX_SENTENCE_LENGTH + 1];
    int64_t l1, l2, c, target, label, local_iter = iter;
    uint64_t next_random;
    real f, g;
    clock_t now;

    boost::mt19937 rand_engine_int;    // rand engine
    boost::uniform_int<> rand_int(0, table_size);    // set range.
    boost::variate_generator<boost::mt19937, boost::uniform_int<>> rand_gen_int(rand_engine_int, rand_int);

    boost::mt19937 rand_engine_double;  // rand engine
    boost::uniform_real<> rand_double(0.0, 1.0);
    boost::variate_generator<boost::mt19937, boost::uniform_real<>> rand_gen_double(rand_engine_double, rand_double);
    
    std::vector<real> neu1;
    std::vector<real> neu1e;

    neu1.reserve(layer1_size);
    neu1e.reserve(layer1_size);

    std::ifstream inFile(train_file, std::ifstream::in | std::ifstream::binary);
    inFile.seekg(file_size / (int64_t)num_threads * (int64_t)tid);
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
                    if(ran < rand_gen_double()) continue;
                }
                sen[sentence_length] = word;
                sentence_length++;
                if(sentence_length >= MAX_SENTENCE_LENGTH) break;
            }
            sentence_position = 0;
        }
        if(inFile.eof() || (word_count > train_words / num_threads )) {
            word_count_actual += word_count - last_word_count;
            local_iter--;
            if(local_iter == 0) break;
            word_count = 0;
            last_word_count = 0;
            sentence_length = 0;
	    if(inFile.eof()) {
	      inFile.clear();
	      inFile.seekg(file_size / (int64_t)num_threads * (int64_t)tid);
	    }
	    else {
	      inFile.seekg(file_size / (int64_t)num_threads * (int64_t)tid);
	    }
            continue;
        }
        word = sen[sentence_position];
        if(word == -1) continue;
        // Network Initialization
        for(c = 0; c < layer1_size; c++) neu1[c] = 0;
        for(c = 0; c < layer1_size; c++) neu1e[c] = 0;
        
        b = rand_gen_int() % window;
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
		      next_random = rand_gen_int();
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

void Word2Vec::TrainModel() {
    std::ofstream outFile;

    std::vector<std::thread> th;
    starting_alpha = alpha;

    std::cout << "Starting training using file " << train_file << std::endl;
    if(read_vocab_file != "") ReadVocab();
    else ExtractVocabFromTrainFile();
    if(save_vocab_file != "") SaveVocab();
    if(output_file[0] == 0) return;

    // Initialization
    InitNet();
    if(negative > 0)
        InitUnigramTable();

    start = clock(); // start to measure time

    for(int i = 0; i < num_threads; i++) {
      th.push_back(std::thread(&Word2Vec::TrainModelThread, this, i));
    }

    for(auto &t : th) {
      t.join();
    }

    //TrainModelThread(); // Main routine ??

    outFile.open(output_file, std::ofstream::out | std::ofstream::binary);
    if(classes == 0) {
        outFile << vocab_size << " " << layer1_size << std::endl;
        for(unsigned int a = 0; a < vocab_size; a++) {
            outFile << vocab[a].word << " ";
            if(binary) for(unsigned int b = 0; b < layer1_size; b++) outFile << syn0[a * layer1_size + b] << " ";
            else for(unsigned int b = 0; b < layer1_size; b++) outFile << syn0[a * layer1_size + b] << " ";
            outFile << std::endl;
        }
    } else {
        // Run K-means on the word vectors
        unsigned int clcn = classes, iter = 10, closeid;
        real closev, x;

        std::vector<int> centcn, cl;
        std::vector<real> cent;

        centcn.reserve(classes);
        cl.reserve(vocab_size);
        cent.reserve(classes * layer1_size);

        for (unsigned int a = 0; a < vocab_size; a++) cl[a] = a % clcn;
        for (unsigned int a = 0; a < iter; a++) {
            for (unsigned int b = 0; b < clcn * layer1_size; b++) cent[b] = 0;
            for (unsigned int b = 0; b < clcn; b++) centcn[b] = 1;
            for (unsigned int c = 0; c < vocab_size; c++) {
                for (unsigned int d = 0; d < layer1_size; d++) cent[layer1_size * cl[c] + d] += syn0[c * layer1_size + d];
                centcn[cl[c]]++;
            }
            for (unsigned int b = 0; b < clcn; b++) {
                closev = 0;
                for (unsigned int c = 0; c < layer1_size; c++) {
                    cent[layer1_size * b + c] /= centcn[b];
                    closev += cent[layer1_size * b + c] * cent[layer1_size * b + c];
                }
                closev = sqrt(closev);
                for (unsigned int c = 0; c < layer1_size; c++) cent[layer1_size * b + c] /= closev;
            }
            for (unsigned int c = 0; c < vocab_size; c++) {
                closev = -10;
                closeid = 0;
                for (unsigned int d = 0; d < clcn; d++) {
                    x = 0;
                    for (unsigned int b = 0; b < layer1_size; b++) x += cent[layer1_size * d + b] * syn0[c * layer1_size + b];
                    if (x > closev) {
                        closev = x;
                        closeid = d;
                    }
                }
                cl[c] = closeid;
            }
        }
        // Save the K-means classes
        for (unsigned int a = 0; a < vocab_size; a++) outFile << vocab[a].word << "    " << cl[a] << std::endl;

    }
    outFile.close();
}

// End of Learning Net


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
    std::cout << "\t-wordvector <file>\n";
    std::cout << "\t\tRead the trained word vector file\n";
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

// TODO : A proper exception handling is needed.
Word2Vec *arg_to_w2v(int argc, char **argv) {
    int i;

    int layer1_size = 100;
    std::string train_file ="", save_vocab_file = "", read_vocab_file = "";
    int debug_mode =2, binary = 0, cbow = 0; 
    real alpha = 0.05;
    std::string output_file = "";
    int window = 5;
    real sample = 1e-3;
    int hs = 0, negative = 5, num_threads = 12;
    int64_t iter = 5;
    int min_count = 5;
    int64_t classes = 0;
    std::string wordvector_file = "";

    if ((i = argpos("-size", argc, argv)) > 0) layer1_size = atoi(argv[i + 1]);
    if ((i = argpos("-train", argc, argv)) > 0) train_file = argv[i + 1];
    if ((i = argpos("-save-vocab", argc, argv)) > 0) save_vocab_file = argv[i + 1];
    if ((i = argpos("-read-vocab", argc, argv)) > 0) read_vocab_file = argv[i + 1];
    if ((i = argpos("-debug", argc, argv)) > 0) debug_mode = atoi(argv[i + 1]);
    if ((i = argpos("-binary", argc, argv)) > 0) binary = atoi(argv[i + 1]);
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
    if ((i = argpos("-classes", argc, argv)) > 0) classes = atoi(argv[i + 1]);
    if ((i = argpos("-wordvector", argc, argv)) > 0) wordvector_file = argv[i + 1];

    Word2Vec *w2v = new Word2Vec (             
                train_file, 
                output_file, 
                min_count, 
                debug_mode, 
                binary, 
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
                classes,
                cbow, 
                wordvector_file );

    return w2v;
}

int main(int argc, char **argv) {

    // Modori's birthday is the source of randomness.
    std::srand(19870518);

    Word2Vec *w2v;

    if(argc < 2) { printHelp(); return 1; }
    else {
        w2v = arg_to_w2v(argc, argv);
    }

    w2v->TrainModel();

    return 0;
}
