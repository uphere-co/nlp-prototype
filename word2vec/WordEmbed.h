#ifndef WORDEMBED_H
#define WORDEMBED_H

#include <stdlib.h>
#include <string>
#include <array>
#include <vector>

#include <fstream>

#define MAX_CODE_LENGTH 40

typedef float real;

struct vocab_word {
    uint64_t cn;
    std::vector<uint64_t> point;
    std::string word, code;
    char codelen;
};

// compile time constants
const uint64_t table_size = 100*1000*1000;
const uint64_t vocab_hash_size = 30*1000*1000;

// Base class for word embedding models
class WordEmbed {
    protected:
        std::string train_file, output_file;
        unsigned int min_count;
        int debug_mode, binary;
        std::string save_vocab_file, read_vocab_file;
        
        bool ready_to_train;
        
        int64_t train_words;
        unsigned int min_reduce;
        unsigned int vocab_size;
        int64_t file_size;

         // HashMap for words. vocab_hash[WORD_HASH] = WORD_POSITION
        std::array<int, vocab_hash_size> vocab_hash;
        std::vector<vocab_word> vocab;

    public:
        WordEmbed (
                std::string train_file, //use text data from <file> to train the model
                std::string output_file = "",
                unsigned int min_count = 5, 
                int debug_mode = 2,
                int binary = 0,
                std::string save_vocab_file = "",
                std::string read_vocab_file = ""):
            train_file (train_file),
            output_file (output_file),
            min_count (min_count), 
            debug_mode (debug_mode),
            binary (binary),
            save_vocab_file (save_vocab_file),
            read_vocab_file (read_vocab_file)
        {
        };
        
        virtual void TrainModel () = 0;

        bool CheckReady () { return true; }; //implement

        static inline bool VocabFreqCompare (const vocab_word &a, const vocab_word &b) {
            return (a.cn > b.cn);
        };

        void ReadWord (std::string& word, std::ifstream& inFile);
        int GetWordHash (std::string& word);
        void ResetHash ();
        int SearchVocab (std::string& word);
        void SaveVocab ();
        void ReadVocab ();
        int ReadWordIndex  (std::ifstream& inFile);
        int AddWordToVocab (std::string word);
        void SortVocab ();
        void ReduceVocab ();

        void ExtractVocabFromTrainFile ();

};

#endif /* WORDEMBED_H */
