#include <stdlib.h>
#include <string>
#include <fstream>
#include <iostream>
#include <algorithm>

#include <inttypes.h>

#include "utils.h"

#include "WordEmbed.h"

void WordEmbed::ExtractVocabFromTrainFile() {
    std::string word;
    std::ifstream inFile;
    int64_t a, i;

    vocab_hash.fill(-1); //initVocabHash();

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
            printf("%" PRIu64"K%c", train_words / 1000, 13);
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
        printf("Vocab size: %d\n", vocab_size);
        printf("Words in the train file: %" PRIu64"\n", train_words);
    }
    file_size = inFile.tellg();
    inFile.close();
}

// Save Vocab
void WordEmbed::SaveVocab() {
    int64_t i;
    std::ofstream outFile;
    outFile.open(save_vocab_file, std::ofstream::out | std::ofstream::binary);
    for(i = 0; i < vocab_size; i++) {
        outFile << vocab[i].word << " ";
        outFile << vocab[i].cn << "\n";
    }
    outFile.close();
}

// Read Vocab
void WordEmbed::ReadVocab() {
    std::string line;
    std::vector<std::string> word;
    std::ifstream inFile;
    inFile.open(read_vocab_file, std::ifstream::in | std::ifstream::binary);
    if(inFile.fail()) {
        std::cout << "Vocabulary file not found!\n";
        exit(1);
    }
    
    vocab_hash.fill(-1); //initVocabHash();

    vocab_size = 0;

    int a;
    while(1) {
        word.clear();
        std::getline(inFile,line);
        if(inFile.eof()) break;
        split(line, word); // todo : re-implement this with getline
        a = AddWordToVocab(word[0]);
        std::cout << word[0] << " ";
        std::cout << word[1] << "\n";
        vocab[a].cn = atof(word[1].c_str()); // Q: why atof, not atoi?
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

    file_size = inFile.tellg(); // Q: Shouldn't we set std::ifstream::ate instead of ...::in ?
    inFile.close(); // todo : Let us find a way to handle file in an error-safe manner.
}

// End of HashMap for dictionary

// Reads a single word from a file, assuming space + tab + EOL to be word boundaries
void WordEmbed::ReadWord(std::string& word, std::ifstream& inFile) {
    word.clear();
    if ( !inFile.eof() && (inFile.get() == '\n') ) {
        word = "</s>";
        return;
    }
    int a = 0, ch;
    while (!inFile.eof()) {
        ch = inFile.get();
        if ((ch == 13) || (ch == ' ') || (ch == '\t')) // ASCII 13 => CR
            continue;
        else if  (ch == '\n') {
            inFile.unget();
            break;
        }
        word += ch;
        a++;
    }
    return;
}

// Returns hash value of a word
int WordEmbed::GetWordHash (std::string& word) {
    uint64_t hash = 0;
    for(unsigned int a = 0; a < word.length(); a++)
        hash = hash * 257 + (uint64_t)word.at(a);
    hash = hash % vocab_hash_size;
    return hash;
}

// Returns position of a word in the vocabulary; if the word is not found, returns -1
int WordEmbed::SearchVocab (std::string& word) {
    uint64_t hash = GetWordHash(word);
    int pos;
    while(1) {
        pos = vocab_hash[hash]; 
        if( (pos == -1) || (word == vocab[pos].word) )
            return pos;
        hash = (hash + 1) % vocab_hash_size;
    }
    return -1;
}

// Reads a word and returns its index in the vocabulary
int WordEmbed::ReadWordIndex (std::ifstream& inFile) {
    std::string word;
    ReadWord(word, inFile);
    if(inFile.eof()) return -1;
    return SearchVocab(word);
}

// Adds a word to the vocabulary, and returns an index of a word in the vocabulary
int WordEmbed::AddWordToVocab (std::string word) {
    uint64_t hash;
    vocab_word s_word;

    s_word.word = word;
    s_word.cn = 0;

    vocab.push_back(s_word);
    vocab_size++;

    hash = GetWordHash(word);
    while( vocab_hash[hash] != -1 )
        hash = (hash + 1) % vocab_hash_size;
    vocab_hash[hash] = vocab_size - 1;
    return vocab_size - 1;
}

// Sorts the vocabulary by frequency using word counts
// Assumed that SortVocab() is always excuted right after learning vocab from training file
void WordEmbed::SortVocab() {
    uint64_t hash;
    // Sort the vocabulary and keep </s> at the first position
    std::sort(vocab.begin()+1, vocab.end(), VocabFreqCompare);

    vocab_hash.fill(-1); //initVocabHash();

    train_words = 0;
    unsigned int vocab_size0 = vocab_size;
    for (unsigned int a = 0; a < vocab_size0; a++) {
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
    for(unsigned int a = 0; a < vocab_size; a++)
        vocab[a].point.reserve(MAX_CODE_LENGTH);
}

// Reduces the vocabulary by removing infrequent tokens. Neutralized by min_reduce = 0
void WordEmbed::ReduceVocab() {
    unsigned int b = 0;
    for(unsigned int a = 0; a < vocab_size; a++) {
        if (vocab[a].cn > min_reduce) {
            vocab[b].cn = vocab[a].cn;
            vocab[b].word = vocab[a].word; // JH: what happen to other elements, like point?
            b++;
        }
    }
    vocab_size = b;
    
    // Hash will be re-computed, as it is not actual
    ResetHash();
    
    fflush(stdout); // print first
    min_reduce++; // As ReduceVocab() is called, more words will be removed in the vocabulary
}

// Reset hash
void WordEmbed::ResetHash() {
    uint64_t hash;
    vocab_hash.fill(-1); //initVocabHash();
    
    for(unsigned int a = 0; a < vocab_size; a++) {
        hash = GetWordHash(vocab[a].word);
        while(vocab_hash[hash] != -1) hash = (hash + 1) % vocab_hash_size;
        vocab_hash[hash] = a; // The value is the index(position) of the word
    }
}
