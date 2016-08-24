#include <fstream>
#include <sstream>
#include <string>
#include <algorithm>
#include <map>

#include "utils/hdf5.h"
#include "utils/string.h"
using namespace util::io;

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

hashmap_t WordCount(TokenizedFile & file){
    std::string line;
    hashmap_t word_count;
    while (std::getline(file.val, line)){  
        std::istringstream iss{line};
        auto words = util::string::split(line);
        for(auto x : words) {
            auto isin = word_count.find(x);
            if(isin != word_count.end()) {
                word_count[x]+=1;
            } else {
                word_count[x]= 1;
            }
        }
    }
    return word_count;
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

int main(){
    using namespace word2vec;
    //std::vector<std::vector<double>> wordvector;
    std::string infile_name = "data.sample.txt";
    TokenizedFile infile{infile_name};
    auto word_count = WordCount(infile);
    //InsertSpecialTag(word_count);
    auto word_count_values = MapValues(word_count);
    auto word_count_keys = MapKeys(word_count);
    std::vector<char> concat_words = Concat(word_count_keys);
    
    //PrintWordCount(word_count);

    H5file file{H5name{"data.h5"}, hdf5::FileMode::rw_exist};
    file.writeRawData(H5name{"1b.short_sents.word_count"},word_count_values);
    file.writeRawData(H5name{"1b.short_sents.bar.word_key"},concat_words);
    
    //concat_read = file.readRawData(H5name{"bar.word_key"},concat_words); 
    //auto words = ToStrings(concat_read);
    //assert(words=word_count_keys);
    return 0;
}
