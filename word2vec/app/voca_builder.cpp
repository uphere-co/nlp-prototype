#include <fstream>
#include <sstream>
#include <string>
#include <algorithm>
#include <map>

#include "utils/hdf5.h"
#include "utils/string.h"
#include "utils/profiling.h"

using namespace util::io;

namespace word2vec{
namespace type{
  
using int_t = int;
using float_t = float;
using char_t = char;

}//namespace word2vec::type
}//namespace word2vec

namespace word2vec{
using hashmap_t = std::map<std::string, type::int_t>;

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

hashmap_t WordCount(TokenizedFile & file){
    std::string line;
    hashmap_t word_count;
    while (std::getline(file.val, line)){  
        std::istringstream iss{line};
        auto words = util::string::split(line);
        for(auto const &x : words)
            word_count[x]+=1;
    }
    return word_count;
}
  
void PrintWordCount(hashmap_t const &word_count){
    for(auto x : word_count){
        std::cout << x.first << " " <<x.second<< std::endl;
    }
}

auto FilteredMapValues(hashmap_t const &map, hashmap_t::mapped_type cutoff){
    std::vector<hashmap_t::mapped_type> values;
    for(auto x : map){
        if(x.second<cutoff) continue;
        values.push_back(x.second);
    }
    return values;
}
auto FilteredMapKeys(hashmap_t const &map, hashmap_t::mapped_type cutoff){
    std::vector<hashmap_t::key_type> values;
    for(auto x : map){
        if(x.second<cutoff) continue;
        values.push_back(x.first);
    }
    return values;
}
auto MapValues(hashmap_t const &map){
    return FilteredMapValues(map, 0);
}
auto MapKeys(hashmap_t const &map){
    return FilteredMapKeys(map, 0);
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
        it=std::find(it, end, '\0');
        ++it;
    }
    return words;
}
}//namespace word2vec

int main(){
    using namespace word2vec;
    auto timer=util::Timer{};
    //std::string infile_name = "data.txt";
    std::string infile_name = "data.test";
    TokenizedFile infile{infile_name};
    timer.here_then_reset("Setup.");
    auto word_count = WordCount(infile);
    timer.here_then_reset("Word count.");
    auto word_count_values = FilteredMapValues(word_count,5);
    auto word_count_keys = FilteredMapKeys(word_count,5);
    timer.here_then_reset("Occurence filtering.");
    std::vector<char> concat_words = Concat(word_count_keys);
    timer.here_then_reset("Transfrom to HDF5 raw data format.");
    
    PrintWordCount(word_count);
    timer.here_then_reset("print counts.");

    H5file file{H5name{"data.h5"}, hdf5::FileMode::replace};
    file.writeRawData(H5name{"1b.training.1M.count"},word_count_values);
    file.writeRawData(H5name{"1b.training.1M.word"},concat_words);
    timer.here_then_reset("Write to HDF5 file.");
    
    //concat_read = file.readRawData(H5name{"bar.word_key"},concat_words); 
    //auto words = ToStrings(concat_read);
    //assert(words=word_count_keys);
    return 0;
}
