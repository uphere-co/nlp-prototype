#include <fstream>
#include <sstream>
#include <string>
#include <algorithm>
#include <map>

#include "utils/hdf5.h"
#include "utils/string.h"
using namespace util::io;

namespace tfkld{
namespace type{
  
using int_t = int;
using float_t = float;
using char_t = char;
}//namespace tfkld::type
}//namespace tfkld

namespace tfkld{

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
using namespace tfkld::type;

using hashmap_t = std::map<std::string, int_t>;
using vocab_t = std::vector<std::string>;

vocab_t VocabLearn(TokenizedFile &file) {
  std::string line;
  vocab_t vocab;
  while(std::getline(file.val, line)){
    std::istringstream iss{line};
    auto words = util::string::split(line);
    for(auto x : words){
      auto isin = std::find(vocab.begin(), vocab.end(), x);
      if(isin != vocab.end()) {
	vocab.push_back(x);
      } else{
	continue;
      }
    }
  }
  
  return vocab;
}
  
void PrintVocab(hashmap_t const &word_count){
    for(auto x : word_count){
        std::cout << x.first << " " <<x.second<< std::endl;
    }
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
}//namespace tfkld

int main(){
    using namespace tfkld;
    std::string train_file = "data.txt";
    TokenizedFile infile{train_file};
    
    //std::vector<char> concat_words = Concat(word_count_keys);
    
    //H5file file{H5name{"data.h5"}, hdf5::FileMode::replace};
    //file.writeRawData(H5name{"1b.training.1M.count"},word_count_values);
    //file.writeRawData(H5name{"1b.training.1M.word"},concat_words);
    
    //concat_read = file.readRawData(H5name{"bar.word_key"},concat_words); 
    //auto words = ToStrings(concat_read);
    //assert(words=word_count_keys);
    return 0;
}
