#ifndef RNN_PARSER_VOCA
#define RNN_PARSER_VOCA

#include<string>
#include<vector>
#include<map>

namespace rnn{
namespace parser{
namespace wordrep{

using word2idx_type = std::map<std::string, size_t>;

class VocaIndex{
public:
   VocaIndex(word2idx_type const &word2idxs) : val{word2idxs}{}
   size_t getIndex(std::string word) const {return val.find(word)->second;}
private:
   std::map<std::string, size_t> val;
};

class Voca{
public:
   Voca(std::vector<char> raw_data, int max_word_len)
   : val{raw_data}, max_word_len{max_word_len}, voca_size{raw_data.size()/max_word_len}{}
   std::string getWord(int idx) const {
      std::string word{val.data()+idx*max_word_len, 74};
      return std::string{word.c_str()};
   }
   VocaIndex indexing() const{
      word2idx_type word_to_idx;
      for(size_t i=0; i<this->size(); ++i){
         word_to_idx[this->getWord(i)]=i;
      }
      return VocaIndex{word_to_idx};
   }
   size_t size() const {return voca_size;}
private:
   std::vector<char> val;
   int max_word_len;
   size_t voca_size;
};

}//namespace rnn::wordrep
}//namespace rnn::parser
}//namespace rnn


#endif //RNN_PARSER_VOCA
