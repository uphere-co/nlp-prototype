#ifndef RNN_PARSER_VOCA
#define RNN_PARSER_VOCA

#include<string>
#include<vector>
#include<map>

namespace rnn{
namespace parser{
namespace wordrep{

using float_type = float;

struct Word{
   //calling c_str() is necessary to truncate zero-filled end regions of fixed length string.
   Word(std::string word) : val{word.c_str()}{}
   bool operator<(const Word &word)  const { return this->val < word.val;}
   std::string val;
};

using word2idx_type = std::map<Word, size_t>;
class VocaIndexMap{
public:
   VocaIndexMap(word2idx_type const &word2idxs) : val{word2idxs}{}
   size_t getIndex(Word word) const {return val.find(word)->second;}
private:
   std::map<Word, size_t> val;
};

class Voca{
public:
   Voca(std::vector<char> raw_data, int max_word_len)
   : val{raw_data}, max_word_len{max_word_len}, voca_size{raw_data.size()/max_word_len}{}
   Word getWord(int idx) const {
      std::string word{val.data()+idx*max_word_len, 74};
      return Word{word};
   }
   VocaIndexMap indexing() const{
      word2idx_type word_to_idx;
      for(size_t i=0; i<this->size(); ++i){
         word_to_idx[this->getWord(i)]=i;
      }
      return VocaIndexMap{word_to_idx};
   }
   size_t size() const {return voca_size;}
private:
   std::vector<char> val;
   int max_word_len;
   size_t voca_size;
};

class WordVec{
public:
   using value_type = std::vector<float_type>;
   WordVec(value_type vec) : val{vec}{}
   value_type getEmptyWordVec(int dim) const {return WordVec{value_type(dim)}}

   value_type val;
};

class VocaRep{
public:
   VocaRep(std::vector<float_type> raw_data, int voca_size, int word_dim)
    : val{raw_data}, voca_size{voca_size}, word_dim{word_dim} {}
   WordVec getWordVec(int idx) const{
      WordVec::value_type vec(100);
      auto beg = std::cbegin(val) + idx*word_dim;
      std::copy(beg, beg+word_dim, std::begin(vec));
      return WordVec{vec};
   }
//private:
   std::vector<float_type> val;
   int voca_size;
   int word_dim;
};


}//namespace rnn::wordrep
}//namespace rnn::parser
}//namespace rnn


#endif //RNN_PARSER_VOCA
