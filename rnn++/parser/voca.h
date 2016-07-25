#ifndef RNN_PARSER_VOCA
#define RNN_PARSER_VOCA

#include<string>
#include<vector>

namespace rnn{
namespace parser{
namespace wordrep{

class Voca{
public:
   Voca(std::vector<char> raw_data, int max_word_len)
   : val{raw_data}, max_word_len{max_word_len}, voca_size{raw_data.size()/max_word_len}{}
   std::string const getWord(int idx){
      std::string word{val.data()+idx*max_word_len, 74};
      return std::string{word.c_str()};
   }
   int size() const {return voca_size;}
private:
   std::vector<char> val;
   int max_word_len;
   size_t voca_size;
};

}//namespace rnn::wordrep
}//namespace rnn::parser
}//namespace rnn


#endif //RNN_PARSER_VOCA
