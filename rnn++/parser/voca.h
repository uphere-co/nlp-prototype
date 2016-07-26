#ifndef RNN_PARSER_VOCA
#define RNN_PARSER_VOCA

#include<ostream>
#include<string>
#include<vector>
#include<map>
#include<algorithm>

#include<gsl.h>

namespace rnn{
namespace parser{
namespace wordrep{

struct Word{
    Word(gsl::cstring_span<> word) : span{word}{}
    bool operator<(const Word &word)  const { return this->span < word.span;}
    gsl::cstring_span<> span;
};
std::ostream& operator<<(std::ostream& os, const Word& obj) {
    os<<gsl::to_string(obj.span);
    return os;
}

using word2idx_type = std::map<Word, size_t>;
class VocaIndex{
public:
    VocaIndex(word2idx_type const &word2idxs) : val{word2idxs}{}
    size_t getIndex(Word word) const {return val.find(word)->second;}
private:
    std::map<Word, size_t> val;
};

class Voca{
public:
    Voca(std::vector<char> raw_data, int max_word_len)
    : _val{raw_data}, span{_val}, max_word_len{max_word_len},
    voca_size{raw_data.size()/max_word_len}{}
    Word getWord(int idx) const {
        auto beg=std::cbegin(_val)+idx*max_word_len;
        auto end=std::find(beg, beg+max_word_len, '\0');
        gsl::cstring_span<> word = span.subspan(idx*max_word_len, end-beg);
        return Word{word};
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
    std::vector<char> _val;
    gsl::cstring_span<> span;
    int max_word_len;
    size_t voca_size;
};

}//namespace rnn::wordrep
}//namespace rnn::parser
}//namespace rnn


#endif //RNN_PARSER_VOCA
