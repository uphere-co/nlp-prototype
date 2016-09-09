#include "parser/wordvec.h"
#include "parser/config.h"

namespace rnn{
namespace wordrep{

WordVectors& operator +=(WordVectors& out, const WordVectors& x){
    for(auto const &p : x.val) out.val[p.first] = p.second;
    return out; 
}
WordVectors operator +(const WordVectors& x, const WordVectors& y){
    WordVectors out{x};
    out+=y;
    return out;
}

// template 
// class WordBlock_base<rnn::config::word_dim>;
// using WordBlock = WordBlock_base<rnn::config::word_dim>;

}//namespace rnn::wordrep
}//namespace rnn
