#include "TFIDF.h"

using namespace util;
using namespace util::io;

using namespace tfkld::type;

namespace tfkld{

real_t val_idf(int64_t D, int_t Dt) {
    return log(D/(real_t)Dt);
}
    
void MakeTFIDF(std::vector<real_t> &idf, std::vector<SpValue> &values, vocab_t const &vocab, doc_t const &docs) {

    hashmap_t df;
    int64_t D = docs.size();
    
    for(auto x : values) df[x.row] += 1;

    if(df.size() != vocab.size()) {
        std::cout << "Sanity check failed!\n";
        exit(1);
    }
    
    for(auto x : df) idf.push_back(val_idf(D,x.second));

    for(auto &x : values) x.val *= idf[x.row];

}

void MakeTFIDF(std::vector<real_t> &idf, std::vector<SpValue> &values) {

    for(auto &x : values) x.val *= idf[x.row];

}
    
}//namespace tfkld
