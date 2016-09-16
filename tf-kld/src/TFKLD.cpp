#include "src/TFKLD.h"

using namespace util;
using namespace util::io;

using namespace tfkld::type;

namespace tfkld{

void MakeTFKLD(std::vector<float_t> &kld, std::vector<std::string> &tag, std::vector<SpValue> &values, int64_t &count, vocab_t const &vocab, doc_t const &docs) {

    float_t ep=0.05;
    float_t p=ep;
    float_t q=ep;
    float_t np=ep;
    float_t nq=ep;
    
    float_t div;
    for(int64_t a = 0; a < vocab.size(); ++a) {
        for(int i = 0; i < tag.size(); ++i) {
            
            auto isin1 = docs[i*2].find(a);
            auto isin2 = docs[i*2+1].find(a);

            if(tag[i] == "-1") {
                if(isin1 != docs[i*2].end() && isin2 != docs[i*2+1].end()) {
                    q++;
                }
                if(isin1 != docs[i*2].end() && isin2 == docs[i*2+1].end()) {
                    nq++;
                }
                if(isin1 == docs[i*2].end() && isin2 != docs[i*2+1].end()) {
                    nq++;
                }
            } else { // tag[i] == 1;
                if(isin1 != docs[i*2].end() && isin2 != docs[i*2+1].end()) {
                    p++;
                }
                if(isin1 != docs[i*2].end() && isin2 == docs[i*2+1].end()) {
                    np++;
                }
                if(isin1 == docs[i*2].end() && isin2 != docs[i*2+1].end()) {
                    np++;
                }
            }
        }

        div = (p/(p+np))*log((p/(p+np))/(q/(q+nq)) + 1e-7) + (np/(p+np))*log((np/(p+np))/(nq/(q+nq)) + 1e-7);        
        kld.push_back(div);
        p = ep;
        q = ep;
        np = ep;
        nq = ep;
        
    }

    for(auto &x : values) x.val *= kld[x.row];
        
}

void MakeTFKLD(std::vector<float_t> &kld, std::vector<SpValue> &values) {

    for(auto &x : values) x.val *= kld[x.row];
        
}
    
}//namespace tfkld
