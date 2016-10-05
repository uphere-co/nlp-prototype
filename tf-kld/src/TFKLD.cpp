#include "src/TFKLD.h"
#include "src/Vocab.h"

using namespace util;
using namespace util::io;

using namespace tfkld::type;

using namespace arma;

namespace tfkld{

void MakeTFKLD(Param const &params, Documents &document) {

    real_t ep=0.05;
    real_t p=ep;
    real_t q=ep;
    real_t np=ep;
    real_t nq=ep;
    
    real_t div;
    for(int64_t a = 0; a < document.vocab.size(); ++a) {
        for(int i = 0; i < document.tag.size(); ++i) {
            
            auto isin1 = document.docs[i*2].find(a);
            auto isin2 = document.docs[i*2+1].find(a);

            if(document.tag[i] == "-1") {
                if(isin1 != document.docs[i*2].end() && isin2 != document.docs[i*2+1].end()) {
                    q++;
                }
                if(isin1 != document.docs[i*2].end() && isin2 == document.docs[i*2+1].end()) {
                    nq++;
                }
                if(isin1 == document.docs[i*2].end() && isin2 != document.docs[i*2+1].end()) {
                    nq++;
                }
            } else { // document.tag[i] == 1;
                if(isin1 != document.docs[i*2].end() && isin2 != document.docs[i*2+1].end()) {
                    p++;
                }
                if(isin1 != document.docs[i*2].end() && isin2 == document.docs[i*2+1].end()) {
                    np++;
                }
                if(isin1 == document.docs[i*2].end() && isin2 != document.docs[i*2+1].end()) {
                    np++;
                }
            }
        }

        div = (p/(p+np))*log((p/(p+np))/(q/(q+nq)) + 1e-7) + (np/(p+np))*log((np/(p+np))/(nq/(q+nq)) + 1e-7);        
        document.kld.push_back(div);
        p = ep;
        q = ep;
        np = ep;
        nq = ep;
        
    }

    for(auto &x : document.values) x.val *= pow(document.kld[x.row],params.power);
        
}

void MakeTFKLD(Param const &params, std::vector<real_t> &kld, std::vector<SpValue> &values) {
    for(auto &x : values) x.val *= pow(kld[x.row],params.power);
        
}

void MakeTFKLD_without_calculating_KLD(Param const &params, Documents &document) {
    for(auto &x : document.values) x.val *= pow(document.kld[x.row], params.power);
}


void runTransductive(Param const &params, Documents &doc_train, Documents &doc_test) {
    auto timer = Timer{};

    int64_t tdocs = doc_train.docs.size();

    doc_train.docs.insert( doc_train.docs.end(), doc_test.docs.begin(), doc_test.docs.end() );

    doc_train.values.clear();
    fillValue(doc_train);
    MakeTFKLD(params, doc_train);

    sp_mat inMat;

    fillMat(doc_train, inMat);

    if(params.verbose == 1) timer.here_then_reset("Filled the Matrix.\n");
    
    mat U;
    vec s;
    mat V;

    svds(U,s,V,inMat,doc_train.K_dim);

    if(params.verbose == 1) timer.here_then_reset("SVD is complete.\n");

    auto svec = makeSimMat(V);

    if(params.verbose == 1) timer.here_then_reset("Made Similarity Vectors.\n");
    
    std::ofstream fout{"train_KLD.dat"};

    int64_t count{0};
    int lcount = 1;
    
    for(int i = 0; i < tdocs/2; i++) {
        fout << doc_train.tag[count] << " ";
        for(auto y : svec[i]) {
            fout << lcount << ":" << y << " ";
            lcount++;
        }
        fout << "\n";
        lcount = 1;
        count++;
    }

    if(params.verbose == 1) timer.here_then_reset("Writing Similarity Vectors is Done.\n");

    sp_mat inMat2;

    fillValue(doc_test);
    MakeTFKLD_without_calculating_KLD(params, doc_test);
    fillMat(doc_test, inMat2);
    
    if(params.verbose == 1) timer.here_then_reset("Filled the Matrix.\n");

    mat U2;
    vec s2;
    mat V2;

    svds(U2,s2,V2,inMat2,doc_test.K_dim);

    if(params.verbose == 1) timer.here_then_reset("SVD is complete.\n");

    auto svec2 = makeSimMat(V2);
    
    if(params.verbose == 1) timer.here_then_reset("Made Similarity Vectors.\n");

    std::ofstream fout2{"test_KLD.dat"};

    count = 0;
    lcount = 1;

    for(auto x : svec2) {
        fout2 << doc_test.tag[count] << " ";
        for(auto y : x) {
            fout2 << lcount << ":" << y << " ";
            lcount++;
        }
        fout2 << "\n";
        lcount = 1;
        count++;
    }

    if(params.verbose == 1) timer.here_then_reset("Writing Similarity Vectors is Done.\n");


    fout2.close();
    
}

void runInductive(Param const &params, Documents &doc_train, Documents &doc_test) {

    auto timer = Timer{};

    sp_mat inMat;

    fillValue(doc_train);
    MakeTFKLD(params, doc_train);
    fillMat(doc_train, inMat);

    if(params.verbose == 1) timer.here_then_reset("Filled the Matrix.\n");
    
    mat U;
    vec s;
    mat V;

    svds(U,s,V,inMat,doc_train.K_dim);

    if(params.verbose == 1) timer.here_then_reset("SVD is complete.\n");

    auto svec = makeSimMat(V);

    if(params.verbose == 1) timer.here_then_reset("Made Similarity Vectors.\n");
    
    std::ofstream fout{"train_KLD.dat"};

    int64_t count{0};
    int lcount = 1;
    for(auto x : svec) {
        fout << doc_train.tag[count] << " ";
        for(auto y : x) {
            fout << lcount << ":" << y << " ";
            lcount++;
        }
        fout << "\n";
        lcount = 1;
        count++;
    }

    if(params.verbose == 1) timer.here_then_reset("Writing Similarity Vectors is Done.\n");

    sp_mat inMat2;

    fillValue(doc_test);
    MakeTFKLD(params, doc_test);
    fillMat(doc_test, inMat2);
    
    if(params.verbose == 1) timer.here_then_reset("Filled the Matrix.\n");

    mat U2;
    vec s2;
    mat V2;

    svds(U2,s2,V2,inMat2,doc_test.K_dim);

    if(params.verbose == 1) timer.here_then_reset("SVD is complete.\n");

    auto svec2 = makeSimMat(V2);
    normalizeSimMat(svec2);
    
    if(params.verbose == 1) timer.here_then_reset("Made Similarity Vectors.\n");

    std::ofstream fout2{"test_KLD.dat"};

    count = 0;
    lcount = 1;

    for(auto x : svec2) {
        fout2 << doc_test.tag[count] << " ";
        for(auto y : x) {
            fout2 << lcount << ":" << y << " ";
            lcount++;
        }
        fout2 << "\n";
        lcount = 1;
        count++;
    }

    if(params.verbose == 1) timer.here_then_reset("Writing Similarity Vectors is Done.\n");


    fout2.close();

}

void runTFKLD(Param const &params, Documents &doc_train, Documents &doc_test) {

    if(params.mode == 0) runInductive(params, doc_train, doc_test);
    if(params.mode == 1) runTransductive(params, doc_train, doc_test);
    
}
    
}//namespace tfkld
