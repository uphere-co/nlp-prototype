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

    /*
void runTransductive(Param const &params) {
    auto timer = Timer{};

    int K_dim = params.kdim;

    std::string fin_name = params.trainFile;
    MSParaFile fin{fin_name};

    std::string fin_name2 = params.testFile;
    MSParaFile fin2{fin_name2};
    
    auto vocab = LearnVocab(fin);
    if(params.verbose == 1) timer.here_then_reset("\nConstructed Vocabulary.\n");
    fin.setBegin();
    auto docs = LearnPairSentence(vocab,fin);
    if(params.verbose == 1) timer.here_then_reset("\nConstructed Paragraphs.\n");
    fin.setBegin();
    auto tag = LearnTag(fin);
    if(params.verbose == 1) timer.here_then_reset("\nConstructed Tag.\n");

    int64_t tdocs = docs.size();
    
    auto vocab2 = vocab;
    auto docs2 = LearnPairSentence(vocab2,fin2);
    if(params.verbose == 1) timer.here_then_reset("\nConstructed Paragraphs.\n");
    fin2.setBegin();
    auto tag2 = LearnTag(fin2);
    if(params.verbose == 1) timer.here_then_reset("\nConstructed Tag.\n");

    std::vector<SpValue> values;
    std::vector<real_t> kld;

    fillValue(values, vocab, docs);
    MakeTFKLD(params, kld, tag, values, vocab, docs);

    docs.insert( docs.end(), docs2.begin(), docs2.end() );

    values.clear();
    fillValue(values, vocab, docs);
    MakeTFKLD(params, kld, values);

    int64_t n_rows, n_cols;
    n_rows = vocab.size();
    n_cols = docs.size();

    sp_mat inMat(n_rows, n_cols);

    fillMat(values, vocab, docs, inMat);

    if(params.verbose == 1) timer.here_then_reset("Filled the Matrix.\n");
    
    mat U;
    vec s;
    mat V;

    svds(U,s,V,inMat,K_dim);

    if(params.verbose == 1) timer.here_then_reset("SVD is complete.\n");

    auto svec = makeSimMat(V);

    if(params.verbose == 1) timer.here_then_reset("Made Similarity Vectors.\n");
    
    std::ofstream fout{"train_KLD.dat"};

    int64_t count{0};
    int lcount = 1;
    
    for(int i = 0; i < tdocs/2; i++) {
        fout << tag[count] << " ";
        for(auto y : svec[i]) {
            fout << lcount << ":" << y << " ";
            lcount++;
        }
        fout << "\n";
        lcount = 1;
        count++;
    }

    if(params.verbose == 1) timer.here_then_reset("Writing Similarity Vectors is Done.\n");

    std::string vocab_filename = "vocab.dat";
    std::ofstream vocab_out{vocab_filename};

    std::string kld_filename = "kld.dat";
    std::ofstream kld_out{kld_filename};
    
    for(auto x : vocab) {
        vocab_out << x.first << " " << x.second << std::endl;
    }

    for(auto x : kld) {
        kld_out << pow(x,params.power) << std::endl;
    }    
    
    vocab_out.close();
    kld_out.close();

    int64_t n_rows2, n_cols2;
    n_rows2 = vocab2.size();
    n_cols2 = docs2.size();

    sp_mat inMat2(n_rows2, n_cols2);

    std::vector<SpValue> values2;

    fillValue(values2, vocab2, docs2);
    MakeTFKLD(params, kld, values2);
    fillMat(values2, vocab2, docs2, inMat2);
    
    if(params.verbose == 1) timer.here_then_reset("Filled the Matrix.\n");

    mat U2;
    vec s2;
    mat V2;

    svds(U2,s2,V2,inMat2,K_dim);

    if(params.verbose == 1) timer.here_then_reset("SVD is complete.\n");

    auto svec2 = makeSimMat(V2);
    
    if(params.verbose == 1) timer.here_then_reset("Made Similarity Vectors.\n");

    std::ofstream fout2{"test_KLD.dat"};

    count = 0;
    lcount = 1;

    for(auto x : svec2) {
        fout2 << tag2[count] << " ";
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

void runInductive(Param const &params) {

    auto timer = Timer{};

    int K_dim = params.kdim;
    
    std::string fin_name = params.trainFile;
    MSParaFile fin{fin_name};

    auto vocab = LearnVocab(fin);
    if(params.verbose == 1) timer.here_then_reset("\nConstructed Vocabulary.\n");
    fin.setBegin();
    auto docs = LearnPairSentence(vocab,fin);
    if(params.verbose == 1) timer.here_then_reset("\nConstructed Paragraphs.\n");
    fin.setBegin();
    auto tag = LearnTag(fin);
    if(params.verbose == 1) timer.here_then_reset("\nConstructed Tag.\n");
    
    int64_t n_rows, n_cols;
    n_rows = vocab.size();
    n_cols = docs.size();

    sp_mat inMat(n_rows, n_cols);

    std::vector<SpValue> values;
    std::vector<real_t> idf;
    std::vector<real_t> kld;

    fillValue(values, vocab, docs);
    MakeTFKLD(params, kld, tag, values, vocab, docs);
    fillMat(values, vocab, docs, inMat);

    if(params.verbose == 1) timer.here_then_reset("Filled the Matrix.\n");
    
    mat U;
    vec s;
    mat V;

    svds(U,s,V,inMat,K_dim);

    if(params.verbose == 1) timer.here_then_reset("SVD is complete.\n");

    auto svec = makeSimMat(V);

    if(params.verbose == 1) timer.here_then_reset("Made Similarity Vectors.\n");
    
    std::ofstream fout{"train_KLD.dat"};

    int64_t count{0};
    int lcount = 1;
    for(auto x : svec) {
        fout << tag[count] << " ";
        for(auto y : x) {
            fout << lcount << ":" << y << " ";
            lcount++;
        }
        fout << "\n";
        lcount = 1;
        count++;
    }

    if(params.verbose == 1) timer.here_then_reset("Writing Similarity Vectors is Done.\n");

    std::string vocab_filename = "vocab.dat";
    std::ofstream vocab_out{vocab_filename};

    std::string kld_filename = "kld.dat";
    std::ofstream kld_out{kld_filename};
    
    for(auto x : vocab) {
        vocab_out << x.first << " " << x.second << std::endl;
    }

    for(auto x : kld) {
        kld_out << pow(x,params.power) << std::endl;
    }    
    
    vocab_out.close();
    kld_out.close();

    std::string fin_name2 = params.testFile;
    MSParaFile fin2{fin_name2};

    std::string vocabread_filename = "vocab.dat";
    std::ifstream vocabread_in{vocabread_filename};

    //auto vocab2 = ReadVocab(vocabread_in);
    auto vocab2 = vocab;
    
    auto docs2 = LearnPairSentence(vocab2,fin2);
    if(params.verbose == 1) timer.here_then_reset("\nConstructed Paragraphs.\n");
    fin2.setBegin();
    auto tag2 = LearnTag(fin2);
    if(params.verbose == 1) timer.here_then_reset("\nConstructed Tag.\n");

    int64_t n_rows2, n_cols2;
    n_rows2 = vocab2.size();
    n_cols2 = docs2.size();

    sp_mat inMat2(n_rows2, n_cols2);

    std::vector<SpValue> values2;

    fillValue(values2, vocab2, docs2);
    MakeTFKLD(params, kld, values2);
    fillMat(values2, vocab2, docs2, inMat2);
    
    if(params.verbose == 1) timer.here_then_reset("Filled the Matrix.\n");

    mat U2;
    vec s2;
    mat V2;

    svds(U2,s2,V2,inMat2,K_dim);

    if(params.verbose == 1) timer.here_then_reset("SVD is complete.\n");

    auto svec2 = makeSimMat(V2);
    normalizeSimMat(svec2);
    
    if(params.verbose == 1) timer.here_then_reset("Made Similarity Vectors.\n");

    std::ofstream fout2{"test_KLD.dat"};

    count = 0;
    lcount = 1;

    for(auto x : svec2) {
        fout2 << tag2[count] << " ";
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

void runTFKLD(Param const &params) {

    if(params.mode == 0) runInductive(params);
    if(params.mode == 1) runTransductive(params);
    
    }*/
    
}//namespace tfkld
