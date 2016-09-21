#include "tests/test01.h"

namespace tfkld{
namespace test{

void runTFKLD_test01(Param const &params){

    using namespace util;
    using namespace util::io;
    using namespace tfkld;
    using namespace tfkld::type;
    using namespace arma;

    auto timer = Timer{};

    int K_dim = params.kdim;
    
    std::string fin_name = params.trainFile;
    MSParaFile fin{fin_name};

    auto vocab = LearnVocab(fin);
    timer.here_then_reset("\nConstructed Vocabulary.\n");
    fin.setBegin();
    auto docs = LearnPara(vocab,fin);
    timer.here_then_reset("\nConstructed Paragraphs.\n");
    fin.setBegin();
    auto tag = LearnTag(fin);
    timer.here_then_reset("\nConstructed Tag.\n");
    
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

    timer.here_then_reset("Filled the Matrix.\n");
    
    mat U;
    vec s;
    mat V;

    svds(U,s,V,inMat,K_dim);

    timer.here_then_reset("SVD is complete.\n");

    auto svec = makeSimMat(V);

    timer.here_then_reset("Made Similarity Vectors.\n");
    
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

    timer.here_then_reset("Writing Similarity Vectors is Done.\n");

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
    
    auto docs2 = LearnPara(vocab2,fin2);
    timer.here_then_reset("\nConstructed Paragraphs.\n");
    fin2.setBegin();
    auto tag2 = LearnTag(fin2);
    timer.here_then_reset("\nConstructed Tag.\n");

    int64_t n_rows2, n_cols2;
    n_rows2 = vocab2.size();
    n_cols2 = docs2.size();

    sp_mat inMat2(n_rows2, n_cols2);

    std::vector<SpValue> values2;

    fillValue(values2, vocab2, docs2);
    MakeTFKLD(params, kld, values2);
    fillMat(values2, vocab2, docs2, inMat2);
    
    timer.here_then_reset("Filled the Matrix.\n");

    mat U2;
    vec s2;
    mat V2;

    svds(U2,s2,V2,inMat2,K_dim);

    timer.here_then_reset("SVD is complete.\n");

    auto svec2 = makeSimMat(V2);
    normalizeSimMat(svec2);
    
    timer.here_then_reset("Made Similarity Vectors.\n");

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

    timer.here_then_reset("Writing Similarity Vectors is Done.\n");


    fout2.close();
    
}

}//namespace test
}//namespace tfkld
