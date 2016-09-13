#include <armadillo>
#include <fstream>
#include <set>
#include <sstream>
#include <string>
#include <algorithm>
#include <map>
#include <iterator>
#include <unordered_set>
#include <utility>

#include "src/TFKLD.h"

#include "utils/hdf5.h"
#include "utils/string.h"
#include "utils/profiling.h"

#include <boost/algorithm/string.hpp>

using namespace util;
using namespace util::io;


int main(){

    using namespace tfkld;
    using namespace arma;

    auto timer = Timer{};

    int K_dim = 100;
    
    std::string fin_name = "Tk_msr_paraphrase_train.txt";
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
    std::vector<float_t> idf;
    std::vector<float_t> kld;
    int64_t count = 0;

    fillValue(values, count, vocab, docs);
    //MakeTFIDF(idf, values, count, vocab, docs);
    MakeTFKLD(kld, tag, values, count, vocab, docs);
    fillMat(values, count, vocab, docs, inMat);

    timer.here_then_reset("Filled the Matrix.\n");
    
    mat U;
    vec s;
    mat V;

    svds(U,s,V,inMat,K_dim);

    timer.here_then_reset("SVD is complete.\n");

    auto svec = makeSimMat(V);

    timer.here_then_reset("Made Similarity Vectors.\n");
    
    std::ofstream fout{"train_KLD.dat"};

    count = 0;
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
        kld_out << x << std::endl;
    }


    
    /*
    H5file file{H5name{"data.h5"}, hdf5::FileMode::create};
    auto vocab_word = getVocabWord(vocab);
    auto vocab_index = getVocabIndex(vocab);
    std::vector<char> concat_words = Concat(vocab_word);
    file.writeRawData(H5name{"MSR.training.vocab.word"},concat_words);
    file.writeRawData(H5name{"MSR.training.vocab.index"},vocab_index);
    */

    
    vocab_out.close();
    kld_out.close();

    std::string fin_name2 = "Tk_msr_paraphrase_test.txt";
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
    int64_t count2 = 0;

    fillValue(values2, count2, vocab2, docs2);
    //MakeTFIDF(idf, values2);
    MakeTFKLD(kld, values2);
    fillMat(values2, count2, vocab2, docs2, inMat2);
    
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
    
    return 0;
}
