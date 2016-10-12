#include "src/Query.h"

namespace tfkld{

double calculateDistance(arma::mat a, arma::mat b) {

    double sum = 0;
    for(int i = 0; i < a.n_cols; i++) {
        sum += a(i)*b(i);
    }
    return sum;
}
   
void searchSentence(svm::SVM_param svmparam, Documents &document, std::string sen) {

    Documents resultDocs;
    resultDocs = document;

    hashmap_t doc = document.makeSentoDoc(sen);

    resultDocs.docs.push_back(doc);
    resultDocs.values.clear();

    fillValue(resultDocs);
    arma::sp_mat inMat;
    fillMat(resultDocs,inMat);
    arma::mat U;
    arma::vec s;
    arma::mat V;

    
    arma::svds(U,s,V,inMat,resultDocs.K_dim);


    
    
    int V_last_row = V.n_rows - 1;

    MSParaFile inFile{"Tk_msr_paraphrase_train.txt"};
    std::vector<std::string> raw_sentence = get_raw_sentence(inFile);

    std::vector<std::string> tag;
    std::vector<std::vector<float>> svec;
    std::vector<float> vec;
    
    tag.push_back("+1");

    double neg_sum = 0;
    for(int i=0;i<V.n_rows - 1; i++) {
        //double distance = dot(V.row(i),V.row(V_last_row));
        for(int w=0;w<resultDocs.K_dim;w++) {
            vec.push_back(V.row(i)[w] + V.row(V_last_row)[w]);
        }
        for(int w=0;w<resultDocs.K_dim;w++) {
            vec.push_back(std::abs(V.row(i)[w] - V.row(V_last_row)[w]));
        }


        svec.push_back(vec);


        int q = svm::predicting::onePredict(tag, svec, svmparam);
        if(q != 0) std::cout << raw_sentence[i] << std::endl;
        
        vec.clear();
        svec.clear();
        neg_sum=0;
    }
            
    
}

}
