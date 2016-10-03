#include "src/Matrix.h"
#include "src/Vocab.h"
#include "src/TFKLD.h"
#include "src/SVM.h"

#include "tests/test01.h"
#include "tests/test02.h"

#include "utils/h5io.h"
#include "utils/help.h"

int main(int argc, char **argv){

    using namespace tfkld;
    using namespace tfkld::test;
    using namespace tfkld::util;
    using namespace arma;
    using namespace tfkld::svm;
    using namespace tfkld::svm::training;
    using namespace tfkld::svm::predicting;

    Param params;

    if(argc < 2) {
        printHelp();
        return 1;
    } else {
        ArgPass(argc, argv, params);
    }

    ////////////////////////////////////////////

    std::vector<std::string> tag{"+1","-1","+1","-1"};
    std::vector<std::vector<float>> svec;
    std::vector<float> vec;
    mParam *mparams;
    vec.push_back(3);vec.push_back(1);vec.push_back(4);
    svec.push_back(vec);
    vec.clear();
    vec.push_back(3);vec.push_back(2);vec.push_back(4);
    svec.push_back(vec);
    vec.clear();
    vec.push_back(2);vec.push_back(6);vec.push_back(4);
    svec.push_back(vec);
    vec.clear();
    vec.push_back(1);vec.push_back(3);vec.push_back(7);
    svec.push_back(vec);
    vec.clear();

    mparams = Do_Train(tag,svec);
    int n;
    if(mparams->bias>=0)
        n=(mparams -> nr_feature)+1;
    else
        n=(mparams -> nr_feature);
    
    int w_size = n;
    int nr_w;
    if(mparams->nr_class==2 && mparams->solver_type != "MCSVM_CS")
        nr_w=1;
    else
        nr_w=mparams->nr_class;

    mainPredict(tag, svec, mparams);

    return 0;
}
