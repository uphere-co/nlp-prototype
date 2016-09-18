#include "src/Matrix.h"
#include "src/Vocab.h"
#include "src/TFKLD.h"

#include "tests/test01.h"
#include "tests/test02.h"

#include "utils/help.h"

int main(int argc, char **argv){
    
    using namespace util;
    using namespace util::io;
    using namespace tfkld;
    using namespace tfkld::util;
    using namespace arma;

    Param params;

    if(argc < 2) {
        printHelp();
        return 1;
    } else {
        ArgPass(argc, argv, params);
    }
    
    //runTFKLD_test01(); // Inductive Learning
    runTFKLD_test02(params); // Transductive Learning
    return 0;
}
