#include "src/Matrix.h"
#include "src/Vocab.h"
#include "src/TFKLD.h"

#include "tests/test01.h"
#include "tests/test02.h"

int main(){
    
    using namespace util;
    using namespace util::io;
    using namespace tfkld;
    using namespace arma;


    //runTFKLD_test01(); // Inductive Learning
    runTFKLD_test02(); // Transductive Learning
    return 0;
}
