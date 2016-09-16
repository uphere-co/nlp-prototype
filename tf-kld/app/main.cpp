#include "src/Matrix.h"
#include "src/Vocab.h"
#include "src/TFKLD.h"

#include "tests/test01.h"

int main(){
    
    using namespace util;
    using namespace util::io;
    using namespace tfkld;
    using namespace arma;


    runTFKLD_test01();
    
    return 0;
}
