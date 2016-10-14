#include "src/Matrix.h"
#include "src/Vocab.h"
#include "src/TFIDF.h"
#include "src/Type.h"
#include "src/Query.h"

#include "utils/help.h"

int main(int argc, char **argv){

    using namespace tfkld;
    using namespace tfkld::type;
    using namespace tfkld::util;

    Param params;

    if(argc < 2) {
        printHelp();
        return 1;
    } else {
        ArgPass(argc, argv, params);
    }

    
    Documents document;

    runTFIDF(params, document);
    
    return 0;
}
