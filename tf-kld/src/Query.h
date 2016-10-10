#pragma once

#include "src/Matrix.h"
#include "src/Vocab.h"
#include "src/SVM.h"

namespace tfkld{

    // List of sentences that met searching condition defined by threshold
void searchSentence(svm::SVM_param svmparam, Documents &document, std::string question);



}//namespace tfkld
