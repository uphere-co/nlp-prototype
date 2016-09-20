#include "utils/help.h"

namespace tfkld{
namespace util{

void printHelp() {
  std::cout << "c++ TF-KLD implementation \n\n";
  std::cout << "Options:\n";
  std::cout << "Parameters for running:\n";
  std::cout << "\t-train <file>\n";
  std::cout << "\t\tUse text data from <file> to calculate the TF-KLD as a training file\n";
  std::cout << "\t-test <file>\n";
  std::cout << "\t\tUse text data from <file> to calculate the TF-KLD as a test file\n";
  std::cout << "\t-dim <int>\n";
  std::cout << "\t\tSet the cardinality of the latent space; default is 100\n";
  std::cout << "\t-power <float>\n";
  std::cout << "\t\tSet the power of TF-KLD value; default is 1.0\n";
  std::cout << "\t-inductive <int>\n";
  std::cout << "\t\tChoose Inductive or Transductive learning; default is Inductive learning (0 is Transductive learning)\n";
  std::cout << "\nExamples:\n";
  std::cout << "./tfkld -train train.txt -test test.txt -dim 200 -power 1.5 -inductive 0\n\n";
  
}

int ArgPos(char *str, int argc, char **argv) {
  int a;
  for(a = 1; a < argc; a++) if (!std::strcmp(str, argv[a])) {
      if(a == argc - 1) {
          printf("Argument missing for %s\n", str);
          exit(1);
      }
      return a;
  }
  return -1;
}

void ArgPass(int argc, char **argv, Param &params) {
  int i;
  std::cout << params.trainFile << std::endl;
  
  if ((i = ArgPos((char *)"-train", argc, argv)) > 0) params.trainFile = argv[i + 1];
  if ((i = ArgPos((char *)"-test", argc, argv)) > 0) params.testFile = argv[i + 1];
  if ((i = ArgPos((char *)"-dim", argc, argv)) > 0) params.kdim = atoi(argv[i + 1]);
  if ((i = ArgPos((char *)"-power", argc, argv)) > 0) params.power = atof(argv[i + 1]);
  if ((i = ArgPos((char *)"-inductive", argc, argv)) > 0) params.inductive = atoi(argv[i + 1]);
}
    
}//namespace of util
}//namespace of tfkld
