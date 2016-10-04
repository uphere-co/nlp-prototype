#include "utils/help.h"

namespace tfkld{
namespace util{

void printHelp() {
  std::cout << "c++ TF-KLD implementation \n\n"
            << "Options:\n"
            << "Parameters for running:\n"
            << "\t-train <file>\n"
            << "\t\tUse text data from <file> to calculate the TF-KLD as a training file\n"
            << "\t-test <file>\n"
            << "\t\tUse text data from <file> to calculate the TF-KLD as a test file\n"
            << "\t-dim <int>\n"
            << "\t\tSet the cardinality of the latent space; default is 100\n"
            << "\t-power <float>\n"
            << "\t\tSet the power of TF-KLD value; default is 1.0\n"
            << "\t-mode <int>\n"
            << "\t\tChoose Inductive or Transductive learning;\n"
            << "\t\tdefault is Transductive learning (mode 1) (0 is Inductive learning)\n"
            << "\t-verbose <int>\n"
            << "\t\tVerbose mode; default is 0\n"
            << "\nExamples:\n"
            << "./tfkld -train train.txt -test test.txt -dim 200 -power 1.5 -mode 1 -verbose 1\n\n";
  
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
  
  if ((i = ArgPos((char *)"-train", argc, argv)) > 0) params.trainFile = argv[i + 1];
  if ((i = ArgPos((char *)"-test", argc, argv)) > 0) params.testFile = argv[i + 1];
  if ((i = ArgPos((char *)"-dim", argc, argv)) > 0) params.kdim = atoi(argv[i + 1]);
  if ((i = ArgPos((char *)"-power", argc, argv)) > 0) params.power = atof(argv[i + 1]);
  if ((i = ArgPos((char *)"-mode", argc, argv)) > 0) params.mode = atoi(argv[i + 1]);
  if ((i = ArgPos((char *)"-verbose", argc, argv)) > 0) params.verbose = atoi(argv[i + 1]);
}
    
}//namespace of util
}//namespace of tfkld
