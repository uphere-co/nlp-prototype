#pragma once

#include <cstring>
#include <iostream>

#include "src/Matrix.h"

namespace tfkld{
namespace util{

void printHelp();
int ArgPos(char *str, int argc, char **argv);
void ArgPass(int argc, char **argv, Param &params);
 
}
}
