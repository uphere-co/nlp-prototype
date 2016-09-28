#pragma once

#include <string>
#include <iostream>
#include <vector>



#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include "/home/mo/repo/srcp/exercises/linear.h"
#define Malloc(type,n) (type *)malloc((n)*sizeof(type))
#define INF HUGE_VAL

namespace tfkld{

struct mParam
{
    std::string solver_type;
    int nr_class;
    int nr_feature;
    double bias;
    int *label;
    double *w;
};

void print_null(const char *s);
struct mParam* Do_Train(std::vector<std::string> &tag, std::vector<std::vector<float>> &svec);
void parse_command_line(int argc, char **argv, char *input_file_name, char *model_file_name);
void read_problem_mem(std::vector<std::string> &tag, std::vector<std::vector<float>> &svec);
void do_cross_validation();
void do_find_parameter_C();
    
}//namespace tfkld;
