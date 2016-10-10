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
#include "liblinear/linear.h"
#define Malloc(type,n) (type *)malloc((n)*sizeof(type))
#define INF HUGE_VAL

namespace tfkld{
namespace svm{
struct mParam
{
    std::string solver_type;
    int nr_class;
    int nr_feature;
    double bias;
    int *label;
    double *w;
};

struct SVM_param {
    std::string solver_type;
    int nr_class;
    int nr_feature;
    double bias;
    std::vector<int> label;
    std::vector<double> w;
    
};
 
namespace training{

void print_null(const char *s);
struct SVM_param Do_Train(std::vector<std::string> &tag, std::vector<std::vector<float>> &svec);
void parse_command_line(int argc, char **argv, char *input_file_name, char *model_file_name);
void read_problem_mem(std::vector<std::string> &tag, std::vector<std::vector<float>> &svec);
void do_cross_validation();
void do_find_parameter_C();
}//namespace train

namespace predicting{

int print_null_p(const char *s,...);
void exit_input_error_p(int line_num);
static char* readline(FILE *input);
int do_one_predict(std::vector<std::string> &tag, std::vector<std::vector<float>> &svec);
void do_predict(std::vector<std::string> &tag, std::vector<std::vector<float>> &svec);
void exit_with_help_p();
struct model *load_model_mem(SVM_param svmparam);
int onePredict(std::vector<std::string> &tag, std::vector<std::vector<float>> &svec, SVM_param svmparam);
void mainPredict(std::vector<std::string> &tag, std::vector<std::vector<float>> &svec, mParam *mparams);
    
}//namespace predicting
}//namespace svm
}//namespace tfkld
