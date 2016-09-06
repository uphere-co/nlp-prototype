#include "template.hh"

template void printout<int> (std::vector<int>*);

template std::vector<int>* create();

template void push_back<int> (std::vector<int>*, int);


template void printout<double> (std::vector<double>*);

template std::vector<double>* create();

template void push_back<double> (std::vector<double>*, double);


