#include "template.hh"

template void printout<int> (std::vector<int>*);

template std::vector<int>* create();

template void push_back<int> (std::vector<int>*, int);

