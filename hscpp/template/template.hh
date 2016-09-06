#include <iostream>
#include <vector>

template <class T>
void printout ( std::vector<T>* xs ) {
  for( auto x : *xs ) { 
    std::cout << x << std::endl;
  }
}

template <class T>
void push_back( std::vector<T>* xs, T x ) {
  xs->push_back(x);
}

template <class T>
std::vector<T>* create( void ) {
  std::vector<T>* v = new std::vector<T>() ;
  return v;
}

