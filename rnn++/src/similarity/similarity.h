#pragma once

#include "similarity/dep_similarity.h"

#include <iostream>

using namespace std;

class Engine {
  int n;
  vector<int> t; 
    
public:
  Engine(int m) {
    n = m;
    t.push_back(101);
    t.push_back(102);
    t.push_back(103);
  }; 
  virtual void showme( ) { cout << "Engine: " << n << endl; }
  virtual vector<int>* getVector() { return &t ; }
  virtual void addContents( vector<int>* v ) {
    for( auto& x : *v ) {
      t.push_back( x ); 
    }
  }


};
