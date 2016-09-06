#include <iostream>

template <class T>
void length ( T* a ) {
  std::cout << a->size() << std::endl;
}

#define Wrap_length(T) inline void wrap_length(T a) { length(a); }


//MYFUNCWRAP(int)
