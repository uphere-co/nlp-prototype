#include <iostream>

template <class T>
void testfunction ( std::vector<T>* a ) {
  std::cout << a->size() << std::endl;
}

#define Wrap_testfunction(T)				\
  inline void testfunction_ ## T (void* a) {            \
    std::vector<T>* p_a = reinterpret_cast< std::vector<T>* >(a);	\
    testfunction(p_a);  					\
  }                                                     \
  auto wrap_length_inline = testfunction_ ## T ;




//MYFUNCWRAP(int)
