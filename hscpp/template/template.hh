#include <iostream>
#include <vector>

template <class T>
void printout ( std::vector<T>* xs ) {
  for( auto x : *xs ) { 
    std::cout << x << std::endl;
  }
}

#define w_printout(T)                                                   \
    extern "C" {                                                        \
	void w_printout_ ## T ( void* xs );                             \
    }                                                                   \
    inline void w_printout_ ## T ( void* xs ) {		                \
	std::vector<T>* xs0 = reinterpret_cast<std::vector<T>* >(xs);	\
	printout( xs0 );                                                \
    }                                                                   \
    auto a_printout_ ## T = w_printout_ ## T  ; 


template <class T>
void push_back( std::vector<T>* xs, T x ) {
  xs->push_back(x);
}


#define w_push_back(T)                                                  \
    extern "C" {                                                        \
	void w_push_back_ ## T ( void* xs, T x );	           	\
    }                                                                   \
    inline void w_push_back_ ## T ( void* xs, T x ) {			\
	std::vector<T>* xs0 = reinterpret_cast<std::vector<T>* >(xs);	\
	push_back( xs0,  x );				\
    }                                                                   \
    auto a_push_back_ ## T = w_push_back_ ## T  ; 


template <class T>
std::vector<T>* create( void ) {
  std::vector<T>* v = new std::vector<T>() ;
  return v;
}


#define w_create(T)                                                     \
    extern "C" {                                                        \
	void* w_create_ ## T ( void );	                        	\
    }                                                                   \
    inline void* w_create_ ## T () {	                 		\
	std::vector<T>* xs = create<T>();				\
	return reinterpret_cast<void*>(xs);                             \
    }                                                                   \
    auto a_create_ ## T = w_create_ ## T  ; 

