#include <memory>

using namespace std;

template<class T> 
class unique_ptr_wrapper {
private:
    unique_ptr<T> p_uniq; 
public:
    unique_ptr_wrapper( unique_ptr<T>& p ) { p_uniq = std::move(p); }
    T* get() { return p_uniq.get(); }
};


#define unique_ptr_wrapper_type(T)                              \
  typedef unique_ptr_wrapper<T> unique_ptr_wrapper_ ## T ;      \
  typedef unique_ptr_wrapper_ ## T * T ## _p;

