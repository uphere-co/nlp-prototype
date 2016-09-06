#include "template.hh"

extern "C" {
  int myfuncwrapper( int );
}

template int myfunc<int> (int);

void testtest( void )
{
  myfunc(203982);
  //myfuncwrapper(3);
}

MYFUNCWRAP(int)

// inline int myfuncwrapper(int a ) { return a; }

auto clib2_inline = myfuncwrapper;
