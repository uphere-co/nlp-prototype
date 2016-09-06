#include <iostream>
#include <vector>
#include <stdio.h>

#include "template.hh"

extern "C" {
  void testfunction_int(void* v); 
}

Wrap_testfunction(int)

// auto clib_inline = myfuncwrapper;
	 

//template int myfunc<int> (int);

// void helloworld( std::vector<int> a )
// {
//   printf(" hello world \n");
//   myfuncwrapper( 3 );
// }

// void cliblinktest (void)
// {
//   printf("cliblinktest: hello world\n");
// }

