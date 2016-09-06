#include <iostream>
#include <vector>
#include <stdio.h>

#include "template.hh"

extern "C" {
    void helloworld( std::vector<int> ) ;
}

extern "C" {
    void helloworld( std::vector<int> );
}

extern "C" {
  int myfuncwrapper(int); 
}

//#ifndef __MYFUNCWRAP__
MYFUNCWRAP(int)
//#endif

auto clib_inline = myfuncwrapper;
	 

template int myfunc<int> (int);

void helloworld( std::vector<int> a )
{
  printf(" hello world \n");
  myfuncwrapper( 3 );
}

void cliblinktest (void)
{
  printf("cliblinktest: hello world\n");
}

