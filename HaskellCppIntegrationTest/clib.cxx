
#include <vector>
#include <stdio.h>

extern "C" {
    void helloworld( std::vector<int> ) ;
}

void helloworld( std::vector<int> a )
{
  printf(" hello world \n");
}

void cliblinktest (void)
{
  printf("cliblinktest: hello world\n");
}

