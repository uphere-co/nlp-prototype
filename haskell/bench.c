#include <stdio.h>

int main (int argc, char** argv)
{
  FILE* file;
  file = fopen( "randomtest.dat", "r");

  float e;
  
  float sum;

  int i ;
  float x, y , c, sumtemp;
  sum = 0;
  c = 0;
  for( i = 0 ; i < 100000000 ; i++ ) {
    fread( &e , sizeof(float) , 1 , file);
    x = e*e;
    y = x-c;
    sumtemp = sum+y;
    c = (sumtemp - sum) - y;
    sum = sumtemp;
    // sum += e*e;
  }
  //sum = 0;
  //for( i = 0; i < 10000000 ; i++ ) {
  //  sum += e[i]*e[i];
  //}
  printf("%e",sum);
  fclose(file);
}
