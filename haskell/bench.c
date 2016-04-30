#include <stdio.h>

#define N 100000000

void load_file( float dat[] )
{
  FILE* file;
  int i;
  
  file = fopen( "randomtest.dat", "r");

  for( i = 0 ; i < N ; i++ ) {
    fread( dat+i, sizeof(float) , 1 , file);
  }

  fclose(file);
}

int main (int argc, char** argv)
{

  static float dat[N];
  load_file( dat );

  float sum, e;
  int i ;
  float x, y , c, sumtemp;
  sum = 0; c = 0; e = 0;

  for( i = 0; i < N ; i++ ) {
    e = dat[i];
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
}
