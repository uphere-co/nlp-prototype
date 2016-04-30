#include <stdio.h>

#define N 100000000
#define M 1000

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


void test_vector_dot( ) 
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
  }
  printf("sum = %e\n",sum);
}

void test_matrix_square_trace()
{
  static float dat[N];
  static float mat[M][M];

  int i,j,k ;
  float sum; 
  load_file( dat );

  for (i = 0 ; i < M ; i++) {
    for(j = 0 ; j < M ; j++) {
      float s;
      s =0;
      for(k = 0; k < M ; k++) {
	s += dat[i*M+k]*dat[k*M+j];
      }
      mat[i][j] = s;
    }
  }
  sum = 0 ;
  for (i =0 ; i < M ; i++ ) {
    sum += mat[i][i];
  }
  printf("trace = %e\n", sum ); 
}

int main (int argc, char** argv)
{
  //test_vector_dot();
  test_matrix_square_trace();
  return 0;
}
