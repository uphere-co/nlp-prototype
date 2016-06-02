#include <math.h>
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

void test_matrix_vector()
{
  static float dat[N];
  static float matrix[100][200];
  static float vector[200];
  static float rvector[200];
  static float bvector[200];

  int i, j, k, n ;
  float sum ; 
  load_file( dat );

  for( n = 0 ; n < 10000 ; n++ ) { 
  
  for( i = 0 ; i < 100 ; i++ ) {
    for(j = 0 ; j < 200 ; j++ ) {
      matrix[i][j] = dat[ n + i* 200 + j ];
    }
  }
  for( i = 0 ; i < 200 ; i++ ) { 
    vector[i] = dat[ 20000 + i ];
  }
  for( i = 0 ; i < 100 ; i++  ) {
    bvector[i] = dat[ 20200  +  i] ; 
  }

  for( i = 0 ; i < 100 ; i++ ) {
    rvector[i] = 0; 
    for( k = 0 ; k <200 ; k++ ) {
      rvector[i] += matrix[i][k]*vector[k];
    }
    
    rvector[i] += bvector[i];
    rvector[i] /= 100.0;
    rvector[i] = tanhf(rvector[i]);
  }
  sum = 0;
  for( i = 0; i < 100; i++ ) {
    //printf("%e\n" , rvector[i]);
    sum += rvector[i];
    
  }
  printf("sum = %f\n", sum );  
  }  
}
  

int main (int argc, char** argv)
{
  //test_vector_dot();
  test_matrix_square_trace();
  //test_matrix_vector();
  return 0;
}
