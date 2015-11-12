/***************************************************************************************************
 * FILE: openmp4x-mat-mat-multiplication.c
 *
 * INPUT: Nil
 *
 * OUTPUT: Displays Host and device output matrices
 *
 * CREATED: August,2013
 *
 * EMAIL: hpcfte@cdac.in
 *
 ***************************************************************************************************/


#include <stdio.h>

 #define SIZE 10
 #pragma omp declare target

int Mat_Mat_Mul(int(*Matrix_A)[SIZE], \
                int(*Matrix_B)[SIZE], \
                int(*Matrix_Device)[SIZE]) 
  
{
  int i, j, k, numthreads;
#pragma omp target map(Matrix_A[0:SIZE][0:SIZE])	\
  map(Matrix_B[0:SIZE][0:SIZE]) \
  map(Matrix_Device[0:SIZE][0:SIZE])
  {
#pragma omp parallel for
    for(i=0;i<SIZE;i++){
      numthreads = omp_get_num_threads();
      for(j=0;j<SIZE;j++){
	int sum=0;
	for(k=0;k<SIZE;k++)
	  sum+= Matrix_A[i][k] * Matrix_B[k][j];
	Matrix_Device[i][j] = sum;
      }
    }
  }
    return numthreads;
}

int main()
{
  int i, j, k, numthreads, numdevices;
  int Matrix_A[SIZE][SIZE], \
    Matrix_B[SIZE][SIZE], \
    Matrix_Host[SIZE][SIZE], \
    Matrix_Device[SIZE][SIZE]; 

  numdevices = omp_get_num_devices();
  printf("Number of devices: %d\n", numdevices);

  for(i=0; i<SIZE; i++){
    for(j=0; j<SIZE; j++){
      Matrix_A[i][j]=i+j;
      Matrix_B[i][j]=i+j;
      Matrix_Host[i][j]=0.0;
      Matrix_Device[i][j]=0.0;
    }
  }


  for(i=0;i<SIZE;i++){
    for(j=0;j<SIZE;j++){
      int sum=0;
      for(k=0;k<SIZE;k++)
	sum+= Matrix_A[i][k] * Matrix_B[k][j];
      Matrix_Host[i][j] = sum;
    }
  }
  printf("Host Matrix\n");
  for(i=0; i<SIZE; i++){
    for(j=0; j<SIZE; j++){
      printf("%d ",Matrix_Host[i][j]);
    }
    printf("\n");
  }
  

  numthreads = Mat_Mat_Mul(Matrix_A,Matrix_B,Matrix_Device);
  printf("Computing device matrix with %d threads\n", numthreads);    

  printf("Device Matrix\n");
  for(i=0; i<SIZE; i++){
    for(j=0; j<SIZE; j++){
      printf("%d ",Matrix_Device[i][j]);
    }
    printf("\n");
  }
}

 
