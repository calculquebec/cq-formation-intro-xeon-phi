#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <math.h>

#define Niter 100  //Number of loop iterations
#define SIZE 64000 //Size of arrays

#pragma omp declare target
//This function will be compiled only for the target device (mic)
void some_work(float *in, float *out)
//Computes the sin squared for each element of in
{
   int i;
//OpenMP pragma for parallel for loop
//private(i) indicates each thread will have a private variable i
#pragma omp parallel for private(i)
   for (i = 0; i < SIZE; i++)
   {
      double sinx = sin( in[i] );
      out[i] = sinx*sinx;
   }
}
#pragma omp end declare target

double get_time()
//Returns the current time in seconds
{
   struct timeval tp;
   gettimeofday(&tp, NULL);
   return (double)(tp.tv_sec) + (double)(tp.tv_usec)/1E6;
}


int main(int argc, char* argv[])
{
   int i;
   double tstart, tend;
   float *in_array, *out_array;

   //  Allocate arrays and check for errors
   in_array = (float*)malloc(SIZE*sizeof(float));
   out_array = (float*)malloc(SIZE*sizeof(float));
   if (!in_array || !out_array)
   {
      printf("*** Error - Allocation error\n");
      return 0;
   }

   // Initialize arrays
   for (i = 0; i < SIZE; i++)
   {
      in_array[i] = (float) (i + 1);
      out_array[i] = 0;
   }

   //Start the timer
   tstart = get_time();
   
   for(i = 0; i < Niter; i++)
   {
//Pragma for offloading work onto the target
//The array in_array will be copied from host->target at the start
//out_array will be copied from target->host at the end
#pragma omp target map(to:in_array[0:SIZE]) map(from:out_array[0:SIZE])
       {
           some_work(in_array, out_array);
       }
   }
   //Stop the clock
   tend = get_time();

   // Print time
   printf("Time:  %.3lf seconds\n", (double) tend - tstart);
   free(in_array);
   free(out_array);

   return 0;
}
