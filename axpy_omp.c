#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <omp.h>
#include <sys/time.h>

#define SIZE 64*1024*1024
#define ITERATIONS 1000
#define FLOPSPERCALC 2

double get_time()
{
  double tsecs = 0.0;
  struct timeval mytime;
  gettimeofday(&mytime, (struct timezone*)0);
  tsecs = (double)(mytime.tv_sec + mytime.tv_usec*1.0e-6);
  return tsecs;
}

#ifdef OFFLOAD
#pragma omp declare target
#endif
float fa[SIZE] __attribute__((align(64)));
float fb[SIZE] __attribute__((align(64)));
#ifdef OFFLOAD
#pragma omp end declare target
#endif

int main(int argc, char *argv[])
{
  int i,j,k;
  int numthreads;
  double tstart, tstop, ttime;
  double gflops = 0.0;
  float a=1.1;

#pragma omp parallel for proc_bind(close)
    for(i=0; i<SIZE; i++)
      {
	if (i==0) numthreads = omp_get_num_threads();
	fa[i] = (float)i + 0.1;
	fb[i] = (float)i + 0.2;
      }

    printf("OpenMP threads: %d\n", numthreads);

    tstart = get_time();
#ifdef OFFLOAD
    printf("OpenMP 4 Offload \n");
     #pragma omp target map(fa,fb)
#endif
#pragma omp parallel private(i) proc_bind(close)
    for (i=0; i < ITERATIONS; i++)
      {
#pragma omp for nowait
	for(j=0; j < SIZE; j++)
	  {
	    fa[j]=a*fa[j]+fb[j];
	  }
      }
    tstop = get_time();
    gflops = (double)( 1.0e-9 * ITERATIONS * FLOPSPERCALC * SIZE);
    ttime = tstop - tstart;
    if ((ttime) > 0.0)
      printf("GFLOPS = %10.3f, SECS = %10.3f, GFLOPS per sec = %10.3f\n",gflops,ttime,gflops/ttime);
    return(0);
}
