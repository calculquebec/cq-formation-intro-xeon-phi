// Sample code reduction.cpp
// Example showing use of OpenMP 4.0 pragmas for offload calculation

# include <stdio.h>

#define SIZE 1000
#pragma omp declare target
int reduce(int *inarray)
{
  int sum = 0;
  int i;
#pragma omp target map(to:inarray[0:SIZE]) map(sum)
  {
    for(i = 0; i < SIZE; i++)
      {
	sum += inarray[i];
      }
  }
  return sum;
}

int main()
{
  int inarray[SIZE], sum, validSum, i;
  validSum=0;
  for(i = 0; i < SIZE; i++)
    {
      inarray[i] = i;
      validSum += i;
    }

  sum = 0;
  sum = reduce(inarray);
  printf("sum reduction = %d, validSum = %d\n", sum, validSum);
}
