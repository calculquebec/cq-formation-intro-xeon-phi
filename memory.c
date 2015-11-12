#include <stdio.h>

int main(int argc, char* argv[])
{

   int data = 5;
   
   #pragma omp target map(data)
       {
           data += 2;
       }

   printf("data: %d\n", data);

   return 0;
}
