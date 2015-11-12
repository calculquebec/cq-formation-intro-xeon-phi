#include <stdio.h>

#pragma offload_attribute(push, target(mic))
static int *data;
#pragma offload_attribute(pop)

//This function will be compiled only for the target device (mic)
__attribute__((target(mic))) void add2(int *a)
{
  *a = *a + 2;
}

int main(int argc, char* argv[])
{


   data = (int*)malloc(sizeof(int));
   *data = 5;
   //#pragma offload target(mic:0)			     \
     //       in(data   : length(1)  )			     \
     //   out(data : length(1)  )
#pragma offload target(mic:0) 
       {
           add2(data);
       }

   printf("data: %d\n", *data);
   free(data);
   return 0;
}
