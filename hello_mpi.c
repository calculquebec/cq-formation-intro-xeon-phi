
#include <stdio.h>
#include <mpi.h>

int main (int argc, char * argv[])

{

    int rank, size;

    MPI_Init( &argc, &argv );

    MPI_Comm_rank( MPI_COMM_WORLD,&rank );

    MPI_Comm_size( MPI_COMM_WORLD,&size );

#ifdef __MIC
    printf( "MIC: Hello from processor %d of %d\n", rank, size );
#else
    printf( "CPU: Hello from processor %d of %d\n", rank, size );
#endif
    MPI_Finalize();

    return 0;
}
