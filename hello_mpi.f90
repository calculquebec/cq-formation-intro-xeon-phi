PROGRAM hello

USE mpi

INTEGER err, rank, size


    CALL MPI_Init(err)
   
    CALL MPI_Comm_rank (MPI_COMM_WORLD,rank, ierr)
   
    CALL MPI_Comm_size (MPI_COMM_WORLD,size, ierr)

    WRITE(*,*) 'Hello from processor ', rank, ' of ', size
    
    CALL  MPI_Finalize(err)

END PROGRAM hello
