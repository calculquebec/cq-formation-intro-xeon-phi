PROGRAM hello

USE omp_lib
IMPLICIT NONE

INTEGER :: rank = 0, size = 1


!$OMP target
!$OMP parallel private(rank)
    rank = OMP_GET_THREAD_NUM()
    size = OMP_GET_NUM_THREADS()
    WRITE(*,*) "Hello from processor ", rank, " of ", size
!$OMP END parallel
!$OMP END target

END PROGRAM hello
