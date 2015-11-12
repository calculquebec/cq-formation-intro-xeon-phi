PROGRAM hello

USE omp_lib
IMPLICIT NONE

INTEGER :: rank = 0, size = 1


!DIR$ OFFLOAD BEGIN target(mic)
!$OMP parallel private(rank)
    rank = OMP_GET_THREAD_NUM()
    size = OMP_GET_NUM_THREADS()
    WRITE(*,*) "Hello from processor ", rank, " of ", size
!$OMP END parallel
!DIR$ END OFFLOAD

END PROGRAM hello
