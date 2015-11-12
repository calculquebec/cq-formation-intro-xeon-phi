PROGRAM axpy

USE omp_lib
IMPLICIT NONE


INTEGER*4, PARAMETER :: SIZE = 1048576
INTEGER*4, PARAMETER :: ITERATIONS = 200000
INTEGER*4, PARAMETER :: FLOPSPERCALC = 2


REAL, DIMENSION(:), ALLOCATABLE :: fa, fb
INTEGER :: i, j, k
INTEGER :: numthreads
INTEGER :: tstart, tstop, trate
DOUBLE PRECISION :: ttime
DOUBLE PRECISION :: gflops = 0.0
DOUBLE PRECISION :: a = 1.1

ALLOCATE(fa(SIZE))
ALLOCATE(fb(SIZE))

 call kmp_set_defaults("KMP_AFFINITY=compact")
!$OMP PARALLEL DO
DO i = 1, SIZE
   IF (i == 1) THEN
      numthreads = OMP_GET_NUM_THREADS()
   END IF
   fa(i) = REAL(i) + 0.1
   fb(i) = REAL(i) + 0.2
ENDDO

 WRITE(*,*) "OpenMP threads: ", numthreads

call system_clock(count_rate=trate) !Find the time rate
call system_clock(count=tstart) ! Start timer

!$OMP PARALLEL DO private(j)
DO i = 1, ITERATIONS
    DO j = 1, SIZE
        fa(j) = a*fa(j) + fb(j)
    ENDDO
ENDDO

call system_clock(count=tstop) ! Stop Timer
gflops = 1.0E-9 * REAL(ITERATIONS) * REAL(FLOPSPERCALC) * REAL(SIZE)
ttime = real(tstop - tstart)/real(trate)
 
IF (ttime .gt. 0.0) THEN
    WRITE(*,*) "GFLOPS = ", gflops
    WRITE(*,*) "SECS = ", ttime
    WRITE(*,*) "GFLOPS per sec = ", gflops/ttime
ENDIF

END PROGRAM axpy
