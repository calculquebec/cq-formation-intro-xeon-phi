PROGRAM offload

USE omp_lib
IMPLICIT NONE

INTEGER, PARAMETER :: Niter = 100  !Number of loop iterations
INTEGER, PARAMETER :: SIZE = 64000 !Size of arrays

!variables for looping and timing
INTEGER :: i, tstart, tend, trate



!Declare in_array and out_array on the target
!DIR$ OPTIONS /offload_attribute_target=mic
DOUBLE PRECISION, DIMENSION(SIZE) :: in_array, out_array
!DIR$ END OPTIONS

   ! Initialize arrays
   DO i = 1,SIZE
      in_array(i) = float(i+1)
      out_array(i) = 0
   ENDDO

   !Start the timer
   call system_clock(count_rate=trate) !Find the time rate
   call system_clock(count=tstart)     !Start Timer
   
   DO i = 1,Niter
!Pragma for offloading work onto the target
!The array in_array will be copied from host->target at the start
!out_array will be copied from target->host at the end
!DIR$ OFFLOAD BEGIN target(mic:0)
!DIR& in(in_array: length(SIZE))
!DIR& out(out_array: length(SIZE))
      call some_work(in_array, out_array)
!DIR$ END OFFLOAD
   ENDDO

   !Stop the clock
   call system_clock(count=tend)      ! Stop Timer


   !Print time
   WRITE(*,*) 'Time: ', real(tend-tstart)/real(trate), &
       ' s'

!This function will be compiled only for the target device (mic)

CONTAINS
!DIR$ ATTRIBUTES OFFLOAD : mic :: some_work
SUBROUTINE some_work(in, out)
!Computes the sin squared for each element of in
   INTEGER :: i
   DOUBLE PRECISION :: sinx
   DOUBLE PRECISION, DIMENSION(SIZE) :: in, out
!OpenMP pragma for parallel for loop
!$OMP PARALLEL DO
   DO i = 1, SIZE
      sinx = sin( in(i) )
      out(i) = sinx*sinx
   ENDDO
!$OMP END PARALLEL DO
END SUBROUTINE some_work
END PROGRAM offload
