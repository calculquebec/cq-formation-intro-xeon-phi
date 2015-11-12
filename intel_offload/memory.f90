! This function will be compiled only for the target device (mic)

!DIR$ ATTRIBUTES OFFLOAD : mic :: add2
SUBROUTINE add2(data)
        integer, dimension(1) :: data
        data(1) = data(1) + 2
END SUBROUTINE add2


PROGRAM memory

USE mic_lib

IMPLICIT none

!DIR$ ATTRIBUTES OFFLOAD : mic :: add2

!DIR$ OPTIONS /offload_attribute_target=mic
      integer, dimension(1) :: data
!DIR$ END OPTIONS

      data(1) = 5
!DIR$ OFFLOAD BEGIN target(mic:0) 
!DIR& in(data: length(1)) 
!DIR& out(data: length(1)) 
     call add2(data)
!DIR$ END OFFLOAD
   WRITE(*,*)  'Data: ', data(1)

END PROGRAM memory
