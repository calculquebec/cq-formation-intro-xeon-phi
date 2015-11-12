! This function will be compiled only for the target device (mic)

PROGRAM memory

  IMPLICIT none

  integer :: dat

  dat = 5

!$OMP target map(dat)
    dat = dat+2
!$OMP end target

  WRITE(*,*)  'Data: ', dat

END PROGRAM memory
