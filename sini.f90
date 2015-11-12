SUBROUTINE SINI(in, out, i)
!$OMP declare target
  DOUBLE PRECISION, INTENT(IN) :: in(*)
  DOUBLE PRECISION, INTENT(OUT) :: out(*)
  INTEGER, INTENT(IN) :: i

  DOUBLE PRECISION sinx

  sinx = sin( in(i) )
  out(i) = sinx*sinx
END SUBROUTINE SINI
