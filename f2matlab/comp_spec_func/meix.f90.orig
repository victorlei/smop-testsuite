PROGRAM meix
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:10

!       =========================================================
!       Purpose: This program computes the exponential integral
!                Ei(x) using subroutine EIX
!       Example:
!                  x        Ei(x)
!                -----------------------
!                  0    -.10000000+301
!                  1     .18951178E+01
!                  2     .49542344E+01
!                  3     .99338326E+01
!                  4     .19630874E+02
!                  5     .40185275E+02
!       =========================================================

DOUBLE PRECISION :: ei,x
WRITE(*,*)'Please enter x '
!        READ(*,*)X
x=5
WRITE(*,*)
WRITE(*,*)'   x         Ei(x)'
WRITE(*,*)'------------------------'
CALL eix(x,ei)
WRITE(*,10)x,ei
10      FORMAT(1X,f5.1,e18.8)
END PROGRAM meix


SUBROUTINE eix(x,ei)

!       ============================================
!       Purpose: Compute exponential integral Ei(x)
!       Input :  x  --- Argument of Ei(x)
!       Output:  EI --- Ei(x) ( x > 0 )
!       ============================================



DOUBLE PRECISION, INTENT(IN)             :: x
DOUBLE PRECISION, INTENT(OUT)            :: ei
IMPLICIT DOUBLE PRECISION (a-h,o-z)
IF (x == 0.0) THEN
  ei=-1.0D+300
ELSE IF (x <= 40.0) THEN
  ei=1.0D0
  r=1.0D0
  DO  k=1,100
    r=r*k*x/(k+1.0D0)**2
    ei=ei+r
    IF (DABS(r/ei) <= 1.0D-15) EXIT
  END DO
  20         ga=0.5772156649015328D0
  ei=ga+DLOG(x)+x*ei
ELSE
  ei=1.0D0
  r=1.0D0
  DO  k=1,20
    r=r*k/x
    ei=ei+r
  END DO
  ei=DEXP(x)/x*ei
END IF
RETURN
END SUBROUTINE eix
