PROGRAM me1xb
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:10

!       =========================================================
!       Purpose: This program computes the exponential integral
!                E1(x) using subroutine E1XB
!       Input :  x  --- Argument of E1(x)  ( x > 0 )
!       Output:  E1 --- E1(x)
!       Example:
!                  x          E1(x)
!                -------------------------
!                 0.0     .1000000000+301
!                 1.0     .2193839344E+00
!                 2.0     .4890051071E-01
!                 3.0     .1304838109E-01
!                 4.0     .3779352410E-02
!                 5.0     .1148295591E-02
!       =========================================================

DOUBLE PRECISION :: e1,x
WRITE(*,*)'Please enter x '
!        READ(*,*) X
x=5.0
WRITE(*,*)'   x          E1(x)'
WRITE(*,*)' -------------------------'
CALL e1xb(x,e1)
WRITE(*,10)x,e1
10      FORMAT(1X,f5.1,e20.10)
END PROGRAM me1xb


SUBROUTINE e1xb(x,e1)

!       ============================================
!       Purpose: Compute exponential integral E1(x)
!       Input :  x  --- Argument of E1(x)
!       Output:  E1 --- E1(x)  ( x > 0 )
!       ============================================



DOUBLE PRECISION, INTENT(IN)             :: x
DOUBLE PRECISION, INTENT(OUT)            :: e1
IMPLICIT DOUBLE PRECISION (a-h,o-z)
IF (x == 0.0) THEN
  e1=1.0D+300
ELSE IF (x <= 1.0) THEN
  e1=1.0D0
  r=1.0D0
  DO  k=1,25
    r=-r*k*x/(k+1.0D0)**2
    e1=e1+r
    IF (DABS(r) <= DABS(e1)*1.0D-15) EXIT
  END DO
  15         ga=0.5772156649015328D0
  e1=-ga-DLOG(x)+x*e1
ELSE
  m=20+INT(80.0/x)
  t0=0.0D0
  DO  k=m,1,-1
    t0=k/(1.0D0+k/(x+t0))
  END DO
  t=1.0D0/(x+t0)
  e1=DEXP(-x)*t
END IF
RETURN
END SUBROUTINE e1xb
