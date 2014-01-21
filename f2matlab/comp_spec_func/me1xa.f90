PROGRAM me1xa
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:10

!       =========================================================
!       Purpose: This program computes the exponential integral
!                E1(x) using subroutine E1XA
!       Input :  x  --- Argument of E1(x)  ( x > 0 )
!       Output:  E1 --- E1(x)
!       Example:
!                  x        E1(x)
!                ----------------------
!                 0.0     .1000000+301
!                 1.0     .2193839E+00
!                 2.0     .4890051E-01
!                 3.0     .1304838E-01
!                 4.0     .3779352E-02
!                 5.0     .1148296E-02
!       =========================================================

DOUBLE PRECISION :: e1,x
WRITE(*,*)'Please enter x '
!        READ(*,*) X
x=5.0
WRITE(*,*)'   x        E1(x)'
WRITE(*,*)' ----------------------'
CALL e1xa(x,e1)
WRITE(*,10)x,e1
10      FORMAT(1X,f5.1,e17.7)
END PROGRAM me1xa


SUBROUTINE e1xa(x,e1)

!       ============================================
!       Purpose: Compute exponential integral E1(x)
!       Input :  x  --- Argument of E1(x)
!       Output:  E1 --- E1(x) ( x > 0 )
!       ============================================



DOUBLE PRECISION, INTENT(IN)             :: x
DOUBLE PRECISION, INTENT(OUT)            :: e1
IMPLICIT DOUBLE PRECISION (a-h,o-z)
IF (x == 0.0) THEN
  e1=1.0D+300
ELSE IF (x <= 1.0) THEN
  e1=-DLOG(x)+((((1.07857D-3*x-9.76004D-3)*x+5.519968D-2)*x  &
      -0.24991055D0)*x+0.99999193D0)*x-0.57721566D0
ELSE
  es1=(((x+8.5733287401D0)*x+18.059016973D0)*x  &
      +8.6347608925D0)*x+0.2677737343D0
  es2=(((x+9.5733223454D0)*x+25.6329561486D0)*x  &
      +21.0996530827D0)*x+3.9584969228D0
  e1=DEXP(-x)/x*es1/es2
END IF
RETURN
END SUBROUTINE e1xa
