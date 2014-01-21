PROGRAM mitsl0
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:11

!       ===========================================================
!       Purpose: This program evaluates the integral of modified
!                Struve function L0(t) with respect to t from 0
!                to x using subroutine ITSL0
!       Input :  x   --- Upper limit  ( x ò 0 )
!       Output:  TL0 --- Integration of L0(t) from 0 to x
!       Example:
!                      x        L0(t)dt
!                   -----------------------
!                     0.0    .0000000D+00
!                     5.0    .3003079D+02
!                    10.0    .2990773D+04
!                    15.0    .3526179D+06
!                    20.0    .4475860D+08
!                    30.0    .7955389D+12
!                    40.0    .1508972D+17
!                    50.0    .2962966D+21
!       ===========================================================

IMPLICIT DOUBLE PRECISION (a-h,o-z)
WRITE(*,*)'Please enter x '
!        READ(*,*)X
x=50.0
WRITE(*,*)'   x        L0(t)dt'
WRITE(*,*)'-----------------------'
CALL itsl0(x,tl0)
WRITE(*,10)x,tl0
10      FORMAT(1X,f5.1,d16.7)
END PROGRAM mitsl0


SUBROUTINE itsl0(x,tl0)

!       ===========================================================
!       Purpose: Evaluate the integral of modified Struve function
!                L0(t) with respect to t from 0 to x
!       Input :  x   --- Upper limit  ( x ò 0 )
!       Output:  TL0 --- Integration of L0(t) from 0 to x
!       ===========================================================


DOUBLE PRECISION, INTENT(IN)             :: x
DOUBLE PRECISION, INTENT(OUT)            :: tl0
IMPLICIT DOUBLE PRECISION (a-h,o-z)
DIMENSION a(18)

pi=3.141592653589793D0
r=1.0D0
IF (x <= 20.0) THEN
  s=0.5D0
  DO  k=1,100
    rd=1.0D0
    IF (k == 1) rd=0.5D0
    r=r*rd*k/(k+1.0D0)*(x/(2.0D0*k+1.0D0))**2
    s=s+r
    IF (DABS(r/s) < 1.0D-12) EXIT
  END DO
  15         tl0=2.0D0/pi*x*x*s
ELSE
  s=1.0D0
  DO  k=1,10
    r=r*k/(k+1.0D0)*((2.0D0*k+1.0D0)/x)**2
    s=s+r
    IF (DABS(r/s) < 1.0D-12) EXIT
  END DO
  25         el=.57721566490153D0
  s0=-s/(pi*x*x)+2.0D0/pi*(DLOG(2.0D0*x)+el)
  a0=1.0D0
  a1=5.0D0/8.0D0
  a(1)=a1
  DO  k=1,10
    af=((1.5D0*(k+.50D0)*(k+5.0D0/6.0D0)*a1-.5D0*  &
        (k+.5D0)**2*(k-.5D0)*a0))/(k+1.0D0)
    a(k+1)=af
    a0=a1
    a1=af
  END DO
  ti=1.0D0
  r=1.0D0
  DO  k=1,11
    r=r/x
    ti=ti+a(k)*r
  END DO
  tl0=ti/DSQRT(2*pi*x)*DEXP(x)+s0
END IF
RETURN
END SUBROUTINE itsl0
