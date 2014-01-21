PROGRAM mitth0
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:11

!       ===========================================================
!       Purpose: This program evaluates the integral of H0(t)/t
!                with respect to t from x to infinity using
!                subroutine ITTH0
!       Input :  x   --- Lower limit  ( x ò 0 )
!       Output:  TTH --- Integration of H0(t)/t from x to infinity
!       Example:
!                    x        H0(t)/t dt
!                 -----------------------
!                   0.0      1.57079633
!                   5.0       .07954575
!                  10.0       .04047175
!                  15.0       .04276558
!                  20.0       .04030796
!                  30.0       .01815256
!                  40.0       .01621331
!                  50.0       .01378661
!       =======================================================

IMPLICIT DOUBLE PRECISION (a-h,o-z)
WRITE(*,*)'Please enter x '
!        READ(*,*)X
x=50.0
WRITE(*,*) '   x       H0(t)/t dt'
WRITE(*,*)'-----------------------'
CALL itth0(x,tth)
WRITE(*,10)x,tth
10      FORMAT(1X,f5.1,1X,e16.8)
END PROGRAM mitth0


SUBROUTINE itth0(x,tth)

!       ===========================================================
!       Purpose: Evaluate the integral H0(t)/t with respect to t
!                from x to infinity
!       Input :  x   --- Lower limit  ( x ò 0 )
!       Output:  TTH --- Integration of H0(t)/t from x to infinity
!       ===========================================================



DOUBLE PRECISION, INTENT(IN)             :: x
DOUBLE PRECISION, INTENT(OUT)            :: tth
IMPLICIT DOUBLE PRECISION (a-h,o-z)
pi=3.141592653589793D0
s=1.0D0
r=1.0D0
IF (x < 24.5D0) THEN
  DO  k=1,60
    r=-r*x*x*(2.0*k-1.0D0)/(2.0*k+1.0D0)**3
    s=s+r
    IF (DABS(r) < DABS(s)*1.0D-12) EXIT
  END DO
  15         tth=pi/2.0D0-2.0D0/pi*x*s
ELSE
  DO  k=1,10
    r=-r*(2.0*k-1.0D0)**3/((2.0*k+1.0D0)*x*x)
    s=s+r
    IF (DABS(r) < DABS(s)*1.0D-12) EXIT
  END DO
  25         tth=2.0D0/(pi*x)*s
  t=8.0D0/x
  xt=x+.25D0*pi
  f0=(((((.18118D-2*t-.91909D-2)*t+.017033D0)*t  &
      -.9394D-3)*t-.051445D0)*t-.11D-5)*t+.7978846D0
  g0=(((((-.23731D-2*t+.59842D-2)*t+.24437D-2)*t  &
      -.0233178D0)*t+.595D-4)*t+.1620695D0)*t
  tty=(f0*DSIN(xt)-g0*DCOS(xt))/(DSQRT(x)*x)
  tth=tth+tty
END IF
RETURN
END SUBROUTINE itth0
