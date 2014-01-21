PROGRAM mstvl1
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:13

!       =====================================================
!       Purpose: This program computes the modified Struve
!                function L1(x) using subroutine STVL1
!       Input :  x   --- Argument of L1(x) ( x ò 0 )
!       Output:  SL1 --- L1(x)
!       Example:
!                     x        L1(x)
!                 -----------------------
!                   0.0   .00000000D+00
!                   5.0   .23728216D+02
!                  10.0   .26703583D+04
!                  15.0   .32812429D+06
!                  20.0   .42454973D+08
!                  30.0   .76853204D+12
!                  40.0   .14707396D+17
!                  50.0   .29030786D+21
!       =====================================================

IMPLICIT DOUBLE PRECISION (a-h,o-z)
WRITE(*,*)'Please enter x '
!        READ(*,*)X
x=50.0
WRITE(*,*)'   x        L1(x)'
WRITE(*,*)'-----------------------'
CALL stvl1(x,sl1)
WRITE(*,10)x,sl1
10      FORMAT(1X,f5.1,d16.8)
END PROGRAM mstvl1


SUBROUTINE stvl1(x,sl1)

!       ================================================
!       Purpose: Compute modified Struve function L1(x)
!       Input :  x   --- Argument of L1(x) ( x ò 0 )
!       Output:  SL1 --- L1(x)
!       ================================================



DOUBLE PRECISION, INTENT(IN)             :: x
DOUBLE PRECISION, INTENT(OUT)            :: sl1
IMPLICIT DOUBLE PRECISION (a-h,o-z)
pi=3.141592653589793D0
r=1.0D0
IF (x <= 20.0D0) THEN
  s=0.0D0
  DO  k=1,60
    r=r*x*x/(4.0D0*k*k-1.0D0)
    s=s+r
    IF (DABS(r) < DABS(s)*1.0D-12) EXIT
  END DO
  15         sl1=2.0D0/pi*s
ELSE
  s=1.0D0
  km=INT(.50*x)
  IF (x > 50) km=25
  DO  k=1,km
    r=r*(2.0D0*k+3.0D0)*(2.0D0*k+1.0D0)/(x*x)
    s=s+r
    IF (DABS(r/s) < 1.0D-12) EXIT
  END DO
  25         sl1=2.0D0/pi*(-1.0D0+1.0D0/(x*x)+3.0D0*s/x**4)
  a1=DEXP(x)/DSQRT(2.0D0*pi*x)
  r=1.0D0
  bi1=1.0D0
  DO  k=1,16
    r=-0.125D0*r*(4.0D0-(2.0D0*k-1.0D0)**2)/(k*x)
    bi1=bi1+r
    IF (DABS(r/bi1) < 1.0D-12) EXIT
  END DO
  35         sl1=sl1+a1*bi1
END IF
RETURN
END SUBROUTINE stvl1
