PROGRAM mstvl0
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:13

!       =====================================================
!       Purpose: This program computes modified Struve
!                function L0(x) using subroutine STVL0
!       Input :  x   --- Argument of L0(x) ( x ò 0 )
!       Output:  SL0 --- L0(x)
!       Example:
!                   x        L0(x)
!               ------------------------
!                  0.0   .00000000D+00
!                  5.0   .27105917D+02
!                 10.0   .28156522D+04
!                 15.0   .33964933D+06
!                 20.0   .43558283D+08
!                 30.0   .78167230D+12
!                 40.0   .14894775D+17
!                 50.0   .29325538D+21
!       =====================================================
IMPLICIT DOUBLE PRECISION (a-h,o-z)
WRITE(*,*)'Please enter x '
!        READ(*,*)X
x=50.0
WRITE(*,*)'   x        L0(x)'
WRITE(*,*)'-----------------------'
CALL stvl0(x,sl0)
WRITE(*,10)x,sl0
10      FORMAT(1X,f5.1,d16.8)
END PROGRAM mstvl0


SUBROUTINE stvl0(x,sl0)

!       ================================================
!       Purpose: Compute modified Struve function L0(x)
!       Input :  x   --- Argument of L0(x) ( x ò 0 )
!       Output:  SL0 --- L0(x)
!       ================================================



DOUBLE PRECISION, INTENT(IN)             :: x
DOUBLE PRECISION, INTENT(OUT)            :: sl0
IMPLICIT DOUBLE PRECISION (a-h,o-z)
pi=3.141592653589793D0
s=1.0D0
r=1.0D0
IF (x <= 20.0D0) THEN
  a0=2.0D0*x/pi
  DO  k=1,60
    r=r*(x/(2.0D0*k+1.0D0))**2
    s=s+r
    IF (DABS(r/s) < 1.0D-12) EXIT
  END DO
  15         sl0=a0*s
ELSE
  km=INT(.5*(x+1.0))
  IF (x >= 50.0) km=25
  DO  k=1,km
    r=r*((2.0D0*k-1.0D0)/x)**2
    s=s+r
    IF (DABS(r/s) < 1.0D-12) EXIT
  END DO
  25         a1=DEXP(x)/DSQRT(2.0D0*pi*x)
  r=1.0D0
  bi0=1.0D0
  DO  k=1,16
    r=0.125D0*r*(2.0D0*k-1.0D0)**2/(k*x)
    bi0=bi0+r
    IF (DABS(r/bi0) < 1.0D-12) EXIT
  END DO
  35         bi0=a1*bi0
  sl0=-2.0D0/(pi*x)*s+bi0
END IF
RETURN
END SUBROUTINE stvl0
