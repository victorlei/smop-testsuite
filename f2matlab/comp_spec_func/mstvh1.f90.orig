PROGRAM mstvh1
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:13

!       =====================================================
!       Purpose: This program computes Struve function
!                H1(x) using subroutine STVH1
!       Input :  x   --- Argument of H1(x) ( x ò 0 )
!       Output:  SH1 --- H1(x)
!       Example:
!                   x          H1(x)
!                -----------------------
!                  0.0       .00000000
!                  5.0       .80781195
!                 10.0       .89183249
!                 15.0       .66048730
!                 20.0       .47268818
!                 25.0       .53880362
!       =====================================================

IMPLICIT DOUBLE PRECISION (a-h,o-z)
WRITE(*,*)'Please enter x '
!        READ(*,*)X
x=25.0
WRITE(*,*)'   x          H1(x)'
WRITE(*,*)'-----------------------'
CALL stvh1(x,sh1)
WRITE(*,10)x,sh1
10      FORMAT(1X,f5.1,e16.8)
END PROGRAM mstvh1


SUBROUTINE stvh1(x,sh1)

!       =============================================
!       Purpose: Compute Struve function H1(x)
!       Input :  x   --- Argument of H1(x) ( x ò 0 )
!       Output:  SH1 --- H1(x)
!       =============================================



DOUBLE PRECISION, INTENT(IN)             :: x
DOUBLE PRECISION, INTENT(OUT)            :: sh1
IMPLICIT DOUBLE PRECISION (a-h,o-z)
pi=3.141592653589793D0
r=1.0D0
IF (x <= 20.0D0) THEN
  s=0.0D0
  a0=-2.0D0/pi
  DO  k=1,60
    r=-r*x*x/(4.0D0*k*k-1.0D0)
    s=s+r
    IF (DABS(r) < DABS(s)*1.0D-12) EXIT
  END DO
  15         sh1=a0*s
ELSE
  s=1.0D0
  km=INT(.5*x)
  IF (x > 50.d0) km=25
  DO  k=1,km
    r=-r*(4.0D0*k*k-1.0D0)/(x*x)
    s=s+r
    IF (DABS(r) < DABS(s)*1.0D-12) EXIT
  END DO
  25         t=4.0D0/x
  t2=t*t
  p1=((((.42414D-5*t2-.20092D-4)*t2+.580759D-4)*t2  &
      -.223203D-3)*t2+.29218256D-2)*t2+.3989422819D0
  q1=t*(((((-.36594D-5*t2+.1622D-4)*t2-.398708D-4)*  &
      t2+.1064741D-3)*t2-.63904D-3)*t2+.0374008364D0)
  ta1=x-.75D0*pi
  by1=2.0D0/DSQRT(x)*(p1*DSIN(ta1)+q1*DCOS(ta1))
  sh1=2.0/pi*(1.0D0+s/(x*x))+by1
END IF
RETURN
END SUBROUTINE stvh1
