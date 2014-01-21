PROGRAM mstvh0
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:13

!       ====================================================
!       Purpose: This program computes Struve function
!                H0(x) using subroutine STVH0
!       Input :  x   --- Argument of H0(x) ( x ò 0 )
!       Output:  SH0 --- H0(x)
!       Example:
!                   x          H0(x)
!                ----------------------
!                  0.0       .00000000
!                  5.0      -.18521682
!                 10.0       .11874368
!                 15.0       .24772383
!                 20.0       .09439370
!                 25.0      -.10182519
!       ====================================================

IMPLICIT DOUBLE PRECISION (a-h,o-z)
WRITE(*,*)'Please enter x '
!        READ(*,*)X
x=25.0
WRITE(*,*)'   x          H0(x)'
WRITE(*,*)'----------------------'
CALL stvh0(x,sh0)
WRITE(*,10)x,sh0
10      FORMAT(1X,f5.1,e16.8)
END PROGRAM mstvh0


SUBROUTINE stvh0(x,sh0)

!       =============================================
!       Purpose: Compute Struve function H0(x)
!       Input :  x   --- Argument of H0(x) ( x ò 0 )
!       Output:  SH0 --- H0(x)
!       =============================================



DOUBLE PRECISION, INTENT(IN)             :: x
DOUBLE PRECISION, INTENT(OUT)            :: sh0
IMPLICIT DOUBLE PRECISION (a-h,o-z)
pi=3.141592653589793D0
s=1.0D0
r=1.0D0
IF (x <= 20.0D0) THEN
  a0=2.0*x/pi
  DO  k=1,60
    r=-r*x/(2.0D0*k+1.0D0)*x/(2.0D0*k+1.0D0)
    s=s+r
    IF (DABS(r) < DABS(s)*1.0D-12) EXIT
  END DO
  15         sh0=a0*s
ELSE
  km=INT(.5*(x+1.0))
  IF (x >= 50.0) km=25
  DO  k=1,km
    r=-r*((2.0D0*k-1.0D0)/x)**2
    s=s+r
    IF (DABS(r) < DABS(s)*1.0D-12) EXIT
  END DO
  25         t=4.0D0/x
  t2=t*t
  p0=((((-.37043D-5*t2+.173565D-4)*t2-.487613D-4)  &
      *t2+.17343D-3)*t2-.1753062D-2)*t2+.3989422793D0
  q0=t*(((((.32312D-5*t2-.142078D-4)*t2+.342468D-4)*  &
      t2-.869791D-4)*t2+.4564324D-3)*t2-.0124669441D0)
  ta0=x-.25D0*pi
  by0=2.0D0/DSQRT(x)*(p0*DSIN(ta0)+q0*DCOS(ta0))
  sh0=2.0D0/(pi*x)*s+by0
END IF
RETURN
END SUBROUTINE stvh0
