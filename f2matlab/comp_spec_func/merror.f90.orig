PROGRAM merror
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:10

!       ===================================================
!       Purpose: This program computes the error function
!                erf(x) using subroutine ERROR
!       Input:   x   --- Argument of erf(x)
!       Output:  ERR --- erf(x)
!       Example:
!                  x         erf(x)
!                ---------------------
!                 1.0       .84270079
!                 2.0       .99532227
!                 3.0       .99997791
!                 4.0       .99999998
!                 5.0      1.00000000
!       ===================================================

IMPLICIT DOUBLE PRECISION (a-h,o-z)
WRITE(*,*)'Please enter x '
!        READ(*,*)X
x=1.0
WRITE(*,*)'   x         erf(x)'
WRITE(*,*)'---------------------'
CALL errorf(x,er)
WRITE(*,10)x,er
10      FORMAT(1X,f5.2,f15.8)
END PROGRAM merror


SUBROUTINE errorf(x,ERR)

!       =========================================
!       Purpose: Compute error function erf(x)
!       Input:   x   --- Argument of erf(x)
!       Output:  ERR --- erf(x)
!       =========================================



DOUBLE PRECISION, INTENT(IN)             :: x
DOUBLE PRECISION, INTENT(OUT)            :: ERR
IMPLICIT DOUBLE PRECISION (a-h,o-z)
eps=1.0D-15
pi=3.141592653589793D0
x2=x*x
IF (DABS(x) < 3.5D0) THEN
  er=1.0D0
  r=1.0D0
  DO  k=1,50
    r=r*x2/(k+0.5D0)
    er=er+r
    IF (DABS(r) <= DABS(er)*eps) EXIT
  END DO
  15         c0=2.0D0/DSQRT(pi)*x*DEXP(-x2)
  ERR=c0*er
ELSE
  er=1.0D0
  r=1.0D0
  DO  k=1,12
    r=-r*(k-0.5D0)/x2
    er=er+r
  END DO
  c0=DEXP(-x2)/(DABS(x)*DSQRT(pi))
  ERR=1.0D0-c0*er
  IF (x < 0.0) ERR=-ERR
END IF
RETURN
END SUBROUTINE errorf
