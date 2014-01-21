PROGRAM mothpl
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:12

!       =========================================================
!       Purpose: This program computes orthogonal polynomials:
!                Tn(x) or Un(x) or Ln(x) or Hn(x), and their
!                derivatives using subroutine OTHPL
!       Input :  KF --- Function code
!                       KF=1 for Chebyshev polynomial Tn(x)
!                       KF=2 for Chebyshev polynomial Un(x)
!                       KF=3 for Laguerre polynomial Ln(x)
!                       KF=4 for Hermite polynomial Hn(x)
!                n ---  Order of orthogonal polynomials
!                x ---  Argument
!       Output:  PL(n) --- Tn(x) or Un(x) or Ln(x) or Hn(x)
!                DPL(n)--- Tn'(x) or Un'(x) or Ln'(x) or Hn'(x)
!                          n = 0,1,2,...,N ( N ó 100 )
!       =========================================================

IMPLICIT DOUBLE PRECISION (a-h,o-z)
DIMENSION pl(0:100),dpl(0:100)
WRITE(*,*)'KF,N,x = ?'
!        READ(*,*)KF,N,X
kf=4
n=5
x=1.2
WRITE(*,20)kf,n,x
WRITE(*,*)
CALL othpl(kf,n,x,pl,dpl)
IF (kf == 1) WRITE(*,*)'  n          Tn(x)            Tn''(X)'
IF (kf == 2) WRITE(*,*)'  n          Un(x)            Un''(X)'
IF (kf == 3) WRITE(*,*)'  n          Ln(x)            Ln''(X)'
IF (kf == 4) WRITE(*,*)'  n          Hn(x)            Hn''(X)'
WRITE(*,*)'-----------------------------------------'
DO  k=0,n
  WRITE(*,30)k,pl(k),dpl(k)
END DO
20      FORMAT(1X,3HKF=,i3,5X,5HNMAX=,i3,5X,2HX=,f6.3)
30      FORMAT(1X,i3,2D18.8)
END PROGRAM mothpl


SUBROUTINE othpl(kf,n,x,pl,dpl)

!       ==========================================================
!       Purpose: Compute orthogonal polynomials: Tn(x) or Un(x),
!                or Ln(x) or Hn(x), and their derivatives
!       Input :  KF --- Function code
!                       KF=1 for Chebyshev polynomial Tn(x)
!                       KF=2 for Chebyshev polynomial Un(x)
!                       KF=3 for Laguerre polynomial Ln(x)
!                       KF=4 for Hermite polynomial Hn(x)
!                n ---  Order of orthogonal polynomials
!                x ---  Argument of orthogonal polynomials
!       Output:  PL(n) --- Tn(x) or Un(x) or Ln(x) or Hn(x)
!                DPL(n)--- Tn'(x) or Un'(x) or Ln'(x) or Hn'(x)
!       =========================================================


INTEGER, INTENT(IN)                      :: kf
INTEGER, INTENT(IN)                      :: n
DOUBLE PRECISION, INTENT(IN)             :: x
DOUBLE PRECISION, INTENT(OUT)            :: pl(0:n)
DOUBLE PRECISION, INTENT(OUT)            :: dpl(0:n)
IMPLICIT DOUBLE PRECISION (a-h,o-z)


a=2.0D0
b=0.0D0
c=1.0D0
y0=1.0D0
y1=2.0D0*x
dy0=0.0D0
dy1=2.0D0
pl(0)=1.0D0
pl(1)=2.0D0*x
dpl(0)=0.0D0
dpl(1)=2.0D0
IF (kf == 1) THEN
  y1=x
  dy1=1.0D0
  pl(1)=x
  dpl(1)=1.0D0
ELSE IF (kf == 3) THEN
  y1=1.0D0-x
  dy1=-1.0D0
  pl(1)=1.0D0-x
  dpl(1)=-1.0D0
END IF
DO  k=2,n
  IF (kf == 3) THEN
    a=-1.0D0/k
    b=2.0D0+a
    c=1.0D0+a
  ELSE IF (kf == 4) THEN
    c=2.0D0*(k-1.0D0)
  END IF
  yn=(a*x+b)*y1-c*y0
  dyn=a*y1+(a*x+b)*dy1-c*dy0
  pl(k)=yn
  dpl(k)=dyn
  y0=y1
  y1=yn
  dy0=dy1
  dy1=dyn
END DO
RETURN
END SUBROUTINE othpl
