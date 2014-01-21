PROGRAM mlagzo
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:11

!       ===========================================================
!       Purpose : This program computes the zeros of Laguerre
!                 polynomial Ln(x) in the interval [0,ì] and the
!                 corresponding weighting coefficients for Gauss-
!                 Laguerre integration using subroutine LAGZO
!       Input :   n    --- Order of the Laguerre polynomial
!                 X(n) --- Zeros of the Laguerre polynomial
!                 W(n) --- Corresponding weighting coefficients
!       ===========================================================

IMPLICIT DOUBLE PRECISION (a-h,o-z)
DIMENSION x(100),w(100)
WRITE(*,*)'Please enter the order of Ln(x), n '
!        READ(*,*)N
n=5
WRITE(*,20)n
CALL lagzo(n,x,w)
WRITE(*,*)'  Nodes and weights for Gauss-Lagurre integration'
WRITE(*,*)
WRITE(*,*)'  i             xi                      Wi'
WRITE(*,*)' -----------------------------------------', '------------'
DO  j=1,n
  WRITE(*,30)j,x(j),w(j)
END DO
20      FORMAT(1X,'n =',i3)
30      FORMAT(1X,i3,3X,d22.13,3X,d22.13)
END PROGRAM mlagzo


SUBROUTINE lagzo(n,x,w)

!       =========================================================
!       Purpose : Compute the zeros of Laguerre polynomial Ln(x)
!                 in the interval [0,ì], and the corresponding
!                 weighting coefficients for Gauss-Laguerre
!                 integration
!       Input :   n    --- Order of the Laguerre polynomial
!                 X(n) --- Zeros of the Laguerre polynomial
!                 W(n) --- Corresponding weighting coefficients
!       =========================================================


INTEGER, INTENT(IN)                      :: n
DOUBLE PRECISION, INTENT(IN OUT)         :: x(n)
DOUBLE PRECISION, INTENT(OUT)            :: w(n)
IMPLICIT DOUBLE PRECISION (a-h,o-z)


hn=1.0D0/n
DO  nr=1,n
  IF (nr == 1) z=hn
  IF (nr > 1) z=x(nr-1)+hn*nr**1.27
  it=0
  DO
    10         it=it+1
    z0=z
    p=1.0D0
    DO  i=1,nr-1
      p=p*(z-x(i))
    END DO
    f0=1.0D0
    f1=1.0D0-z
    DO  k=2,n
      pf=((2.0D0*k-1.0D0-z)*f1-(k-1.0D0)*f0)/k
      pd=k/z*(pf-f1)
      f0=f1
      f1=pf
    END DO
    fd=pf/p
    q=0.0D0
    DO  i=1,nr-1
      wp=1.0D0
      DO  j=1,nr-1
        IF (.NOT.(j == i)) wp=wp*(z-x(j))
      END DO
      q=q+wp
    END DO
    gd=(pd-q*fd)/p
    z=z-fd/gd
    IF (.NOT.(it <= 40.AND.DABS((z-z0)/z) > 1.0D-15)) EXIT
  END DO
  x(nr)=z
  w(nr)=1.0D0/(z*pd*pd)
END DO
RETURN
END SUBROUTINE lagzo
