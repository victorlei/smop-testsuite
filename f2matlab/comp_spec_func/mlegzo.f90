PROGRAM mlegzo
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:12

!       ============================================================
!       Purpose : This program computes the zeros of Legendre
!                 polynomial Pn(x) in the interval [-1,1] and the
!                 corresponding weighting coefficients for Gauss-
!                 Legendre integration using subroutine LEGZO
!       Input :   n    --- Order of the Legendre polynomial
!       Output:   X(n) --- Zeros of the Legendre polynomial
!                 W(n) --- Corresponding weighting coefficients
!       ============================================================

IMPLICIT DOUBLE PRECISION (a-h,o-z)
DIMENSION x(120),w(120)
WRITE(*,*)'Please enter the order of Pn(x), n '
!        READ(*,*)N
n=5
WRITE(*,15)n
CALL legzo(n,x,w)
WRITE(*,*)'  Nodes and weights for Gauss-Legendre integration'
WRITE(*,*)
WRITE(*,*)'  i              xi                   Wi'
WRITE(*,*)' ------------------------------------------------'
DO  i=1,n
  WRITE(*,20)i,x(i),w(i)
END DO
15      FORMAT(1X,'n =',i3)
20      FORMAT(1X,i3,1X,f22.13,d22.13)
END PROGRAM mlegzo


SUBROUTINE legzo(n,x,w)

!       =========================================================
!       Purpose : Compute the zeros of Legendre polynomial Pn(x)
!                 in the interval [-1,1], and the corresponding
!                 weighting coefficients for Gauss-Legendre
!                 integration
!       Input :   n    --- Order of the Legendre polynomial
!       Output:   X(n) --- Zeros of the Legendre polynomial
!                 W(n) --- Corresponding weighting coefficients
!       =========================================================


INTEGER, INTENT(IN)                      :: n
DOUBLE PRECISION, INTENT(IN OUT)         :: x(n)
DOUBLE PRECISION, INTENT(OUT)            :: w(n)
IMPLICIT DOUBLE PRECISION (a-h,o-z)


n0=(n+1)/2
DO  nr=1,n0
  z=DCOS(3.1415926D0*(nr-0.25D0)/n)
  DO
    10       z0=z
    p=1.0D0
    DO  i=1,nr-1
      p=p*(z-x(i))
    END DO
    f0=1.0D0
    IF (nr == n0.AND.n /= 2*INT(n/2)) z=0.0D0
    f1=z
    DO  k=2,n
      pf=(2.0D0-1.0D0/k)*z*f1-(1.0D0-1.0D0/k)*f0
      pd=k*(f1-z*pf)/(1.0D0-z*z)
      f0=f1
      f1=pf
    END DO
    IF (z == 0.0) EXIT
    fd=pf/p
    q=0.0D0
    DO  i=1,nr-1
      wp=1.0D0
      DO  j=1,nr-1
        IF (j /= i) wp=wp*(z-x(j))
      END DO
      q=q+wp
    END DO
    gd=(pd-q*fd)/p
    z=z-fd/gd
    IF (.NOT.(DABS(z-z0) > DABS(z)*1.0D-15)) EXIT
  END DO
  40      x(nr)=z
  x(n+1-nr)=-z
  w(nr)=2.0D0/((1.0D0-z*z)*pd*pd)
  w(n+1-nr)=w(nr)
END DO
RETURN
END SUBROUTINE legzo
