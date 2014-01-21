PROGRAM mherzo
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:11

!       ===========================================================
!       Purpose : This program computes the zeros of Hermite
!                 polynomial Ln(x) in the interval [-ì,ì] and the
!                 corresponding weighting coefficients for Gauss-
!                 Hermite integration using subroutine HERZO
!       Input :   n    --- Order of the Hermite polynomial
!                 X(n) --- Zeros of the Hermite polynomial
!                 W(n) --- Corresponding weighting coefficients
!       ===========================================================

IMPLICIT DOUBLE PRECISION (a-h,o-z)
DIMENSION x(100),w(100)
WRITE(*,*)'Please enter the order of Hn(x), n '
!        READ(*,*)N
n=5
WRITE(*,20)n
CALL herzo(n,x,w)
WRITE(*,*)'  Nodes and weights for Gauss-Hermite integration'
WRITE(*,*)
WRITE(*,*)'  i             xi                      Wi'
WRITE(*,*)' -----------------------------------------', '------------'
DO  j=1,n
  WRITE(*,30)j,x(j),w(j)
END DO
20      FORMAT(1X,'n =',i3)
30      FORMAT(1X,i3,3X,d22.13,3X,d22.13)
END PROGRAM mherzo


SUBROUTINE herzo(n,x,w)

!       ========================================================
!       Purpose : Compute the zeros of Hermite polynomial Ln(x)
!                 in the interval [-ì,ì], and the corresponding
!                 weighting coefficients for Gauss-Hermite
!                 integration
!       Input :   n    --- Order of the Hermite polynomial
!                 X(n) --- Zeros of the Hermite polynomial
!                 W(n) --- Corresponding weighting coefficients
!       ========================================================


INTEGER, INTENT(IN)                      :: n
DOUBLE PRECISION, INTENT(IN OUT)         :: x(n)
DOUBLE PRECISION, INTENT(OUT)            :: w(n)
IMPLICIT DOUBLE PRECISION (a-h,o-z)


hn=1.0D0/n
zl=-1.1611D0+1.46D0*n**0.5
DO  nr=1,INT(n/2)
  IF (nr == 1) z=zl
  IF (nr /= 1) z=z-hn*(INT(n/2)+1-nr)
  it=0
  DO
    10         it=it+1
    z0=z
    f0=1.0D0
    f1=2.0D0*z
    DO  k=2,n
      hf=2.0D0*z*f1-2.0D0*(k-1.0D0)*f0
      hd=2.0D0*k*f1
      f0=f1
      f1=hf
    END DO
    p=1.0D0
    DO  i=1,nr-1
      p=p*(z-x(i))
    END DO
    fd=hf/p
    q=0.0D0
    DO  i=1,nr-1
      wp=1.0D0
      DO  j=1,nr-1
        IF (.NOT.(j == i)) wp=wp*(z-x(j))
      END DO
      q=q+wp
    END DO
    gd=(hd-q*fd)/p
    z=z-fd/gd
    IF (.NOT.(it <= 40.AND.DABS((z-z0)/z) > 1.0D-15)) EXIT
  END DO
  x(nr)=z
  x(n+1-nr)=-z
  r=1.0D0
  DO  k=1,n
    r=2.0D0*r*k
  END DO
  w(nr)=3.544907701811D0*r/(hd*hd)
  w(n+1-nr)=w(nr)
END DO
IF (n /= 2*INT(n/2)) THEN
  r1=1.0D0
  r2=1.0D0
  DO  j=1,n
    r1=2.0D0*r1*j
    IF (j >= INT((n+1)/2)) r2=r2*j
  END DO
  w(INT(n/2)+1)=0.88622692545276D0*r1/(r2*r2)
  x(INT(n/2)+1)=0.0D0
END IF
RETURN
END SUBROUTINE herzo
