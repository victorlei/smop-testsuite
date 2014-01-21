PROGRAM mlpni
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:12

!     ========================================================
!     Purpose: This program computes the Legendre polynomials
!     Pn(x), Pn'(x) and the integral of Pn(t) from 0
!     to x using subroutine LPNI
!     Input :  x --- Argument of Pn(x)
!     n --- Degree of Pn(x) ( n = 0,1,... )
!     Output:  PN(n) --- Pn(x)
!     PD(n) --- Pn'(x)
!     PL(n) --- Integral of Pn(t) from 0 to x
!     Example: x = 0.50
!     n       Pn(x)         Pn'(x)        Pn(t)dt
!     ---------------------------------------------
!     0    1.00000000     .00000000     .50000000
!     1     .50000000    1.00000000     .12500000
!     2    -.12500000    1.50000000    -.18750000
!     3    -.43750000     .37500000    -.14843750
!     4    -.28906250   -1.56250000     .05859375
!     5     .08984375   -2.22656250     .11816406
!     ========================================================

DOUBLE PRECISION :: pn,pd,pl,x
DIMENSION pn(0:100),pd(0:100),pl(0:100)
WRITE(*,*)'  Please enter Nmax and x'
!     READ(*,*)N,X
n=5
x=.5
WRITE(*,30)x
WRITE(*,*)
WRITE(*,*)'  n        Pn(x)          Pn''(X)         PN(T)DT'
WRITE(*,*)' ---------------------------------------------------'
CALL lpni(n,x,pn,pd,pl)
DO  k=0,n
  WRITE(*,20)k,pn(k),pd(k),pl(k)
END DO
20   FORMAT(1X,i3,3E16.8)
30   FORMAT(3X,'x =',f5.2)
END PROGRAM mlpni


SUBROUTINE lpni(n,x,pn,pd,pl)

!     =====================================================
!     Purpose: Compute Legendre polynomials Pn(x), Pn'(x)
!     and the integral of Pn(t) from 0 to x
!     Input :  x --- Argument of Pn(x)
!     n --- Degree of Pn(x) ( n = 0,1,... )
!     Output:  PN(n) --- Pn(x)
!     PD(n) --- Pn'(x)
!     PL(n) --- Integral of Pn(t) from 0 to x
!     =====================================================


INTEGER, INTENT(IN)                      :: n
DOUBLE PRECISION, INTENT(IN)             :: x
DOUBLE PRECISION, INTENT(OUT)            :: pn(0:n)
DOUBLE PRECISION, INTENT(OUT)            :: pd(0:n)
DOUBLE PRECISION, INTENT(OUT)            :: pl(0:n)
IMPLICIT DOUBLE PRECISION (p,r,x)


pn(0)=1.0D0
pn(1)=x
pd(0)=0.0D0
pd(1)=1.0D0
pl(0)=x
pl(1)=0.5D0*x*x
p0=1.0D0
p1=x
DO  k=2,n
  pf=(2.0D0*k-1.0D0)/k*x*p1-(k-1.0D0)/k*p0
  pn(k)=pf
  IF (DABS(x) == 1.0D0) THEN
    pd(k)=0.5D0*x**(k+1)*k*(k+1.0D0)
  ELSE
    pd(k)=k*(p1-x*pf)/(1.0D0-x*x)
  END IF
  pl(k)=(x*pn(k)-pn(k-1))/(k+1.0D0)
  p0=p1
  p1=pf
  IF (.NOT.(k == 2*INT(k/2))) THEN
    r=1.0D0/(k+1.0D0)
    n1=(k-1)/2
    DO  j=1,n1
      r=(0.5D0/j-1.0D0)*r
    END DO
    pl(k)=pl(k)+r
  END IF
END DO
RETURN
END SUBROUTINE lpni
