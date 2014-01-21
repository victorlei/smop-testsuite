PROGRAM mlpn
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:12

!       ========================================================
!       Purpose: This program computes the Legendre polynomials
!                Pn(x) and their derivatives Pn'(x) using
!                subroutine LPN
!       Input :  x --- Argument of Pn(x)
!                n --- Degree of Pn(x) ( n = 0,1,...)
!       Output:  PN(n) --- Pn(x)
!                PD(n) --- Pn'(x)
!       Example:    x = 0.5
!                  n          Pn(x)            Pn'(x)
!                ---------------------------------------
!                  0       1.00000000        .00000000
!                  1        .50000000       1.00000000
!                  2       -.12500000       1.50000000
!                  3       -.43750000        .37500000
!                  4       -.28906250      -1.56250000
!                  5        .08984375      -2.22656250
!       ========================================================

DOUBLE PRECISION :: pn,pd,x
DIMENSION pn(0:100),pd(0:100)
WRITE(*,*)'  Please enter Nmax and x '
!        READ(*,*)N,X
n=5
x=.5
WRITE(*,30)x
WRITE(*,*)
CALL lpn(n,x,pn,pd)
WRITE(*,*)'  n         Pn(x)           Pn''(X)'
WRITE(*,*)'---------------------------------------'
DO  k=0,n
  WRITE(*,20)k,pn(k),pd(k)
END DO
20      FORMAT(1X,i3,2E17.8)
30      FORMAT(3X,'x =',f5.1)
END PROGRAM mlpn


SUBROUTINE lpn(n,x,pn,pd)

!       ===============================================
!       Purpose: Compute Legendre polynomials Pn(x)
!                and their derivatives Pn'(x)
!       Input :  x --- Argument of Pn(x)
!                n --- Degree of Pn(x) ( n = 0,1,...)
!       Output:  PN(n) --- Pn(x)
!                PD(n) --- Pn'(x)
!       ===============================================


INTEGER, INTENT(IN)                      :: n
DOUBLE PRECISION, INTENT(IN)             :: x
DOUBLE PRECISION, INTENT(OUT)            :: pn(0:n)
DOUBLE PRECISION, INTENT(OUT)            :: pd(0:n)
IMPLICIT DOUBLE PRECISION (p,x)


pn(0)=1.0D0
pn(1)=x
pd(0)=0.0D0
pd(1)=1.0D0
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
  p0=p1
  p1=pf
END DO
RETURN
END SUBROUTINE lpn
