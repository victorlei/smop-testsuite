PROGRAM mbernob
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:09

!       ===========================================================
!       Purpose: This program computes Bernoulli number Bn using
!                subroutine BERNOB
!       Example: Compute Bernouli number Bn for n = 0,1,...,10
!                Computed results:

!                   n            Bn
!                 --------------------------
!                   0     .100000000000D+01
!                   1    -.500000000000D+00
!                   2     .166666666667D+00
!                   4    -.333333333333D-01
!                   6     .238095238095D-01
!                   8    -.333333333333D-01
!                  10     .757575757576D-01
!       ===========================================================

DOUBLE PRECISION :: b
DIMENSION b(0:200)
WRITE(*,*)'  Please enter Nmax'
!        READ(*,*)N
n=10
CALL bernob(n,b)
WRITE(*,*)'   n            Bn'
WRITE(*,*)' --------------------------'
WRITE(*,20)0,b(0)
WRITE(*,20)1,b(1)
DO  k=2,n,2
  WRITE(*,20)k,b(k)
END DO
20      FORMAT(2X,i3,d22.12)
END PROGRAM mbernob


SUBROUTINE bernob(n,bn)

!       ======================================
!       Purpose: Compute Bernoulli number Bn
!       Input :  n --- Serial number
!       Output:  BN(n) --- Bn
!       ======================================


INTEGER, INTENT(IN)                      :: n
DOUBLE PRECISION, INTENT(OUT)            :: bn(0:n)
IMPLICIT DOUBLE PRECISION (a-h,o-z)


tpi=6.283185307179586D0
bn(0)=1.0D0
bn(1)=-0.5D0
bn(2)=1.0D0/6.0D0
r1=(2.0D0/tpi)**2
DO  m=4,n,2
  r1=-r1*(m-1)*m/(tpi*tpi)
  r2=1.0D0
  DO  k=2,10000
    s=(1.0D0/k)**m
    r2=r2+s
    IF (s < 1.0D-15) EXIT
  END DO
  bn(m)=r1*r2
END DO
RETURN
END SUBROUTINE bernob
