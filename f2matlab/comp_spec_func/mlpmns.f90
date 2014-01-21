PROGRAM mlpmns
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:12

!       ========================================================
!       Purpose: This program computes the associated Legendre
!                functions Pmn(x) and their derivatives Pmn'(x)
!                for a given order using subroutine LPMNS
!       Input :  x --- Argument of Pmn(x)
!                m --- Order of Pmn(x),  m = 0,1,2,...,n
!                n --- Degree of Pmn(x), n = 0,1,2,...,N
!       Output:  PM(n) --- Pmn(x)
!                PD(n) --- Pmn'(x)
!       Examples:
!                m = 1,  N = 5,  x = .5
!                n        Pmn(x)           Pmn'(x)
!               -------------------------------------
!                0    .00000000D+00    .00000000D+00
!                1    .86602540D+00   -.57735027D+00
!                2    .12990381D+01    .17320508D+01
!                3    .32475953D+00    .62786842D+01
!                4   -.13531647D+01    .57735027D+01
!                5   -.19282597D+01   -.43977853D+01

!                m = 2,  N = 6,  x = 2.5
!                n        Pmn(x)           Pmn'(x)
!               -------------------------------------
!                0    .00000000D+00    .00000000D+00
!                1    .00000000D+00    .00000000D+00
!                2    .15750000D+02    .15000000D+02
!                3    .19687500D+03    .26625000D+03
!                4    .16832813D+04    .29812500D+04
!                5    .12230859D+05    .26876719D+05
!                6    .81141416D+05    .21319512D+06
!       =======================================================

IMPLICIT DOUBLE PRECISION (p,x,y)
DIMENSION pm(0:200),pd(0:200)
WRITE(*,*)'Please enter m, N, and x '
!        READ(*,*)M,N,X
m=1
n=5
x=.5
WRITE(*,30)m,n,x
CALL lpmns(m,n,x,pm,pd)
WRITE(*,*)
WRITE(*,*)'  n        Pmn(x)           Pmn''(X)    '
WRITE(*,*)' -------------------------------------'
DO  j=0,n
  WRITE(*,20)j,pm(j),pd(j)
END DO
20      FORMAT(1X,i3,2D17.8)
30      FORMAT(1X,'m =',i2,',  ','n =',i2,',  ','x =',f5.1)
END PROGRAM mlpmns


SUBROUTINE lpmns(m,n,x,pm,pd)

!       ========================================================
!       Purpose: Compute associated Legendre functions Pmn(x)
!                and Pmn'(x) for a given order
!       Input :  x --- Argument of Pmn(x)
!                m --- Order of Pmn(x),  m = 0,1,2,...,n
!                n --- Degree of Pmn(x), n = 0,1,2,...,N
!       Output:  PM(n) --- Pmn(x)
!                PD(n) --- Pmn'(x)
!       ========================================================


INTEGER, INTENT(IN)                      :: m
INTEGER, INTENT(IN)                      :: n
DOUBLE PRECISION, INTENT(IN)             :: x
DOUBLE PRECISION, INTENT(OUT)            :: pm(0:n)
DOUBLE PRECISION, INTENT(OUT)            :: pd(0:n)
IMPLICIT DOUBLE PRECISION (a-h,o-z)


DO  k=0,n
  pm(k)=0.0D0
  pd(k)=0.0D0
END DO
IF (DABS(x) == 1.0D0) THEN
  DO  k=0,n
    IF (m == 0) THEN
      pm(k)=1.0D0
      pd(k)=0.5D0*k*(k+1.0)
      IF (x < 0.0) THEN
        pm(k)=(-1)**k*pm(k)
        pd(k)=(-1)**(k+1)*pd(k)
      END IF
    ELSE IF (m == 1) THEN
      pd(k)=1.0D+300
    ELSE IF (m == 2) THEN
      pd(k)=-0.25D0*(k+2.0)*(k+1.0)*k*(k-1.0)
      IF (x < 0.0) pd(k)=(-1)**(k+1)*pd(k)
    END IF
  END DO
  RETURN
END IF
x0=DABS(1.0D0-x*x)
pm0=1.0D0
pmk=pm0
DO  k=1,m
  pmk=(2.0D0*k-1.0D0)*DSQRT(x0)*pm0
  pm0=pmk
END DO
pm1=(2.0D0*m+1.0D0)*x*pm0
pm(m)=pmk
pm(m+1)=pm1
DO  k=m+2,n
  pm2=((2.0D0*k-1.0D0)*x*pm1-(k+m-1.0D0)*pmk)/(k-m)
  pm(k)=pm2
  pmk=pm1
  pm1=pm2
END DO
pd(0)=((1.0D0-m)*pm(1)-x*pm(0))/(x*x-1.0)
DO  k=1,n
  pd(k)=(k*x*pm(k)-(k+m)*pm(k-1))/(x*x-1.0D0)
END DO
RETURN
END SUBROUTINE lpmns
