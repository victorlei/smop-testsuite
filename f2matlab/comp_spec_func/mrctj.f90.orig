PROGRAM mrctj
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:12

!       =======================================================
!       Purpose: This program computes the Riccati-Bessel
!                functions of the first kind, and their
!                derivatives using subroutine RCTJ
!       Input:   x --- Argument of Riccati-Bessel function
!                n --- Order of jn(x)  ( 0 ó n ó 250 )
!       Output:  RJ(n) --- xújn(x)
!                DJ(n) --- [xújn(x)]'
!       Example: x = 10.0
!                  n        xújn(x)             [xújn(x)]'
!                --------------------------------------------
!                  0    -.5440211109D+00    -.8390715291D+00
!                  1     .7846694180D+00    -.6224880527D+00
!                  2     .7794219363D+00     .6287850307D+00
!                  3    -.3949584498D+00     .8979094712D+00
!                  4    -.1055892851D+01     .2739869063D-01
!                  5    -.5553451162D+00    -.7782202931D+00
!       =======================================================

IMPLICIT DOUBLE PRECISION (a-h,o-z)
DIMENSION rj(0:250),dj(0:250)
WRITE(*,*)'  Please enter n and x '
!        READ(*,*)N,X
n=5
x=10.0
WRITE(*,30)n,x
IF (n <= 10) THEN
  ns=1
ELSE
  WRITE(*,*)'  Please enter order step Ns'
!           READ(*,*)NS
  ns=1
END IF
WRITE(*,*)
CALL rctj(n,x,nm,rj,dj)
WRITE(*,*)
WRITE(*,*)'  n        xújn(x)             [xújn(x)]'''
WRITE(*,*)'--------------------------------------------'
DO  k=0,nm,ns
  WRITE(*,20)k,rj(k),dj(k)
END DO
20      FORMAT(1X,i3,2D20.10)
30      FORMAT(3X,6HNMAX =,i3,',    ',3HX =,f7.2)
END PROGRAM mrctj


SUBROUTINE rctj(n,x,nm,rj,dj)

!       ========================================================
!       Purpose: Compute Riccati-Bessel functions of the first
!                kind and their derivatives
!       Input:   x --- Argument of Riccati-Bessel function
!                n --- Order of jn(x)  ( n = 0,1,2,... )
!       Output:  RJ(n) --- xújn(x)
!                DJ(n) --- [xújn(x)]'
!                NM --- Highest order computed
!       Routines called:
!                MSTA1 and MSTA2 for computing the starting
!                point for backward recurrence
!       ========================================================


INTEGER, INTENT(IN)                      :: n
DOUBLE PRECISION, INTENT(IN)             :: x
INTEGER, INTENT(OUT)                     :: nm
DOUBLE PRECISION, INTENT(OUT)            :: rj(0:n)
DOUBLE PRECISION, INTENT(OUT)            :: dj(0:n)
IMPLICIT DOUBLE PRECISION (a-h,o-z)


nm=n
IF (DABS(x) < 1.0D-100) THEN
  DO  k=0,n
    rj(k)=0.0D0
    dj(k)=0.0D0
  END DO
  dj(0)=1.0D0
  RETURN
END IF
rj(0)=DSIN(x)
rj(1)=rj(0)/x-DCOS(x)
rj0=rj(0)
rj1=rj(1)
IF (n >= 2) THEN
  m=msta1(x,200)
  IF (m < n) THEN
    nm=m
  ELSE
    m=msta2(x,n,15)
  END IF
  f0=0.0D0
  f1=1.0D-100
  DO  k=m,0,-1
    f=(2.0D0*k+3.0D0)*f1/x-f0
    IF (k <= nm) rj(k)=f
    f0=f1
    f1=f
  END DO
  IF (DABS(rj0) > DABS(rj1)) cs=rj0/f
  IF (DABS(rj0) <= DABS(rj1)) cs=rj1/f0
  DO  k=0,nm
    rj(k)=cs*rj(k)
  END DO
END IF
dj(0)=DCOS(x)
DO  k=1,nm
  dj(k)=-k*rj(k)/x+rj(k-1)
END DO
RETURN
END SUBROUTINE rctj


INTEGER FUNCTION msta1(x,mp)

!       ===================================================
!       Purpose: Determine the starting point for backward
!                recurrence such that the magnitude of
!                Jn(x) at that point is about 10^(-MP)
!       Input :  x     --- Argument of Jn(x)
!                MP    --- Value of magnitude
!       Output:  MSTA1 --- Starting point
!       ===================================================



DOUBLE PRECISION, INTENT(IN OUT)         :: x
INTEGER, INTENT(IN)                      :: mp
IMPLICIT DOUBLE PRECISION (a-h,o-z)
a0=DABS(x)
n0=INT(1.1*a0)+1
f0=envj(n0,a0)-mp
n1=n0+5
f1=envj(n1,a0)-mp
DO  it=1,20
  nn=n1-(n1-n0)/(1.0D0-f0/f1)
  f=envj(nn,a0)-mp
  IF(ABS(nn-n1) < 1) EXIT
  n0=n1
  f0=f1
  n1=nn
  f1=f
END DO
20     msta1=INT(nn)
RETURN
END FUNCTION msta1


INTEGER FUNCTION msta2(x,n,mp)

!       ===================================================
!       Purpose: Determine the starting point for backward
!                recurrence such that all Jn(x) has MP
!                significant digits
!       Input :  x  --- Argument of Jn(x)
!                n  --- Order of Jn(x)
!                MP --- Significant digit
!       Output:  MSTA2 --- Starting point
!       ===================================================



DOUBLE PRECISION, INTENT(IN OUT)         :: x
INTEGER, INTENT(IN)                      :: n
INTEGER, INTENT(IN)                      :: mp
IMPLICIT DOUBLE PRECISION (a-h,o-z)
a0=DABS(x)
hmp=0.5D0*mp
ejn=envj(n,a0)
IF (ejn <= hmp) THEN
  obj=mp
  n0=INT(1.1*a0)
ELSE
  obj=hmp+ejn
  n0=n
END IF
f0=envj(n0,a0)-obj
n1=n0+5
f1=envj(n1,a0)-obj
DO  it=1,20
  nn=n1-(n1-n0)/(1.0D0-f0/f1)
  f=envj(nn,a0)-obj
  IF (ABS(nn-n1) < 1) EXIT
  n0=n1
  f0=f1
  n1=nn
  f1=f
END DO
20      msta2=INT(nn+10)
RETURN
END FUNCTION msta2

REAL*8 FUNCTION envj(n,x)


INTEGER, INTENT(IN)                      :: n
DOUBLE PRECISION, INTENT(IN)             :: x

envj=0.5D0*DLOG10(6.28D0*n)-n*DLOG10(1.36D0*x/n)
RETURN
END FUNCTION envj
