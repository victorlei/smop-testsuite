PROGRAM msphi
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:12

!       ======================================================
!       Purpose: This program computes the modified spherical
!                Bessel functions of the first kind in(x) and
!                in'(x) using subroutine SPHI
!       Input :  x --- Argument of in(x)
!                n --- Order of in(x) ( 0 ó n ó 250 )
!       Output:  SI(n) --- in(x)
!                DI(n) --- in'(x)
!       Example: x = 10.0
!                  n          in(x)               in'(x)
!                --------------------------------------------
!                  0     .1101323287D+04     .9911909633D+03
!                  1     .9911909633D+03     .9030850948D+03
!                  2     .8039659985D+03     .7500011637D+03
!                  3     .5892079640D+03     .5682828129D+03
!                  4     .3915204237D+03     .3934477522D+03
!                  5     .2368395827D+03     .2494166741D+03
!       ======================================================

IMPLICIT DOUBLE PRECISION (a-h,o-z)
DIMENSION si(0:250),di(0:250)
WRITE(*,*)'Please enter n and x '
!        READ(*,*)N,X
n=5
x=10.0
WRITE(*,30)n,x
IF (n <= 10) THEN
  ns=1
ELSE
  WRITE(*,*) 'Please enter order step Ns'
!           READ(*,*) NS
  ns=1
END IF
CALL sphi(n,x,nm,si,di)
WRITE(*,*)
WRITE(*,*)'  n          in(x)               in''(X)'
WRITE(*,*)'--------------------------------------------'
DO  k=0,nm,ns
  WRITE(*,20)k,si(k),di(k)
END DO
20      FORMAT(1X,i3,2D20.10)
30      FORMAT(3X,'Nmax =',i3,',     ','x =',f6.1)
END PROGRAM msphi


SUBROUTINE sphi(n,x,nm,si,di)

!       ========================================================
!       Purpose: Compute modified spherical Bessel functions
!                of the first kind, in(x) and in'(x)
!       Input :  x --- Argument of in(x)
!                n --- Order of in(x) ( n = 0,1,2,... )
!       Output:  SI(n) --- in(x)
!                DI(n) --- in'(x)
!                NM --- Highest order computed
!       Routines called:
!                MSTA1 and MSTA2 for computing the starting
!                point for backward recurrence
!       ========================================================


INTEGER, INTENT(IN)                      :: n
DOUBLE PRECISION, INTENT(IN)             :: x
INTEGER, INTENT(OUT)                     :: nm
DOUBLE PRECISION, INTENT(OUT)            :: si(0:n)
DOUBLE PRECISION, INTENT(OUT)            :: di(0:n)
IMPLICIT DOUBLE PRECISION (a-h,o-z)


nm=n
IF (DABS(x) < 1.0D-100) THEN
  DO  k=0,n
    si(k)=0.0D0
    di(k)=0.0D0
  END DO
  si(0)=1.0D0
  di(1)=0.333333333333333D0
  RETURN
END IF
si(0)=DSINH(x)/x
si(1)=-(DSINH(x)/x-DCOSH(x))/x
si0=si(0)
IF (n >= 2) THEN
  m=msta1(x,200)
  IF (m < n) THEN
    nm=m
  ELSE
    m=msta2(x,n,15)
  END IF
  f0=0.0D0
  f1=1.0D0-100
  DO  k=m,0,-1
    f=(2.0D0*k+3.0D0)*f1/x+f0
    IF (k <= nm) si(k)=f
    f0=f1
    f1=f
  END DO
  cs=si0/f
  DO  k=0,nm
    si(k)=cs*si(k)
  END DO
END IF
di(0)=si(1)
DO  k=1,nm
  di(k)=si(k-1)-(k+1.0D0)/x*si(k)
END DO
RETURN
END SUBROUTINE sphi


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
