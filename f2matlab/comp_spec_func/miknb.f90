PROGRAM miknb
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:11

!       =============================================================
!       Purpose: This program computes modified Bessel functions
!                In(x) and Kn(x), and their derivatives using
!                subroutine IKNB
!       Input:   x --- Argument of In(x) and Kn(x) ( 0 ó x ó 700 )
!                n --- Order of In(x) and Kn(x)
!                      ( n = 0,1,..., n ó 250 )
!       Output:  BI(n) --- In(x)
!                DI(n) --- In'(x)
!                BK(n) --- Kn(x)
!                DK(n) --- Kn'(x)
!       Example: Nmax = 5,    x = 10.0

!     n      In(x)          In'(x)         Kn(x)         Kn'(x)
!    ---------------------------------------------------------------
!     0   .2815717D+04   .2670988D+04   .1778006D-04  -.1864877D-04
!     1   .2670988D+04   .2548618D+04   .1864877D-04  -.1964494D-04
!     2   .2281519D+04   .2214685D+04   .2150982D-04  -.2295074D-04
!     3   .1758381D+04   .1754005D+04   .2725270D-04  -.2968563D-04
!     4   .1226491D+04   .1267785D+04   .3786144D-04  -.4239728D-04
!     5   .7771883D+03   .8378964D+03   .5754185D-04  -.6663236D-04
!       =============================================================

IMPLICIT DOUBLE PRECISION (a-h,o-z)
DIMENSION bi(0:250),di(0:250),bk(0:250),dk(0:250)
WRITE(*,*)'  Please enter n, x '
!        READ(*,*)N,X
n=5
x=10.0
WRITE(*,25)n,x
WRITE(*,*)
IF (n <= 10) THEN
  ns=1
ELSE
  WRITE(*,*)'  Please enter order step Ns'
!           READ(*,*)NS
  ns=1
END IF
CALL iknb(n,x,nm,bi,di,bk,dk)
WRITE(*,*)'  n      In(x)          In''(X) ', '        Kn(x)         Kn''(X) '
WRITE(*,*)' -------------------------------',  &
    '--------------------------------'
DO  k=0,nm,ns
  WRITE(*,20)k,bi(k),di(k),bk(k),dk(k)
END DO
20      FORMAT(1X,i3,4D15.7)
25      FORMAT(3X,6HNMAX =,i3,',    ',3HX =,f5.1)
END PROGRAM miknb


SUBROUTINE iknb(n,x,nm,bi,di,bk,dk)

!       ============================================================
!       Purpose: Compute modified Bessel functions In(x) and Kn(x),
!                and their derivatives
!       Input:   x --- Argument of In(x) and Kn(x) ( 0 ó x ó 700 )
!                n --- Order of In(x) and Kn(x)
!       Output:  BI(n) --- In(x)
!                DI(n) --- In'(x)
!                BK(n) --- Kn(x)
!                DK(n) --- Kn'(x)
!                NM --- Highest order computed
!       Routines called:
!                MSTA1 and MSTA2 for computing the starting point
!                for backward recurrence
!       ===========================================================


INTEGER, INTENT(IN)                      :: n
DOUBLE PRECISION, INTENT(IN)             :: x
INTEGER, INTENT(OUT)                     :: nm
DOUBLE PRECISION, INTENT(OUT)            :: bi(0:n)
DOUBLE PRECISION, INTENT(OUT)            :: di(0:n)
DOUBLE PRECISION, INTENT(OUT)            :: bk(0:n)
DOUBLE PRECISION, INTENT(OUT)            :: dk(0:n)
IMPLICIT DOUBLE PRECISION (a-h,o-z)


pi=3.141592653589793D0
el=0.5772156649015329D0
nm=n
IF (x <= 1.0D-100) THEN
  DO  k=0,n
    bi(k)=0.0D0
    di(k)=0.0D0
    bk(k)=1.0D+300
    dk(k)=-1.0D+300
  END DO
  bi(0)=1.0D0
  di(1)=0.5D0
  RETURN
END IF
IF (n == 0) nm=1
m=msta1(x,200)
IF (m < nm) THEN
  nm=m
ELSE
  m=msta2(x,nm,15)
END IF
bs=0.0D0
sk0=0.0D0
f0=0.0D0
f1=1.0D-100
DO  k=m,0,-1
  f=2.0D0*(k+1.0D0)/x*f1+f0
  IF (k <= nm) bi(k)=f
  IF (k /= 0.AND.k == 2*INT(k/2)) sk0=sk0+4.0D0*f/k
  bs=bs+2.0D0*f
  f0=f1
  f1=f
END DO
s0=DEXP(x)/(bs-f)
DO  k=0,nm
  bi(k)=s0*bi(k)
END DO
IF (x <= 8.0D0) THEN
  bk(0)=-(DLOG(0.5D0*x)+el)*bi(0)+s0*sk0
  bk(1)=(1.0D0/x-bi(1)*bk(0))/bi(0)
ELSE
  a0=DSQRT(pi/(2.0D0*x))*DEXP(-x)
  k0=16
  IF (x >= 25.0) k0=10
  IF (x >= 80.0) k0=8
  IF (x >= 200.0) k0=6
  DO  l=0,1
    bkl=1.0D0
    vt=4.0D0*l
    r=1.0D0
    DO  k=1,k0
      r=0.125D0*r*(vt-(2.0*k-1.0)**2)/(k*x)
      bkl=bkl+r
    END DO
    bk(l)=a0*bkl
  END DO
END IF
g0=bk(0)
g1=bk(1)
DO  k=2,nm
  g=2.0D0*(k-1.0D0)/x*g1+g0
  bk(k)=g
  g0=g1
  g1=g
END DO
di(0)=bi(1)
dk(0)=-bk(1)
DO  k=1,nm
  di(k)=bi(k-1)-k/x*bi(k)
  dk(k)=-bk(k-1)-k/x*bk(k)
END DO
RETURN
END SUBROUTINE iknb


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
!       Input :  x     --- Argument of Jn(x)
!                n     --- Order of Jn(x)
!                MP    --- Significant digit
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
