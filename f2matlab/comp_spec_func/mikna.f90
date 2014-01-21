PROGRAM mikna
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:11

!       =============================================================
!       Purpose: This program computes modified Bessel functions
!                In(x) and Kn(x), and their derivatives using
!                subroutine IKNA
!       Input:   x --- Argument of In(x) and Kn(x) ( x ע 0 )
!                n --- Order of In(x) and Kn(x)
!                      ( n = 0,1,תתת, n ף 250 )
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
CALL ikna(n,x,nm,bi,di,bk,dk)
WRITE(*,*)'  n      In(x)          In''(X) ', '        Kn(x)         Kn''(X) '
WRITE(*,*)' -------------------------------',  &
    '--------------------------------'
DO  k=0,nm,ns
  WRITE(*,20)k,bi(k),di(k),bk(k),dk(k)
END DO
20      FORMAT(1X,i3,4D15.7)
25      FORMAT(3X,6HNMAX =,i3,',    ',3HX =,f5.1)
END PROGRAM mikna


SUBROUTINE ikna(n,x,nm,bi,di,bk,dk)

!       ========================================================
!       Purpose: Compute modified Bessel functions In(x) and
!                Kn(x), and their derivatives
!       Input:   x --- Argument of In(x) and Kn(x) ( x ע 0 )
!                n --- Order of In(x) and Kn(x)
!       Output:  BI(n) --- In(x)
!                DI(n) --- In'(x)
!                BK(n) --- Kn(x)
!                DK(n) --- Kn'(x)
!                NM --- Highest order computed
!       Routines called:
!            (1) IK01A for computing I0(x),I1(x),K0(x) & K1(x)
!            (2) MSTA1 and MSTA2 for computing the starting
!                point for backward recurrence
!       ========================================================


INTEGER, INTENT(IN)                      :: n
DOUBLE PRECISION, INTENT(IN)             :: x
INTEGER, INTENT(OUT)                     :: nm
DOUBLE PRECISION, INTENT(OUT)            :: bi(0:n)
DOUBLE PRECISION, INTENT(OUT)            :: di(0:n)
DOUBLE PRECISION, INTENT(OUT)            :: bk(0:n)
DOUBLE PRECISION, INTENT(OUT)            :: dk(0:n)
IMPLICIT DOUBLE PRECISION (a-h,o-z)


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
CALL ik01a(x,bi0,di0,bi1,di1,bk0,dk0,bk1,dk1)
bi(0)=bi0
bi(1)=bi1
bk(0)=bk0
bk(1)=bk1
di(0)=di0
di(1)=di1
dk(0)=dk0
dk(1)=dk1
IF (n <= 1) RETURN
IF (x > 40.0.AND.n < INT(0.25*x)) THEN
  h0=bi0
  h1=bi1
  DO  k=2,n
    h=-2.0D0*(k-1.0D0)/x*h1+h0
    bi(k)=h
    h0=h1
    h1=h
  END DO
ELSE
  m=msta1(x,200)
  IF (m < n) THEN
    nm=m
  ELSE
    m=msta2(x,n,15)
  END IF
  f0=0.0D0
  f1=1.0D-100
  DO  k=m,0,-1
    f=2.0D0*(k+1.0D0)*f1/x+f0
    IF (k <= nm) bi(k)=f
    f0=f1
    f1=f
  END DO
  s0=bi0/f
  DO  k=0,nm
    bi(k)=s0*bi(k)
  END DO
END IF
g0=bk0
g1=bk1
DO  k=2,nm
  g=2.0D0*(k-1.0D0)/x*g1+g0
  bk(k)=g
  g0=g1
  g1=g
END DO
DO  k=2,nm
  di(k)=bi(k-1)-k/x*bi(k)
  dk(k)=-bk(k-1)-k/x*bk(k)
END DO
RETURN
END SUBROUTINE ikna


SUBROUTINE ik01a(x,bi0,di0,bi1,di1,bk0,dk0,bk1,dk1)

!       =========================================================
!       Purpose: Compute modified Bessel functions I0(x), I1(1),
!                K0(x) and K1(x), and their derivatives
!       Input :  x   --- Argument ( x ע 0 )
!       Output:  BI0 --- I0(x)
!                DI0 --- I0'(x)
!                BI1 --- I1(x)
!                DI1 --- I1'(x)
!                BK0 --- K0(x)
!                DK0 --- K0'(x)
!                BK1 --- K1(x)
!                DK1 --- K1'(x)
!       =========================================================


DOUBLE PRECISION, INTENT(IN)             :: x
DOUBLE PRECISION, INTENT(OUT)            :: bi0
DOUBLE PRECISION, INTENT(OUT)            :: di0
DOUBLE PRECISION, INTENT(OUT)            :: bi1
DOUBLE PRECISION, INTENT(OUT)            :: di1
DOUBLE PRECISION, INTENT(OUT)            :: bk0
DOUBLE PRECISION, INTENT(OUT)            :: dk0
DOUBLE PRECISION, INTENT(OUT)            :: bk1
DOUBLE PRECISION, INTENT(OUT)            :: dk1
IMPLICIT DOUBLE PRECISION (a-h,o-z)
DIMENSION a(12),b(12),a1(8)

pi=3.141592653589793D0
el=0.5772156649015329D0
x2=x*x
IF (x == 0.0D0) THEN
  bi0=1.0D0
  bi1=0.0D0
  bk0=1.0D+300
  bk1=1.0D+300
  di0=0.0D0
  di1=0.5D0
  dk0=-1.0D+300
  dk1=-1.0D+300
  RETURN
ELSE IF (x <= 18.0) THEN
  bi0=1.0D0
  r=1.0D0
  DO  k=1,50
    r=0.25D0*r*x2/(k*k)
    bi0=bi0+r
    IF (DABS(r/bi0) < 1.0D-15) EXIT
  END DO
  20         bi1=1.0D0
  r=1.0D0
  DO  k=1,50
    r=0.25D0*r*x2/(k*(k+1))
    bi1=bi1+r
    IF (DABS(r/bi1) < 1.0D-15) EXIT
  END DO
  30         bi1=0.5D0*x*bi1
ELSE
  DATA a/0.125D0,7.03125D-2, 7.32421875D-2,1.1215209960938D-1,  &
      2.2710800170898D-1,5.7250142097473D-1,  &
      1.7277275025845D0,6.0740420012735D0,  &
      2.4380529699556D01,1.1001714026925D02,  &
      5.5133589612202D02,3.0380905109224D03/
  DATA b/-0.375D0,-1.171875D-1, -1.025390625D-1,-1.4419555664063D-1,  &
      -2.7757644653320D-1,-6.7659258842468D-1,  &
      -1.9935317337513D0,-6.8839142681099D0,  &
      -2.7248827311269D01,-1.2159789187654D02,  &
      -6.0384407670507D02,-3.3022722944809D03/
  k0=12
  IF (x >= 35.0) k0=9
  IF (x >= 50.0) k0=7
  ca=DEXP(x)/DSQRT(2.0D0*pi*x)
  bi0=1.0D0
  xr=1.0D0/x
  DO  k=1,k0
    bi0=bi0+a(k)*xr**k
  END DO
  bi0=ca*bi0
  bi1=1.0D0
  DO  k=1,k0
    bi1=bi1+b(k)*xr**k
  END DO
  bi1=ca*bi1
END IF
IF (x <= 9.0D0) THEN
  ct=-(DLOG(x/2.0D0)+el)
  bk0=0.0D0
  w0=0.0D0
  r=1.0D0
  DO  k=1,50
    w0=w0+1.0D0/k
    r=0.25D0*r/(k*k)*x2
    bk0=bk0+r*(w0+ct)
    IF (DABS((bk0-ww)/bk0) < 1.0D-15) EXIT
    ww=bk0
  END DO
  70         bk0=bk0+ct
ELSE
  DATA a1/0.125D0,0.2109375D0, 1.0986328125D0,1.1775970458984D01,  &
      2.1461706161499D02,5.9511522710323D03,  &
      2.3347645606175D05,1.2312234987631D07/
  cb=0.5D0/x
  xr2=1.0D0/x2
  bk0=1.0D0
  DO  k=1,8
    bk0=bk0+a1(k)*xr2**k
  END DO
  bk0=cb*bk0/bi0
END IF
bk1=(1.0D0/x-bi1*bk0)/bi0
di0=bi1
di1=bi0-bi1/x
dk0=-bk1
dk1=-bk0-bk1/x
RETURN
END SUBROUTINE ik01a


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
