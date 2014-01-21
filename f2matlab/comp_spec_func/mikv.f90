PROGRAM mikv
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:11

!       ============================================================
!       Purpose: This program computes modified Bessel functions
!                Iv(x) and Kv(x) with an arbitrary order, and
!                their derivatives using subroutine IKV
!       Input :  x --- Argument ( x ע 0 )
!                v --- Order of Iv(x) and Kv(x)
!                      ( v = n+v0, 0 ף n ף 250 , 0 ף v0 < 1 )
!       Output:  BI(n) --- In+v0(x)
!                DI(n) --- In+v0'(x)
!                BK(n) --- Kn+v0(x)
!                DK(n) --- Kn+v0'(x)
!       Example: v = n+v0,   v0 = .25,   x = 10.0

!     n       Iv(x)          Iv'(x)         Kv(x)          Kv'(x)
!    ----------------------------------------------------------------
!     0   .28064359D+04  .26631677D+04  .17833184D-04 -.18709581D-04
!     1   .25930068D+04  .24823101D+04  .19155411D-04 -.20227611D-04
!     2   .21581842D+04  .21074153D+04  .22622037D-04 -.24245369D-04
!     3   .16218239D+04  .16310915D+04  .29335327D-04 -.32156018D-04
!     4   .11039987D+04  .11526244D+04  .41690000D-04 -.47053577D-04
!     5   .68342498D+03  .74520058D+03  .64771827D-04 -.75695209D-04
!       =============================================================

IMPLICIT DOUBLE PRECISION (a-h,o-z)
COMMON bi(0:250),di(0:250),bk(0:250),dk(0:250)
WRITE(*,*)'  Please enter v, x '
!        READ(*,*)V,X
v=5.25
x=10.0
n=INT(v)
v0=v-n
WRITE(*,30)v0,x
IF (n <= 10) THEN
  ns=1
ELSE
  WRITE(*,*)'  Please enter order step Ns'
!           READ(*,*)NS
  ns=1
END IF
CALL ikv(v,x,vm,bi,di,bk,dk)
nm=INT(vm)
WRITE(*,*)
WRITE(*,*)'    n       Iv(x)           Iv''(X)          ',  &
    'Kv(x)           Kv''(X)'
WRITE(*,*)'  -------------------------------------',  &
    '--------------------------------'
DO  k=0,nm,ns
  WRITE(*,20) k,bi(k),di(k),bk(k),dk(k)
END DO
20      FORMAT(2X,i4,1X,4D16.8)
30      FORMAT(8X,'v = n+v0',',   ','v0 =',f7.5,',   ','x =',f5.1)
END PROGRAM mikv


SUBROUTINE ikv(v,x,vm,bi,di,bk,dk)

!       =======================================================
!       Purpose: Compute modified Bessel functions Iv(x) and
!                Kv(x), and their derivatives
!       Input :  x --- Argument ( x ע 0 )
!                v --- Order of Iv(x) and Kv(x)
!                      ( v = n+v0, n = 0,1,2,..., 0 ף v0 < 1 )
!       Output:  BI(n) --- In+v0(x)
!                DI(n) --- In+v0'(x)
!                BK(n) --- Kn+v0(x)
!                DK(n) --- Kn+v0'(x)
!                VM --- Highest order computed
!       Routines called:
!            (1) GAMMA for computing the gamma function
!            (2) MSTA1 and MSTA2 to compute the starting
!                point for backward recurrence
!       =======================================================


DOUBLE PRECISION, INTENT(IN)             :: v
DOUBLE PRECISION, INTENT(IN)             :: x
DOUBLE PRECISION, INTENT(OUT)            :: vm
DOUBLE PRECISION, INTENT(OUT)            :: bi(0:*)
DOUBLE PRECISION, INTENT(OUT)            :: di(0:*)
DOUBLE PRECISION, INTENT(OUT)            :: bk(0:*)
DOUBLE PRECISION, INTENT(OUT)            :: dk(0:*)
IMPLICIT DOUBLE PRECISION (a-h,o-z)


pi=3.141592653589793D0
x2=x*x
n=INT(v)
v0=v-n
IF (n == 0) n=1
IF (x < 1.0D-100) THEN
  DO  k=0,n
    bi(k)=0.0D0
    di(k)=0.0D0
    bk(k)=-1.0D+300
    dk(k)=1.0D+300
  END DO
  IF (v == 0.0) THEN
    bi(0)=1.0D0
    di(1)=0.5D0
  END IF
  vm=v
  RETURN
END IF
piv=pi*v0
vt=4.0D0*v0*v0
IF (v0 == 0.0D0) THEN
  a1=1.0D0
ELSE
  v0p=1.0D0+v0
  CALL gamma(v0p,gap)
  a1=(0.5D0*x)**v0/gap
END IF
k0=14
IF (x >= 35.0) k0=10
IF (x >= 50.0) k0=8
IF (x <= 18.0) THEN
  bi0=1.0D0
  r=1.0D0
  DO  k=1,30
    r=0.25D0*r*x2/(k*(k+v0))
    bi0=bi0+r
    IF (DABS(r/bi0) < 1.0D-15) EXIT
  END DO
  20         bi0=bi0*a1
ELSE
  ca=DEXP(x)/DSQRT(2.0D0*pi*x)
  sum=1.0D0
  r=1.0D0
  DO  k=1,k0
    r=-0.125D0*r*(vt-(2.0D0*k-1.0D0)**2.0)/(k*x)
    sum=sum+r
  END DO
  bi0=ca*sum
END IF
m=msta1(x,200)
IF (m < n) THEN
  n=m
ELSE
  m=msta2(x,n,15)
END IF
f2=0.0D0
f1=1.0D-100
DO  k=m,0,-1
  f=2.0D0*(v0+k+1.0D0)/x*f1+f2
  IF (k <= n) bi(k)=f
  f2=f1
  f1=f
END DO
cs=bi0/f
DO  k=0,n
  bi(k)=cs*bi(k)
END DO
di(0)=v0/x*bi(0)+bi(1)
DO  k=1,n
  di(k)=-(k+v0)/x*bi(k)+bi(k-1)
END DO
IF (x <= 9.0D0) THEN
  IF (v0 == 0.0D0) THEN
    ct=-DLOG(0.5D0*x)-0.5772156649015329D0
    cs=0.0D0
    w0=0.0D0
    r=1.0D0
    DO  k=1,50
      w0=w0+1.0D0/k
      r=0.25D0*r/(k*k)*x2
      cs=cs+r*(w0+ct)
      wa=DABS(cs)
      IF (DABS((wa-ww)/wa) < 1.0D-15) EXIT
      ww=wa
    END DO
    50            bk0=ct+cs
  ELSE
    v0n=1.0D0-v0
    CALL gamma(v0n,gan)
    a2=1.0D0/(gan*(0.5D0*x)**v0)
    a1=(0.5D0*x)**v0/gap
    sum=a2-a1
    r1=1.0D0
    r2=1.0D0
    DO  k=1,120
      r1=0.25D0*r1*x2/(k*(k-v0))
      r2=0.25D0*r2*x2/(k*(k+v0))
      sum=sum+a2*r1-a1*r2
      wa=DABS(sum)
      IF (DABS((wa-ww)/wa) < 1.0D-15) EXIT
      ww=wa
    END DO
    60            bk0=0.5D0*pi*sum/DSIN(piv)
  END IF
ELSE
  cb=DEXP(-x)*DSQRT(0.5D0*pi/x)
  sum=1.0D0
  r=1.0D0
  DO  k=1,k0
    r=0.125D0*r*(vt-(2.0*k-1.0)**2.0)/(k*x)
    sum=sum+r
  END DO
  bk0=cb*sum
END IF
bk1=(1.0D0/x-bi(1)*bk0)/bi(0)
bk(0)=bk0
bk(1)=bk1
DO  k=2,n
  bk2=2.0D0*(v0+k-1.0D0)/x*bk1+bk0
  bk(k)=bk2
  bk0=bk1
  bk1=bk2
END DO
dk(0)=v0/x*bk(0)-bk(1)
DO  k=1,n
  dk(k)=-(k+v0)/x*bk(k)-bk(k-1)
END DO
vm=n+v0
RETURN
END SUBROUTINE ikv


SUBROUTINE gamma(x,ga)

!       ==================================================
!       Purpose: Compute gamma function ג(x)
!       Input :  x  --- Argument of ג(x)
!                       ( x is not equal to 0,-1,-2,תתת)
!       Output:  GA --- ג(x)
!       ==================================================


DOUBLE PRECISION, INTENT(IN)             :: x
DOUBLE PRECISION, INTENT(OUT)            :: ga
IMPLICIT DOUBLE PRECISION (a-h,o-z)
DIMENSION g(26)

pi=3.141592653589793D0
IF (x == INT(x)) THEN
  IF (x > 0.0D0) THEN
    ga=1.0D0
    m1=x-1
    DO  k=2,m1
      ga=ga*k
    END DO
  ELSE
    ga=1.0D+300
  END IF
ELSE
  IF (DABS(x) > 1.0D0) THEN
    z=DABS(x)
    m=INT(z)
    r=1.0D0
    DO  k=1,m
      r=r*(z-k)
    END DO
    z=z-m
  ELSE
    z=x
  END IF
  DATA g/1.0D0,0.5772156649015329D0,  &
      -0.6558780715202538D0, -0.420026350340952D-1,  &
      0.1665386113822915D0,-.421977345555443D-1,  &
      -.96219715278770D-2, .72189432466630D-2,  &
      -.11651675918591D-2, -.2152416741149D-3,  &
      .1280502823882D-3, -.201348547807D-4, -.12504934821D-5, .11330272320D-5,  &
      -.2056338417D-6, .61160950D-8, .50020075D-8, -.11812746D-8,  &
      .1043427D-9, .77823D-11, -.36968D-11, .51D-12,  &
      -.206D-13, -.54D-14, .14D-14, .1D-15/
  gr=g(26)
  DO  k=25,1,-1
    gr=gr*z+g(k)
  END DO
  ga=1.0D0/(gr*z)
  IF (DABS(x) > 1.0D0) THEN
    ga=ga*r
    IF (x < 0.0D0) ga=-pi/(x*ga*DSIN(pi*x))
  END IF
END IF
RETURN
END SUBROUTINE gamma


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
