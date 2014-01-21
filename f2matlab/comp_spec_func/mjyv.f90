PROGRAM mjyv
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:11

!     ============================================================
!     Purpose: This program computes Bessel functions Jv(x) and
!     Yv(x) and their derivatives using subroutine JYV
!     Input :  x --- Argument of Jv(x) and Yv(x)
!     v --- Order of Jv(x) and Yv(x)
!     ( v = n+v0,  0 ף n ף 250, 0 ף v0 < 1 )
!     Output:  BJ(n) --- Jn+v0(x)
!     DJ(n) --- Jn+v0'(x)
!     BY(n) --- Yn+v0(x)
!     DY(n) --- Yn+v0'(x)
!     Example: Compute Jv(x) and Yv(x) and their derivatives
!     for v = 0.25(1.0)5.25 and x = 10.0
!     Computation results:

!     v =  5.25,      x = 10.00

!     v        Jv(x)         Jv'(x)        Yv(x)         Yv'(x)
!     ------------------------------------------------------------
!     .25   -.20639379    -.13476340     .14493044    -.21381777
!     1.25    .12960355    -.22259423     .21744103     .11775031
!     2.25    .23879467     .07587475    -.09057018     .23781932
!     3.25   -.02214595     .24599211    -.25819761    -.00665596
!     4.25   -.25318954     .08545961    -.07725827    -.22536285
!     5.25   -.19306516    -.15183033     .19252809    -.17833551
!     ============================================================

IMPLICIT DOUBLE PRECISION (a-h,o-z)
COMMON bj(0:250),dj(0:250),by(0:250),dy(0:250)
WRITE(*,*)'  Please enter v, x '
!     READ(*,*)V,X
v=5.25
x=10.0
WRITE(*,20)v,x
IF (v <= 8) THEN
  ns=1
ELSE
  WRITE(*,*)'  Please enter order step Ns'
!      READ(*,*)NS
  ns=2
END IF
CALL jyv(v,x,vm,bj,dj,by,dy)
nm=INT(vm)
v0=vm-nm
WRITE(*,*)
WRITE(*,*)'   v         Jv(x)           Jv''(X)',  &
    '          Yv(x)           Yv''(X)'
WRITE(*,*)' ---------------------------------------------',  &
    '------------------------'
DO  k=0,nm,ns
  vk=k+v0
  WRITE(*,15)vk,bj(k),dj(k),by(k),dy(k)
END DO
15   FORMAT(1X,f6.2,4D16.8)
20   FORMAT(8X,3HV =,f6.2,',    ',3HX =,f6.2)
END PROGRAM mjyv


SUBROUTINE jyv(v,x,vm,bj,dj,by,dy)

!     =======================================================
!     Purpose: Compute Bessel functions Jv(x) and Yv(x)
!     and their derivatives
!     Input :  x --- Argument of Jv(x) and Yv(x)
!     v --- Order of Jv(x) and Yv(x)
!     ( v = n+v0, 0 ף v0 < 1, n = 0,1,2,... )
!     Output:  BJ(n) --- Jn+v0(x)
!     DJ(n) --- Jn+v0'(x)
!     BY(n) --- Yn+v0(x)
!     DY(n) --- Yn+v0'(x)
!     VM --- Highest order computed
!     Routines called:
!     (1) GAMMA for computing gamma function
!     (2) MSTA1 and MSTA2 for computing the starting
!     point for backward recurrence
!     =======================================================


DOUBLE PRECISION, INTENT(IN)             :: v
DOUBLE PRECISION, INTENT(IN)             :: x
DOUBLE PRECISION, INTENT(OUT)            :: vm
DOUBLE PRECISION, INTENT(OUT)            :: bj(0:*)
DOUBLE PRECISION, INTENT(OUT)            :: dj(0:*)
DOUBLE PRECISION, INTENT(OUT)            :: by(0:*)
DOUBLE PRECISION, INTENT(OUT)            :: dy(0:*)
IMPLICIT DOUBLE PRECISION (a-h,o-z)


el=.5772156649015329D0
pi=3.141592653589793D0
rp2=.63661977236758D0
x2=x*x
n=INT(v)
v0=v-n
IF (x < 1.0D-100) THEN
  DO  k=0,n
    bj(k)=0.0D0
    dj(k)=0.0D0
    by(k)=-1.0D+300
    dy(k)=1.0D+300
  END DO
  IF (v0 == 0.0) THEN
    bj(0)=1.0D0
    dj(1)=0.5D0
  ELSE
    dj(0)=1.0D+300
  END IF
  vm=v
  RETURN
END IF
IF (x <= 12.0) THEN
  DO  l=0,1
    vl=v0+l
    bjvl=1.0D0
    r=1.0D0
    DO  k=1,40
      r=-0.25D0*r*x2/(k*(k+vl))
      bjvl=bjvl+r
      IF (DABS(r) < DABS(bjvl)*1.0D-15) EXIT
    END DO
    20     vg=1.0D0+vl
    CALL gamma(vg,ga)
    a=(0.5D0*x)**vl/ga
    IF (l == 0) bjv0=bjvl*a
    IF (l == 1) bjv1=bjvl*a
  END DO
ELSE
  k0=11
  IF (x >= 35.0) k0=10
  IF (x >= 50.0) k0=8
  DO  j=0,1
    vv=4.0D0*(j+v0)*(j+v0)
    px=1.0D0
    rp=1.0D0
    DO  k=1,k0
      rp=-0.78125D-2*rp*(vv-(4.0*k-3.0)**2.0)*(vv-  &
          (4.0*k-1.0)**2.0)/(k*(2.0*k-1.0)*x2)
      px=px+rp
    END DO
    qx=1.0D0
    rq=1.0D0
    DO  k=1,k0
      rq=-0.78125D-2*rq*(vv-(4.0*k-1.0)**2.0)*(vv-  &
          (4.0*k+1.0)**2.0)/(k*(2.0*k+1.0)*x2)
      qx=qx+rq
    END DO
    qx=0.125D0*(vv-1.0)*qx/x
    xk=x-(0.5D0*(j+v0)+0.25D0)*pi
    a0=DSQRT(rp2/x)
    ck=DCOS(xk)
    sk=DSIN(xk)
    IF (j == 0) THEN
      bjv0=a0*(px*ck-qx*sk)
      byv0=a0*(px*sk+qx*ck)
    ELSE IF (j == 1) THEN
      bjv1=a0*(px*ck-qx*sk)
      byv1=a0*(px*sk+qx*ck)
    END IF
  END DO
END IF
bj(0)=bjv0
bj(1)=bjv1
dj(0)=v0/x*bj(0)-bj(1)
dj(1)=-(1.0D0+v0)/x*bj(1)+bj(0)
IF (n >= 2.AND.n <= INT(0.9*x)) THEN
  f0=bjv0
  f1=bjv1
  DO  k=2,n
    f=2.0D0*(k+v0-1.0D0)/x*f1-f0
    bj(k)=f
    f0=f1
    f1=f
  END DO
ELSE IF (n >= 2) THEN
  m=msta1(x,200)
  IF (m < n) THEN
    n=m
  ELSE
    m=msta2(x,n,15)
  END IF
  f2=0.0D0
  f1=1.0D-100
  DO  k=m,0,-1
    f=2.0D0*(v0+k+1.0D0)/x*f1-f2
    IF (k <= n) bj(k)=f
    f2=f1
    f1=f
  END DO
  IF (DABS(bjv0) > DABS(bjv1)) THEN
    cs=bjv0/f
  ELSE
    cs=bjv1/f2
  END IF
  DO  k=0,n
    bj(k)=cs*bj(k)
  END DO
END IF
DO  k=2,n
  dj(k)=-(k+v0)/x*bj(k)+bj(k-1)
END DO
IF (x <= 12.0D0) THEN
  IF (v0 /= 0.0) THEN
    DO  l=0,1
      vl=v0+l
      bjvl=1.0D0
      r=1.0D0
      DO  k=1,40
        r=-0.25D0*r*x2/(k*(k-vl))
        bjvl=bjvl+r
        IF (DABS(r) < DABS(bjvl)*1.0D-15) EXIT
      END DO
      70      vg=1.0D0-vl
      CALL gamma(vg,gb)
      b=(2.0D0/x)**vl/gb
      IF (l == 0) bju0=bjvl*b
      IF (l == 1) bju1=bjvl*b
    END DO
    pv0=pi*v0
    pv1=pi*(1.0D0+v0)
    byv0=(bjv0*DCOS(pv0)-bju0)/DSIN(pv0)
    byv1=(bjv1*DCOS(pv1)-bju1)/DSIN(pv1)
  ELSE
    ec=DLOG(x/2.0D0)+el
    cs0=0.0D0
    w0=0.0D0
    r0=1.0D0
    DO  k=1,30
      w0=w0+1.0D0/k
      r0=-0.25D0*r0/(k*k)*x2
      cs0=cs0+r0*w0
    END DO
    byv0=rp2*(ec*bjv0-cs0)
    cs1=1.0D0
    w1=0.0D0
    r1=1.0D0
    DO  k=1,30
      w1=w1+1.0D0/k
      r1=-0.25D0*r1/(k*(k+1))*x2
      cs1=cs1+r1*(2.0D0*w1+1.0D0/(k+1.0D0))
    END DO
    byv1=rp2*(ec*bjv1-1.0D0/x-0.25D0*x*cs1)
  END IF
END IF
by(0)=byv0
by(1)=byv1
DO  k=2,n
  byvk=2.0D0*(v0+k-1.0D0)/x*byv1-byv0
  by(k)=byvk
  byv0=byv1
  byv1=byvk
END DO
dy(0)=v0/x*by(0)-by(1)
DO  k=1,n
  dy(k)=-(k+v0)/x*by(k)+by(k-1)
END DO
vm=n+v0
RETURN
END SUBROUTINE jyv


SUBROUTINE gamma(x,ga)

!     ==================================================
!     Purpose: Compute gamma function ג(x)
!     Input :  x  --- Argument of ג(x)
!     ( x is not equal to 0,-1,-2,תתת)
!     Output:  GA --- ג(x)
!     ==================================================


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

!     ===================================================
!     Purpose: Determine the starting point for backward
!     recurrence such that the magnitude of
!     Jn(x) at that point is about 10^(-MP)
!     Input :  x     --- Argument of Jn(x)
!     MP    --- Value of magnitude
!     Output:  MSTA1 --- Starting point
!     ===================================================



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
20   msta1=INT(nn)
RETURN
END FUNCTION msta1


INTEGER FUNCTION msta2(x,n,mp)

!     ===================================================
!     Purpose: Determine the starting point for backward
!     recurrence such that all Jn(x) has MP
!     significant digits
!     Input :  x  --- Argument of Jn(x)
!     n  --- Order of Jn(x)
!     MP --- Significant digit
!     Output:  MSTA2 --- Starting point
!     ===================================================



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
20   msta2=INT(nn+10)
RETURN
END FUNCTION msta2

REAL*8 FUNCTION envj(n,x)


INTEGER, INTENT(IN)                      :: n
DOUBLE PRECISION, INTENT(IN)             :: x

envj=0.5D0*DLOG10(6.28D0*n)-n*DLOG10(1.36D0*x/n)
RETURN
END FUNCTION envj
