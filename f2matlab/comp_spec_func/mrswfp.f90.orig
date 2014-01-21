PROGRAM mrswfp
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:12

!     ==============================================================
!     Purpose: This program computes the radial prolate spheriodal
!     functions of the first and second kinds, and their
!     derivatives using subroutine RSWFP
!     Input :  m  --- Mode parameter, m = 0,1,2,...
!     n  --- Mode parameter, n = m,m+1,m+2,...
!     c  --- Spheroidal parameter
!     cv --- Characteristic value
!     x  --- Argument of radial function ( x > 1.0 )
!     KF --- Function code
!     KF=1 for the first kind
!     KF=2 for the second kind
!     KF=3 for both the first and second kinds
!     Output:  R1F --- Radial function of the first kind
!     R1D --- Derivative of the radial function of
!     the first kind
!     R2F --- Radial function of the second kind
!     R2D --- Derivative of the radial function of
!     the second kind
!     Example:
!     KD= 1, m = 2, n = 3, c = 5.0 and cv =19.1359819110

!     x      R23(1)(c,x)   R23(1)'(c,x)    R23(2)(c,x)   R23(2)'(c,x)
!     ------------------------------------------------------------------
!     -
!     1.(7).1 (-8) 1.6240735      1.6240735 ( 6)-3.0786785 (14) 3
!     .0786784
!     1.005  (-3) 8.0600009      1.5998506     -6.2737713 ( 3) 1.2299041
!     1.1   (-1) 1.3578875      1.0702727 (-1)-4.3693218      3.5698419
!     1.5   (-1) 1.2002958 (-1)-8.0657929 (-1) 1.2623910 (-1) 4.8469847
!     5.0   (-2) 3.9455888 (-2) 4.2556949 (-2)-1.0099734 (-1) 2.0031280
!     ==============================================================

IMPLICIT DOUBLE PRECISION (a-h,o-z)
DIMENSION eg(200)
WRITE(*,*)'Please enter: KF --- Kind choice code'
WRITE(*,*)'KF = ?'
!     READ(*,*)KF
kf=3
WRITE(*,10)kf
WRITE(*,*)'Please enter m, n, c and x ( x > 1 )'
!     READ(*,*)M,N,C,X
m=2
n=3
c=5.0
x=5.0
CALL segv(m,n,c,1,cv,eg)
WRITE(*,20)m,n,c,cv,x
WRITE(*,*)
CALL rswfp(m,n,c,x,cv,kf,r1f,r1d,r2f,r2d)
WRITE(*,*)'   x      Rmn(1)(c,x)      Rmn''(1)(C,X)     ',  &
    'Rmn(2)(c,x)      Rmn''(2)(C,X)'
WRITE(*,*)'-----------------------------------------------',  &
    '---------------------------'
IF (kf == 1) THEN
  WRITE(*,30)x,r1f,r1d
ELSE IF (kf == 2) THEN
  WRITE(*,40)x,r2f,r2d
ELSE IF (kf == 3) THEN
  WRITE(*,30)x,r1f,r1d,r2f,r2d
END IF
IF (kf == 3) THEN
  WRITE(*,50)r1f*r2d-r2f*r1d,1.0D0/(c*(x*x-1.0D0))
  WRITE(*,60)
END IF
10   FORMAT(1X,3HKF=,i3)
20   FORMAT(1X,2HM=,i2,',   ',2HN=,i3,',   ',2HC=,f5.1,  &
    ',   ',4HCV =,d20.12,',  ',2HX=,f5.2)
30   FORMAT(1X,f5.2,4D17.8)
40   FORMAT(1X,f5.2,34X,4D17.8)
50   FORMAT(1X,/1X,'Wronskian check:',/1X,'Computed value =',  &
    d17.8,5X,'Exact value =',d17.8)
60   FORMAT(1X,/1X,'Caution: This check is not accurate if it ',  &
    'involves',/1X,'         the subtraction of two ', 'similar numbers')
END PROGRAM mrswfp


SUBROUTINE rswfp(m,n,c,x,cv,kf,r1f,r1d,r2f,r2d)

!     ==============================================================
!     Purpose: Compute prolate spheriodal radial functions of the
!     first and second kinds, and their derivatives
!     Input :  m  --- Mode parameter, m = 0,1,2,...
!     n  --- Mode parameter, n = m,m+1,m+2,...
!     c  --- Spheroidal parameter
!     x  --- Argument of radial function ( x > 1.0 )
!     cv --- Characteristic value
!     KF --- Function code
!     KF=1 for the first kind
!     KF=2 for the second kind
!     KF=3 for both the first and second kinds
!     Output:  R1F --- Radial function of the first kind
!     R1D --- Derivative of the radial function of
!     the first kind
!     R2F --- Radial function of the second kind
!     R2D --- Derivative of the radial function of
!     the second kind
!     Routines called:
!     (1) SDMN for computing expansion coefficients dk
!     (2) RMN1 for computing prolate and oblate radial
!     functions of the first kind
!     (3) RMN2L for computing prolate and oblate radial
!     functions of the second kind for a large argument
!     (4) RMN2SP for computing the prolate radial function
!     of the second kind for a small argument
!     ==============================================================

IMPLICIT DOUBLE PRECISION (a-h,o-z)
INTEGER, INTENT(IN OUT)                  :: m
INTEGER, INTENT(IN OUT)                  :: n
DOUBLE PRECISION, INTENT(IN OUT)         :: c
DOUBLE PRECISION, INTENT(IN OUT)         :: x
DOUBLE PRECISION, INTENT(IN OUT)         :: cv
INTEGER, INTENT(IN)                      :: kf
DOUBLE PRECISION, INTENT(IN OUT)         :: r1f
DOUBLE PRECISION, INTENT(IN OUT)         :: r1d
DOUBLE PRECISION, INTENT(IN OUT)         :: r2f
DOUBLE PRECISION, INTENT(IN OUT)         :: r2d
DIMENSION df(200)

kd=1
CALL sdmn(m,n,c,cv,kd,df)
IF (kf /= 2) THEN
  CALL rmn1(m,n,c,x,df,kd,r1f,r1d)
END IF
IF (kf > 1) THEN
  CALL rmn2l(m,n,c,x,df,kd,r2f,r2d,id)
  IF (id > -8) THEN
    CALL rmn2sp(m,n,c,x,cv,df,kd,r2f,r2d)
  END IF
END IF
RETURN
END SUBROUTINE rswfp


SUBROUTINE sdmn(m,n,c,cv,kd,df)

!     =====================================================
!     Purpose: Compute the expansion coefficients of the
!     prolate and oblate spheroidal functions, dk
!     Input :  m  --- Mode parameter
!     n  --- Mode parameter
!     c  --- Spheroidal parameter
!     cv --- Characteristic value
!     KD --- Function code
!     KD=1 for prolate; KD=-1 for oblate
!     Output:  DF(k) --- Expansion coefficients dk;
!     DF(1), DF(2), ... correspond to
!     d0, d2, ... for even n-m and d1,
!     d3, ... for odd n-m
!     =====================================================

IMPLICIT DOUBLE PRECISION (a-h,o-z)
INTEGER, INTENT(IN)                      :: m
INTEGER, INTENT(IN)                      :: n
DOUBLE PRECISION, INTENT(IN)             :: c
DOUBLE PRECISION, INTENT(IN OUT)         :: cv
INTEGER, INTENT(IN)                      :: kd
DOUBLE PRECISION, INTENT(OUT)            :: df(200)
DIMENSION a(200),d(200),g(200)

sw=0.0
fl=0.0
nm=25+INT(0.5*(n-m)+c)
IF (c < 1.0D-10) THEN
  DO  i=1,nm
    df(i)=0D0
  END DO
  df((n-m)/2+1)=1.0D0
  RETURN
END IF
cs=c*c*kd
ip=1
IF (n-m == 2*INT((n-m)/2)) ip=0
DO  i=1,nm+2
  IF (ip == 0) k=2*(i-1)
  IF (ip == 1) k=2*i-1
  dk0=m+k
  dk1=m+k+1
  dk2=2*(m+k)
  d2k=2*m+k
  a(i)=(d2k+2.0)*(d2k+1.0)/((dk2+3.0)*(dk2+5.0))*cs
  d(i)=dk0*dk1+(2.0*dk0*dk1-2.0*m*m-1.0)/((dk2-1.0) *(dk2+3.0))*cs
  g(i)=k*(k-1.0)/((dk2-3.0)*(dk2-1.0))*cs
END DO
fs=1.0D0
f1=0.0D0
f0=1.0D-100
kb=0
df(nm+1)=0.0D0
DO  k=nm,1,-1
  f=-((d(k+1)-cv)*f0+a(k+1)*f1)/g(k+1)
  IF (DABS(f) > DABS(df(k+1))) THEN
    df(k)=f
    f1=f0
    f0=f
    IF (DABS(f) > 1.0D+100) THEN
      DO  k1=k,nm
        df(k1)=df(k1)*1.0D-100
      END DO
      f1=f1*1.0D-100
      f0=f0*1.0D-100
    END IF
  ELSE
    kb=k
    fl=df(k+1)
    f1=1.0D-100
    f2=-(d(1)-cv)/a(1)*f1
    df(1)=f1
    IF (kb == 1) THEN
      fs=f2
    ELSE IF (kb == 2) THEN
      df(2)=f2
      fs=-((d(2)-cv)*f2+g(2)*f1)/a(2)
    ELSE
      df(2)=f2
      DO  j=3,kb+1
        f=-((d(j-1)-cv)*f2+g(j-1)*f1)/a(j-1)
        IF (j <= kb) df(j)=f
        IF (DABS(f) > 1.0D+100) THEN
          DO  k1=1,j
            df(k1)=df(k1)*1.0D-100
          END DO
          f=f*1.0D-100
          f2=f2*1.0D-100
        END IF
        f1=f2
        f2=f
      END DO
      fs=f
    END IF
    EXIT
  END IF
END DO
35   su1=0.0D0
r1=1.0D0
DO  j=m+ip+1,2*(m+ip)
  r1=r1*j
END DO
su1=df(1)*r1
DO  k=2,kb
  r1=-r1*(k+m+ip-1.5D0)/(k-1.0D0)
  su1=su1+r1*df(k)
END DO
su2=0.0D0
DO  k=kb+1,nm
  IF (k /= 1) r1=-r1*(k+m+ip-1.5D0)/(k-1.0D0)
  su2=su2+r1*df(k)
  IF (DABS(sw-su2) < DABS(su2)*1.0D-14) EXIT
  sw=su2
END DO
55   r3=1.0D0
DO  j=1,(m+n+ip)/2
  r3=r3*(j+0.5D0*(n+m+ip))
END DO
r4=1.0D0
DO  j=1,(n-m-ip)/2
  r4=-4.0D0*r4*j
END DO
s0=r3/(fl*(su1/fs)+su2)/r4
DO  k=1,kb
  df(k)=fl/fs*s0*df(k)
END DO
DO  k=kb+1,nm
  df(k)=s0*df(k)
END DO
RETURN
END SUBROUTINE sdmn


SUBROUTINE rmn1(m,n,c,x,df,kd,r1f,r1d)

!     =======================================================
!     Purpose: Compute prolate and oblate spheroidal radial
!     functions of the first kind for given m, n, c and x
!     Routines called:
!     (1) SCKB for computing expansion coefficients c2k
!     (2) SPHJ for computing the spherical Bessel
!     functions of the first kind
!     =======================================================

IMPLICIT DOUBLE PRECISION (a-h,o-z)
INTEGER, INTENT(IN)                      :: m
INTEGER, INTENT(IN)                      :: n
DOUBLE PRECISION, INTENT(IN)             :: c
DOUBLE PRECISION, INTENT(IN)             :: x
DOUBLE PRECISION, INTENT(IN)             :: df(200)
INTEGER, INTENT(IN)                      :: kd
DOUBLE PRECISION, INTENT(OUT)            :: r1f
DOUBLE PRECISION, INTENT(OUT)            :: r1d
DIMENSION ck(200), sj(0:251),dj(0:251)

sw=0.0
eps=1.0D-14
ip=1
nm1=INT((n-m)/2)
IF (n-m == 2*nm1) ip=0
nm=25+nm1+INT(c)
reg=1.0D0
IF (m+nm > 80) reg=1.0D-200
r0=reg
DO  j=1,2*m+ip
  r0=r0*j
END DO
r=r0
suc=r*df(1)
DO  k=2,nm
  r=r*(m+k-1.0)*(m+k+ip-1.5D0)/(k-1.0D0)/(k+ip-1.5D0)
  suc=suc+r*df(k)
  IF (k > nm1.AND.DABS(suc-sw) < DABS(suc)*eps) EXIT
  sw=suc
END DO
IF (x == 0.0) THEN
  CALL sckb(m,n,c,df,ck)
  sum=0.0D0
  DO  j=1,nm
    sum=sum+ck(j)
    IF (DABS(sum-sw1) < DABS(sum)*eps) EXIT
    sw1=sum
  END DO
  30    r1=1.0D0
  DO  j=1,(n+m+ip)/2
    r1=r1*(j+0.5D0*(n+m+ip))
  END DO
  r2=1.0D0
  DO  j=1,m
    r2=2.0D0*c*r2*j
  END DO
  r3=1.0D0
  DO  j=1,(n-m-ip)/2
    r3=r3*j
  END DO
  sa0=(2.0*(m+ip)+1.0)*r1/(2.0**n*c**ip*r2*r3)
  IF (ip == 0) THEN
    r1f=sum/(sa0*suc)*df(1)*reg
    r1d=0.0D0
  ELSE IF (ip == 1) THEN
    r1f=0.0D0
    r1d=sum/(sa0*suc)*df(1)*reg
  END IF
  RETURN
END IF
cx=c*x
nm2=2*nm+m
CALL sphj(nm2,cx,nm2,sj,dj)
a0=(1.0D0-kd/(x*x))**(0.5D0*m)/suc
r1f=0.0D0
DO  k=1,nm
  l=2*k+m-n-2+ip
  IF (l == 4*INT(l/4)) lg=1
  IF (l /= 4*INT(l/4)) lg=-1
  IF (k == 1) THEN
    r=r0
  ELSE
    r=r*(m+k-1.0)*(m+k+ip-1.5D0)/(k-1.0D0)/(k+ip-1.5D0)
  END IF
  np=m+2*k-2+ip
  r1f=r1f+lg*r*df(k)*sj(np)
  IF (k > nm1.AND.DABS(r1f-sw) < DABS(r1f)*eps) EXIT
  sw=r1f
END DO
55   r1f=r1f*a0
b0=kd*m/x**3.0D0/(1.0-kd/(x*x))*r1f
sud=0.0D0
DO  k=1,nm
  l=2*k+m-n-2+ip
  IF (l == 4*INT(l/4)) lg=1
  IF (l /= 4*INT(l/4)) lg=-1
  IF (k == 1) THEN
    r=r0
  ELSE
    r=r*(m+k-1.0)*(m+k+ip-1.5D0)/(k-1.0D0)/(k+ip-1.5D0)
  END IF
  np=m+2*k-2+ip
  sud=sud+lg*r*df(k)*dj(np)
  IF (k > nm1.AND.DABS(sud-sw) < DABS(sud)*eps) EXIT
  sw=sud
END DO
65   r1d=b0+a0*c*sud
RETURN
END SUBROUTINE rmn1


SUBROUTINE sckb(m,n,c,df,ck)

!     ======================================================
!     Purpose: Compute the expansion coefficients of the
!     prolate and oblate spheroidal functions, c2k
!     Input :  m  --- Mode parameter
!     n  --- Mode parameter
!     c  --- Spheroidal parameter
!     DF(k) --- Expansion coefficients dk
!     Output:  CK(k) --- Expansion coefficients ck;
!     CK(1), CK(2), ... correspond to
!     c0, c2, ...
!     ======================================================

IMPLICIT DOUBLE PRECISION (a-h,o-z)
INTEGER, INTENT(IN)                      :: m
INTEGER, INTENT(IN)                      :: n
DOUBLE PRECISION, INTENT(IN OUT)         :: c
DOUBLE PRECISION, INTENT(IN)             :: df(200)
DOUBLE PRECISION, INTENT(OUT)            :: ck(200)


IF (c <= 1.0D-10) c=1.0D-10
nm=25+INT(0.5*(n-m)+c)
ip=1
IF (n-m == 2*INT((n-m)/2)) ip=0
reg=1.0D0
IF (m+nm > 80) reg=1.0D-200
fac=-0.5D0**m
DO  k=0,nm-1
  fac=-fac
  i1=2*k+ip+1
  r=reg
  DO  i=i1,i1+2*m-1
    r=r*i
  END DO
  i2=k+m+ip
  DO  i=i2,i2+k-1
    r=r*(i+0.5D0)
  END DO
  sum=r*df(k+1)
  DO  i=k+1,nm
    d1=2.0D0*i+ip
    d2=2.0D0*m+d1
    d3=i+m+ip-0.5D0
    r=r*d2*(d2-1.0D0)*i*(d3+k)/(d1*(d1-1.0D0)*(i-k)*d3)
    sum=sum+r*df(i+1)
    IF (DABS(sw-sum) < DABS(sum)*1.0D-14) EXIT
    sw=sum
  END DO
  25    r1=reg
  DO  i=2,m+k
    r1=r1*i
  END DO
  ck(k+1)=fac*sum/r1
END DO
RETURN
END SUBROUTINE sckb


SUBROUTINE sphj(n,x,nm,sj,dj)

!     =======================================================
!     Purpose: Compute spherical Bessel functions jn(x) and
!     their derivatives
!     Input :  x --- Argument of jn(x)
!     n --- Order of jn(x)  ( n = 0,1,תתת )
!     Output:  SJ(n) --- jn(x)
!     DJ(n) --- jn'(x)
!     NM --- Highest order computed
!     Routines called:
!     MSTA1 and MSTA2 for computing the starting
!     point for backward recurrence
!     =======================================================

IMPLICIT DOUBLE PRECISION (a-h,o-z)
INTEGER, INTENT(IN)                      :: n
DOUBLE PRECISION, INTENT(IN)             :: x
INTEGER, INTENT(OUT)                     :: nm
DOUBLE PRECISION, INTENT(OUT)            :: sj(0:n)
DOUBLE PRECISION, INTENT(OUT)            :: dj(0:n)


nm=n
IF (DABS(x) == 1.0D-100) THEN
  DO  k=0,n
    sj(k)=0.0D0
    dj(k)=0.0D0
  END DO
  sj(0)=1.0D0
  dj(1)=.3333333333333333D0
  RETURN
END IF
sj(0)=DSIN(x)/x
sj(1)=(sj(0)-DCOS(x))/x
IF (n >= 2) THEN
  sa=sj(0)
  sb=sj(1)
  m=msta1(x,200)
  IF (m < n) THEN
    nm=m
  ELSE
    m=msta2(x,n,15)
  END IF
  f0=0.0D0
  f1=1.0D0-100
  DO  k=m,0,-1
    f=(2.0D0*k+3.0D0)*f1/x-f0
    IF (k <= nm) sj(k)=f
    f0=f1
    f1=f
  END DO
  IF (DABS(sa) > DABS(sb)) cs=sa/f
  IF (DABS(sa) <= DABS(sb)) cs=sb/f0
  DO  k=0,nm
    sj(k)=cs*sj(k)
  END DO
END IF
dj(0)=(DCOS(x)-DSIN(x)/x)/x
DO  k=1,nm
  dj(k)=sj(k-1)-(k+1.0D0)*sj(k)/x
END DO
RETURN
END SUBROUTINE sphj


INTEGER FUNCTION msta1(x,mp)

!     ===================================================
!     Purpose: Determine the starting point for backward
!     recurrence such that the magnitude of
!     Jn(x) at that point is about 10^(-MP)
!     Input :  x     --- Argument of Jn(x)
!     MP    --- Value of magnitude
!     Output:  MSTA1 --- Starting point
!     ===================================================


IMPLICIT DOUBLE PRECISION (a-h,o-z)
DOUBLE PRECISION, INTENT(IN OUT)         :: x
INTEGER, INTENT(IN)                      :: mp
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


IMPLICIT DOUBLE PRECISION (a-h,o-z)
DOUBLE PRECISION, INTENT(IN OUT)         :: x
INTEGER, INTENT(IN)                      :: n
INTEGER, INTENT(IN)                      :: mp
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


SUBROUTINE rmn2l(m,n,c,x,df,kd,r2f,r2d,id)

!     ========================================================
!     Purpose: Compute prolate and oblate spheroidal radial
!     functions of the second kind for given m, n,
!     c and a large cx
!     Routine called:
!     SPHY for computing the spherical Bessel
!     functions of the second kind
!     ========================================================

IMPLICIT DOUBLE PRECISION (a-h,o-z)
INTEGER, INTENT(IN)                      :: m
INTEGER, INTENT(IN)                      :: n
DOUBLE PRECISION, INTENT(IN)             :: c
DOUBLE PRECISION, INTENT(IN)             :: x
DOUBLE PRECISION, INTENT(IN)             :: df(200)
INTEGER, INTENT(IN)                      :: kd
DOUBLE PRECISION, INTENT(OUT)            :: r2f
DOUBLE PRECISION, INTENT(OUT)            :: r2d
INTEGER, INTENT(OUT)                     :: id
DIMENSION  sy(0:251),dy(0:251)

sw=0.0
eps=1.0D-14
ip=1
nm1=INT((n-m)/2)
IF (n-m == 2*nm1) ip=0
nm=25+nm1+INT(c)
reg=1.0D0
IF (m+nm > 80) reg=1.0D-200
nm2=2*nm+m
cx=c*x
CALL sphy(nm2,cx,nm2,sy,dy)
r0=reg
DO  j=1,2*m+ip
  r0=r0*j
END DO
r=r0
suc=r*df(1)
DO  k=2,nm
  r=r*(m+k-1.0)*(m+k+ip-1.5D0)/(k-1.0D0)/(k+ip-1.5D0)
  suc=suc+r*df(k)
  IF (k > nm1.AND.DABS(suc-sw) < DABS(suc)*eps) EXIT
  sw=suc
END DO
20   a0=(1.0D0-kd/(x*x))**(0.5D0*m)/suc
r2f=0.0
DO  k=1,nm
  l=2*k+m-n-2+ip
  IF (l == 4*INT(l/4)) lg=1
  IF (l /= 4*INT(l/4)) lg=-1
  IF (k == 1) THEN
    r=r0
  ELSE
    r=r*(m+k-1.0)*(m+k+ip-1.5D0)/(k-1.0D0)/(k+ip-1.5D0)
  END IF
  np=m+2*k-2+ip
  r2f=r2f+lg*r*(df(k)*sy(np))
  eps1=DABS(r2f-sw)
  IF (k > nm1.AND.eps1 < DABS(r2f)*eps) EXIT
  sw=r2f
END DO
55   id1=INT(LOG10(eps1/DABS(r2f)+eps))
r2f=r2f*a0
IF (np >= nm2) THEN
  id=10
  RETURN
END IF
b0=kd*m/x**3.0D0/(1.0-kd/(x*x))*r2f
sud=0.0D0
DO  k=1,nm
  l=2*k+m-n-2+ip
  IF (l == 4*INT(l/4)) lg=1
  IF (l /= 4*INT(l/4)) lg=-1
  IF (k == 1) THEN
    r=r0
  ELSE
    r=r*(m+k-1.0)*(m+k+ip-1.5D0)/(k-1.0D0)/(k+ip-1.5D0)
  END IF
  np=m+2*k-2+ip
  sud=sud+lg*r*(df(k)*dy(np))
  eps2=DABS(sud-sw)
  IF (k > nm1.AND.eps2 < DABS(sud)*eps) EXIT
  sw=sud
END DO
65   r2d=b0+a0*c*sud
id2=INT(LOG10(eps2/DABS(sud)+eps))
id=MAX(id1,id2)
RETURN
END SUBROUTINE rmn2l


SUBROUTINE sphy(n,x,nm,sy,dy)

!     ======================================================
!     Purpose: Compute spherical Bessel functions yn(x) and
!     their derivatives
!     Input :  x --- Argument of yn(x) ( x ע 0 )
!     n --- Order of yn(x) ( n = 0,1,תתת )
!     Output:  SY(n) --- yn(x)
!     DY(n) --- yn'(x)
!     NM --- Highest order computed
!     ======================================================

IMPLICIT DOUBLE PRECISION (a-h,o-z)
INTEGER, INTENT(IN)                      :: n
DOUBLE PRECISION, INTENT(IN)             :: x
INTEGER, INTENT(OUT)                     :: nm
DOUBLE PRECISION, INTENT(OUT)            :: sy(0:n)
DOUBLE PRECISION, INTENT(OUT)            :: dy(0:n)


nm=n
IF (x < 1.0D-60) THEN
  DO  k=0,n
    sy(k)=-1.0D+300
    dy(k)=1.0D+300
  END DO
  RETURN
END IF
sy(0)=-DCOS(x)/x
sy(1)=(sy(0)-DSIN(x))/x
f0=sy(0)
f1=sy(1)
DO  k=2,n
  f=(2.0D0*k-1.0D0)*f1/x-f0
  sy(k)=f
  IF (DABS(f) >= 1.0D+300) EXIT
  f0=f1
  f1=f
END DO
20   nm=k-1
dy(0)=(DSIN(x)+DCOS(x)/x)/x
DO  k=1,nm
  dy(k)=sy(k-1)-(k+1.0D0)*sy(k)/x
END DO
RETURN
END SUBROUTINE sphy


SUBROUTINE rmn2sp(m,n,c,x,cv,df,kd,r2f,r2d)

!     ======================================================
!     Purpose: Compute prolate spheroidal radial function
!     of the second kind with a small argument
!     Routines called:
!     (1) LPMNS for computing the associated Legendre
!     functions of the first kind
!     (2) LQMNS for computing the associated Legendre
!     functions of the second kind
!     (3) KMN for computing expansion coefficients
!     and joining factors
!     ======================================================

IMPLICIT DOUBLE PRECISION (a-h,o-z)
INTEGER, INTENT(IN)                      :: m
INTEGER, INTENT(IN)                      :: n
DOUBLE PRECISION, INTENT(IN OUT)         :: c
DOUBLE PRECISION, INTENT(IN)             :: x
DOUBLE PRECISION, INTENT(IN OUT)         :: cv
DOUBLE PRECISION, INTENT(IN)             :: df(200)
INTEGER, INTENT(IN OUT)                  :: kd
DOUBLE PRECISION, INTENT(OUT)            :: r2f
DOUBLE PRECISION, INTENT(OUT)            :: r2d
DIMENSION pm(0:251),pd(0:251),qm(0:251),qd(0:251), dn(200)

IF (DABS(df(1)) < 1.0D-280) THEN
  r2f=1.0D+300
  r2d=1.0D+300
  RETURN
END IF
eps=1.0D-14
ip=1
nm1=INT((n-m)/2)
IF (n-m == 2*nm1) ip=0
nm=25+nm1+INT(c)
nm2=2*nm+m
CALL kmn(m,n,c,cv,kd,df,dn,ck1,ck2)
CALL lpmns(m,nm2,x,pm,pd)
CALL lqmns(m,nm2,x,qm,qd)
su0=0.0D0
DO  k=1,nm
  j=2*k-2+m+ip
  su0=su0+df(k)*qm(j)
  IF (k > nm1.AND.DABS(su0-sw) < DABS(su0)*eps) EXIT
  sw=su0
END DO
15   sd0=0.0D0
DO  k=1,nm
  j=2*k-2+m+ip
  sd0=sd0+df(k)*qd(j)
  IF (k > nm1.AND.DABS(sd0-sw) < DABS(sd0)*eps) EXIT
  sw=sd0
END DO
25   su1=0.0D0
sd1=0.0D0
DO  k=1,m
  j=m-2*k+ip
  IF (j < 0) j=-j-1
  su1=su1+dn(k)*qm(j)
  sd1=sd1+dn(k)*qd(j)
END DO
ga=((x-1.0D0)/(x+1.0D0))**(0.5D0*m)
DO  k=1,m
  j=m-2*k+ip
  IF (.NOT.(j >= 0)) THEN
    IF (j < 0) j=-j-1
    r1=1.0D0
    DO  j1=1,j
      r1=(m+j1)*r1
    END DO
    r2=1.0D0
    DO  j2=1,m-j-2
      r2=j2*r2
    END DO
    r3=1.0D0
    sf=1.0D0
    DO  l1=1,j
      r3=0.5D0*r3*(-j+l1-1.0)*(j+l1)/((m+l1)*l1)*(1.0-x)
      sf=sf+r3
    END DO
    IF (m-j >= 2) gb=(m-j-1.0D0)*r2
    IF (m-j <= 1) gb=1.0D0
    spl=r1*ga*gb*sf
    su1=su1+(-1)**(j+m)*dn(k)*spl
    spd1=m/(x*x-1.0D0)*spl
    gc=0.5D0*j*(j+1.0)/(m+1.0)
    sd=1.0D0
    r4=1.0D0
    DO  l1=1,j-1
      r4=0.5D0*r4*(-j+l1)*(j+l1+1.0)/((m+l1+1.0)*l1) *(1.0-x)
      sd=sd+r4
    END DO
    spd2=r1*ga*gb*gc*sd
    sd1=sd1+(-1)**(j+m)*dn(k)*(spd1+spd2)
  END IF
END DO
su2=0.0D0
ki=int((2*m+1+ip)/2)
nm3=nm+ki
DO  k=ki,nm3
  j=2*k-1-m-ip
  su2=su2+dn(k)*pm(j)
  IF (j > m.AND.DABS(su2-sw) < DABS(su2)*eps) EXIT
  sw=su2
END DO
65   sd2=0.0D0
DO  k=ki,nm3
  j=2*k-1-m-ip
  sd2=sd2+dn(k)*pd(j)
  IF (j > m.AND.DABS(sd2-sw) < DABS(sd2)*eps) EXIT
  sw=sd2
END DO
75   sum=su0+su1+su2
sdm=sd0+sd1+sd2
r2f=sum/ck2
r2d=sdm/ck2
RETURN
END SUBROUTINE rmn2sp


SUBROUTINE lpmns(m,n,x,pm,pd)

!     ========================================================
!     Purpose: Compute associated Legendre functions Pmn(x)
!     and Pmn'(x) for a given order
!     Input :  x --- Argument of Pmn(x)
!     m --- Order of Pmn(x),  m = 0,1,2,...,n
!     n --- Degree of Pmn(x), n = 0,1,2,...,N
!     Output:  PM(n) --- Pmn(x)
!     PD(n) --- Pmn'(x)
!     ========================================================

IMPLICIT DOUBLE PRECISION (a-h,o-z)
INTEGER, INTENT(IN)                      :: m
INTEGER, INTENT(IN)                      :: n
DOUBLE PRECISION, INTENT(IN)             :: x
DOUBLE PRECISION, INTENT(OUT)            :: pm(0:n)
DOUBLE PRECISION, INTENT(OUT)            :: pd(0:n)


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


SUBROUTINE lqmns(m,n,x,qm,qd)

!     ========================================================
!     Purpose: Compute associated Legendre functions Qmn(x)
!     and Qmn'(x) for a given order
!     Input :  x --- Argument of Qmn(x)
!     m --- Order of Qmn(x),  m = 0,1,2,...
!     n --- Degree of Qmn(x), n = 0,1,2,...
!     Output:  QM(n) --- Qmn(x)
!     QD(n) --- Qmn'(x)
!     ========================================================

IMPLICIT DOUBLE PRECISION (a-h,o-z)
INTEGER, INTENT(IN)                      :: m
INTEGER, INTENT(IN)                      :: n
DOUBLE PRECISION, INTENT(IN)             :: x
DOUBLE PRECISION, INTENT(OUT)            :: qm(0:n)
DOUBLE PRECISION, INTENT(OUT)            :: qd(0:n)


DO  k=0,n
  qm(k)=0.0D0
  qd(k)=0.0D0
END DO
IF (DABS(x) == 1.0D0) THEN
  DO  k=0,n
    qm(k)=1.0D+300
    qd(k)=1.0D+300
  END DO
  RETURN
END IF
ls=1
IF (DABS(x) > 1.0D0) ls=-1
xq=DSQRT(ls*(1.0D0-x*x))
q0=0.5D0*DLOG(DABS((x+1.0)/(x-1.0)))
q00=q0
q10=-1.0D0/xq
q01=x*q0-1.0D0
q11=-ls*xq*(q0+x/(1.0D0-x*x))
qf0=q00
qf1=q10
DO  k=2,m
  qm0=-2.0D0*(k-1.0)/xq*x*qf1-ls*(k-1.0)*(2.0-k)*qf0
  qf0=qf1
  qf1=qm0
END DO
IF (m == 0) qm0=q00
IF (m == 1) qm0=q10
qm(0)=qm0
IF (DABS(x) < 1.0001D0) THEN
  IF (m == 0.AND.n > 0) THEN
    qf0=q00
    qf1=q01
    DO  k=2,n
      qf2=((2.0*k-1.0D0)*x*qf1-(k-1.0)*qf0)/k
      qm(k)=qf2
      qf0=qf1
      qf1=qf2
    END DO
  END IF
  qg0=q01
  qg1=q11
  DO  k=2,m
    qm1=-2.0D0*(k-1.0)/xq*x*qg1-ls*k*(3.0-k)*qg0
    qg0=qg1
    qg1=qm1
  END DO
  IF (m == 0) qm1=q01
  IF (m == 1) qm1=q11
  qm(1)=qm1
  IF (m == 1.AND.n > 1) THEN
    qh0=q10
    qh1=q11
    DO  k=2,n
      qh2=((2.0*k-1.0D0)*x*qh1-k*qh0)/(k-1.0)
      qm(k)=qh2
      qh0=qh1
      qh1=qh2
    END DO
  ELSE IF (m >= 2) THEN
    qg0=q00
    qg1=q01
    qh0=q10
    qh1=q11
    DO  l=2,n
      q0l=((2.0D0*l-1.0D0)*x*qg1-(l-1.0D0)*qg0)/l
      q1l=((2.0*l-1.0D0)*x*qh1-l*qh0)/(l-1.0D0)
      qf0=q0l
      qf1=q1l
      DO  k=2,m
        qmk=-2.0D0*(k-1.0)/xq*x*qf1-ls*(k+l-1.0)* (l+2.0-k)*qf0
        qf0=qf1
        qf1=qmk
      END DO
      qm(l)=qmk
      qg0=qg1
      qg1=q0l
      qh0=qh1
      qh1=q1l
    END DO
  END IF
ELSE
  IF (DABS(x) > 1.1) THEN
    km=40+m+n
  ELSE
    km=(40+m+n)*INT(-1.0-1.8*LOG(x-1.0))
  END IF
  qf2=0.0D0
  qf1=1.0D0
  DO  k=km,0,-1
    qf0=((2.0*k+3.0D0)*x*qf1-(k+2.0-m)*qf2)/(k+m+1.0)
    IF (k <= n) qm(k)=qf0
    qf2=qf1
    qf1=qf0
  END DO
  DO  k=0,n
    qm(k)=qm(k)*qm0/qf0
  END DO
END IF
IF (DABS(x) < 1.0D0) THEN
  DO  k=0,n
    qm(k)=(-1)**m*qm(k)
  END DO
END IF
qd(0)=((1.0D0-m)*qm(1)-x*qm(0))/(x*x-1.0)
DO  k=1,n
  qd(k)=(k*x*qm(k)-(k+m)*qm(k-1))/(x*x-1.0)
END DO
RETURN
END SUBROUTINE lqmns


SUBROUTINE kmn(m,n,c,cv,kd,df,dn,ck1,ck2)

!     ===================================================
!     Purpose: Compute the expansion coefficients of the
!     prolate and oblate spheroidal functions
!     and joining factors
!     ===================================================

IMPLICIT DOUBLE PRECISION (a-h,o-z)
INTEGER, INTENT(IN)                      :: m
INTEGER, INTENT(IN)                      :: n
DOUBLE PRECISION, INTENT(IN)             :: c
DOUBLE PRECISION, INTENT(IN)             :: cv
INTEGER, INTENT(IN)                      :: kd
DOUBLE PRECISION, INTENT(IN)             :: df(200)
DOUBLE PRECISION, INTENT(OUT)            :: dn(200)
DOUBLE PRECISION, INTENT(OUT)            :: ck1
DOUBLE PRECISION, INTENT(OUT)            :: ck2
DIMENSION u(200),v(200),w(200), tp(200),rk(200)

nm=25+INT(0.5*(n-m)+c)
nn=nm+m
cs=c*c*kd
ip=1
IF (n-m == 2*INT((n-m)/2)) ip=0
DO  i=1,nn+3
  IF (ip == 0) k=-2*(i-1)
  IF (ip == 1) k=-(2*i-3)
  gk0=2.0D0*m+k
  gk1=(m+k)*(m+k+1.0D0)
  gk2=2.0D0*(m+k)-1.0D0
  gk3=2.0D0*(m+k)+3.0D0
  u(i)=gk0*(gk0-1.0D0)*cs/(gk2*(gk2+2.0D0))
  v(i)=gk1-cv+(2.0D0*(gk1-m*m)-1.0D0)*cs/(gk2*gk3)
  w(i)=(k+1.0D0)*(k+2.0D0)*cs/((gk2+2.0D0)*gk3)
END DO
DO  k=1,m
  t=v(m+1)
  DO  l=0,m-k-1
    t=v(m-l)-w(m-l+1)*u(m-l)/t
  END DO
  rk(k)=-u(k)/t
END DO
r=1.0D0
DO  k=1,m
  r=r*rk(k)
  dn(k)=df(1)*r
END DO
tp(nn)=v(nn+1)
DO  k=nn-1,m+1,-1
  tp(k)=v(k+1)-w(k+2)*u(k+1)/tp(k+1)
  IF (k > m+1) rk(k)=-u(k)/tp(k)
END DO
IF (m == 0) dnp=df(1)
IF (m /= 0) dnp=dn(m)
dn(m+1)=(-1)**ip*dnp*cs/((2.0*m-1.0)*(2.0*m+1.0-4.0*ip) *tp(m+1))
DO  k=m+2,nn
  dn(k)=rk(k)*dn(k-1)
END DO
r1=1.0D0
DO  j=1,(n+m+ip)/2
  r1=r1*(j+0.5D0*(n+m+ip))
END DO
nm1=(n-m)/2
r=1.0D0
DO  j=1,2*m+ip
  r=r*j
END DO
su0=r*df(1)
DO  k=2,nm
  r=r*(m+k-1.0)*(m+k+ip-1.5D0)/(k-1.0D0)/(k+ip-1.5D0)
  su0=su0+r*df(k)
  IF (k > nm1.AND.DABS((su0-sw)/su0) < 1.0D-14) EXIT
  sw=su0
END DO
55   IF (.NOT.(kd == 1)) THEN
  r2=1.0D0
  DO  j=1,m
    r2=2.0D0*c*r2*j
  END DO
  r3=1.0D0
  DO  j=1,(n-m-ip)/2
    r3=r3*j
  END DO
  sa0=(2.0*(m+ip)+1.0)*r1/(2.0**n*c**ip*r2*r3*df(1))
  ck1=sa0*su0
  IF (kd == -1) RETURN
END IF
70   r4=1.0D0
DO  j=1,(n-m-ip)/2
  r4=4.0D0*r4*j
END DO
r5=1.0D0
DO  j=1,m
  r5=r5*(j+m)/c
END DO
g0=dn(m)
IF (m == 0) g0=df(1)
sb0=(ip+1.0)*c**(ip+1)/(2.0*ip*(m-2.0)+1.0)/(2.0*m-1.0)
ck2=(-1)**ip*sb0*r4*r5*g0/r1*su0
RETURN
END SUBROUTINE kmn


SUBROUTINE segv(m,n,c,kd,cv,eg)

!     =========================================================
!     Purpose: Compute the characteristic values of spheroidal
!     wave functions
!     Input :  m  --- Mode parameter
!     n  --- Mode parameter
!     c  --- Spheroidal parameter
!     KD --- Function code
!     KD=1 for Prolate; KD=-1 for Oblate
!     Output:  CV --- Characteristic value for given m, n and c
!     EG(L) --- Characteristic value for mode m and n'
!     ( L = n' - m + 1 )
!     =========================================================

IMPLICIT DOUBLE PRECISION (a-h,o-z)
INTEGER, INTENT(IN)                      :: m
INTEGER, INTENT(IN)                      :: n
DOUBLE PRECISION, INTENT(IN)             :: c
INTEGER, INTENT(IN)                      :: kd
DOUBLE PRECISION, INTENT(OUT)            :: cv
DOUBLE PRECISION, INTENT(OUT)            :: eg(200)
DIMENSION b(100),h(100),d(300),e(300),f(300),cv0(100),a(300) ,g(300)

IF (c < 1.0D-10) THEN
  DO  i=1,n
    eg(i)=(i+m)*(i+m-1.0D0)
  END DO
ELSE
  icm=INT((n-m+2)/2)
  nm=10+INT(0.5*(n-m)+c)
  cs=c*c*kd
  DO  l=0,1
    DO  i=1,nm
      IF (l == 0) k=2*(i-1)
      IF (l == 1) k=2*i-1
      dk0=m+k
      dk1=m+k+1
      dk2=2*(m+k)
      d2k=2*m+k
      a(i)=(d2k+2.0)*(d2k+1.0)/((dk2+3.0)*(dk2+5.0))*cs
      d(i)=dk0*dk1+(2.0*dk0*dk1-2.0*m*m-1.0)/((dk2-1.0) *(dk2+3.0))*cs
      g(i)=k*(k-1.0)/((dk2-3.0)*(dk2-1.0))*cs
    END DO
    DO  k=2,nm
      e(k)=DSQRT(a(k-1)*g(k))
      f(k)=e(k)*e(k)
    END DO
    f(1)=0.0D0
    e(1)=0.0D0
    xa=d(nm)+DABS(e(nm))
    xb=d(nm)-DABS(e(nm))
    nm1=nm-1
    DO  i=1,nm1
      t=DABS(e(i))+DABS(e(i+1))
      t1=d(i)+t
      IF (xa < t1) xa=t1
      t1=d(i)-t
      IF (t1 < xb) xb=t1
    END DO
    DO  i=1,icm
      b(i)=xa
      h(i)=xb
    END DO
    DO  k=1,icm
      DO  k1=k,icm
        IF (b(k1) < b(k)) THEN
          b(k)=b(k1)
          EXIT
        END IF
      END DO
      35      IF (k /= 1.AND.h(k) < h(k-1)) h(k)=h(k-1)
      DO WHILE (1==1)
        40       x1=(b(k)+h(k))/2.0D0
        cv0(k)=x1
        IF (DABS((b(k)-h(k))/x1) < 1.0D-14) EXIT
        j=0
        s=1.0D0
        DO  i=1,nm
          IF (s == 0.0D0) s=s+1.0D-30
          t=f(i)/s
          s=d(i)-t-x1
          IF (s < 0.0D0) j=j+1
        END DO
        IF (j < k) THEN
          h(k)=x1
        ELSE
          b(k)=x1
          IF (j >= icm) THEN
            b(icm)=x1
          ELSE
            IF (h(j+1) < x1) h(j+1)=x1
            IF (x1 < b(j)) b(j)=x1
          END IF
        END IF
      END DO
      50      cv0(k)=x1
      IF (l == 0) eg(2*k-1)=cv0(k)
      IF (l == 1) eg(2*k)=cv0(k)
    END DO
  END DO
END IF
70   cv=eg(n-m+1)
RETURN
END SUBROUTINE segv
