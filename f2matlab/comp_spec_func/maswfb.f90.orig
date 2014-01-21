PROGRAM maswfb
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:09

!     ============================================================
!     Purpose: This program computes the prolate and oblate
!     spheroidal angular functions of the first kind
!     and their derivatives using subroutine ASWFB
!     Input :  m  --- Mode parameter,  m = 0,1,2,...
!     n  --- Mode parameter,  n = m,m+1,...
!     c  --- Spheroidal parameter
!     x  --- Argument of angular function, |x| ó 1.0
!     KD --- Function code
!     KD=1 for prolate;  KD=-1 for oblate
!     cv --- Characteristic value
!     Output:  S1F --- Angular function of the first kind
!     S1D --- Derivative of the angular function of
!     the first kind
!     Examples:
!     KD = 1, m = 2, n = 3, c = 3.0 and cv = 14.8277782138
!     x         Smn(c,x)            Smn'(c,x)
!     --------------------------------------------
!     0.2      .28261309D+01       .12418631D+02
!     0.5      .49938554D+01       .92761604D+00
!     0.8      .31693975D+01      -.12646552D+02

!     KD =-1, m = 2, n = 3, c = 3.0 and cv = 8.8093939208
!     x         Smn(-ic,x)         Smn'(-ic,x)
!     --------------------------------------------
!     0.2      .29417848D+01       .14106305D+02
!     0.5      .64138827D+01       .76007194D+01
!     0.8      .60069873D+01      -.14387479D+02
!     ============================================================

IMPLICIT DOUBLE PRECISION (a-h,o-z)
DIMENSION eg(200)
WRITE(*,*)'Please enter KD, m, n and c '
!     READ(*,*)KD,M,N,C
m=2
n=3
c=3.0
kd=1
WRITE(*,10)kd,m,n,c
CALL segv(m,n,c,kd,cv,eg)
WRITE(*,20)cv
WRITE(*,*)
IF (kd == 1 ) THEN
  WRITE(*,*)'    x         Smn(c,x)            Smn''(C,X)'
ELSE IF (kd == -1) THEN
  WRITE(*,*)'    x         Smn(-ic,x)         Smn''(-IC,X)'
END IF
WRITE(*,*)'  --------------------------------------------'
DO  i=0,20
  x=-1.0D0+0.1D0*i
  IF (i == 0) x=0.0D0
  IF (i == 20) x=1.0D0
  CALL aswfb(m,n,c,x,kd,cv,s1f,s1d)
  WRITE(*,30)x,s1f,s1d
END DO
10   FORMAT(1X,'KD ='i2,', ','m =',i2,', ','n =',i2,', ','c =',f5.1)
20   FORMAT(1X,' cv =',f18.10)
30   FORMAT(1X,f5.1,2D20.8)
END PROGRAM maswfb


SUBROUTINE aswfb(m,n,c,x,kd,cv,s1f,s1d)

!     ===========================================================
!     Purpose: Compute the prolate and oblate spheroidal angular
!     functions of the first kind and their derivatives
!     Input :  m  --- Mode parameter,  m = 0,1,2,...
!     n  --- Mode parameter,  n = m,m+1,...
!     c  --- Spheroidal parameter
!     x  --- Argument of angular function, |x| < 1.0
!     KD --- Function code
!     KD=1 for prolate;  KD=-1 for oblate
!     cv --- Characteristic value
!     Output:  S1F --- Angular function of the first kind
!     S1D --- Derivative of the angular function of
!     the first kind
!     Routines called:
!     (1) SDMN for computing expansion coefficients dk
!     (2) LPMNS for computing associated Legendre function
!     of the first kind Pmn(x)
!     ===========================================================


INTEGER, INTENT(IN)                      :: m
INTEGER, INTENT(IN)                      :: n
DOUBLE PRECISION, INTENT(IN OUT)         :: c
DOUBLE PRECISION, INTENT(IN OUT)         :: x
INTEGER, INTENT(IN OUT)                  :: kd
DOUBLE PRECISION, INTENT(IN OUT)         :: cv
DOUBLE PRECISION, INTENT(OUT)            :: s1f
DOUBLE PRECISION, INTENT(OUT)            :: s1d
IMPLICIT DOUBLE PRECISION (a-h,o-z)
DIMENSION df(200),pm(0:251),pd(0:251)

eps=1.0D-14
ip=1
sw=0.0
IF (n-m == 2*INT((n-m)/2)) ip=0
nm=25+INT((n-m)/2+c)
nm2=2*nm+m
CALL sdmn(m,n,c,cv,kd,df)
CALL lpmns(m,nm2,x,pm,pd)
su1=0.0D0
DO  k=1,nm
  mk=m+2*(k-1)+ip
  su1=su1+df(k)*pm(mk+1)
  IF (DABS(sw-su1) < DABS(su1)*eps) EXIT
  sw=su1
END DO
15   s1f=(-1)**m*su1
su1=0.0D0
DO  k=1,nm
  mk=m+2*(k-1)+ip
  su1=su1+df(k)*pd(mk+1)
  IF (DABS(sw-su1) < DABS(su1)*eps) EXIT
  sw=su1
END DO
25   s1d=(-1)**m*su1
RETURN
END SUBROUTINE aswfb


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


INTEGER, INTENT(IN)                      :: m
INTEGER, INTENT(IN)                      :: n
DOUBLE PRECISION, INTENT(IN)             :: c
DOUBLE PRECISION, INTENT(IN OUT)         :: cv
INTEGER, INTENT(IN)                      :: kd
DOUBLE PRECISION, INTENT(OUT)            :: df(200)
IMPLICIT DOUBLE PRECISION (a-h,o-z)
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


INTEGER, INTENT(IN)                      :: m
INTEGER, INTENT(IN)                      :: n
DOUBLE PRECISION, INTENT(IN)             :: c
INTEGER, INTENT(IN)                      :: kd
DOUBLE PRECISION, INTENT(OUT)            :: cv
DOUBLE PRECISION, INTENT(OUT)            :: eg(200)
IMPLICIT DOUBLE PRECISION (a-h,o-z)
DIMENSION b(100),h(100),d(300),e(300),f(300),cv0(100), a(300),g(300)

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


INTEGER, INTENT(IN)                      :: m
INTEGER, INTENT(IN)                      :: n
DOUBLE PRECISION, INTENT(IN)             :: x
DOUBLE PRECISION, INTENT(OUT)            :: pm(0:n+1+1)
DOUBLE PRECISION, INTENT(OUT)            :: pd(0:n+1)
IMPLICIT DOUBLE PRECISION (a-h,o-z)


DO  k=0,n
  pm(k+1)=0.0D0
  pd(k+1)=0.0D0
END DO
IF (DABS(x) == 1.0D0) THEN
  DO  k=0,n
    IF (m == 0) THEN
      pm(k+1)=1.0D0
      pd(k+1)=0.5D0*k*(k+1.0)
      IF (x < 0.0) THEN
        pm(k+1)=(-1)**k*pm(k+1)
        pd(k+1)=(-1)**(k+1)*pd(k+1)
      END IF
    ELSE IF (m == 1) THEN
      pd(k+1)=1.0D+300
    ELSE IF (m == 2) THEN
      pd(k+1)=-0.25D0*(k+2.0)*(k+1.0)*k*(k-1.0)
      IF (x < 0.0) pd(k+1)=(-1)**(k+1)*pd(k+1)
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
pm(m+1)=pmk
pm(m+1+1)=pm1
DO  k=m+2,n
  pm2=((2.0D0*k-1.0D0)*x*pm1-(k+m-1.0D0)*pmk)/(k-m)
  pm(k+1)=pm2
  pmk=pm1
  pm1=pm2
END DO
DO  k=1,n
  pm(k)=(-1)**m*pm(k)
  pd(k)=(-1)**m*pd(k)
END DO
pd(0+1)=((1.0D0-m)*pm(1+1)-x*pm(0+1))/(x*x-1.0)
DO  k=1,n
  pd(k+1)=(k*x*pm(k+1)-(k+m)*pm(k-1+1))/(x*x-1.0D0)
END DO
RETURN
END SUBROUTINE lpmns
