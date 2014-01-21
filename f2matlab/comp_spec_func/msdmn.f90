PROGRAM msdmn
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:12

!       ===========================================================
!       Purpose: This program computes the expansion coefficients
!                of the prolate and oblate spheroidal functions,
!                dk, using subroutine SDMN
!       Input :  m  --- Mode parameter
!                n  --- Mode parameter
!                c  --- Spheroidal parameter
!                cv --- Characteristic value
!                KD --- Function code
!                       KD=1 for prolate; KD=-1 for oblate
!       Output:  DF(k) --- Expansion coefficients dk;
!                          DF(1), DF(2),... correspond to
!                          d0, d2,... for even n-m and d1,
!                          d3,... for odd n-m
!       Example: Compute the first 12 expansion coefficients for
!                KD= 1, m=2, n=2, c=3.0 and cv=7.1511005241; and
!                KD=-1, m=2, n=2, c=3.0 and cv=4.5264604622

!                Coefficients of Prolate and oblate functions

!                  r          dr(c)             dr(-ic)
!                -------------------------------------------
!                  0     .9237882817D+00    .1115434000D+01
!                  2    -.2901607696D-01    .4888489020D-01
!                  4     .8142246173D-03    .1600845667D-02
!                  6    -.1632270292D-04    .3509183384D-04
!                  8     .2376699010D-06    .5416293446D-06
!                 10    -.2601391701D-08    .6176624069D-08
!                 12     .2209142844D-10    .5407431236D-10
!                 14    -.1494812074D-12    .3745889118D-12
!                 16     .8239302207D-15    .2103624480D-14
!                 18    -.3768260778D-17    .9768323113D-17
!                 20     .1452384658D-19    .3812753620D-19
!                 22    -.4780280430D-22    .1268321726D-21
!       ===========================================================

IMPLICIT DOUBLE PRECISION (a-h,o-z)
DIMENSION df(200),eg(200)
WRITE(*,*)'Please enter KD, m, n and c '
! READ(*,*)KD,M,N,C
kd=1
m=2
n=2
c=3.0
CALL segv(m,n,c,kd,cv,eg)
WRITE(*,30)kd,m,n,c,cv
CALL sdmn(m,n,c,cv,kd,df)
WRITE(*,*)
IF (kd == 1) THEN
  WRITE(*,*)'Coefficients of Prolate function'
  WRITE(*,*)
  WRITE(*,*)'   r             dr(c)'
ELSE
  WRITE(*,*)'Coefficients of Oblate function'
  WRITE(*,*)
  WRITE(*,*)'   r            dr(-ic)'
END IF
WRITE(*,*)'----------------------------'
nm=25+INT(0.5*(n-m)+c)
DO  k=1,nm
  IF (n-m == 2*INT((n-m)/2)) THEN
    j=2*(k-1)
  ELSE
    j=2*k-1
  END IF
  WRITE(*,20)j,df(k)
END DO
20      FORMAT(2X,i3,4X,d18.10)
30      FORMAT(1X,3HKD=,i3,',  ',2HM=,i3,',  ',2HN=,i3,',  ',2HC=,  &
    f5.1,',  ',4HCV =,f18.10)
END PROGRAM msdmn


SUBROUTINE sdmn(m,n,c,cv,kd,df)

!       =====================================================
!       Purpose: Compute the expansion coefficients of the
!                prolate and oblate spheroidal functions, dk
!       Input :  m  --- Mode parameter
!                n  --- Mode parameter
!                c  --- Spheroidal parameter
!                cv --- Characteristic value
!                KD --- Function code
!                       KD=1 for prolate; KD=-1 for oblate
!       Output:  DF(k) --- Expansion coefficients dk;
!                          DF(1), DF(2),... correspond to
!                          d0, d2,... for even n-m and d1,
!                          d3,... for odd n-m
!       =====================================================


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
35      su1=0.0D0
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
55      r3=1.0D0
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

