PROGRAM mfcoef
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:11

!     =========================================================
!     Purpose: This program computes the expansion coefficients
!     for Mathieu and modified Mathieu functions using
!     subroutine FCOEF
!     Input :  m  --- Order of Mathieu functions
!     q  --- Parameter of Mathieu functions
!     KD --- Case code
!     KD=1 for cem(x,q)  ( m = 0,2,4,...)
!     KD=2 for cem(x,q)  ( m = 1,3,5,...)
!     KD=3 for sem(x,q)  ( m = 1,3,5,...)
!     KD=4 for sem(x,q)  ( m = 2,4,6,...)
!     A  --- Characteristic value of Mathieu
!     functions for given m and q
!     Output:  FC(k) --- Expansion coefficients of Mathieu
!     functions ( k= 1,2,...,KM )
!     FC(1),FC(2),FC(3),... correspond to
!     A0,A2,A4,... for KD=1 case, A1,A3,
!     A5,... for KD=2 case, B1,B3,B5,...
!     for KD=3 case and B2,B4,B6,... for
!     KD=4 case
!     Example: Compute the first 12 expansion coefficients of
!     even and odd Mathieu functions for given
!     m = 10 And q= 5.0
!     Expansion coefficients of Mathieu functions

!     n         Amn(q)            Bmn(q)
!     ----------------------------------------
!     0     .16788542D-05
!     2     .33619515D-04     .33444320D-04
!     4     .64298667D-03     .64297621D-03
!     6     .10784807D-01     .10784806D-01
!     8     .13767512D+00     .13767512D+00
!     10     .98395564D+00     .98395564D+00
!     12    -.11280678D+00    -.11280678D+00
!     14     .58929627D-02     .58929627D-02
!     16    -.18916571D-03    -.18916571D-03
!     18     .42264064D-05     .42264064D-05
!     20    -.70485101D-07    -.70485101D-07
!     22     .91820256D-09     .91820256D-09
!     =========================================================

IMPLICIT DOUBLE PRECISION (a-h,o-z)
DIMENSION fc(251),fb(251)
WRITE(*,*)'Please enter m and q '
!     READ(*,*) M,Q
m=10
q=5.0
WRITE(*,30)m,q
WRITE(*,*)
IF (m == 2*INT(m/2)) THEN
  ki=1
  kf=4
  ks=3
ELSE IF (m /= 2*INT(m/2)) THEN
  ki=2
  kf=3
  ks=1
END IF
DO  kd=ki,kf,ks
  CALL cva2(kd,m,q,a)
  CALL fcoef(kd,m,q,a,fc)
  IF (kd == ki) THEN
    DO  l=1,m/2+8
      fb(l)=fc(l)
    END DO
  END IF
END DO
WRITE(*,40)
WRITE(*,*)
WRITE(*,45)
WRITE(*,*)'----------------------------------------'
DO  l=1,m/2+8
  IF (m == 2*INT(m/2)) THEN
    IF (m == 0) WRITE(*,35)2*l-2,fb(l)
    IF (m > 0.AND.l == 1) WRITE(*,35)2*l-2,fb(l)
    IF (m > 0.AND.l /= 1) WRITE(*,35)2*l-2,fb(l),fc(l-1)
  ELSE
    WRITE(*,35)2*l-1,fb(l),fc(l)
  END IF
END DO
30   FORMAT(1X,2HM=,i3,',    ',2HQ=,f6.2)
35   FORMAT(1X,i3,2D18.8)
40   FORMAT(1X,'Expansion coefficients of Mathieu functions')
45   FORMAT(1X,'  n         Amn(q)        ','    Bmn(q)')
END PROGRAM mfcoef


SUBROUTINE fcoef(kd,m,q,a,fc)

!     =====================================================
!     Purpose: Compute expansion coefficients for Mathieu
!     functions and modified Mathieu functions
!     Input :  m  --- Order of Mathieu functions
!     q  --- Parameter of Mathieu functions
!     KD --- Case code
!     KD=1 for cem(x,q)  ( m = 0,2,4,...)
!     KD=2 for cem(x,q)  ( m = 1,3,5,...)
!     KD=3 for sem(x,q)  ( m = 1,3,5,...)
!     KD=4 for sem(x,q)  ( m = 2,4,6,...)
!     A  --- Characteristic value of Mathieu
!     functions for given m and q
!     Output:  FC(k) --- Expansion coefficients of Mathieu
!     functions ( k= 1,2,...,KM )
!     FC(1),FC(2),FC(3),... correspond to
!     A0,A2,A4,... for KD=1 case, A1,A3,
!     A5,... for KD=2 case, B1,B3,B5,...
!     for KD=3 case and B2,B4,B6,... for
!     KD=4 case
!     =====================================================


INTEGER, INTENT(IN)                      :: kd
INTEGER, INTENT(IN)                      :: m
DOUBLE PRECISION, INTENT(IN)             :: q
DOUBLE PRECISION, INTENT(IN)             :: a
DOUBLE PRECISION, INTENT(OUT)            :: fc(251)
IMPLICIT DOUBLE PRECISION (a-h,o-z)


DO  i=1,251
  fc(i)=0.0D0
END DO
IF (q <= 1.0D0) THEN
  qm=7.5+56.1*SQRT(q)-134.7*q+90.7*SQRT(q)*q
ELSE
  qm=17.0+3.1*SQRT(q)-.126*q+.0037*SQRT(q)*q
END IF
km=INT(qm+0.5*m)
DO  k=1,km+1
  fc(k)=0.0D0
END DO
IF (q == 0.0D0) THEN
  IF (kd == 1) THEN
    fc((m+2)/2)=1.0D0
    IF (m == 0) fc(1)=1.0D0/DSQRT(2.0D0)
  ELSE IF (kd == 4) THEN
    fc(m/2)=1.0D0
  ELSE
    fc((m+1)/2)=1.0D0
  END IF
  RETURN
END IF
kb=0
s=0.0D0
f=1.0D-100
u=0.0D0
fc(km)=0.0D0
igoon=1
DO WHILE (igoon == 1)
  IF (kd == 1) THEN
    DO  k=km,3,-1
      v=u
      u=f
      f=(a-4.0D0*k*k)*u/q-v
      IF (DABS(f) < DABS(fc(k+1))) THEN
        kb=k
        fc(1)=1.0D-100
        sp=0.0D0
        f3=fc(k+1)
        fc(2)=a/q*fc(1)
        fc(3)=(a-4.0D0)*fc(2)/q-2.0D0*fc(1)
        u=fc(2)
        f1=fc(3)
        DO  i=3,kb
          v=u
          u=f1
          f1=(a-4.0D0*(i-1.0D0)**2)*u/q-v
          fc(i+1)=f1
          IF (i == kb) THEN
            f2=f1
          END IF
          IF (i /= kb) sp=sp+f1*f1
        END DO
        sp=sp+2.0D0*fc(1)**2+fc(2)**2+fc(3)**2
        ss=s+sp*(f3/f2)**2
        s0=DSQRT(1.0D0/ss)
        DO  j=1,km
          IF (j <= kb+1) THEN
            fc(j)=s0*fc(j)*f3/f2
          ELSE
            fc(j)=s0*fc(j)
          END IF
        END DO
        igoon=0
        EXIT                   !GO TO 85
      ELSE
        fc(k)=f
        s=s+f*f
      END IF
    END DO
    IF (igoon == 0) EXIT
    fc(2)=q*fc(3)/(a-4.0D0-2.0D0*q*q/a)
    fc(1)=q/a*fc(2)
    s=s+2.0D0*fc(1)**2+fc(2)**2
    s0=DSQRT(1.0D0/s)
    DO  k=1,km
      fc(k)=s0*fc(k)
    END DO
  ELSE IF (kd == 2.OR.kd == 3) THEN
    igoon2=1
    DO  k=km,3,-1
      v=u
      u=f
      f=(a-(2.0D0*k-1)**2)*u/q-v
      IF (DABS(f) >= DABS(fc(k))) THEN
        fc(k-1)=f
        s=s+f*f
      ELSE
        kb=k
        f3=fc(k)
        igoon2=0
        EXIT
      END IF
    END DO
    IF (igoon2 == 1) THEN
      fc(1)=q/(a-1.0D0-(-1)**kd*q)*fc(2)
      s=s+fc(1)*fc(1)
      s0=DSQRT(1.0D0/s)
      DO  k=1,km
        fc(k)=s0*fc(k)
      END DO
      EXIT                    !GO TO 85
    END IF
    45    fc(1)=1.0D-100
    fc(2)=(a-1.0D0-(-1)**kd*q)/q*fc(1)
    sp=0.0D0
    u=fc(1)
    f1=fc(2)
    DO  i=2,kb-1
      v=u
      u=f1
      f1=(a-(2.0D0*i-1.0D0)**2)*u/q-v
      IF (i /= kb-1) THEN
        fc(i+1)=f1
        sp=sp+f1*f1
      ELSE
        f2=f1
      END IF
    END DO
    sp=sp+fc(1)**2+fc(2)**2
    ss=s+sp*(f3/f2)**2
    s0=1.0D0/DSQRT(ss)
    DO  j=1,km
      IF (j < kb) THEN
        fc(j)=s0*fc(j)*f3/f2
      END IF
      IF (j >= kb) THEN
        fc(j)=s0*fc(j)
      END IF
    END DO
  ELSE IF (kd == 4) THEN
    igoon2=1
    DO  k=km,3,-1
      v=u
      u=f
      f=(a-4.0D0*k*k)*u/q-v
      IF (DABS(f) >= DABS(fc(k))) THEN
        fc(k-1)=f
        s=s+f*f
      ELSE
        kb=k
        f3=fc(k)
        igoon2=0
        EXIT
      END IF
    END DO
    IF (igoon2 == 1) THEN
      fc(1)=q/(a-4.0D0)*fc(2)
      s=s+fc(1)*fc(1)
      s0=DSQRT(1.0D0/s)
      DO  k=1,km
        fc(k)=s0*fc(k)
      END DO
      EXIT                    !GO TO 85
    END IF
    70    fc(1)=1.0D-100
    fc(2)=(a-4.0D0)/q*fc(1)
    sp=0.0D0
    u=fc(1)
    f1=fc(2)
    DO  i=2,kb-1
      v=u
      u=f1
      f1=(a-4.0D0*i*i)*u/q-v
      IF (i /= kb-1) THEN
        fc(i+1)=f1
        sp=sp+f1*f1
      ELSE
        f2=f1
      END IF
    END DO
    sp=sp+fc(1)**2+fc(2)**2
    ss=s+sp*(f3/f2)**2
    s0=1.0D0/DSQRT(ss)
    DO  j=1,km
      IF (j < kb) THEN
        fc(j)=s0*fc(j)*f3/f2
      END IF
      IF (j >= kb) THEN
        fc(j)=s0*fc(j)
      END IF
    END DO
  END IF
  igoon=0
END DO
85   IF (fc(1) < 0.0D0) THEN
  DO  j=1,km
    fc(j)=-fc(j)
  END DO
END IF
RETURN
END SUBROUTINE fcoef


SUBROUTINE cva2(kd,m,q,a)

!     ======================================================
!     Purpose: Calculate a specific characteristic value of
!     Mathieu functions
!     Input :  m  --- Order of Mathieu functions
!     q  --- Parameter of Mathieu functions
!     KD --- Case code
!     KD=1 for cem(x,q)  ( m = 0,2,4,...)
!     KD=2 for cem(x,q)  ( m = 1,3,5,...)
!     KD=3 for sem(x,q)  ( m = 1,3,5,...)
!     KD=4 for sem(x,q)  ( m = 2,4,6,...)
!     Output:  A  --- Characteristic value
!     Routines called:
!     (1) REFINE for finding accurate characteristic
!     value using an iteration method
!     (2) CV0 for finding initial characteristic
!     values using polynomial approximation
!     (3) CVQM for computing initial characteristic
!     values for q ó 3*m
!     (3) CVQL for computing initial characteristic
!     values for q ò m*m
!     ======================================================



INTEGER, INTENT(IN OUT)                  :: kd
INTEGER, INTENT(IN)                      :: m
DOUBLE PRECISION, INTENT(IN)             :: q
DOUBLE PRECISION, INTENT(OUT)            :: a
IMPLICIT DOUBLE PRECISION (a-h,o-z)
IF (m <= 12.OR.q <= 3.0*m.OR.q > m*m) THEN
  CALL cv0(kd,m,q,a)
  IF (q /= 0.0D0) THEN
    CALL refine(kd,m,q,a,1)
  END IF
ELSE
  ndiv=10
  delta=(m-3.0)*m/ndiv
  IF ((q-3.0*m) <= (m*m-q)) THEN
    DO
      nn=INT((q-3.0*m)/delta)+1
      delta=(q-3.0*m)/nn
      q1=2.0*m
      CALL cvqm(m,q1,a1)
      q2=3.0*m
      CALL cvqm(m,q2,a2)
      qq=3.0*m
      DO  i=1,nn
        qq=qq+delta
        a=(a1*q2-a2*q1+(a2-a1)*qq)/(q2-q1)
        iflag=1
        IF (i == nn) iflag=-1
        CALL refine(kd,m,qq,a,iflag)
        q1=q2
        q2=qq
        a1=a2
        a2=a
      END DO
      IF (iflag == -10) THEN
        ndiv=ndiv*2
        delta=(m-3.0)*m/ndiv
        EXIT
      END IF
    END DO
  ELSE
    DO
      nn=INT((m*m-q)/delta)+1
      delta=(m*m-q)/nn
      q1=m*(m-1.0)
      CALL cvql(kd,m,q1,a1)
      q2=m*m
      CALL cvql(kd,m,q2,a2)
      qq=m*m
      DO  i=1,nn
        qq=qq-delta
        a=(a1*q2-a2*q1+(a2-a1)*qq)/(q2-q1)
        iflag=1
        IF (i == nn) iflag=-1
        CALL refine(kd,m,qq,a,iflag)
        q1=q2
        q2=qq
        a1=a2
        a2=a
      END DO
      IF (iflag == -10) THEN
        ndiv=ndiv*2
        delta=(m-3.0)*m/ndiv
        EXIT
      END IF
    END DO
  END IF
END IF
RETURN
END SUBROUTINE cva2


SUBROUTINE refine(kd,m,q,a,iflag)

!     =====================================================
!     Purpose: calculate the accurate characteristic value
!     by the secant method
!     Input :  m --- Order of Mathieu functions
!     q --- Parameter of Mathieu functions
!     A --- Initial characteristic value
!     Output:  A --- Refineed characteristic value
!     Routine called:  CVF for computing the value of F for
!     characteristic equation
!     ========================================================



INTEGER, INTENT(IN OUT)                  :: kd
INTEGER, INTENT(IN)                      :: m
DOUBLE PRECISION, INTENT(IN OUT)         :: q
DOUBLE PRECISION, INTENT(IN OUT)         :: a
INTEGER, INTENT(IN OUT)                  :: iflag
IMPLICIT DOUBLE PRECISION (a-h,o-z)
eps=1.0D-14
mj=10+m
x0=a
CALL cvf(kd,m,q,x0,mj,f0)
x1=1.002*a
CALL cvf(kd,m,q,x1,mj,f1)
DO  it=1,100
  mj=mj+1
  x=x1-(x1-x0)/(1.0D0-f0/f1)
  CALL cvf(kd,m,q,x,mj,f)
  IF (ABS(1.0-x1/x) < eps.OR.f == 0.0) EXIT
  x0=x1
  f0=f1
  x1=x
  f1=f
END DO
a=x
RETURN
END SUBROUTINE refine


SUBROUTINE cvf(kd,m,q,a,mj,f)

!     ======================================================
!     Purpose: Compute the value of F for characteristic
!     equation of Mathieu functions
!     Input :  m --- Order of Mathieu functions
!     q --- Parameter of Mathieu functions
!     A --- Characteristic value
!     Output:  F --- Value of F for characteristic equation
!     ======================================================



INTEGER, INTENT(IN)                      :: kd
INTEGER, INTENT(IN)                      :: m
DOUBLE PRECISION, INTENT(IN)             :: q
DOUBLE PRECISION, INTENT(IN)             :: a
INTEGER, INTENT(IN)                      :: mj
DOUBLE PRECISION, INTENT(OUT)            :: f
IMPLICIT DOUBLE PRECISION (a-h,o-z)
b=a
ic=INT(m/2)
l=0
l0=0
j0=2
jf=ic
IF (kd == 1) l0=2
IF (kd == 1) j0=3
IF (kd == 2.OR.kd == 3) l=1
IF (kd == 4) jf=ic-1
t1=0.0D0
DO  j=mj,ic+1,-1
  t1=-q*q/((2.0D0*j+l)**2-b+t1)
END DO
IF (m <= 2) THEN
  t2=0.0D0
  IF (kd == 1.AND.m == 0) THEN
    t1=t1+t1
  END IF
  IF (kd == 1.AND.m == 2) THEN
    t1=-2.0*q*q/(4.0-b+t1)-4.0
  END IF
  IF (kd == 2.AND.m == 1) t1=t1+q
  IF (kd == 3.AND.m == 1) t1=t1-q
ELSE
  IF (kd == 1) t0=4.0D0-b+2.0D0*q*q/b
  IF (kd == 2) t0=1.0D0-b+q
  IF (kd == 3) t0=1.0D0-b-q
  IF (kd == 4) t0=4.0D0-b
  t2=-q*q/t0
  DO  j=j0,jf
    t2=-q*q/((2.0D0*j-l-l0)**2-b+t2)
  END DO
END IF
f=(2.0D0*ic+l)**2+t1+t2-b
RETURN
END SUBROUTINE cvf


SUBROUTINE cv0(kd,m,q,a0)

!     =====================================================
!     Purpose: Compute the initial characteristic value of
!     Mathieu functions for m ó 12  or q ó 300 or
!     q ò m*m
!     Input :  m  --- Order of Mathieu functions
!     q  --- Parameter of Mathieu functions
!     Output:  A0 --- Characteristic value
!     Routines called:
!     (1) CVQM for computing initial characteristic
!     value for q ó 3*m
!     (2) CVQL for computing initial characteristic
!     value for q ò m*m
!     ====================================================



INTEGER, INTENT(IN OUT)                  :: kd
INTEGER, INTENT(IN)                      :: m
DOUBLE PRECISION, INTENT(IN)             :: q
DOUBLE PRECISION, INTENT(OUT)            :: a0
IMPLICIT DOUBLE PRECISION (a-h,o-z)
q2=q*q
IF (m == 0) THEN
  IF (q <= 1.0) THEN
    a0=(((.0036392*q2-.0125868)*q2+.0546875)*q2-.5)*q2
  ELSE IF (q <= 10.0) THEN
    a0=((3.999267D-3*q-9.638957D-2)*q-.88297)*q +.5542818
  ELSE
    CALL cvql(kd,m,q,a0)
  END IF
ELSE IF (m == 1) THEN
  IF (q <= 1.0.AND.kd == 2) THEN
    a0=(((-6.51E-4*q-.015625)*q-.125)*q+1.0)*q+1.0
  ELSE IF (q <= 1.0.AND.kd == 3) THEN
    a0=(((-6.51E-4*q+.015625)*q-.125)*q-1.0)*q+1.0
  ELSE IF (q <= 10.0.AND. kd == 2) THEN
    a0=(((-4.94603D-4*q+1.92917D-2)*q-.3089229) *q+1.33372)*q+.811752
  ELSE IF (q <= 10.0.AND.kd == 3) THEN
    a0=((1.971096D-3*q-5.482465D-2)*q-1.152218) *q+1.10427
  ELSE
    CALL cvql(kd,m,q,a0)
  END IF
ELSE IF (m == 2) THEN
  IF (q <= 1.0.AND.kd == 1) THEN
    a0=(((-.0036391*q2+.0125888)*q2-.0551939)*q2 +.416667)*q2+4.0
  ELSE IF (q <= 1.0.AND.kd == 4) THEN
    a0=(.0003617*q2-.0833333)*q2+4.0
  ELSE IF (q <= 15.AND.kd == 1) THEN
    a0=(((3.200972D-4*q-8.667445D-3)*q -1.829032D-4)*q+.9919999)*q+3.3290504
  ELSE IF (q <= 10.0.AND.kd == 4) THEN
    a0=((2.38446D-3*q-.08725329)*q-4.732542D-3) *q+4.00909
  ELSE
    CALL cvql(kd,m,q,a0)
  END IF
ELSE IF (m == 3) THEN
  IF (q <= 1.0.AND.kd == 2) THEN
    a0=((6.348E-4*q+.015625)*q+.0625)*q2+9.0
  ELSE IF (q <= 1.0.AND.kd == 3) THEN
    a0=((6.348E-4*q-.015625)*q+.0625)*q2+9.0
  ELSE IF (q <= 20.0.AND.kd == 2) THEN
    a0=(((3.035731D-4*q-1.453021D-2)*q +.19069602)*q-.1039356)*q+8.9449274
  ELSE IF (q <= 15.0.AND.kd == 3) THEN
    a0=((9.369364D-5*q-.03569325)*q+.2689874)*q +8.771735
  ELSE
    CALL cvql(kd,m,q,a0)
  END IF
ELSE IF (m == 4) THEN
  IF (q <= 1.0.AND.kd == 1) THEN
    a0=((-2.1E-6*q2+5.012E-4)*q2+.0333333)*q2+16.0
  ELSE IF (q <= 1.0.AND.kd == 4) THEN
    a0=((3.7E-6*q2-3.669E-4)*q2+.0333333)*q2+16.0
  ELSE IF (q <= 25.0.AND.kd == 1) THEN
    a0=(((1.076676D-4*q-7.9684875D-3)*q +.17344854)*q-.5924058)*q+16.620847
  ELSE IF (q <= 20.0.AND.kd == 4) THEN
    a0=((-7.08719D-4*q+3.8216144D-3)*q +.1907493)*q+15.744
  ELSE
    CALL cvql(kd,m,q,a0)
  END IF
ELSE IF (m == 5) THEN
  IF (q <= 1.0.AND.kd == 2) THEN
    a0=((6.8E-6*q+1.42E-5)*q2+.0208333)*q2+25.0
  ELSE IF (q <= 1.0.AND.kd == 3) THEN
    a0=((-6.8E-6*q+1.42E-5)*q2+.0208333)*q2+25.0
  ELSE IF (q <= 35.0.AND.kd == 2) THEN
    a0=(((2.238231D-5*q-2.983416D-3)*q +.10706975)*q-.600205)*q+25.93515
  ELSE IF (q <= 25.0.AND.kd == 3) THEN
    a0=((-7.425364D-4*q+2.18225D-2)*q +4.16399D-2)*q+24.897
  ELSE
    CALL cvql(kd,m,q,a0)
  END IF
ELSE IF (m == 6) THEN
  IF (q <= 1.0) THEN
    a0=(.4D-6*q2+.0142857)*q2+36.0
  ELSE IF (q <= 40.0.AND.kd == 1) THEN
    a0=(((-1.66846D-5*q+4.80263D-4)*q +2.53998D-2)*q-.181233)*q+36.423
  ELSE IF (q <= 35.0.AND.kd == 4) THEN
    a0=((-4.57146D-4*q+2.16609D-2)*q-2.349616D-2)*q +35.99251
  ELSE
    CALL cvql(kd,m,q,a0)
  END IF
ELSE IF (m == 7) THEN
  IF (q <= 10.0) THEN
    CALL cvqm(m,q,a0)
  ELSE IF (q <= 50.0.AND.kd == 2) THEN
    a0=(((-1.411114D-5*q+9.730514D-4)*q -3.097887D-3)*q+3.533597D-2)*q+49.0547
  ELSE IF (q <= 40.0.AND.kd == 3) THEN
    a0=((-3.043872D-4*q+2.05511D-2)*q -9.16292D-2)*q+49.19035
  ELSE
    CALL cvql(kd,m,q,a0)
  END IF
ELSE IF (m >= 8) THEN
  IF (q <= 3.*m) THEN
    CALL cvqm(m,q,a0)
  ELSE IF (q > m*m) THEN
    CALL cvql(kd,m,q,a0)
  ELSE
    IF (m == 8.AND.kd == 1) THEN
      a0=(((8.634308D-6*q-2.100289D-3)*q+.169072)*q -4.64336)*q+109.4211
    ELSE IF (m == 8.AND.kd == 4) THEN
      a0=((-6.7842D-5*q+2.2057D-3)*q+.48296)*q+56.59
    ELSE IF (m == 9.AND.kd == 2) THEN
      a0=(((2.906435D-6*q-1.019893D-3)*q+.1101965)*q -3.821851)*q+127.6098
    ELSE IF (m == 9.AND.kd == 3) THEN
      a0=((-9.577289D-5*q+.01043839)*q+.06588934)*q +78.0198
    ELSE IF (m == 10.AND.kd == 1) THEN
      a0=(((5.44927D-7*q-3.926119D-4)*q+.0612099)*q -2.600805)*q+138.1923
    ELSE IF (m == 10.AND.kd == 4) THEN
      a0=((-7.660143D-5*q+.01132506)*q-.09746023)*q +99.29494
    ELSE IF (m == 11.AND.kd == 2) THEN
      a0=(((-5.67615D-7*q+7.152722D-6)*q+.01920291)*q -1.081583)*q+140.88
    ELSE IF (m == 11.AND.kd == 3) THEN
      a0=((-6.310551D-5*q+.0119247)*q-.2681195)*q +123.667
    ELSE IF (m == 12.AND.kd == 1) THEN
      a0=(((-2.38351D-7*q-2.90139D-5)*q+.02023088)*q -1.289)*q+171.2723
    ELSE IF (m == 12.AND.kd == 4) THEN
      a0=(((3.08902D-7*q-1.577869D-4)*q+.0247911)*q -1.05454)*q+161.471
    END IF
  END IF
END IF
RETURN
END SUBROUTINE cv0


SUBROUTINE cvql(kd,m,q,a0)

!     ========================================================
!     Purpose: Compute the characteristic value of Mathieu
!     functions  for q ò 3m
!     Input :  m  --- Order of Mathieu functions
!     q  --- Parameter of Mathieu functions
!     Output:  A0 --- Initial characteristic value
!     ========================================================



INTEGER, INTENT(IN)                      :: kd
INTEGER, INTENT(IN)                      :: m
DOUBLE PRECISION, INTENT(IN)             :: q
DOUBLE PRECISION, INTENT(OUT)            :: a0
IMPLICIT DOUBLE PRECISION (a-h,o-z)
IF (kd == 1.OR.kd == 2) w=2.0D0*m+1.0D0
IF (kd == 3.OR.kd == 4) w=2.0D0*m-1.0D0
w2=w*w
w3=w*w2
w4=w2*w2
w6=w2*w4
d1=5.0+34.0/w2+9.0/w4
d2=(33.0+410.0/w2+405.0/w4)/w
d3=(63.0+1260.0/w2+2943.0/w4+486.0/w6)/w2
d4=(527.0+15617.0/w2+69001.0/w4+41607.0/w6)/w3
c1=128.0
p2=q/w4
p1=DSQRT(p2)
cv1=-2.0*q+2.0*w*DSQRT(q)-(w2+1.0)/8.0
cv2=(w+3.0/w)+d1/(32.0*p1)+d2/(8.0*c1*p2)
cv2=cv2+d3/(64.0*c1*p1*p2)+d4/(16.0*c1*c1*p2*p2)
a0=cv1-cv2/(c1*p1)
RETURN
END SUBROUTINE cvql


SUBROUTINE cvqm(m,q,a0)

!     =====================================================
!     Purpose: Compute the characteristic value of Mathieu
!     functions for q ó m*m
!     Input :  m  --- Order of Mathieu functions
!     q  --- Parameter of Mathieu functions
!     Output:  A0 --- Initial characteristic value
!     =====================================================



INTEGER, INTENT(IN)                      :: m
DOUBLE PRECISION, INTENT(IN)             :: q
DOUBLE PRECISION, INTENT(OUT)            :: a0
IMPLICIT DOUBLE PRECISION (a-h,o-z)
hm1=.5*q/(m*m-1.0)
hm3=.25*hm1**3/(m*m-4.0)
hm5=hm1*hm3*q/((m*m-1.0)*(m*m-9.0))
a0=m*m+q*(hm1+(5.0*m*m+7.0)*hm3+(9.0*m**4+58.0*m*m+29.0)*hm5)
RETURN
END SUBROUTINE cvqm
