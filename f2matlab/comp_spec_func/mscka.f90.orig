PROGRAM mscka
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:12

!     ============================================================
!     Purpose: This program computes the expansion coefficients
!     of the prolate and oblate spheroidal functions,
!     c2k, using subroutine SCKA
!     Input :  m  --- Mode parameter
!     n  --- Mode parameter
!     c  --- Spheroidal parameter
!     cv --- Characteristic value
!     KD --- Function code
!     KD=1 for prolate; KD=-1 for oblate
!     Output:  CK(k) --- Expansion coefficients ck;
!     CK(1), CK(2),... correspond to
!     c0, c2,...
!     Example: Compute the first 13 expansion coefficients C2k for
!     KD= 1, m=2, n=3, c=3.0 and cv=14.8277782138; and
!     KD=-1, m=2, n=3, c=3.0 and cv=8.80939392077

!     Coefficients of Prolate and oblate functions

!     k         C2k(c)              C2k(-ic)
!     ---------------------------------------------
!     0     .9173213327D+01     .2489664942D+02
!     1     .4718258929D+01    -.1205287032D+02
!     2     .9841212916D+00     .2410564082D+01
!     3     .1151870224D+00    -.2735821590D+00
!     4     .8733916403D-02     .2026057157D-01
!     5     .4663888254D-03    -.1061946315D-02
!     6     .1853910398D-04     .4158091152D-04
!     7     .5708084895D-06    -.1264400411D-05
!     8     .1402786472D-07     .3074963448D-07
!     9     .2817194508D-09    -.6120579463D-09
!     10     .4712094447D-11     .1015900041D-10
!     11     .6667838485D-13    -.1427953361D-12
!     12     .8087995432D-15     .1721924955D-14
!     ============================================================

IMPLICIT DOUBLE PRECISION (a-h,o-z)
DIMENSION ck(200),eg(200)
WRITE(*,*)'Please KD, m, n and c '
!     READ(*,*)KD,M,N,C
kd=1
m=2
n=3
c=3.0
CALL segv(m,n,c,kd,cv,eg)
WRITE(*,30)kd,m,n,c,cv
CALL scka(m,n,c,cv,kd,ck)
WRITE(*,*)
IF (kd == 1) THEN
  WRITE(*,*)'Coefficients of Prolate function'
  WRITE(*,*)
  WRITE(*,*)'   k            C2k(c)'
ELSE
  WRITE(*,*)'Coefficients of Oblate function'
  WRITE(*,*)
  WRITE(*,*)'   k           C2k(-ic)'
END IF
WRITE(*,*)'----------------------------'
nm=25+INT((n-m)/2+c)
DO  k=1,nm
  WRITE(*,20)k-1,ck(k)
END DO
20   FORMAT(2X,i3,4X,d18.10)
30   FORMAT(1X,3HKD=,i3,',  ',2HM=,i3,',  ',2HN=,i3,',  ',2HC=,  &
    f5.1,',  ',4HCV =,f18.10)
END PROGRAM mscka


SUBROUTINE scka(m,n,c,cv,kd,ck)

!     ======================================================
!     Purpose: Compute the expansion coefficients of the
!     prolate and oblate spheroidal functions, c2k
!     Input :  m  --- Mode parameter
!     n  --- Mode parameter
!     c  --- Spheroidal parameter
!     cv --- Characteristic value
!     KD --- Function code
!     KD=1 for prolate; KD=-1 for oblate
!     Output:  CK(k) --- Expansion coefficients ck;
!     CK(1), CK(2),... correspond to
!     c0, c2,...
!     ======================================================


INTEGER, INTENT(IN)                      :: m
INTEGER, INTENT(IN)                      :: n
DOUBLE PRECISION, INTENT(IN OUT)         :: c
DOUBLE PRECISION, INTENT(IN)             :: cv
INTEGER, INTENT(IN)                      :: kd
DOUBLE PRECISION, INTENT(OUT)            :: ck(200)
IMPLICIT DOUBLE PRECISION (a-h,o-z)


IF (c <= 1.0D-10) c=1.0D-10
nm=25+INT((n-m)/2+c)
cs=c*c*kd
ip=1
IF (n-m == 2*INT((n-m)/2)) ip=0
fs=1.0D0
f1=0.0D0
f0=1.0D-100
kb=0
ck(nm+1)=0.0D0
DO  k=nm,1,-1
  f=(((2.0D0*k+m+ip)*(2.0D0*k+m+1.0D0+ip)-cv+cs)*f0  &
      -4.0D0*(k+1.0D0)*(k+m+1.0D0)*f1)/cs
  IF (DABS(f) > DABS(ck(k+1))) THEN
    ck(k)=f
    f1=f0
    f0=f
    IF (DABS(f) > 1.0D+100) THEN
      DO  k1=nm,k,-1
        ck(k1)=ck(k1)*1.0D-100
      END DO
      f1=f1*1.0D-100
      f0=f0*1.0D-100
    END IF
  ELSE
    kb=k
    fl=ck(k+1)
    f1=1.0D0
    f2=0.25D0*((m+ip)*(m+ip+1.0)-cv+cs)/(m+1.0)*f1
    ck(1)=f1
    IF (kb == 1) THEN
      fs=f2
    ELSE IF (kb == 2) THEN
      ck(2)=f2
      fs=0.125D0*(((m+ip+2.0)*(m+ip+3.0)-cv+cs)*f2 -cs*f1)/(m+2.0)
    ELSE
      ck(2)=f2
      DO  j=3,kb+1
        f=0.25D0*(((2.0*j+m+ip-4.0)*(2.0*j+m+ip-  &
            3.0)-cv+cs)*f2-cs*f1)/((j-1.0)*(j+m-1.0))
        IF (j <= kb) ck(j)=f
        f1=f2
        f2=f
      END DO
      fs=f
    END IF
    EXIT
  END IF
END DO
20   su1=0.0D0
DO  k=1,kb
  su1=su1+ck(k)
END DO
su2=0.0D0
DO  k=kb+1,nm
  su2=su2+ck(k)
END DO
r1=1.0D0
DO  j=1,(n+m+ip)/2
  r1=r1*(j+0.5D0*(n+m+ip))
END DO
r2=1.0D0
DO  j=1,(n-m-ip)/2
  r2=-r2*j
END DO
IF (kb == 0) THEN
  s0=r1/(2.0D0**n*r2*su2)
ELSE
  s0=r1/(2.0D0**n*r2*(fl/fs*su1+su2))
END IF
DO  k=1,kb
  ck(k)=fl/fs*s0*ck(k)
END DO
DO  k=kb+1,nm
  ck(k)=s0*ck(k)
END DO
RETURN
END SUBROUTINE scka


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
