PROGRAM mklvnzo
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:11

!     ==============================================================
!     Purpose: This program computes the first NT zeros of Kelvin
!     functions and their derivatives using subroutine
!     KLVNZO
!     Input :  NT --- Total number of zeros
!     Example: NT = 5

!     Zeros of Kelvin functions ber x, bei x, ker x and kei x

!     m       ber x          bei x          ker x          kei x
!     ---------------------------------------------------------------
!     1     2.84891782     5.02622395     1.71854296     3.91466761
!     2     7.23882945     9.45540630     6.12727913     8.34422506
!     3    11.67396355    13.89348785    10.56294271    12.78255715
!     4    16.11356383    18.33398346    15.00268812    17.22314372
!     5    20.55463158    22.77543929    19.44381663    21.66464214

!     Zeros of Kelvin Functions ber'x, bei'x, ker'x and kei'x

!     m       ber'x          bei'x          ker'x          kei'x
!     ---------------------------------------------------------------
!     1     6.03871081     3.77267330     2.66583979     4.93181194
!     2    10.51364251     8.28098785     7.17212212     9.40405458
!     3    14.96844542    12.74214752    11.63218639    13.85826916
!     4    19.41757493    17.19343175    16.08312025    18.30717294
!     5    23.86430432    21.64114394    20.53067845    22.75379258
!     ==============================================================

IMPLICIT DOUBLE PRECISION (a-h,o-z)
DIMENSION r1(50),r2(50),r3(50),r4(50),r5(50),r6(50), r7(50),r8(50)
WRITE(*,35)
WRITE(*,*)'Please enter NT '
!     READ(*,*)NT
nt=5
WRITE(*,25)
WRITE(*,*)
WRITE(*,*)'   m       ber x          bei x          ker x', '          kei x'
WRITE(*,*)' ------------------------------------------------',  &
    '---------------'
CALL klvnzo(nt,1,r1)
CALL klvnzo(nt,2,r2)
CALL klvnzo(nt,3,r3)
CALL klvnzo(nt,4,r4)
DO  l=1,nt
  WRITE(*,20)l,r1(l),r2(l),r3(l),r4(l)
END DO
CALL klvnzo(nt,5,r5)
CALL klvnzo(nt,6,r6)
CALL klvnzo(nt,7,r7)
CALL klvnzo(nt,8,r8)
WRITE(*,*)
WRITE(*,30)
WRITE(*,*)
WRITE(*,*)'   m       ber''X          BEI''X          KER''X',  &
    '          kei''X'
WRITE(*,*)' ------------------------------------------------',  &
    '---------------'
DO  l=1,nt
  WRITE(*,20)l,r5(l),r6(l),r7(l),r8(l)
END DO

20   FORMAT(1X,i3,1X,f14.8,1X,f14.8,1X,f14.8,1X,f14.8)
25   FORMAT(4X,'Zeros of Kelvin functions ber x, bei x,' ,' ker x and kei x')
30   FORMAT(4X,'Zeros of Kelvin functions ber''X, BEI''X,'  &
    ,' ker''X AND KEI''X')
35   FORMAT(1X,'NT is the number of the zeros')
END PROGRAM mklvnzo


SUBROUTINE klvnzo(nt,kd,zo)

!     ====================================================
!     Purpose: Compute the zeros of Kelvin functions
!     Input :  NT  --- Total number of zeros
!     KD  --- Function code
!     KD=1 to 8 for ber x, bei x, ker x, kei x,
!     ber'x, bei'x, ker'x and kei'x,
!     respectively.
!     Output:  ZO(M) --- the M-th zero of Kelvin function
!     for code KD
!     Routine called:
!     KLVNA for computing Kelvin functions and
!     their derivatives
!     ====================================================


INTEGER, INTENT(IN)                      :: nt
INTEGER, INTENT(IN)                      :: kd
DOUBLE PRECISION, INTENT(OUT)            :: zo(nt)
IMPLICIT DOUBLE PRECISION (a-h,o-z)
DIMENSION  rt0(8)

rt0(1)=2.84891
rt0(2)=5.02622
rt0(3)=1.71854
rt0(4)=3.91467
rt0(5)=6.03871
rt0(6)=3.77268
rt0(7)=2.66584
rt0(8)=4.93181
rt=rt0(kd)
DO  m=1,nt
  DO
    10     CALL klvna(rt,ber,bei,ger,gei,der,dei,her,hei)
    IF (kd == 1) THEN
      rt=rt-ber/der
    ELSE IF (kd == 2) THEN
      rt=rt-bei/dei
    ELSE IF (kd == 3) THEN
      rt=rt-ger/her
    ELSE IF (kd == 4) THEN
      rt=rt-gei/hei
    ELSE IF (kd == 5) THEN
      ddr=-bei-der/rt
      rt=rt-der/ddr
    ELSE IF (kd == 6) THEN
      ddi=ber-dei/rt
      rt=rt-dei/ddi
    ELSE IF (kd == 7) THEN
      gdr=-gei-her/rt
      rt=rt-her/gdr
    ELSE
      gdi=ger-hei/rt
      rt=rt-hei/gdi
    END IF
    IF (DABS(rt-rt0(kd)) > 5.0D-10) THEN
      rt0(kd)=rt
    ELSE
      EXIT
    END IF
  END DO
  zo(m)=rt
  rt=rt+4.44D0
END DO
RETURN
END SUBROUTINE klvnzo


SUBROUTINE klvna(x,ber,bei,ger,gei,der,dei,her,hei)

!     ======================================================
!     Purpose: Compute Kelvin functions ber x, bei x, ker x
!     and kei x, and their derivatives  ( x > 0 )
!     Input :  x   --- Argument of Kelvin functions
!     Output:  BER --- ber x
!     BEI --- bei x
!     GER --- ker x
!     GEI --- kei x
!     DER --- ber'x
!     DEI --- bei'x
!     HER --- ker'x
!     HEI --- kei'x
!     ================================================



DOUBLE PRECISION, INTENT(IN)             :: x
DOUBLE PRECISION, INTENT(OUT)            :: ber
DOUBLE PRECISION, INTENT(OUT)            :: bei
DOUBLE PRECISION, INTENT(OUT)            :: ger
DOUBLE PRECISION, INTENT(OUT)            :: gei
DOUBLE PRECISION, INTENT(OUT)            :: der
DOUBLE PRECISION, INTENT(OUT)            :: dei
DOUBLE PRECISION, INTENT(OUT)            :: her
DOUBLE PRECISION, INTENT(OUT)            :: hei
IMPLICIT DOUBLE PRECISION (a-h,o-z)
pi=3.141592653589793D0
el=.5772156649015329D0
eps=1.0D-15
IF (x == 0.0D0) THEN
  ber=1.0D0
  bei=0.0D0
  ger=1.0D+300
  gei=-.25D0*pi
  der=0.0D0
  dei=0.0D0
  her=-1.0D+300
  hei=0.0D0
  RETURN
END IF
x2=.25D0*x*x
x4=x2*x2
IF (DABS(x) < 10.0D0) THEN
  ber=1.0D0
  r=1.0D0
  DO  m=1,60
    r=-.25D0*r/(m*m)/(2.0D0*m-1.0D0)**2*x4
    ber=ber+r
    IF (DABS(r/ber) < eps) EXIT
  END DO
  15    bei=x2
  r=x2
  DO  m=1,60
    r=-.25D0*r/(m*m)/(2.0D0*m+1.0D0)**2*x4
    bei=bei+r
    IF (DABS(r/bei) < eps) EXIT
  END DO
  25    ger=-(DLOG(x/2.0D0)+el)*ber+.25D0*pi*bei
  r=1.0D0
  gs=0.0D0
  DO  m=1,60
    r=-.25D0*r/(m*m)/(2.0D0*m-1.0D0)**2*x4
    gs=gs+1.0D0/(2.0D0*m-1.0D0)+1.0D0/(2.0D0*m)
    ger=ger+r*gs
    IF (DABS(r*gs/ger) < eps) EXIT
  END DO
  35    gei=x2-(DLOG(x/2.0D0)+el)*bei-.25D0*pi*ber
  r=x2
  gs=1.0D0
  DO  m=1,60
    r=-.25D0*r/(m*m)/(2.0D0*m+1.0D0)**2*x4
    gs=gs+1.0D0/(2.0D0*m)+1.0D0/(2.0D0*m+1.0D0)
    gei=gei+r*gs
    IF (DABS(r*gs/gei) < eps) EXIT
  END DO
  45    der=-.25D0*x*x2
  r=der
  DO  m=1,60
    r=-.25D0*r/m/(m+1.0D0)/(2.0D0*m+1.0D0)**2*x4
    der=der+r
    IF (DABS(r/der) < eps) EXIT
  END DO
  55    dei=.5D0*x
  r=dei
  DO  m=1,60
    r=-.25D0*r/(m*m)/(2.d0*m-1.d0)/(2.d0*m+1.d0)*x4
    dei=dei+r
    IF (DABS(r/dei) < eps) EXIT
  END DO
  65    r=-.25D0*x*x2
  gs=1.5D0
  her=1.5D0*r-ber/x-(DLOG(x/2.d0)+el)*der+.25*pi*dei
  DO  m=1,60
    r=-.25D0*r/m/(m+1.0D0)/(2.0D0*m+1.0D0)**2*x4
    gs=gs+1.0D0/(2*m+1.0D0)+1.0D0/(2*m+2.0D0)
    her=her+r*gs
    IF (DABS(r*gs/her) < eps) EXIT
  END DO
  75    r=.5D0*x
  gs=1.0D0
  hei=.5D0*x-bei/x-(DLOG(x/2.d0)+el)*dei-.25*pi*der
  DO  m=1,60
    r=-.25D0*r/(m*m)/(2*m-1.0D0)/(2*m+1.0D0)*x4
    gs=gs+1.0D0/(2.0D0*m)+1.0D0/(2*m+1.0D0)
    hei=hei+r*gs
    IF (DABS(r*gs/hei) < eps) RETURN
  END DO
ELSE
  pp0=1.0D0
  pn0=1.0D0
  qp0=0.0D0
  qn0=0.0D0
  r0=1.0D0
  km=18
  IF (DABS(x) >= 40.0) km=10
  fac=1.0D0
  DO  k=1,km
    fac=-fac
    xt=.25D0*k*pi-INT(.125D0*k)*2.0D0*pi
    cs=COS(xt)
    ss=SIN(xt)
    r0=.125D0*r0*(2.0D0*k-1.0D0)**2/k/x
    rc=r0*cs
    rs=r0*ss
    pp0=pp0+rc
    pn0=pn0+fac*rc
    qp0=qp0+rs
    qn0=qn0+fac*rs
  END DO
  xd=x/DSQRT(2.0D0)
  xe1=DEXP(xd)
  xe2=DEXP(-xd)
  xc1=1.d0/DSQRT(2.0D0*pi*x)
  xc2=DSQRT(.5D0*pi/x)
  cp0=DCOS(xd+.125D0*pi)
  cn0=DCOS(xd-.125D0*pi)
  sp0=DSIN(xd+.125D0*pi)
  sn0=DSIN(xd-.125D0*pi)
  ger=xc2*xe2*(pn0*cp0-qn0*sp0)
  gei=xc2*xe2*(-pn0*sp0-qn0*cp0)
  ber=xc1*xe1*(pp0*cn0+qp0*sn0)-gei/pi
  bei=xc1*xe1*(pp0*sn0-qp0*cn0)+ger/pi
  pp1=1.0D0
  pn1=1.0D0
  qp1=0.0D0
  qn1=0.0D0
  r1=1.0D0
  fac=1.0D0
  DO  k=1,km
    fac=-fac
    xt=.25D0*k*pi-INT(.125D0*k)*2.0D0*pi
    cs=DCOS(xt)
    ss=DSIN(xt)
    r1=.125D0*r1*(4.d0-(2.0D0*k-1.0D0)**2)/k/x
    rc=r1*cs
    rs=r1*ss
    pp1=pp1+fac*rc
    pn1=pn1+rc
    qp1=qp1+fac*rs
    qn1=qn1+rs
  END DO
  her=xc2*xe2*(-pn1*cn0+qn1*sn0)
  hei=xc2*xe2*(pn1*sn0+qn1*cn0)
  der=xc1*xe1*(pp1*cp0+qp1*sp0)-hei/pi
  dei=xc1*xe1*(pp1*sp0-qp1*cp0)+her/pi
END IF
RETURN
END SUBROUTINE klvna
