PROGRAM mklvna
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:11

!       =======================================================
!       Purpose: This program computes Kelvin functions ber x,
!                bei x, ker x and kei x, and their derivatives
!                using subroutine KLVNA
!       Input :  x   --- Argument of Kelvin functions
!       Output:  BER --- ber x
!                BEI --- bei x
!                GER --- ker x
!                GEI --- kei x
!                DER --- ber'x
!                DEI --- bei'x
!                HER --- ker'x
!                HEI --- kei'x
!       Example:

!      x       ber x          bei x          ker x          kei x
!    -----------------------------------------------------------------
!      0    .1000000D+01    0              ì            -.7853982D+00
!      5   -.6230082D+01   .1160344D+00  -.1151173D-01   .1118759D-01
!     10    .1388405D+03   .5637046D+02   .1294663D-03  -.3075246D-03
!     15   -.2967255D+04  -.2952708D+04  -.1514347D-07   .7962894D-05
!     20    .4748937D+05   .1147752D+06  -.7715233D-07  -.1858942D-06

!      x       ber'x          bei'x          ker'x          kei'x
!    -----------------------------------------------------------------
!      0     0              0            - ì              0
!      5   -.3845339D+01  -.4354141D+01   .1719340D-01  -.8199865D-03
!     10    .5119526D+02   .1353093D+03  -.3155969D-03   .1409138D-03
!     15    .9105533D+02  -.4087755D+04   .5644678D-05  -.5882223D-05
!     20   -.4880320D+05   .1118550D+06  -.7501859D-07   .1906243D-06
!       =======================================================

IMPLICIT DOUBLE PRECISION (a-h,o-z)
WRITE(*,*)'Please enter x '
!        READ(*,*)X
x=20
CALL klvna(x,ber,bei,ger,gei,der,dei,her,hei)
WRITE(*,*)'   x        ber x           bei x',  &
    '           ker x           kei x'
WRITE(*,*)'--------------------------------',  &
    '--------------------------------------'
WRITE(*,10)x,ber,bei,ger,gei
WRITE(*,*)
WRITE(*,*)'   x        ber''X           BEI''X',  &
    '           ker''X           KEI''X'
WRITE(*,*)'--------------------------------',  &
    '--------------------------------------'
WRITE(*,10)x,der,dei,her,hei
10      FORMAT(1X,f5.1,4D16.8)
END PROGRAM mklvna


SUBROUTINE klvna(x,ber,bei,ger,gei,der,dei,her,hei)

!       ======================================================
!       Purpose: Compute Kelvin functions ber x, bei x, ker x
!                and kei x, and their derivatives  ( x > 0 )
!       Input :  x   --- Argument of Kelvin functions
!       Output:  BER --- ber x
!                BEI --- bei x
!                GER --- ker x
!                GEI --- kei x
!                DER --- ber'x
!                DEI --- bei'x
!                HER --- ker'x
!                HEI --- kei'x
!       ================================================



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
  gei=-0.25D0*pi
  der=0.0D0
  dei=0.0D0
  her=-1.0D+300
  hei=0.0D0
  RETURN
END IF
x2=0.25D0*x*x
x4=x2*x2
IF (DABS(x) < 10.0D0) THEN
  ber=1.0D0
  r=1.0D0
  DO  m=1,60
    r=-0.25D0*r/(m*m)/(2.0D0*m-1.0D0)**2*x4
    ber=ber+r
    IF (DABS(r) < DABS(ber)*eps) EXIT
  END DO
  15         bei=x2
  r=x2
  DO  m=1,60
    r=-0.25D0*r/(m*m)/(2.0D0*m+1.0D0)**2*x4
    bei=bei+r
    IF (DABS(r) < DABS(bei)*eps) EXIT
  END DO
  25         ger=-(DLOG(x/2.0D0)+el)*ber+0.25D0*pi*bei
  r=1.0D0
  gs=0.0D0
  DO  m=1,60
    r=-0.25D0*r/(m*m)/(2.0D0*m-1.0D0)**2*x4
    gs=gs+1.0D0/(2.0D0*m-1.0D0)+1.0D0/(2.0D0*m)
    ger=ger+r*gs
    IF (DABS(r*gs) < DABS(ger)*eps) EXIT
  END DO
  35         gei=x2-(DLOG(x/2.0D0)+el)*bei-0.25D0*pi*ber
  r=x2
  gs=1.0D0
  DO  m=1,60
    r=-0.25D0*r/(m*m)/(2.0D0*m+1.0D0)**2*x4
    gs=gs+1.0D0/(2.0D0*m)+1.0D0/(2.0D0*m+1.0D0)
    gei=gei+r*gs
    IF (DABS(r*gs) < DABS(gei)*eps) EXIT
  END DO
  45         der=-0.25D0*x*x2
  r=der
  DO  m=1,60
    r=-0.25D0*r/m/(m+1.0D0)/(2.0D0*m+1.0D0)**2*x4
    der=der+r
    IF (DABS(r) < DABS(der)*eps) EXIT
  END DO
  55         dei=0.5D0*x
  r=dei
  DO  m=1,60
    r=-0.25D0*r/(m*m)/(2.d0*m-1.d0)/(2.d0*m+1.d0)*x4
    dei=dei+r
    IF (DABS(r) < DABS(dei)*eps) EXIT
  END DO
  65         r=-0.25D0*x*x2
  gs=1.5D0
  her=1.5D0*r-ber/x-(DLOG(x/2.d0)+el)*der+0.25*pi*dei
  DO  m=1,60
    r=-0.25D0*r/m/(m+1.0D0)/(2.0D0*m+1.0D0)**2*x4
    gs=gs+1.0D0/(2*m+1.0D0)+1.0D0/(2*m+2.0D0)
    her=her+r*gs
    IF (DABS(r*gs) < DABS(her)*eps) EXIT
  END DO
  75         r=0.5D0*x
  gs=1.0D0
  hei=0.5D0*x-bei/x-(DLOG(x/2.d0)+el)*dei-0.25*pi*der
  DO  m=1,60
    r=-0.25D0*r/(m*m)/(2*m-1.0D0)/(2*m+1.0D0)*x4
    gs=gs+1.0D0/(2.0D0*m)+1.0D0/(2*m+1.0D0)
    hei=hei+r*gs
    IF (DABS(r*gs) < DABS(hei)*eps) RETURN
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
    xt=0.25D0*k*pi-INT(0.125D0*k)*2.0D0*pi
    cs=COS(xt)
    ss=SIN(xt)
    r0=0.125D0*r0*(2.0D0*k-1.0D0)**2/k/x
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
  cp0=DCOS(xd+0.125D0*pi)
  cn0=DCOS(xd-0.125D0*pi)
  sp0=DSIN(xd+0.125D0*pi)
  sn0=DSIN(xd-0.125D0*pi)
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
    xt=0.25D0*k*pi-INT(0.125D0*k)*2.0D0*pi
    cs=DCOS(xt)
    ss=DSIN(xt)
    r1=0.125D0*r1*(4.d0-(2.0D0*k-1.0D0)**2)/k/x
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
