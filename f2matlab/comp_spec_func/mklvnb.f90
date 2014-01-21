PROGRAM mklvnb
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:11

!       ========================================================
!       Purpose: This program computes Kelvin functions ber x,
!                bei x, ker x and kei x, and their derivatives
!                using subroutine KLVNB
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
!     x       ber x         bei x         ker x         kei x
!   -------------------------------------------------------------
!     0    .100000D+01   .000000D+00    ì           -.785398D+00
!     5   -.623008D+01   .116034D+00  -.115117D-01   .111876D-01
!    10    .138840D+03   .563705D+02   .129466D-03  -.307525D-03
!    15   -.296725D+04  -.295271D+04  -.151433D-07   .796289D-05
!    20    .474894D+05   .114775D+06  -.771523D-07  -.185894D-06

!     x       ber'x         bei'x         ker'x         kei'x
!   -------------------------------------------------------------
!     0    .000000D+00   .000000D+00  - ì            .000000D+00
!     5   -.384534D+01  -.435414D+01   .171934D-01  -.819979D-03
!    10    .511952D+02   .135309D+03  -.315597D-03   .140914D-03
!    15    .910555D+02  -.408776D+04   .564468D-05  -.588222D-05
!    20   -.488032D+05   .111855D+06  -.750186D-07   .190624D-06
!       ========================================================

IMPLICIT DOUBLE PRECISION (a-h,o-z)
WRITE(*,*)'Please enter x '
!        READ(*,*)X
x=20
WRITE(*,*)'   x        ber x           bei x',  &
    '           ker x           kei x'
WRITE(*,*)'--------------------------------',  &
    '--------------------------------------'
CALL klvnb(x,ber,bei,ger,gei,der,dei,her,hei)
WRITE(*,10)x,ber,bei,ger,gei
WRITE(*,*)
WRITE(*,*)'   x        ber''X           BEI''X',  &
    '           ker''X           KEI''X'
WRITE(*,*)'--------------------------------',  &
    '--------------------------------------'
WRITE(*,10)x,der,dei,her,hei
10      FORMAT(1X,f5.1,4D16.6)
END PROGRAM mklvnb


SUBROUTINE klvnb(x,ber,bei,ger,gei,der,dei,her,hei)

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
IF (x == 0.0D0) THEN
  ber=1.0D0
  bei=0.0D0
  ger=1.0D+300
  gei=-.25D0*pi
  der=0.0D0
  dei=0.0D0
  her=-1.0D+300
  hei=0.0D0
ELSE IF (x < 8.0D0) THEN
  t=x/8.0D0
  t2=t*t
  u=t2*t2
  ber=((((((-.901D-5*u+.122552D-2)*u-.08349609D0)*u  &
      +2.64191397D0)*u-32.36345652D0)*u +113.77777774D0)*u-64.0D0)*u+1.0D0
  bei=t*t*((((((.11346D-3*u-.01103667D0)*u +.52185615D0)*u-10.56765779D0)*u  &
      +72.81777742D0)*u-113.77777774D0)*u+16.0D0)
  ger=((((((-.2458D-4*u+.309699D-2)*u-.19636347D0)  &
      *u+5.65539121D0)*u-60.60977451D0)*u+  &
      171.36272133D0)*u-59.05819744D0)*u-.57721566D0
  ger=ger-DLOG(.5D0*x)*ber+.25D0*pi*bei
  gei=t2*((((((.29532D-3*u-.02695875D0)*u +1.17509064D0)*u-21.30060904D0)*u  &
      +124.2356965D0)*u-142.91827687D0)*u +6.76454936D0)
  gei=gei-DLOG(.5D0*x)*bei-.25D0*pi*ber
  der=x*t2*((((((-.394D-5*u+.45957D-3)*u  &
      -.02609253D0)*u+.66047849D0)*u-6.0681481D0)*u +14.22222222D0)*u-4.0D0)
  dei=x*((((((.4609D-4*u-.379386D-2)*u+.14677204D0)  &
      *u-2.31167514D0)*u+11.37777772D0)*u -10.66666666D0)*u+.5D0)
  her=x*t2*((((((-.1075D-4*u+.116137D-2)*u  &
      -.06136358D0)*u+1.4138478D0)*u-11.36433272D0)  &
      *u+21.42034017D0)*u-3.69113734D0)
  her=her-DLOG(.5D0*x)*der-ber/x+.25D0*pi*dei
  hei=x*((((((.11997D-3*u-.926707D-2)*u  &
      +.33049424D0)*u-4.65950823D0)*u+19.41182758D0)  &
      *u-13.39858846D0)*u+.21139217D0)
  hei=hei-DLOG(.5D0*x)*dei-bei/x-.25D0*pi*der
ELSE
  t=8.0D0/x
  DO  l=1,2
    v=(-1)**l*t
    tpr=((((.6D-6*v-.34D-5)*v-.252D-4)*v-.906D-4) *v*v+.0110486D0)*v
    tpi=((((.19D-5*v+.51D-5)*v*v-.901D-4)*v  &
        -.9765D-3)*v-.0110485D0)*v-.3926991D0
    IF (l == 1) THEN
      tnr=tpr
      tni=tpi
    END IF
  END DO
  yd=x/DSQRT(2.0D0)
  ye1=DEXP(yd+tpr)
  ye2=DEXP(-yd+tnr)
  yc1=1.0D0/DSQRT(2.0D0*pi*x)
  yc2=DSQRT(pi/(2.0D0*x))
  csp=DCOS(yd+tpi)
  ssp=DSIN(yd+tpi)
  csn=DCOS(-yd+tni)
  ssn=DSIN(-yd+tni)
  ger=yc2*ye2*csn
  gei=yc2*ye2*ssn
  fxr=yc1*ye1*csp
  fxi=yc1*ye1*ssp
  ber=fxr-gei/pi
  bei=fxi+ger/pi
  DO  l=1,2
    v=(-1)**l*t
    ppr=(((((.16D-5*v+.117D-4)*v+.346D-4)*v+.5D-6)  &
        *v-.13813D-2)*v-.0625001D0)*v+.7071068D0
    ppi=(((((-.32D-5*v-.24D-5)*v+.338D-4)*v+  &
        .2452D-3)*v+.13811D-2)*v-.1D-6)*v+.7071068D0
    IF (l == 1) THEN
      pnr=ppr
      pni=ppi
    END IF
  END DO
  her=gei*pni-ger*pnr
  hei=-(gei*pnr+ger*pni)
  der=fxr*ppr-fxi*ppi-hei/pi
  dei=fxi*ppr+fxr*ppi+her/pi
END IF
RETURN
END SUBROUTINE klvnb
