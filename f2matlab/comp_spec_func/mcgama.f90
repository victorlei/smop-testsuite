PROGRAM mcgama
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:10

!     ==========================================================
!     Purpose: This program computes the gamma function â(z)
!     or ln[â(z)] for a complex argument using
!     subroutine CGAMA
!     Input :  x  --- Real part of z
!     y  --- Imaginary part of z
!     KF --- Function code
!     KF=0 for ln[â(z)]
!     KF=1 for â(z)
!     Output:  GR --- Real part of ln[â(z)] or â(z)
!     GI --- Imaginary part of ln[â(z)] or â(z)
!     Examples:

!     x         y           Re[â(z)]           Im[â(z)]
!     --------------------------------------------------------
!     2.50      5.00     .2267360319D-01    -.1172284404D-01
!     5.00     10.00     .1327696517D-01     .3639011746D-02
!     2.50     -5.00     .2267360319D-01     .1172284404D-01
!     5.00    -10.00     .1327696517D-01    -.3639011746D-02

!     x         y          Re[lnâ(z)]         Im[lnâ(z)]
!     ---------------------------------------------------------
!     2.50      5.00    -.3668103262D+01     .5806009801D+01
!     5.00     10.00    -.4285507444D+01     .1911707090D+02
!     2.50     -5.00    -.3668103262D+01    -.5806009801D+01
!     5.00    -10.00    -.4285507444D+01    -.1911707090D+02
!     ==========================================================

DOUBLE PRECISION :: x,y,gr,gi
WRITE(*,*)'  Please enter KF, x and y'
!     READ(*,*)KF,X,Y
kf=1
x=2.5
y=5.0
WRITE(*,*)
IF (kf == 1) THEN
  WRITE(*,*)'       x         y           Re[â(z)]', '           Im[â(z)]'
ELSE
  WRITE(*,*)'       x         y          Re[lnâ(z)]', '         Im[lnâ(z)]'
END IF
WRITE(*,*)'    ------------------------------------', '---------------------'
CALL cgama(x,y,kf,gr,gi)
WRITE(*,10)x,y,gr,gi
10   FORMAT(1X,2F10.2,2D20.10)
END PROGRAM mcgama


SUBROUTINE cgama(x,y,kf,gr,gi)

!     =========================================================
!     Purpose: Compute the gamma function â(z) or ln[â(z)]
!     for a complex argument
!     Input :  x  --- Real part of z
!     y  --- Imaginary part of z
!     KF --- Function code
!     KF=0 for ln[â(z)]
!     KF=1 for â(z)
!     Output:  GR --- Real part of ln[â(z)] or â(z)
!     GI --- Imaginary part of ln[â(z)] or â(z)
!     ========================================================


DOUBLE PRECISION, INTENT(IN OUT)         :: x
DOUBLE PRECISION, INTENT(IN OUT)         :: y
INTEGER, INTENT(IN)                      :: kf
DOUBLE PRECISION, INTENT(OUT)            :: gr
DOUBLE PRECISION, INTENT(OUT)            :: gi
IMPLICIT DOUBLE PRECISION (a-h,o-z)
DIMENSION a(10)

x1=0.0
pi=3.141592653589793D0
DATA a/8.333333333333333D-02,-2.777777777777778D-03,  &
    7.936507936507937D-04,-5.952380952380952D-04,  &
    8.417508417508418D-04,-1.917526917526918D-03,  &
    6.410256410256410D-03,-2.955065359477124D-02,  &
    1.796443723688307D-01,-1.39243221690590D+00/
IF (y == 0.0D0.AND.x == INT(x).AND.x <= 0.0D0) THEN
  gr=1.0D+300
  gi=0.0D0
  RETURN
ELSE IF (x < 0.0D0) THEN
  x1=x
  y1=y
  x=-x
  y=-y
END IF
x0=x
IF (x <= 7.0) THEN
  na=INT(7-x)
  x0=x+na
END IF
z1=DSQRT(x0*x0+y*y)
th=DATAN(y/x0)
gr=(x0-.5D0)*DLOG(z1)-th*y-x0+0.5D0*DLOG(2.0D0*pi)
gi=th*(x0-0.5D0)+y*DLOG(z1)-y
DO  k=1,10
  t=z1**(1-2*k)
  gr=gr+a(k)*t*DCOS((2.0D0*k-1.0D0)*th)
  gi=gi-a(k)*t*DSIN((2.0D0*k-1.0D0)*th)
END DO
IF (x <= 7.0) THEN
  gr1=0.0D0
  gi1=0.0D0
  DO  j=0,na-1
    gr1=gr1+.5D0*DLOG((x+j)**2+y*y)
    gi1=gi1+DATAN(y/(x+j))
  END DO
  gr=gr-gr1
  gi=gi-gi1
END IF
IF (x1 < 0.0D0) THEN
  z1=DSQRT(x*x+y*y)
  th1=DATAN(y/x)
  sr=-DSIN(pi*x)*DCOSH(pi*y)
  si=-DCOS(pi*x)*DSINH(pi*y)
  z2=DSQRT(sr*sr+si*si)
  th2=DATAN(si/sr)
  IF (sr < 0.0D0) THEN
    th2=pi+th2
  END IF
  gr=DLOG(pi/(z1*z2))-gr
  gi=-th1-th2-gi
  x=x1
  y=y1
END IF
IF (kf == 1) THEN
  g0=DEXP(gr)
  gr=g0*DCOS(gi)
  gi=g0*DSIN(gi)
END IF
RETURN
END SUBROUTINE cgama
