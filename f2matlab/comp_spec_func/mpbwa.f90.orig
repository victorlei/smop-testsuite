PROGRAM mpbwa
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:12

!       ============================================================
!       Purpose: This program computes the parabolic cylinder
!                functions W(a,ñx) and their derivatives using
!                subroutine PBWA
!       Input  : a --- Parameter  ( 0 ó |a| ó 5 )
!                x --- Argument of W(a,ñx)  ( 0 ó |x| ó 5 )
!       Output : W1F --- W(a,x)
!                W1D --- W'(a,x)
!                W2F --- W(a,-x)
!                W2D --- W'(a,-x)
!       Example: x = 5.0
!                 a      W(a,x)     W'(a,x)    W(a,-x)   W'(a,-x)
!              ----------------------------------------------------
!                0.5   .1871153    .1915744  -.8556585   4.4682493
!                1.5  -.0215853    .0899870 -8.8586002  -9.3971967
!                0.0   .3009549   -.7148233   .6599634   1.7552224
!               -0.5  -.1934088  -1.3474400   .6448148   -.6781011
!               -1.5  -.5266539    .8219516  -.2822774  -1.4582283
!               -5.0   .0893618  -1.8118641   .5386084    .2698553
!       ============================================================

IMPLICIT DOUBLE PRECISION (a-h,o-z)
WRITE(*,*)'Please enter a and x '
!        READ(*,*)A,X
a=-5.0
x=5.0
WRITE(*,10)a,x
WRITE(*,*)
WRITE(*,*)'   a       W(a,x)          W''(A,X)',  &
    '         W(a,-x)         W''(A,-X)'
WRITE(*,*)' -----------------------------------------',  &
    '----------------------------'
CALL pbwa(a,x,w1f,w1d,w2f,w2d)
WRITE(*,20)a,w1f,w1d,w2f,w2d
10      FORMAT(1X,'a=',f5.1,3X,'x=',f5.1)
20      FORMAT(1X,f5.1,4D16.8)
END PROGRAM mpbwa


SUBROUTINE pbwa(a,x,w1f,w1d,w2f,w2d)

!       ======================================================
!       Purpose: Compute parabolic cylinder functions W(a,ñx)
!                and their derivatives
!       Input  : a --- Parameter  ( 0 ó |a| ó 5 )
!                x --- Argument of W(a,ñx)  ( 0 ó |x| ó 5 )
!       Output : W1F --- W(a,x)
!                W1D --- W'(a,x)
!                W2F --- W(a,-x)
!                W2D --- W'(a,-x)
!       Routine called:
!               CGAMA for computing complex gamma function
!       ======================================================


DOUBLE PRECISION, INTENT(IN)             :: a
DOUBLE PRECISION, INTENT(IN)             :: x
DOUBLE PRECISION, INTENT(OUT)            :: w1f
DOUBLE PRECISION, INTENT(OUT)            :: w1d
DOUBLE PRECISION, INTENT(OUT)            :: w2f
DOUBLE PRECISION, INTENT(OUT)            :: w2d
IMPLICIT DOUBLE PRECISION (a,b,d-h,o-y)
IMPLICIT COMPLEX *16 (c,z)
DIMENSION h(100),d(100)

x1=0.0
eps=1.0D-15
p0=0.59460355750136D0
IF (a == 0.0D0) THEN
  g1=3.625609908222D0
  g2=1.225416702465D0
ELSE
  x1=0.25D0
  y1=0.5D0*a
  CALL cgama(x1,y1,1,ugr,ugi)
  g1=DSQRT(ugr*ugr+ugi*ugi)
  x2=0.75D0
  CALL cgama(x2,y1,1,vgr,vgi)
  g2=DSQRT(vgr*vgr+vgi*vgi)
END IF
f1=DSQRT(g1/g2)
f2=DSQRT(2.0D0*g2/g1)
h0=1.0D0
h1=a
h(1)=a
DO  l1=4,200,2
  m=l1/2
  hl=a*h1-0.25D0*(l1-2.0D0)*(l1-3.0D0)*h0
  h(m)=hl
  h0=h1
  h1=hl
END DO
y1f=1.0D0
r=1.0D0
DO  k=1,100
  r=0.5D0*r*x*x/(k*(2.0D0*k-1.0D0))
  r1=h(k)*r
  y1f=y1f+r1
  IF (DABS(r1/y1f) <= eps.AND.k > 30) EXIT
END DO
20      y1d=a
r=1.0D0
DO  k=1,100
  r=0.5D0*r*x*x/(k*(2.0D0*k+1.0D0))
  r1=h(k+1)*r
  y1d=y1d+r1
  IF (DABS(r1/y1d) <= eps.AND.k > 30) EXIT
END DO
30      y1d=x*y1d
d1=1.0D0
d2=a
d(1)=1.0D0
d(2)=a
DO  l2=5,160,2
  m=(l2+1)/2
  dl=a*d2-0.25D0*(l2-2.0D0)*(l2-3.0D0)*d1
  d(m)=dl
  d1=d2
  d2=dl
END DO
y2f=1.0D0
r=1.0D0
DO  k=1,100
  r=0.5D0*r*x*x/(k*(2.0D0*k+1.0D0))
  r1=d(k+1)*r
  y2f=y2f+r1
  IF (DABS(r1/y2f) <= eps.AND.k > 30) EXIT
END DO
50      y2f=x*y2f
y2d=1.0D0
r=1.0D0
DO  k=1,100
  r=0.5D0*r*x*x/(k*(2.0D0*k-1.0D0))
  r1=d(k+1)*r
  y2d=y2d+r1
  IF (DABS(r1/y2d) <= eps.AND.k > 30) EXIT
END DO
60      w1f=p0*(f1*y1f-f2*y2f)
w2f=p0*(f1*y1f+f2*y2f)
w1d=p0*(f1*y1d-f2*y2d)
w2d=p0*(f1*y1d+f2*y2d)
RETURN
END SUBROUTINE pbwa


SUBROUTINE cgama(x,y,kf,gr,gi)

!       =========================================================
!       Purpose: Compute complex gamma function â(z) or Ln[â(z)]
!       Input :  x  --- Real part of z
!                y  --- Imaginary part of z
!                KF --- Function code
!                       KF=0 for Ln[â(z)]
!                       KF=1 for â(z)
!       Output:  GR --- Real part of Ln[â(z)] or â(z)
!                GI --- Imaginary part of Ln[â(z)] or â(z)
!       ========================================================


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
