PROGRAM mitjyb
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:11

!       ===========================================================
!       Purpose: This program evaluates the integral of Bessel
!                functions J0(t) and Y0(t) with respect to t
!                from 0 to x using subroutine ITJYB
!       Input :  x  --- Upper limit of the integral ( x ò 0 )
!       Output:  TJ --- Integration of J0(t) from 0 to x
!                TY --- Integration of Y0(t) from 0 to x
!       Example:
!                   x         J0(t)dt          Y0(t)dt
!                ---------------------------------------
!                  5.0       .71531192       .19971938
!                 10.0      1.06701130       .24129032
!                 15.0      1.20516194       .00745772
!                 20.0      1.05837882      -.16821598
!                 25.0       .87101492      -.09360793
!                 30.0       .88424909       .08822971
!       ===========================================================

DOUBLE PRECISION :: x,tj,ty
WRITE(*,*)'Pleas enter x '
!        READ(*,*)X
x=30.0
WRITE(*,*)'   x         J0(t)dt          Y0(t)dt'
WRITE(*,*)'---------------------------------------'
CALL itjyb(x,tj,ty)
WRITE(*,10)x,tj,ty
10      FORMAT(1X,f5.1,2F16.8)
END PROGRAM mitjyb


SUBROUTINE itjyb(x,tj,ty)

!       =======================================================
!       Purpose: Integrate Bessel functions J0(t) and Y0(t)
!                with respect to t from 0 to x ( x ò 0 )
!       Input :  x  --- Upper limit of the integral
!       Output:  TJ --- Integration of J0(t) from 0 to x
!                TY --- Integration of Y0(t) from 0 to x
!       =======================================================



DOUBLE PRECISION, INTENT(IN)             :: x
DOUBLE PRECISION, INTENT(OUT)            :: tj
DOUBLE PRECISION, INTENT(OUT)            :: ty
IMPLICIT DOUBLE PRECISION (a-h,o-z)
pi=3.141592653589793D0
IF (x == 0.0D0) THEN
  tj=0.0D0
  ty=0.0D0
ELSE IF (x <= 4.0D0) THEN
  x1=x/4.0D0
  t=x1*x1
  tj=(((((((-.133718D-3*t+.2362211D-2)*t  &
      -.025791036D0)*t+.197492634D0)*t-1.015860606D0)  &
      *t+3.199997842D0)*t-5.333333161D0)*t+4.0D0)*x1
  ty=((((((((.13351D-4*t-.235002D-3)*t+.3034322D-2)*  &
      t-.029600855D0)*t+.203380298D0)*t-.904755062D0)  &
      *t+2.287317974D0)*t-2.567250468D0)*t +1.076611469D0)*x1
  ty=2.0D0/pi*DLOG(x/2.0D0)*tj-ty
ELSE IF (x <= 8.0D0) THEN
  xt=x-.25D0*pi
  t=16.0D0/(x*x)
  f0=((((((.1496119D-2*t-.739083D-2)*t+.016236617D0)  &
      *t-.022007499D0)*t+.023644978D0) *t-.031280848D0)*t+.124611058D0)*4.0D0/x
  g0=(((((.1076103D-2*t-.5434851D-2)*t+.01242264D0)  &
      *t-.018255209)*t+.023664841D0)*t-.049635633D0) *t+.79784879D0
  tj=1.0D0-(f0*DCOS(xt)-g0*DSIN(xt))/DSQRT(x)
  ty=-(f0*DSIN(xt)+g0*DCOS(xt))/DSQRT(x)
ELSE
  t=64.0D0/(x*x)
  xt=x-.25D0*pi
  f0=(((((((-.268482D-4*t+.1270039D-3)*t  &
      -.2755037D-3)*t+.3992825D-3)*t-.5366169D-3)*t  &
      +.10089872D-2)*t-.40403539D-2)*t+.0623347304D0) *8.0D0/x
  g0=((((((-.226238D-4*t+.1107299D-3)*t-.2543955D-3)  &
      *t+.4100676D-3)*t-.6740148D-3)*t+.17870944D-2)  &
      *t-.01256424405D0)*t+.79788456D0
  tj=1.0D0-(f0*DCOS(xt)-g0*DSIN(xt))/DSQRT(x)
  ty=-(f0*DSIN(xt)+g0*DCOS(xt))/DSQRT(x)
END IF
RETURN
END SUBROUTINE itjyb

