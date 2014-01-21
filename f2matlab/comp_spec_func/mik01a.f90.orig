PROGRAM mik01a
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:11

!       =============================================================
!       Purpose: This program computes the modified Bessel functions
!                I0(x), I1(x), K0(x), K1(x), and their derivatives
!                using subroutine IK01A
!       Input :  x   --- Argument ( x ò 0 )
!       Output:  BI0 --- I0(x)
!                DI0 --- I0'(x)
!                BI1 --- I1(x)
!                DI1 --- I1'(x)
!                BK0 --- K0(x)
!                DK0 --- K0'(x)
!                BK1 --- K1(x)
!                DK1 --- K1'(x)
!       Example:

!         x      I0(x)         I0'(x)        I1(x)         I1'(x)
!       -------------------------------------------------------------
!        1.0  .1266066D+01  .5651591D+00  .5651591D+00  .7009068D+00
!       10.0  .2815717D+04  .2670988D+04  .2670988D+04  .2548618D+04
!       20.0  .4355828D+08  .4245497D+08  .4245497D+08  .4143553D+08
!       30.0  .7816723D+12  .7685320D+12  .7685320D+12  .7560546D+12
!       40.0  .1489477D+17  .1470740D+17  .1470740D+17  .1452709D+17
!       50.0  .2932554D+21  .2903079D+21  .2903079D+21  .2874492D+21

!         x      K0(x)         K0'(x)        K1(x)         K1'(x)
!       -------------------------------------------------------------
!        1.0  .4210244D+00 -.6019072D+00  .6019072D+00 -.1022932D+01
!       10.0  .1778006D-04 -.1864877D-04  .1864877D-04 -.1964494D-04
!       20.0  .5741238D-09 -.5883058D-09  .5883058D-09 -.6035391D-09
!       30.0  .2132477D-13 -.2167732D-13  .2167732D-13 -.2204735D-13
!       40.0  .8392861D-18 -.8497132D-18  .8497132D-18 -.8605289D-18
!       50.0  .3410168D-22 -.3444102D-22  .3444102D-22 -.3479050D-22
!       =============================================================

IMPLICIT DOUBLE PRECISION (a-h,o-z)
WRITE(*,*)'Please enter x '
!        READ(*,*)X
x=1.0
WRITE(*,10)x
WRITE(*,*)'  x       I0(x)          I0''(X)         I1(X)',  &
    '          I1''(X)'
WRITE(*,*)'-------------------------------------------',  &
    '----------------------'
CALL ik01a(x,bi0,di0,bi1,di1,bk0,dk0,bk1,dk1)
WRITE(*,20)x,bi0,di0,bi1,di1
WRITE(*,*)
WRITE(*,*)'  x       K0(x)          K0''(X)         K1(X)',  &
    '          K1''(X)'
WRITE(*,*)'-------------------------------------------',  &
    '----------------------'
WRITE(*,20)x,bk0,dk0,bk1,dk1
10      FORMAT(3X 'x =',f5.1)
20      FORMAT(1X,f4.1,4D15.7)
END PROGRAM mik01a


SUBROUTINE ik01a(x,bi0,di0,bi1,di1,bk0,dk0,bk1,dk1)

!       =========================================================
!       Purpose: Compute modified Bessel functions I0(x), I1(1),
!                K0(x) and K1(x), and their derivatives
!       Input :  x   --- Argument ( x ò 0 )
!       Output:  BI0 --- I0(x)
!                DI0 --- I0'(x)
!                BI1 --- I1(x)
!                DI1 --- I1'(x)
!                BK0 --- K0(x)
!                DK0 --- K0'(x)
!                BK1 --- K1(x)
!                DK1 --- K1'(x)
!       =========================================================


DOUBLE PRECISION, INTENT(IN)             :: x
DOUBLE PRECISION, INTENT(OUT)            :: bi0
DOUBLE PRECISION, INTENT(OUT)            :: di0
DOUBLE PRECISION, INTENT(OUT)            :: bi1
DOUBLE PRECISION, INTENT(OUT)            :: di1
DOUBLE PRECISION, INTENT(OUT)            :: bk0
DOUBLE PRECISION, INTENT(OUT)            :: dk0
DOUBLE PRECISION, INTENT(OUT)            :: bk1
DOUBLE PRECISION, INTENT(OUT)            :: dk1
IMPLICIT DOUBLE PRECISION (a-h,o-z)
DIMENSION a(12),b(12),a1(8)

ww=0.0
pi=3.141592653589793D0
el=0.5772156649015329D0
x2=x*x
IF (x == 0.0D0) THEN
  bi0=1.0D0
  bi1=0.0D0
  bk0=1.0D+300
  bk1=1.0D+300
  di0=0.0D0
  di1=0.5D0
  dk0=-1.0D+300
  dk1=-1.0D+300
  RETURN
ELSE IF (x <= 18.0D0) THEN
  bi0=1.0D0
  r=1.0D0
  DO  k=1,50
    r=0.25D0*r*x2/(k*k)
    bi0=bi0+r
    IF (DABS(r/bi0) < 1.0D-15) EXIT
  END DO
  20         bi1=1.0D0
  r=1.0D0
  DO  k=1,50
    r=0.25D0*r*x2/(k*(k+1))
    bi1=bi1+r
    IF (DABS(r/bi1) < 1.0D-15) EXIT
  END DO
  30         bi1=0.5D0*x*bi1
ELSE
  DATA a/0.125D0,7.03125D-2, 7.32421875D-2,1.1215209960938D-1,  &
      2.2710800170898D-1,5.7250142097473D-1,  &
      1.7277275025845D0,6.0740420012735D0,  &
      2.4380529699556D01,1.1001714026925D02,  &
      5.5133589612202D02,3.0380905109224D03/
  DATA b/-0.375D0,-1.171875D-1, -1.025390625D-1,-1.4419555664063D-1,  &
      -2.7757644653320D-1,-6.7659258842468D-1,  &
      -1.9935317337513D0,-6.8839142681099D0,  &
      -2.7248827311269D01,-1.2159789187654D02,  &
      -6.0384407670507D02,-3.3022722944809D03/
  k0=12
  IF (x >= 35.0) k0=9
  IF (x >= 50.0) k0=7
  ca=DEXP(x)/DSQRT(2.0D0*pi*x)
  bi0=1.0D0
  xr=1.0D0/x
  DO  k=1,k0
    bi0=bi0+a(k)*xr**k
  END DO
  bi0=ca*bi0
  bi1=1.0D0
  DO  k=1,k0
    bi1=bi1+b(k)*xr**k
  END DO
  bi1=ca*bi1
END IF
IF (x <= 9.0D0) THEN
  ct=-(DLOG(x/2.0D0)+el)
  bk0=0.0D0
  w0=0.0D0
  r=1.0D0
  DO  k=1,50
    w0=w0+1.0D0/k
    r=0.25D0*r/(k*k)*x2
    bk0=bk0+r*(w0+ct)
    IF (DABS((bk0-ww)/bk0) < 1.0D-15) EXIT
    ww=bk0
  END DO
  70         bk0=bk0+ct
ELSE
  DATA a1/0.125D0,0.2109375D0, 1.0986328125D0,1.1775970458984D01,  &
      2.1461706161499D02,5.9511522710323D03,  &
      2.3347645606175D05,1.2312234987631D07/
  cb=0.5D0/x
  xr2=1.0D0/x2
  bk0=1.0D0
  DO  k=1,8
    bk0=bk0+a1(k)*xr2**k
  END DO
  bk0=cb*bk0/bi0
END IF
bk1=(1.0D0/x-bi1*bk0)/bi0
di0=bi1
di1=bi0-bi1/x
dk0=-bk1
dk1=-bk0-bk1/x
RETURN
END SUBROUTINE ik01a
