PROGRAM mcik01
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:10

!       =============================================================
!       Purpose: This program computes the modified Bessel functions
!                I0(z), I1(z), K0(z), K1(z), and their derivatives
!                for a complex argument using subroutine CIK01
!       Input :  z --- Complex argument
!       Output:  CBI0 --- I0(z)
!                CDI0 --- I0'(z)
!                CBI1 --- I1(z)
!                CDI1 --- I1'(z)
!                CBK0 --- K0(z)
!                CDK0 --- K0'(z)
!                CBK1 --- K1(z)
!                CDK1 --- K1'(z)
!       Example: z = 20.0 + i 10.0

!     n      Re[In(z)]      Im[In(z)]      Re[In'(z)]     Im[In'(z)]
!    -----------------------------------------------------------------
!     0   -.38773811D+08 -.13750292D+08 -.37852037D+08 -.13869150D+08
!     1   -.37852037D+08 -.13869150D+08 -.36982347D+08 -.13952566D+08

!     n      Re[Kn(z)]      Im[Kn(z)]      Re[Kn'(z)]     Im[Kn'(z)]
!    -----------------------------------------------------------------
!     0   -.37692389D-09  .39171613D-09  .38056380D-09 -.40319029D-09
!     1   -.38056380D-09  .40319029D-09  .38408264D-09 -.41545502D-09
!       =============================================================

IMPLICIT DOUBLE PRECISION (x,y)
IMPLICIT COMPLEX*16 (c,z)
WRITE(*,*)'  Please enter x and y (z=x+iy) '
!        READ(*,*)X,Y
x=20.0
y=10.0
z=CMPLX(x,y)
WRITE(*,30)x,y
CALL cik01(z,cbi0,cdi0,cbi1,cdi1,cbk0,cdk0,cbk1,cdk1)
WRITE(*,*)
WRITE(*,*)'  n      Re[In(z)]      Im[In(z)]',  &
    '      Re[In''(Z)]     IM[IN''(Z)]'
WRITE(*,*)' -------------------------------',  &
    '----------------------------------'
WRITE(*,10)cbi0,cdi0
WRITE(*,20)cbi1,cdi1
WRITE(*,*)
WRITE(*,*)'  n      Re[Kn(z)]      Im[Kn(z)]',  &
    '      Re[Kn''(Z)]     IM[KN''(Z)]'
WRITE(*,*)' -------------------------------',  &
    '----------------------------------'
WRITE(*,10)cbk0,cdk0
WRITE(*,20)cbk1,cdk1
10      FORMAT(3X,'0',2X,4D15.7)
20      FORMAT(3X,'1',2X,4D15.7)
30      FORMAT(3X,3HZ =,f7.2,' + i',f7.2)
END PROGRAM mcik01


SUBROUTINE cik01(z,cbi0,cdi0,cbi1,cdi1,cbk0,cdk0,cbk1,cdk1)

!       ==========================================================
!       Purpose: Compute modified Bessel functions I0(z), I1(z),
!                K0(z), K1(z), and their derivatives for a
!                complex argument
!       Input :  z --- Complex argument
!       Output:  CBI0 --- I0(z)
!                CDI0 --- I0'(z)
!                CBI1 --- I1(z)
!                CDI1 --- I1'(z)
!                CBK0 --- K0(z)
!                CDK0 --- K0'(z)
!                CBK1 --- K1(z)
!                CDK1 --- K1'(z)
!       ==========================================================


COMPLEX, INTENT(IN)                      :: z
COMPLEX, INTENT(OUT)                     :: cbi0
COMPLEX, INTENT(OUT)                     :: cdi0
COMPLEX, INTENT(OUT)                     :: cbi1
COMPLEX, INTENT(OUT)                     :: cdi1
COMPLEX, INTENT(OUT)                     :: cbk0
COMPLEX, INTENT(OUT)                     :: cdk0
COMPLEX, INTENT(OUT)                     :: cbk1
COMPLEX, INTENT(OUT)                     :: cdk1
IMPLICIT DOUBLE PRECISION (a,b,d-h,o-y)
IMPLICIT COMPLEX*16 (c,z)
DIMENSION a(12),b(12),a1(10)

pi=3.141592653589793D0
ci=(0.0D0,1.0D0)
a0=CDABS(z)
z2=z*z
z1=z
IF (a0 == 0.0D0) THEN
  cbi0=(1.0D0,0.0D0)
  cbi1=(0.0D0,0.0D0)
  cdi0=(0.0D0,0.0D0)
  cdi1=(0.5D0,0.0D0)
  cbk0=(1.0D+300,0.0D0)
  cbk1=(1.0D+300,0.0D0)
  cdk0=-(1.0D+300,0.0D0)
  cdk1=-(1.0D+300,0.0D0)
  RETURN
END IF
IF (REAL(z) < 0.0) z1=-z
IF (a0 <= 18.0) THEN
  cbi0=(1.0D0,0.0D0)
  cr=(1.0D0,0.0D0)
  DO  k=1,50
    cr=0.25D0*cr*z2/(k*k)
    cbi0=cbi0+cr
    IF (CDABS(cr/cbi0) < 1.0D-15) EXIT
  END DO
  15         cbi1=(1.0D0,0.0D0)
  cr=(1.0D0,0.0D0)
  DO  k=1,50
    cr=0.25D0*cr*z2/(k*(k+1))
    cbi1=cbi1+cr
    IF (CDABS(cr/cbi1) < 1.0D-15) EXIT
  END DO
  25         cbi1=0.5D0*z1*cbi1
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
  IF (a0 >= 35.0) k0=9
  IF (a0 >= 50.0) k0=7
  ca=CDEXP(z1)/CDSQRT(2.0D0*pi*z1)
  cbi0=(1.0D0,0.0D0)
  zr=1.0D0/z1
  DO  k=1,k0
    cbi0=cbi0+a(k)*zr**k
  END DO
  cbi0=ca*cbi0
  cbi1=(1.0D0,0.0D0)
  DO  k=1,k0
    cbi1=cbi1+b(k)*zr**k
  END DO
  cbi1=ca*cbi1
END IF
IF (a0 <= 9.0) THEN
  cs=(0.0D0,0.0D0)
  ct=-CDLOG(0.5D0*z1)-0.5772156649015329D0
  w0=0.0D0
  cr=(1.0D0,0.0D0)
  DO  k=1,50
    w0=w0+1.0D0/k
    cr=0.25D0*cr/(k*k)*z2
    cs=cs+cr*(w0+ct)
    IF (CDABS((cs-cw)/cs) < 1.0D-15) GO TO 45
    cw=cs
  END DO
  45         cbk0=ct+cs
ELSE
  DATA a1/0.125D0,0.2109375D0, 1.0986328125D0,1.1775970458984D01,  &
      2.1461706161499D02,5.9511522710323D03,  &
      2.3347645606175D05,1.2312234987631D07, 8.401390346421D08,7.2031420482627D10/
  cb=0.5D0/z1
  zr2=1.0D0/z2
  cbk0=(1.0D0,0.0D0)
  DO  k=1,10
    cbk0=cbk0+a1(k)*zr2**k
  END DO
  cbk0=cb*cbk0/cbi0
END IF
cbk1=(1.0D0/z1-cbi1*cbk0)/cbi0
IF (REAL(z) < 0.0) THEN
  IF (DIMAG(z) < 0.0) cbk0=cbk0+ci*pi*cbi0
  IF (DIMAG(z) > 0.0) cbk0=cbk0-ci*pi*cbi0
  IF (DIMAG(z) < 0.0) cbk1=-cbk1+ci*pi*cbi1
  IF (DIMAG(z) > 0.0) cbk1=-cbk1-ci*pi*cbi1
  cbi1=-cbi1
END IF
cdi0=cbi1
cdi1=cbi0-1.0D0/z*cbi1
cdk0=-cbk1
cdk1=-cbk0-1.0D0/z*cbk1
RETURN
END SUBROUTINE cik01
