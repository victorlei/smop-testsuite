PROGRAM mcjy01
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:10

!       ================================================================
!       Purpose: This program computes Bessel functions J0(z), J1(z),
!                Y0(z), Y1(z), and their derivatives for a complex
!                argument using subroutine CJY01
!       Input :  z --- Complex argument
!       Output:  CBJ0 --- J0(z)
!                CDJ0 --- J0'(z)
!                CBJ1 --- J1(z)
!                CDJ1 --- J1'(z)
!                CBY0 --- Y0(z)
!                CDY0 --- Y0'(z)
!                CBY1 --- Y1(z)
!                CDY1 --- Y1'(z)
!       Example: z =  4.0 + i  2.0

!     n     Re[Jn(z)]       Im[Jn(z)]       Re[Jn'(z)]      Im[Jn'(z)]
!   --------------------------------------------------------------------
!     0  -.13787022D+01   .39054236D+00   .50735255D+00   .12263041D+01
!     1  -.50735255D+00  -.12263041D+01  -.11546013D+01   .58506793D+00

!     n     Re[Yn(z)]       Im[Yn(z)]       Re[Yn'(z)]      Im[Yn'(z)]
!   --------------------------------------------------------------------
!     0  -.38145893D+00  -.13291649D+01  -.12793101D+01   .51220420D+00
!     1   .12793101D+01  -.51220420D+00  -.58610052D+00  -.10987930D+01
!       ================================================================

IMPLICIT DOUBLE PRECISION (x,y)
IMPLICIT COMPLEX*16 (c,z)
WRITE(*,*)'  Please enter x,y (z=x+iy) '
!        READ(*,*)X,Y
x=4.0
y=2.0
z=CMPLX(x,y)
WRITE(*,30)x,y
CALL cjy01(z,cbj0,cdj0,cbj1,cdj1,cby0,cdy0,cby1,cdy1)
WRITE(*,*)
WRITE(*,*)'  n      Re[Jn(z)]       Im[Jn(z)]',  &
    '       Re[Jn''(Z)]      IM[JN''(Z)]'
WRITE(*,*)' -------------------------------------',  &
    '-------------------------------'
WRITE(*,10)cbj0,cdj0
WRITE(*,20)cbj1,cdj1
WRITE(*,*)
WRITE(*,*)'  n      Re[Yn(z)]       Im[Yn(z)]',  &
    '       Re[Yn''(Z)]      IM[YN''(Z)]'
WRITE(*,*)' -------------------------------------',  &
    '-------------------------------'
WRITE(*,10)cby0,cdy0
WRITE(*,20)cby1,cdy1
10      FORMAT(3X,'0',2X,4D16.8)
20      FORMAT(3X,'1',2X,4D16.8)
30      FORMAT(3X,3HZ =,f6.2,' + i',f6.2)
END PROGRAM mcjy01


SUBROUTINE cjy01(z,cbj0,cdj0,cbj1,cdj1,cby0,cdy0,cby1,cdy1)

!       =======================================================
!       Purpose: Compute Bessel functions J0(z), J1(z), Y0(z),
!                Y1(z), and their derivatives for a complex
!                argument
!       Input :  z --- Complex argument
!       Output:  CBJ0 --- J0(z)
!                CDJ0 --- J0'(z)
!                CBJ1 --- J1(z)
!                CDJ1 --- J1'(z)
!                CBY0 --- Y0(z)
!                CDY0 --- Y0'(z)
!                CBY1 --- Y1(z)
!                CDY1 --- Y1'(z)
!       =======================================================


COMPLEX, INTENT(IN)                      :: z
COMPLEX, INTENT(OUT)                     :: cbj0
COMPLEX, INTENT(OUT)                     :: cdj0
COMPLEX, INTENT(OUT)                     :: cbj1
COMPLEX, INTENT(OUT)                     :: cdj1
COMPLEX, INTENT(OUT)                     :: cby0
COMPLEX, INTENT(OUT)                     :: cdy0
COMPLEX, INTENT(OUT)                     :: cby1
COMPLEX, INTENT(OUT)                     :: cdy1
IMPLICIT DOUBLE PRECISION (a,b,e,p,r,w)
IMPLICIT COMPLEX*16 (c,z)
DIMENSION a(12),b(12),a1(12),b1(12)

pi=3.141592653589793D0
el=0.5772156649015329D0
rp2=2.0D0/pi
ci=(0.0D0,1.0D0)
a0=CDABS(z)
z2=z*z
z1=z
IF (a0 == 0.0D0) THEN
  cbj0=(1.0D0,0.0D0)
  cbj1=(0.0D0,0.0D0)
  cdj0=(0.0D0,0.0D0)
  cdj1=(0.5D0,0.0D0)
  cby0=-(1.0D300,0.0D0)
  cby1=-(1.0D300,0.0D0)
  cdy0=(1.0D300,0.0D0)
  cdy1=(1.0D300,0.0D0)
  RETURN
END IF
IF (REAL(z) < 0.0) z1=-z
IF (a0 <= 12.0) THEN
  cbj0=(1.0D0,0.0D0)
  cr=(1.0D0,0.0D0)
  DO  k=1,40
    cr=-0.25D0*cr*z2/(k*k)
    cbj0=cbj0+cr
    IF (CDABS(cr) < CDABS(cbj0)*1.0D-15) EXIT
  END DO
  15         cbj1=(1.0D0,0.0D0)
  cr=(1.0D0,0.0D0)
  DO  k=1,40
    cr=-0.25D0*cr*z2/(k*(k+1.0D0))
    cbj1=cbj1+cr
    IF (CDABS(cr) < CDABS(cbj1)*1.0D-15) EXIT
  END DO
  25         cbj1=0.5D0*z1*cbj1
  w0=0.0D0
  cr=(1.0D0,0.0D0)
  cs=(0.0D0,0.0D0)
  DO  k=1,40
    w0=w0+1.0D0/k
    cr=-0.25D0*cr/(k*k)*z2
    cp=cr*w0
    cs=cs+cp
    IF (CDABS(cp) < CDABS(cs)*1.0D-15) EXIT
  END DO
  35         cby0=rp2*(CDLOG(z1/2.0D0)+el)*cbj0-rp2*cs
  w1=0.0D0
  cr=(1.0D0,0.0D0)
  cs=(1.0D0,0.0D0)
  DO  k=1,40
    w1=w1+1.0D0/k
    cr=-0.25D0*cr/(k*(k+1))*z2
    cp=cr*(2.0D0*w1+1.0D0/(k+1.0D0))
    cs=cs+cp
    IF (CDABS(cp) < CDABS(cs)*1.0D-15) EXIT
  END DO
  45         cby1=rp2*((CDLOG(z1/2.0D0)+el)*cbj1-1.0D0/z1-.25D0*z1*cs)
ELSE
  DATA a/-.703125D-01,.112152099609375D+00,  &
      -.5725014209747314D+00,.6074042001273483D+01,  &
      -.1100171402692467D+03,.3038090510922384D+04,  &
      -.1188384262567832D+06,.6252951493434797D+07,  &
      -.4259392165047669D+09,.3646840080706556D+11,  &
      -.3833534661393944D+13,.4854014686852901D+15/
  DATA b/ .732421875D-01,-.2271080017089844D+00,  &
      .1727727502584457D+01,-.2438052969955606D+02,  &
      .5513358961220206D+03,-.1825775547429318D+05,  &
      .8328593040162893D+06,-.5006958953198893D+08,  &
      .3836255180230433D+10,-.3649010818849833D+12,  &
      .4218971570284096D+14,-.5827244631566907D+16/
  DATA a1/.1171875D+00,-.144195556640625D+00,  &
      .6765925884246826D+00,-.6883914268109947D+01,  &
      .1215978918765359D+03,-.3302272294480852D+04,  &
      .1276412726461746D+06,-.6656367718817688D+07,  &
      .4502786003050393D+09,-.3833857520742790D+11,  &
      .4011838599133198D+13,-.5060568503314727D+15/
  DATA b1/-.1025390625D+00,.2775764465332031D+00,  &
      -.1993531733751297D+01,.2724882731126854D+02,  &
      -.6038440767050702D+03,.1971837591223663D+05,  &
      -.8902978767070678D+06,.5310411010968522D+08,  &
      -.4043620325107754D+10,.3827011346598605D+12,  &
      -.4406481417852278D+14,.6065091351222699D+16/
  k0=12
  IF (a0 >= 35.0) k0=10
  IF (a0 >= 50.0) k0=8
  ct1=z1-.25D0*pi
  cp0=(1.0D0,0.0D0)
  DO  k=1,k0
    cp0=cp0+a(k)*z1**(-2*k)
  END DO
  cq0=-0.125D0/z1
  DO  k=1,k0
    cq0=cq0+b(k)*z1**(-2*k-1)
  END DO
  cu=CDSQRT(rp2/z1)
  cbj0=cu*(cp0*CDCOS(ct1)-cq0*CDSIN(ct1))
  cby0=cu*(cp0*CDSIN(ct1)+cq0*CDCOS(ct1))
  ct2=z1-.75D0*pi
  cp1=(1.0D0,0.0D0)
  DO  k=1,k0
    cp1=cp1+a1(k)*z1**(-2*k)
  END DO
  cq1=0.375D0/z1
  DO  k=1,k0
    cq1=cq1+b1(k)*z1**(-2*k-1)
  END DO
  cbj1=cu*(cp1*CDCOS(ct2)-cq1*CDSIN(ct2))
  cby1=cu*(cp1*CDSIN(ct2)+cq1*CDCOS(ct2))
END IF
IF (REAL(z) < 0.0) THEN
  IF (DIMAG(z) < 0.0) cby0=cby0-2.0D0*ci*cbj0
  IF (DIMAG(z) > 0.0) cby0=cby0+2.0D0*ci*cbj0
  IF (DIMAG(z) < 0.0) cby1=-(cby1-2.0D0*ci*cbj1)
  IF (DIMAG(z) > 0.0) cby1=-(cby1+2.0D0*ci*cbj1)
  cbj1=-cbj1
END IF
cdj0=-cbj1
cdj1=cbj0-1.0D0/z*cbj1
cdy0=-cby1
cdy1=cby0-1.0D0/z*cby1
RETURN
END SUBROUTINE cjy01
