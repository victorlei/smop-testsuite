PROGRAM me1z
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:10

!       =========================================================
!       Purpose: This program computes the complex exponential
!                integral E1(z) using subroutine E1Z
!       Example:
!                     z            Re[E1(z)]       Im[E1(z)]
!                -----------------------------------------------
!                 3.0    2.0    -.90959209D-02   -.69001793D-02
!                 3.0   -2.0    -.90959209D-02    .69001793D-02
!                -3.0    2.0    -.28074891D+01    .59603353D+01
!                -3.0   -2.0    -.28074891D+01   -.59603353D+01
!                25.0   10.0    -.29302080D-12    .40391222D-12
!                25.0  -10.0    -.29302080D-12   -.40391222D-12
!               -25.0   10.0     .27279957D+10   -.49430610D+09
!               -25.0  -10.0     .27279957D+10    .49430610D+09
!       =========================================================

IMPLICIT COMPLEX*16 (c,z)
IMPLICIT DOUBLE PRECISION (d-h,o-y)
WRITE(*,*)'Please enter x and y ( z =x+iy ) '
!        READ(*,*)X,Y
x=3.0
y=2.0
z=CMPLX(x,y)
CALL e1z(z,ce1)
WRITE(*,*)
WRITE(*,*)'       z           Re[E1(z)]        Im[E1(z)]'
WRITE(*,*)' -----------------------------------------------'
WRITE(*,10)x,y,ce1
10      FORMAT(1X,f5.1,2X,f5.1,1X,2D17.8)
END PROGRAM me1z


SUBROUTINE e1z(z,ce1)

!       ====================================================
!       Purpose: Compute complex exponential integral E1(z)
!       Input :  z   --- Argument of E1(z)
!       Output:  CE1 --- E1(z)
!       ====================================================


COMPLEX, INTENT(IN)                      :: z
COMPLEX, INTENT(OUT)                     :: ce1
IMPLICIT COMPLEX*16 (c,z)
IMPLICIT DOUBLE PRECISION (d-h,o-y)

pi=3.141592653589793D0
el=0.5772156649015328D0
x=REAL(z)
a0=CDABS(z)
IF (a0 == 0.0D0) THEN
  ce1=(1.0D+300,0.0D0)
ELSE IF (a0 <= 10.0.OR.x < 0.0.AND.a0 < 20.0) THEN
  ce1=(1.0D0,0.0D0)
  cr=(1.0D0,0.0D0)
  DO  k=1,150
    cr=-cr*k*z/(k+1.0D0)**2
    ce1=ce1+cr
    IF (CDABS(cr) <= CDABS(ce1)*1.0D-15) EXIT
  END DO
  15         ce1=-el-CDLOG(z)+z*ce1
ELSE
  ct0=(0.0D0,0.0D0)
  DO  k=120,1,-1
    ct0=k/(1.0D0+k/(z+ct0))
  END DO
  ct=1.0D0/(z+ct0)
  ce1=CDEXP(-z)*ct
  IF (x <= 0.0.AND.DIMAG(z) == 0.0) ce1=ce1-pi*(0.0D0,1.0D0)
END IF
RETURN
END SUBROUTINE e1z
