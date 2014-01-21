PROGRAM mcpsi
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:10

!     =========================================================
!     Purpose: This program computes the psi function psi(z)
!     for a complex argument using subroutine CPSI
!     Input :  x   --- Real part of z
!     y   --- Imaginary part of z
!     Output:  PSR --- Real part of psi(z)
!     PSI --- Imaginary part of psi(z)
!     Examples:
!     x       y      Re[psi(z)]     Im[psi(z)]
!     -------------------------------------------
!     3.0     2.0     1.16459152      .67080728
!     3.0    -2.0     1.16459152     -.67080728
!     -3.0     2.0     1.39536075     2.62465344
!     -3.0    -2.0     1.39536075    -2.62465344
!     =========================================================

DOUBLE PRECISION :: x,y,psr,psi
WRITE(*,*)'Please enter x and y ( z=x+iy)'
!     READ(*,*)X,Y
x=3.0
y=2.0
WRITE(*,20)x,y
WRITE(*,*)
WRITE(*,*)'   x       y      Re[Psi(z)]      Im[Psi(z)]'
WRITE(*,*)' ----------------------------------------------'
CALL cpsi(x,y,psr,psi)
WRITE(*,10)x,y,psr,psi
10   FORMAT(1X,f5.1,3X,f5.1,2X,2E16.8)
20   FORMAT(1X,2HX=,f6.2,6X,2HY=,f6.2)
END PROGRAM mcpsi


SUBROUTINE cpsi(x,y,psr,psi)

!     =============================================
!     Purpose: Compute the psi function for a
!     complex argument
!     Input :  x   --- Real part of z
!     y   --- Imaginary part of z
!     Output:  PSR --- Real part of psi(z)
!     PSI --- Imaginary part of psi(z)
!     =============================================


DOUBLE PRECISION, INTENT(IN OUT)         :: x
DOUBLE PRECISION, INTENT(IN OUT)         :: y
DOUBLE PRECISION, INTENT(OUT)            :: psr
DOUBLE PRECISION, INTENT(OUT)            :: psi
IMPLICIT DOUBLE PRECISION (a-h,o-z)
DIMENSION a(8)
DATA a/-.8333333333333D-01,.83333333333333333D-02,  &
    -.39682539682539683D-02,.41666666666666667D-02,  &
    -.75757575757575758D-02,.21092796092796093D-01,  &
    -.83333333333333333D-01,.4432598039215686D0/

x1=0.0
pi=3.141592653589793D0
IF (y == 0.0D0.AND.x == INT(x).AND.x <= 0.0D0) THEN
  psr=1.0D+300
  psi=0.0D0
ELSE
  IF (x < 0.0D0) THEN
    x1=x
    y1=y
    x=-x
    y=-y
  END IF
  x0=x
  IF (x < 8.0D0) THEN
    n=INT(8-INT(x))
    x0=x+n
  END IF
  IF (x0 == 0.0D0.AND.y /= 0.0D0) th=0.5D0*pi
  IF (x0 /= 0.0D0) th=DATAN(y/x0)
  z2=x0*x0+y*y
  z0=DSQRT(z2)
  psr=DLOG(z0)-0.5D0*x0/z2
  psi=th+0.5D0*y/z2
  DO  k=1,8
    psr=psr+a(k)*z2**(-k)*DCOS(2.0D0*k*th)
    psi=psi-a(k)*z2**(-k)*DSIN(2.0D0*k*th)
  END DO
  IF (x < 8.0D0) THEN
    rr=0.0D0
    ri=0.0D0
    DO  k=1,n
      rr=rr+(x0-k)/((x0-k)**2.0D0+y*y)
      ri=ri+y/((x0-k)**2.0D0+y*y)
    END DO
    psr=psr-rr
    psi=psi+ri
  END IF
  IF (x1 < 0.0D0) THEN
    tn=DTAN(pi*x)
    tm=DTANH(pi*y)
    ct2=tn*tn+tm*tm
    psr=psr+x/(x*x+y*y)+pi*(tn-tn*tm*tm)/ct2
    psi=psi-y/(x*x+y*y)-pi*tm*(1.0D0+tn*tn)/ct2
    x=x1
    y=y1
  END IF
END IF
RETURN
END SUBROUTINE cpsi
