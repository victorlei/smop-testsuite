PROGRAM mcerror
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:10

!       ============================================================
!       Purpose: This program computes the error function erf(z)
!                for a complex argument using subroutine CERROR
!       Input :  x   --- Real part of z
!                y   --- Imaginary part of z  ( y ó 3.0 )
!       Output:  ERR --- Real part of erf(z)
!                ERI --- Imaginary part of erf(z)
!       Example:
!                   x       y       Re[erf(z)]      Im[erf(z)]
!                 ---------------------------------------------
!                  1.0     2.0      -.53664357     -5.04914370
!                  2.0     2.0      1.15131087       .12729163
!                  3.0     2.0       .99896328      -.00001155
!                  4.0     2.0      1.00000057      -.00000051
!                  5.0     2.0      1.00000000       .00000000
!       ============================================================

IMPLICIT COMPLEX *16 (c,z)
DOUBLE PRECISION :: x,y
WRITE(*,*)'X,Y=?'
! READ(*,*)X,Y
x=2.0
y=2.0
WRITE(*,*)'   x      y      Re[erf(z)]      Im[erf(z)]'
WRITE(*,*)' ---------------------------------------------'
z=CMPLX(x,y)
CALL cerror(z,cer)
WRITE(*,10) z,cer
WRITE(*,*) z,cer
10      FORMAT(1X,f5.1,2X,f5.1,1X,2E16.8)
END PROGRAM mcerror


SUBROUTINE cerror(z,cer)

!       ====================================================
!       Purpose: Compute error function erf(z) for a complex
!                argument (z=x+iy)
!       Input :  z   --- Complex argument
!       Output:  CER --- erf(z)
!       ====================================================


COMPLEX, INTENT(IN)                      :: z
COMPLEX, INTENT(OUT)                     :: cer
IMPLICIT COMPLEX *16 (c,z)
DOUBLE PRECISION :: a0,pi

a0=CDABS(z)
c0=CDEXP(-z*z)
pi=3.141592653589793D0
z1=z
IF (REAL(z) < 0.0) THEN
  z1=-z
END IF
IF (a0 <= 5.8D0) THEN
  cs=z1
  cr=z1
  DO  k=1,120
    cr=cr*z1*z1/(k+0.5D0)
    cs=cs+cr
    IF (CDABS(cr/cs) < 1.0D-15) EXIT
  END DO
  15         cer=2.0D0*c0*cs/DSQRT(pi)
ELSE
  cl=1.0D0/z1
  cr=cl
  DO  k=1,13
    cr=-cr*(k-0.5D0)/(z1*z1)
    cl=cl+cr
    IF (CDABS(cr/cl) < 1.0D-15) EXIT
  END DO
  25         cer=1.0D0-c0*cl/DSQRT(pi)
END IF
IF (REAL(z) < 0.0) THEN
  cer=-cer
END IF
RETURN
END SUBROUTINE cerror


