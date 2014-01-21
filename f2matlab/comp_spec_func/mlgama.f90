PROGRAM mlgama
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:12

!       ===================================================
!       Purpose: This program computes the gamma function
!                â(x) for x > 0 using subroutine LGAMA
!       Examples:
!                  x           â(x)
!                -------------------------
!                 0.5     .1772453851D+01
!                 2.5     .1329340388D+01
!                 5.0     .2400000000D+02
!                 7.5     .1871254306D+04
!                10.0     .3628800000D+06
!       ===================================================

IMPLICIT DOUBLE PRECISION (g,x)
WRITE(*,*)'   x           â(x)'
WRITE(*,*)' -------------------------'
DO  l=0,20,5
  x=0.5D0*l
  IF (l == 0) x=0.5
  CALL lgama(1,x,gl)
  WRITE(*,20)x,gl
END DO
WRITE(*,*) 'Please enter x:'
!        READ(*,*) X
x=10.0
CALL lgama(1,x,gl)
WRITE(*,20)x,gl
20      FORMAT(1X,f5.1,d20.10)
END PROGRAM mlgama


SUBROUTINE lgama(kf,x,gl)

!       ==================================================
!       Purpose: Compute gamma function â(x) or ln[â(x)]
!       Input:   x  --- Argument of â(x) ( x > 0 )
!                KF --- Function code
!                       KF=1 for â(x); KF=0 for ln[â(x)]
!       Output:  GL --- â(x) or ln[â(x)]
!       ==================================================


INTEGER, INTENT(IN OUT)                  :: kf
DOUBLE PRECISION, INTENT(IN)             :: x
DOUBLE PRECISION, INTENT(OUT)            :: gl
IMPLICIT DOUBLE PRECISION (a-h,o-z)
DIMENSION a(10)
DATA a/8.333333333333333D-02,-2.777777777777778D-03,  &
    7.936507936507937D-04,-5.952380952380952D-04,  &
    8.417508417508418D-04,-1.917526917526918D-03,  &
    6.410256410256410D-03,-2.955065359477124D-02,  &
    1.796443723688307D-01,-1.39243221690590D+00/

x0=x
ifoo=1
IF (x == 1.0.OR.x == 2.0) THEN
  gl=0.0D0
  ifoo=0
ELSE IF (x <= 7.0) THEN
  n=INT(7-x)
  x0=x+n
END IF
IF (ifoo == 1) THEN
  x2=1.0D0/(x0*x0)
  xp=6.283185307179586477D0
  gl0=a(10)
  DO  k=9,1,-1
    gl0=gl0*x2+a(k)
  END DO
  gl=gl0/x0+0.5D0*DLOG(xp)+(x0-.5D0)*DLOG(x0)-x0
  IF (x <= 7.0) THEN
    DO  k=1,n
      gl=gl-DLOG(x0-1.0D0)
      x0=x0-1.0D0
    END DO
  END IF
END IF
20     IF (kf == 1) gl=DEXP(gl)
RETURN
END SUBROUTINE lgama
