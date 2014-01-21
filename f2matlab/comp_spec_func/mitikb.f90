PROGRAM mitikb
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:11

!       ============================================================
!       Purpose: This program evaluates the integral of modified
!                Bessel functions I0(t) and K0(t) with respect to t
!                from 0 to x using subroutine ITIKB
!       Input :  x  --- Upper limit of the integral  ( x ò 0 )
!       Output:  TI --- Integration of I0(t) from 0 to x
!                TK --- Integration of K0(t) from 0 to x
!       Example:
!                    x         I0(t)dt         K0(t)dt
!                 -------------------------------------
!                   5.0     .318487D+02       1.567387
!                  10.0     .299305D+04       1.570779
!                  15.0     .352619D+06       1.570796
!                  20.0     .447586D+08       1.570796
!                  25.0     .589919D+10       1.570796
!       ============================================================

IMPLICIT DOUBLE PRECISION (a-h,o-z)
WRITE(*,*)'Please enter x '
!        READ(*,*)X
x=25.0
WRITE(*,*)'   x         I0(t)dt         K0(t)dt'
WRITE(*,*)'--------------------------------------'
CALL itikb(x,ti,tk)
WRITE(*,10)x,ti,tk
10      FORMAT(1X,f5.1,d16.6,f15.6)
END PROGRAM mitikb


SUBROUTINE itikb(x,ti,tk)

!       =======================================================
!       Purpose: Integrate Bessel functions I0(t) and K0(t)
!                with respect to t from 0 to x
!       Input :  x  --- Upper limit of the integral ( x ò 0 )
!       Output:  TI --- Integration of I0(t) from 0 to x
!                TK --- Integration of K0(t) from 0 to x
!       =======================================================



DOUBLE PRECISION, INTENT(IN)             :: x
DOUBLE PRECISION, INTENT(OUT)            :: ti
DOUBLE PRECISION, INTENT(OUT)            :: tk
IMPLICIT DOUBLE PRECISION (a-h,o-z)
pi=3.141592653589793D0
IF (x == 0.0D0) THEN
  ti=0.0D0
ELSE IF (x < 5.0D0) THEN
  t1=x/5.0D0
  t=t1*t1
  ti=((((((((.59434D-3*t+.4500642D-2)*t  &
      +.044686921D0)*t+.300704878D0)*t+1.471860153D0)  &
      *t+4.844024624D0)*t+9.765629849D0)*t +10.416666367D0)*t+5.0D0)*t1
ELSE IF (x >= 5.0.AND.x <= 8.0D0) THEN
  t=5.0D0/x
  ti=(((-.015166D0*t-.0202292D0)*t+.1294122D0)*t -.0302912D0)*t+.4161224D0
  ti=ti*DEXP(x)/DSQRT(x)
ELSE
  t=8.0D0/x
  ti=(((((-.0073995D0*t+.017744D0)*t-.0114858D0)*t  &
      +.55956D-2)*t+.59191D-2)*t+.0311734D0)*t +.3989423D0
  ti=ti*DEXP(x)/DSQRT(x)
END IF
IF (x == 0.0D0) THEN
  tk=0.0D0
ELSE IF (x <= 2.0D0) THEN
  t1=x/2.0D0
  t=t1*t1
  tk=((((((.116D-5*t+.2069D-4)*t+.62664D-3)*t  &
      +.01110118D0)*t+.11227902D0)*t+.50407836D0)*t +.84556868D0)*t1
  tk=tk-DLOG(x/2.0D0)*ti
ELSE IF (x > 2.0.AND.x <= 4.0D0) THEN
  t=2.0D0/x
  tk=(((.0160395D0*t-.0781715D0)*t+.185984D0)*t -.3584641D0)*t+1.2494934D0
  tk=pi/2.0D0-tk*DEXP(-x)/DSQRT(x)
ELSE IF (x > 4.0.AND.x <= 7.0D0) THEN
  t=4.0D0/x
  tk=(((((.37128D-2*t-.0158449D0)*t+.0320504D0)*t  &
      -.0481455D0)*t+.0787284D0)*t-.1958273D0)*t +1.2533141D0
  tk=pi/2.0D0-tk*DEXP(-x)/DSQRT(x)
ELSE
  t=7.0D0/x
  tk=(((((.33934D-3*t-.163271D-2)*t+.417454D-2)*t  &
      -.933944D-2)*t+.02576646D0)*t-.11190289D0)*t +1.25331414D0
  tk=pi/2.0D0-tk*DEXP(-x)/DSQRT(x)
END IF
RETURN
END SUBROUTINE itikb
