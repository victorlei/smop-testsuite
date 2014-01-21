PROGRAM mitika
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:11

!       ============================================================
!       Purpose: This program evaluates the integral of modified
!                Bessel functions I0(t) and K0(t) with respect to t
!                from 0 to x using subroutine ITIKA
!       Input :  x  --- Upper limit of the integral  ( x ò 0 )
!       Output:  TI --- Integration of I0(t) from 0 to x
!                TK --- Integration of K0(t) from 0 to x
!       Example:
!                    x         I0(t)dt         K0(t)dt
!                 --------------------------------------
!                   5.0    .31848668D+02     1.56738739
!                  10.0    .29930445D+04     1.57077931
!                  15.0    .35262048D+06     1.57079623
!                  20.0    .44758593D+08     1.57079633
!                  25.0    .58991731D+10     1.57079633
!       ============================================================

IMPLICIT DOUBLE PRECISION (a-h,o-z)
WRITE(*,*)'Please enter x '
!        READ(*,*)X
x=25.0
WRITE(*,*)'   x         I0(t)dt         K0(t)dt'
WRITE(*,*)' --------------------------------------'
CALL itika(x,ti,tk)
WRITE(*,10)x,ti,tk
10      FORMAT(1X,f5.1,d17.8,f15.8)
END PROGRAM mitika


SUBROUTINE itika(x,ti,tk)

!       =======================================================
!       Purpose: Integrate modified Bessel functions I0(t) and
!                K0(t) with respect to t from 0 to x
!       Input :  x  --- Upper limit of the integral  ( x ò 0 )
!       Output:  TI --- Integration of I0(t) from 0 to x
!                TK --- Integration of K0(t) from 0 to x
!       =======================================================


DOUBLE PRECISION, INTENT(IN)             :: x
DOUBLE PRECISION, INTENT(OUT)            :: ti
DOUBLE PRECISION, INTENT(OUT)            :: tk
IMPLICIT DOUBLE PRECISION (a-h,o-z)
DIMENSION a(10)

pi=3.141592653589793D0
el=.5772156649015329D0
DATA a/.625D0,1.0078125D0, 2.5927734375D0,9.1868591308594D0,  &
    4.1567974090576D+1,2.2919635891914D+2,  &
    1.491504060477D+3,1.1192354495579D+4, 9.515939374212D+4,9.0412425769041D+5/
IF (x == 0.0D0) THEN
  ti=0.0D0
  tk=0.0D0
  RETURN
ELSE IF (x < 20.0D0) THEN
  x2=x*x
  ti=1.0D0
  r=1.0D0
  DO  k=1,50
    r=.25D0*r*(2*k-1.0D0)/(2*k+1.0D0)/(k*k)*x2
    ti=ti+r
    IF (DABS(r/ti) < 1.0D-12) EXIT
  END DO
  15         ti=ti*x
ELSE
  ti=1.0D0
  r=1.0D0
  DO  k=1,10
    r=r/x
    ti=ti+a(k)*r
  END DO
  rc1=1.0D0/DSQRT(2.0D0*pi*x)
  ti=rc1*DEXP(x)*ti
END IF
IF (x < 12.0D0) THEN
  e0=el+DLOG(x/2.0D0)
  b1=1.0D0-e0
  b2=0.0D0
  rs=0.0D0
  r=1.0D0
  DO  k=1,50
    r=.25D0*r*(2*k-1.0D0)/(2*k+1.0D0)/(k*k)*x2
    b1=b1+r*(1.0D0/(2*k+1)-e0)
    rs=rs+1.0D0/k
    b2=b2+r*rs
    tk=b1+b2
    IF (DABS((tk-tw)/tk) < 1.0D-12) EXIT
    tw=tk
  END DO
  30         tk=tk*x
ELSE
  tk=1.0D0
  r=1.0D0
  DO  k=1,10
    r=-r/x
    tk=tk+a(k)*r
  END DO
  rc2=DSQRT(pi/(2.0D0*x))
  tk=pi/2.0D0-rc2*tk*DEXP(-x)
END IF
RETURN
END SUBROUTINE itika
