PROGRAM mittjya
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:11

!       ===========================================================
!       Purpose: This program computes the integral of [1-J0(t)]/t
!                with respect to t from 0 to x and Y0(t)/t with
!                respect to t from x to ì using subroutine ITTJYA
!       Input :  x   --- Variable in the limits  ( x ò 0 )
!       Output:  TTJ --- Integration of [1-J0(t)]/t from 0 to x
!                TTY --- Integration of Y0(t)/t from x to ì
!       Example:
!                  x       [1-J0(t)]/tdt       Y0(t)/tdt
!               -------------------------------------------
!                 5.0     .15403472D+01    -.46322055D-01
!                10.0     .21778664D+01    -.22987934D-01
!                15.0     .25785507D+01     .38573574D-03
!                20.0     .28773106D+01     .85031527D-02
!                25.0     .31082313D+01     .35263393D-02
!       ===========================================================

DOUBLE PRECISION :: x,ttj,tty
WRITE(*,*)'Please enter x '
!        READ(*,*)X
x=25.0
WRITE(*,*)'   x      [1-J0(t)]/tdt       Y0(t)/tdt'
WRITE(*,*)'-------------------------------------------'
CALL ittjya(x,ttj,tty)
WRITE(*,10)x,ttj,tty
10      FORMAT(1X,f5.1,2D18.8)
END PROGRAM mittjya


SUBROUTINE ittjya(x,ttj,tty)

!       =========================================================
!       Purpose: Integrate [1-J0(t)]/t with respect to t from 0
!                to x, and Y0(t)/t with respect to t from x to ì
!       Input :  x   --- Variable in the limits  ( x ò 0 )
!       Output:  TTJ --- Integration of [1-J0(t)]/t from 0 to x
!                TTY --- Integration of Y0(t)/t from x to ì
!       =========================================================



DOUBLE PRECISION, INTENT(IN)             :: x
DOUBLE PRECISION, INTENT(OUT)            :: ttj
DOUBLE PRECISION, INTENT(OUT)            :: tty
IMPLICIT DOUBLE PRECISION (a-h,o-z)
pi=3.141592653589793D0
el=.5772156649015329D0
IF (x == 0.0D0) THEN
  ttj=0.0D0
  tty=-1.0D+300
ELSE IF (x <= 20.0D0) THEN
  ttj=1.0D0
  r=1.0D0
  DO  k=2,100
    r=-.25D0*r*(k-1.0D0)/(k*k*k)*x*x
    ttj=ttj+r
    IF (DABS(r) < DABS(ttj)*1.0D-12) EXIT
  END DO
  15         ttj=ttj*.125D0*x*x
  e0=.5D0*(pi*pi/6.0D0-el*el)-(.5D0*DLOG(x/2.0D0)+el) *DLOG(x/2.0D0)
  b1=el+DLOG(x/2.0D0)-1.5D0
  rs=1.0D0
  r=-1.0D0
  DO  k=2,100
    r=-.25D0*r*(k-1.0D0)/(k*k*k)*x*x
    rs=rs+1.0D0/k
    r2=r*(rs+1.0D0/(2.0D0*k)-(el+DLOG(x/2.0D0)))
    b1=b1+r2
    IF (DABS(r2) < DABS(b1)*1.0D-12) EXIT
  END DO
  25         tty=2.0D0/pi*(e0+.125D0*x*x*b1)
ELSE
  a0=DSQRT(2.0D0/(pi*x))
  DO  l=0,1
    vt=4.0D0*l*l
    px=1.0D0
    r=1.0D0
    DO  k=1,14
      r=-.0078125D0*r*(vt-(4.0D0*k-3.0D0)**2)  &
          /(x*k)*(vt-(4.0D0*k-1.0D0)**2) /((2.0D0*k-1.0D0)*x)
      px=px+r
      IF (DABS(r) < DABS(px)*1.0D-12) EXIT
    END DO
    35            qx=1.0D0
    r=1.0D0
    DO  k=1,14
      r=-.0078125D0*r*(vt-(4.0D0*k-1.0D0)**2)  &
          /(x*k)*(vt-(4.0D0*k+1.0D0)**2) /(2.0D0*k+1.0D0)/x
      qx=qx+r
      IF (DABS(r) < DABS(qx)*1.0D-12) EXIT
    END DO
    45            qx=.125D0*(vt-1.0D0)/x*qx
    xk=x-(.25D0+.5D0*l)*pi
    bj1=a0*(px*DCOS(xk)-qx*DSIN(xk))
    by1=a0*(px*DSIN(xk)+qx*DCOS(xk))
    IF (l == 0) THEN
      bj0=bj1
      by0=by1
    END IF
  END DO
  t=2.0D0/x
  g0=1.0D0
  r0=1.0D0
  DO  k=1,10
    r0=-k*k*t*t*r0
    g0=g0+r0
  END DO
  g1=1.0D0
  r1=1.0D0
  DO  k=1,10
    r1=-k*(k+1.0D0)*t*t*r1
    g1=g1+r1
  END DO
  ttj=2.0D0*g1*bj0/(x*x)-g0*bj1/x+el+DLOG(x/2.0D0)
  tty=2.0D0*g1*by0/(x*x)-g0*by1/x
END IF
RETURN
END SUBROUTINE ittjya
