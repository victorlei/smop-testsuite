PROGRAM mittika
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:11

!       ============================================================
!       Purpose: This program computes the integral of [I0(t)-1]/t
!                with respect to t from 0 to x and K0(t)/t with
!                respect to t from x to ì using subroutine ITTIKA
!       Input :  x   --- Variable in the limits  ( x ò 0 )
!       Output:  TTI --- Integration of [I0(t)-1]/t from 0 to x
!                TTK --- Integration of K0(t)/t from x to ì
!       Example:
!                   x     [1-I0(t)]/tdt     K0(t)/tdt
!                ---------------------------------------
!                  5.0   .71047763D+01   .58635626D-03
!                 10.0   .34081537D+03   .15629282D-05
!                 15.0   .25437619D+05   .59837472D-08
!                 20.0   .23673661D+07   .26790545D-10
!                 25.0   .24652751D+09   .13100706D-12
!       ============================================================

DOUBLE PRECISION :: x,tti,ttk
WRITE(*,*)'Please enter x '
!        READ(*,*)X
x=25.0
WRITE(*,*)'   x     [1-I0(t)]/tdt     K0(t)/tdt'
WRITE(*,*)'---------------------------------------'
CALL ittika(x,tti,ttk)
WRITE(*,10)x,tti,ttk
10      FORMAT(1X,f5.1,2D16.8)
END PROGRAM mittika


SUBROUTINE ittika(x,tti,ttk)

!       =========================================================
!       Purpose: Integrate [I0(t)-1]/t with respect to t from 0
!                to x, and K0(t)/t with respect to t from x to ì
!       Input :  x   --- Variable in the limits  ( x ò 0 )
!       Output:  TTI --- Integration of [I0(t)-1]/t from 0 to x
!                TTK --- Integration of K0(t)/t from x to ì
!       =========================================================


DOUBLE PRECISION, INTENT(IN)             :: x
DOUBLE PRECISION, INTENT(OUT)            :: tti
DOUBLE PRECISION, INTENT(OUT)            :: ttk
IMPLICIT DOUBLE PRECISION (a-h,o-z)
DIMENSION c(8)

pi=3.141592653589793D0
el=.5772156649015329D0
DATA c/1.625D0,4.1328125D0, 1.45380859375D+1,6.553353881835D+1,  &
    3.6066157150269D+2,2.3448727161884D+3, 1.7588273098916D+4,1.4950639538279D+5/
IF (x == 0.0D0) THEN
  tti=0.0D0
  ttk=1.0D+300
  RETURN
END IF
IF (x < 40.0D0) THEN
  tti=1.0D0
  r=1.0D0
  DO  k=2,50
    r=.25D0*r*(k-1.0D0)/(k*k*k)*x*x
    tti=tti+r
    IF (DABS(r/tti) < 1.0D-12) EXIT
  END DO
  15         tti=tti*.125D0*x*x
ELSE
  tti=1.0D0
  r=1.0D0
  DO  k=1,8
    r=r/x
    tti=tti+c(k)*r
  END DO
  rc=x*DSQRT(2.0D0*pi*x)
  tti=tti*DEXP(x)/rc
END IF
IF (x <= 12.0D0) THEN
  e0=(.5D0*DLOG(x/2.0D0)+el)*DLOG(x/2.0D0) +pi*pi/24.0D0+.5D0*el*el
  b1=1.5D0-(el+DLOG(x/2.0D0))
  rs=1.0D0
  r=1.0D0
  DO  k=2,50
    r=.25D0*r*(k-1.0D0)/(k*k*k)*x*x
    rs=rs+1.0D0/k
    r2=r*(rs+1.0D0/(2.0D0*k)-(el+DLOG(x/2.0D0)))
    b1=b1+r2
    IF (DABS(r2/b1) < 1.0D-12) EXIT
  END DO
  30         ttk=e0-.125D0*x*x*b1
ELSE
  ttk=1.0D0
  r=1.0D0
  DO  k=1,8
    r=-r/x
    ttk=ttk+c(k)*r
  END DO
  rc=x*DSQRT(2.0D0/pi*x)
  ttk=ttk*DEXP(-x)/rc
END IF
RETURN
END SUBROUTINE ittika
