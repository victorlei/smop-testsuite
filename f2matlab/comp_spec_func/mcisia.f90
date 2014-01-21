PROGRAM mcisia
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:10

!     ========================================================
!     Purpose: This program computes the cosine and sine
!     integrals using subroutine CISIA
!     Input :  x  --- Argument of Ci(x) and Si(x)
!     Output:  CI --- Ci(x)
!     SI --- Si(x)
!     Example:
!     x         Ci(x)          Si(x)
!     ------------------------------------
!     0.0     - ì             .00000000
!     5.0     -.19002975     1.54993124
!     10.0     -.04545643     1.65834759
!     20.0      .04441982     1.54824170
!     30.0     -.03303242     1.56675654
!     40.0      .01902001     1.58698512
!     ========================================================

DOUBLE PRECISION :: ci,si,x
WRITE(*,*)'Please enter x '
!     READ(*,*) X
x=5.0
WRITE(*,*)'   x         Ci(x)          Si(x)'
WRITE(*,*)'------------------------------------'
CALL cisia(x,ci,si)
IF (x /= 0.0D0) WRITE(*,10)x,ci,si
IF (x == 0.0D0) WRITE(*,20)
10   FORMAT(1X,f5.1,2F15.8)
20   FORMAT(3X,' .0',4X,' - i',13X,'.00000000')
END PROGRAM mcisia



SUBROUTINE cisia(x,ci,si)

!     =============================================
!     Purpose: Compute cosine and sine integrals
!     Si(x) and Ci(x)  ( x ò 0 )
!     Input :  x  --- Argument of Ci(x) and Si(x)
!     Output:  CI --- Ci(x)
!     SI --- Si(x)
!     =============================================


DOUBLE PRECISION, INTENT(IN)             :: x
DOUBLE PRECISION, INTENT(OUT)            :: ci
DOUBLE PRECISION, INTENT(OUT)            :: si
IMPLICIT DOUBLE PRECISION (a-h,o-z)
DIMENSION bj(101)

p2=1.570796326794897D0
el=.5772156649015329D0
eps=1.0D-15
x2=x*x
IF (x == 0.0D0) THEN
  ci=-1.0D+300
  si=0.0D0
ELSE IF (x <= 16.0D0) THEN
  xr=-.25D0*x2
  ci=el+DLOG(x)+xr
  DO  k=2,40
    xr=-.5D0*xr*(k-1)/(k*k*(2*k-1))*x2
    ci=ci+xr
    IF (DABS(xr) < DABS(ci)*eps) EXIT
  END DO
  15    xr=x
  si=x
  DO  k=1,40
    xr=-.5D0*xr*(2*k-1)/k/(4*k*k+4*k+1)*x2
    si=si+xr
    IF (DABS(xr) < DABS(si)*eps) RETURN
  END DO
ELSE IF (x <= 32.0D0) THEN
  m=INT(47.2+.82*x)
  xa1=0.0D0
  xa0=1.0D-100
  DO  k=m,1,-1
    xa=4.0D0*k*xa0/x-xa1
    bj(k)=xa
    xa1=xa0
    xa0=xa
  END DO
  xs=bj(1)
  DO  k=3,m,2
    xs=xs+2.0D0*bj(k)
  END DO
  bj(1)=bj(1)/xs
  DO  k=2,m
    bj(k)=bj(k)/xs
  END DO
  xr=1.0D0
  xg1=bj(1)
  DO  k=2,m
    xr=.25D0*xr*(2.0*k-3.0)**2/((k-1.0)*(2.0*k-1.0)**2)*x
    xg1=xg1+bj(k)*xr
  END DO
  xr=1.0D0
  xg2=bj(1)
  DO  k=2,m
    xr=.25D0*xr*(2.0*k-5.0)**2/((k-1.0)*(2.0*k-3.0)**2)*x
    xg2=xg2+bj(k)*xr
  END DO
  xcs=DCOS(x/2.0D0)
  xss=DSIN(x/2.0D0)
  ci=el+DLOG(x)-x*xss*xg1+2*xcs*xg2-2*xcs*xcs
  si=x*xcs*xg1+2*xss*xg2-DSIN(x)
ELSE
  xr=1.0D0
  xf=1.0D0
  DO  k=1,9
    xr=-2.0D0*xr*k*(2*k-1)/x2
    xf=xf+xr
  END DO
  xr=1.0D0/x
  xg=xr
  DO  k=1,8
    xr=-2.0D0*xr*(2*k+1)*k/x2
    xg=xg+xr
  END DO
  ci=xf*DSIN(x)/x-xg*DCOS(x)/x
  si=p2-xf*DCOS(x)/x-xg*DSIN(x)/x
END IF
RETURN
END SUBROUTINE cisia

