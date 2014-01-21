PROGRAM mitjya
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:11

!       ===========================================================
!       Purpose: This program evaluates the integral of Bessel
!                functions J0(t) and Y0(t) with respect to t
!                from 0 to x using subroutine ITJYA
!       Input :  x  --- Upper limit of the integral ( x ò 0 )
!       Output:  TJ --- Integration of J0(t) from 0 to x
!                TY --- Integration of Y0(t) from 0 to x
!       Example:
!                   x         J0(t)dt          Y0(t)dt
!                ---------------------------------------
!                  5.0       .71531192       .19971938
!                 10.0      1.06701130       .24129032
!                 15.0      1.20516194       .00745772
!                 20.0      1.05837882      -.16821598
!                 25.0       .87101492      -.09360793
!                 30.0       .88424909       .08822971
!       ===========================================================

DOUBLE PRECISION :: x,tj,ty
WRITE(*,*)'Please enter x '
!        READ(*,*)X
x=30.0
WRITE(*,*)'   x         J0(t)dt          Y0(t)dt'
WRITE(*,*)'---------------------------------------'
CALL itjya(x,tj,ty)
WRITE(*,10)x,tj,ty
10      FORMAT(1X,f5.1,2F16.8)
END PROGRAM mitjya


SUBROUTINE itjya(x,tj,ty)

!       ==========================================================
!       Purpose: Integrate Bessel functions J0(t) & Y0(t) with
!                respect to t from 0 to x
!       Input :  x  --- Upper limit of the integral ( x ò 0 )
!       Output:  TJ --- Integration of J0(t) from 0 to x
!                TY --- Integration of Y0(t) from 0 to x
!       =======================================================


DOUBLE PRECISION, INTENT(IN)             :: x
DOUBLE PRECISION, INTENT(OUT)            :: tj
DOUBLE PRECISION, INTENT(OUT)            :: ty
IMPLICIT DOUBLE PRECISION (a-h,o-z)
DIMENSION a(18)

pi=3.141592653589793D0
el=.5772156649015329D0
eps=1.0D-12
IF (x == 0.0D0) THEN
  tj=0.0D0
  ty=0.0D0
ELSE IF (x <= 20.0D0) THEN
  x2=x*x
  tj=x
  r=x
  DO  k=1,60
    r=-.25D0*r*(2*k-1.0D0)/(2*k+1.0D0)/(k*k)*x2
    tj=tj+r
    IF (DABS(r) < DABS(tj)*eps) EXIT
  END DO
  15         ty1=(el+DLOG(x/2.0D0))*tj
  rs=0.0D0
  ty2=1.0D0
  r=1.0D0
  DO  k=1,60
    r=-.25D0*r*(2*k-1.0D0)/(2*k+1.0D0)/(k*k)*x2
    rs=rs+1.0D0/k
    r2=r*(rs+1.0D0/(2.0D0*k+1.0D0))
    ty2=ty2+r2
    IF (DABS(r2) < DABS(ty2)*eps) EXIT
  END DO
  25         ty=(ty1-x*ty2)*2.0D0/pi
ELSE
  a0=1.0D0
  a1=5.0D0/8.0D0
  a(1)=a1
  DO  k=1,16
    af=((1.5D0*(k+.5D0)*(k+5.0D0/6.0D0)*a1-.5D0  &
        *(k+.5D0)*(k+.5D0)*(k-.5D0)*a0))/(k+1.0D0)
    a(k+1)=af
    a0=a1
    a1=af
  END DO
  bf=1.0D0
  r=1.0D0
  DO  k=1,8
    r=-r/(x*x)
    bf=bf+a(2*k)*r
  END DO
  bg=a(1)/x
  r=1.0D0/x
  DO  k=1,8
    r=-r/(x*x)
    bg=bg+a(2*k+1)*r
  END DO
  xp=x+.25D0*pi
  rc=DSQRT(2.0D0/(pi*x))
  tj=1.0D0-rc*(bf*DCOS(xp)+bg*DSIN(xp))
  ty=rc*(bg*DCOS(xp)-bf*DSIN(xp))
END IF
RETURN
END SUBROUTINE itjya
