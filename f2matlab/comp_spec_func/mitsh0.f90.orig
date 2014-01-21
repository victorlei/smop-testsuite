PROGRAM mitsh0
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:11

!       ====================================================
!       Purpose: This program evaluates the integral of
!                Struve function H0(t) with respect to t
!                from 0 and x using subroutine ITSH0
!       Input :  x   --- Upper limit  ( x ò 0 )
!       Output:  TH0 --- Integration of H0(t) from 0 and x
!       Example:
!                    x        H0(t)dt
!                 ----------------------
!                   0.0       .0000000
!                   5.0      2.0442437
!                  10.0      2.5189577
!                  15.0      2.5415824
!                  20.0      2.5484517
!                  30.0      3.0625848
!                  40.0      3.1484123
!                  50.0      3.2445168
!       ====================================================

IMPLICIT DOUBLE PRECISION (a-h,o-z)
WRITE(*,*)'Please enter x '
!        READ(*,*)X
x=50.0
WRITE(*,*)'   x         H0(t)dt'
WRITE(*,*)'----------------------'
CALL itsh0(x,th0)
WRITE(*,10)x,th0
10      FORMAT(1X,f5.1,e16.7)
END PROGRAM mitsh0


SUBROUTINE itsh0(x,th0)

!       ===================================================
!       Purpose: Evaluate the integral of Struve function
!                H0(t) with respect to t from 0 and x
!       Input :  x   --- Upper limit  ( x ò 0 )
!       Output:  TH0 --- Integration of H0(t) from 0 and x
!       ===================================================


DOUBLE PRECISION, INTENT(IN)             :: x
DOUBLE PRECISION, INTENT(OUT)            :: th0
IMPLICIT DOUBLE PRECISION (a-h,o-z)
DIMENSION a(25)

pi=3.141592653589793D0
r=1.0D0
IF (x <= 30.0) THEN
  s=0.5D0
  DO  k=1,100
    rd=1.0D0
    IF (k == 1) rd=0.5D0
    r=-r*rd*k/(k+1.0D0)*(x/(2.0D0*k+1.0D0))**2
    s=s+r
    IF (DABS(r) < DABS(s)*1.0D-12) EXIT
  END DO
  15         th0=2.0D0/pi*x*x*s
ELSE
  s=1.0D0
  DO  k=1,12
    r=-r*k/(k+1.0D0)*((2.0D0*k+1.0D0)/x)**2
    s=s+r
    IF (DABS(r) < DABS(s)*1.0D-12) EXIT
  END DO
  25         el=.57721566490153D0
  s0=s/(pi*x*x)+2.0D0/pi*(DLOG(2.0D0*x)+el)
  a0=1.0D0
  a1=5.0D0/8.0D0
  a(1)=a1
  DO  k=1,20
    af=((1.5D0*(k+.5D0)*(k+5.0D0/6.0D0)*a1-.5D0  &
        *(k+.5D0)*(k+.5D0)*(k-.5D0)*a0))/(k+1.0D0)
    a(k+1)=af
    a0=a1
    a1=af
  END DO
  bf=1.0D0
  r=1.0D0
  DO  k=1,10
    r=-r/(x*x)
    bf=bf+a(2*k)*r
  END DO
  bg=a(1)/x
  r=1.0D0/x
  DO  k=1,10
    r=-r/(x*x)
    bg=bg+a(2*k+1)*r
  END DO
  xp=x+.25D0*pi
  ty=DSQRT(2.0D0/(pi*x))*(bg*DCOS(xp)-bf*DSIN(xp))
  th0=ty+s0
END IF
RETURN
END SUBROUTINE itsh0
