PROGRAM mgamma
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:11

!       ====================================================
!       Purpose: This program computes the gamma function
!                ג(x) using subroutine GAMMA
!       Examples:
!                   x            ג(x)
!                ----------------------------
!                  1/3       2.678938534708
!                  0.5       1.772453850906
!                 -0.5      -3.544907701811
!                 -1.5       2.363271801207
!                  5.0      24.000000000000
!       ====================================================

DOUBLE PRECISION :: a,x,ga
DIMENSION a(5)
DATA a/.333333333333333333D0,0.5D0,-0.5D0,-1.5,5.0D0/
WRITE(*,*)'     x            ג(x)'
WRITE(*,*)' ----------------------------'
DO  k=1,5
  x=a(k)
  CALL gamma(x,ga)
  WRITE(*,20)x,ga
END DO
WRITE(*,*) 'Please enter x:'
!        READ(*,*) X
x=.5
CALL gamma(x,ga)
WRITE(*,20)x,ga
20      FORMAT(1X,f8.4,e20.12)
END PROGRAM mgamma


SUBROUTINE gamma(x,ga)

!       ==================================================
!       Purpose: Compute the gamma function ג(x)
!       Input :  x  --- Argument of ג(x)
!                       ( x is not equal to 0,-1,-2,תתת )
!       Output:  GA --- ג(x)
!       ==================================================


DOUBLE PRECISION, INTENT(IN)             :: x
DOUBLE PRECISION, INTENT(OUT)            :: ga
IMPLICIT DOUBLE PRECISION (a-h,o-z)
DIMENSION g(26)

pi=3.141592653589793D0
IF (x == INT(x)) THEN
  IF (x > 0.0D0) THEN
    ga=1.0D0
    m1=x-1
    DO  k=2,m1
      ga=ga*k
    END DO
  ELSE
    ga=1.0D+300
  END IF
ELSE
  IF (DABS(x) > 1.0D0) THEN
    z=DABS(x)
    m=INT(z)
    r=1.0D0
    DO  k=1,m
      r=r*(z-k)
    END DO
    z=z-m
  ELSE
    z=x
  END IF
  DATA g/1.0D0,0.5772156649015329D0,  &
      -0.6558780715202538D0, -0.420026350340952D-1,  &
      0.1665386113822915D0,-.421977345555443D-1,  &
      -.96219715278770D-2, .72189432466630D-2,  &
      -.11651675918591D-2, -.2152416741149D-3,  &
      .1280502823882D-3, -.201348547807D-4, -.12504934821D-5, .11330272320D-5,  &
      -.2056338417D-6, .61160950D-8, .50020075D-8, -.11812746D-8,  &
      .1043427D-9, .77823D-11, -.36968D-11, .51D-12,  &
      -.206D-13, -.54D-14, .14D-14, .1D-15/
  gr=g(26)
  DO  k=25,1,-1
    gr=gr*z+g(k)
  END DO
  ga=1.0D0/(gr*z)
  IF (DABS(x) > 1.0D0) THEN
    ga=ga*r
    IF (x < 0.0D0) ga=-pi/(x*ga*DSIN(pi*x))
  END IF
END IF
RETURN
END SUBROUTINE gamma
