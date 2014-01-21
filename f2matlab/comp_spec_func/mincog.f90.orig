PROGRAM mincog
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:11

!       ==========================================================
!       Purpose: This program computes the incomplete gamma
!                function r(a,x), ג(a,x) and P(a,x) using
!                subroutine INCOG
!       Input :  a   --- Parameter
!                x   --- Argument
!       Output:  GIN --- r(a,x)
!                GIM --- ג(a,x)
!                GIP --- P(a,x)
!       Example:
!            a     x      r(a,x)         ג(a,x)         P(a,x)
!           -------------------------------------------------------
!           3.0   5.0  .17506960D+01  .24930404D+00  .87534798D+00
!       ===========================================================


DOUBLE PRECISION :: a,x,gin,gim,gip
WRITE(*,*)'Plese enter a and x'
!        READ(*,*)A,X
a=3.0
x=5.0
WRITE(*,*)
WRITE(*,*)'   a     x      r(a,x)         g(a,x)         P(a,x)'
WRITE(*,*)' --------------------------------------------', '------------'
CALL incog(a,x,gin,gim,gip)
WRITE(*,10)a,x,gin,gim,gip
10      FORMAT(1X,f5.1,1X,f5.1,3D15.8)
END PROGRAM mincog


SUBROUTINE incog(a,x,gin,gim,gip)

!       ===================================================
!       Purpose: Compute the incomplete gamma function
!                r(a,x), ג(a,x) and P(a,x)
!       Input :  a   --- Parameter ( a ף 170 )
!                x   --- Argument
!       Output:  GIN --- r(a,x)
!                GIM --- ג(a,x)
!                GIP --- P(a,x)
!       Routine called: GAMMA for computing ג(x)
!       ===================================================



DOUBLE PRECISION, INTENT(IN)             :: a
DOUBLE PRECISION, INTENT(IN)             :: x
DOUBLE PRECISION, INTENT(OUT)            :: gin
DOUBLE PRECISION, INTENT(OUT)            :: gim
DOUBLE PRECISION, INTENT(OUT)            :: gip
IMPLICIT DOUBLE PRECISION (a-h,o-z)
xam=-x+a*DLOG(x)
IF (xam > 700.0.OR.a > 170.0) THEN
  WRITE(*,*)'a and/or x too large'
  STOP
END IF
IF (x == 0.0) THEN
  gin=0.0
  CALL gamma(a,ga)
  gim=ga
  gip=0.0
ELSE IF (x <= 1.0+a) THEN
  s=1.0D0/a
  r=s
  DO  k=1,60
    r=r*x/(a+k)
    s=s+r
    IF (DABS(r/s) < 1.0D-15) EXIT
  END DO
  15         gin=DEXP(xam)*s
  CALL gamma(a,ga)
  gip=gin/ga
  gim=ga-gin
ELSE IF (x > 1.0+a) THEN
  t0=0.0D0
  DO  k=60,1,-1
    t0=(k-a)/(1.0D0+k/(x+t0))
  END DO
  gim=DEXP(xam)/(x+t0)
  CALL gamma(a,ga)
  gin=ga-gim
  gip=1.0D0-gim/ga
END IF
END SUBROUTINE incog


SUBROUTINE gamma(x,ga)

!       ==================================================
!       Purpose: Compute gamma function ג(x)
!       Input :  x  --- Argument of ג(x)
!                       ( x is not equal to 0,-1,-2,תתת)
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
