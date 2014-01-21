PROGRAM mik01b
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:11

!       =============================================================
!       Purpose: This program computes the modified Bessel functions
!                I0(x), I1(x), K0(x), K1(x), and their derivatives
!                using subroutine IK01B
!       Input :  x   --- Argument ( x ò 0 )
!       Output:  BI0 --- I0(x)
!                DI0 --- I0'(x)
!                BI1 --- I1(x)
!                DI1 --- I1'(x)
!                BK0 --- K0(x)
!                DK0 --- K0'(x)
!                BK1 --- K1(x)
!                DK1 --- K1'(x)
!       Example:

!         x      I0(x)         I0'(x)        I1(x)         I1'(x)
!       -------------------------------------------------------------
!        1.0   .126607D+01   .565159D+00   .565159D+00   .700907D+00
!       10.0   .281572D+04   .267099D+04   .267099D+04   .254862D+04
!       20.0   .435583D+08   .424550D+08   .424550D+08   .414355D+08
!       30.0   .781672D+12   .768532D+12   .768532D+12   .756055D+12
!       40.0   .148948D+17   .147074D+17   .147074D+17   .145271D+17
!       50.0   .293255D+21   .290308D+21   .290308D+21   .287449D+21

!         x      K0(x)         K0'(x)        K1(x)         K1'(x)
!       -------------------------------------------------------------
!        1.0   .421024D+00  -.601907D+00   .601907D+00  -.102293D+01
!       10.0   .177801D-04  -.186488D-04   .186488D-04  -.196449D-04
!       20.0   .574124D-09  -.588306D-09   .588306D-09  -.603539D-09
!       30.0   .213248D-13  -.216773D-13   .216773D-13  -.220474D-13
!       40.0   .839286D-18  -.849713D-18   .849713D-18  -.860529D-18
!       50.0   .341017D-22  -.344410D-22   .344410D-22  -.347905D-22
!       =============================================================

IMPLICIT DOUBLE PRECISION (a-h,o-z)
WRITE(*,*)'Please enter x '
!        READ(*,*)X
x=1.0
WRITE(*,10)x
WRITE(*,*)'  x       I0(x)          I0''(X)         I1(X)',  &
    '          I1''(X)'
WRITE(*,*)'-------------------------------------------',  &
    '----------------------'
CALL ik01b(x,bi0,di0,bi1,di1,bk0,dk0,bk1,dk1)
WRITE(*,20)x,bi0,di0,bi1,di1
WRITE(*,*)
WRITE(*,*)'  x       K0(x)          K0''(X)         K1(X)',  &
    '          K1''(X)'
WRITE(*,*)'-------------------------------------------',  &
    '----------------------'
WRITE(*,20)x,bk0,dk0,bk1,dk1
10      FORMAT(3X 'x =',f5.1)
20      FORMAT(1X,f4.1,4D15.7)
END PROGRAM mik01b


SUBROUTINE ik01b(x,bi0,di0,bi1,di1,bk0,dk0,bk1,dk1)

!       =========================================================
!       Purpose: Compute modified Bessel functions I0(x), I1(1),
!                K0(x) and K1(x), and their derivatives
!       Input :  x   --- Argument ( x ò 0 )
!       Output:  BI0 --- I0(x)
!                DI0 --- I0'(x)
!                BI1 --- I1(x)
!                DI1 --- I1'(x)
!                BK0 --- K0(x)
!                DK0 --- K0'(x)
!                BK1 --- K1(x)
!                DK1 --- K1'(x)
!       =========================================================



DOUBLE PRECISION, INTENT(IN)             :: x
DOUBLE PRECISION, INTENT(OUT)            :: bi0
DOUBLE PRECISION, INTENT(OUT)            :: di0
DOUBLE PRECISION, INTENT(OUT)            :: bi1
DOUBLE PRECISION, INTENT(OUT)            :: di1
DOUBLE PRECISION, INTENT(OUT)            :: bk0
DOUBLE PRECISION, INTENT(OUT)            :: dk0
DOUBLE PRECISION, INTENT(OUT)            :: bk1
DOUBLE PRECISION, INTENT(OUT)            :: dk1
IMPLICIT DOUBLE PRECISION (a-h,o-z)
IF (x == 0.0D0) THEN
  bi0=1.0D0
  bi1=0.0D0
  bk0=1.0D+300
  bk1=1.0D+300
  di0=0.0D0
  di1=0.5D0
  dk0=-1.0D+300
  dk1=-1.0D+300
  RETURN
ELSE IF (x <= 3.75D0) THEN
  t=x/3.75D0
  t2=t*t
  bi0=(((((.0045813D0*t2+.0360768D0)*t2+.2659732D0)  &
      *t2+1.2067492D0)*t2+3.0899424D0)*t2 +3.5156229D0)*t2+1.0D0
  bi1=x*((((((.00032411D0*t2+.00301532D0)*t2  &
      +.02658733D0)*t2+.15084934D0)*t2+.51498869D0) *t2+.87890594D0)*t2+.5D0)
ELSE
  t=3.75D0/x
  bi0=((((((((.00392377D0*t-.01647633D0)*t  &
      +.02635537D0)*t-.02057706D0)*t+.916281D-2)*t  &
      -.157565D-2)*t+.225319D-2)*t+.01328592D0)*t +.39894228D0)*DEXP(x)/DSQRT(x)
  bi1=((((((((-.420059D-2*t+.01787654D0)*t  &
      -.02895312D0)*t+.02282967D0)*t-.01031555D0)*t  &
      +.163801D-2)*t-.00362018D0)*t-.03988024D0)*t +.39894228D0)*DEXP(x)/DSQRT(x)
END IF
IF (x <= 2.0D0) THEN
  t=x/2.0D0
  t2=t*t
  bk0=(((((.0000074D0*t2+.0001075D0)*t2+.00262698D0)  &
      *t2+.0348859D0)*t2+.23069756D0)*t2+.4227842D0) *t2-.57721566D0-bi0*DLOG(t)
  bk1=((((((-.00004686D0*t2-.00110404D0)*t2  &
      -.01919402D0)*t2-.18156897D0)*t2-.67278579D0)  &
      *t2+.15443144D0)*t2+1.0D0)/x+bi1*DLOG(t)
ELSE
  t=2.0D0/x
  t2=t*t
  bk0=((((((.00053208D0*t-.0025154D0)*t+.00587872D0)  &
      *t-.01062446D0)*t+.02189568D0)*t-.07832358D0)  &
      *t+1.25331414D0)*DEXP(-x)/DSQRT(x)
  bk1=((((((-.00068245D0*t+.00325614D0)*t  &
      -.00780353D0)*t+.01504268D0)*t-.0365562D0)*t+  &
      .23498619D0)*t+1.25331414D0)*DEXP(-x)/DSQRT(x)
END IF
di0=bi1
di1=bi0-bi1/x
dk0=-bk1
dk1=-bk0-bk1/x
RETURN
END SUBROUTINE ik01b
