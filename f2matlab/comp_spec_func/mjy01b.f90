PROGRAM mjy01b
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:11

!       =========================================================
!       Purpose: This program computes Bessel functions Jn(x)
!                and Yn(x) ( n=0,1 ) and their derivatives
!                using subroutine JY01B
!       Input :  x   --- Argument of Jn(x) & Yn(x) ( x ò 0 )
!       Output:  BJ0 --- J0(x)
!                DJ0 --- J0'(x)
!                BJ1 --- J1(x)
!                DJ1 --- J1'(x)
!                BY0 --- Y0(x)
!                DY0 --- Y0'(x)
!                BY1 --- Y1(x)
!                DY1 --- Y1'(x)
!       Example:

!        x       J0(x)        J0'(x)       J1(x)        J1'(x)
!       ---------------------------------------------------------
!        1     .76519769   -.44005059    .44005059    .32514710
!        5    -.17759677    .32757914   -.32757914   -.11208094
!       10    -.24593576   -.04347275    .04347275   -.25028304
!       20     .16702466   -.06683312    .06683312    .16368301
!       30    -.08636798    .11875106   -.11875106   -.08240961
!       40     .00736689   -.12603832    .12603832    .00421593
!       50     .05581233    .09751183   -.09751183    .05776256

!        x       Y0(x)        Y0'(x)       Y1(x)        Y1'(x)
!      ---------------------------------------------------------
!        1     .08825696    .78121282   -.78121282    .86946979
!        5    -.30851763   -.14786314    .14786314   -.33809025
!       10     .05567117   -.24901542    .24901542    .03076962
!       20     .06264060    .16551161   -.16551161    .07091618
!       30    -.11729573   -.08442557    .08442557   -.12010992
!       40     .12593642    .00579351   -.00579351    .12608125
!       50    -.09806500    .05679567   -.05679567   -.09692908
!       =========================================================

IMPLICIT DOUBLE PRECISION (a-h,o-z)
WRITE(*,*)'Please enter x'
!        READ(*,*)X
x=50
WRITE(*,20)x
WRITE(*,*)'  x          J0(x)          J0''(X)         J1(X)',  &
    '          J1''(X)'
WRITE(*,*)'------------------------------------------',  &
    '----------------------------'
CALL jy01b(x,bj0,dj0,bj1,dj1,by0,dy0,by1,dy1)
WRITE(*,10)x,bj0,dj0,bj1,dj1
WRITE(*,*)
WRITE(*,*)'  x          Y0(x)          Y0''(X)         Y1(X)',  &
    '          Y1''(X)'
WRITE(*,*)'------------------------------------------',  &
    '----------------------------'
WRITE(*,10)x,by0,dy0,by1,dy1
10      FORMAT(1X,f5.1,4E16.8)
20      FORMAT(3X,'x =',f5.1)
END PROGRAM mjy01b


SUBROUTINE jy01b(x,bj0,dj0,bj1,dj1,by0,dy0,by1,dy1)

!       =======================================================
!       Purpose: Compute Bessel functions J0(x), J1(x), Y0(x),
!                Y1(x), and their derivatives
!       Input :  x   --- Argument of Jn(x) & Yn(x) ( x ò 0 )
!       Output:  BJ0 --- J0(x)
!                DJ0 --- J0'(x)
!                BJ1 --- J1(x)
!                DJ1 --- J1'(x)
!                BY0 --- Y0(x)
!                DY0 --- Y0'(x)
!                BY1 --- Y1(x)
!                DY1 --- Y1'(x)
!       =======================================================



DOUBLE PRECISION, INTENT(IN)             :: x
DOUBLE PRECISION, INTENT(OUT)            :: bj0
DOUBLE PRECISION, INTENT(OUT)            :: dj0
DOUBLE PRECISION, INTENT(OUT)            :: bj1
DOUBLE PRECISION, INTENT(OUT)            :: dj1
DOUBLE PRECISION, INTENT(OUT)            :: by0
DOUBLE PRECISION, INTENT(OUT)            :: dy0
DOUBLE PRECISION, INTENT(OUT)            :: by1
DOUBLE PRECISION, INTENT(OUT)            :: dy1
IMPLICIT DOUBLE PRECISION (a-h,o-z)
pi=3.141592653589793D0
IF (x == 0.0D0) THEN
  bj0=1.0D0
  bj1=0.0D0
  dj0=0.0D0
  dj1=0.5D0
  by0=-1.0D+300
  by1=-1.0D+300
  dy0=1.0D+300
  dy1=1.0D+300
  RETURN
ELSE IF (x <= 4.0D0) THEN
  t=x/4.0D0
  t2=t*t
  bj0=((((((-.5014415D-3*t2+.76771853D-2)*t2  &
      -.0709253492D0)*t2+.4443584263D0)*t2 -1.7777560599D0)*t2+3.9999973021D0)  &
      *t2-3.9999998721D0)*t2+1.0D0
  bj1=t*(((((((-.1289769D-3*t2+.22069155D-2)  &
      *t2-.0236616773D0)*t2+.1777582922D0)*t2  &
      -.8888839649D0)*t2+2.6666660544D0)*t2 -3.9999999710D0)*t2+1.9999999998D0)
  by0=(((((((-.567433D-4*t2+.859977D-3)*t2  &
      -.94855882D-2)*t2+.0772975809D0)*t2 -.4261737419D0)*t2+1.4216421221D0)*t2  &
      -2.3498519931D0)*t2+1.0766115157)*t2 +.3674669052D0
  by0=2.0D0/pi*DLOG(x/2.0D0)*bj0+by0
  by1=((((((((.6535773D-3*t2-.0108175626D0)*t2  &
      +.107657606D0)*t2-.7268945577D0)*t2  &
      +3.1261399273D0)*t2-7.3980241381D0)*t2  &
      +6.8529236342D0)*t2+.3932562018D0)*t2 -.6366197726D0)/x
  by1=2.0D0/pi*DLOG(x/2.0D0)*bj1+by1
ELSE
  t=4.0D0/x
  t2=t*t
  a0=DSQRT(2.0D0/(pi*x))
  p0=((((-.9285D-5*t2+.43506D-4)*t2-.122226D-3)*t2  &
      +.434725D-3)*t2-.4394275D-2)*t2+.999999997D0
  q0=t*(((((.8099D-5*t2-.35614D-4)*t2+.85844D-4)*t2  &
      -.218024D-3)*t2+.1144106D-2)*t2-.031249995D0)
  ta0=x-.25D0*pi
  bj0=a0*(p0*DCOS(ta0)-q0*DSIN(ta0))
  by0=a0*(p0*DSIN(ta0)+q0*DCOS(ta0))
  p1=((((.10632D-4*t2-.50363D-4)*t2+.145575D-3)*t2  &
      -.559487D-3)*t2+.7323931D-2)*t2+1.000000004D0
  q1=t*(((((-.9173D-5*t2+.40658D-4)*t2-.99941D-4)*t2  &
      +.266891D-3)*t2-.1601836D-2)*t2+.093749994D0)
  ta1=x-.75D0*pi
  bj1=a0*(p1*DCOS(ta1)-q1*DSIN(ta1))
  by1=a0*(p1*DSIN(ta1)+q1*DCOS(ta1))
END IF
dj0=-bj1
dj1=bj0-bj1/x
dy0=-by1
dy1=by0-by1/x
RETURN
END SUBROUTINE jy01b
