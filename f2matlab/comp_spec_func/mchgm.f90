PROGRAM mchgm
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:10

!     =======================================================
!     Purpose: This program computes the confluent
!     hypergeometric function M(a,b,x) using
!     subroutine CHGM
!     Input  : a  --- Parameter
!     b  --- Parameter ( b <> 0,-1,-2,... )
!     x  --- Argument
!     Output:  HG --- M(a,b,x)
!     Example:
!     a       b       x          M(a,b,x)
!     -----------------------------------------
!     1.5     2.0    20.0     .1208527185D+09
!     4.5     2.0    20.0     .1103561117D+12
!     -1.5     2.0    20.0     .1004836854D+05
!     -4.5     2.0    20.0    -.3936045244D+03
!     1.5     2.0    50.0     .8231906643D+21
!     4.5     2.0    50.0     .9310512715D+25
!     -1.5     2.0    50.0     .2998660728D+16
!     -4.5     2.0    50.0    -.1806547113D+13
!     =======================================================

IMPLICIT DOUBLE PRECISION (a-h,o-z)
WRITE(*,*)'Please enter a, b and x '
!     READ(*,*)A,B,X
a=1.5
b=2.0
x=20.0
WRITE(*,*)'   a       b       x          M(a,b,x)'
WRITE(*,*)' -----------------------------------------'
CALL chgm(a,b,x,hg)
WRITE(*,10)a,b,x,hg
10   FORMAT(1X,f5.1,3X,f5.1,3X,f5.1,d20.10)
END PROGRAM mchgm


SUBROUTINE chgm(a,b,x,hg)

!     ===================================================
!     Purpose: Compute confluent hypergeometric function
!     M(a,b,x)
!     Input  : a  --- Parameter
!     b  --- Parameter ( b <> 0,-1,-2,... )
!     x  --- Argument
!     Output:  HG --- M(a,b,x)
!     Routine called: GAMMA for computing ג(x)
!     ===================================================



DOUBLE PRECISION, INTENT(IN OUT)         :: a
DOUBLE PRECISION, INTENT(IN)             :: b
DOUBLE PRECISION, INTENT(IN OUT)         :: x
DOUBLE PRECISION, INTENT(OUT)            :: hg
IMPLICIT DOUBLE PRECISION (a-h,o-z)
pi=3.141592653589793D0
a0=a
a1=a
x0=x
hg=0.0D0
IF (b == 0.0D0.OR.b == -ABS(INT(b))) THEN
  hg=1.0D+300
ELSE IF (a == 0.0D0.OR.x == 0.0D0) THEN
  hg=1.0D0
ELSE IF (a == -1.0D0) THEN
  hg=1.0D0-x/b
ELSE IF (a == b) THEN
  hg=DEXP(x)
ELSE IF (a-b == 1.0D0) THEN
  hg=(1.0D0+x/b)*DEXP(x)
ELSE IF (a == 1.0D0.AND.b == 2.0D0) THEN
  hg=(DEXP(x)-1.0D0)/x
ELSE IF (a == INT(a).AND.a < 0.0D0) THEN
  m=INT(-a)
  r=1.0D0
  hg=1.0D0
  DO  k=1,m
    r=r*(a+k-1.0D0)/k/(b+k-1.0D0)*x
    hg=hg+r
  END DO
END IF
IF (hg /= 0.0D0) RETURN
IF (x < 0.0D0) THEN
  a=b-a
  a0=a
  x=DABS(x)
END IF
IF (a < 2.0D0) nl=0
IF (a >= 2.0D0) THEN
  nl=1
  la=INT(a)
  a=a-la-1.0D0
END IF
DO  n=0,nl
  IF (a0 >= 2.0D0) a=a+1.0D0
  IF (x <= 30.0D0+DABS(b).OR.a < 0.0D0) THEN
    hg=1.0D0
    rg=1.0D0
    DO  j=1,500
      rg=rg*(a+j-1.0D0)/(j*(b+j-1.0D0))*x
      hg=hg+rg
      IF (DABS(rg/hg) < 1.0D-15) EXIT
    END DO
  ELSE
    CALL gamma(a,ta)
    CALL gamma(b,tb)
    xg=b-a
    CALL gamma(xg,tba)
    sum1=1.0D0
    sum2=1.0D0
    r1=1.0D0
    r2=1.0D0
    DO  i=1,8
      r1=-r1*(a+i-1.0D0)*(a-b+i)/(x*i)
      r2=-r2*(b-a+i-1.0D0)*(a-i)/(x*i)
      sum1=sum1+r1
      sum2=sum2+r2
    END DO
    hg1=tb/tba*x**(-a)*DCOS(pi*a)*sum1
    hg2=tb/ta*DEXP(x)*x**(a-b)*sum2
    hg=hg1+hg2
  END IF
  25    IF (n == 0) y0=hg
  IF (n == 1) y1=hg
END DO
IF (a0 >= 2.0D0) THEN
  DO  i=1,la-1
    hg=((2.0D0*a-b+x)*y1+(b-a)*y0)/a
    y0=y1
    y1=hg
    a=a+1.0D0
  END DO
END IF
IF (x0 < 0.0D0) hg=hg*DEXP(x0)
a=a1
x=x0
RETURN
END SUBROUTINE chgm


SUBROUTINE gamma(x,ga)

!     ==================================================
!     Purpose: Compute gamma function ג(x)
!     Input :  x  --- Argument of ג(x)
!     ( x is not equal to 0,-1,-2,תתת)
!     Output:  GA --- ג(x)
!     ==================================================


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
