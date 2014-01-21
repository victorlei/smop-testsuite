PROGRAM mcchg
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:10

!     ===========================================================
!     Purpose: This program computes confluent hypergeometric
!     function M(a,b,z) with real parameters a, b, and
!     a complex argument z using subroutine CCHG
!     Input :  a --- Parameter
!     b --- Parameter
!     z --- Complex argument
!     Output:  CHG --- M(a,b,z)
!     Examples:
!     a      b        z        Re[M(a,b,z)]   Im[M(a,b,z)]
!     -------------------------------------------------------
!     3.3   4.25    10 + 0i    .61677489D+04    0
!     3.3   4.25    25 + 0i    .95781835D+10  -.15738228D-03
!     3.3   4.25     3 -  i    .75828716D+01  -.86815474D+01
!     3.3   4.25    15 +10i   -.58313765D+06  -.48195426D+05
!     ===========================================================

IMPLICIT DOUBLE PRECISION (a,b,x,y)
IMPLICIT COMPLEX *16 (c,z)
WRITE(*,*)'Please enter a, b, x and y (z=x+iy) '
!     READ(*,*)A,B,X,Y
a=3.3
b=4.25
x=15.0
y=10.0
WRITE(*,20)a,b,x,y
z=CMPLX(x,y)
CALL cchg(a,b,z,chg)
WRITE(*,10)chg
print *, chg
10   FORMAT(10X,'M(a,b,z) =',d18.8,' + i ',d18.8)
20   FORMAT(1X,'a =',f5.1,',  ','b =',f5.1,',  ','x =',f5.1, ',  ','y =',f5.1)
END PROGRAM mcchg


SUBROUTINE cchg(a,b,z,chg)

!     ===================================================
!     Purpose: Compute confluent hypergeometric function
!     M(a,b,z) with real parameters a, b and a
!     complex argument z
!     Input :  a --- Parameter
!     b --- Parameter
!     z --- Complex argument
!     Output:  CHG --- M(a,b,z)
!     Routine called: GAMMA for computing gamma function
!     ===================================================

IMPLICIT DOUBLE PRECISION (a,b,d-h,o-y)
IMPLICIT COMPLEX *16 (c,z)
DOUBLE PRECISION, INTENT(IN OUT)         :: a
DOUBLE PRECISION, INTENT(IN)             :: b
COMPLEX, INTENT(IN OUT)                  :: z
COMPLEX, INTENT(OUT)                     :: chg

chw=0.0
pi=3.141592653589793D0
ci=(0.0D0,1.0D0)
a0=a
a1=a
z0=z
IF (b == 0.0.OR.b == -INT(ABS(b))) THEN
  chg=(1.0D+300,0.0D0)
ELSE IF (a == 0.0D0.OR.z == 0.0D0) THEN
  chg=(1.0D0,0.0D0)
ELSE IF (a == -1.0D0) THEN
  chg=1.0D0-z/b
ELSE IF (a == b) THEN
  chg=CDEXP(z)
ELSE IF (a-b == 1.0D0) THEN
  chg=(1.0D0+z/b)*CDEXP(z)
ELSE IF (a == 1.0D0.AND.b == 2.0D0) THEN
  chg=(CDEXP(z)-1.0D0)/z
ELSE IF (a == INT(a).AND.a < 0.0D0) THEN
  m=INT(-a)
  cr=(1.0D0,0.0D0)
  chg=(1.0D0,0.0D0)
  DO  k=1,m
    cr=cr*(a+k-1.0D0)/k/(b+k-1.0D0)*z
    chg=chg+cr
  END DO
ELSE
  x0=REAL(z)
  IF (x0 < 0.0D0) THEN
    a=b-a
    a0=a
    z=-z
  END IF
  IF (a < 2.0D0) nl=0
  IF (a >= 2.0D0) THEN
    nl=1
    la=INT(a)
    a=a-la-1.0D0
  END IF
  DO  n=0,nl
    IF (a0 >= 2.0D0) a=a+1.0D0
    IF (CDABS(z) < 20.0D0+ABS(b).OR.a < 0.0D0) THEN
      chg=(1.0D0,0.0D0)
      crg=(1.0D0,0.0D0)
      DO  j=1,500
        crg=crg*(a+j-1.0D0)/(j*(b+j-1.0D0))*z
        chg=chg+crg
        IF (CDABS((chg-chw)/chg) < 1.d-15) EXIT
        chw=chg
      END DO
    ELSE
      CALL gamma(a,g1)
      CALL gamma(b,g2)
      ba=b-a
      CALL gamma(ba,g3)
      cs1=(1.0D0,0.0D0)
      cs2=(1.0D0,0.0D0)
      cr1=(1.0D0,0.0D0)
      cr2=(1.0D0,0.0D0)
      DO  i=1,8
        cr1=-cr1*(a+i-1.0D0)*(a-b+i)/(z*i)
        cr2=cr2*(b-a+i-1.0D0)*(i-a)/(z*i)
        cs1=cs1+cr1
        cs2=cs2+cr2
      END DO
      x=REAL(z)
      y=DIMAG(z)
      IF (x == 0.0.AND.y >= 0.0) THEN
        phi=0.5D0*pi
      ELSE IF (x == 0.0.AND.y <= 0.0) THEN
        phi=-0.5D0*pi
      ELSE
        phi=DATAN(y/x)
      END IF
      IF (phi > -0.5*pi.AND.phi < 1.5*pi) ns=1
      IF (phi > -1.5*pi.AND.phi <= -0.5*pi) ns=-1
      cfac=CDEXP(ns*ci*pi*a)
      IF (y == 0.0D0) cfac=DCOS(pi*a)
      chg1=g2/g3*z**(-a)*cfac*cs1
      chg2=g2/g1*CDEXP(z)*z**(a-b)*cs2
      chg=chg1+chg2
    END IF
    25     IF (n == 0) cy0=chg
    IF (n == 1) cy1=chg
  END DO
  IF (a0 >= 2.0D0) THEN
    DO  i=1,la-1
      chg=((2.0D0*a-b+z)*cy1+(b-a)*cy0)/a
      cy0=cy1
      cy1=chg
      a=a+1.0D0
    END DO
  END IF
  IF (x0 < 0.0D0) chg=chg*CDEXP(-z)
END IF
a=a1
z=z0
RETURN
END SUBROUTINE cchg


SUBROUTINE gamma(x,ga)

!     ==================================================
!     Purpose: Compute gamma function ג(x)
!     Input :  x  --- Argument of ג(x)
!     ( x is not equal to 0,-1,-2,תתת)
!     Output:  GA --- ג(x)
!     ==================================================


IMPLICIT DOUBLE PRECISION (a-h,o-z)
DOUBLE PRECISION, INTENT(IN)             :: x
DOUBLE PRECISION, INTENT(OUT)            :: ga
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
