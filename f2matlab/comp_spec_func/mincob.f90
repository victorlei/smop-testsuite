PROGRAM mincob
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:11

!       =========================================================
!       Purpose: This program computes the incomplete beta
!                function Ix(a,b) using subroutine INCOB
!       Input :  a --- Parameter
!                b --- Parameter
!                x --- Argument ( 0 ף x ף 1 )
!       Output:  BIX --- Ix(a,b)
!       Example:
!                  a       b       x       Ix(a,b)
!                -----------------------------------
!                 1.0     3.0     .25     .57812500
!       =========================================================

IMPLICIT DOUBLE PRECISION (a-h,o-z)
WRITE(*,*)'Please enter a, b and x ( 0 ף x ף 1 )'
!        READ(*,*)A,B,X
a=1.0
b=3.0
x=.25
WRITE(*,*)
WRITE(*,*)'   a       b       x       Ix(a,b)'
WRITE(*,*)' -----------------------------------'
CALL incob(a,b,x,bix)
WRITE(*,10)a,b,x,bix
10      FORMAT(1X,f5.1,3X,f5.1,3X,f5.2,f14.8)
END PROGRAM mincob


SUBROUTINE incob(a,b,x,bix)

!       ========================================================
!       Purpose: Compute the incomplete beta function Ix(a,b)
!       Input :  a --- Parameter
!                b --- Parameter
!                x --- Argument ( 0 ף x ף 1 )
!       Output:  BIX --- Ix(a,b)
!       Routine called: BETA for computing beta function B(p,q)
!       ========================================================


DOUBLE PRECISION, INTENT(IN)             :: a
DOUBLE PRECISION, INTENT(IN)             :: b
DOUBLE PRECISION, INTENT(IN)             :: x
DOUBLE PRECISION, INTENT(OUT)            :: bix
IMPLICIT DOUBLE PRECISION (a-h,o-z)
DIMENSION dk(51),fk(51)

s0=(a+1.0D0)/(a+b+2.0D0)
CALL beta(a,b,bt)
IF (x <= s0) THEN
  DO  k=1,20
    dk(2*k)=k*(b-k)*x/(a+2.0D0*k-1.0D0)/(a+2.0D0*k)
  END DO
  DO  k=0,20
    dk(2*k+1)=-(a+k)*(a+b+k)*x/(a+2.d0*k)/(a+2.0*k+1.0)
  END DO
  t1=0.0D0
  DO  k=20,1,-1
    t1=dk(k)/(1.0D0+t1)
  END DO
  ta=1.0D0/(1.0D0+t1)
  bix=x**a*(1.0D0-x)**b/(a*bt)*ta
ELSE
  DO  k=1,20
    fk(2*k)=k*(a-k)*(1.0D0-x)/(b+2.*k-1.0)/(b+2.0*k)
  END DO
  DO  k=0,20
    fk(2*k+1)=-(b+k)*(a+b+k)*(1.d0-x)/ (b+2.d0*k)/(b+2.d0*k+1.d0)
  END DO
  t2=0.0D0
  DO  k=20,1,-1
    t2=fk(k)/(1.0D0+t2)
  END DO
  tb=1.0D0/(1.0D0+t2)
  bix=1.0D0-x**a*(1.0D0-x)**b/(b*bt)*tb
END IF
RETURN
END SUBROUTINE incob


SUBROUTINE beta(p,q,bt)

!       ==========================================
!       Purpose: Compute the beta function B(p,q)
!       Input :  p --- Parameter  ( p > 0 )
!                q --- Parameter  ( q > 0 )
!       Output:  BT --- B(p,q)
!       Routine called: GAMMA for computing ג(x)
!       ==========================================



DOUBLE PRECISION, INTENT(IN)             :: p
DOUBLE PRECISION, INTENT(IN)             :: q
DOUBLE PRECISION, INTENT(OUT)            :: bt
IMPLICIT DOUBLE PRECISION (a-h,o-z)
CALL gamma(p,gp)
CALL gamma(q,gq)
ppq=p+q
CALL gamma(ppq,gpq)
bt=gp*gq/gpq
RETURN
END SUBROUTINE beta


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
