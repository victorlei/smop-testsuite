PROGRAM mstvlv
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:13

!       ======================================================
!       Purpose:  This program computes the modified Struve
!                 function Lv(x) for an arbitrary order v
!                 using subroutine STVLV
!       Input :   v   --- Order of Lv(x)  ( |v| ף 20 )
!                 x   --- Argument of Lv(x) ( x ע 0 )
!       Output:   SLV --- Lv(x)
!       Example:  x = 10.0
!                   v          Lv(x)
!                 ------------------------
!                  0.5     .27785323D+04
!                  1.5     .24996698D+04
!                  2.5     .20254774D+04
!                  3.5     .14816746D+04
!                  4.5     .98173460D+03
!                  5.5     .59154277D+03
!       =====================================================

IMPLICIT DOUBLE PRECISION (a-h,o-z)
WRITE(*,*)'Please enter v and x '
!        READ(*,*)V,X
v=5.5
x=10.0
WRITE(*,30)v,x
WRITE(*,*)
WRITE(*,*)'   v          Lv(x)'
WRITE(*,*)' ------------------------'
CALL stvlv(v,x,slv)
WRITE(*,20)v,slv
20      FORMAT(1X,f5.1,d18.8)
30      FORMAT(1X,'v =',f5.1,6X,'x =',f5.1)
END PROGRAM mstvlv


SUBROUTINE stvlv(v,x,slv)

!       ======================================================
!       Purpose:  Compute modified Struve function Lv(x) with
!                 an arbitrary order v
!       Input :   v   --- Order of Lv(x)  ( |v| ף 20 )
!                 x   --- Argument of Lv(x) ( x ע 0 )
!       Output:   SLV --- Lv(x)
!       Routine called: GAMMA to compute the gamma function
!       ======================================================



DOUBLE PRECISION, INTENT(IN)             :: v
DOUBLE PRECISION, INTENT(IN)             :: x
DOUBLE PRECISION, INTENT(OUT)            :: slv
IMPLICIT DOUBLE PRECISION (a-h,o-z)
pi=3.141592653589793D0
IF (x == 0.0D0) THEN
  IF (v > -1.0.OR.INT(v)-v == 0.5D0) THEN
    slv=0.0D0
  ELSE IF (v < -1.0D0) THEN
    slv=(-1)**(INT(0.5D0-v)-1)*1.0D+300
  ELSE IF (v == -1.0D0) THEN
    slv=2.0D0/pi
  END IF
  RETURN
END IF
IF (x <= 40.0D0) THEN
  v0=v+1.5D0
  CALL gamma(v0,ga)
  s=2.0D0/(DSQRT(pi)*ga)
  r1=1.0D0
  DO  k=1,100
    va=k+1.5D0
    CALL gamma(va,ga)
    vb=v+k+1.5D0
    CALL gamma(vb,gb)
    r1=r1*(0.5D0*x)**2
    r2=r1/(ga*gb)
    s=s+r2
    IF (DABS(r2/s) < 1.0D-12) EXIT
  END DO
  15         slv=(0.5D0*x)**(v+1.0D0)*s
ELSE
  sa=-1.0D0/pi*(0.5D0*x)**(v-1.0)
  v0=v+0.5D0
  CALL gamma(v0,ga)
  s=-DSQRT(pi)/ga
  r1=-1.0D0
  DO  k=1,12
    va=k+0.5D0
    CALL gamma(va,ga)
    vb=-k+v+0.5D0
    CALL gamma(vb,gb)
    r1=-r1/(0.5D0*x)**2
    s=s+r1*ga/gb
  END DO
  s0=sa*s
  u=DABS(v)
  n=INT(u)
  u0=u-n
  DO  l=0,1
    vt=u0+l
    r=1.0D0
    biv=1.0D0
    DO  k=1,16
      r=-0.125*r*(4.0*vt*vt-(2.0*k-1.0D0)**2)/(k*x)
      biv=biv+r
      IF (DABS(r/biv) < 1.0D-12) EXIT
    END DO
    30            IF (l == 0) biv0=biv
  END DO
  bf0=biv0
  bf1=biv
  DO  k=2,n
    bf=-2.0D0*(k-1.0+u0)/x*bf1+bf0
    bf0=bf1
    bf1=bf
  END DO
  IF (n == 0) biv=biv0
  IF (n > 1) biv=bf
  slv=DEXP(x)/DSQRT(2.0D0*pi*x)*biv+s0
END IF
RETURN
END SUBROUTINE stvlv


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
