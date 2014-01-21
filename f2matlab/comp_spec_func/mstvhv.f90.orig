PROGRAM mstvhv
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:13

!       ======================================================
!       Purpose:  This program computes Struve function Hv(x)
!                 for an arbitrary order using subroutine
!                 STVHV
!       Input :   v  --- Order of Hv(x)  ( -8.0 ף v ף 12.5 )
!                 x  --- Argument of Hv(x) ( x ע 0 )
!       Output:   HV --- Hv(x)
!       Example:  x = 10.0
!                   v           Hv(x)
!                 -----------------------
!                   .5     .46402212D+00
!                  1.5     .14452322D+01
!                  2.5     .31234632D+01
!                  3.5     .53730255D+01
!                  4.5     .72083122D+01
!                  5.5     .76851132D+01
!       ======================================================

IMPLICIT DOUBLE PRECISION (a-h,o-z)
WRITE(*,*)'Please v and x '
!        READ(*,*)V,X
v=5.5
x=10.0
WRITE(*,30)v,x
WRITE(*,*)
WRITE(*,*)'   v           Hv(x)'
WRITE(*,*)' -----------------------'
CALL stvhv(v,x,hv)
WRITE(*,20)v,hv
20      FORMAT(1X,f5.1,d18.8)
30      FORMAT(1X,'v =',f5.1,6X,'x =',f5.1)
END PROGRAM mstvhv


SUBROUTINE stvhv(v,x,hv)

!       =====================================================
!       Purpose: Compute Struve function Hv(x) with an
!                arbitrary order v
!       Input :  v  --- Order of Hv(x)  ( -8.0 ף v ף 12.5 )
!                x  --- Argument of Hv(x) ( x ע 0 )
!       Output:  HV --- Hv(x)
!       Routine called: GAMMA to compute the gamma function
!       =====================================================



DOUBLE PRECISION, INTENT(IN)             :: v
DOUBLE PRECISION, INTENT(IN)             :: x
DOUBLE PRECISION, INTENT(OUT)            :: hv
IMPLICIT DOUBLE PRECISION (a-h,o-z)
pi=3.141592653589793D0
IF (x == 0.0D0) THEN
  IF (v > -1.0.OR.INT(v)-v == 0.5D0) THEN
    hv=0.0D0
  ELSE IF (v < -1.0D0) THEN
    hv=(-1)**(INT(0.5D0-v)-1)*1.0D+300
  ELSE IF (v == -1.0D0) THEN
    hv=2.0D0/pi
  END IF
  RETURN
END IF
IF (x <= 20.0D0) THEN
  v0=v+1.5D0
  CALL gamma(v0,ga)
  s=2.0D0/(DSQRT(pi)*ga)
  r1=1.0D0
  DO  k=1,100
    va=k+1.5D0
    CALL gamma(va,ga)
    vb=v+k+1.5D0
    CALL gamma(vb,gb)
    r1=-r1*(0.5D0*x)**2
    r2=r1/(ga*gb)
    s=s+r2
    IF (DABS(r2) < DABS(s)*1.0D-12) EXIT
  END DO
  15         hv=(0.5D0*x)**(v+1.0D0)*s
ELSE
  sa=(0.5D0*x)**(v-1.0)/pi
  v0=v+0.5D0
  CALL gamma(v0,ga)
  s=DSQRT(pi)/ga
  r1=1.0D0
  DO  k=1,12
    va=k+0.5D0
    CALL gamma(va,ga)
    vb=-k+v+0.5D0
    CALL gamma(vb,gb)
    r1=r1/(0.5D0*x)**2
    s=s+r1*ga/gb
  END DO
  s0=sa*s
  u=DABS(v)
  n=INT(u)
  u0=u-n
  DO  l=0,1
    vt=4.0D0*(u0+l)**2
    r1=1.0D0
    pu1=1.0D0
    DO  k=1,12
      r1=-0.0078125D0*r1*(vt-(4.0*k-3.0D0)**2)*  &
          (vt-(4.0D0*k-1.0)**2)/((2.0D0*k-1.0)*k*x*x)
      pu1=pu1+r1
    END DO
    qu1=1.0D0
    r2=1.0D0
    DO  k=1,12
      r2=-0.0078125D0*r2*(vt-(4.0D0*k-1.0)**2)*  &
          (vt-(4.0D0*k+1.0)**2)/((2.0D0*k+1.0)*k*x*x)
      qu1=qu1+r2
    END DO
    qu1=0.125D0*(vt-1.0D0)/x*qu1
    IF (l == 0) THEN
      pu0=pu1
      qu0=qu1
    END IF
  END DO
  t0=x-(0.5*u0+0.25D0)*pi
  t1=x-(0.5*u0+0.75D0)*pi
  sr=DSQRT(2.0D0/(pi*x))
  by0=sr*(pu0*DSIN(t0)+qu0*DCOS(t0))
  by1=sr*(pu1*DSIN(t1)+qu1*DCOS(t1))
  bf0=by0
  bf1=by1
  DO  k=2,n
    bf=2.0D0*(k-1.0+u0)/x*bf1-bf0
    bf0=bf1
    bf1=bf
  END DO
  IF (n == 0) byv=by0
  IF (n == 1) byv=by1
  IF (n > 1) byv=bf
  hv=byv+s0
END IF
RETURN
END SUBROUTINE stvhv


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
