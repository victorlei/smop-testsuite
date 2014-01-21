PROGRAM mpbdv
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:12

!       =========================================================
!       Purpose: This program computes the parabolic cylinder
!                functions Dv(x) and their derivatives using
!                subroutine PBDV
!       Input:   x --- Argument of Dv(x)
!                v --- Order of Dv(x)
!       Output:  DV(na) --- Dn+v0(x)
!                DP(na) --- Dn+v0'(x)
!                ( na = |n|, n = int(v), v0 = v-n, |v0| < 1
!                  n = 0,ס1,ס2,תתת, |n| ף 100 )
!                PDF --- Dv(x)
!                PDD --- Dv'(x)
!       Example: v = 5.5,  x =10.0,  v0 = 0.5,  n = 0,1,...,5

!                  n+v0      Dv(x)           Dv'(x)
!                ---------------------------------------
!                  0.5   .43971930D-10  -.21767183D-09
!                  1.5   .43753148D-09  -.21216995D-08
!                  2.5   .43093569D-08  -.20452956D-07
!                  3.5   .41999741D-07  -.19491595D-06
!                  4.5   .40491466D-06  -.18355745D-05
!                  5.5   .38601477D-05  -.17073708D-04

!                Dv(x)= .38601477D-05,  Dv'(x)=-.17073708D-04
!       =========================================================

IMPLICIT DOUBLE PRECISION (a-h,o-z)
DIMENSION dv(0:100),dp(0:100)
WRITE(*,*)'Please enter v and  x '
!        READ(*,*)V,X
v=5.5
x=10.0
WRITE(*,20)v,x
nv=INT(v)
v0=v-nv
na=ABS(nv)
CALL pbdv(v,x,dv,dp,pdf,pdd)
WRITE(*,*)
WRITE(*,*)'   v       Dv(x)           Dv''(X)'
WRITE(*,*)'---------------------------------------'
DO  k=0,na
  vk=k*ISIGN(1,nv)+v0
  WRITE(*,30)vk,dv(k),dp(k)
END DO
WRITE(*,*)
WRITE(*,40)v,pdf,pdd
20      FORMAT(1X,'v =',f6.2,',    ','x =',f6.2)
30      FORMAT(1X,f5.1,2D16.8)
40      FORMAT(1X,'v =',f5.1,',  Dv(x)=',d14.8,',   Dv''(X)=',d14.8)
END PROGRAM mpbdv


SUBROUTINE pbdv(v,x,dv,dp,pdf,pdd)

!       ====================================================
!       Purpose: Compute parabolic cylinder functions Dv(x)
!                and their derivatives
!       Input:   x --- Argument of Dv(x)
!                v --- Order of Dv(x)
!       Output:  DV(na) --- Dn+v0(x)
!                DP(na) --- Dn+v0'(x)
!                ( na = |n|, v0 = v-n, |v0| < 1,
!                  n = 0,ס1,ס2,תתת )
!                PDF --- Dv(x)
!                PDD --- Dv'(x)
!       Routines called:
!             (1) DVSA for computing Dv(x) for small |x|
!             (2) DVLA for computing Dv(x) for large |x|
!       ====================================================


DOUBLE PRECISION, INTENT(IN OUT)         :: v
DOUBLE PRECISION, INTENT(IN)             :: x
DOUBLE PRECISION, INTENT(OUT)            :: dv(0:*)
DOUBLE PRECISION, INTENT(OUT)            :: dp(0:*)
DOUBLE PRECISION, INTENT(OUT)            :: pdf
DOUBLE PRECISION, INTENT(OUT)            :: pdd
IMPLICIT DOUBLE PRECISION (a-h,o-z)


xa=DABS(x)
vh=v
v=v+DSIGN(1.0D0,v)
nv=INT(v)
v0=v-nv
na=ABS(nv)
ep=DEXP(-.25D0*x*x)
IF (na >= 1) ja=1
IF (v >= 0.0) THEN
  IF (v0 == 0.0) THEN
    pd0=ep
    pd1=x*ep
  ELSE
    DO  l=0,ja
      v1=v0+l
      IF (xa <= 5.8) CALL dvsa(v1,x,pd1)
      IF (xa > 5.8) CALL dvla(v1,x,pd1)
      IF (l == 0) pd0=pd1
    END DO
  END IF
  dv(0)=pd0
  dv(1)=pd1
  DO  k=2,na
    pdf=x*pd1-(k+v0-1.0D0)*pd0
    dv(k)=pdf
    pd0=pd1
    pd1=pdf
  END DO
ELSE
  IF (x <= 0.0) THEN
    IF (xa <= 5.8D0)  THEN
      CALL dvsa(v0,x,pd0)
      v1=v0-1.0D0
      CALL dvsa(v1,x,pd1)
    ELSE
      CALL dvla(v0,x,pd0)
      v1=v0-1.0D0
      CALL dvla(v1,x,pd1)
    END IF
    dv(0)=pd0
    dv(1)=pd1
    DO  k=2,na
      pd=(-x*pd1+pd0)/(k-1.0D0-v0)
      dv(k)=pd
      pd0=pd1
      pd1=pd
    END DO
  ELSE IF (x <= 2.0) THEN
    v2=nv+v0
    IF (nv == 0) v2=v2-1.0D0
    nk=INT(-v2)
    CALL dvsa(v2,x,f1)
    v1=v2+1.0D0
    CALL dvsa(v1,x,f0)
    dv(nk)=f1
    dv(nk-1)=f0
    DO  k=nk-2,0,-1
      f=x*f0+(k-v0+1.0D0)*f1
      dv(k)=f
      f1=f0
      f0=f
    END DO
  ELSE
    IF (xa <= 5.8) CALL dvsa(v0,x,pd0)
    IF (xa > 5.8) CALL dvla(v0,x,pd0)
    dv(0)=pd0
    m=100+na
    f1=0.0D0
    f0=1.0D-30
    DO  k=m,0,-1
      f=x*f0+(k-v0+1.0D0)*f1
      IF (k <= na) dv(k)=f
      f1=f0
      f0=f
    END DO
    s0=pd0/f
    DO  k=0,na
      dv(k)=s0*dv(k)
    END DO
  END IF
END IF
DO  k=0,na-1
  v1=ABS(v0)+k
  IF (v >= 0.0D0) THEN
    dp(k)=0.5D0*x*dv(k)-dv(k+1)
  ELSE
    dp(k)=-0.5D0*x*dv(k)-v1*dv(k+1)
  END IF
END DO
pdf=dv(na-1)
pdd=dp(na-1)
v=vh
RETURN
END SUBROUTINE pbdv


SUBROUTINE dvsa(va,x,pd)

!       ===================================================
!       Purpose: Compute parabolic cylinder function Dv(x)
!                for small argument
!       Input:   x  --- Argument
!                va --- Order
!       Output:  PD --- Dv(x)
!       Routine called: GAMMA for computing ג(x)
!       ===================================================



DOUBLE PRECISION, INTENT(IN)             :: va
DOUBLE PRECISION, INTENT(IN)             :: x
DOUBLE PRECISION, INTENT(OUT)            :: pd
IMPLICIT DOUBLE PRECISION (a-h,o-z)
eps=1.0D-15
pi=3.141592653589793D0
sq2=DSQRT(2.0D0)
ep=DEXP(-.25D0*x*x)
va0=0.5D0*(1.0D0-va)
IF (va == 0.0) THEN
  pd=ep
ELSE
  IF (x == 0.0) THEN
    IF (va0 <= 0.0.AND.va0 == INT(va0)) THEN
      pd=0.0D0
    ELSE
      CALL gamma(va0,ga0)
      pd=DSQRT(pi)/(2.0D0**(-.5D0*va)*ga0)
    END IF
  ELSE
    CALL gamma(-va,g1)
    a0=2.0D0**(-0.5D0*va-1.0D0)*ep/g1
    vt=-.5D0*va
    CALL gamma(vt,g0)
    pd=g0
    r=1.0D0
    DO  m=1,250
      vm=.5D0*(m-va)
      CALL gamma(vm,gm)
      r=-r*sq2*x/m
      r1=gm*r
      pd=pd+r1
      IF (DABS(r1) < DABS(pd)*eps) EXIT
    END DO
    15            pd=a0*pd
  END IF
END IF
RETURN
END SUBROUTINE dvsa


SUBROUTINE dvla(va,x,pd)

!       ====================================================
!       Purpose: Compute parabolic cylinder functions Dv(x)
!                for large argument
!       Input:   x  --- Argument
!                va --- Order
!       Output:  PD --- Dv(x)
!       Routines called:
!             (1) VVLA for computing Vv(x) for large |x|
!             (2) GAMMA for computing ג(x)
!       ====================================================



DOUBLE PRECISION, INTENT(IN)             :: va
DOUBLE PRECISION, INTENT(IN)             :: x
DOUBLE PRECISION, INTENT(OUT)            :: pd
IMPLICIT DOUBLE PRECISION (a-h,o-z)
pi=3.141592653589793D0
eps=1.0D-12
ep=DEXP(-.25*x*x)
a0=DABS(x)**va*ep
r=1.0D0
pd=1.0D0
DO  k=1,16
  r=-0.5D0*r*(2.0*k-va-1.0)*(2.0*k-va-2.0)/(k*x*x)
  pd=pd+r
  IF (DABS(r/pd) < eps) EXIT
END DO
15      pd=a0*pd
IF (x < 0.0D0) THEN
  x1=-x
  CALL vvla(va,x1,vl)
  CALL gamma(-va,gl)
  pd=pi*vl/gl+DCOS(pi*va)*pd
END IF
RETURN
END SUBROUTINE dvla


SUBROUTINE vvla(va,x,pv)

!       ===================================================
!       Purpose: Compute parabolic cylinder function Vv(x)
!                for large argument
!       Input:   x  --- Argument
!                va --- Order
!       Output:  PV --- Vv(x)
!       Routines called:
!             (1) DVLA for computing Dv(x) for large |x|
!             (2) GAMMA for computing ג(x)
!       ===================================================



DOUBLE PRECISION, INTENT(IN)             :: va
DOUBLE PRECISION, INTENT(IN)             :: x
DOUBLE PRECISION, INTENT(OUT)            :: pv
IMPLICIT DOUBLE PRECISION (a-h,o-z)
pi=3.141592653589793D0
eps=1.0D-12
qe=DEXP(0.25*x*x)
a0=DABS(x)**(-va-1.0D0)*DSQRT(2.0D0/pi)*qe
r=1.0D0
pv=1.0D0
DO  k=1,18
  r=0.5D0*r*(2.0*k+va-1.0)*(2.0*k+va)/(k*x*x)
  pv=pv+r
  IF (DABS(r/pv) < eps) EXIT
END DO
15      pv=a0*pv
IF (x < 0.0D0) THEN
  x1=-x
  CALL dvla(va,x1,pdl)
  CALL gamma(-va,gl)
  dsl=DSIN(pi*va)*DSIN(pi*va)
  pv=dsl*gl/pi*pdl-DCOS(pi*va)*pv
END IF
RETURN
END SUBROUTINE vvla


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
