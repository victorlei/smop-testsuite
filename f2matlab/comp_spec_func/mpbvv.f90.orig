PROGRAM mpbvv
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:12

!       ========================================================
!       Purpose: This program computes the parabolic cylinder
!                functions Vv(x) and Vv'(x) using subroutine
!                PBVV
!       Input:   x --- Argument of Vv(x)
!                v --- Order of Vv(x)
!       Output:  VV(na) --- Vv(x)
!                VP(na) --- Vv'(x)
!                ( na = |n|, v = n+v0, n = int(v), |v0| < 1
!                  n = 0,ס1,ס2,תתת, |n| ף 100 )
!                PVF --- Vv(x)
!                PVD --- Vv'(x)
!       Example: v = 5.5,  x =10.0,  v0 = 0.5,  n = 0,1,2,...,5

!                  n+v0      Vv(x)           Vv'(x)
!                ---------------------------------------
!                  0.5   .18522719D+10   .89761157D+10
!                  1.5   .19016268D+09   .90145854D+09
!                  2.5   .19741946D+08   .91452949D+08
!                  3.5   .20733667D+07   .93751130D+07
!                  4.5   .22038231D+06   .97145511D+06
!                  5.5   .23719356D+05   .10178553D+06

!                Vv(x)= .23719356D+05,  Vv'(x)= .10178553D+06
!       =========================================================

IMPLICIT DOUBLE PRECISION (a-h,o-z)
DIMENSION vv(0:100),vp(0:100)
WRITE(*,*)'Please enter v and  x '
!        READ(*,*)V,X
v=5.5
x=10.0
WRITE(*,20)v,x
nv=INT(v)
v0=v-nv
na=ABS(nv)
CALL pbvv(v,x,vv,vp,pvf,pvd)
WRITE(*,*)
WRITE(*,*)'   v       Vv(x)           Vv''(X)'
WRITE(*,*)'---------------------------------------'
DO  k=0,na
  vk=k*ISIGN(1,nv)+v0
  WRITE(*,30)vk,vv(k),vp(k)
END DO
WRITE(*,*)
WRITE(*,40)v,pvf,pvd
20      FORMAT(1X,'v =',f6.2,',    ','x =',f6.2)
30      FORMAT(1X,f5.1,2D16.8)
40      FORMAT(1X,'v =',f5.1,',  Vv(x)=',d14.8,',   Vv''(X)=',d14.8)
END PROGRAM mpbvv


SUBROUTINE pbvv(v,x,vv,vp,pvf,pvd)

!       ===================================================
!       Purpose: Compute parabolic cylinder functions Vv(x)
!                and their derivatives
!       Input:   x --- Argument of Vv(x)
!                v --- Order of Vv(x)
!       Output:  VV(na) --- Vv(x)
!                VP(na) --- Vv'(x)
!                ( na = |n|, v = n+v0, |v0| < 1
!                  n = 0,ס1,ס2,תתת )
!                PVF --- Vv(x)
!                PVD --- Vv'(x)
!       Routines called:
!             (1) VVSA for computing Vv(x) for small |x|
!             (2) VVLA for computing Vv(x) for large |x|
!       ===================================================


DOUBLE PRECISION, INTENT(IN OUT)         :: v
DOUBLE PRECISION, INTENT(IN)             :: x
DOUBLE PRECISION, INTENT(OUT)            :: vv(0:*)
DOUBLE PRECISION, INTENT(OUT)            :: vp(0:*)
DOUBLE PRECISION, INTENT(OUT)            :: pvf
DOUBLE PRECISION, INTENT(OUT)            :: pvd
IMPLICIT DOUBLE PRECISION (a-h,o-z)


pi=3.141592653589793D0
xa=DABS(x)
vh=v
v=v+DSIGN(1.0D0,v)
nv=INT(v)
v0=v-nv
na=ABS(nv)
qe=DEXP(0.25D0*x*x)
q2p=DSQRT(2.0D0/pi)
IF (na >= 1) ja=1
IF (v <= 0.0) THEN
  IF (v0 == 0.0) THEN
    IF (xa <= 7.5) CALL vvsa(v0,x,pv0)
    IF (xa > 7.5) CALL vvla(v0,x,pv0)
    f0=q2p*qe
    f1=x*f0
    vv(0)=pv0
    vv(1)=f0
    vv(2)=f1
  ELSE
    DO  l=0,ja
      v1=v0-l
      IF (xa <= 7.5) CALL vvsa(v1,x,f1)
      IF (xa > 7.5) CALL vvla(v1,x,f1)
      IF (l == 0) THEN
        f0=f1
      END IF
    END DO
    vv(0)=f0
    vv(1)=f1
  END IF
  kv=2
  IF (v0 == 0.0) kv=3
  DO  k=kv,na
    f=x*f1+(k-v0-2.0D0)*f0
    vv(k)=f
    f0=f1
    f1=f
  END DO
ELSE
  IF (x >= 0.0.AND.x <= 7.5D0) THEN
    v2=v
    IF (v2 < 1.0) v2=v2+1.0D0
    CALL vvsa(v2,x,f1)
    v1=v2-1.0D0
    kv=INT(v2)
    CALL vvsa(v1,x,f0)
    vv(kv)=f1
    vv(kv-1)=f0
    DO  k=kv-2,0,-1
      f=x*f0-(k+v0+2.0D0)*f1
      IF (k <= na) vv(k)=f
      f1=f0
      f0=f
    END DO
  ELSE IF (x > 7.5D0) THEN
    CALL vvla(v0,x,pv0)
    m=100+ABS(na)
    vv(1)=pv0
    f1=0.0D0
    f0=1.0D-40
    DO  k=m,0,-1
      f=x*f0-(k+v0+2.0D0)*f1
      IF (k <= na) vv(k)=f
      f1=f0
      f0=f
    END DO
    s0=pv0/f
    DO  k=0,na
      vv(k)=s0*vv(k)
    END DO
  ELSE
    IF (xa <= 7.5D0) THEN
      CALL vvsa(v0,x,f0)
      v1=v0+1.0
      CALL vvsa(v1,x,f1)
    ELSE
      CALL vvla(v0,x,f0)
      v1=v0+1.0D0
      CALL vvla(v1,x,f1)
    END IF
    vv(0)=f0
    vv(1)=f1
    DO  k=2,na
      f=(x*f1-f0)/(k+v0)
      vv(k)=f
      f0=f1
      f1=f
    END DO
  END IF
END IF
DO  k=0,na-1
  v1=v0+k
  IF (v >= 0.0D0) THEN
    vp(k)=0.5D0*x*vv(k)-(v1+1.0D0)*vv(k+1)
  ELSE
    vp(k)=-0.5D0*x*vv(k)+vv(k+1)
  END IF
END DO
pvf=vv(na-1)
pvd=vp(na-1)
v=vh
RETURN
END SUBROUTINE pbvv


SUBROUTINE vvsa(va,x,pv)

!       ===================================================
!       Purpose: Compute parabolic cylinder function Vv(x)
!                for small argument
!       Input:   x  --- Argument
!                va --- Order
!       Output:  PV --- Vv(x)
!       Routine called : GAMMA for computing ג(x)
!       ===================================================



DOUBLE PRECISION, INTENT(IN)             :: va
DOUBLE PRECISION, INTENT(IN)             :: x
DOUBLE PRECISION, INTENT(OUT)            :: pv
IMPLICIT DOUBLE PRECISION (a-h,o-z)
eps=1.0D-15
pi=3.141592653589793D0
ep=DEXP(-.25D0*x*x)
va0=1.0D0+0.5D0*va
IF (x == 0.0) THEN
  IF (va0 <= 0.0.AND.va0 == INT(va0).OR.va == 0.0) THEN
    pv=0.0D0
  ELSE
    vb0=-0.5D0*va
    sv0=DSIN(va0*pi)
    CALL gamma(va0,ga0)
    pv=2.0D0**vb0*sv0/ga0
  END IF
ELSE
  sq2=DSQRT(2.0D0)
  a0=2.0D0**(-.5D0*va)*ep/(2.0D0*pi)
  sv=DSIN(-(va+.5D0)*pi)
  v1=-.5D0*va
  CALL gamma(v1,g1)
  pv=(sv+1.0D0)*g1
  r=1.0D0
  fac=1.0D0
  DO  m=1,250
    vm=.5D0*(m-va)
    CALL gamma(vm,gm)
    r=r*sq2*x/m
    fac=-fac
    gw=fac*sv+1.0D0
    r1=gw*r*gm
    pv=pv+r1
    IF (DABS(r1/pv) < eps.AND.gw /= 0.0) EXIT
  END DO
  15         pv=a0*pv
END IF
RETURN
END SUBROUTINE vvsa


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
a0=x**va*ep
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
