PROGRAM mlamv
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:11

!       =======================================================
!       Purpose: This program computes the lambda functions
!                for an arbitrary order, and their derivative
!                using subroutine LAMV
!       Input :  x --- Argument of lambda function
!                v --- Order of lambda function
!                      ( v = n+v0, 0 ó n ó 250, 0 ó v0 < 1 )
!       Output:  VL(n) --- Lambda function of order n+v0
!                DL(n) --- Derivative of lambda function
!       Example: x = 10.0

!                   v         Lambda(x)        Lambda'(x)
!                 ------------------------------------------
!                  0.25    -.12510515D+00    -.78558916D-01
!                  0.50    -.54402111D-01    -.78466942D-01
!                  0.75    -.13657787D-01    -.66234027D-01
!                  1.00     .86945492D-02    -.50926063D-01
!                  1.25     .19639729D-01    -.36186221D-01
!                  1.50     .23540083D-01    -.23382658D-01
!                  1.75     .23181910D-01    -.12893894D-01
!                  2.00     .20370425D-01    -.46703503D-02
!                  2.25     .16283799D-01     .15101684D-02
!                  2.50     .11691329D-01     .59243767D-02
!       =======================================================

IMPLICIT DOUBLE PRECISION (a-h,o-z)
DIMENSION vl(0:250),dl(0:250)
WRITE(*,*)'  Please enter v and x '
! READ(*,*)V,X
v=2.5
x=10.0
WRITE(*,20)v,x
IF (v <= 8) THEN
  ns=1
ELSE
  WRITE(*,*)'  Please enter order step Ns'
!    READ(*,*)NS
  ns=1
END IF
WRITE(*,*)
WRITE(*,*) '   v         Lambda(x)        Lambda''(X)'
WRITE(*,*)'-------------------------------------------'
CALL lamv(v,x,vm,vl,dl)
nm=INT(vm)
v0=vm-nm
DO  k=0,nm,ns
  vk=k+v0
  WRITE(*,15)vk,vl(k),dl(k)
END DO
15      FORMAT(1X,f6.2,2D18.8)
20      FORMAT(1X,'v =',f6.2,'    ','x =',f8.2)
END PROGRAM mlamv


SUBROUTINE lamv(v,x,vm,vl,dl)

!       =========================================================
!       Purpose: Compute lambda function with arbitrary order v,
!                and their derivative
!       Input :  x --- Argument of lambda function
!                v --- Order of lambda function
!       Output:  VL(n) --- Lambda function of order n+v0
!                DL(n) --- Derivative of lambda function
!                VM --- Highest order computed
!       Routines called:
!            (1) MSTA1 and MSTA2 for computing the starting
!                point for backward recurrence
!            (2) GAM0 for computing gamma function (|x| ó 1)
!       =========================================================


DOUBLE PRECISION, INTENT(IN)             :: v
DOUBLE PRECISION, INTENT(OUT)            :: x
DOUBLE PRECISION, INTENT(OUT)            :: vm
DOUBLE PRECISION, INTENT(OUT)            :: vl(0:*)
DOUBLE PRECISION, INTENT(OUT)            :: dl(0:*)
IMPLICIT DOUBLE PRECISION (a-h,o-z)


pi=3.141592653589793D0
rp2=0.63661977236758D0
x=DABS(x)
x2=x*x
n=INT(v)
v0=v-n
vm=v
IF (x <= 12.0D0) THEN
  DO  k=0,n
    vk=v0+k
    bk=1.0D0
    r=1.0D0
    DO  i=1,50
      r=-0.25D0*r*x2/(i*(i+vk))
      bk=bk+r
      IF (DABS(r) < DABS(bk)*1.0D-15) EXIT
    END DO
    15            vl(k)=bk
    uk=1.0D0
    r=1.0D0
    DO  i=1,50
      r=-0.25D0*r*x2/(i*(i+vk+1.0D0))
      uk=uk+r
      IF (DABS(r) < DABS(uk)*1.0D-15) EXIT
    END DO
    dl(k)=-0.5D0*x/(vk+1.0D0)*uk
  END DO
  RETURN
END IF
k0=11
IF (x >= 35.0D0) k0=10
IF (x >= 50.0D0) k0=8
DO  j=0,1
  vv=4.0D0*(j+v0)*(j+v0)
  px=1.0D0
  rp=1.0D0
  DO  k=1,k0
    rp=-0.78125D-2*rp*(vv-(4.0*k-3.0)**2.0)*(vv-  &
        (4.0*k-1.0)**2.0)/(k*(2.0*k-1.0)*x2)
    px=px+rp
  END DO
  qx=1.0D0
  rq=1.0D0
  DO  k=1,k0
    rq=-0.78125D-2*rq*(vv-(4.0*k-1.0)**2.0)*(vv-  &
        (4.0*k+1.0)**2.0)/(k*(2.0*k+1.0)*x2)
    qx=qx+rq
  END DO
  qx=0.125D0*(vv-1.0D0)*qx/x
  xk=x-(0.5D0*(j+v0)+0.25D0)*pi
  a0=DSQRT(rp2/x)
  ck=DCOS(xk)
  sk=DSIN(xk)
  IF (j == 0) bjv0=a0*(px*ck-qx*sk)
  IF (j == 1) bjv1=a0*(px*ck-qx*sk)
END DO
IF (v0 == 0.0D0) THEN
  ga=1.0D0
ELSE
  CALL gam0(v0,ga)
  ga=v0*ga
END IF
fac=(2.0D0/x)**v0*ga
vl(0)=bjv0
dl(0)=-bjv1+v0/x*bjv0
vl(1)=bjv1
dl(1)=bjv0-(1.0D0+v0)/x*bjv1
r0=2.0D0*(1.0D0+v0)/x
IF (n <= 1) THEN
  vl(0)=fac*vl(0)
  dl(0)=fac*dl(0)-v0/x*vl(0)
  vl(1)=fac*r0*vl(1)
  dl(1)=fac*r0*dl(1)-(1.0D0+v0)/x*vl(1)
  RETURN
END IF
IF (n >= 2.AND.n <= INT(0.9*x)) THEN
  f0=bjv0
  f1=bjv1
  DO  k=2,n
    f=2.0D0*(k+v0-1.0D0)/x*f1-f0
    f0=f1
    f1=f
    vl(k)=f
  END DO
ELSE IF (n >= 2) THEN
  m=msta1(x,200)
  IF (m < n) THEN
    n=m
  ELSE
    m=msta2(x,n,15)
  END IF
  f2=0.0D0
  f1=1.0D-100
  DO  k=m,0,-1
    f=2.0D0*(v0+k+1.0D0)/x*f1-f2
    IF (k <= n) vl(k)=f
    f2=f1
    f1=f
  END DO
  IF (DABS(bjv0) > DABS(bjv1)) cs=bjv0/f
  IF (DABS(bjv0) <= DABS(bjv1)) cs=bjv1/f2
  DO  k=0,n
    vl(k)=cs*vl(k)
  END DO
END IF
vl(0)=fac*vl(0)
DO  j=1,n
  rc=fac*r0
  vl(j)=rc*vl(j)
  dl(j-1)=-0.5D0*x/(j+v0)*vl(j)
  r0=2.0D0*(j+v0+1)/x*r0
END DO
dl(n)=2.0D0*(v0+n)*(vl(n-1)-vl(n))/x
vm=n+v0
RETURN
END SUBROUTINE lamv


SUBROUTINE gam0 (x,ga)

!       ================================================
!       Purpose: Compute gamma function â(x)
!       Input :  x  --- Argument of â(x)  ( |x| ó 1 )
!       Output:  GA --- â(x)
!       ================================================


DOUBLE PRECISION, INTENT(IN)             :: x
DOUBLE PRECISION, INTENT(OUT)            :: ga
IMPLICIT DOUBLE PRECISION (a-h,o-z)
DIMENSION g(25)
DATA g/1.0D0,0.5772156649015329D0,  &
    -0.6558780715202538D0, -0.420026350340952D-1,  &
    0.1665386113822915D0, -.421977345555443D-1,  &
    -.96219715278770D-2, .72189432466630D-2,  &
    -.11651675918591D-2, -.2152416741149D-3,  &
    .1280502823882D-3, -.201348547807D-4, -.12504934821D-5, .11330272320D-5,  &
    -.2056338417D-6, .61160950D-8, .50020075D-8, -.11812746D-8,  &
    .1043427D-9, .77823D-11, -.36968D-11, .51D-12,  &
    -.206D-13, -.54D-14, .14D-14/

gr=(25)
DO  k=24,1,-1
  gr=gr*x+g(k)
END DO
ga=1.0D0/(gr*x)
RETURN
END SUBROUTINE gam0


INTEGER FUNCTION msta1(x,mp)

!       ===================================================
!       Purpose: Determine the starting point for backward
!                recurrence such that the magnitude of
!                Jn(x) at that point is about 10^(-MP)
!       Input :  x     --- Argument of Jn(x)
!                MP    --- Value of magnitude
!       Output:  MSTA1 --- Starting point
!       ===================================================



DOUBLE PRECISION, INTENT(IN OUT)         :: x
INTEGER, INTENT(IN)                      :: mp
IMPLICIT DOUBLE PRECISION (a-h,o-z)
a0=DABS(x)
n0=INT(1.1*a0)+1
f0=envj(n0,a0)-mp
n1=n0+5
f1=envj(n1,a0)-mp
DO  it=1,20
  nn=n1-(n1-n0)/(1.0D0-f0/f1)
  f=envj(nn,a0)-mp
  IF(ABS(nn-n1) < 1) EXIT
  n0=n1
  f0=f1
  n1=nn
  f1=f
END DO
20     msta1=INT(nn)
RETURN
END FUNCTION msta1


INTEGER FUNCTION msta2(x,n,mp)

!       ===================================================
!       Purpose: Determine the starting point for backward
!                recurrence such that all Jn(x) has MP
!                significant digits
!       Input :  x  --- Argument of Jn(x)
!                n  --- Order of Jn(x)
!                MP --- Significant digit
!       Output:  MSTA2 --- Starting point
!       ===================================================



DOUBLE PRECISION, INTENT(IN OUT)         :: x
INTEGER, INTENT(IN)                      :: n
INTEGER, INTENT(IN)                      :: mp
IMPLICIT DOUBLE PRECISION (a-h,o-z)
a0=DABS(x)
hmp=0.5D0*mp
ejn=envj(n,a0)
IF (ejn <= hmp) THEN
  obj=mp
  n0=INT(1.1*a0)
ELSE
  obj=hmp+ejn
  n0=n
END IF
f0=envj(n0,a0)-obj
n1=n0+5
f1=envj(n1,a0)-obj
DO  it=1,20
  nn=n1-(n1-n0)/(1.0D0-f0/f1)
  f=envj(nn,a0)-obj
  IF (ABS(nn-n1) < 1) EXIT
  n0=n1
  f0=f1
  n1=nn
  f1=f
END DO
20      msta2=INT(nn+10)
RETURN
END FUNCTION msta2

REAL*8 FUNCTION envj(n,x)


INTEGER, INTENT(IN)                      :: n
DOUBLE PRECISION, INTENT(IN)             :: x

envj=0.5D0*DLOG10(6.28D0*n)-n*DLOG10(1.36D0*x/n)
RETURN
END FUNCTION envj
