PROGRAM mcjyvb
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:10

!       =============================================================
!       Purpose: This program computes Bessel functions Jv(z), Yv(z),
!                and their derivatives for a complex argument using
!                subroutine CJYVB
!       Input :  z --- Complex argument
!                v --- Order of Jv(z) and Yv(z)
!                      ( v = n+v0, 0 ף n ף 250, 0 ף v0 < 1 )
!       Output:  CBJ(n) --- Jn+v0(z)
!                CDJ(n) --- Jn+v0'(z)
!                CBY(n) --- Yn+v0(z)
!                CDY(n) --- Yn+v0'(z)
!       Example:
!                v = n +v0,  v0 = 1/3,   z = 4.0 + i 2.0

!     n     Re[Jv(z)]       Im[Jv(z)]      Re[Jv'(z)]      Im[Jv'(z)]
!    -------------------------------------------------------------------
!     0  -.13829878D+01  -.30855145D+00  -.18503756D+00   .13103689D+01
!     1   .82553327D-01  -.12848394D+01  -.12336901D+01   .45079506D-01
!     2   .10843924D+01  -.39871046D+00  -.33046401D+00  -.84574964D+00
!     3   .74348135D+00   .40665987D+00   .45318486D+00  -.42198992D+00
!     4   .17802266D+00   .44526939D+00   .39624497D+00   .97902890D-01
!     5  -.49008598D-01   .21085409D+00   .11784299D+00   .19422044D+00

!     n     Re[Yv(z)]      Im[Yv(z)]       Re[Yv'(z)]      Im[Yv'(z)]
!    -------------------------------------------------------------------
!     0   .34099851D+00  -.13440666D+01  -.13544477D+01  -.15470699D+00
!     1   .13323787D+01   .53735934D-01  -.21467271D-01  -.11807457D+01
!     2   .38393305D+00   .10174248D+01   .91581083D+00  -.33147794D+00
!     3  -.49924295D+00   .71669181D+00   .47786442D+00   .37321597D+00
!     4  -.57179578D+00   .27099289D+00  -.12111686D+00   .23405313D+00
!     5  -.25700924D+00   .24858555D+00  -.43023156D+00  -.13123662D+00
!       =============================================================

IMPLICIT DOUBLE PRECISION (v,x,y)
IMPLICIT COMPLEX*16 (c,z)
DIMENSION cbj(0:250),cdj(0:250),cby(0:250),cdy(0:250)
WRITE(*,*)'  Please enter v, x and y ( z=x+iy )'
!        READ(*,*)V,X,Y
v=5
x=4.0
y=2.0
z=CMPLX(x,y)
n=INT(v)
v0=v-n
WRITE(*,25)v0,x,y
IF (n <= 8) THEN
  ns=1
ELSE
  WRITE(*,*)'  Please enter order step Ns'
!           READ(*,*)NS
  ns=1
END IF
CALL cjyvb(v,z,vm,cbj,cdj,cby,cdy)
nm=INT(vm)
WRITE(*,*)
WRITE(*,*)'  n       Re[Jv(z)]       Im[Jv(z)]',  &
    '       Re[Jv''(Z)]      IM[JV''(Z)]'
WRITE(*,*)' ----------------------------------',  &
    '-----------------------------------'
DO  k=0,nm,ns
  WRITE(*,20) k,cbj(k),cdj(k)
END DO
WRITE(*,*)
WRITE(*,*)'  n       Re[Yv(z)]       Im[Yv(z)]',  &
    '       Re[Yv''(Z)]      IM[YV''(Z)]'
WRITE(*,*)' ----------------------------------',  &
    '-----------------------------------'
DO  k=0,nm,ns
  WRITE(*,20) k,cby(k),cdy(k)
END DO
20      FORMAT(1X,i3,2X,4D16.8)
25      FORMAT(8X,'v = n+v0',',  v0 =',f5.2,',  z =',f7.2,' +',f7.2,'i')
END PROGRAM mcjyvb


SUBROUTINE cjyvb(v,z,vm,cbj,cdj,cby,cdy)

!       ===========================================================
!       Purpose: Compute Bessel functions Jv(z), Yv(z) and their
!                derivatives for a complex argument
!       Input :  z --- Complex argument
!                v --- Order of Jv(z) and Yv(z)
!                      ( v = n+v0, n = 0,1,2,..., 0 ף v0 < 1 )
!       Output:  CBJ(n) --- Jn+v0(z)
!                CDJ(n) --- Jn+v0'(z)
!                CBY(n) --- Yn+v0(z)
!                CDY(n) --- Yn+v0'(z)
!                VM --- Highest order computed
!       Routines called:
!            (1) GAMMA for computing the gamma function
!            (2) MSTA1 and MSTA2 for computing the starting
!                point for backward recurrence
!       ===========================================================


DOUBLE PRECISION, INTENT(IN)             :: v
COMPLEX, INTENT(IN)                      :: z
DOUBLE PRECISION, INTENT(OUT)            :: vm
COMPLEX, INTENT(OUT)                     :: cbj(0:*)
COMPLEX, INTENT(OUT)                     :: cdj(0:*)
COMPLEX, INTENT(OUT)                     :: cby(0:*)
COMPLEX, INTENT(OUT)                     :: cdy(0:*)
IMPLICIT DOUBLE PRECISION (a,b,g,o-y)
IMPLICIT COMPLEX*16 (c,z)


pi=3.141592653589793D0
rp2=.63661977236758D0
ci=(0.0D0,1.0D0)
a0=CDABS(z)
z1=z
z2=z*z
n=INT(v)
v0=v-n
pv0=pi*v0
IF (a0 < 1.0D-100) THEN
  DO  k=0,n
    cbj(k)=(0.0D0,0.0D0)
    cdj(k)=(0.0D0,0.0D0)
    cby(k)=-(1.0D+300,0.0D0)
    cdy(k)=(1.0D+300,0.0D0)
  END DO
  IF (v0 == 0.0) THEN
    cbj(0)=(1.0D0,0.0D0)
    cdj(1)=(0.5D0,0.0D0)
  ELSE
    cdj(0)=(1.0D+300,0.0D0)
  END IF
  vm=v
  RETURN
END IF
IF (REAL(z) < 0.0D0) z1=-z
IF (a0 <= 12.0) THEN
  cjv0=(1.0D0,0.0D0)
  cr=(1.0D0,0.0D0)
  DO  k=1,40
    cr=-0.25D0*cr*z2/(k*(k+v0))
    cjv0=cjv0+cr
    IF (CDABS(cr) < CDABS(cjv0)*1.0D-15) EXIT
  END DO
  20         vg=1.0D0+v0
  CALL gamma(vg,ga)
  ca=(0.5D0*z1)**v0/ga
  cjv0=cjv0*ca
ELSE
  k0=11
  IF (a0 >= 35.0) k0=10
  IF (a0 >= 50.0) k0=8
  vv=4.0D0*v0*v0
  cpz=(1.0D0,0.0D0)
  crp=(1.0D0,0.0D0)
  DO  k=1,k0
    crp=-0.78125D-2*crp*(vv-(4.0*k-3.0)**2.0)*(vv-  &
        (4.0*k-1.0)**2.0)/(k*(2.0*k-1.0)*z2)
    cpz=cpz+crp
  END DO
  cqz=(1.0D0,0.0D0)
  crq=(1.0D0,0.0D0)
  DO  k=1,k0
    crq=-0.78125D-2*crq*(vv-(4.0*k-1.0)**2.0)*(vv-  &
        (4.0*k+1.0)**2.0)/(k*(2.0*k+1.0)*z2)
    cqz=cqz+crq
  END DO
  cqz=0.125D0*(vv-1.0)*cqz/z1
  zk=z1-(0.5D0*v0+0.25D0)*pi
  ca0=CDSQRT(rp2/z1)
  cck=CDCOS(zk)
  csk=CDSIN(zk)
  cjv0=ca0*(cpz*cck-cqz*csk)
  cyv0=ca0*(cpz*csk+cqz*cck)
END IF
IF (a0 <= 12.0) THEN
  IF (v0 /= 0.0) THEN
    cjvn=(1.0D0,0.0D0)
    cr=(1.0D0,0.0D0)
    DO  k=1,40
      cr=-0.25D0*cr*z2/(k*(k-v0))
      cjvn=cjvn+cr
      IF (CDABS(cr) < CDABS(cjvn)*1.0D-15) EXIT
    END DO
    40            vg=1.0D0-v0
    CALL gamma(vg,gb)
    cb=(2.0D0/z1)**v0/gb
    cju0=cjvn*cb
    cyv0=(cjv0*DCOS(pv0)-cju0)/DSIN(pv0)
  ELSE
    cec=CDLOG(z1/2.0D0)+.5772156649015329D0
    cs0=(0.0D0,0.0D0)
    w0=0.0D0
    cr0=(1.0D0,0.0D0)
    DO  k=1,30
      w0=w0+1.0D0/k
      cr0=-0.25D0*cr0/(k*k)*z2
      cs0=cs0+cr0*w0
    END DO
    cyv0=rp2*(cec*cjv0-cs0)
  END IF
END IF
IF (n == 0) n=1
m=msta1(a0,200)
IF (m < n) THEN
  n=m
ELSE
  m=msta2(a0,n,15)
END IF
cf2=(0.0D0,0.0D0)
cf1=(1.0D-100,0.0D0)
DO  k=m,0,-1
  cf=2.0D0*(v0+k+1.0D0)/z1*cf1-cf2
  IF (k <= n) cbj(k)=cf
  cf2=cf1
  cf1=cf
END DO
cs=cjv0/cf
DO  k=0,n
  cbj(k)=cs*cbj(k)
END DO
IF (REAL(z) < 0.0D0) THEN
  cfac0=CDEXP(pv0*ci)
  IF (DIMAG(z) < 0.0D0) THEN
    cyv0=cfac0*cyv0-2.0D0*ci*DCOS(pv0)*cjv0
  ELSE IF (DIMAG(z) > 0.0D0) THEN
    cyv0=cyv0/cfac0+2.0D0*ci*DCOS(pv0)*cjv0
  END IF
  DO  k=0,n
    IF (DIMAG(z) < 0.0D0) THEN
      cbj(k)=CDEXP(-pi*(k+v0)*ci)*cbj(k)
    ELSE IF (DIMAG(z) > 0.0D0) THEN
      cbj(k)=CDEXP(pi*(k+v0)*ci)*cbj(k)
    END IF
  END DO
  z1=z1
END IF
cby(0)=cyv0
DO  k=1,n
  cyy=(cbj(k)*cby(k-1)-2.0D0/(pi*z))/cbj(k-1)
  cby(k)=cyy
END DO
cdj(0)=v0/z*cbj(0)-cbj(1)
DO  k=1,n
  cdj(k)=-(k+v0)/z*cbj(k)+cbj(k-1)
END DO
cdy(0)=v0/z*cby(0)-cby(1)
DO  k=1,n
  cdy(k)=cby(k-1)-(k+v0)/z*cby(k)
END DO
vm=n+v0
RETURN
END SUBROUTINE cjyvb


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
