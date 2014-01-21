PROGRAM mcjyva
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:10

!     ===============================================================
!     Purpose: This program computes Bessel functions Jv(z), Yv(z),
!     and their derivatives for a complex argument using
!     subroutine CJYVA
!     Input :  z --- Complex argument
!     v --- Order of Jv(z) and Yv(z)
!     ( v = n+v0, 0 ף n ף 250, 0 ף v0 < 1 )
!     Output:  CBJ(n) --- Jn+v0(z)
!     CDJ(n) --- Jn+v0'(z)
!     CBY(n) --- Yn+v0(z)
!     CDY(n) --- Yn+v0'(z)
!     Example:
!     v = n +v0,  v0 = 1/3,   z = 4.0 + i 2.0

!     n     Re[Jv(z)]       Im[Jv(z)]      Re[Jv'(z)]      Im[Jv'(z)]
!     ------------------------------------------------------------------
!     0  -.13829878D+01  -.30855145D+00  -.18503756D+00   .13103689D+01
!     1   .82553327D-01  -.12848394D+01  -.12336901D+01   .45079506D-01
!     2   .10843924D+01  -.39871046D+00  -.33046401D+00  -.84574964D+00
!     3   .74348135D+00   .40665987D+00   .45318486D+00  -.42198992D+00
!     4   .17802266D+00   .44526939D+00   .39624497D+00   .97902890D-01
!     5  -.49008598D-01   .21085409D+00   .11784299D+00   .19422044D+00

!     n     Re[Yv(z)]      Im[Yv(z)]       Re[Yv'(z)]      Im[Yv'(z)]
!     ------------------------------------------------------------------
!     0   .34099851D+00  -.13440666D+01  -.13544477D+01  -.15470699D+00
!     1   .13323787D+01   .53735934D-01  -.21467271D-01  -.11807457D+01
!     2   .38393305D+00   .10174248D+01   .91581083D+00  -.33147794D+00
!     3  -.49924295D+00   .71669181D+00   .47786442D+00   .37321597D+00
!     4  -.57179578D+00   .27099289D+00  -.12111686D+00   .23405313D+00
!     5  -.25700924D+00   .24858555D+00  -.43023156D+00  -.13123662D+00
!     ===============================================================

IMPLICIT DOUBLE PRECISION (v,x,y)
IMPLICIT COMPLEX*16 (c,z)
COMMON cbj(0:251),cdj(0:251),cby(0:251),cdy(0:251)
WRITE(*,*)'  Please enter v, x and y ( z=x+iy )'
!     READ(*,*)V,X,Y
v=5+1.0/3.0
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
!      READ(*,*)NS
  ns=1
END IF
CALL cjyva(v,z,vm,cbj,cdj,cby,cdy)
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
20   FORMAT(1X,i3,2X,4D16.8)
25   FORMAT(8X,'v = n+v0',',  v0 =',f5.2,',  z =',f7.2,' +',f7.2,'i')
END PROGRAM mcjyva


SUBROUTINE cjyva(v,z,vm,cbj,cdj,cby,cdy)

!     ===========================================================
!     Purpose: Compute Bessel functions Jv(z), Yv(z) and their
!     derivatives for a complex argument
!     Input :  z --- Complex argument
!     v --- Order of Jv(z) and Yv(z)
!     ( v = n+v0, n = 0,1,2,..., 0 ף v0 < 1 )
!     Output:  CBJ(n) --- Jn+v0(z)
!     CDJ(n) --- Jn+v0'(z)
!     CBY(n) --- Yn+v0(z)
!     CDY(n) --- Yn+v0'(z)
!     VM --- Highest order computed
!     Routines called:
!     (1) GAMMA for computing the gamma function
!     (2) MSTA1 and MSTA2 for computing the starting
!     point for backward recurrence
!     ===========================================================


IMPLICIT DOUBLE PRECISION (a,b,g,o-y)
IMPLICIT COMPLEX*16 (c,z)
DOUBLE PRECISION, INTENT(IN)             :: v
COMPLEX, INTENT(IN)                      :: z
DOUBLE PRECISION, INTENT(OUT)            :: vm
COMPLEX, INTENT(OUT)                     :: cbj(0:*)
COMPLEX, INTENT(OUT)                     :: cdj(0:*)
COMPLEX, INTENT(OUT)                     :: cby(0:*)
COMPLEX, INTENT(OUT)                     :: cdy(0:*)


lb0=10
pi=3.141592653589793D0
rp2=.63661977236758D0
ci=(0.0D0,1.0D0)
a0=CDABS(z)
z1=z
z2=z*z
n=INT(v)
v0=v-n
pv0=pi*v0
pv1=pi*(1.0D0+v0)
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
IF (REAL(z) < 0.0) z1=-z
IF (a0 <= 12.0) THEN
  DO  l=0,1
    vl=v0+l
    cjvl=(1.0D0,0.0D0)
    cr=(1.0D0,0.0D0)
    DO  k=1,40
      cr=-0.25D0*cr*z2/(k*(k+vl))
      cjvl=cjvl+cr
      IF (CDABS(cr) < CDABS(cjvl)*1.0D-15) EXIT
    END DO
    20     vg=1.0D0+vl
    CALL gamma(vg,ga)
    ca=(0.5D0*z1)**vl/ga
    IF (l == 0) cjv0=cjvl*ca
    IF (l == 1) cjv1=cjvl*ca
  END DO
ELSE
  k0=11
  IF (a0 >= 35.0) k0=10
  IF (a0 >= 50.0) k0=8
  DO  j=0,1
    vv=4.0D0*(j+v0)*(j+v0)
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
    zk=z1-(0.5D0*(j+v0)+0.25D0)*pi
    ca0=CDSQRT(rp2/z1)
    cck=CDCOS(zk)
    csk=CDSIN(zk)
    IF (j == 0) THEN
      cjv0=ca0*(cpz*cck-cqz*csk)
      cyv0=ca0*(cpz*csk+cqz*cck)
    ELSE IF (j == 1) THEN
      cjv1=ca0*(cpz*cck-cqz*csk)
      cyv1=ca0*(cpz*csk+cqz*cck)
    END IF
  END DO
END IF
IF (a0 <= 12.0) THEN
  IF (v0 /= 0.0) THEN
    DO  l=0,1
      vl=v0+l
      cjvl=(1.0D0,0.0D0)
      cr=(1.0D0,0.0D0)
      DO  k=1,40
        cr=-0.25D0*cr*z2/(k*(k-vl))
        cjvl=cjvl+cr
        IF (CDABS(cr) < CDABS(cjvl)*1.0D-15) EXIT
      END DO
      50      vg=1.0D0-vl
      CALL gamma(vg,gb)
      cb=(2.0D0/z1)**vl/gb
      IF (l == 0) cju0=cjvl*cb
      IF (l == 1) cju1=cjvl*cb
    END DO
    cyv0=(cjv0*DCOS(pv0)-cju0)/DSIN(pv0)
    cyv1=(cjv1*DCOS(pv1)-cju1)/DSIN(pv1)
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
    cs1=(1.0D0,0.0D0)
    w1=0.0D0
    cr1=(1.0D0,0.0D0)
    DO  k=1,30
      w1=w1+1.0D0/k
      cr1=-0.25D0*cr1/(k*(k))*z2
      cs1=cs1+cr1*(2.0D0*w1+1.0D0/(k+1.0D0))
    END DO
    cyv1=rp2*(cec*cjv1-1.0D0/z1-0.25D0*z1*cs1)
  END IF
END IF
IF (REAL(z) < 0.0D0) THEN
  cfac0=CDEXP(pv0*ci)
  cfac1=CDEXP(pv1*ci)
  IF (DIMAG(z) < 0.0D0) THEN
    cyv0=cfac0*cyv0-2.0D0*ci*DCOS(pv0)*cjv0
    cyv1=cfac1*cyv1-2.0D0*ci*DCOS(pv1)*cjv1
    cjv0=cjv0/cfac0
    cjv1=cjv1/cfac1
  ELSE IF (DIMAG(z) > 0.0D0) THEN
    cyv0=cyv0/cfac0+2.0D0*ci*DCOS(pv0)*cjv0
    cyv1=cyv1/cfac1+2.0D0*ci*DCOS(pv1)*cjv1
    cjv0=cfac0*cjv0
    cjv1=cfac1*cjv1
  END IF
END IF
cbj(0)=cjv0
cbj(1)=cjv1
IF (n >= 2.AND.n <= INT(0.25*a0)) THEN
  cf0=cjv0
  cf1=cjv1
  DO  k=2,n
    cf=2.0D0*(k+v0-1.0D0)/z*cf1-cf0
    cbj(k)=cf
    cf0=cf1
    cf1=cf
  END DO
ELSE IF (n >= 2) THEN
  m=msta1(a0,200)
  IF (m < n) THEN
    n=m
  ELSE
    m=msta2(a0,n,15)
  END IF
  cf2=(0.0D0,0.0D0)
  cf1=(1.0D-100,0.0D0)
  DO  k=m,0,-1
    cf=2.0D0*(v0+k+1.0D0)/z*cf1-cf2
    IF (k <= n) cbj(k)=cf
    cf2=cf1
    cf1=cf
  END DO
  IF (CDABS(cjv0) > CDABS(cjv1)) cs=cjv0/cf
  IF (CDABS(cjv0) <= CDABS(cjv1)) cs=cjv1/cf2
  DO  k=0,n
    cbj(k)=cs*cbj(k)
  END DO
END IF
cdj(0)=v0/z*cbj(0)-cbj(1)
DO  k=1,n
  cdj(k)=-(k+v0)/z*cbj(k)+cbj(k-1)
END DO
cby(0)=cyv0
cby(1)=cyv1
ya0=CDABS(cyv0)
lb=0
cg0=cyv0
cg1=cyv1
DO  k=2,n
  cyk=2.0D0*(v0+k-1.0D0)/z*cg1-cg0
  IF (.NOT.(CDABS(cyk) > 1.0D+290)) THEN
    yak=CDABS(cyk)
    ya1=CDABS(cg0)
    IF (yak < ya0.AND.yak < ya1) lb=k
    cby(k)=cyk
    cg0=cg1
    cg1=cyk
    PRINT *,'cg1=',cg1
  END IF
END DO
DO
  IF (lb <= 4.OR.DIMAG(z) == 0.0D0) EXIT
  95   IF (lb == lb0) EXIT
  ch2=(1.0D0,0.0D0)
  ch1=(0.0D0,0.0D0)
  lb0=lb
  DO  k=lb,1,-1
    ch0=2.0D0*(k+v0)/z*ch1-ch2
    ch2=ch1
    ch1=ch0
  END DO
  cp12=ch0
  cp22=ch2
  ch2=(0.0D0,0.0D0)
  ch1=(1.0D0,0.0D0)
  DO  k=lb,1,-1
    ch0=2.0D0*(k+v0)/z*ch1-ch2
    ch2=ch1
    ch1=ch0
  END DO
  cp11=ch0
  cp21=ch2
  IF (lb == n) cbj(lb+1)=2.0D0*(lb+v0)/z*cbj(lb)-cbj(lb-1)
  IF (CDABS(cbj(0)) > CDABS(cbj(1))) THEN
    cby(lb+1)=(cbj(lb+1)*cyv0-2.0D0*cp11/(pi*z))/cbj(0)
    cby(lb)=(cbj(lb)*cyv0+2.0D0*cp12/(pi*z))/cbj(0)
  ELSE
    cby(lb+1)=(cbj(lb+1)*cyv1-2.0D0*cp21/(pi*z))/cbj(1)
    cby(lb)=(cbj(lb)*cyv1+2.0D0*cp22/(pi*z))/cbj(1)
  END IF
  cyl2=cby(lb+1)
  cyl1=cby(lb)
  DO  k=lb-1,0,-1
    cylk=2.0D0*(k+v0+1.0D0)/z*cyl1-cyl2
    cby(k)=cylk
    cyl2=cyl1
    cyl1=cylk
  END DO
  cyl1=cby(lb)
  cyl2=cby(lb+1)
  DO  k=lb+1,n-1
    cylk=2.0D0*(k+v0)/z*cyl2-cyl1
    cby(k+1)=cylk
    cyl1=cyl2
    cyl2=cylk
  END DO
  DO  k=2,n
    wa=CDABS(cby(k))
    IF (wa < CDABS(cby(k-1))) lb=k
  END DO
END DO
cdy(0)=v0/z*cby(0)-cby(1)
DO  k=1,n
  cdy(k)=cby(k-1)-(k+v0)/z*cby(k)
END DO
vm=n+v0
RETURN
END SUBROUTINE cjyva


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


INTEGER FUNCTION msta1(x,mp)

!     ===================================================
!     Purpose: Determine the starting point for backward
!     recurrence such that the magnitude of
!     Jn(x) at that point is about 10^(-MP)
!     Input :  x     --- Argument of Jn(x)
!     MP    --- Value of magnitude
!     Output:  MSTA1 --- Starting point
!     ===================================================


IMPLICIT DOUBLE PRECISION (a-h,o-z)
DOUBLE PRECISION, INTENT(IN OUT)         :: x
INTEGER, INTENT(IN)                      :: mp
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
20   msta1=INT(nn)
RETURN
END FUNCTION msta1


INTEGER FUNCTION msta2(x,n,mp)

!     ===================================================
!     Purpose: Determine the starting point for backward
!     recurrence such that all Jn(x) has MP
!     significant digits
!     Input :  x  --- Argument of Jn(x)
!     n  --- Order of Jn(x)
!     MP --- Significant digit
!     Output:  MSTA2 --- Starting point
!     ===================================================


IMPLICIT DOUBLE PRECISION (a-h,o-z)
DOUBLE PRECISION, INTENT(IN OUT)         :: x
INTEGER, INTENT(IN)                      :: n
INTEGER, INTENT(IN)                      :: mp

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
20   msta2=INT(nn+10)
RETURN
END FUNCTION msta2

REAL*8 FUNCTION envj(n,x)


INTEGER, INTENT(IN)                      :: n
DOUBLE PRECISION, INTENT(IN)             :: x

envj=0.5D0*DLOG10(6.28D0*n)-n*DLOG10(1.36D0*x/n)
RETURN
END FUNCTION envj
