PROGRAM mcikva
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:10

!       ==============================================================
!       Purpose: This program computes the modified Bessel functions
!                Iv(z), Kv(z) and their derivatives for an arbitrary
!                order and complex argument using subroutine CIKVA
!       Input :  z --- Complex argument z
!                v --- Real order of Iv(z) and Kv(z)
!                      ( v =n+v0,  0 ף n ף 250, 0 ף v0 < 1 )
!       Output:  CBI(n) --- In+v0(z)
!                CDI(n) --- In+v0'(z)
!                CBK(n) --- Kn+v0(z)
!                CDK(n) --- Kn+v0'(z)
!       Example: Compute Iv(z), Kv(z) and their derivatives for
!                v =n+v0, v0=0.25, n =0(1)5, and z =4.0 +i 2.0
!                Computation results:

!                v= n+v0,   v0 = .25,   z =  4.0+ i  2.0

!      n     Re[Iv(z)]      Im[Iv(z)]     Re[Iv'(z)]     Im[Iv'(z)]
!    -----------------------------------------------------------------
!      0  -.19336550D+01  .10328998D+02 -.23119621D+01  .91612230D+01
!      1  -.24735044D+01  .85964317D+01 -.23898329D+01  .78707023D+01
!      2  -.28460107D+01  .54124063D+01 -.24105909D+01  .55204965D+01
!      3  -.23476775D+01  .24445612D+01 -.21145027D+01  .30604463D+01
!      4  -.13829947D+01  .70848630D+00 -.14732387D+01  .12545751D+01
!      5  -.59879982D+00  .64588999D-01 -.78816416D+00  .32629794D+00

!      n     Re[Kv(z)]      Im[Kv(z)]     Re[Kv'(z)]     Im[Kv'(z)]
!     ----------------------------------------------------------------
!      0  -.64820386D-02 -.84715754D-02  .75118612D-02  .89920077D-02
!      1  -.80477525D-02 -.92535355D-02  .96506687D-02  .97789903D-02
!      2  -.12819299D-01 -.11086405D-01  .16310878D-01  .11358076D-01
!      3  -.24574004D-01 -.13462616D-01  .33167751D-01  .11850554D-01
!      4  -.53516204D-01 -.12614703D-01  .75424026D-01  .14407268D-02
!      5  -.12627405D+00  .10581162D-01  .18054884D+00 -.64789392D-01
!       ==============================================================

IMPLICIT DOUBLE PRECISION (a,b,d-h,o-y)
IMPLICIT COMPLEX*16 (c,z)
COMMON  cbi(0:250),cdi(0:250),cbk(0:250),cdk(0:250)
WRITE(*,*)'  Please enter v, x,y ( z=x+iy )'
!        READ(*,*)V,X,Y
v=1.25
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
CALL cikva(v,z,vm,cbi,cdi,cbk,cdk)
nm=INT(vm)
WRITE(*,*)
WRITE(*,*)'   n      Re[Iv(z)]       Im[Iv(z)] ',  &
    '     Re[Iv''(Z)]      IM[IV''(Z)] '
WRITE(*,*)' ---------------------------------------',  &
    '------------------------------'
DO  k=0,nm,ns
  WRITE(*,20)k,cbi(k),cdi(k)
END DO
WRITE(*,*)
WRITE(*,*)'   n      Re[Kv(z)]       Im[Kv(z)] ',  &
    '     Re[Kv''(Z)]      IM[KV''(Z)] '
WRITE(*,*)' ---------------------------------------',  &
    '------------------------------'
DO  k=0,nm,ns
  WRITE(*,20)k,cbk(k),cdk(k)
END DO
20      FORMAT(1X,i4,1X,4D16.8)
25      FORMAT(8X,'v= n+v0',',   ','v0 =',f7.2,',   ','z =',f6.1, '+ i',f6.1)
END PROGRAM mcikva


SUBROUTINE cikva(v,z,vm,cbi,cdi,cbk,cdk)

!       ============================================================
!       Purpose: Compute the modified Bessel functions Iv(z), Kv(z)
!                and their derivatives for an arbitrary order and
!                complex argument
!       Input :  z --- Complex argument
!                v --- Real order of Iv(z) and Kv(z)
!                      ( v = n+v0, n = 0,1,2,תתת, 0 ף v0 < 1 )
!       Output:  CBI(n) --- In+v0(z)
!                CDI(n) --- In+v0'(z)
!                CBK(n) --- Kn+v0(z)
!                CDK(n) --- Kn+v0'(z)
!                VM --- Highest order computed
!       Routines called:
!            (1) GAMMA for computing the gamma function
!            (2) MSTA1 and MSTA2 for computing the starting
!                point for backward recurrence
!       ============================================================


DOUBLE PRECISION, INTENT(IN)             :: v
COMPLEX, INTENT(IN)                      :: z
DOUBLE PRECISION, INTENT(OUT)            :: vm
COMPLEX, INTENT(OUT)                     :: cbi(0:*)
COMPLEX, INTENT(OUT)                     :: cdi(0:*)
COMPLEX, INTENT(OUT)                     :: cbk(0:*)
COMPLEX, INTENT(OUT)                     :: cdk(0:*)
IMPLICIT DOUBLE PRECISION (a,g,p,r,v,w)
IMPLICIT COMPLEX*16 (c,z)


pi=3.141592653589793D0
ci=(0.0D0,1.0D0)
a0=CDABS(z)
z1=z
z2=z*z
n=INT(v)
v0=v-n
piv=pi*v0
vt=4.0D0*v0*v0
IF (n == 0) n=1
IF (a0 < 1.0D-100) THEN
  DO  k=0,n
    cbi(k)=0.0D0
    cdi(k)=0.0D0
    cbk(k)=-1.0D+300
    cdk(k)=1.0D+300
  END DO
  IF (v0 == 0.0) THEN
    cbi(0)=(1.0D0,0.0D0)
    cdi(1)=(0.5D0,0.0D0)
  END IF
  vm=v
  RETURN
END IF
k0=14
IF (a0 >= 35.0) k0=10
IF (a0 >= 50.0) k0=8
IF (REAL(z) < 0.0) z1=-z
IF (a0 < 18.0) THEN
  IF (v0 == 0.0) THEN
    ca1=(1.0D0,0.0D0)
  ELSE
    v0p=1.0D0+v0
    CALL gamma(v0p,gap)
    ca1=(0.5D0*z1)**v0/gap
  END IF
  ci0=(1.0D0,0.0D0)
  cr=(1.0D0,0.0D0)
  DO  k=1,50
    cr=0.25D0*cr*z2/(k*(k+v0))
    ci0=ci0+cr
    IF (CDABS(cr) < CDABS(ci0)*1.0D-15) EXIT
  END DO
  20         cbi0=ci0*ca1
ELSE
  ca=CDEXP(z1)/CDSQRT(2.0D0*pi*z1)
  cs=(1.0D0,0.0D0)
  cr=(1.0D0,0.0D0)
  DO  k=1,k0
    cr=-0.125D0*cr*(vt-(2.0D0*k-1.0D0)**2.0)/(k*z1)
    cs=cs+cr
  END DO
  cbi0=ca*cs
END IF
m=msta1(a0,200)
IF (m < n) THEN
  n=m
ELSE
  m=msta2(a0,n,15)
END IF
cf2=(0.0D0,0.0D0)
cf1=(1.0D-100,0.0D0)
DO  k=m,0,-1
  cf=2.0D0*(v0+k+1.0D0)/z1*cf1+cf2
  IF (k <= n) cbi(k)=cf
  cf2=cf1
  cf1=cf
END DO
cs=cbi0/cf
DO  k=0,n
  cbi(k)=cs*cbi(k)
END DO
IF (a0 <= 9.0) THEN
  IF (v0 == 0.0) THEN
    ct=-CDLOG(0.5D0*z1)-0.5772156649015329D0
    cs=(0.0D0,0.0D0)
    w0=0.0D0
    cr=(1.0D0,0.0D0)
    DO  k=1,50
      w0=w0+1.0D0/k
      cr=0.25D0*cr/(k*k)*z2
      cp=cr*(w0+ct)
      cs=cs+cp
      IF (k >= 10.AND.CDABS(cp/cs) < 1.0D-15) EXIT
    END DO
    45            cbk0=ct+cs
  ELSE
    v0n=1.0D0-v0
    CALL gamma(v0n,gan)
    ca2=1.0D0/(gan*(0.5D0*z1)**v0)
    ca1=(0.5D0*z1)**v0/gap
    csu=ca2-ca1
    cr1=(1.0D0,0.0D0)
    cr2=(1.0D0,0.0D0)
    DO  k=1,50
      cr1=0.25D0*cr1*z2/(k*(k-v0))
      cr2=0.25D0*cr2*z2/(k*(k+v0))
      csu=csu+ca2*cr1-ca1*cr2
      ws=CDABS(csu)
      IF (k >= 10.AND.DABS(ws-ws0)/ws < 1.0D-15) EXIT
      ws0=ws
    END DO
    55            cbk0=0.5D0*pi*csu/DSIN(piv)
  END IF
ELSE
  cb=CDEXP(-z1)*CDSQRT(0.5D0*pi/z1)
  cs=(1.0D0,0.0D0)
  cr=(1.0D0,0.0D0)
  DO  k=1,k0
    cr=0.125D0*cr*(vt-(2.0D0*k-1.0D0)**2.0)/(k*z1)
    cs=cs+cr
  END DO
  cbk0=cb*cs
END IF
cbk1=(1.0D0/z1-cbi(1)*cbk0)/cbi(0)
cbk(0)=cbk0
cbk(1)=cbk1
cg0=cbk0
cg1=cbk1
DO  k=2,n
  cgk=2.0D0*(v0+k-1.0D0)/z1*cg1+cg0
  cbk(k)=cgk
  cg0=cg1
  cg1=cgk
END DO
IF (REAL(z) < 0.0) THEN
  DO  k=0,n
    cvk=CDEXP((k+v0)*pi*ci)
    IF (DIMAG(z) < 0.0D0) THEN
      cbk(k)=cvk*cbk(k)+pi*ci*cbi(k)
      cbi(k)=cbi(k)/cvk
    ELSE IF (DIMAG(z) > 0.0) THEN
      cbk(k)=cbk(k)/cvk-pi*ci*cbi(k)
      cbi(k)=cvk*cbi(k)
    END IF
  END DO
END IF
cdi(0)=v0/z*cbi(0)+cbi(1)
cdk(0)=v0/z*cbk(0)-cbk(1)
DO  k=1,n
  cdi(k)=-(k+v0)/z*cbi(k)+cbi(k-1)
  cdk(k)=-(k+v0)/z*cbk(k)-cbk(k-1)
END DO
vm=n+v0
RETURN
END SUBROUTINE cikva


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
