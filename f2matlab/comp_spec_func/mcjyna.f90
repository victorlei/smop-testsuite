PROGRAM mcjyna
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:10

!     ================================================================
!     Purpose: This program computes Bessel functions Jn(z), Yn(z)
!     and their derivatives for a complex argument using
!     subroutine CJYNA
!     Input :  z --- Complex argument of Jn(z) and Yn(z)
!     n --- Order of Jn(z) and Yn(z)
!     ( n = 0,1,תתת, n ף 250 )
!     Output:  CBJ(n) --- Jn(z)
!     CDJ(n) --- Jn'(z)
!     CBY(n) --- Yn(z)
!     CDY(n) --- Yn'(z)
!     Eaxmple: z = 4.0 + i 2.0

!     n     Re[Jn(z)]       Im[Jn(z)]       Re[Jn'(z)]      Im[Jn'(z)]
!     ------------------------------------------------------------------
!     -
!     0  -.13787022D+01   .39054236D+00   .50735255D+00   .12263041D+01
!     1  -.50735255D+00  -.12263041D+01  -.11546013D+01   .58506793D+00
!     2   .93050039D+00  -.77959350D+00  -.72363400D+00  -.72836666D+00
!     3   .93991546D+00   .23042918D+00   .29742236D+00  -.63587637D+00
!     4   .33565567D+00   .49215925D+00   .47452722D+00  -.29035945D-01
!     5  -.91389835D-02   .28850107D+00   .20054412D+00   .19908868D+00

!     n     Re[Yn(z)]       Im[Yn(z)]       Re[Yn'(z)]      Im[Yn'(z)]
!     ------------------------------------------------------------------
!     --
!     0  -.38145893D+00  -.13291649D+01  -.12793101D+01   .51220420D+00
!     1   .12793101D+01  -.51220420D+00  -.58610052D+00  -.10987930D+01
!     2   .79074211D+00   .86842120D+00   .78932897D+00  -.70142425D+00
!     3  -.29934789D+00   .89064431D+00   .70315755D+00   .24423024D+00
!     4  -.61557299D+00   .37996071D+00   .41126221D-01   .34044655D+00
!     5  -.38160033D+00   .20975121D+00  -.33884827D+00  -.20590670D-01

!     z = 20.0 + i 10.0 ,      Nmax = 5

!     n     Re[Jn(z)]       Im[Jn(z)]       Re[Jn'(z)]      Im[Jn'(z)]
!     ------------------------------------------------------------------
!     --
!     0   .15460268D+04  -.10391216D+04  -.10601232D+04  -.15098284D+04
!     1   .10601232D+04   .15098284D+04   .14734253D+04  -.10783122D+04
!     2  -.14008238D+04   .11175029D+04   .11274890D+04   .13643952D+04
!     3  -.11948548D+04  -.12189620D+04  -.11843035D+04   .11920871D+04
!     4   .96778325D+03  -.12666712D+04  -.12483664D+04  -.93887194D+03
!     5   .13018781D+04   .65878188D+03   .64152944D+03  -.12682398D+04

!     n     Re[Yn(z)]       Im[Yn(z)]       Re[Yn'(z)]      Im[Yn'(z)]
!     ------------------------------------------------------------------
!     --
!     0   .10391216D+04   .15460268D+04   .15098284D+04  -.10601232D+04
!     1  -.15098284D+04   .10601232D+04   .10783122D+04   .14734253D+04
!     2  -.11175029D+04  -.14008238D+04  -.13643952D+04   .11274890D+04
!     3   .12189620D+04  -.11948548D+04  -.11920871D+04  -.11843035D+04
!     4   .12666712D+04   .96778324D+03   .93887194D+03  -.12483664D+04
!     5  -.65878189D+03   .13018781D+04   .12682398D+04   .64152944D+03
!     ================================================================

IMPLICIT DOUBLE PRECISION (a,b,d-h,o-y)
IMPLICIT COMPLEX*16 (c,z)
COMMON cbj(0:251),cdj(0:251),cby(0:251),cdy(0:251)
WRITE(*,*)'  Please enter n, x,y (z=x+iy) '
!     READ(*,*)N,X,Y
n=5
x=4.0
y=2.0
z=CMPLX(x,y)
WRITE(*,40)x,y,n
IF (n <= 8) THEN
  ns=1
ELSE
  WRITE(*,*)'  Please enter order step Ns'
!       READ(*,*)NS
  ns=1
END IF
CALL cjyna(n,z,nm,cbj,cdj,cby,cdy)
WRITE(*,*)
WRITE(*,*)'   n     Re[Jn(z)]       Im[Jn(z)]',  &
    '       Re[Jn''(Z)]      IM[JN''(Z)]'
WRITE(*,*)' -------------------------------------',  &
    '-------------------------------'
DO  k=0,nm,ns
  WRITE(*,30)k,cbj(k),cdj(k)
END DO
WRITE(*,*)
WRITE(*,*)'   n     Re[Yn(z)]       Im[Yn(z)]',  &
    '       Re[Yn''(Z)]      IM[YN''(Z)]'
WRITE(*,*)' -------------------------------------',  &
    '-------------------------------'
DO  k=0,nm,ns
  WRITE(*,30)k,cby(k),cdy(k)
END DO
30   FORMAT(1X,i4,4D16.8)
40   FORMAT(3X,3HZ =,f5.1,' + i ',f5.1,' ,',6X,6HNMAX =,i3)
END PROGRAM mcjyna


SUBROUTINE cjyna(n,z,nm,cbj,cdj,cby,cdy)

!     =======================================================
!     Purpose: Compute Bessel functions Jn(z), Yn(z) and
!     their derivatives for a complex argument
!     Input :  z --- Complex argument of Jn(z) and Yn(z)
!     n --- Order of Jn(z) and Yn(z)
!     Output:  CBJ(n) --- Jn(z)
!     CDJ(n) --- Jn'(z)
!     CBY(n) --- Yn(z)
!     CDY(n) --- Yn'(z)
!     NM --- Highest order computed
!     Rouitines called:
!     (1) CJY01 to calculate J0(z), J1(z), Y0(z), Y1(z)
!     (2) MSTA1 and MSTA2 to calculate the starting
!     point for backward recurrence
!     =======================================================


INTEGER, INTENT(IN)                      :: n
COMPLEX, INTENT(IN)                      :: z
INTEGER, INTENT(OUT)                     :: nm
COMPLEX, INTENT(OUT)                     :: cbj(0:n)
COMPLEX, INTENT(OUT)                     :: cdj(0:n)
COMPLEX, INTENT(OUT)                     :: cby(0:n)
COMPLEX, INTENT(OUT)                     :: cdy(0:n)
IMPLICIT DOUBLE PRECISION (a,b,e,p,r,w,y)
IMPLICIT COMPLEX*16 (c,z)


lb0=0.0
pi=3.141592653589793D0
a0=CDABS(z)
nm=n
IF (a0 < 1.0D-100) THEN
  DO  k=0,n
    cbj(k)=(0.0D0,0.0D0)
    cdj(k)=(0.0D0,0.0D0)
    cby(k)=-(1.0D+300,0.0D0)
    cdy(k)=(1.0D+300,0.0D0)
  END DO
  cbj(0)=(1.0D0,0.0D0)
  cdj(1)=(0.5D0,0.0D0)
  RETURN
END IF
CALL cjy01(z,cbj0,cdj0,cbj1,cdj1,cby0,cdy0,cby1,cdy1)
cbj(0)=cbj0
cbj(1)=cbj1
cby(0)=cby0
cby(1)=cby1
cdj(0)=cdj0
cdj(1)=cdj1
cdy(0)=cdy0
cdy(1)=cdy1
IF (n <= 1) RETURN
IF (n < INT(0.25*a0)) THEN
  cj0=cbj0
  cj1=cbj1
  DO  k=2,n
    cjk=2.0D0*(k-1.0D0)/z*cj1-cj0
    cbj(k)=cjk
    cj0=cj1
    cj1=cjk
  END DO
ELSE
  m=msta1(a0,200)
  IF (m < n) THEN
    nm=m
  ELSE
    m=msta2(a0,n,15)
  END IF
  cf2=(0.0D0,0.0D0)
  cf1=(1.0D-100,0.0D0)
  DO  k=m,0,-1
    cf=2.0D0*(k+1.0D0)/z*cf1-cf2
    IF (k <= nm) cbj(k)=cf
    cf2=cf1
    cf1=cf
  END DO
  IF (CDABS(cbj0) > CDABS(cbj1)) THEN
    cs=cbj0/cf
  ELSE
    cs=cbj1/cf2
  END IF
  DO  k=0,nm
    cbj(k)=cs*cbj(k)
  END DO
END IF
DO  k=2,nm
  cdj(k)=cbj(k-1)-k/z*cbj(k)
END DO
ya0=CDABS(cby0)
lb=0
cg0=cby0
cg1=cby1
DO  k=2,nm
  cyk=2.0D0*(k-1.0D0)/z*cg1-cg0
  IF (.NOT.(CDABS(cyk) > 1.0D+290)) THEN
    yak=CDABS(cyk)
    ya1=CDABS(cg0)
    IF (yak < ya0.AND.yak < ya1) lb=k
    cby(k)=cyk
    cg0=cg1
    cg1=cyk
  END IF
END DO
DO
  IF (lb <= 4.OR.DIMAG(z) == 0.0D0) EXIT
  95    IF (lb == lb0) EXIT
  ch2=(1.0D0,0.0D0)
  ch1=(0.0D0,0.0D0)
  lb0=lb
  DO  k=lb,1,-1
    ch0=2.0D0*k/z*ch1-ch2
    ch2=ch1
    ch1=ch0
  END DO
  cp12=ch0
  cp22=ch2
  ch2=(0.0D0,0.0D0)
  ch1=(1.0D0,0.0D0)
  DO  k=lb,1,-1
    ch0=2.0D0*k/z*ch1-ch2
    ch2=ch1
    ch1=ch0
  END DO
  cp11=ch0
  cp21=ch2
  IF (lb == nm) cbj(lb+1)=2.0D0*lb/z*cbj(lb)-cbj(lb-1)
  IF (CDABS(cbj(0)) > CDABS(cbj(1))) THEN
    cby(lb+1)=(cbj(lb+1)*cby0-2.0D0*cp11/(pi*z))/cbj(0)
    cby(lb)=(cbj(lb)*cby0+2.0D0*cp12/(pi*z))/cbj(0)
  ELSE
    cby(lb+1)=(cbj(lb+1)*cby1-2.0D0*cp21/(pi*z))/cbj(1)
    cby(lb)=(cbj(lb)*cby1+2.0D0*cp22/(pi*z))/cbj(1)
  END IF
  cyl2=cby(lb+1)
  cyl1=cby(lb)
  DO  k=lb-1,0,-1
    cylk=2.0D0*(k+1.0D0)/z*cyl1-cyl2
    cby(k)=cylk
    cyl2=cyl1
    cyl1=cylk
  END DO
  cyl1=cby(lb)
  cyl2=cby(lb+1)
  DO  k=lb+1,nm-1
    cylk=2.0D0*k/z*cyl2-cyl1
    cby(k+1)=cylk
    cyl1=cyl2
    cyl2=cylk
  END DO
  DO  k=2,nm
    wa=CDABS(cby(k))
    IF (wa < CDABS(cby(k-1))) lb=k
  END DO
END DO
DO  k=2,nm
  cdy(k)=cby(k-1)-k/z*cby(k)
END DO
RETURN
END SUBROUTINE cjyna


SUBROUTINE cjy01(z,cbj0,cdj0,cbj1,cdj1,cby0,cdy0,cby1,cdy1)

!     ===========================================================
!     Purpose: Compute complex Bessel functions J0(z), J1(z)
!     Y0(z), Y1(z), and their derivatives
!     Input :  z --- Complex argument
!     Output:  CBJ0 --- J0(z)
!     CDJ0 --- J0'(z)
!     CBJ1 --- J1(z)
!     CDJ1 --- J1'(z)
!     CBY0 --- Y0(z)
!     CDY0 --- Y0'(z)
!     CBY1 --- Y1(z)
!     CDY1 --- Y1'(z)
!     ===========================================================


COMPLEX, INTENT(IN)                      :: z
COMPLEX, INTENT(OUT)                     :: cbj0
COMPLEX, INTENT(OUT)                     :: cdj0
COMPLEX, INTENT(OUT)                     :: cbj1
COMPLEX, INTENT(OUT)                     :: cdj1
COMPLEX, INTENT(OUT)                     :: cby0
COMPLEX, INTENT(OUT)                     :: cdy0
COMPLEX, INTENT(OUT)                     :: cby1
COMPLEX, INTENT(OUT)                     :: cdy1
IMPLICIT DOUBLE PRECISION (a,b,e,p,r,w)
IMPLICIT COMPLEX*16 (c,z)
DIMENSION a(12),b(12),a1(12),b1(12)

pi=3.141592653589793D0
el=0.5772156649015329D0
rp2=2.0D0/pi
ci=(0.0D0,1.0D0)
a0=CDABS(z)
z2=z*z
z1=z
IF (a0 == 0.0D0) THEN
  cbj0=(1.0D0,0.0D0)
  cbj1=(0.0D0,0.0D0)
  cdj0=(0.0D0,0.0D0)
  cdj1=(0.5D0,0.0D0)
  cby0=-(1.0D300,0.0D0)
  cby1=-(1.0D300,0.0D0)
  cdy0=(1.0D300,0.0D0)
  cdy1=(1.0D300,0.0D0)
  RETURN
END IF
IF (REAL(z) < 0.0) z1=-z
IF (a0 <= 12.0) THEN
  cbj0=(1.0D0,0.0D0)
  cr=(1.0D0,0.0D0)
  DO  k=1,40
    cr=-0.25D0*cr*z2/(k*k)
    cbj0=cbj0+cr
    IF (CDABS(cr/cbj0) < 1.0D-15) EXIT
  END DO
  15    cbj1=(1.0D0,0.0D0)
  cr=(1.0D0,0.0D0)
  DO  k=1,40
    cr=-0.25D0*cr*z2/(k*(k+1.0D0))
    cbj1=cbj1+cr
    IF (CDABS(cr/cbj1) < 1.0D-15) EXIT
  END DO
  25    cbj1=0.5D0*z1*cbj1
  w0=0.0D0
  cr=(1.0D0,0.0D0)
  cs=(0.0D0,0.0D0)
  DO  k=1,40
    w0=w0+1.0D0/k
    cr=-0.25D0*cr/(k*k)*z2
    cp=cr*w0
    cs=cs+cp
    IF (CDABS(cp/cs) < 1.0D-15) EXIT
  END DO
  35    cby0=rp2*(CDLOG(z1/2.0D0)+el)*cbj0-rp2*cs
  w1=0.0D0
  cr=(1.0D0,0.0D0)
  cs=(1.0D0,0.0D0)
  DO  k=1,40
    w1=w1+1.0D0/k
    cr=-0.25D0*cr/(k*(k))*z2
    cp=cr*(2.0D0*w1+1.0D0/(k+1.0D0))
    cs=cs+cp
    IF (CDABS(cp/cs) < 1.0D-15) EXIT
  END DO
  45    cby1=rp2*((CDLOG(z1/2.0D0)+el)*cbj1-1.0D0/z1-.25D0*z1*cs)
ELSE
  DATA a/-.703125D-01,.112152099609375D+00,  &
      -.5725014209747314D+00,.6074042001273483D+01,  &
      -.1100171402692467D+03,.3038090510922384D+04,  &
      -.1188384262567832D+06,.6252951493434797D+07,  &
      -.4259392165047669D+09,.3646840080706556D+11,  &
      -.3833534661393944D+13,.4854014686852901D+15/
  DATA b/ .732421875D-01,-.2271080017089844D+00,  &
      .1727727502584457D+01,-.2438052969955606D+02,  &
      .5513358961220206D+03,-.1825775547429318D+05,  &
      .8328593040162893D+06,-.5006958953198893D+08,  &
      .3836255180230433D+10,-.3649010818849833D+12,  &
      .4218971570284096D+14,-.5827244631566907D+16/
  DATA a1/.1171875D+00,-.144195556640625D+00,  &
      .6765925884246826D+00,-.6883914268109947D+01,  &
      .1215978918765359D+03,-.3302272294480852D+04,  &
      .1276412726461746D+06,-.6656367718817688D+07,  &
      .4502786003050393D+09,-.3833857520742790D+11,  &
      .4011838599133198D+13,-.5060568503314727D+15/
  DATA b1/-.1025390625D+00,.2775764465332031D+00,  &
      -.1993531733751297D+01,.2724882731126854D+02,  &
      -.6038440767050702D+03,.1971837591223663D+05,  &
      -.8902978767070678D+06,.5310411010968522D+08,  &
      -.4043620325107754D+10,.3827011346598605D+12,  &
      -.4406481417852278D+14,.6065091351222699D+16/
  k0=12
  IF (a0 >= 35.0) k0=10
  IF (a0 >= 50.0) k0=8
  ct1=z1-0.25D0*pi
  cp0=(1.0D0,0.0D0)
  DO  k=1,k0
    cp0=cp0+a(k)*z1**(-2*k)
  END DO
  cq0=-0.125D0/z1
  DO  k=1,k0
    cq0=cq0+b(k)*z1**(-2*k-1)
  END DO
  cu=CDSQRT(rp2/z1)
  cbj0=cu*(cp0*CDCOS(ct1)-cq0*CDSIN(ct1))
  cby0=cu*(cp0*CDSIN(ct1)+cq0*CDCOS(ct1))
  ct2=z1-0.75D0*pi
  cp1=(1.0D0,0.0D0)
  DO  k=1,k0
    cp1=cp1+a1(k)*z1**(-2*k)
  END DO
  cq1=0.375D0/z1
  DO  k=1,k0
    cq1=cq1+b1(k)*z1**(-2*k-1)
  END DO
  cbj1=cu*(cp1*CDCOS(ct2)-cq1*CDSIN(ct2))
  cby1=cu*(cp1*CDSIN(ct2)+cq1*CDCOS(ct2))
END IF
IF (REAL(z) < 0.0) THEN
  IF (DIMAG(z) < 0.0) cby0=cby0-2.0D0*ci*cbj0
  IF (DIMAG(z) > 0.0) cby0=cby0+2.0D0*ci*cbj0
  IF (DIMAG(z) < 0.0) cby1=-(cby1-2.0D0*ci*cbj1)
  IF (DIMAG(z) > 0.0) cby1=-(cby1+2.0D0*ci*cbj1)
  cbj1=-cbj1
END IF
cdj0=-cbj1
cdj1=cbj0-1.0D0/z*cbj1
cdy0=-cby1
cdy1=cby0-1.0D0/z*cby1
RETURN
END SUBROUTINE cjy01


INTEGER FUNCTION msta1(x,mp)

!     ===================================================
!     Purpose: Determine the starting point for backward
!     recurrence such that the magnitude of
!     Jn(x) at that point is about 10^(-MP)
!     Input :  x     --- Argument of Jn(x)
!     MP    --- Value of magnitude
!     Output:  MSTA1 --- Starting point
!     ===================================================



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
20   msta2=INT(nn+10)
RETURN
END FUNCTION msta2

REAL*8 FUNCTION envj(n,x)


INTEGER, INTENT(IN)                      :: n
DOUBLE PRECISION, INTENT(IN)             :: x

envj=0.5D0*DLOG10(6.28D0*n)-n*DLOG10(1.36D0*x/n)
RETURN
END FUNCTION envj
