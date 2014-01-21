PROGRAM mcikna
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:10

!       =============================================================
!       Purpose: This program computes the modified Bessel functions
!                In(z) and Kn(z), and their derivatives for a
!                complex argument using subroutine CIKNA
!       Input :  z --- Complex argument of In(z) and Kn(z)
!                n --- Order of In(z) and Kn(z)
!                      ( n = 0,1,תתת, n ף 250 )
!       Output:  CBI(n) --- In(z)
!                CDI(n) --- In'(z)
!                CBK(n) --- Kn(z)
!                CDK(n) --- Kn'(z)
!       Example: z = 4.0 + i 2.0 ,      Nmax = 5

!     n     Re[In(z)]      Im[In(z)]      Re[In'(z)]     Im[In'(z)]
!   -----------------------------------------------------------------
!     0  -.19056142D+01  .10403505D+02 -.23059657D+01  .92222463D+01
!     1  -.23059657D+01  .92222463D+01 -.23666457D+01  .83284588D+01
!     2  -.28276772D+01  .62534130D+01 -.24255774D+01  .61553456D+01
!     3  -.25451891D+01  .30884450D+01 -.22270972D+01  .36367893D+01
!     4  -.16265172D+01  .10201656D+01 -.16520416D+01  .16217056D+01
!     5  -.75889410D+00  .15496632D+00 -.94510625D+00  .48575220D+00

!     n     Re[Kn(z)]      Im[Kn(z)]      Re[Kn'(z)]     Im[Kn'(z)]
!   -----------------------------------------------------------------
!     0  -.64221754D-02 -.84393648D-02  .74307276D-02  .89585853D-02
!     1  -.74307276D-02 -.89585853D-02  .88041795D-02  .94880091D-02
!     2  -.11186184D-01 -.10536653D-01  .14012532D-01  .10936010D-01
!     3  -.20594336D-01 -.12913435D-01  .27416815D-01  .12106413D-01
!     4  -.43647447D-01 -.13676173D-01  .60982763D-01  .63953943D-02
!     5  -.10137119D+00  .12264588D-03  .14495731D+00 -.37132068D-01
!       =============================================================

IMPLICIT DOUBLE PRECISION (a,b,d-h,o-y)
IMPLICIT COMPLEX*16 (c,z)
COMMON cbi(0:250),cdi(0:250),cbk(0:250),cdk(0:250)
WRITE(*,*)'  Please input n, x,y (z=x+iy)=?'
!        READ(*,*)N,X,Y
n=5
x=4.0
y=2.0
z=CMPLX(x,y)
WRITE(*,40)x,y,n
IF (n <= 8) THEN
  ns=1
ELSE
  WRITE(*,*)' Please enter order step Ns'
!         READ(*,*)NS
  ns=1
END IF
CALL cikna(n,z,nm,cbi,cdi,cbk,cdk)
WRITE(*,*)
WRITE(*,*)'   n      Re[In(z)]       Im[In(z)]',  &
    '       Re[In''(Z)]      IM[IN''(Z)]'
WRITE(*,*)' -----------------------------------',  &
    '----------------------------------'
DO  k=0,nm,ns
  WRITE(*,30)k,cbi(k),cdi(k)
END DO
WRITE(*,*)
WRITE(*,*)'   n      Re[Kn(z)]       Im[Kn(z)]',  &
    '       Re[Kn''(Z)]      IM[KN''(Z)]'
WRITE(*,*)' -----------------------------------',  &
    '----------------------------------'
DO  k=0,nm,ns
  WRITE(*,30)k,cbk(k),cdk(k)
END DO
30      FORMAT(1X,i4,1X,4D16.8)
40      FORMAT(3X,3HZ =,f7.1,' + i',f7.1,' ,',6X,6HNMAX =,i4)
END PROGRAM mcikna


SUBROUTINE cikna(n,z,nm,cbi,cdi,cbk,cdk)

!       ========================================================
!       Purpose: Compute modified Bessel functions In(z), Kn(x)
!                and their derivatives for a complex argument
!       Input :  z --- Complex argument of In(z) and Kn(z)
!                n --- Order of In(z) and Kn(z)
!       Output:  CBI(n) --- In(z)
!                CDI(n) --- In'(z)
!                CBK(n) --- Kn(z)
!                CDK(n) --- Kn'(z)
!                NM --- Highest order computed
!       Routines called:
!             (1) CIK01 to compute I0(z), I1(z) K0(z) & K1(z)
!             (2) MSTA1 and MSTA2 to compute the starting
!                 point for backward recurrence
!       ========================================================


INTEGER, INTENT(IN)                      :: n
COMPLEX, INTENT(IN)                      :: z
INTEGER, INTENT(OUT)                     :: nm
COMPLEX, INTENT(OUT)                     :: cbi(0:n)
COMPLEX, INTENT(OUT)                     :: cdi(0:n)
COMPLEX, INTENT(OUT)                     :: cbk(0:n)
COMPLEX, INTENT(OUT)                     :: cdk(0:n)
IMPLICIT DOUBLE PRECISION (a,b,p,w,x,y)
IMPLICIT COMPLEX*16 (c,z)


a0=CDABS(z)
nm=n
IF (a0 < 1.0D-100) THEN
  DO  k=0,n
    cbi(k)=(0.0D0,0.0D0)
    cdi(k)=(0.0D0,0.0D0)
    cbk(k)=-(1.0D+300,0.0D0)
    cdk(k)=(1.0D+300,0.0D0)
  END DO
  cbi(0)=(1.0D0,0.0D0)
  cdi(1)=(0.5D0,0.0D0)
  RETURN
END IF
CALL cik01(z,cbi0,cdi0,cbi1,cdi1,cbk0,cdk0,cbk1,cdk1)
cbi(0)=cbi0
cbi(1)=cbi1
cbk(0)=cbk0
cbk(1)=cbk1
cdi(0)=cdi0
cdi(1)=cdi1
cdk(0)=cdk0
cdk(1)=cdk1
IF (n <= 1) RETURN
m=msta1(a0,200)
IF (m < n) THEN
  nm=m
ELSE
  m=msta2(a0,n,15)
END IF
cf2=(0.0D0,0.0D0)
cf1=(1.0D-100,0.0D0)
DO  k=m,0,-1
  cf=2.0D0*(k+1.0D0)/z*cf1+cf2
  IF (k <= nm) cbi(k)=cf
  cf2=cf1
  cf1=cf
END DO
cs=cbi0/cf
DO  k=0,nm
  cbi(k)=cs*cbi(k)
END DO
DO  k=2,nm
  IF (CDABS(cbi(k-1)) > CDABS(cbi(k-2))) THEN
    ckk=(1.0D0/z-cbi(k)*cbk(k-1))/cbi(k-1)
  ELSE
    ckk=(cbi(k)*cbk(k-2)+2.0D0*(k-1.0D0)/(z*z))/cbi(k-2)
  END IF
  cbk(k)=ckk
END DO
DO  k=2,nm
  cdi(k)=cbi(k-1)-k/z*cbi(k)
  cdk(k)=-cbk(k-1)-k/z*cbk(k)
END DO
RETURN
END SUBROUTINE cikna


SUBROUTINE cik01(z,cbi0,cdi0,cbi1,cdi1,cbk0,cdk0,cbk1,cdk1)

!       ==========================================================
!       Purpose: Compute modified complex Bessel functions I0(z),
!                I1(z), K0(z), K1(z), and their derivatives
!       Input :  z --- Complex argument
!       Output:  CBI0 --- I0(z)
!                CDI0 --- I0'(z)
!                CBI1 --- I1(z)
!                CDI1 --- I1'(z)
!                CBK0 --- K0(z)
!                CDK0 --- K0'(z)
!                CBK1 --- K1(z)
!                CDK1 --- K1'(z)
!       ==========================================================


COMPLEX, INTENT(IN)                      :: z
COMPLEX, INTENT(OUT)                     :: cbi0
COMPLEX, INTENT(OUT)                     :: cdi0
COMPLEX, INTENT(OUT)                     :: cbi1
COMPLEX, INTENT(OUT)                     :: cdi1
COMPLEX, INTENT(OUT)                     :: cbk0
COMPLEX, INTENT(OUT)                     :: cdk0
COMPLEX, INTENT(OUT)                     :: cbk1
COMPLEX, INTENT(OUT)                     :: cdk1
IMPLICIT DOUBLE PRECISION (a,b,d-h,o-y)
IMPLICIT COMPLEX*16 (c,z)
DIMENSION a(12),b(12),a1(10)

cw=0.0
pi=3.141592653589793D0
ci=(0.0D0,1.0D0)
a0=CDABS(z)
z2=z*z
z1=z
IF (a0 == 0.0D0) THEN
  cbi0=(1.0D0,0.0D0)
  cbi1=(0.0D0,0.0D0)
  cdi0=(0.0D0,0.0D0)
  cdi1=(0.5D0,0.0D0)
  cbk0=(1.0D+300,0.0D0)
  cbk1=(1.0D+300,0.0D0)
  cdk0=-(1.0D+300,0.0D0)
  cdk1=-(1.0D+300,0.0D0)
  RETURN
END IF
IF (REAL(z) < 0.0) z1=-z
IF (a0 <= 18.0) THEN
  cbi0=(1.0D0,0.0D0)
  cr=(1.0D0,0.0D0)
  DO  k=1,50
    cr=0.25D0*cr*z2/(k*k)
    cbi0=cbi0+cr
    IF (CDABS(cr/cbi0) < 1.0D-15) EXIT
  END DO
  15         cbi1=(1.0D0,0.0D0)
  cr=(1.0D0,0.0D0)
  DO  k=1,50
    cr=0.25D0*cr*z2/(k*(k))
    cbi1=cbi1+cr
    IF (CDABS(cr/cbi1) < 1.0D-15) EXIT
  END DO
  25         cbi1=0.5D0*z1*cbi1
ELSE
  DATA a/0.125D0,7.03125D-2, 7.32421875D-2,1.1215209960938D-1,  &
      2.2710800170898D-1,5.7250142097473D-1,  &
      1.7277275025845D0,6.0740420012735D0,  &
      2.4380529699556D01,1.1001714026925D02,  &
      5.5133589612202D02,3.0380905109224D03/
  DATA b/-0.375D0,-1.171875D-1, -1.025390625D-1,-1.4419555664063D-1,  &
      -2.7757644653320D-1,-6.7659258842468D-1,  &
      -1.9935317337513D0,-6.8839142681099D0,  &
      -2.7248827311269D01,-1.2159789187654D02,  &
      -6.0384407670507D02,-3.3022722944809D03/
  k0=12
  IF (a0 >= 35.0) k0=9
  IF (a0 >= 50.0) k0=7
  ca=CDEXP(z1)/CDSQRT(2.0D0*pi*z1)
  cbi0=(1.0D0,0.0D0)
  zr=1.0D0/z1
  DO  k=1,k0
    cbi0=cbi0+a(k)*zr**k
  END DO
  cbi0=ca*cbi0
  cbi1=(1.0D0,0.0D0)
  DO  k=1,k0
    cbi1=cbi1+b(k)*zr**k
  END DO
  cbi1=ca*cbi1
END IF
IF (a0 <= 9.0) THEN
  cs=(0.0D0,0.0D0)
  ct=-CDLOG(0.5D0*z1)-0.5772156649015329D0
  w0=0.0D0
  cr=(1.0D0,0.0D0)
  DO  k=1,50
    w0=w0+1.0D0/k
    cr=0.25D0*cr/(k*k)*z2
    cs=cs+cr*(w0+ct)
    IF (CDABS((cs-cw)/cs) < 1.0D-15) EXIT
    cw=cs
  END DO
  45         cbk0=ct+cs
ELSE
  DATA a1/0.125D0,0.2109375D0, 1.0986328125D0,1.1775970458984D01,  &
      2.1461706161499D02,5.9511522710323D03,  &
      2.3347645606175D05,1.2312234987631D07, 8.401390346421D08,7.2031420482627D10/
  cb=0.5D0/z1
  zr2=1.0D0/z2
  cbk0=(1.0D0,0.0D0)
  DO  k=1,10
    cbk0=cbk0+a1(k)*zr2**k
  END DO
  cbk0=cb*cbk0/cbi0
END IF
cbk1=(1.0D0/z1-cbi1*cbk0)/cbi0
IF (REAL(z) < 0.0) THEN
  IF (DIMAG(z) < 0.0) cbk0=cbk0+ci*pi*cbi0
  IF (DIMAG(z) > 0.0) cbk0=cbk0-ci*pi*cbi0
  IF (DIMAG(z) < 0.0) cbk1=-cbk1+ci*pi*cbi1
  IF (DIMAG(z) > 0.0) cbk1=-cbk1-ci*pi*cbi1
  cbi1=-cbi1
END IF
cdi0=cbi1
cdi1=cbi0-1.0D0/z*cbi1
cdk0=-cbk1
cdk1=-cbk0-1.0D0/z*cbk1
RETURN
END SUBROUTINE cik01


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
