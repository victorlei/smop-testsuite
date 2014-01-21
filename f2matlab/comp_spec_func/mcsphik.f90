PROGRAM mcsphik
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:10

!       =============================================================
!       Purpose: This program computes the modified spherical Bessel
!                functions and their derivatives for a complex
!                argument using subroutine CSPHIK
!       Input :  z --- Complex argument
!                n --- Order of in(z) & kn(z) ( 0 ó n ó 250 )
!       Output:  CSI(n) --- in(z)
!                CDI(n) --- in'(z)
!                CSK(n) --- kn(z)
!                CDK(n) --- kn'(z)
!       Example: z =4.0+i 2.0

!     n     Re[in(z)]      Im[in(z)]     Re[in'(z)]     Im[in'(z)]
!    ---------------------------------------------------------------
!     0   .2118080D+00   .6101922D+01  -.4439356D+00   .4900150D+01
!     1  -.4439356D+00   .4900150D+01  -.5906477D+00   .4053075D+01
!     2  -.9918756D+00   .3028652D+01  -.7574058D+00   .2785396D+01
!     3  -.9663859D+00   .1375561D+01  -.7689911D+00   .1541649D+01
!     4  -.6018277D+00   .4263967D+00  -.5777565D+00   .6482500D+00
!     5  -.2668530D+00   .6640148D-01  -.3214450D+00   .1866032D+00

!     n     Re[kn(z)]      Im[kn(z)]     Re[kn'(z)]     Im[kn'(z)]
!    ---------------------------------------------------------------
!     0  -.5010582D-02  -.4034862D-02   .6416184D-02   .4340777D-02
!     1  -.6416184D-02  -.4340777D-02   .8445211D-02   .4487936D-02
!     2  -.1016253D-01  -.4714473D-02   .1392804D-01   .4120703D-02
!     3  -.1893595D-01  -.3973987D-02   .2690088D-01   .3192843D-03
!     4  -.3945464D-01   .2977107D-02   .5690203D-01  -.1873044D-01
!     5  -.8727490D-01   .3689398D-01   .1220481D+00  -.9961483D-01
!       =============================================================

IMPLICIT COMPLEX*16 (c,z)
DOUBLE PRECISION :: x,y
DIMENSION csi(0:250),cdi(0:250),csk(0:250),cdk(0:250)
WRITE(*,*)'Please enter n,x,y (z=x+iy) '
!        READ(*,*)N,X,Y
n=5
x=4.0
y=2.0
WRITE(*,30)n,x,y
z=CMPLX(x,y)
IF (n <= 8) THEN
  ns=1
ELSE
  WRITE(*,*)'Please enter order step Ns '
!           READ(*,*)NS
  ns=1
END IF
CALL csphik(n,z,nm,csi,cdi,csk,cdk)
WRITE(*,*)
WRITE(*,*)'  n      Re[in(z)]        Im[in(z)]',  &
    '        Re[in''(Z)]       IM[IN''(Z)]'
WRITE(*,*)'--------------------------------------------',  &
    '----------------------------'
DO  k=0,nm,ns
  WRITE(*,20)k,csi(k),cdi(k)
END DO
WRITE(*,*)
WRITE(*,*)'  n      Re[kn(z)]        Im[kn(z)]',  &
    '        Re[kn''(Z)]       IM[KN''(Z)]'
WRITE(*,*)'--------------------------------------------',  &
    '----------------------------'
DO  k=0,nm,ns
  WRITE(*,20)k,csk(k),cdk(k)
END DO
20      FORMAT(1X,i3,4D17.8)
30      FORMAT(3X,'Nmaz =',i3,',     ','z = ',f8.1,'+ i',f8.1)
END PROGRAM mcsphik


SUBROUTINE csphik(n,z,nm,csi,cdi,csk,cdk)

!       =======================================================
!       Purpose: Compute modified spherical Bessel functions
!                and their derivatives for a complex argument
!       Input :  z --- Complex argument
!                n --- Order of in(z) & kn(z) ( n = 0,1,2,... )
!       Output:  CSI(n) --- in(z)
!                CDI(n) --- in'(z)
!                CSK(n) --- kn(z)
!                CDK(n) --- kn'(z)
!                NM --- Highest order computed
!       Routines called:
!                MSTA1 and MSTA2 for computing the starting
!                point for backward recurrence
!       =======================================================


INTEGER, INTENT(IN)                      :: n
COMPLEX, INTENT(IN)                      :: z
INTEGER, INTENT(OUT)                     :: nm
COMPLEX, INTENT(OUT)                     :: csi(0:n)
COMPLEX, INTENT(OUT)                     :: cdi(0:n)
COMPLEX, INTENT(OUT)                     :: csk(0:n)
COMPLEX, INTENT(OUT)                     :: cdk(0:n)
IMPLICIT COMPLEX*16 (c,z)
DOUBLE PRECISION :: a0,pi


pi=3.141592653589793D0
a0=CDABS(z)
nm=n
IF (a0 < 1.0D-60) THEN
  DO  k=0,n
    csi(k)=0.0D0
    cdi(k)=0.0D0
    csk(k)=1.0D+300
    cdk(k)=-1.0D+300
  END DO
  csi(0)=1.0D0
  cdi(1)=0.3333333333333333D0
  RETURN
END IF
ci=CMPLX(0.0D0,1.0D0)
csinh=CDSIN(ci*z)/ci
ccosh=CDCOS(ci*z)
csi0=csinh/z
csi1=(-csinh/z+ccosh)/z
csi(0)=csi0
csi(1)=csi1
IF (n >= 2) THEN
  m=msta1(a0,200)
  IF (m < n) THEN
    nm=m
  ELSE
    m=msta2(a0,n,15)
  END IF
  cf0=0.0D0
  cf1=1.0D0-100
  DO  k=m,0,-1
    cf=(2.0D0*k+3.0D0)*cf1/z+cf0
    IF (k <= nm) csi(k)=cf
    cf0=cf1
    cf1=cf
  END DO
  IF (CDABS(csi0) > CDABS(csi1)) cs=csi0/cf
  IF (CDABS(csi0) <= CDABS(csi1)) cs=csi1/cf0
  DO  k=0,nm
    csi(k)=cs*csi(k)
  END DO
END IF
cdi(0)=csi(1)
DO  k=1,nm
  cdi(k)=csi(k-1)-(k+1.0D0)*csi(k)/z
END DO
csk(0)=0.5D0*pi/z*CDEXP(-z)
csk(1)=csk(0)*(1.0D0+1.0D0/z)
DO  k=2,nm
  IF (CDABS(csi(k-1)) > CDABS(csi(k-2))) THEN
    csk(k)=(0.5D0*pi/(z*z)-csi(k)*csk(k-1))/csi(k-1)
  ELSE
    csk(k)=(csi(k)*csk(k-2)+(k-0.5D0)*pi/z**3)/csi(k-2)
  END IF
END DO
cdk(0)=-csk(1)
DO  k=1,nm
  cdk(k)=-csk(k-1)-(k+1.0D0)*csk(k)/z
END DO
RETURN
END SUBROUTINE csphik


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
