PROGRAM mciknb
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:10

!       =============================================================
!       Purpose: This program computes the modified Bessel functions
!                In(z) and Kn(z), and their derivatives for a
!                complex argument using subroutine CIKNB
!       Input:   z --- Complex argument
!                n --- Order of In(z) and Kn(z)
!                      ( n = 0,1,תתת, n ף 250 )
!       Output:  CBI(n) --- In(z)
!                CDI(n) --- In'(z)
!                CBK(n) --- Kn(z)
!                CDK(n) --- Kn'(z)
!       Example: Nmax = 5,   z = 4.0 + i 2.0

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
DIMENSION cbi(0:250),cdi(0:250),cbk(0:250),cdk(0:250)
WRITE(*,*)'  Please enter n, x and y (z = x+iy) '
!        READ(*,*)N,X,Y
n=5
x=4.0
y=2.0
WRITE(*,25)n,x,y
z=CMPLX(x,y)
WRITE(*,*)
IF (n <= 8) THEN
  ns=1
ELSE
  WRITE(*,*)'  Please enter order step Ns'
!           READ(*,*)NS
  ns=1
END IF
CALL ciknb(n,z,nm,cbi,cdi,cbk,cdk)
WRITE(*,*)'   n      Re[In(z)]       Im[In(z)]',  &
    '      Re[In''(Z)]       IM[IN''(Z)]'
WRITE(*,*)' ---------------------------------',  &
    '------------------------------------'
DO  k=0,nm,ns
  WRITE(*,20)k,cbi(k),cdi(k)
END DO
WRITE(*,*)
WRITE(*,*)'   n      Re[Kn(z)]       Im[Kn(z)]',  &
    '      Re[Kn''(Z)]       IM[KN''(Z)]'
WRITE(*,*)' ---------------------------------',  &
    '------------------------------------'
DO  k=0,nm,ns
  WRITE(*,20)k,cbk(k),cdk(k)
END DO
20      FORMAT(1X,1X,i3,1X,4D16.8)
25      FORMAT(3X,6HNMAZ =,i3,',    ','z =', f6.1,' + i',f6.1)
END PROGRAM mciknb


SUBROUTINE ciknb(n,z,nm,cbi,cdi,cbk,cdk)

!       ============================================================
!       Purpose: Compute modified Bessel functions In(z) and Kn(z),
!                and their derivatives for a complex argument
!       Input:   z --- Complex argument
!                n --- Order of In(z) and Kn(z)
!       Output:  CBI(n) --- In(z)
!                CDI(n) --- In'(z)
!                CBK(n) --- Kn(z)
!                CDK(n) --- Kn'(z)
!                NM --- Highest order computed
!       Routones called:
!                MSTA1 and MSTA2 to compute the starting point for
!                backward recurrence
!       ===========================================================


INTEGER, INTENT(IN)                      :: n
COMPLEX, INTENT(IN)                      :: z
INTEGER, INTENT(OUT)                     :: nm
COMPLEX, INTENT(OUT)                     :: cbi(0:n)
COMPLEX, INTENT(OUT)                     :: cdi(0:n)
COMPLEX, INTENT(OUT)                     :: cbk(0:n)
COMPLEX, INTENT(OUT)                     :: cdk(0:n)
IMPLICIT DOUBLE PRECISION (a,b,d-h,o-y)
IMPLICIT COMPLEX*16 (c,z)


pi=3.141592653589793D0
el=0.57721566490153D0
a0=CDABS(z)
nm=n
IF (a0 < 1.0D-100) THEN
  DO  k=0,n
    cbi(k)=(0.0D0,0.0D0)
    cbk(k)=(1.0D+300,0.0D0)
    cdi(k)=(0.0D0,0.0D0)
    cdk(k)=-(1.0D+300,0.0D0)
  END DO
  cbi(0)=(1.0D0,0.0D0)
  cdi(1)=(0.5D0,0.0D0)
  RETURN
END IF
z1=z
ci=(0.0D0,1.0D0)
IF (REAL(z) < 0.0) z1=-z
IF (n == 0) nm=1
m=msta1(a0,200)
IF (m < nm) THEN
  nm=m
ELSE
  m=msta2(a0,nm,15)
END IF
cbs=0.0D0
csk0=0.0D0
cf0=0.0D0
cf1=1.0D-100
DO  k=m,0,-1
  cf=2.0D0*(k+1.0D0)*cf1/z1+cf0
  IF (k <= nm) cbi(k)=cf
  IF (k /= 0.AND.k == 2*INT(k/2)) csk0=csk0+4.0D0*cf/k
  cbs=cbs+2.0D0*cf
  cf0=cf1
  cf1=cf
END DO
cs0=CDEXP(z1)/(cbs-cf)
DO  k=0,nm
  cbi(k)=cs0*cbi(k)
END DO
IF (a0 <= 9.0) THEN
  cbk(0)=-(CDLOG(0.5D0*z1)+el)*cbi(0)+cs0*csk0
  cbk(1)=(1.0D0/z1-cbi(1)*cbk(0))/cbi(0)
ELSE
  ca0=CDSQRT(pi/(2.0D0*z1))*CDEXP(-z1)
  k0=16
  IF (a0 >= 25.0) k0=10
  IF (a0 >= 80.0) k0=8
  IF (a0 >= 200.0) k0=6
  DO  l=0,1
    cbkl=1.0D0
    vt=4.0D0*l
    cr=(1.0D0,0.0D0)
    DO  k=1,k0
      cr=0.125D0*cr*(vt-(2.0*k-1.0)**2)/(k*z1)
      cbkl=cbkl+cr
    END DO
    cbk(l)=ca0*cbkl
  END DO
END IF
cg0=cbk(0)
cg1=cbk(1)
DO  k=2,nm
  cg=2.0D0*(k-1.0D0)/z1*cg1+cg0
  cbk(k)=cg
  cg0=cg1
  cg1=cg
END DO
IF (REAL(z) < 0.0) THEN
  fac=1.0D0
  DO  k=0,nm
    IF (DIMAG(z) < 0.0) THEN
      cbk(k)=fac*cbk(k)+ci*pi*cbi(k)
    ELSE
      cbk(k)=fac*cbk(k)-ci*pi*cbi(k)
    END IF
    cbi(k)=fac*cbi(k)
    fac=-fac
  END DO
END IF
cdi(0)=cbi(1)
cdk(0)=-cbk(1)
DO  k=1,nm
  cdi(k)=cbi(k-1)-k/z*cbi(k)
  cdk(k)=-cbk(k-1)-k/z*cbk(k)
END DO
RETURN
END SUBROUTINE ciknb


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
