PROGRAM mch12n
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:10

!     =====================================================
!     Purpose: This program computes Hankel functions of
!     the first and second kinds and their
!     derivatives for a complex argument using
!     subroutine CH12N
!     Input :  z --- Complex argument
!     n --- Order of Hn(1)(z) and Hn(2)(z)
!     ( n = 0,1,תתת, n ף 250 )
!     Output:  CHF1(n) --- Hn(1)(z)
!     CHD1(n) --- Hn(1)'(z)
!     CHF2(n) --- Hn(2)(z)
!     CHD2(n) --- Hn(2)'(z)
!     =====================================================

IMPLICIT DOUBLE PRECISION (a,b,d-h,o-y)
IMPLICIT COMPLEX*16 (c,z)
DIMENSION chf1(0:250),chd1(0:250),chf2(0:250),chd2(0:250)
WRITE(*,*) '  Please enter n, x and y (z=x+iy) '
!      READ(*,*)N,X,Y
n=5
x=1.5
y=1.5
nm=1
WRITE(*,45)x,y,n
z=CMPLX(x,y)
IF (n <= 8) THEN
  ns=1
ELSE
  WRITE(*,*)'  Please enter order step Ns'
!       READ(*,*)NS
  ns=2
END IF
CALL ch12n(n,z,nm,chf1,chd1,chf2,chd2)
WRITE(*,*)
WRITE(*,*)'   n     Re[Hn(1)(z)]     Im[Hn(1)(z)]',  &
    '      Re[Hn(1)''(Z)]     IM[HN(1)''(Z)]'
WRITE(*,*)' -------------------------------------',  &
    '---------------------------------------'
DO  k=0,nm,ns
  WRITE(*,40)k,chf1(k),chd1(k)
END DO
WRITE(*,*)
WRITE(*,*)'   n     Re[Hn(2)(z)]     Im[Hn(2)(z)]',  &
    '      Re[Hn(2)''(Z)]     IM[HN(2)''(Z)]'
WRITE(*,*)' -------------------------------------',  &
    '---------------------------------------'
DO  k=0,nm,ns
  WRITE(*,40)k,chf2(k),chd2(k)
END DO
40   FORMAT(1X,i4,4D18.10)
45   FORMAT(3X,3HZ =,f8.3,' + i ',f8.3,' ,',6X,6HNMAX =,i4)
END PROGRAM mch12n


SUBROUTINE ch12n(n,z,nm,chf1,chd1,chf2,chd2)

!     ====================================================
!     Purpose: Compute Hankel functions of the first and
!     second kinds and their derivatives for a
!     complex argument
!     Input :  z --- Complex argument
!     n --- Order of Hn(1)(z) and Hn(2)(z)
!     Output:  CHF1(n) --- Hn(1)(z)
!     CHD1(n) --- Hn(1)'(z)
!     CHF2(n) --- Hn(2)(z)
!     CHD2(n) --- Hn(2)'(z)
!     NM --- Highest order computed
!     Routines called:
!     (1) CJYNB for computing Jn(z) and Yn(z)
!     (2) CIKNB for computing In(z) and Kn(z)
!     ====================================================


INTEGER, INTENT(IN OUT)                  :: n
COMPLEX, INTENT(IN)                      :: z
INTEGER, INTENT(IN)                      :: nm
COMPLEX, INTENT(OUT)                     :: chf1(0:n)
COMPLEX, INTENT(OUT)                     :: chd1(0:n)
COMPLEX, INTENT(OUT)                     :: chf2(0:n)
COMPLEX, INTENT(OUT)                     :: chd2(0:n)
IMPLICIT DOUBLE PRECISION (a,b,d-h,o-y)
IMPLICIT COMPLEX*16 (c,z)
DIMENSION cbj(0:250),cdj(0:250),cby(0:250),cdy(0:250),  &
    cbi(0:250),cdi(0:250),cbk(0:250),cdk(0:250)


ci=(0.0D0,1.0D0)
pi=3.141592653589793D0
IF (DIMAG(z) < 0.0D0) THEN
  CALL cjynb(n,z,nm,cbj,cdj,cby,cdy)
  DO  k=0,nm
    chf1(k)=cbj(k)+ci*cby(k)
    chd1(k)=cdj(k)+ci*cdy(k)
  END DO
  zi=ci*z
  CALL ciknb(n,zi,nm,cbi,cdi,cbk,cdk)
  cfac=-2.0D0/(pi*ci)
  DO  k=0,nm
    chf2(k)=cfac*cbk(k)
    chd2(k)=cfac*ci*cdk(k)
    cfac=cfac*ci
  END DO
ELSE IF (DIMAG(z) > 0.0D0) THEN
  zi=-ci*z
  CALL ciknb(n,zi,nm,cbi,cdi,cbk,cdk)
  cf1=-ci
  cfac=2.0D0/(pi*ci)
  DO  k=0,nm
    chf1(k)=cfac*cbk(k)
    chd1(k)=-cfac*ci*cdk(k)
    cfac=cfac*cf1
  END DO
  CALL cjynb(n,z,nm,cbj,cdj,cby,cdy)
  DO  k=0,nm
    chf2(k)=cbj(k)-ci*cby(k)
    chd2(k)=cdj(k)-ci*cdy(k)
  END DO
ELSE
  CALL cjynb(n,z,nm,cbj,cdj,cby,cdy)
  DO  k=0,nm
    chf1(k)=cbj(k)+ci*cby(k)
    chd1(k)=cdj(k)+ci*cdy(k)
    chf2(k)=cbj(k)-ci*cby(k)
    chd2(k)=cdj(k)-ci*cdy(k)
  END DO
END IF
RETURN
END SUBROUTINE ch12n


SUBROUTINE cjynb(n,z,nm,cbj,cdj,cby,cdy)

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
!     Routines called:
!     MSTA1 and MSTA2 to calculate the starting
!     point for backward recurrence
!     =======================================================


INTEGER, INTENT(IN)                      :: n
COMPLEX, INTENT(IN)                      :: z
INTEGER, INTENT(OUT)                     :: nm
COMPLEX, INTENT(OUT)                     :: cbj(0:n)
COMPLEX, INTENT(OUT)                     :: cdj(0:n)
COMPLEX, INTENT(OUT)                     :: cby(0:n)
COMPLEX, INTENT(OUT)                     :: cdy(0:n)
IMPLICIT DOUBLE PRECISION (a,b,d-h,o-y)
IMPLICIT COMPLEX*16 (c,z)
DIMENSION  a(4),b(4),a1(4),b1(4)

el=0.5772156649015329D0
pi=3.141592653589793D0
r2p=.63661977236758D0
y0=DABS(DIMAG(z))
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
IF (a0 <= 300.d0.OR.n > 80) THEN
  IF (n == 0) nm=1
  m=INT(msta1(a0,200))
  IF (m < nm) THEN
    nm=m
  ELSE
    m=INT(msta2(a0,nm,15))
  END IF
  cbs=(0.0D0,0.0D0)
  csu=(0.0D0,0.0D0)
  csv=(0.0D0,0.0D0)
  cf2=(0.0D0,0.0D0)
  cf1=(1.0D-100,0.0D0)
  DO  k=m,0,-1
    cf=2.0D0*(k+1.0D0)/z*cf1-cf2
    IF (k <= nm) cbj(k)=cf
    IF (k == 2*INT(k/2).AND.k /= 0) THEN
      IF (y0 <= 1.0D0) THEN
        cbs=cbs+2.0D0*cf
      ELSE
        cbs=cbs+(-1)**(k/2)*2.0D0*cf
      END IF
      csu=csu+(-1)**(k/2)*cf/k
    ELSE IF (k > 1) THEN
      csv=csv+(-1)**(k/2)*k/(k*k-1.0D0)*cf
    END IF
    cf2=cf1
    cf1=cf
  END DO
  IF (y0 <= 1.0D0) THEN
    cs0=cbs+cf
  ELSE
    cs0=(cbs+cf)/CDCOS(z)
  END IF
  DO  k=0,nm
    cbj(k)=cbj(k)/cs0
  END DO
  ce=CDLOG(z/2.0D0)+el
  cby(0)=r2p*(ce*cbj(0)-4.0D0*csu/cs0)
  cby(1)=r2p*(-cbj(0)/z+(ce-1.0D0)*cbj(1)-4.0D0*csv/cs0)
ELSE
  DATA a/-.7031250000000000D-01,.1121520996093750D+00,  &
      -.5725014209747314D+00,.6074042001273483D+01/
  DATA b/ .7324218750000000D-01,-.2271080017089844D+00,  &
      .1727727502584457D+01,-.2438052969955606D+02/
  DATA a1/.1171875000000000D+00,-.1441955566406250D+00,  &
      .6765925884246826D+00,-.6883914268109947D+01/
  DATA b1/-.1025390625000000D+00,.2775764465332031D+00,  &
      -.1993531733751297D+01,.2724882731126854D+02/
  ct1=z-0.25D0*pi
  cp0=(1.0D0,0.0D0)
  DO  k=1,4
    cp0=cp0+a(k)*z**(-2*k)
  END DO
  cq0=-0.125D0/z
  DO  k=1,4
    cq0=cq0+b(k)*z**(-2*k-1)
  END DO
  cu=CDSQRT(r2p/z)
  cbj0=cu*(cp0*CDCOS(ct1)-cq0*CDSIN(ct1))
  cby0=cu*(cp0*CDSIN(ct1)+cq0*CDCOS(ct1))
  cbj(0)=cbj0
  cby(0)=cby0
  ct2=z-0.75D0*pi
  cp1=(1.0D0,0.0D0)
  DO  k=1,4
    cp1=cp1+a1(k)*z**(-2*k)
  END DO
  cq1=0.375D0/z
  DO  k=1,4
    cq1=cq1+b1(k)*z**(-2*k-1)
  END DO
  cbj1=cu*(cp1*CDCOS(ct2)-cq1*CDSIN(ct2))
  cby1=cu*(cp1*CDSIN(ct2)+cq1*CDCOS(ct2))
  cbj(1)=cbj1
  cby(1)=cby1
  DO  k=2,nm
    cbjk=2.0D0*(k-1.0D0)/z*cbj1-cbj0
    cbj(k)=cbjk
    cbj0=cbj1
    cbj1=cbjk
  END DO
END IF
cdj(0)=-cbj(1)
DO  k=1,nm
  cdj(k)=cbj(k-1)-k/z*cbj(k)
END DO
IF (CDABS(cbj(0)) > 1.0D0) THEN
  cby(1)=(cbj(1)*cby(0)-2.0D0/(pi*z))/cbj(0)
END IF
DO  k=2,nm
  IF (CDABS(cbj(k-1)) >= CDABS(cbj(k-2))) THEN
    cyy=(cbj(k)*cby(k-1)-2.0D0/(pi*z))/cbj(k-1)
  ELSE
    cyy=(cbj(k)*cby(k-2)-4.0D0*(k-1.0D0)/(pi*z*z))/cbj(k-2)
  END IF
  cby(k)=cyy
END DO
cdy(0)=-cby(1)
DO  k=1,nm
  cdy(k)=cby(k-1)-k/z*cby(k)
END DO
RETURN
END SUBROUTINE cjynb


SUBROUTINE ciknb(n,z,nm,cbi,cdi,cbk,cdk)

!     ============================================================
!     Purpose: Compute modified Bessel functions In(z) and Kn(z),
!     and their derivatives for a complex argument
!     Input:   z --- Complex argument
!     n --- Order of In(z) and Kn(z)
!     Output:  CBI(n) --- In(z)
!     CDI(n) --- In'(z)
!     CBK(n) --- Kn(z)
!     CDK(n) --- Kn'(z)
!     NM --- Highest order computed
!     Routones called:
!     MSTA1 and MSTA2 to compute the starting point for
!     backward recurrence
!     ===========================================================


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
m=INT(msta1(a0,200))
IF (m < nm) THEN
  nm=m
ELSE
  m=INT(msta2(a0,nm,15))
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
20   msta1=nn
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
20   msta2=nn+10
RETURN
END FUNCTION msta2

REAL*8 FUNCTION envj(n,x)


INTEGER, INTENT(IN)                      :: n
DOUBLE PRECISION, INTENT(IN)             :: x

envj=0.5D0*DLOG10(6.28D0*n)-n*DLOG10(1.36D0*x/n)
RETURN
END FUNCTION envj
