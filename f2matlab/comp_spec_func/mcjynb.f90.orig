PROGRAM mcjynb
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:10

!       ================================================================
!       Purpose: This program computes Bessel functions Jn(z), Yn(z)
!                and their derivatives for a complex argument using
!                subroutine CJYNB
!       Input :  z --- Complex argument of Jn(z) and Yn(z)
!                n --- Order of Jn(z) and Yn(z)
!                      ( n = 0,1,תתת, n ף 250 )
!       Output:  CBJ(n) --- Jn(z)
!                CDJ(n) --- Jn'(z)
!                CBY(n) --- Yn(z)
!                CDY(n) --- Yn'(z)
!       Eaxmple: z = 4.0 + i 2.0

!     n     Re[Jn(z)]       Im[Jn(z)]       Re[Jn'(z)]      Im[Jn'(z)]
!    -------------------------------------------------------------------
!     0  -.13787022D+01   .39054236D+00   .50735255D+00   .12263041D+01
!     1  -.50735255D+00  -.12263041D+01  -.11546013D+01   .58506793D+00
!     2   .93050039D+00  -.77959350D+00  -.72363400D+00  -.72836666D+00
!     3   .93991546D+00   .23042918D+00   .29742236D+00  -.63587637D+00
!     4   .33565567D+00   .49215925D+00   .47452722D+00  -.29035945D-01
!     5  -.91389835D-02   .28850107D+00   .20054412D+00   .19908868D+00

!     n     Re[Yn(z)]       Im[Yn(z)]       Re[Yn'(z)]      Im[Yn'(z)]
!   --------------------------------------------------------------------
!     0  -.38145893D+00  -.13291649D+01  -.12793101D+01   .51220420D+00
!     1   .12793101D+01  -.51220420D+00  -.58610052D+00  -.10987930D+01
!     2   .79074211D+00   .86842120D+00   .78932897D+00  -.70142425D+00
!     3  -.29934789D+00   .89064431D+00   .70315755D+00   .24423024D+00
!     4  -.61557299D+00   .37996071D+00   .41126221D-01   .34044655D+00
!     5  -.38160033D+00   .20975121D+00  -.33884827D+00  -.20590670D-01

!                z = 20.0 + i  10.0 ,      Nmax =  5

!     n     Re[Jn(z)]       Im[Jn(z)]       Re[Jn'(z)]      Im[Jn'(z)]
!   --------------------------------------------------------------------
!     0   .15460268D+04  -.10391216D+04  -.10601232D+04  -.15098284D+04
!     1   .10601232D+04   .15098284D+04   .14734253D+04  -.10783122D+04
!     2  -.14008238D+04   .11175029D+04   .11274890D+04   .13643952D+04
!     3  -.11948548D+04  -.12189620D+04  -.11843035D+04   .11920871D+04
!     4   .96778325D+03  -.12666712D+04  -.12483664D+04  -.93887194D+03
!     5   .13018781D+04   .65878188D+03   .64152944D+03  -.12682398D+04

!     n     Re[Yn(z)]       Im[Yn(z)]       Re[Yn'(z)]      Im[Yn'(z)]
!   --------------------------------------------------------------------
!     0   .10391216D+04   .15460268D+04   .15098284D+04  -.10601232D+04
!     1  -.15098284D+04   .10601232D+04   .10783122D+04   .14734253D+04
!     2  -.11175029D+04  -.14008238D+04  -.13643952D+04   .11274890D+04
!     3   .12189620D+04  -.11948548D+04  -.11920871D+04  -.11843035D+04
!     4   .12666712D+04   .96778324D+03   .93887194D+03  -.12483664D+04
!     5  -.65878189D+03   .13018781D+04   .12682398D+04   .64152944D+03
!       ================================================================

IMPLICIT DOUBLE PRECISION (a,b,d-h,o-y)
IMPLICIT COMPLEX*16 (c,z)
COMMON cbj(0:250),cdj(0:250),cby(0:250),cdy(0:250)
WRITE(*,*)'  Please enter n, x,y (z=x+iy) '
!        READ(*,*)N,X,Y
n=3
x=4.0
y=2.0
z=CMPLX(x,y)
WRITE(*,40)x,y,n
IF (n <= 8) THEN
  ns=1
ELSE
  WRITE(*,*)'  Please enter order step Ns'
!           READ(*,*)NS
  ns=1
END IF
CALL cjynb(n,z,nm,cbj,cdj,cby,cdy)
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
30      FORMAT(1X,i4,4D16.8)
40      FORMAT(3X,3HZ =,f5.1,' + i ',f5.1,' ,',6X,6HNMAX =,i3)
END PROGRAM mcjynb


SUBROUTINE cjynb(n,z,nm,cbj,cdj,cby,cdy)

!       =======================================================
!       Purpose: Compute Bessel functions Jn(z), Yn(z) and
!                their derivatives for a complex argument
!       Input :  z --- Complex argument of Jn(z) and Yn(z)
!                n --- Order of Jn(z) and Yn(z)
!       Output:  CBJ(n) --- Jn(z)
!                CDJ(n) --- Jn'(z)
!                CBY(n) --- Yn(z)
!                CDY(n) --- Yn'(z)
!                NM --- Highest order computed
!       Routines called:
!                MSTA1 and MSTA2 to calculate the starting
!                point for backward recurrence
!       =======================================================


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
IF (a0 <= 300.d0.OR.n > INT(0.25*a0)) THEN
  IF (n == 0) nm=1
  m=msta1(a0,200)
  IF (m < nm) THEN
    nm=m
  ELSE
    m=msta2(a0,nm,15)
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
    cyy=(cbj(k)*cby(k-2)-4.0D0*(k-1.0D0)/(pi*z*z))/cbj(k-2+1 )
  END IF
  cby(k)=cyy
END DO
cdy(0)=-cby(1)
DO  k=1,nm
  cdy(k)=cby(k-1)-k/z*cby(k)
END DO
RETURN
END SUBROUTINE cjynb


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
