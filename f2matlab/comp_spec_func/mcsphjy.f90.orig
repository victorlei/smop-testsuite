PROGRAM mcsphjy
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:10

!       ================================================================
!       Purpose: This program computes the spherical Bessel functions
!                jn(z), yn(z), and their derivatives for a complex
!                argument using subroutine CSPHJY
!       Input :  z --- Complex argument
!                n --- Order of jn(z) & yn(z) ( 0 ó n ó 250 )
!       Output:  CSJ(n) --- jn(z)
!                CDJ(n) --- jn'(z)
!                CSY(n) --- yn(z)
!                CDY(n) --- yn'(z)
!       Example: z = 4.0+i 2.0

!     n     Re[jn(z)]       Im[jn(z)]       Re[jn'(z)]      Im[jn'(z)]
!   --------------------------------------------------------------------
!     0  -.80651523D+00  -.18941093D+00  -.37101203D-01   .75210758D+00
!     1   .37101203D-01  -.75210758D+00  -.67093420D+00   .11885235D+00
!     2   .60314368D+00  -.27298399D+00  -.24288981D+00  -.40737409D+00
!     3   .42955048D+00   .17755176D+00   .18848259D+00  -.24320520D+00
!     4   .12251323D+00   .22087111D+00   .19660170D+00   .17937264D-01
!     5  -.10242676D-01   .10975433D+00   .68951842D-01   .83020305D-01

!     n     Re[yn(z)]       Im[yn(z)]       Re[yn'(z)]      Im[yn'(z)]
!   --------------------------------------------------------------------
!     0   .21734534D+00  -.79487692D+00  -.77049661D+00  -.87010064D-02
!     1   .77049661D+00   .87010064D-02  -.92593503D-01  -.64425800D+00
!     2   .24756293D+00   .56894854D+00   .45127429D+00  -.25839924D+00
!     3  -.23845941D+00   .43646607D+00   .26374403D+00   .12439192D+00
!     4  -.27587985D+00   .20902555D+00  -.67092335D-01   .89500599D-01
!     5  -.70001327D-01   .18807178D+00  -.30472133D+00  -.58661384D-01
!       ================================================================

IMPLICIT COMPLEX*16 (c,z)
DOUBLE PRECISION :: x,y
DIMENSION csj(0:250),cdj(0:250),csy(0:250),cdy(0:250)
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
CALL csphjy(n,z,nm,csj,cdj,csy,cdy)
WRITE(*,*)
WRITE(*,*)'  n      Re[jn(z)]        Im[jn(z)]',  &
    '        Re[jn''(Z)]       IM[JN''(Z)]'
WRITE(*,*)'--------------------------------------------',  &
    '----------------------------'
DO  k=0,nm,ns
  WRITE(*,20)k,csj(k),cdj(k)
END DO
WRITE(*,*)
WRITE(*,*)'  n      Re[yn(z)]        Im[yn(z)]',  &
    '        Re[yn''(Z)]       IM[YN''(Z)]'
WRITE(*,*)'--------------------------------------------',  &
    '----------------------------'
DO  k=0,nm,ns
  WRITE(*,20)k,csy(k),cdy(k)
END DO
20      FORMAT(1X,i3,4D17.8)
30      FORMAT(3X,6HNMAZ =,i3,',     ','z = ',f8.1,'+ i',f8.1)
END PROGRAM mcsphjy


SUBROUTINE csphjy(n,z,nm,csj,cdj,csy,cdy)

!       ==========================================================
!       Purpose: Compute spherical Bessel functions jn(z) & yn(z)
!                and their derivatives for a complex argument
!       Input :  z --- Complex argument
!                n --- Order of jn(z) & yn(z) ( n = 0,1,2,... )
!       Output:  CSJ(n) --- jn(z)
!                CDJ(n) --- jn'(z)
!                CSY(n) --- yn(z)
!                CDY(n) --- yn'(z)
!                NM --- Highest order computed
!       Routines called:
!                MSTA1 and MSTA2 for computing the starting
!                point for backward recurrence
!       ==========================================================


INTEGER, INTENT(IN)                      :: n
COMPLEX, INTENT(IN)                      :: z
INTEGER, INTENT(OUT)                     :: nm
COMPLEX, INTENT(OUT)                     :: csj(0:n)
COMPLEX, INTENT(OUT)                     :: cdj(0:n)
COMPLEX, INTENT(OUT)                     :: csy(0:n)
COMPLEX, INTENT(OUT)                     :: cdy(0:n)
IMPLICIT COMPLEX*16 (c,z)
DOUBLE PRECISION :: a0


a0=CDABS(z)
nm=n
IF (a0 < 1.0D-60) THEN
  DO  k=0,n
    csj(k)=0.0D0
    cdj(k)=0.0D0
    csy(k)=-1.0D+300
    cdy(k)=1.0D+300
  END DO
  csj(0)=(1.0D0,0.0D0)
  cdj(1)=(.333333333333333D0,0.0D0)
  RETURN
END IF
csj(0)=CDSIN(z)/z
csj(1)=(csj(0)-CDCOS(z))/z
IF (n >= 2) THEN
  csa=csj(0)
  csb=csj(1)
  m=msta1(a0,200)
  IF (m < n) THEN
    nm=m
  ELSE
    m=msta2(a0,n,15)
  END IF
  cf0=0.0D0
  cf1=1.0D0-100
  DO  k=m,0,-1
    cf=(2.0D0*k+3.0D0)*cf1/z-cf0
    IF (k <= nm) csj(k)=cf
    cf0=cf1
    cf1=cf
  END DO
  IF (CDABS(csa) > CDABS(csb)) cs=csa/cf
  IF (CDABS(csa) <= CDABS(csb)) cs=csb/cf0
  DO  k=0,nm
    csj(k)=cs*csj(k)
  END DO
END IF
cdj(0)=(CDCOS(z)-CDSIN(z)/z)/z
DO  k=1,nm
  cdj(k)=csj(k-1)-(k+1.0D0)*csj(k)/z
END DO
csy(0)=-CDCOS(z)/z
csy(1)=(csy(0)-CDSIN(z))/z
cdy(0)=(CDSIN(z)+CDCOS(z)/z)/z
cdy(1)=(2.0D0*cdy(0)-CDCOS(z))/z
DO  k=2,nm
  IF (CDABS(csj(k-1)) > CDABS(csj(k-2))) THEN
    csy(k)=(csj(k)*csy(k-1)-1.0D0/(z*z))/csj(k-1)
  ELSE
    csy(k)=(csj(k)*csy(k-2)-(2.0D0*k-1.0D0)/z**3)/csj(k-2)
  END IF
END DO
DO  k=2,nm
  cdy(k)=csy(k-1)-(k+1.0D0)*csy(k)/z
END DO
RETURN
END SUBROUTINE csphjy


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
