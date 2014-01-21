PROGRAM mjynb
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:11

!       ====================================================
!       Purpose: This program computes Bessel functions
!                Jn(x) and Yn(x), and their derivatives
!                using subroutine JYNB
!       Input :  x --- Argument of Jn(x) & Yn(x)  ( x ע 0 )
!                n --- Order of Jn(x) & Yn(x)
!                      ( n = 0,1,2,תתת, n ף 250 )
!       Output:  BJ(n) --- Jn(x)
!                DJ(n) --- Jn'(x)
!                BY(n) --- Yn(x)
!                DY(n) --- Yn'(x)
!       Example:
!                x = 10.0

!                n        Jn(x)           Jn'(x)
!              -------------------------------------
!                0    -.2459358D+00   -.4347275D-01
!               10     .2074861D+00    .8436958D-01
!               20     .1151337D-04    .2011954D-04
!               30     .1551096D-11    .4396479D-11

!                n        Yn(x)           Yn'(x)
!              -------------------------------------
!                0     .5567117D-01   -.2490154D+00
!               10    -.3598142D+00    .1605149D+00
!               20    -.1597484D+04    .2737803D+04
!               30    -.7256142D+10    .2047617D+11
!       ====================================================

IMPLICIT DOUBLE PRECISION (a-h,o-z)
DIMENSION bj(0:250),by(0:250),dj(0:250),dy(0:250)
WRITE(*,*)'  Please enter n, x'
! READ(*,*)N,X
n=0
x=10.0
WRITE(*,30)n,x
IF (n <= 8) THEN
  ns=1
ELSE
  WRITE(*,*)'  Please enter order step Ns'
!    READ(*,*)NS
  ns=1
END IF
WRITE(*,*)
CALL jynb(n,x,nm,bj,dj,by,dy)
WRITE(*,*)'  n        Jn(x)           Jn''(X)'
WRITE(*,*)'--------------------------------------'
DO  k=0,nm,ns
  WRITE(*,40)k,bj(k),dj(k)
END DO
WRITE(*,*)
WRITE(*,*)'  n        Yn(x)           Yn''(X)'
WRITE(*,*)'--------------------------------------'
DO  k=0,nm,ns
  WRITE(*,40)k,by(k),dy(k)
END DO
30      FORMAT(3X,6HNMAX =,i3,',    ',3HX =,f6.1)
40      FORMAT(1X,i3,1X,2D16.7)
END PROGRAM mjynb


SUBROUTINE jynb(n,x,nm,bj,dj,by,dy)

!       =====================================================
!       Purpose: Compute Bessel functions Jn(x), Yn(x) and
!                their derivatives
!       Input :  x --- Argument of Jn(x) and Yn(x) ( x ע 0 )
!                n --- Order of Jn(x) and Yn(x)
!       Output:  BJ(n) --- Jn(x)
!                DJ(n) --- Jn'(x)
!                BY(n) --- Yn(x)
!                DY(n) --- Yn'(x)
!                NM --- Highest order computed
!       Routines called:
!                MSTA1 and MSTA2 to calculate the starting
!                point for backward recurrence
!       =====================================================


INTEGER, INTENT(IN)                      :: n
DOUBLE PRECISION, INTENT(IN)             :: x
INTEGER, INTENT(OUT)                     :: nm
DOUBLE PRECISION, INTENT(OUT)            :: bj(0:n)
DOUBLE PRECISION, INTENT(OUT)            :: dj(0:n)
DOUBLE PRECISION, INTENT(OUT)            :: by(0:n)
DOUBLE PRECISION, INTENT(OUT)            :: dy(0:n)
IMPLICIT DOUBLE PRECISION (a-h,o-z)
DIMENSION  a(4),b(4),a1(4),b1(4)

pi=3.141592653589793D0
r2p=.63661977236758D0
nm=n
IF (x < 1.0D-100) THEN
  DO  k=0,n
    bj(k)=0.0D0
    dj(k)=0.0D0
    by(k)=-1.0D+300
    dy(k)=1.0D+300
  END DO
  bj(0)=1.0D0
  dj(1)=0.5D0
  RETURN
END IF
IF (x <= 300.0.OR.n > INT(0.9*x)) THEN
  IF (n == 0) nm=1
  m=msta1(x,200)
  IF (m < nm) THEN
    nm=m
  ELSE
    m=msta2(x,nm,15)
  END IF
  bs=0.0D0
  su=0.0D0
  sv=0.0D0
  f2=0.0D0
  f1=1.0D-100
  DO  k=m,0,-1
    f=2.0D0*(k+1.0D0)/x*f1-f2
    IF (k <= nm) bj(k)=f
    IF (k == 2*INT(k/2).AND.k /= 0) THEN
      bs=bs+2.0D0*f
      su=su+(-1)**(k/2)*f/k
    ELSE IF (k > 1) THEN
      sv=sv+(-1)**(k/2)*k/(k*k-1.0)*f
    END IF
    f2=f1
    f1=f
  END DO
  s0=bs+f
  DO  k=0,nm
    bj(k)=bj(k)/s0
  END DO
  ec=DLOG(x/2.0D0)+0.5772156649015329D0
  by0=r2p*(ec*bj(0)-4.0D0*su/s0)
  by(0)=by0
  by1=r2p*((ec-1.0D0)*bj(1)-bj(0)/x-4.0D0*sv/s0)
  by(1)=by1
ELSE
  DATA a/-.7031250000000000D-01,.1121520996093750D+00,  &
      -.5725014209747314D+00,.6074042001273483D+01/
  DATA b/ .7324218750000000D-01,-.2271080017089844D+00,  &
      .1727727502584457D+01,-.2438052969955606D+02/
  DATA a1/.1171875000000000D+00,-.1441955566406250D+00,  &
      .6765925884246826D+00,-.6883914268109947D+01/
  DATA b1/-.1025390625000000D+00,.2775764465332031D+00,  &
      -.1993531733751297D+01,.2724882731126854D+02/
  t1=x-0.25D0*pi
  p0=1.0D0
  q0=-0.125D0/x
  DO  k=1,4
    p0=p0+a(k)*x**(-2*k)
    q0=q0+b(k)*x**(-2*k-1)
  END DO
  cu=DSQRT(r2p/x)
  bj0=cu*(p0*DCOS(t1)-q0*DSIN(t1))
  by0=cu*(p0*DSIN(t1)+q0*DCOS(t1))
  bj(0)=bj0
  by(0)=by0
  t2=x-0.75D0*pi
  p1=1.0D0
  q1=0.375D0/x
  DO  k=1,4
    p1=p1+a1(k)*x**(-2*k)
    q1=q1+b1(k)*x**(-2*k-1)
  END DO
  bj1=cu*(p1*DCOS(t2)-q1*DSIN(t2))
  by1=cu*(p1*DSIN(t2)+q1*DCOS(t2))
  bj(1)=bj1
  by(1)=by1
  DO  k=2,nm
    bjk=2.0D0*(k-1.0D0)/x*bj1-bj0
    bj(k)=bjk
    bj0=bj1
    bj1=bjk
  END DO
END IF
dj(0)=-bj(1)
DO  k=1,nm
  dj(k)=bj(k-1)-k/x*bj(k)
END DO
DO  k=2,nm
  byk=2.0D0*(k-1.0D0)*by1/x-by0
  by(k)=byk
  by0=by1
  by1=byk
END DO
dy(0)=-by(1)
DO  k=1,nm
  dy(k)=by(k-1)-k*by(k)/x
END DO
RETURN
END SUBROUTINE jynb


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
