PROGRAM mlamn
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:11

!       ====================================================
!       Purpose: This program computes the lambda functions
!                and their derivatives using subroutine
!                LAMN
!       Input:   x --- Argument of lambda function
!                n --- Order of lambda function
!                      ( n = 0,1,..., n ó 250 )
!       Output:  BL(n) --- Lambda function of order n
!                DL(n) --- Derivative of lambda function
!       Example: Nmax = 5,  x = 10.00

!                 n       lambda(x)        lambda'(x)
!                ---------------------------------------
!                 0    -.24593576D+00    -.43472746D-01
!                 1     .86945492D-02    -.50926063D-01
!                 2     .20370425D-01    -.46703503D-02
!                 3     .28022102D-02     .10540929D-01
!                 4    -.84327431D-02     .89879627D-02
!                 5    -.89879627D-02     .55521954D-03
!       ====================================================
IMPLICIT DOUBLE PRECISION (a-h,o-z)
DIMENSION bl(0:250),dl(0:250)
WRITE(*,*)'  Please enter n,x = ?'
!        READ(*,*)N,X
n=5
x=10.0
WRITE(*,15)n,x
IF (n <= 10) THEN
  ns=1
ELSE
  WRITE(*,*)'  Please enter order step Ns'
!           READ(*,*)NS
  ns=1
END IF
CALL lamn(n,x,nm,bl,dl)
WRITE(*,*)
WRITE(*,*) '  n       lambda(x)        lambda''(X)'
WRITE(*,*)' ---------------------------------------'
DO  k=0,nm,ns
  WRITE(*,20)k,bl(k),dl(k)
END DO
15      FORMAT(1X,3HN =,i4,6X,3HX =,f8.2)
20      FORMAT(1X,i3,2D18.8)
END PROGRAM mlamn


SUBROUTINE lamn(n,x,nm,bl,dl)

!       =========================================================
!       Purpose: Compute lambda functions and their derivatives
!       Input:   x --- Argument of lambda function
!                n --- Order of lambda function
!       Output:  BL(n) --- Lambda function of order n
!                DL(n) --- Derivative of lambda function
!                NM --- Highest order computed
!       Routines called:
!                MSTA1 and MSTA2 for computing the start
!                point for backward recurrence
!       =========================================================


INTEGER, INTENT(IN)                      :: n
DOUBLE PRECISION, INTENT(IN)             :: x
INTEGER, INTENT(OUT)                     :: nm
DOUBLE PRECISION, INTENT(OUT)            :: bl(0:n)
DOUBLE PRECISION, INTENT(OUT)            :: dl(0:n)
IMPLICIT DOUBLE PRECISION (a-h,o-z)


nm=n
IF (DABS(x) < 1.0D-100) THEN
  DO  k=0,n
    bl(k)=0.0D0
    dl(k)=0.0D0
  END DO
  bl(0)=1.0D0
  dl(1)=0.5D0
  RETURN
END IF
IF (x <= 12.0D0) THEN
  x2=x*x
  DO  k=0,n
    bk=1.0D0
    r=1.0D0
    DO  i=1,50
      r=-0.25D0*r*x2/(i*(i+k))
      bk=bk+r
      IF (DABS(r) < DABS(bk)*1.0D-15) EXIT
    END DO
    20            bl(k)=bk
    IF (k >= 1) dl(k-1)=-0.5D0*x/k*bk
  END DO
  uk=1.0D0
  r=1.0D0
  DO  i=1,50
    r=-0.25D0*r*x2/(i*(i+n+1.0D0))
    uk=uk+r
    IF (DABS(r) < DABS(uk)*1.0D-15) EXIT
  END DO
  35         dl(n)=-0.5D0*x/(n+1.0D0)*uk
  RETURN
END IF
IF (n == 0) nm=1
m=msta1(x,200)
IF (m < nm) THEN
  nm=m
ELSE
  m=msta2(x,nm,15)
END IF
bs=0.0D0
f0=0.0D0
f1=1.0D-100
DO  k=m,0,-1
  f=2.0D0*(k+1.0D0)*f1/x-f0
  IF (k <= nm) bl(k)=f
  IF (k == 2*INT(k/2)) bs=bs+2.0D0*f
  f0=f1
  f1=f
END DO
bg=bs-f
DO  k=0,nm
  bl(k)=bl(k)/bg
END DO
r0=1.0D0
DO  k=1,nm
  r0=2.0D0*r0*k/x
  bl(k)=r0*bl(k)
END DO
dl(0)=-0.5D0*x*bl(1)
DO  k=1,nm
  dl(k)=2.0D0*k/x*(bl(k-1)-bl(k))
END DO
RETURN
END SUBROUTINE lamn


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
