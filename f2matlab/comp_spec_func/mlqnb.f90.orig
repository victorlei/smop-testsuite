PROGRAM mlqnb
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:12

!       ===============================================================
!       Purpose: This program computes the Legendre functions Qn(x)
!                and Qn'(x) using subroutine LQNB
!       Input :  x  --- Argument of Qn(x)
!                n  --- Degree of Qn(x)  ( n = 0,1,תתת)
!       Output:  QN(n) --- Qn(x)
!                QD(n) --- Qn'(x)
!       Examples:     x1 = 0.50,    x2 = 2.50

!       n      Qn(x1)        Qn'(x1)       Qn(x2)          Qn'(x2)
!     ----------------------------------------------------------------
!       0     .54930614    1.33333333   .42364893D+00  -.19047619D+00
!       1    -.72534693    1.21597281   .59122325D-01  -.52541546D-01
!       2    -.81866327    -.84270745   .98842555D-02  -.13109214D-01
!       3    -.19865477   -2.87734353   .17695141D-02  -.31202687D-02
!       4     .44017453   -2.23329085   .32843271D-03  -.72261513D-03
!       5     .55508089    1.08422720   .62335892D-04  -.16437427D-03
!       ===============================================================

IMPLICIT DOUBLE PRECISION (a-h,o-z)
DIMENSION qn(0:100),qd(0:100)
WRITE(*,*)'Please enter Nmax and x '
!        READ(*,*)N,X
n=5
x=.5
WRITE(*,40)x
WRITE(*,*)
WRITE(*,*)'  n          Qn(x)           Qn''(X)'
WRITE(*,*)'--------------------------------------'
CALL lqnb(n,x,qn,qd)
DO  k=0,n
  IF (DABS(x) < 1.0) THEN
    WRITE(*,20)k,qn(k),qd(k)
  ELSE
    WRITE(*,30)k,qn(k),qd(k)
  END IF
END DO
20      FORMAT(1X,i3,2F17.8)
30      FORMAT(1X,i3,2D17.8)
40      FORMAT(3X,'x =',f5.2)
END PROGRAM mlqnb


SUBROUTINE lqnb(n,x,qn,qd)

!       ====================================================
!       Purpose: Compute Legendre functions Qn(x) & Qn'(x)
!       Input :  x  --- Argument of Qn(x)
!                n  --- Degree of Qn(x)  ( n = 0,1,2,תתת)
!       Output:  QN(n) --- Qn(x)
!                QD(n) --- Qn'(x)
!       ====================================================


INTEGER, INTENT(IN)                      :: n
DOUBLE PRECISION, INTENT(IN)             :: x
DOUBLE PRECISION, INTENT(OUT)            :: qn(0:n)
DOUBLE PRECISION, INTENT(OUT)            :: qd(0:n)
IMPLICIT DOUBLE PRECISION (a-h,o-z)


eps=1.0D-14
IF (DABS(x) == 1.0D0) THEN
  DO  k=0,n
    qn(k)=1.0D+300
    qd(k)=1.0D+300
  END DO
  RETURN
END IF
IF (x <= 1.021D0) THEN
  x2=DABS((1.0D0+x)/(1.0D0-x))
  q0=0.5D0*DLOG(x2)
  q1=x*q0-1.0D0
  qn(0)=q0
  qn(1)=q1
  qd(0)=1.0D0/(1.0D0-x*x)
  qd(1)=qn(0)+x*qd(0)
  DO  k=2,n
    qf=((2.0D0*k-1.0D0)*x*q1-(k-1.0D0)*q0)/k
    qn(k)=qf
    qd(k)=(qn(k-1)-x*qf)*k/(1.0D0-x*x)
    q0=q1
    q1=qf
  END DO
ELSE
  qc2=1.0D0/x
  DO  j=1,n
    qc2=qc2*j/((2.0*j+1.0D0)*x)
    IF (j == n-1) qc1=qc2
  END DO
  DO  l=0,1
    nl=n+l
    qf=1.0D0
    qr=1.0D0
    DO  k=1,500
      qr=qr*(0.5D0*nl+k-1.0D0)*(0.5D0*(nl-1)+k) /((nl+k-0.5D0)*k*x*x)
      qf=qf+qr
      IF (DABS(qr/qf) < eps) EXIT
    END DO
    30            IF (l == 0) THEN
      qn(n-1)=qf*qc1
    ELSE
      qn(n)=qf*qc2
    END IF
  END DO
  qf2=qn(n)
  qf1=qn(n-1)
  DO  k=n,2,-1
    qf0=((2*k-1.0D0)*x*qf1-k*qf2)/(k-1.0D0)
    qn(k-2)=qf0
    qf2=qf1
    qf1=qf0
  END DO
  qd(0)=1.0D0/(1.0D0-x*x)
  DO  k=1,n
    qd(k)=k*(qn(k-1)-x*qn(k))/(1.0D0-x*x)
  END DO
END IF
RETURN
END SUBROUTINE lqnb
