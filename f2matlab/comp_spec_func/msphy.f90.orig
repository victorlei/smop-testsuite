PROGRAM msphy
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:12

!       ========================================================
!       Purpose: This program computes the spherical Bessel
!                functions yn(x) and yn'(x) using subroutine
!                SPHY
!       Input :  x --- Argument of yn(x) ( x ע 0 )
!                n --- Order of yn(x) ( n = 0,1,תתת, ף 250 )
!       Output:  SY(n) --- yn(x)
!                DY(n) --- yn'(x)
!       Example:   x = 10.0
!                  n          yn(x)               yn'(x)
!                --------------------------------------------
!                  0     .8390715291D-01    -.6279282638D-01
!                  1     .6279282638D-01     .7134858763D-01
!                  2    -.6506930499D-01     .8231361788D-01
!                  3    -.9532747888D-01    -.2693831344D-01
!                  4    -.1659930220D-02    -.9449751377D-01
!                  5     .9383354168D-01    -.5796005523D-01
!       ========================================================

IMPLICIT DOUBLE PRECISION (a-h,o-z)
DIMENSION sy(0:250),dy(0:250)
WRITE(*,*)'Please enter n and x '
!        READ(*,*)N,X
n=5
x=10.0
WRITE(*,30)n,x
IF (n <= 10) THEN
  ns=1
ELSE
  WRITE(*,*)'Please enter order step Ns'
!           READ(*,*)NS
  ns=1
END IF
CALL sphy(n,x,nm,sy,dy)
WRITE(*,*)
WRITE(*,*)'  n          yn(x)               yn''(X)'
WRITE(*,*)'--------------------------------------------'
DO  k=0,nm,ns
  WRITE(*,20)k,sy(k),dy(k)
END DO
20      FORMAT(1X,i3,2D20.10)
30      FORMAT(3X,6HNMAX =,i3,',     ',2HX=,f6.1)
END PROGRAM msphy


SUBROUTINE sphy(n,x,nm,sy,dy)

!       ======================================================
!       Purpose: Compute spherical Bessel functions yn(x) and
!                their derivatives
!       Input :  x --- Argument of yn(x) ( x ע 0 )
!                n --- Order of yn(x) ( n = 0,1,תתת )
!       Output:  SY(n) --- yn(x)
!                DY(n) --- yn'(x)
!                NM --- Highest order computed
!       ======================================================


INTEGER, INTENT(IN)                      :: n
DOUBLE PRECISION, INTENT(IN)             :: x
INTEGER, INTENT(OUT)                     :: nm
DOUBLE PRECISION, INTENT(OUT)            :: sy(0:n)
DOUBLE PRECISION, INTENT(OUT)            :: dy(0:n)
IMPLICIT DOUBLE PRECISION (a-h,o-z)


nm=n
IF (x < 1.0D-60) THEN
  DO  k=0,n
    sy(k)=-1.0D+300
    dy(k)=1.0D+300
  END DO
  RETURN
END IF
sy(0)=-DCOS(x)/x
sy(1)=(sy(0)-DSIN(x))/x
f0=sy(0)
f1=sy(1)
DO  k=2,n
  f=(2.0D0*k-1.0D0)*f1/x-f0
  sy(k)=f
  IF (DABS(f) >= 1.0D+300) EXIT
  f0=f1
  f1=f
END DO
20       nm=k-1
dy(0)=(DSIN(x)+DCOS(x)/x)/x
DO  k=1,nm
  dy(k)=sy(k-1)-(k+1.0D0)*sy(k)/x
END DO
RETURN
END SUBROUTINE sphy
