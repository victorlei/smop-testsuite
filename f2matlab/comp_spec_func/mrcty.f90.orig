PROGRAM mrcty
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:12

!       =======================================================
!       Purpose: This program computes the Riccati-Bessel
!                functions of the second kind and their
!                derivatives using subroutine RCTY
!       Input:   x --- Argument of Riccati-Bessel function
!                n --- Order of yn(x)
!       Output:  RY(n) --- xúyn(x)
!                DY(n) --- [xúyn(x)]'
!       Example: x = 10.0
!                  n        xúyn(x)             [xúyn(x)]'
!                --------------------------------------------
!                  0     .8390715291D+00    -.5440211109D+00
!                  1     .6279282638D+00     .7762787027D+00
!                  2    -.6506930499D+00     .7580668738D+00
!                  3    -.9532747888D+00    -.3647106133D+00
!                  4    -.1659930220D-01    -.9466350679D+00
!                  5     .9383354168D+00    -.4857670106D+00
!       =======================================================

IMPLICIT DOUBLE PRECISION (a-h,o-z)
DIMENSION ry(0:250),dy(0:250)
WRITE(*,*)'  Please enter n and x '
!        READ(*,*)N,X
n=5
x=10.0
WRITE(*,30)n,x
IF (n <= 10) THEN
  ns=1
ELSE
  WRITE(*,*)'  Please enter order step Ns'
!           READ(*,*)NS
  ns=1
END IF
WRITE(*,*)
CALL rcty(n,x,nm,ry,dy)
WRITE(*,*)
WRITE(*,*)'  n        xúyn(x)             [xúyn(x)]'''
WRITE(*,*)'--------------------------------------------'
DO  k=0,nm,ns
  WRITE(*,20)k,ry(k),dy(k)
END DO
20      FORMAT(1X,i3,2D20.10)
30      FORMAT(3X,6HNMAX =,i3,',    ',3HX =,f6.2)
END PROGRAM mrcty


SUBROUTINE rcty(n,x,nm,ry,dy)

!       ========================================================
!       Purpose: Compute Riccati-Bessel functions of the second
!                kind and their derivatives
!       Input:   x --- Argument of Riccati-Bessel function
!                n --- Order of yn(x)
!       Output:  RY(n) --- xúyn(x)
!                DY(n) --- [xúyn(x)]'
!                NM --- Highest order computed
!       ========================================================


INTEGER, INTENT(IN)                      :: n
DOUBLE PRECISION, INTENT(IN)             :: x
INTEGER, INTENT(OUT)                     :: nm
DOUBLE PRECISION, INTENT(OUT)            :: ry(0:n)
DOUBLE PRECISION, INTENT(OUT)            :: dy(0:n)
IMPLICIT DOUBLE PRECISION (a-h,o-z)


nm=n
IF (x < 1.0D-60) THEN
  DO  k=0,n
    ry(k)=-1.0D+300
    dy(k)=1.0D+300
  END DO
  ry(0)=-1.0D0
  dy(0)=0.0D0
  RETURN
END IF
ry(0)=-DCOS(x)
ry(1)=ry(0)/x-DSIN(x)
rf0=ry(0)
rf1=ry(1)
DO  k=2,n
  rf2=(2.0D0*k-1.0D0)*rf1/x-rf0
  IF (DABS(rf2) > 1.0D+300) EXIT
  ry(k)=rf2
  rf0=rf1
  rf1=rf2
END DO
20      nm=k-1
dy(0)=DSIN(x)
DO  k=1,nm
  dy(k)=-k*ry(k)/x+ry(k-1)
END DO
RETURN
END SUBROUTINE rcty
