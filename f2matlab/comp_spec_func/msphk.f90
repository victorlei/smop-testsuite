PROGRAM msphk
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:12

!       ======================================================
!       Purpose: This program computes the modified spherical
!                Bessel functions kn(x) and kn'(x) using
!                subroutine SPHK
!       Input :  x --- Argument of kn(x)  ( x ò 0 )
!                n --- Order of kn(x) ( n ó 250 )
!       Output:  SK(n) --- kn(x)
!                DK(n) --- kn'(x)
!       Example: x= 10.0
!                  n          kn(x)               kn'(x)
!                --------------------------------------------
!                  0     .7131404291D-05    -.7844544720D-05
!                  1     .7844544720D-05    -.8700313235D-05
!                  2     .9484767707D-05    -.1068997503D-04
!                  3     .1258692857D-04    -.1451953914D-04
!                  4     .1829561771D-04    -.2173473743D-04
!                  5     .2905298451D-04    -.3572740841D-04
!       ======================================================

IMPLICIT DOUBLE PRECISION (a-h,o-z)
DIMENSION sk(0:250),dk(0:250)
WRITE(*,*)'Please enter n and x '
!        READ(*,*)N,X
n=5
x=10.0
WRITE(*,30)n,x
IF (n <= 10) THEN
  ns=1
ELSE
  WRITE(*,*) 'Please enter order step Ns'
!           READ(*,*) NS
  ns=1
END IF
CALL sphk(n,x,nm,sk,dk)
WRITE(*,*)
WRITE(*,*)'  n          kn(x)               kn''(X)'
WRITE(*,*)'--------------------------------------------'
DO  k=0,nm,ns
  WRITE(*,20)k,sk(k),dk(k)
END DO
20      FORMAT(1X,i3,2D20.10)
30      FORMAT(3X,'Nmax =',i3,',     ','x =',f6.1)
END PROGRAM msphk


SUBROUTINE sphk(n,x,nm,sk,dk)

!       =====================================================
!       Purpose: Compute modified spherical Bessel functions
!                of the second kind, kn(x) and kn'(x)
!       Input :  x --- Argument of kn(x)  ( x ò 0 )
!                n --- Order of kn(x) ( n = 0,1,2,... )
!       Output:  SK(n) --- kn(x)
!                DK(n) --- kn'(x)
!                NM --- Highest order computed
!       =====================================================


INTEGER, INTENT(IN)                      :: n
DOUBLE PRECISION, INTENT(IN)             :: x
INTEGER, INTENT(OUT)                     :: nm
DOUBLE PRECISION, INTENT(OUT)            :: sk(0:n)
DOUBLE PRECISION, INTENT(OUT)            :: dk(0:n)
IMPLICIT DOUBLE PRECISION (a-h,o-z)


pi=3.141592653589793D0
nm=n
IF (x < 1.0D-60) THEN
  DO  k=0,n
    sk(k)=1.0D+300
    dk(k)=-1.0D+300
  END DO
  RETURN
END IF
sk(0)=0.5D0*pi/x*DEXP(-x)
sk(1)=sk(0)*(1.0D0+1.0D0/x)
f0=sk(0)
f1=sk(1)
DO  k=2,n
  f=(2.0D0*k-1.0D0)*f1/x+f0
  sk(k)=f
  IF (DABS(f) > 1.0D+300) EXIT
  f0=f1
  f1=f
END DO
20      nm=k-1
dk(0)=-sk(1)
DO  k=1,nm
  dk(k)=-sk(k-1)-(k+1.0D0)/x*sk(k)
END DO
RETURN
END SUBROUTINE sphk
