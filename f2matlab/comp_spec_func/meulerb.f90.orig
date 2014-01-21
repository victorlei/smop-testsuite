PROGRAM meulerb
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:11

!       ==========================================================
!       Purpose: This program computes Euler number En using
!                subroutine EULERB
!       Example: Compute Euler number En for n = 0,2,...,10
!                Computed results:

!                   n            En
!                 --------------------------
!                   0     .100000000000D+01
!                   2    -.100000000000D+01
!                   4     .500000000000D+01
!                   6    -.610000000000D+02
!                   8     .138500000000D+04
!                  10    -.505210000000D+05
!       ==========================================================

DOUBLE PRECISION :: e
DIMENSION e(0:200)
WRITE(*,*)'  Please enter Nmax '
!        READ(*,*)N
n=10
CALL eulerb(n,e)
WRITE(*,*)'   n            En'
WRITE(*,*)' --------------------------'
DO  k=0,n,2
  WRITE(*,20)k,e(k)
END DO
20      FORMAT(2X,i3,d22.12)
END PROGRAM meulerb


SUBROUTINE eulerb(n,en)

!       ======================================
!       Purpose: Compute Euler number En
!       Input :  n --- Serial number
!       Output:  EN(n) --- En
!       ======================================


INTEGER, INTENT(IN)                      :: n
DOUBLE PRECISION, INTENT(OUT)            :: en(0:n)
IMPLICIT DOUBLE PRECISION (a-h,o-z)


hpi=2.0D0/3.141592653589793D0
en(0)=1.0D0
en(2)=-1.0D0
r1=-4.0D0*hpi**3
DO  m=4,n,2
  r1=-r1*(m-1)*m*hpi*hpi
  r2=1.0D0
  isgn=1.0D0
  DO  k=3,1000,2
    isgn=-isgn
    s=(1.0D0/k)**(m+1)
    r2=r2+isgn*s
    IF (s < 1.0D-15) EXIT
  END DO
  en(m)=r1*r2
END DO
RETURN
END SUBROUTINE eulerb
