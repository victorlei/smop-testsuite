PROGRAM meulera
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:11

!       ==========================================================
!       Purpose: This program computes Euler number En using
!                subroutine EULERA
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
CALL eulera(n,e)
WRITE(*,*)'   n            En'
WRITE(*,*)' --------------------------'
DO  k=0,n,2
  WRITE(*,20)k,e(k)
END DO
20      FORMAT(2X,i3,d22.12)
END PROGRAM meulera


SUBROUTINE eulera(n,en)

!       ======================================
!       Purpose: Compute Euler number En
!       Input :  n --- Serial number
!       Output:  EN(n) --- En
!       ======================================


INTEGER, INTENT(IN)                      :: n
DOUBLE PRECISION, INTENT(OUT)            :: en(0:n)
IMPLICIT DOUBLE PRECISION (a-h,o-z)


en(0)=1.0D0
DO  m=1,n/2
  s=1.0D0
  DO  k=1,m-1
    r=1.0D0
    DO  j=1,2*k
      r=r*(2.0D0*m-2.0D0*k+j)/j
    END DO
    s=s+r*en(2*k)
  END DO
  en(2*m)=-s
END DO
RETURN
END SUBROUTINE eulera
