PROGRAM mcjk
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:10

!       ============================================================
!       Purpose: This program computes the expansion coefficients
!                for the asymptotic expansion of Bessel functions
!                with large orders using subroutine CJK
!       Input :  Km   --- Maximum k
!       Output:  A(L) --- Cj(k) where j and k are related to L by
!                         L=j+1+[k*(k+1)]/2; j,k=0,1,2,...,Km
!       ============================================================

IMPLICIT DOUBLE PRECISION (a-h,o-z)
DIMENSION a(231)
WRITE(*,*)'Please enter Km ( ó 20 )'
!        READ(*,*)KM
km=2
lm=km+1+(km*(km+1))/2
CALL cjk(km,a)
DO  k=1,lm
  WRITE(*,15)k,a(k)
END DO
15      FORMAT(1X,i3,d25.14)
END PROGRAM mcjk


SUBROUTINE cjk(km,a)

!       ========================================================
!       Purpose: Compute the expansion coefficients for the
!                asymptotic expansion of Bessel functions
!                with large orders
!       Input :  Km   --- Maximum k
!       Output:  A(L) --- Cj(k) where j and k are related to L
!                         by L=j+1+[k*(k+1)]/2; j,k=0,1,...,Km
!       ========================================================


INTEGER, INTENT(IN)                      :: km
DOUBLE PRECISION, INTENT(OUT)            :: a(*)
IMPLICIT DOUBLE PRECISION (a-h,o-z)


a(1)=1.0D0
f0=1.0D0
g0=1.0D0
DO  k=0,km-1
  l1=(k+1)*(k+2)/2+1
  l2=(k+1)*(k+2)/2+k+2
  f=(0.5D0*k+0.125D0/(k+1))*f0
  g=-(1.5D0*k+0.625D0/(3.0*(k+1.0D0)))*g0
  a(l1)=f
  a(l2)=g
  f0=f
  g0=g
END DO
DO  k=1,km-1
  DO  j=1,k
    l3=k*(k+1)/2+j+1
    l4=(k+1)*(k+2)/2+j+1
    a(l4)=(j+0.5D0*k+0.125D0/(2.0*j+k+1.0))*a(l3)  &
        -(j+0.5D0*k-1.0+0.625D0/(2.0*j+k+1.0))*a(l3-1)
  END DO
END DO
RETURN
END SUBROUTINE cjk
