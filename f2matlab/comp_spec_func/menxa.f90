PROGRAM menxa
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:10

!       =========================================================
!       Purpose: This program computes the exponential integral
!                En(x) using subroutine ENXA
!       Example: x = 10.0
!                   n         En(x)
!                 ----------------------
!                   0     .45399930D-05
!                   1     .41569689D-05
!                   2     .38302405D-05
!                   3     .35487626D-05
!                   4     .33041014D-05
!                   5     .30897289D-05
!       =========================================================

IMPLICIT DOUBLE PRECISION (a-h,o-z)
DIMENSION en(0:100)
WRITE(*,*)'Please enter n and x '
!        READ(*,*)N,X
n=5
x=10.0
WRITE(*,20)n,x
WRITE(*,*)
WRITE(*,*)'   n         En(x)'
WRITE(*,*)' ----------------------'
CALL enxa(n,x,en)
DO  k=0,n
  WRITE(*,30)k,en(k)
END DO
20      FORMAT(5X,i3,',   ','x=',f5.1)
30      FORMAT(2X,i3,d18.8)
END PROGRAM menxa


SUBROUTINE enxa(n,x,en)

!       ============================================
!       Purpose: Compute exponential integral En(x)
!       Input :  x --- Argument of En(x) ( x ó 20 )
!                n --- Order of En(x)
!       Output:  EN(n) --- En(x)
!       Routine called: E1XB for computing E1(x)
!       ============================================


INTEGER, INTENT(IN)                      :: n
DOUBLE PRECISION, INTENT(IN)             :: x
DOUBLE PRECISION, INTENT(OUT)            :: en(0:n)
IMPLICIT DOUBLE PRECISION (a-h,o-z)


en(0)=DEXP(-x)/x
CALL e1xb(x,e1)
en(1)=e1
DO  k=2,n
  ek=(DEXP(-x)-x*e1)/(k-1.0D0)
  en(k)=ek
  e1=ek
END DO
RETURN
END SUBROUTINE enxa


SUBROUTINE e1xb(x,e1)

!       ============================================
!       Purpose: Compute exponential integral E1(x)
!       Input :  x  --- Argument of E1(x)
!       Output:  E1 --- E1(x)
!       ============================================



DOUBLE PRECISION, INTENT(IN)             :: x
DOUBLE PRECISION, INTENT(OUT)            :: e1
IMPLICIT DOUBLE PRECISION (a-h,o-z)
IF (x == 0.0) THEN
  e1=1.0D+300
ELSE IF (x <= 1.0) THEN
  e1=1.0D0
  r=1.0D0
  DO  k=1,25
    r=-r*k*x/(k+1.0D0)**2
    e1=e1+r
    IF (DABS(r) <= DABS(e1)*1.0D-15) EXIT
  END DO
  15         ga=0.5772156649015328D0
  e1=-ga-DLOG(x)+x*e1
ELSE
  m=20+INT(80.0/x)
  t0=0.0D0
  DO  k=m,1,-1
    t0=k/(1.0D0+k/(x+t0))
  END DO
  t=1.0D0/(x+t0)
  e1=DEXP(-x)*t
END IF
RETURN
END SUBROUTINE e1xb
