PROGRAM menxb
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:10

!       =========================================================
!       Purpose: This program computes the exponential integral
!                En(x) using subroutine ENXB
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
CALL enxb(n,x,en)
DO  k=0,n
  WRITE(*,30)k,en(k)
END DO
20      FORMAT(5X,i3,',   ','x=',f5.1)
30      FORMAT(2X,i3,d18.8)
END PROGRAM menxb


SUBROUTINE enxb(n,x,en)

!       ===============================================
!       Purpose: Compute exponential integral En(x)
!       Input :  x --- Argument of En(x)
!                n --- Order of En(x)  (n = 0,1,2,...)
!       Output:  EN(n) --- En(x)
!       ===============================================


INTEGER, INTENT(IN)                      :: n
DOUBLE PRECISION, INTENT(IN)             :: x
DOUBLE PRECISION, INTENT(OUT)            :: en(0:n)
IMPLICIT DOUBLE PRECISION (a-h,o-z)


IF (x == 0.0) THEN
  en(0)=1.0D+300
  en(1)=1.0D+300
  DO  k=2,n
    en(k)=1.0D0/(k-1.0)
  END DO
  RETURN
ELSE IF (x <= 1.0) THEN
  en(0)=DEXP(-x)/x
  DO  l=1,n
    rp=1.0D0
    DO  j=1,l-1
      rp=-rp*x/j
    END DO
    ps=-0.5772156649015328D0
    DO  m=1,l-1
      ps=ps+1.0D0/m
    END DO
    ens=rp*(-DLOG(x)+ps)
    s=0.0D0
    DO  m=0,20
      IF (.NOT.(m == l-1)) THEN
        r=1.0D0
        DO  j=1,m
          r=-r*x/j
        END DO
        s=s+r/(m-l+1.0D0)
        IF (DABS(s-s0) < DABS(s)*1.0D-15) EXIT
        s0=s
      END IF
    END DO
    35            en(l)=ens-s
  END DO
ELSE
  en(0)=DEXP(-x)/x
  m=15+INT(100.0/x)
  DO  l=1,n
    t0=0.0D0
    DO  k=m,1,-1
      t0=(l+k-1.0D0)/(1.0D0+k/(x+t0))
    END DO
    t=1.0D0/(x+t0)
    en(l)=DEXP(-x)*t
  END DO
END IF
END SUBROUTINE enxb
