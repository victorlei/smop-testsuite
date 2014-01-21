PROGRAM mlpmn
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:12

!       ==========================================================
!       Purpose: This program computes the associated Legendre
!                functions Pmn(x) and their derivatives Pmn'(x)
!                using subroutine LPMN
!       Input :  x --- Argument of Pmn(x)
!                m --- Order of Pmn(x),  m = 0,1,2,...,n
!                n --- Degree of Pmn(x), n = 0,1,2,...,N
!       Output:  PM(m,n) --- Pmn(x)
!                PD(m,n) --- Pmn'(x)
!       Example: x = 0.50
!          Pmn(x):
!          m\n        1            2            3            4
!         --------------------------------------------------------
!           0      .500000     -.125000     -.437500     -.289063
!           1     -.866025    -1.299038     -.324760     1.353165
!           2      .000000     2.250000     5.625000     4.218750
!           3      .000000      .000000    -9.742786   -34.099750
!           4      .000000      .000000      .000000    59.062500

!          Pmn'(x):
!          m\n        1            2            3            4
!         --------------------------------------------------------
!           0     1.000000     1.500000      .375000    -1.562500
!           1      .577350    -1.732051    -6.278684    -5.773503
!           2      .000000    -3.000000     3.750000    33.750000
!           3      .000000      .000000    19.485572      .000000
!           4      .000000      .000000      .000000  -157.500000
!       ==========================================================

IMPLICIT DOUBLE PRECISION (p,x)
DIMENSION pm(0:100,0:100),pd(0:100,0:100)
WRITE(*,*)'  Please enter m, n and x'
!        READ(*,*) M,N,X
m=2
n=4
x=.5
WRITE(*,*)
WRITE(*,*)'  m     n      x          Pmn(x)         Pmn''(X)'
WRITE(*,*)' ---------------------------------------------------'
CALL lpmn(100,m,n,x,pm,pd)
DO  j=0,n
  WRITE(*,10)m,j,x,pm(m,j),pd(m,j)
END DO
10      FORMAT(1X,i3,3X,i3,3X,f5.1,2E17.8)
END PROGRAM mlpmn


SUBROUTINE lpmn(mm,m,n,x,pm,pd)

!       =====================================================
!       Purpose: Compute the associated Legendre functions
!                Pmn(x) and their derivatives Pmn'(x)
!       Input :  x  --- Argument of Pmn(x)
!                m  --- Order of Pmn(x),  m = 0,1,2,...,n
!                n  --- Degree of Pmn(x), n = 0,1,2,...,N
!                mm --- Physical dimension of PM and PD
!       Output:  PM(m,n) --- Pmn(x)
!                PD(m,n) --- Pmn'(x)
!       =====================================================


INTEGER, INTENT(IN OUT)                  :: mm
INTEGER, INTENT(IN)                      :: m
INTEGER, INTENT(IN)                      :: n
DOUBLE PRECISION, INTENT(IN)             :: x
DOUBLE PRECISION, INTENT(OUT)            :: pm(0:mm,0:n)
DOUBLE PRECISION, INTENT(OUT)            :: pd(0:mm,0:n)
IMPLICIT DOUBLE PRECISION (p,x)


DO  i=0,n
  DO  j=0,m
    pm(j,i)=0.0D0
    pd(j,i)=0.0D0
  END DO
END DO
pm(0,0)=1.0D0
IF (DABS(x) == 1.0D0) THEN
  DO  i=1,n
    pm(0,i)=x**i
    pd(0,i)=0.5D0*i*(i+1.0D0)*x**(i+1)
  END DO
  DO  j=1,n
    DO  i=1,m
      IF (i == 1) THEN
        pd(i,j)=1.0D+300
      ELSE IF (i == 2) THEN
        pd(i,j)=-0.25D0*(j+2)*(j+1)*j*(j-1)*x**(j+1)
      END IF
    END DO
  END DO
  RETURN
END IF
ls=1
IF (DABS(x) > 1.0D0) ls=-1
xq=DSQRT(ls*(1.0D0-x*x))
xs=ls*(1.0D0-x*x)
DO  i=1,m
  pm(i,i)=-ls*(2.0D0*i-1.0D0)*xq*pm(i-1,i-1)
END DO
DO  i=0,m
  pm(i,i+1)=(2.0D0*i+1.0D0)*x*pm(i,i)
END DO
DO  i=0,m
  DO  j=i+2,n
    pm(i,j)=((2.0D0*j-1.0D0)*x*pm(i,j-1)- (i+j-1.0D0)*pm(i,j-2))/(j-i)
  END DO
END DO
pd(0,0)=0.0D0
DO  j=1,n
  pd(0,j)=ls*j*(pm(0,j-1)-x*pm(0,j))/xs
END DO
DO  i=1,m
  DO  j=i,n
    pd(i,j)=ls*i*x*pm(i,j)/xs+(j+i) *(j-i+1.0D0)/xq*pm(i-1,j)
  END DO
END DO
RETURN
END SUBROUTINE lpmn
