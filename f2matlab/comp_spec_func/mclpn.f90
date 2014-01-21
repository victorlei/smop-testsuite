PROGRAM mclpn
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:10

!       ==========================================================
!       Purpose: This program computes the Legendre polynomials
!                Pn(z) and Pn'(z) for a complex argument using
!                subroutine CLPN
!       Input :  x --- Real part of z
!                y --- Imaginary part of z
!                n --- Degree of Pn(z), n = 0,1,...,N
!       Output:  CPN(n) --- Pn(z)
!                CPD(n) --- Pn'(z)
!       Example: z = 3.0 +2.0 i

!       n    Re[Pn(z)]     Im[Pn(z)]     Re[Pn'(z)]   Im[Pn'(z)]
!      -----------------------------------------------------------
!       0   .100000D+01   .000000D+00   .000000D+00   .000000D+00
!       1   .300000D+01   .200000D+01   .100000D+01   .000000D+00
!       2   .700000D+01   .180000D+02   .900000D+01   .600000D+01
!       3  -.270000D+02   .112000D+03   .360000D+02   .900000D+02
!       4  -.539000D+03   .480000D+03  -.180000D+03   .790000D+03
!       5  -.461700D+04   .562000D+03  -.481500D+04   .441000D+04
!       ==========================================================

IMPLICIT DOUBLE PRECISION (x,y)
IMPLICIT COMPLEX *16 (c,z)
DIMENSION cpn(0:100),cpd(0:100)
WRITE(*,*)'  Please enter Nmax, x and y (z=x+iy)'
!        READ(*,*)N,X,Y
n=5
x=3.0
y=2.0
WRITE(*,30)x,y
WRITE(*,*)
CALL clpn(n,x,y,cpn,cpd)
WRITE(*,*)'  n    Re[Pn(z)]     Im[Pn(z)]     Re[Pn''(Z)]', '   Im[Pn''(Z)]'
WRITE(*,*)' ---------------------------------------------', '--------------'
DO  k=0,n
  WRITE(*,20)k,cpn(k),cpd(k)
END DO
20      FORMAT(1X,i3,4D14.6)
30      FORMAT(3X,'x =',f5.1,',  ','y =',f5.1)
END PROGRAM mclpn


SUBROUTINE clpn(n,x,y,cpn,cpd)

!       ==================================================
!       Purpose: Compute Legendre polynomials Pn(z) and
!                their derivatives Pn'(z) for a complex
!                argument
!       Input :  x --- Real part of z
!                y --- Imaginary part of z
!                n --- Degree of Pn(z), n = 0,1,2,...
!       Output:  CPN(n) --- Pn(z)
!                CPD(n) --- Pn'(z)
!       ==================================================


INTEGER, INTENT(IN)                      :: n
DOUBLE PRECISION, INTENT(IN)             :: x
DOUBLE PRECISION, INTENT(IN OUT)         :: y
COMPLEX, INTENT(OUT)                     :: cpn(0:n)
COMPLEX, INTENT(OUT)                     :: cpd(0:n)
IMPLICIT DOUBLE PRECISION (x,y)
IMPLICIT COMPLEX *16 (c,z)


z=CMPLX(x,y)
cpn(0)=(1.0D0,0.0D0)
cpn(1)=z
cpd(0)=(0.0D0,0.0D0)
cpd(1)=(1.0D0,0.0D0)
cp0=(1.0D0,0.0D0)
cp1=z
DO  k=2,n
  cpf=(2.0D0*k-1.0D0)/k*z*cp1-(k-1.0D0)/k*cp0
  cpn(k)=cpf
  IF (DABS(x) == 1.0D0.AND.y == 0.0D0) THEN
    cpd(k)=0.5D0*x**(k)*k*(k+1.0D0)
  ELSE
    cpd(k)=k*(cp1-z*cpf)/(1.0D0-z*z)
  END IF
  cp0=cp1
  cp1=cpf
END DO
RETURN
END SUBROUTINE clpn
