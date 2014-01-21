PROGRAM mjelp
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:11

!       ============================================================
!       Purpose: This program computes Jacobian elliptic functions
!                sn u, cn u and dn u using subroutine JELP
!       Input  : u   --- Argument of Jacobian elliptic fuctions
!                Hk  --- Modulus k ( 0 ó k ó 1 )
!       Output : ESN --- sn u
!                ECN --- cn u
!                EDN --- dn u
!                EPH --- phi ( in degrees )
!       Example:
!                k = .5, ( K(k) = 1.68575035 ), and u = u0*K

!                u0       phi       sn u        cn u        dn u
!              ----------------------------------------------------
!               0.0      .0000    .0000000   1.0000000   1.0000000
!               0.5    47.0586    .7320508    .6812500    .9306049
!               1.0    90.0000   1.0000000    .0000000    .8660254
!               1.5   132.9414    .7320508   -.6812500    .9306049
!               2.0   180.0000    .0000000  -1.0000000   1.0000000
!       ============================================================

IMPLICIT DOUBLE PRECISION (a-h,o-z)
WRITE(*,*)'Please enter k and u '
!        READ(*,*)HK,U
hk=.5
u=1.0*1.68575035
WRITE(*,*)
WRITE(*,*)'   k        u          phi        sn u', '        cn u        dn u'
WRITE(*,*)' -------------------------------------',  &
    '---------------------------'
CALL jelp(u,hk,esn,ecn,edn,eph)
WRITE(*,10)hk,u,eph,esn,ecn,edn
10      FORMAT(1X,f5.3,f12.7,2X,f9.5,3F12.7)
END PROGRAM mjelp


SUBROUTINE jelp(u,hk,esn,ecn,edn,eph)

!       ========================================================
!       Purpose: Compute Jacobian elliptic functions sn u, cn u
!                and dn u
!       Input  : u   --- Argument of Jacobian elliptic fuctions
!                Hk  --- Modulus k ( 0 ó k ó 1 )
!       Output : ESN --- sn u
!                ECN --- cn u
!                EDN --- dn u
!                EPH --- phi ( in degrees )
!       ========================================================

IMPLICIT DOUBLE PRECISION (a-h,o-z)
DOUBLE PRECISION, INTENT(IN)             :: u
DOUBLE PRECISION, INTENT(IN)             :: hk
DOUBLE PRECISION, INTENT(OUT)            :: esn
DOUBLE PRECISION, INTENT(OUT)            :: ecn
DOUBLE PRECISION, INTENT(OUT)            :: edn
DOUBLE PRECISION, INTENT(OUT)            :: eph
DIMENSION r(40)

pi=3.14159265358979D0
a0=1.0D0
b0=DSQRT(1.0D0-hk*hk)
DO  n=1,40
  a=(a0+b0)/2.0D0
  b=DSQRT(a0*b0)
  c=(a0-b0)/2.0D0
  r(n)=c/a
  IF (c < 1.0D-7) EXIT
  a0=a
  b0=b
END DO
15      dn=2.0D0**n*a*u
DO  j=n,1,-1
  t=r(j)*DSIN(dn)
  sa=DATAN(t/DSQRT(DABS(1.0D0-t*t)))
  d=.5D0*(dn+sa)
  dn=d
END DO
eph=d*180.0D0/pi
esn=DSIN(d)
ecn=DCOS(d)
edn=DSQRT(1.0D0-hk*hk*esn*esn)
RETURN
END SUBROUTINE jelp
