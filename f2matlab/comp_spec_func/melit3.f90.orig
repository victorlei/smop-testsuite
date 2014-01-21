PROGRAM melit3
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:10

!       ==========================================================
!       Purpose: This program computes the elliptic integral of
!                the third kind using subroutine ELIT3
!       Input :  Phi --- Argument ( in degrees )
!                 k  --- Modulus   ( 0 ó k ó 1 )
!                 c  --- Parameter ( 0 ó c ó 1 )
!       Output:  EL3 ÄÄÄ Value of the elliptic integral of the
!                        third kind
!       ==========================================================

IMPLICIT DOUBLE PRECISION (a-h,o-z)
WRITE(*,*)'Please enter phi, k and c '
!        READ(*,*)PHI,HK,C
phi=45
hk=.5
c=.5
CALL elit3(phi,hk,c,el3)
WRITE(*,10)el3
10      FORMAT(1X,'EL3=',f12.8)
END PROGRAM melit3


SUBROUTINE elit3(phi,hk,c,el3)

!       =========================================================
!       Purpose: Compute the elliptic integral of the third kind
!                using Gauss-Legendre quadrature
!       Input :  Phi --- Argument ( in degrees )
!                 k  --- Modulus   ( 0 ó k ó 1.0 )
!                 c  --- Parameter ( 0 ó c ó 1.0 )
!       Output:  EL3 --- Value of the elliptic integral of the
!                        third kind
!       =========================================================


DOUBLE PRECISION, INTENT(IN)             :: phi
DOUBLE PRECISION, INTENT(IN OUT)         :: hk
DOUBLE PRECISION, INTENT(IN)             :: c
DOUBLE PRECISION, INTENT(OUT)            :: el3
IMPLICIT DOUBLE PRECISION (a-h,o-z)
DIMENSION t(10),w(10)
LOGICAL :: lb1,lb2
DATA t/.9931285991850949,.9639719272779138,  &
    .9122344282513259,.8391169718222188, .7463319064601508,.6360536807265150,  &
    .5108670019508271,.3737060887154195, .2277858511416451,.7652652113349734D-1/
DATA w/.1761400713915212D-1,.4060142980038694D-1,  &
    .6267204833410907D-1,.8327674157670475D-1,  &
    .1019301198172404,.1181945319615184, .1316886384491766,.1420961093183820,  &
    .1491729864726037,.1527533871307258/

lb1=hk == 1.0D0.AND.DABS(phi-90.0) <= 1.0D-8
lb2=c == 1.0D0.AND.DABS(phi-90.0) <= 1.0D-8
IF (lb1.OR.lb2) THEN
  el3=1.0D+300
  RETURN
END IF
c1=0.87266462599716D-2*phi
c2=c1
el3=0.0D0
DO  i=1,10
  c0=c2*t(i)
  t1=c1+c0
  t2=c1-c0
  f1=1.0D0/((1.0D0-c*DSIN(t1)*DSIN(t1))* DSQRT(1.0D0-hk*hk*DSIN(t1)*DSIN(t1)))
  f2=1.0D0/((1.0D0-c*DSIN(t2)*DSIN(t2))* DSQRT(1.0D0-hk*hk*DSIN(t2)*DSIN(t2)))
  el3=el3+w(i)*(f1+f2)
END DO
el3=c1*el3
RETURN
END SUBROUTINE elit3
