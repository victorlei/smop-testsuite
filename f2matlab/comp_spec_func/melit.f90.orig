PROGRAM melit
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:10

!       ==========================================================
!       Purpose: This program computes complete and incomplete
!                elliptic integrals F(k,phi) and E(k,phi) using
!                subroutine ELIT
!       Input  : HK  --- Modulus k ( 0 ó k ó 1 )
!                Phi --- Argument ( in degrees )
!       Output : FE  --- F(k,phi)
!                EE  --- E(k,phi)
!       Example:
!                k = .5

!                 phi     F(k,phi)       E(k,phi)
!                -----------------------------------
!                   0      .00000000      .00000000
!                  15      .26254249      .26106005
!                  30      .52942863      .51788193
!                  45      .80436610      .76719599
!                  60     1.08955067     1.00755556
!                  75     1.38457455     1.23988858
!                  90     1.68575035     1.46746221
!       ==========================================================

IMPLICIT DOUBLE PRECISION (a-h,o-z)
WRITE(*,*)'Please enter k and phi (in degs.) '
!        READ(*,*)HK,PHI
hk=.5
phi=90
WRITE(*,*)
WRITE(*,*)'  phi     F(k,phi)       E(k,phi)'
WRITE(*,*)' -----------------------------------'
CALL elit(hk,phi,fe,ee)
WRITE(*,10)phi,fe,ee
10      FORMAT(1X,f6.2,2F13.8)
END PROGRAM melit


SUBROUTINE elit(hk,phi,fe,ee)

!       ==================================================
!       Purpose: Compute complete and incomplete elliptic
!                integrals F(k,phi) and E(k,phi)
!       Input  : HK  --- Modulus k ( 0 ó k ó 1 )
!                Phi --- Argument ( in degrees )
!       Output : FE  --- F(k,phi)
!                EE  --- E(k,phi)
!       ==================================================



DOUBLE PRECISION, INTENT(IN)             :: hk
DOUBLE PRECISION, INTENT(IN)             :: phi
DOUBLE PRECISION, INTENT(OUT)            :: fe
DOUBLE PRECISION, INTENT(OUT)            :: ee
IMPLICIT DOUBLE PRECISION (a-h,o-z)
g=0.0D0
pi=3.14159265358979D0
a0=1.0D0
b0=DSQRT(1.0D0-hk*hk)
d0=(pi/180.0D0)*phi
r=hk*hk
IF (hk == 1.0D0.AND.phi == 90.0D0) THEN
  fe=1.0D+300
  ee=1.0D0
ELSE IF (hk == 1.0D0) THEN
  fe=DLOG((1.0D0+DSIN(d0))/DCOS(d0))
  ee=DSIN(d0)
ELSE
  fac=1.0D0
  DO  n=1,40
    a=(a0+b0)/2.0D0
    b=DSQRT(a0*b0)
    c=(a0-b0)/2.0D0
    fac=2.0D0*fac
    r=r+fac*c*c
    IF (phi /= 90.0D0) THEN
      d=d0+DATAN((b0/a0)*DTAN(d0))
      g=g+c*DSIN(d)
      d0=d+pi*INT(d/pi+.5D0)
    END IF
    a0=a
    b0=b
    IF (c < 1.0D-7) EXIT
  END DO
  15         ck=pi/(2.0D0*a)
  ce=pi*(2.0D0-r)/(4.0D0*a)
  IF (phi == 90.0D0) THEN
    fe=ck
    ee=ce
  ELSE
    fe=d/(fac*a)
    ee=fe*ce/ck+g
  END IF
END IF
RETURN
END SUBROUTINE elit
