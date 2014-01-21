PROGRAM mfcs
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:11

!     =======================================================
!     Purpose: This program computes the Fresnel integrals
!     C(x) and S(x) using subroutine FCS
!     Input :  x --- Argument of C(x) and S(x)
!     Output:  C --- C(x)
!     S --- S(x)
!     Example:
!     x          C(x)          S(x)
!     -----------------------------------
!     0.0      .00000000      .00000000
!     0.5      .49234423      .06473243
!     1.0      .77989340      .43825915
!     1.5      .44526118      .69750496
!     2.0      .48825341      .34341568
!     2.5      .45741301      .61918176
!     =======================================================

IMPLICIT DOUBLE PRECISION (a-h,o-z)
WRITE(*,*)'Please enter x '
!     READ(*,*) X
x=2.5
WRITE(*,*)'   x          C(x)          S(x)'
WRITE(*,*)' -----------------------------------'
CALL fcs(x,c,s)
WRITE(*,10)x,c,s
10   FORMAT(1X,f5.1,2F15.8)
END PROGRAM mfcs


SUBROUTINE fcs(x,c,s)

!     =================================================
!     Purpose: Compute Fresnel integrals C(x) and S(x)
!     Input :  x --- Argument of C(x) and S(x)
!     Output:  C --- C(x)
!     S --- S(x)
!     =================================================



DOUBLE PRECISION, INTENT(IN OUT)         :: x
DOUBLE PRECISION, INTENT(OUT)            :: c
DOUBLE PRECISION, INTENT(OUT)            :: s
IMPLICIT DOUBLE PRECISION (a-h,o-z)
eps=1.0D-15
pi=3.141592653589793D0
xa=DABS(x)
px=pi*xa
t=.5D0*px*xa
t2=t*t
IF (xa == 0.0) THEN
  c=0.0D0
  s=0.0D0
ELSE IF (xa < 2.5D0) THEN
  r=xa
  c=r
  DO  k=1,50
    r=-.5D0*r*(4.0D0*k-3.0D0)/k/(2.0D0*k-1.0D0) /(4.0D0*k+1.0D0)*t2
    c=c+r
    IF (DABS(r) < DABS(c)*eps) EXIT
  END DO
  15    s=xa*t/3.0D0
  r=s
  DO  k=1,50
    r=-.5D0*r*(4.0D0*k-1.0D0)/k/(2.0D0*k+1.0D0) /(4.0D0*k+3.0D0)*t2
    s=s+r
    IF (DABS(r) < DABS(s)*eps) EXIT
  END DO
ELSE IF (xa < 4.5D0) THEN
  m=INT(42.0+1.75*t)
  su=0.0D0
  c=0.0D0
  s=0.0D0
  f1=0.0D0
  f0=1.0D-100
  DO  k=m,0,-1
    f=(2.0D0*k+3.0D0)*f0/t-f1
    IF (k == INT(k/2)*2) THEN
      c=c+f
    ELSE
      s=s+f
    END IF
    su=su+(2.0D0*k+1.0D0)*f*f
    f1=f0
    f0=f
  END DO
  q=DSQRT(su)
  c=c*xa/q
  s=s*xa/q
ELSE
  r=1.0D0
  f=1.0D0
  DO  k=1,20
    r=-.25D0*r*(4.0D0*k-1.0D0)*(4.0D0*k-3.0D0)/t2
    f=f+r
  END DO
  r=1.0D0/(px*xa)
  g=r
  DO  k=1,12
    r=-.25D0*r*(4.0D0*k+1.0D0)*(4.0D0*k-1.0D0)/t2
    g=g+r
  END DO
  t0=t-INT(t/(2.0D0*pi))*2.0D0*pi
  c=.5D0+(f*DSIN(t0)-g*DCOS(t0))/px
  s=.5D0-(f*DCOS(t0)+g*DSIN(t0))/px
END IF
40   IF (x < 0.0D0) THEN
  c=-c
  s=-s
END IF
RETURN
END SUBROUTINE fcs
