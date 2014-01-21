PROGRAM mfcszo
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:11

!     ===========================================================
!     Purpose : This program computes the complex zeros of the
!     Fresnel integral C(z) or S(z) using subroutine
!     FCSZO
!     Input :   KF  --- Function code
!     KF=1 for C(z) or KF=2 for S(z)
!     NT  --- Total number of zeros
!     Output:   ZO(L) --- L-th zero of C(z) or S(z)
!     Example:  NT=10

!     n     Complex zeros of C(z)        Complex zeros of S(z)
!     ------------------------------------------------------------
!     1    1.7436675 + i .30573506      2.0092570 + i .28854790
!     2    2.6514596 + i .25290396      2.8334772 + i .24428524
!     3    3.3203593 + i .22395346      3.4675331 + i .21849268
!     4    3.8757345 + i .20474747      4.0025782 + i .20085103
!     5    4.3610635 + i .19066973      4.4741893 + i .18768859
!     6    4.7976077 + i .17970801      4.9006784 + i .17732036
!     7    5.1976532 + i .17081930      5.2929467 + i .16884418
!     8    5.5690602 + i .16339854      5.6581068 + i .16172492
!     9    5.9172173 + i .15706585      6.0011034 + i .15562108
!     10    6.2460098 + i .15156826      6.3255396 + i .15030246
!     ===========================================================

IMPLICIT DOUBLE PRECISION (e,p,w)
IMPLICIT COMPLEX *16 (c,z)
DIMENSION zo(100)
WRITE(*,*)'Please Enter KF and NT '
!     READ(*,*)KF,NT
kf=2
nt=10
WRITE(*,20)kf,nt
WRITE(*,*)' *****     Please Wait !     *****'
CALL fcszo(kf,nt,zo)
WRITE(*,*)
IF (kf == 1) WRITE(*,*)'  n        Complex zeros of C(z)'
IF (kf == 2) WRITE(*,*)'  n        Complex zeros of S(z)'
WRITE(*,*)'-----------------------------------'
DO  i=1,nt
  WRITE(*,30) i,zo(i)
END DO
20   FORMAT(2X,'KF=',i2,',     ','NT=',i3)
30   FORMAT(1X,i3,f13.8,2X,2H+i,f13.8)
END PROGRAM mfcszo


SUBROUTINE fcszo(kf,nt,zo)

!     ===============================================================
!     Purpose: Compute the complex zeros of Fresnel integral C(z)
!     or S(z) using modified Newton's iteration method
!     Input :  KF  --- Function code
!     KF=1 for C(z) or KF=2 for S(z)
!     NT  --- Total number of zeros
!     Output:  ZO(L) --- L-th zero of C(z) or S(z)
!     Routines called:
!     (1) CFC for computing Fresnel integral C(z)
!     (2) CFS for computing Fresnel integral S(z)
!     ==============================================================


INTEGER, INTENT(IN)                      :: kf
INTEGER, INTENT(IN)                      :: nt
COMPLEX, INTENT(IN OUT)                  :: zo(nt)
IMPLICIT DOUBLE PRECISION (e,p,w)
IMPLICIT COMPLEX *16 (c,z)


w=0.0
pi=3.141592653589793D0
DO  nr=1,nt
  IF (kf == 1) psq=DSQRT(4.0D0*nr-1.0D0)
  IF (kf == 2) psq=2.0D0*nr**(0.5)
  px=psq-DLOG(pi*psq)/(pi*pi*psq**3.0)
  py=DLOG(pi*psq)/(pi*psq)
  z=CMPLX(px,py)
  IF (kf == 2) THEN
    IF (nr == 2) z=(2.8334,0.2443)
    IF (nr == 3) z=(3.4674,0.2185)
    IF (nr == 4) z=(4.0025,0.2008)
  END IF
  it=0
  DO
    15     it=it+1
    IF (kf == 1) CALL cfc(z,zf,zd)
    IF (kf == 2) CALL cfs(z,zf,zd)
    zp=(1.0D0,0.0D0)
    DO  i=1,nr-1
      zp=zp*(z-zo(i))
    END DO
    zfd=zf/zp
    zq=(0.0D0,0.0D0)
    DO  i=1,nr-1
      zw=(1.0D0,0.0D0)
      DO  j=1,nr-1
        IF (.NOT.(j == i)) zw=zw*(z-zo(j))
      END DO
      zq=zq+zw
    END DO
    zgd=(zd-zq*zfd)/zp
    z=z-zfd/zgd
    w0=w
    w=CDABS(z)
    IF (.NOT.(it <= 50.AND.DABS((w-w0)/w) > 1.0D-12)) EXIT
  END DO
  zo(nr)=z
END DO
RETURN
END SUBROUTINE fcszo


SUBROUTINE cfc(z,zf,zd)

!     =========================================================
!     Purpose: Compute complex Fresnel integral C(z) and C'(z)
!     Input :  z --- Argument of C(z)
!     Output:  ZF --- C(z)
!     ZD --- C'(z)
!     =========================================================


COMPLEX, INTENT(IN)                      :: z
COMPLEX, INTENT(OUT)                     :: zf
COMPLEX, INTENT(OUT)                     :: zd
IMPLICIT DOUBLE PRECISION (e,p,w)
IMPLICIT COMPLEX *16 (c,s,z)

eps=1.0D-14
pi=3.141592653589793D0
w0=CDABS(z)
zp=0.5D0*pi*z*z
zp2=zp*zp
z0=(0.0D0,0.0D0)
IF (z == z0) THEN
  c=z0
ELSE IF (w0 <= 2.5) THEN
  cr=z
  c=cr
  igoon=1
  DO k=1,80
    cr=-.5D0*cr*(4.0D0*k-3.0D0)/k/(2.0D0*k-1.0D0) /(4.0D0*k+1.0D0)*zp2
    c=c+cr
    wa=CDABS(c)
    IF (DABS((wa-wa0)/wa) < eps.AND.k > 10) igoon=0
    IF (igoon == 1) THEN
      wa0=wa
    ELSE
      EXIT
    END IF
  END DO
ELSE IF (w0 > 2.5.AND.w0 < 4.5) THEN
  m=85
  c=z0
  cf1=z0
  cf0=(1.0D-100,0.0D0)
  DO  k=m,0,-1
    cf=(2.0D0*k+3.0D0)*cf0/zp-cf1
    IF (k == INT(k/2)*2) c=c+cf
    cf1=cf0
    cf0=cf
  END DO
  c=CDSQRT(2.0D0/(pi*zp))*CDSIN(zp)/cf*c
ELSE
  cr=(1.0D0,0.0D0)
  cf=(1.0D0,0.0D0)
  DO  k=1,20
    cr=-.25D0*cr*(4.0D0*k-1.0D0)*(4.0D0*k-3.0D0)/zp2
    cf=cf+cr
  END DO
  cr=1.0D0/(pi*z*z)
  cg=cr
  DO  k=1,12
    cr=-.25D0*cr*(4.0D0*k+1.0D0)*(4.0D0*k-1.0D0)/zp2
    cg=cg+cr
  END DO
  c=.5D0+(cf*CDSIN(zp)-cg*CDCOS(zp))/(pi*z)
END IF
30   zf=c
zd=CDCOS(0.5*pi*z*z)
RETURN
END SUBROUTINE cfc


SUBROUTINE cfs(z,zf,zd)

!     =========================================================
!     Purpose: Compute complex Fresnel Integral S(z) and S'(z)
!     Input :  z  --- Argument of S(z)
!     Output:  ZF --- S(z)
!     ZD --- S'(z)
!     =========================================================


COMPLEX, INTENT(IN)                      :: z
COMPLEX, INTENT(OUT)                     :: zf
COMPLEX, INTENT(OUT)                     :: zd
IMPLICIT DOUBLE PRECISION (e,p,w)
IMPLICIT COMPLEX *16 (c,s,z)

wb0=0.0
eps=1.0D-14
pi=3.141592653589793D0
w0=CDABS(z)
zp=0.5D0*pi*z*z
zp2=zp*zp
z0=(0.0D0,0.0D0)
IF (z == z0) THEN
  s=z0
ELSE IF (w0 <= 2.5) THEN
  s=z*zp/3.0D0
  cr=s
  igoon=1
  DO k=1,80
    cr=-.5D0*cr*(4.0D0*k-1.0D0)/k/(2.0D0*k+1.0D0) /(4.0D0*k+3.0D0)*zp2
    s=s+cr
    wb=CDABS(s)
    IF (DABS(wb-wb0) < eps.AND.k > 10) igoon=1
    IF (igoon == 1) THEN
      wb0=wb
    ELSE
      EXIT
    END IF
  END DO
ELSE IF (w0 > 2.5.AND.w0 < 4.5) THEN
  m=85
  s=z0
  cf1=z0
  cf0=(1.0D-100,0.0D0)
  DO  k=m,0,-1
    cf=(2.0D0*k+3.0D0)*cf0/zp-cf1
    IF (k /= INT(k/2)*2) s=s+cf
    cf1=cf0
    cf0=cf
  END DO
  s=CDSQRT(2.0D0/(pi*zp))*CDSIN(zp)/cf*s
ELSE
  cr=(1.0D0,0.0D0)
  cf=(1.0D0,0.0D0)
  DO  k=1,20
    cr=-.25D0*cr*(4.0D0*k-1.0D0)*(4.0D0*k-3.0D0)/zp2
    cf=cf+cr
  END DO
  cr=1.0D0/(pi*z*z)
  cg=cr
  DO  k=1,12
    cr=-.25D0*cr*(4.0D0*k+1.0D0)*(4.0D0*k-1.0D0)/zp2
    cg=cg+cr
  END DO
  s=.5D0-(cf*CDCOS(zp)+cg*CDSIN(zp))/(pi*z)
END IF
30   zf=s
zd=CDSIN(0.5*pi*z*z)
RETURN
END SUBROUTINE cfs
