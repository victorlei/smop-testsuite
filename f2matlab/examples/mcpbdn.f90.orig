PROGRAM mcpbdn
 
!From the book "Computation of Special Functions."
! by Shanjie Zhang, and Jianming Jin, New York : Wiley, c1996.

! Code converted using TO_F90 by Alan Miller
! Date: 2002-05-22  Time: 09:54:19

!       =============================================================
!       Purpose: This program computes parabolic cylinder functions
!                Dn(z) for an integer order and a complex argument
!                using subroutine CPBDN
!       Input :  x --- Real part of z
!                y --- Imaginary part of z
!                n --- Order of Dn(z)
!       Output:  CPB(|n|) --- Dn(z)
!                CPD(|n|) --- Dn'(z)
!       Example:
!                z = 5.0+ 5.0 i

!     n     Re[Dn(z)]      Im[Dn(z)]      Re[Dn'(z)]     Im[Dn'(z)]
!   -----------------------------------------------------------------
!     0   .99779828D+00  .66321897D-01 -.23286910D+01 -.26603004D+01
!     1   .46573819D+01  .53206009D+01  .26558457D+01 -.24878635D+02
!     2  -.43138931D+01  .49823592D+02  .14465848D+03 -.10313305D+03
!     3  -.28000219D+03  .21690729D+03  .12293320D+04  .30720802D+03
!     4  -.24716057D+04 -.46494526D+03  .38966424D+04  .82090067D+04
!    -1   .10813809D+00 -.90921592D-01 -.50014908D+00 -.23280660D-01
!    -2   .24998820D-02 -.19760577D-01 -.52486940D-01  .47769856D-01
!    -3  -.15821033D-02 -.23090595D-02 -.68249161D-03  .10032670D-01
!    -4  -.37829961D-03 -.10158757D-03  .89032322D-03  .11093416D-02
!       =============================================================

IMPLICIT DOUBLE PRECISION (x,y)
IMPLICIT COMPLEX*16 (c,z)
DIMENSION cpb(0:100),cpd(0:100)
WRITE(*,*)'Please enter n, x and y '
! READ(*,*)N,X,Y
! WRITE(*,20)N,X,Y
n=5
x=5
y=5
z=CMPLX(x,y)
n0=ABS(n)
CALL cpbdn(n,z,cpb,cpd)
WRITE(*,*)
IF (n >= 0) THEN
  WRITE(*,*)'  n     Re[Dn(z)]       Im[Dn(z)]       ',  &
      'Re[Dn''(Z)]      IM[DN''(Z)]'
ELSE
  WRITE(*,*)' -n     Re[Dn(z)]       Im[Dn(z)]       ',  &
      'Re[Dn''(Z)]      IM[DN''(Z)]'
  print *,' -n     Re[Dn(z)]       Im[Dn(z)]       ',  &
      'Re[Dn''(Z)]      IM[DN''(Z)]'
END IF
WRITE(*,*)'-------------------------------------------',  &
    '-------------------------'
DO  i=0,n0
  WRITE(*,10)i,cpb(i+1),cpd(i+1)
END DO
10      FORMAT(1X,i3,4D16.8)
20      FORMAT(1X,'N =',i3,',   z =x+iy :',f6.2,'+',f6.2,' i')
END PROGRAM mcpbdn


SUBROUTINE cpbdn(n,z,cpb,cpd)

!       ==================================================
!       Purpose: Compute the parabolic cylinder functions
!                 Dn(z) and Dn'(z) for a complex argument
!       Input:   z --- Complex argument of Dn(z)
!                n --- Order of Dn(z)  ( n=0,ס1,ס2,תתת )
!       Output:  CPB(|n|) --- Dn(z)
!                CPD(|n|) --- Dn'(z)
!       Routines called:
!            (1) CPDSA for computing Dn(z) for a small |z|
!            (2) CPDLA for computing Dn(z) for a large |z|
!       ==================================================

IMPLICIT DOUBLE PRECISION (a-b,d-h,o-y)
IMPLICIT COMPLEX*16 (c,z)

INTEGER, INTENT(IN)                      :: n
COMPLEX, INTENT(IN)                      :: z
COMPLEX, INTENT(OUT)                     :: cpb(0:*)
COMPLEX, INTENT(OUT)                     :: cpd(0:*)

pi=3.141592653589793D0
x=REAL(z)
a0=CDABS(z)
c0=(0.0D0,0.0D0)
ca0=CDEXP(-0.25D0*z*z)
IF (n >= 0) THEN
  cf0=ca0
  cf1=z*ca0
  cpb(0+1)=cf0
  cpb(1+1)=cf1
  DO  k=2,n
    cf=z*cf1-(k-1.0D0)*cf0
    cpb(k+1)=cf
    cf0=cf1
    cf1=cf
  END DO
ELSE
  n0=-n
  IF (x <= 0.0.OR.CDABS(z) == 0.0) THEN
    cf0=ca0
    cpb(0+1)=cf0
    z1=-z
    IF (a0 <= 7.0) THEN
      CALL cpdsa(-1,z1,cf1)
    ELSE
      CALL cpdla(-1,z1,cf1)
    END IF
    cf1=DSQRT(2.0D0*pi)/ca0-cf1
    cpb(1+1)=cf1
    DO  k=2,n0
      cf=(-z*cf1+cf0)/(k-1.0D0)
      cpb(k+1)=cf
      cf0=cf1
      cf1=cf
    END DO
  ELSE
    IF (a0 <= 3.0) THEN
      CALL cpdsa(-n0,z,cfa)
      cpb(n0+1)=cfa
      n1=n0+1
      CALL cpdsa(-n1,z,cfb)
      cpb(n1+1)=cfb
      nm1=n0-1
      DO  k=nm1,0,-1
        cf=z*cfa+(k+1.0D0)*cfb
        cpb(k+1)=cf
        cfb=cfa
        cfa=cf
      END DO
    ELSE
      m=100+ABS(n)
      cfa=c0
      cfb=(1.0D-30,0.0D0)
      DO  k=m,0,-1
        cf=z*cfb+(k+1.0D0)*cfa
        IF (k <= n0) cpb(k+1)=cf
        cfa=cfb
        cfb=cf
      END DO
      cs0=ca0/cf
      DO  k=0,n0
        cpb(k+1)=cs0*cpb(k+1)
      END DO
    END IF
  END IF
END IF
cpd(0+1)=-0.5D0*z*cpb(0+1)
IF (n >= 0) THEN
  DO  k=1,n
    cpd(k+1)=-0.5D0*z*cpb(k+1)+k*cpb(k-1+1)
  END DO
ELSE
  DO  k=1,n0
    cpd(k+1)=0.5D0*z*cpb(k+1)-cpb(k-1+1)
  END DO
END IF
RETURN
END SUBROUTINE cpbdn


SUBROUTINE cpdsa(n,z,cdn)

!       ===========================================================
!       Purpose: Compute complex parabolic cylinder function Dn(z)
!                for small argument
!       Input:   z   --- complex argument of D(z)
!                n   --- Order of D(z) (n = 0,-1,-2,תתת)
!       Output:  CDN --- Dn(z)
!       Routine called: GAIH for computing ג(x), x=n/2 (n=1,2,...)
!       ===========================================================


IMPLICIT DOUBLE PRECISION (a-b,d-h,o-y)
IMPLICIT COMPLEX*16 (c,z)
INTEGER, INTENT(IN)                      :: n
COMPLEX, INTENT(IN)                      :: z
COMPLEX, INTENT(OUT)                     :: cdn

eps=1.0D-15
pi=3.141592653589793D0
sq2=DSQRT(2.0D0)
ca0=CDEXP(-.25D0*z*z)
va0=0.5D0*(1.0D0-n)
IF (n == 0.0) THEN
  cdn=ca0
ELSE
  IF (CDABS(z) == 0.0) THEN
    IF (va0 <= 0.0.AND.va0 == INT(va0)) THEN
      cdn=0.0D0
    ELSE
      CALL gaih(va0,ga0)
      pd=DSQRT(pi)/(2.0D0**(-.5D0*n)*ga0)
      cdn=CMPLX(pd,0.0D0)
    END IF
  ELSE
    xn=-n
    CALL gaih(xn,g1)
    cb0=2.0D0**(-0.5D0*n-1.0D0)*ca0/g1
    vt=-.5D0*n
    CALL gaih(vt,g0)
    cdn=CMPLX(g0,0.0D0)
    cr=(1.0D0,0.0D0)
    DO  m=1,250
      vm=.5D0*(m-n)
      CALL gaih(vm,gm)
      cr=-cr*sq2*z/m
      cdw=gm*cr
      cdn=cdn+cdw
      IF (CDABS(cdw) < CDABS(cdn)*eps) EXIT
    END DO
20  cdn=cb0*cdn
  END IF
END IF
RETURN
END SUBROUTINE cpdsa


SUBROUTINE cpdla(n,z,cdn)

!       ===========================================================
!       Purpose: Compute complex parabolic cylinder function Dn(z)
!                for large argument
!       Input:   z   --- Complex argument of Dn(z)
!                n   --- Order of Dn(z) (n = 0,ס1,ס2,תתת)
!       Output:  CDN --- Dn(z)
!       ===========================================================


IMPLICIT DOUBLE PRECISION (a-b,d-h,o-y)
IMPLICIT COMPLEX*16 (c,z)
INTEGER, INTENT(IN)                      :: n
COMPLEX, INTENT(IN)                      :: z
COMPLEX, INTENT(OUT)                     :: cdn

cb0=z**n*CDEXP(-.25D0*z*z)
cr=(1.0D0,0.0D0)
cdn=(1.0D0,0.0D0)
DO  k=1,16
  cr=-0.5D0*cr*(2.0*k-n-1.0)*(2.0*k-n-2.0)/(k*z*z)
  cdn=cdn+cr
  IF (CDABS(cr) < CDABS(cdn)*1.0D-12) EXIT
END DO
15      cdn=cb0*cdn
RETURN
END SUBROUTINE cpdla


SUBROUTINE gaih(x,ga)

!       =====================================================
!       Purpose: Compute gamma function ג(x)
!       Input :  x  --- Argument of ג(x), x = n/2, n=1,2,תתת
!       Output:  GA --- ג(x)
!       =====================================================


IMPLICIT DOUBLE PRECISION (a-h,o-z)
DOUBLE PRECISION, INTENT(IN)             :: x
DOUBLE PRECISION, INTENT(OUT)            :: ga

pi=3.141592653589793D0
IF (x == INT(x).AND.x > 0.0) THEN
  ga=1.0D0
  m1=INT(x-1.0)
  DO  k=2,m1
    ga=ga*k
  END DO
ELSE IF (x+.5D0 == INT(x+.5D0).AND.x > 0.0) THEN
  m=INT(x)
  ga=DSQRT(pi)
  DO  k=1,m
    ga=0.5D0*ga*(2.0D0*k-1.0D0)
  END DO
END IF
RETURN
END SUBROUTINE gaih
