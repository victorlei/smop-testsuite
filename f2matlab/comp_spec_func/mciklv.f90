PROGRAM mciklv
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:10

!       =========================================================
!       Purpose: This program computes modified Bessel functions
!                Iv(z) and Kv(z) and their derivatives for a
!                large order and a complex argument using
!                subroutine CIKLV
!       Input:   v --- Order of Iv(z) and Kv(z)
!                z --- Complex argument
!       Output:  CBIV --- Iv(z)
!                CDIV --- Iv'(z)
!                CBKV --- Kv(z)
!                CDKV --- Kv'(z)
!       Examples:
!                v =100.00,    z =   4.00 + i   2.00

!       Iv(z) = -.7373606617-123 + .6461109082-123 i
!       Iv'(z)= -.8307094243-122 + .2030132500-121 i
!       Kv(z) = -.3836166007+121 - .3356017795+121 i
!       Kv'(z)=  .1103271276+123 + .2886519240+122 i

!                v =100.50,    z =   4.00 + i   2.00
!       Iv(z) = -.1289940051-123 + .6845756182-124 i
!       Iv'(z)= -.1907996261-122 + .2672465997-122 i
!       Kv(z) = -.3008779281+122 - .1593719779+122 i
!       Kv'(z)=  .7653781978+123 + .1857772148+122 i
!       =========================================================

IMPLICIT DOUBLE PRECISION (v,x,y)
IMPLICIT COMPLEX*16 (c,z)
WRITE(*,*)'Please enter v,x,y ( z = x+iy )'
!        READ(*,*)V,X,Y
v=100.00
x=4.0
y=2.0
WRITE(*,10)v,x,y
z=CMPLX(x,y)
CALL ciklv(v,z,cbiv,cdiv,cbkv,cdkv)
WRITE(*,*)
WRITE(*,20)cbiv
WRITE(*,30)cdiv
WRITE(*,*)
WRITE(*,40)cbkv
WRITE(*,50)cdkv
10      FORMAT(8X,'v =',f6.2,',    ','z =',f7.2,' + i',f7.2)
20      FORMAT(8X,'Iv(z) =',d17.10,' + i ',d17.10)
30      FORMAT(8X,'Iv''(Z)=',d17.10,' + I ',d17.10)
40      FORMAT(8X,'Kv(z) =',d17.10,' + i ',d17.10)
50      FORMAT(8X,'Kv''(Z)=',d17.10,' + I ',d17.10)
END PROGRAM mciklv



SUBROUTINE ciklv(v,z,cbiv,cdiv,cbkv,cdkv)

!       =====================================================
!       Purpose: Compute modified Bessel functions Iv(z) and
!                Kv(z) and their derivatives with a complex
!                argument and a large order
!       Input:   v --- Order of Iv(z) and Kv(z)
!                z --- Complex argument
!       Output:  CBIV --- Iv(z)
!                CDIV --- Iv'(z)
!                CBKV --- Kv(z)
!                CDKV --- Kv'(z)
!       Routine called:
!                CJK to compute the expansion coefficients
!       ====================================================


DOUBLE PRECISION, INTENT(IN)             :: v
COMPLEX, INTENT(IN)                      :: z
COMPLEX, INTENT(OUT)                     :: cbiv
COMPLEX, INTENT(OUT)                     :: cdiv
COMPLEX, INTENT(OUT)                     :: cbkv
COMPLEX, INTENT(OUT)                     :: cdkv
IMPLICIT DOUBLE PRECISION (a,b,d-h,o-y)
IMPLICIT COMPLEX*16 (c,z)
DIMENSION cf(12),a(91)

pi=3.141592653589793D0
km=12
CALL cjk(km,a)
DO  l=1,0,-1
  v0=v-l
  cws=CDSQRT(1.0D0+(z/v0)*(z/v0))
  ceta=cws+CDLOG(z/v0/(1.0D0+cws))
  ct=1.0D0/cws
  ct2=ct*ct
  DO  k=1,km
    l0=k*(k+1)/2+1
    lf=l0+k
    cf(k)=a(lf)
    DO  i=lf-1,l0,-1
      cf(k)=cf(k)*ct2+a(i)
    END DO
    cf(k)=cf(k)*ct**k
  END DO
  vr=1.0D0/v0
  csi=(1.0D0,0.0D0)
  DO  k=1,km
    csi=csi+cf(k)*vr**k
  END DO
  cbiv=CDSQRT(ct/(2.0D0*pi*v0))*CDEXP(v0*ceta)*csi
  IF (l == 1) cfi=cbiv
  csk=(1.0D0,0.0D0)
  DO  k=1,km
    csk=csk+(-1)**k*cf(k)*vr**k
  END DO
  cbkv=CDSQRT(pi*ct/(2.0D0*v0))*CDEXP(-v0*ceta)*csk
  IF (l == 1) cfk=cbkv
END DO
cdiv=cfi-v/z*cbiv
cdkv=-cfk-v/z*cbkv
RETURN
END SUBROUTINE ciklv


SUBROUTINE cjk(km,a)

!       ========================================================
!       Purpose: Compute the expansion coefficients for the
!                asymptotic expansion of Bessel functions
!                with large orders
!       Input :  Km   --- Maximum k
!       Output:  A(L) --- Cj(k) where j and k are related to L
!                         by L=j+1+[k*(k+1)]/2; j,k=0,1,...,Km
!       ========================================================


INTEGER, INTENT(IN)                      :: km
DOUBLE PRECISION, INTENT(OUT)            :: a(*)
IMPLICIT DOUBLE PRECISION (a-h,o-z)


a(1)=1.0D0
f0=1.0D0
g0=1.0D0
DO  k=0,km-1
  l1=(k+1)*(k+2)/2+1
  l2=(k+1)*(k+2)/2+k+2
  f=(0.5D0*k+0.125D0/(k+1))*f0
  g=-(1.5D0*k+0.625D0/(3.0*(k+1.0D0)))*g0
  a(l1)=f
  a(l2)=g
  f0=f
  g0=g
END DO
DO  k=1,km-1
  DO  j=1,k
    l3=k*(k+1)/2+j+1
    l4=(k+1)*(k+2)/2+j+1
    a(l4)=(j+0.5D0*k+0.125D0/(2.0*j+k+1.0))*a(l3)  &
        -(j+0.5D0*k-1.0+0.625D0/(2.0*j+k+1.0))*a(l3-1)
  END DO
END DO
RETURN
END SUBROUTINE cjk

