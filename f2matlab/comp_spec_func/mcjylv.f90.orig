PROGRAM mcjylv
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:10

!       ========================================================
!       Purpose: This program computes Bessel functions Jv(z)
!                and Yv(z) and their derivatives with a large
!                order and complex argument using subroutine
!                CJYLV
!       Input:   v --- Order of Jv(z) and Yv(z)
!                z --- Complex argument
!       Output:  CBJV --- Jv(z)
!                CDJV --- Jv'(z)
!                CBYV --- Yv(z)
!                CDYV --- Yv'(z)
!       Examples:
!                v = 100.00,    z = 4.00 + 2.00 i

!                Jv(z) = -.6444792518-123 + .6619157435-123 i
!                Jv'(z)= -.6251103777-122 + .1967638668-121 i
!                Yv(z) =  .2403065353+121 + .2472039414+121 i
!                Yv'(z)= -.7275814786+122 - .2533588851+122 i

!                v =100.5,     z = 4.00 + 2.00 i

!                Jv(z) = -.1161315754-123 + .7390127781-124 i
!                Jv'(z)= -.1588519437-122 + .2652227059-122 i
!                Yv(z) =  .1941381412+122 + .1237578195+122 i
!                Yv'(z)= -.5143285247+123 - .5320026773+122 i
!       ========================================================

IMPLICIT DOUBLE PRECISION (v,x,y)
IMPLICIT COMPLEX*16 (c,z)
WRITE(*,*)'Please enter v,x and y ( z = x+iy )'
!        READ(*,*)V,X,Y
v=100.0
x=4.0
y=2.0
WRITE(*,10)v,x,y
z=CMPLX(x,y)
CALL cjylv(v,z,cbjv,cdjv,cbyv,cdyv)
WRITE(*,*)
WRITE(*,20)cbjv
WRITE(*,30)cdjv
WRITE(*,*)
WRITE(*,40)cbyv
WRITE(*,50)cdyv
10      FORMAT(8X,'v = ',f6.2,',    ','z =',f7.2,' + i ',f7.2)
20      FORMAT(8X,'Jv(z) =',d17.10,' + i',d17.10)
30      FORMAT(8X,'Jv''(Z)=',d17.10,' + I',d17.10)
40      FORMAT(8X,'Yv(z) =',d17.10,' + i',d17.10)
50      FORMAT(8X,'Yv''(Z)=',d17.10,' + I',d17.10)
END PROGRAM mcjylv


SUBROUTINE cjylv(v,z,cbjv,cdjv,cbyv,cdyv)

!       ===================================================
!       Purpose: Compute Bessel functions Jv(z) and Yv(z)
!                and their derivatives with a complex
!                argument and a large order
!       Input:   v --- Order of Jv(z) and Yv(z)
!                z --- Complex argument
!       Output:  CBJV --- Jv(z)
!                CDJV --- Jv'(z)
!                CBYV --- Yv(z)
!                CDYV --- Yv'(z)
!       Routine called:
!                CJK to compute the expansion coefficients
!       ===================================================


DOUBLE PRECISION, INTENT(IN)             :: v
COMPLEX, INTENT(IN)                      :: z
COMPLEX, INTENT(OUT)                     :: cbjv
COMPLEX, INTENT(OUT)                     :: cdjv
COMPLEX, INTENT(OUT)                     :: cbyv
COMPLEX, INTENT(OUT)                     :: cdyv
IMPLICIT DOUBLE PRECISION (a,b,d-h,o-y)
IMPLICIT COMPLEX*16 (c,z)
DIMENSION cf(12),a(91)

km=12
CALL cjk(km,a)
pi=3.141592653589793D0
DO  l=1,0,-1
  v0=v-l
  cws=CDSQRT(1.0D0-(z/v0)*(z/v0))
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
  csj=(1.0D0,0.0D0)
  DO  k=1,km
    csj=csj+cf(k)*vr**k
  END DO
  cbjv=CDSQRT(ct/(2.0D0*pi*v0))*CDEXP(v0*ceta)*csj
  IF (l == 1) cfj=cbjv
  csy=(1.0D0,0.0D0)
  DO  k=1,km
    csy=csy+(-1)**k*cf(k)*vr**k
  END DO
  cbyv=-CDSQRT(2.0D0*ct/(pi*v0))*CDEXP(-v0*ceta)*csy
  IF (l == 1) cfy=cbyv
END DO
cdjv=-v/z*cbjv+cfj
cdyv=-v/z*cbyv+cfy
RETURN
END SUBROUTINE cjylv


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

