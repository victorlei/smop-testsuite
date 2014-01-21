PROGRAM mcerzo
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:10

!     ===============================================================
!     Purpose : This program evaluates the complex zeros of error
!     function erf(z) using subroutine CERZO
!     Input:    NT --- Total number of zeros
!     Example:  NT = 10

!     n     complex zeros of erf(z)     n     complex zeros of erf(z)
!     ------------------------------------------------------------------
!     -
!     1   1.450616163 + i 1.880943000   6   4.158998400 + i 4.435571444
!     2   2.244659274 + i 2.616575141   7   4.516319400 + i 4.780447644
!     3   2.839741047 + i 3.175628100   8   4.847970309 + i 5.101588043
!     4   3.335460735 + i 3.646174376   9   5.158767908 + i 5.403332643
!     5   3.769005567 + i 4.060697234  10   5.452192201 + i 5.688837437
!     ===============================================================

IMPLICIT DOUBLE PRECISION (e,p,w)
IMPLICIT COMPLEX *16 (c,z)
DIMENSION zo(100)
WRITE(*,*)'Please Enter NT '
!      READ(*,*)NT
nt=10
WRITE(*,20)nt
CALL cerzo(nt,zo)
WRITE(*,*)'  *****    Please Wait ...    *****'
WRITE(*,*)
WRITE(*,*)'  n        Complex zeros of erf(z)'
WRITE(*,*)'-------------------------------------'
DO  i=1,nt
  WRITE(*,30) i,zo(i)
END DO
20   FORMAT(2X,'NT=',i3)
30   FORMAT(1X,i3,2X,f13.8,2X,2H+i,f13.8)
END PROGRAM mcerzo


SUBROUTINE cerzo(nt,zo)

!     ===============================================================
!     Purpose : Evaluate the complex zeros of error function erf(z)
!     using the modified Newton's iteration method
!     Input :   NT --- Total number of zeros
!     Output:   ZO(L) --- L-th zero of erf(z), L=1,2,...,NT
!     Routine called: CERF for computing erf(z) and erf'(z)
!     ===============================================================


IMPLICIT DOUBLE PRECISION (e,p,w)
IMPLICIT COMPLEX *16 (c,z)
INTEGER, INTENT(IN)                      :: nt
COMPLEX, INTENT(OUT)                     :: zo(nt)


zo=0.0
w=0.0
pi=3.141592653589793D0
DO  nr=1,nt
  pu=DSQRT(pi*(4.0D0*nr-0.5D0))
  pv=pi*DSQRT(2.0D0*nr-0.25D0)
  px=0.5*pu-0.5*DLOG(pv)/pu
  py=0.5*pu+0.5*DLOG(pv)/pu
  z=CMPLX(px,py)
  it=0
  DO WHILE (1==1)
    15     it=it+1
    CALL cerf(z,zf,zd)
    zp=(1.0D0,0.0D0)
    DO  i=1,nr-1
      zp=zp*(z-zo(i))
    END DO
    zfd=zf/zp
    zq=(0.0D0,0.0D0)
    DO  i=1,nr-1
      zw=(1.0D0,0.0D0)
      DO  j=1,nr-1
        IF (j /= i) THEN
          zw=zw*(z-zo(j))
        END IF
      END DO
      zq=zq+zw
    END DO
    zgd=(zd-zq*zfd)/zp
    z=z-zfd/zgd
    w0=w
    w=CDABS(z)
    IF (it <= 50.AND.DABS((w-w0)/w) > 1.0D-11) EXIT
  END DO
  zo(nr)=z
END DO
RETURN
END SUBROUTINE cerzo


SUBROUTINE cerf(z,cer,cder)

!     ==========================================================
!     Purpose: Compute complex Error function erf(z) & erf'(z)
!     Input:   z   --- Complex argument of erf(z)
!     x   --- Real part of z
!     y   --- Imaginary part of z
!     Output:  CER --- erf(z)
!     CDER --- erf'(z)
!     ==========================================================

IMPLICIT DOUBLE PRECISION (a-h,o-z)
COMPLEX *16, INTENT(IN)                  :: z
COMPLEX *16, INTENT(OUT)                 :: cer
COMPLEX *16, INTENT(OUT)                 :: cder


w=0.0
w1=0.0
w2=0.0
eps=1.0D-12
pi=3.141592653589793D0
x=REAL(z)
y=DIMAG(z)
x2=x*x
IF (x <= 3.5D0) THEN
  er=1.0D0
  r=1.0D0
  DO  k=1,100
    r=r*x2/(k+0.5D0)
    er=er+r
    IF (DABS(er-w) <= eps*DABS(er)) EXIT
    w=er
  END DO
  15    c0=2.0D0/DSQRT(pi)*x*DEXP(-x2)
  er0=c0*er
ELSE
  er=1.0D0
  r=1.0D0
  DO  k=1,12
    r=-r*(k-0.5D0)/x2
    er=er+r
  END DO
  c0=DEXP(-x2)/(x*DSQRT(pi))
  er0=1.0D0-c0*er
END IF
IF (y == 0.0D0) THEN
  ERR=er0
  eri=0.0D0
ELSE
  cs=DCOS(2.0D0*x*y)
  ss=DSIN(2.0D0*x*y)
  er1=DEXP(-x2)*(1.0D0-cs)/(2.0D0*pi*x)
  ei1=DEXP(-x2)*ss/(2.0D0*pi*x)
  er2=0.0D0
  DO  n=1,100
    er2=er2+DEXP(-.25D0*n*n)/(n*n+4.0D0*x2)*(2.0D0*x  &
        -2.0D0*x*DCOSH(n*y)*cs+n*DSINH(n*y)*ss)
    IF (DABS((er2-w1)/er2) < eps) EXIT
    w1=er2
  END DO
  30    c0=2.0D0*DEXP(-x2)/pi
  ERR=er0+er1+c0*er2
  ei2=0.0D0
  DO  n=1,100
    ei2=ei2+DEXP(-.25D0*n*n)/(n*n+4.0D0*x2)*(2.0D0*x  &
        *DCOSH(n*y)*ss+n*DSINH(n*y)*cs)
    IF (DABS((ei2-w2)/ei2) < eps) EXIT
    w2=ei2
  END DO
  40    eri=ei1+c0*ei2
END IF
cer=CMPLX(ERR,eri)
cder=2.0D0/DSQRT(pi)*CDEXP(-z*z)
RETURN
END SUBROUTINE cerf
