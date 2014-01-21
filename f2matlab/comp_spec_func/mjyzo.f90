PROGRAM mjyzo
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:11

!       ==========================================================
!       Purpose: This program computes the zeros of Bessel
!                functions Jn(x), Yn(x), and their derivatives
!                using subroutine JYZO
!       Input :  n --- Order of Bessel functions ( n ó 100 )
!                NT --- Number of zeros
!       Output:  RJ0(m) --- m-th zero of Jn(x),  m=1,2,...,NT
!                RJ1(m) --- m-th zero of Jn'(x), m=1,2,...,NT
!                RY0(m) --- m-th zero of Yn(x),  m=1,2,...,NT
!                RY1(m) --- m-th zero of Yn'(x), m=1,2,...,NT
!       Example: n = 1, NT =5

!      Zeros of Bessel funcions Jn(x), Yn(x) and their derivatives
!                                 ( n = 1 )
!       m       jnm           j'nm          ynm           y'nm
!      -----------------------------------------------------------
!       1     3.8317060     1.8411838     2.1971413     3.6830229
!       2     7.0155867     5.3314428     5.4296810     6.9415000
!       3    10.1734681     8.5363164     8.5960059    10.1234047
!       4    13.3236919    11.7060049    11.7491548    13.2857582
!       5    16.4706301    14.8635886    14.8974421    16.4400580
!       ==========================================================

IMPLICIT DOUBLE PRECISION (a-h,o-z)
DIMENSION rj0(101),rj1(101),ry0(101),ry1(101)
WRITE(*,*)'Please enter n and NT '
!        READ(*,*)N,NT
n=1
nt=5
WRITE(*,*)
CALL jyzo(n,nt,rj0,rj1,ry0,ry1)
WRITE(*,30)
WRITE(*,40)n
WRITE(*,*)'  m       jnm           j''NM          YNM', '           y''NM'
WRITE(*,*)' ----------------------------------------', '-------------------'
DO  m=1,nt
  WRITE(*,50)m,rj0(m),rj1(m),ry0(m),ry1(m)
END DO
30      FORMAT(2X,'Zeros of Bessel funcions Jn(x), Yn(x)',  &
    ' and their derivatives')
40      FORMAT(30X,'( n =',i2,' )')
50      FORMAT(1X,i3,4F14.7)
END PROGRAM mjyzo


SUBROUTINE jyzo(n,nt,rj0,rj1,ry0,ry1)

!       ======================================================
!       Purpose: Compute the zeros of Bessel functions Jn(x),
!                Yn(x), and their derivatives
!       Input :  n  --- Order of Bessel functions ( n ó 101 )
!                NT --- Number of zeros (roots)
!       Output:  RJ0(L) --- L-th zero of Jn(x),  L=1,2,...,NT
!                RJ1(L) --- L-th zero of Jn'(x), L=1,2,...,NT
!                RY0(L) --- L-th zero of Yn(x),  L=1,2,...,NT
!                RY1(L) --- L-th zero of Yn'(x), L=1,2,...,NT
!       Routine called: JYNDD for computing Jn(x), Yn(x), and
!                       their first and second derivatives
!       ======================================================


INTEGER, INTENT(IN)                      :: n
INTEGER, INTENT(IN OUT)                  :: nt
DOUBLE PRECISION, INTENT(OUT)            :: rj0(nt)
DOUBLE PRECISION, INTENT(OUT)            :: rj1(nt)
DOUBLE PRECISION, INTENT(OUT)            :: ry0(nt)
DOUBLE PRECISION, INTENT(OUT)            :: ry1(nt)
IMPLICIT DOUBLE PRECISION (a-h,o-z)


IF (n <= 20) THEN
  x=2.82141+1.15859*n
ELSE
  x=n+1.85576*n**0.33333+1.03315/n**0.33333
END IF
l=0
DO
  DO
    10       x0=x
    CALL jyndd(n,x,bjn,djn,fjn,byn,dyn,fyn)
    x=x-bjn/djn
    IF (.NOT.(DABS(x-x0) > 1.0D-9)) EXIT
  END DO
  l=l+1
  rj0(l)=x
  x=x+3.1416+(0.0972+0.0679*n-0.000354*n**2)/l
  IF (.NOT.(l < nt)) EXIT
END DO
IF (n <= 20) THEN
  x=0.961587+1.07703*n
ELSE
  x=n+0.80861*n**0.33333+0.07249/n**0.33333
END IF
IF (n == 0) x=3.8317
l=0
DO
  DO
    15       x0=x
    CALL jyndd(n,x,bjn,djn,fjn,byn,dyn,fyn)
    x=x-djn/fjn
    IF (.NOT.(DABS(x-x0) > 1.0D-9)) EXIT
  END DO
  l=l+1
  rj1(l)=x
  x=x+3.1416+(0.4955+0.0915*n-0.000435*n**2)/l
  IF (.NOT.(l < nt)) EXIT
END DO
IF (n <= 20) THEN
  x=1.19477+1.08933*n
ELSE
  x=n+0.93158*n**0.33333+0.26035/n**0.33333
END IF
l=0
DO
  DO
    20       x0=x
    CALL jyndd(n,x,bjn,djn,fjn,byn,dyn,fyn)
    x=x-byn/dyn
    IF (.NOT.(DABS(x-x0) > 1.0D-9)) EXIT
  END DO
  l=l+1
  ry0(l)=x
  x=x+3.1416+(0.312+0.0852*n-0.000403*n**2)/l
  IF (.NOT.(l < nt)) EXIT
END DO
IF (n <= 20) THEN
  x=2.67257+1.16099*n
ELSE
  x=n+1.8211*n**0.33333+0.94001/n**0.33333
END IF
l=0
DO
  DO
    25       x0=x
    CALL jyndd(n,x,bjn,djn,fjn,byn,dyn,fyn)
    x=x-dyn/fyn
    IF (.NOT.(DABS(x-x0) > 1.0D-9)) EXIT
  END DO
  l=l+1
  ry1(l)=x
  x=x+3.1416+(0.197+0.0643*n-0.000286*n**2)/l
  IF (.NOT.(l < nt)) EXIT
END DO
RETURN
END SUBROUTINE jyzo


SUBROUTINE jyndd(n,x,bjn,djn,fjn,byn,dyn,fyn)

!       ===========================================================
!       Purpose: Compute Bessel functions Jn(x) and Yn(x), and
!                their first and second derivatives
!       Input:   x   ---  Argument of Jn(x) and Yn(x) ( x > 0 )
!                n   ---  Order of Jn(x) and Yn(x)
!       Output:  BJN ---  Jn(x)
!                DJN ---  Jn'(x)
!                FJN ---  Jn"(x)
!                BYN ---  Yn(x)
!                DYN ---  Yn'(x)
!                FYN ---  Yn"(x)
!       ===========================================================


INTEGER, INTENT(IN)                      :: n
DOUBLE PRECISION, INTENT(IN)             :: x
DOUBLE PRECISION, INTENT(OUT)            :: bjn
DOUBLE PRECISION, INTENT(OUT)            :: djn
DOUBLE PRECISION, INTENT(OUT)            :: fjn
DOUBLE PRECISION, INTENT(OUT)            :: byn
DOUBLE PRECISION, INTENT(OUT)            :: dyn
DOUBLE PRECISION, INTENT(OUT)            :: fyn
IMPLICIT DOUBLE PRECISION (a-h,o-z)
DIMENSION bj(102),by(102)

DO  nt=1,900
  mt=INT(0.5*LOG10(6.28*nt)-nt*LOG10(1.36*DABS(x)/nt))
  IF (mt > 20) EXIT
END DO
15      m=nt
bs=0.0D0
f0=0.0D0
f1=1.0D-35
su=0.0D0
DO  k=m,0,-1
  f=2.0D0*(k+1.0D0)*f1/x-f0
  IF (k <= n+1) bj(k+1)=f
  IF (k == 2*INT(k/2)) THEN
    bs=bs+2.0D0*f
    IF (k /= 0) su=su+(-1)**(k/2)*f/k
  END IF
  f0=f1
  f1=f
END DO
DO  k=0,n+1
  bj(k+1)=bj(k+1)/(bs-f)
END DO
bjn=bj(n+1)
ec=0.5772156649015329D0
e0=0.3183098861837907D0
s1=2.0D0*e0*(DLOG(x/2.0D0)+ec)*bj(1)
f0=s1-8.0D0*e0*su/(bs-f)
f1=(bj(2)*f0-2.0D0*e0/x)/bj(1)
by(1)=f0
by(2)=f1
DO  k=2,n+1
  f=2.0D0*(k-1.0D0)*f1/x-f0
  by(k+1)=f
  f0=f1
  f1=f
END DO
byn=by(n+1)
djn=-bj(n+2)+n*bj(n+1)/x
dyn=-by(n+2)+n*by(n+1)/x
fjn=(n*n/(x*x)-1.0D0)*bjn-djn/x
fyn=(n*n/(x*x)-1.0D0)*byn-dyn/x
RETURN
END SUBROUTINE jyndd



