PROGRAM mjdzo
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:11

!     =============================================================
!     Purpose: This program computes the zeros of Bessel functions
!     Jn(x) and Jn'(x), and arranges them in the order
!     of their values
!     Input :  NT    --- Number of total zeros ( NT ף 1200 )
!     Output:  ZO(L) --- Value of the L-th zero of Jn(x) and
!     Jn'(x)
!     N(L)  --- n, order of Jn(x) or Jn'(x) associated
!     with the L-th zero
!     M(L)  --- m, serial number of the zeros of Jn(x)
!     or Jn'(x) associated with the L-th zero
!     ( L is the serial number of all the
!     zeros of Jn(x) and Jn'(x) )
!     P(L)  --- 1 (TE) or 2 (TM), a code for designating the
!     zeros of Jn(x) or Jn'(x)
!     In the waveguide applications, the zeros
!     of Jn(x) correspond to TM modes and those
!     of Jn'(x) correspond to TE modes.
!     =============================================================

IMPLICIT DOUBLE PRECISION (a-h,o-z)
DIMENSION n(1400),m(1400),zo(0:1400)
WRITE(*,*)'NT=?'
!     READ(*,*)NT
nt=5
WRITE(*,60)nt
WRITE(*,70)
CALL jdzo(nt,n,m,p,zo)
WRITE(*,*)
ks=nt/101+1
DO  k0=1,ks
  WRITE(*,*)' Table           Zeros of Bessel', ' functions Jn(x) and Jn''(X)'
  WRITE(*,*)
  WRITE(*,*)' ----------------------------------',  &
      '----------------------------------'
  DO  k=1,50
    j1=100*(k0-1)+k+1
    j2=j1+50
    IF (j1 <= nt+1.AND.j2 <= nt+1) THEN
      WRITE(*,65)j1-1,p(j1),n(j1),m(j1),zo(j1), j2-1,p(j2),n(j2),m(j2),zo(j2)
    ELSE IF (j1 <= nt+1.AND.j2 > nt+1) THEN
      WRITE(*,65)j1-1,p(j1),n(j1),m(j1),zo(j1)
    END IF
  END DO
  WRITE(*,*)' ----------------------------------',  &
      '----------------------------------'
  WRITE(*,75)
END DO
60   FORMAT(1X,'Total number of the zeros:',i5)
65   FORMAT(1X,i4,3X,a2,i4,2H -,i2,f14.8,3X,1H|,2X,i4, 3X,a2,i4,2H -,i2,f14.8)
70   FORMAT(15X,'***  Please wait.  The program is running  ***')
75   FORMAT(/)
END PROGRAM mjdzo


SUBROUTINE jdzo(nt,n,m,p,zo)

!     ===========================================================
!     Purpose: Compute the zeros of Bessel functions Jn(x) and
!     Jn'(x), and arrange them in the order of their
!     magnitudes
!     Input :  NT    --- Number of total zeros ( NT ף 1200 )
!     Output:  ZO(L) --- Value of the L-th zero of Jn(x)
!     and Jn'(x)
!     N(L)  --- n, order of Jn(x) or Jn'(x) associated
!     with the L-th zero
!     M(L)  --- m, serial number of the zeros of Jn(x)
!     or Jn'(x) associated with the L-th zero
!     ( L is the serial number of all the
!     zeros of Jn(x) and Jn'(x) )
!     P(L)  --- TM or TE, a code for designating the
!     zeros of Jn(x)  or Jn'(x).
!     In the waveguide applications, the zeros
!     of Jn(x) correspond to TM modes and
!     those of Jn'(x) correspond to TE modes
!     Routine called:    BJNDD for computing Jn(x), Jn'(x) and
!     Jn''(x)
!     =============================================================


INTEGER, INTENT(IN)                      :: nt
INTEGER, INTENT(OUT)                     :: n(1400)
INTEGER, INTENT(OUT)                     :: m(1400)
DOUBLE PRECISION, INTENT(OUT)            :: p
DOUBLE PRECISION, INTENT(OUT)            :: zo(0:1400)
IMPLICIT DOUBLE PRECISION (a-h,o-z)
DIMENSION  n1(70),m1(70), zoc(0:70),bj(101),dj(101),fj(101)

x=0.0
IF (nt < 600) THEN
  xm=-1.0+2.248485*nt**0.5-.0159382*nt+3.208775E-4 *nt**1.5
  nm=INT(14.5+.05875*nt)
  mm=INT(.02*nt)+6
ELSE
  xm=5.0+1.445389*nt**.5+.01889876*nt-2.147763E-4 *nt**1.5
  nm=INT(27.8+.0327*nt)
  mm=INT(.01088*nt)+10
END IF
l0=0
DO  i=1,nm
  x1=.407658+.4795504*(i-1)**.5+.983618*(i-1)
  x2=1.99535+.8333883*(i-1)**.5+.984584*(i-1)
  l1=0
  DO  j=1,mm
    ifoo=1
    IF (.NOT.(i == 1.AND.j == 1)) THEN
      x=x1
      DO
        10       CALL bjndd(i,x,bj,dj,fj)
        x0=x
        x=x-dj(i)/fj(i)
        IF (x1 > xm) THEN
          ifoo=0
          EXIT
        END IF
        IF (.NOT.(DABS(x-x0) > 1.0D-10)) EXIT
      END DO
    END IF
    IF (ifoo == 1) THEN
      15      l1=l1+1
      n1(l1)=i-1
      m1(l1)=j
      IF (i == 1) m1(l1)=j-1
      p1(l1)=1
      zoc(l1)=x
      IF (i <= 15) THEN
        x1=x+3.057+.0122*(i-1)+(1.555+.41575*(i-1))/(j+1)**2
      ELSE
        x1=x+2.918+.01924*(i-1)+(6.26+.13205*(i-1))/(j+1)**2
      END IF
    END IF
    20     x=x2
    ifoo=1
    DO
      25      CALL bjndd(i,x,bj,dj,fj)
      x0=x
      x=x-bj(i)/dj(i)
      IF (x > xm) THEN
        ifoo=0
        EXIT
      END IF
      IF (.NOT.(DABS(x-x0) > 1.0D-10)) EXIT
    END DO
    IF (ifoo == 1) THEN
      l1=l1+1
      n1(l1)=i-1
      m1(l1)=j
      p1(l1)=2
      zoc(l1)=x
      IF (i <= 15) THEN
        x2=x+3.11+.0138*(i-1)+(.04832+.2804*(i-1))/(j+1)**2
      ELSE
        x2=x+3.001+.0105*(i-1)+(11.52+.48525*(i-1))/(j+3)**2
      END IF
    END IF
  END DO
  l=l0+l1
  l2=l
  DO
    35     IF (l0 == 0) THEN
      DO  k=1,l
        zo(k)=zoc(k)
        n(k)=n1(k)
        m(k)=m1(k)
        p(k)=p1(k)
      END DO
      l1=0
    ELSE IF (l0 /= 0) THEN
      IF (zo(l0) >= zoc(l1)) THEN
        zo(l0+l1)=zo(l0)
        n(l0+l1)=n(l0)
        m(l0+l1)=m(l0)
        p(l0+l1)=p(l0)
        l0=l0-1
      ELSE
        zo(l0+l1)=zoc(l1)
        n(l0+l1)=n1(l1)
        m(l0+l1)=m1(l1)
        p(l0+l1)=p1(l1)
        l1=l1-1
      END IF
    END IF
    IF (.NOT.(l1 /= 0)) EXIT
  END DO
  l0=l2
END DO
RETURN
END SUBROUTINE jdzo


SUBROUTINE bjndd(n,x,bj,dj,fj)

!     =====================================================
!     Purpose: Compute Bessel functions Jn(x) and their
!     first and second derivatives ( n= 0,1,תתת )
!     Input:   x ---  Argument of Jn(x)  ( x ע 0 )
!     n ---  Order of Jn(x)
!     Output:  BJ(n+1) ---  Jn(x)
!     DJ(n+1) ---  Jn'(x)
!     FJ(n+1) ---  Jn"(x)
!     =====================================================

IMPLICIT DOUBLE PRECISION (a-h,o-z)
INTEGER, INTENT(IN)                      :: n
DOUBLE PRECISION, INTENT(IN)             :: x
DOUBLE PRECISION, INTENT(OUT)            :: bj(101)
DOUBLE PRECISION, INTENT(OUT)            :: dj(101)
DOUBLE PRECISION, INTENT(OUT)            :: fj(101)


DO  nt=1,900
  mt=INT(0.5*LOG10(6.28*nt)-nt*LOG10(1.36*DABS(x)/nt))
  IF (mt > 20) EXIT
END DO
15   m=nt
bs=0.0D0
f0=0.0D0
f1=1.0D-35
DO  k=m,0,-1
  f=2.0D0*(k+1.0D0)*f1/x-f0
  IF (k <= n) bj(k+1)=f
  IF (k == 2*INT(k/2)) bs=bs+2.0D0*f
  f0=f1
  f1=f
END DO
DO  k=0,n
  bj(k+1)=bj(k+1)/(bs-f)
END DO
dj(1)=-bj(2)
fj(1)=-1.0D0*bj(1)-dj(1)/x
DO  k=1,n
  dj(k+1)=bj(k)-k*bj(k+1)/x
  fj(k+1)=(k*k/(x*x)-1.0D0)*bj(k+1)-dj(k+1)/x
END DO
RETURN
END SUBROUTINE bjndd
