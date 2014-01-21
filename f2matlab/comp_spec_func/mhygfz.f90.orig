PROGRAM mhygfz
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:11

!     ============================================================
!     Purpose: This program computes hypergeometric function for
!     a complex argument, F(a,b,c,z), using subroutine
!     HYGFZ
!     Input :  a --- Parameter
!     b --- Parameter
!     c --- Parameter,  c <> 0,-1,-2,...
!     x --- Real part of complex argument z
!     y --- Imaginary part of complex argument z
!     ( z = x+iy )
!     Output:  ZHF --- F(a,b,c,z)
!     Examples:
!     a     b     c    z = x+ iy             F(a,b,c,z)
!     --------------------------------------------------------------
!     3.2   1.8   6.7   1.0+0.0 i    .54689992D+01+.00000000D+00 i
!     3.2  -1.8   6.7   1.0+0.0 i    .33750635D+00+.00000000D+00 i
!     -5.0   3.3   6.7   5.2+4.8 i    .11682745D+03+.60389104D+03 i
!     3.3  -6.0   3.7   5.2-4.8 i    .17620425D+05+.38293812D+05 i
!     -7.0   3.3  -3.7   5.2-4.8 i   -.11772779D+11-.14382286D+11 i
!     4.3  -8.0  -3.7   5.2+4.8 i    .13161188D+13-.10129870D+12 i
!     3.3   5.8   6.7   0.2+0.1 i    .17330557D+01+.63401030D+00 i
!     3.5  -2.4   6.7   0.2+0.5 i    .64762241D+00-.52110507D+00 i
!     3.3   4.3   6.7   0.8+0.3 i   -.14830086D+01+.83744258D+01 i
!     7.0   5.0   4.1   3.0-1.0 i   -.40376095D-02-.29566326D-02 i
!     5.0   7.0   4.1   3.0-1.0 i   -.40376095D-02-.29566326D-02 i
!     3.5   1.2   9.7   0.6+0.9 i    .10343044D+01+.54473814D+00 i
!     2.1   5.4   9.7   0.5+0.7 i    .68850442D+00+.12274187D+01 i
!     8.7   3.2   6.7   0.5+0.7 i   -.90046505D+00-.11198900D+01 i
!     8.7   2.7   6.7   0.6+0.9 i   -.46083890D+00-.54575701D+00 i
!     ============================================================

IMPLICIT DOUBLE PRECISION (a-h,o-y)
IMPLICIT COMPLEX *16 (z)
WRITE(*,*)'Please enter a,b,c,x and y '
!     READ(*,*)A,B,C,X,Y
a=8.7
b=2.7
c=6.7
x=.6
y=.9
z=CMPLX(x,y)
CALL hygfz(a,b,c,z,zhf)
WRITE(*,*)
WRITE(*,*)'     a      b      c      x      y',  &
    '          Re[F]           Im[F]'
WRITE(*,*)'   --------------------------------',  &
    '-----------------------------------'
WRITE(*,10)a,b,c,x,y,zhf
10   FORMAT(1X,5F7.1,2X,2D16.8)
END PROGRAM mhygfz


SUBROUTINE hygfz(a,b,c,z,zhf)

!     ======================================================
!     Purpose: Compute the hypergeometric function for a
!     complex argument, F(a,b,c,z)
!     Input :  a --- Parameter
!     b --- Parameter
!     c --- Parameter,  c <> 0,-1,-2,...
!     z --- Complex argument
!     Output:  ZHF --- F(a,b,c,z)
!     Routines called:
!     (1) GAMMA for computing gamma function
!     (2) PSI for computing psi function
!     ======================================================


DOUBLE PRECISION, INTENT(IN OUT)         :: a
DOUBLE PRECISION, INTENT(IN OUT)         :: b
DOUBLE PRECISION, INTENT(IN OUT)         :: c
COMPLEX, INTENT(IN)                      :: z
COMPLEX, INTENT(OUT)                     :: zhf
IMPLICIT DOUBLE PRECISION (a-h,o-y)
IMPLICIT COMPLEX *16 (z)
LOGICAL :: l0,l1,l2,l3,l4,l5,l6

zw=0.0
w0=0.0
x=REAL(z)
y=DIMAG(z)
eps=1.0D-15
l0=c == INT(c).AND.c < 0.0D0
l1=DABS(1.0D0-x) < eps.AND.y == 0.0D0.AND.c-a-b <= 0.0D0
l2=CDABS(z+1.0D0) < eps.AND.DABS(c-a+b-1.0D0) < eps
l3=a == INT(a).AND.a < 0.0D0
l4=b == INT(b).AND.b < 0.0D0
l5=c-a == INT(c-a).AND.c-a <= 0.0D0
l6=c-b == INT(c-b).AND.c-b <= 0.0D0
aa=a
bb=b
a0=CDABS(z)
IF (a0 > 0.95D0) eps=1.0D-8
pi=3.141592653589793D0
el=.5772156649015329D0
IF (l0.OR.l1) THEN
  WRITE(*,*)'The hypergeometric series is divergent'
  RETURN
END IF
IF (a0 == 0.0D0.OR.a == 0.0D0.OR.b == 0.0D0) THEN
  zhf=(1.0D0,0.0D0)
ELSE IF (z == 1.0D0.AND.c-a-b > 0.0D0) THEN
  CALL gamma(c,gc)
  CALL gamma(c-a-b,gcab)
  CALL gamma(c-a,gca)
  CALL gamma(c-b,gcb)
  zhf=gc*gcab/(gca*gcb)
ELSE IF (l2) THEN
  g0=DSQRT(pi)*2.0D0**(-a)
  CALL gamma(c,g1)
  CALL gamma(1.0D0+a/2.0D0-b,g2)
  CALL gamma(0.5D0+0.5D0*a,g3)
  zhf=g0*g1/(g2*g3)
ELSE IF (l3.OR.l4) THEN
  IF (l3) nm=INT(ABS(a))
  IF (l4) nm=INT(ABS(b))
  zhf=(1.0D0,0.0D0)
  zr=(1.0D0,0.0D0)
  DO  k=1,nm
    zr=zr*(a+k-1.0D0)*(b+k-1.0D0)/(k*(c+k-1.0D0))*z
    zhf=zhf+zr
  END DO
ELSE IF (l5.OR.l6) THEN
  IF (l5) nm=INT(ABS(c-a))
  IF (l6) nm=INT(ABS(c-b))
  zhf=(1.0D0,0.0D0)
  zr=(1.0D0,0.0D0)
  DO  k=1,nm
    zr=zr*(c-a+k-1.0D0)*(c-b+k-1.0D0)/(k*(c+k-1.0D0))*z
    zhf=zhf+zr
  END DO
  zhf=(1.0D0-z)**(c-a-b)*zhf
ELSE IF (a0 <= 1.0D0) THEN
  IF (x < 0.0D0) THEN
    z1=z/(z-1.0D0)
    IF (c > a.AND.b < a.AND.b > 0.0) THEN
      a=bb
      b=aa
    END IF
    zc0=1.0D0/((1.0D0-z)**a)
    zhf=(1.0D0,0.0D0)
    zr0=(1.0D0,0.0D0)
    DO  k=1,500
      zr0=zr0*(a+k-1.0D0)*(c-b+k-1.0D0)/(k*(c+k-1.0D0))*z1
      zhf=zhf+zr0
      IF (CDABS(zhf-zw) < CDABS(zhf)*eps) EXIT
      zw=zhf
    END DO
    25     zhf=zc0*zhf
  ELSE IF (a0 >= 0.90D0) THEN
    gm=0.0D0
    mcab=INT(c-a-b+eps*DSIGN(1.0D0,c-a-b))
    IF (DABS(c-a-b-mcab) < eps) THEN
      m=INT(c-a-b)
      CALL gamma(a,ga)
      CALL gamma(b,gb)
      CALL gamma(c,gc)
      CALL gamma(a+m,gam)
      CALL gamma(b+m,gbm)
      CALL psi(a,pa)
      CALL psi(b,pb)
      IF (m /= 0) gm=1.0D0
      DO  j=1,ABS(m)-1
        gm=gm*j
      END DO
      rm=1.0D0
      DO  j=1,ABS(m)
        rm=rm*j
      END DO
      zf0=(1.0D0,0.0D0)
      zr0=(1.0D0,0.0D0)
      zr1=(1.0D0,0.0D0)
      sp0=0.d0
      sp=0.0D0
      IF (m >= 0) THEN
        zc0=gm*gc/(gam*gbm)
        zc1=-gc*(z-1.0D0)**m/(ga*gb*rm)
        DO  k=1,m-1
          zr0=zr0*(a+k-1.d0)*(b+k-1.d0)/(k*(k-m))*(1.d0-z)
          zf0=zf0+zr0
        END DO
        DO  k=1,m
          sp0=sp0+1.0D0/(a+k-1.0D0)+1.0/(b+k-1.0D0)-1.d0/k
        END DO
        zf1=pa+pb+sp0+2.0D0*el+CDLOG(1.0D0-z)
        DO  k=1,500
          sp=sp+(1.0D0-a)/(k*(a+k-1.0D0))+(1.0D0-b)/ (k*(b+k-1.0D0))
          sm=0.0D0
          DO  j=1,m
            sm=sm+(1.0D0-a)/((j+k)*(a+j+k-1.0D0)) +1.0D0/(b+j+k-1.0D0)
          END DO
          zp=pa+pb+2.0D0*el+sp+sm+CDLOG(1.0D0-z)
          zr1=zr1*(a+m+k-1.0D0)*(b+m+k-1.0D0)/(k*(m+k)) *(1.0D0-z)
          zf1=zf1+zr1*zp
          IF (CDABS(zf1-zw) < CDABS(zf1)*eps) EXIT
          zw=zf1
        END DO
        60       zhf=zf0*zc0+zf1*zc1
      ELSE IF (m < 0) THEN
        m=-m
        zc0=gm*gc/(ga*gb*(1.0D0-z)**m)
        zc1=-(-1)**m*gc/(gam*gbm*rm)
        DO  k=1,m-1
          zr0=zr0*(a-m+k-1.0D0)*(b-m+k-1.0D0)/(k*(k-m)) *(1.0D0-z)
          zf0=zf0+zr0
        END DO
        DO  k=1,m
          sp0=sp0+1.0D0/k
        END DO
        zf1=pa+pb-sp0+2.0D0*el+CDLOG(1.0D0-z)
        DO  k=1,500
          sp=sp+(1.0D0-a)/(k*(a+k-1.0D0))+(1.0D0-b)/(k* (b+k-1.0D0))
          sm=0.0D0
          DO  j=1,m
            sm=sm+1.0D0/(j+k)
          END DO
          zp=pa+pb+2.0D0*el+sp-sm+CDLOG(1.0D0-z)
          zr1=zr1*(a+k-1.d0)*(b+k-1.d0)/(k*(m+k))*(1.d0-z)
          zf1=zf1+zr1*zp
          IF (CDABS(zf1-zw) < CDABS(zf1)*eps) EXIT
          zw=zf1
        END DO
        85       zhf=zf0*zc0+zf1*zc1
      END IF
    ELSE
      CALL gamma(a,ga)
      CALL gamma(b,gb)
      CALL gamma(c,gc)
      CALL gamma(c-a,gca)
      CALL gamma(c-b,gcb)
      CALL gamma(c-a-b,gcab)
      CALL gamma(a+b-c,gabc)
      zc0=gc*gcab/(gca*gcb)
      zc1=gc*gabc/(ga*gb)*(1.0D0-z)**(c-a-b)
      zhf=(0.0D0,0.0D0)
      zr0=zc0
      zr1=zc1
      DO  k=1,500
        zr0=zr0*(a+k-1.d0)*(b+k-1.d0)/(k*(a+b-c+k))*(1.d0-z)
        zr1=zr1*(c-a+k-1.0D0)*(c-b+k-1.0D0)/(k*(c-a-b+k)) *(1.0D0-z)
        zhf=zhf+zr0+zr1
        IF (CDABS(zhf-zw) < CDABS(zhf)*eps) EXIT
        zw=zhf
      END DO
      95      zhf=zhf+zc0+zc1
    END IF
  ELSE
    z00=(1.0D0,0.0D0)
    IF (c-a < a.AND.c-b < b) THEN
      z00=(1.0D0-z)**(c-a-b)
      a=c-a
      b=c-b
    END IF
    zhf=(1.0D0,0.d0)
    zr=(1.0D0,0.0D0)
    DO  k=1,1500
      zr=zr*(a+k-1.0D0)*(b+k-1.0D0)/(k*(c+k-1.0D0))*z
      zhf=zhf+zr
      IF (CDABS(zhf-zw) <= CDABS(zhf)*eps) EXIT
      zw=zhf
    END DO
    105    zhf=z00*zhf
  END IF
ELSE IF (a0 > 1.0D0) THEN
  mab=INT(a-b+eps*DSIGN(1.0D0,a-b))
  IF (DABS(a-b-mab) < eps.AND.a0 <= 1.1D0) b=b+eps
  IF (DABS(a-b-mab) > eps) THEN
    CALL gamma(a,ga)
    CALL gamma(b,gb)
    CALL gamma(c,gc)
    CALL gamma(a-b,gab)
    CALL gamma(b-a,gba)
    CALL gamma(c-a,gca)
    CALL gamma(c-b,gcb)
    zc0=gc*gba/(gca*gb*(-z)**a)
    zc1=gc*gab/(gcb*ga*(-z)**b)
    zr0=zc0
    zr1=zc1
    zhf=(0.0D0,0.0D0)
    DO  k=1,500
      zr0=zr0*(a+k-1.0D0)*(a-c+k)/((a-b+k)*k*z)
      zr1=zr1*(b+k-1.0D0)*(b-c+k)/((b-a+k)*k*z)
      zhf=zhf+zr0+zr1
      IF (CDABS((zhf-zw)/zhf) <= eps) EXIT
      zw=zhf
    END DO
    115    zhf=zhf+zc0+zc1
  ELSE
    IF (a-b < 0.0D0) THEN
      a=bb
      b=aa
    END IF
    ca=c-a
    cb=c-b
    nca=INT(ca+eps*DSIGN(1.0D0,ca))
    ncb=INT(cb+eps*DSIGN(1.0D0,cb))
    IF (DABS(ca-nca) < eps.OR.DABS(cb-ncb) < eps) c=c+eps
    CALL gamma(a,ga)
    CALL gamma(c,gc)
    CALL gamma(c-b,gcb)
    CALL psi(a,pa)
    CALL psi(c-a,pca)
    CALL psi(a-c,pac)
    mab=INT(a-b+eps)
    zc0=gc/(ga*(-z)**b)
    CALL gamma(a-b,gm)
    zf0=gm/gcb*zc0
    zr=zc0
    DO  k=1,mab-1
      zr=zr*(b+k-1.0D0)/(k*z)
      t0=a-b-k
      CALL gamma(t0,g0)
      CALL gamma(c-b-k,gcbk)
      zf0=zf0+zr*g0/gcbk
    END DO
    IF (mab == 0) zf0=(0.0D0,0.0D0)
    zc1=gc/(ga*gcb*(-z)**a)
    sp=-2.0D0*el-pa-pca
    DO  j=1,mab
      sp=sp+1.0D0/j
    END DO
    zp0=sp+CDLOG(-z)
    sq=1.0D0
    DO  j=1,mab
      sq=sq*(b+j-1.0D0)*(b-c+j)/j
    END DO
    zf1=(sq*zp0)*zc1
    zr=zc1
    rk1=1.0D0
    sj1=0.0D0
    DO  k=1,10000
      zr=zr/z
      rk1=rk1*(b+k-1.0D0)*(b-c+k)/(k*k)
      rk2=rk1
      DO  j=k+1,k+mab
        rk2=rk2*(b+j-1.0D0)*(b-c+j)/j
      END DO
      sj1=sj1+(a-1.0D0)/(k*(a+k-1.0D0))+(a-c-1.0D0)/ (k*(a-c+k-1.0D0))
      sj2=sj1
      DO  j=k+1,k+mab
        sj2=sj2+1.0D0/j
      END DO
      zp=-2.0D0*el-pa-pac+sj2-1.0D0/(k+a-c) -pi/DTAN(pi*(k+a-c))+CDLOG(-z)
      zf1=zf1+rk2*zr*zp
      ws=CDABS(zf1)
      IF (DABS((ws-w0)/ws) < eps) EXIT
      w0=ws
    END DO
    150    zhf=zf0+zf1
  END IF
END IF
155  a=aa
b=bb
IF (k > 150) WRITE(*,160)
160  FORMAT(1X,'Warning! You should check the accuracy')
RETURN
END SUBROUTINE hygfz


SUBROUTINE gamma(x,ga)

!     ==================================================
!     Purpose: Compute gamma function ג(x)
!     Input :  x  --- Argument of ג(x)
!     ( x is not equal to 0,-1,-2,תתת)
!     Output:  GA --- ג(x)
!     ==================================================


DOUBLE PRECISION, INTENT(IN)             :: x
DOUBLE PRECISION, INTENT(OUT)            :: ga
IMPLICIT DOUBLE PRECISION (a-h,o-z)
DIMENSION g(26)

pi=3.141592653589793D0
IF (x == INT(x)) THEN
  IF (x > 0.0D0) THEN
    ga=1.0D0
    m1=x-1
    DO  k=2,m1
      ga=ga*k
    END DO
  ELSE
    ga=1.0D+300
  END IF
ELSE
  IF (DABS(x) > 1.0D0) THEN
    z=DABS(x)
    m=INT(z)
    r=1.0D0
    DO  k=1,m
      r=r*(z-k)
    END DO
    z=z-m
  ELSE
    z=x
  END IF
  DATA g/1.0D0,0.5772156649015329D0,  &
      -0.6558780715202538D0, -0.420026350340952D-1,  &
      0.1665386113822915D0,-.421977345555443D-1,  &
      -.96219715278770D-2, .72189432466630D-2,  &
      -.11651675918591D-2, -.2152416741149D-3,  &
      .1280502823882D-3, -.201348547807D-4, -.12504934821D-5, .11330272320D-5,  &
      -.2056338417D-6, .61160950D-8, .50020075D-8, -.11812746D-8,  &
      .1043427D-9, .77823D-11, -.36968D-11, .51D-12,  &
      -.206D-13, -.54D-14, .14D-14, .1D-15/
  gr=g(26)
  DO  k=25,1,-1
    gr=gr*z+g(k)
  END DO
  ga=1.0D0/(gr*z)
  IF (DABS(x) > 1.0D0) THEN
    ga=ga*r
    IF (x < 0.0D0) ga=-pi/(x*ga*DSIN(pi*x))
  END IF
END IF
RETURN
END SUBROUTINE gamma


SUBROUTINE psi(x,ps)

!     ======================================
!     Purpose: Compute Psi function
!     Input :  x  --- Argument of psi(x)
!     Output:  PS --- psi(x)
!     ======================================



DOUBLE PRECISION, INTENT(IN)             :: x
DOUBLE PRECISION, INTENT(OUT)            :: ps
IMPLICIT DOUBLE PRECISION (a-h,o-z)
xa=DABS(x)
pi=3.141592653589793D0
el=.5772156649015329D0
s=0.0D0
IF (x == INT(x).AND.x <= 0.0) THEN
  ps=1.0D+300
  RETURN
ELSE IF (xa == INT(xa)) THEN
  n=xa
  DO  k=1 ,n-1
    s=s+1.0D0/k
  END DO
  ps=-el+s
ELSE IF (xa+.5 == INT(xa+.5)) THEN
  n=xa-.5
  DO  k=1,n
    s=s+1.0/(2.0D0*k-1.0D0)
  END DO
  ps=-el+2.0D0*s-1.386294361119891D0
ELSE
  IF (xa < 10.0) THEN
    n=10-INT(xa)
    DO  k=0,n-1
      s=s+1.0D0/(xa+k)
    END DO
    xa=xa+n
  END IF
  x2=1.0D0/(xa*xa)
  a1=-.8333333333333D-01
  a2=.83333333333333333D-02
  a3=-.39682539682539683D-02
  a4=.41666666666666667D-02
  a5=-.75757575757575758D-02
  a6=.21092796092796093D-01
  a7=-.83333333333333333D-01
  a8=.4432598039215686D0
  ps=DLOG(xa)-.5D0/xa+x2*(((((((a8*x2+a7)*x2+  &
      a6)*x2+a5)*x2+a4)*x2+a3)*x2+a2)*x2+a1)
  ps=ps-s
END IF
IF (x < 0.0) ps=ps-pi*DCOS(pi*x)/DSIN(pi*x)-1.0D0/x
RETURN
END SUBROUTINE psi
