PROGRAM mhygfx
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:11

!     ============================================================
!     Purpose: This program computes the hypergeometric function
!     F(a,b,c,x) using subroutine HYGFX
!     Input :  a --- Parameter
!     b --- Parameter
!     c --- Parameter, c <> 0,-1,-2,...
!     x --- Argument ( x ף 1 )
!     Output:  HF --- F(a,b,c,x)
!     Example:
!     b = 3.30,  c = 6.70
!     a     F(a,b,c,.25)     F(a,b,c,.55)    F(a,b,c,.85)
!     ------------------------------------------------------
!     -2.5   .72356129D+00    .46961432D+00   .29106096D+00
!     -0.5   .93610145D+00    .85187390D+00   .75543187D+00
!     0.5   .10689695D+01    .11795358D+01   .13510497D+01
!     2.5   .14051563D+01    .23999063D+01   .57381566D+01

!     a = 3.30,  b = 6.70
!     c     F(a,b,c,.25)     F(a,b,c,.55)    F(a,b,c,.85)
!     ------------------------------------------------------
!     -5.5   .15090670D+05    .10170778D+11   .58682088D+19
!     -0.5  -.21631479D+04   -.30854772D+07  -.10217370D+13
!     0.5   .26451677D+03    .11967860D+06   .92370648D+10
!     4.5   .41946916D+01    .58092729D+02   .20396914D+05
!     ============================================================

IMPLICIT DOUBLE PRECISION (a-h,o-z)
WRITE(*,*)'Please enter a,b,c and x '
!     READ(*,*)A,B,C,X
a=-2.5
b=3.3
c=6.7
x=.25
CALL hygfx(a,b,c,x,hf)
WRITE(*,10)a,b,c,x
WRITE(*,20)hf
10   FORMAT(1X,'a =',f5.2,',    ','b =',f5.2,',   ','c =',f5.2,  &
    ',    ','x =',f5.2)
20   FORMAT(1X,'F(a,b,c,x)=',d16.8)
END PROGRAM mhygfx


SUBROUTINE hygfx(a,b,c,x,hf)

!     ====================================================
!     Purpose: Compute hypergeometric function F(a,b,c,x)
!     Input :  a --- Parameter
!     b --- Parameter
!     c --- Parameter, c <> 0,-1,-2,...
!     x --- Argument   ( x < 1 )
!     Output:  HF --- F(a,b,c,x)
!     Routines called:
!     (1) GAMMA for computing gamma function
!     (2) PSI for computing psi function
!     ====================================================


DOUBLE PRECISION, INTENT(IN OUT)         :: a
DOUBLE PRECISION, INTENT(IN OUT)         :: b
DOUBLE PRECISION, INTENT(IN)             :: c
DOUBLE PRECISION, INTENT(IN OUT)         :: x
DOUBLE PRECISION, INTENT(OUT)            :: hf
IMPLICIT DOUBLE PRECISION (a-h,o-z)
LOGICAL :: l0,l1,l2,l3,l4,l5

hw=0.0
pi=3.141592653589793D0
el=.5772156649015329D0
l0=c == INT(c).AND.c < 0.0
l1=1.0D0-x < 1.0D-15.AND.c-a-b <= 0.0
l2=a == INT(a).AND.a < 0.0
l3=b == INT(b).AND.b < 0.0
l4=c-a == INT(c-a).AND.c-a <= 0.0
l5=c-b == INT(c-b).AND.c-b <= 0.0
IF (l0.OR.l1) THEN
  WRITE(*,*)'The hypergeometric series is divergent'
  RETURN
END IF
eps=1.0D-15
IF (x > 0.95) eps=1.0D-8
IF (x == 0.0.OR.a == 0.0.OR.b == 0.0) THEN
  hf=1.0D0
  RETURN
ELSE IF (1.0D0-x == eps.AND.c-a-b > 0.0) THEN
  CALL gamma(c,gc)
  CALL gamma(c-a-b,gcab)
  CALL gamma(c-a,gca)
  CALL gamma(c-b,gcb)
  hf=gc*gcab/(gca*gcb)
  RETURN
ELSE IF (1.0D0+x <= eps.AND.DABS(c-a+b-1.0) <= eps) THEN
  g0=DSQRT(pi)*2.0D0**(-a)
  CALL gamma(c,g1)
  CALL gamma(1.0D0+a/2.0-b,g2)
  CALL gamma(0.5D0+0.5*a,g3)
  hf=g0*g1/(g2*g3)
  RETURN
ELSE IF (l2.OR.l3) THEN
  IF (l2) nm=INT(ABS(a))
  IF (l3) nm=INT(ABS(b))
  hf=1.0D0
  r=1.0D0
  DO  k=1,nm
    r=r*(a+k-1.0D0)*(b+k-1.0D0)/(k*(c+k-1.0D0))*x
    hf=hf+r
  END DO
  RETURN
ELSE IF (l4.OR.l5) THEN
  IF (l4) nm=INT(ABS(c-a))
  IF (l5) nm=INT(ABS(c-b))
  hf=1.0D0
  r=1.0D0
  DO  k=1,nm
    r=r*(c-a+k-1.0D0)*(c-b+k-1.0D0)/(k*(c+k-1.0D0))*x
    hf=hf+r
  END DO
  hf=(1.0D0-x)**(c-a-b)*hf
  RETURN
END IF
aa=a
bb=b
x1=x
IF (x < 0.0D0) THEN
  x=x/(x-1.0D0)
  IF (c > a.AND.b < a.AND.b > 0.0) THEN
    a=bb
    b=aa
  END IF
  b=c-b
END IF
IF (x >= 0.75D0) THEN
  gm=0.0D0
  IF (DABS(c-a-b-INT(c-a-b)) < 1.0D-15) THEN
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
    f0=1.0D0
    r0=1.0D0
    r1=1.0D0
    sp0=0.d0
    sp=0.0D0
    IF (m >= 0) THEN
      c0=gm*gc/(gam*gbm)
      c1=-gc*(x-1.0D0)**m/(ga*gb*rm)
      DO  k=1,m-1
        r0=r0*(a+k-1.0D0)*(b+k-1.0)/(k*(k-m))*(1.0-x)
        f0=f0+r0
      END DO
      DO  k=1,m
        sp0=sp0+1.0D0/(a+k-1.0)+1.0/(b+k-1.0)-1.0/k
      END DO
      f1=pa+pb+sp0+2.0D0*el+DLOG(1.0D0-x)
      DO  k=1,250
        sp=sp+(1.0D0-a)/(k*(a+k-1.0))+(1.0-b)/(k*(b+k-1.0))
        sm=0.0D0
        DO  j=1,m
          sm=sm+(1.0D0-a)/((j+k)*(a+j+k-1.0))+1.0/ (b+j+k-1.0)
        END DO
        rp=pa+pb+2.0D0*el+sp+sm+DLOG(1.0D0-x)
        r1=r1*(a+m+k-1.0D0)*(b+m+k-1.0)/(k*(m+k))*(1.0-x)
        f1=f1+r1*rp
        IF (DABS(f1-hw) < DABS(f1)*eps) EXIT
        hw=f1
      END DO
      60      hf=f0*c0+f1*c1
    ELSE IF (m < 0) THEN
      m=-m
      c0=gm*gc/(ga*gb*(1.0D0-x)**m)
      c1=-(-1)**m*gc/(gam*gbm*rm)
      DO  k=1,m-1
        r0=r0*(a-m+k-1.0D0)*(b-m+k-1.0)/(k*(k-m))*(1.0-x)
        f0=f0+r0
      END DO
      DO  k=1,m
        sp0=sp0+1.0D0/k
      END DO
      f1=pa+pb-sp0+2.0D0*el+DLOG(1.0D0-x)
      DO  k=1,250
        sp=sp+(1.0D0-a)/(k*(a+k-1.0))+(1.0-b)/(k*(b+k-1.0))
        sm=0.0D0
        DO  j=1,m
          sm=sm+1.0D0/(j+k)
        END DO
        rp=pa+pb+2.0D0*el+sp-sm+DLOG(1.0D0-x)
        r1=r1*(a+k-1.0D0)*(b+k-1.0)/(k*(m+k))*(1.0-x)
        f1=f1+r1*rp
        IF (DABS(f1-hw) < DABS(f1)*eps) EXIT
        hw=f1
      END DO
      85      hf=f0*c0+f1*c1
    END IF
  ELSE
    CALL gamma(a,ga)
    CALL gamma(b,gb)
    CALL gamma(c,gc)
    CALL gamma(c-a,gca)
    CALL gamma(c-b,gcb)
    CALL gamma(c-a-b,gcab)
    CALL gamma(a+b-c,gabc)
    c0=gc*gcab/(gca*gcb)
    c1=gc*gabc/(ga*gb)*(1.0D0-x)**(c-a-b)
    hf=0.0D0
    r0=c0
    r1=c1
    DO  k=1,250
      r0=r0*(a+k-1.0D0)*(b+k-1.0)/(k*(a+b-c+k))*(1.0-x)
      r1=r1*(c-a+k-1.0D0)*(c-b+k-1.0)/(k*(c-a-b+k)) *(1.0-x)
      hf=hf+r0+r1
      IF (DABS(hf-hw) < DABS(hf)*eps) EXIT
      hw=hf
    END DO
    95     hf=hf+c0+c1
  END IF
ELSE
  a0=1.0D0
  IF (c > a.AND.c < 2.0D0*a.AND. c > b.AND.c < 2.0D0*b) THEN
    a0=(1.0D0-x)**(c-a-b)
    a=c-a
    b=c-b
  END IF
  hf=1.0D0
  r=1.0D0
  DO  k=1,250
    r=r*(a+k-1.0D0)*(b+k-1.0D0)/(k*(c+k-1.0D0))*x
    hf=hf+r
    IF (DABS(hf-hw) <= DABS(hf)*eps) EXIT
    hw=hf
  END DO
  105   hf=a0*hf
END IF
IF (x1 < 0.0D0) THEN
  x=x1
  c0=1.0D0/(1.0D0-x)**aa
  hf=c0*hf
END IF
a=aa
b=bb
IF (k > 120) WRITE(*,115)
115  FORMAT(1X,'Warning! You should check the accuracy')
RETURN
END SUBROUTINE hygfx


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
