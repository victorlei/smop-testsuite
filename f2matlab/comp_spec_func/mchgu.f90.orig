PROGRAM mchgu
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:10

!     =======================================================
!     Purpose: This program computes the confluent
!     hypergeometric function U(a,b,x) using
!     subroutine CHGU
!     Input  : a  --- Parameter
!     b  --- Parameter
!     x  --- Argument  ( x ע 0 )
!     Output:  HU --- U(a,b,x)
!     MD --- Method code
!     Example:
!     a       b       x        U(a,b,x)
!     --------------------------------------
!     -2.5     2.5     5.0     -9.02812446
!     -1.5     2.5     5.0      2.15780560
!     -.5     2.5     5.0      1.76649370
!     .0     2.5     5.0      1.00000000
!     .5     2.5     5.0       .49193496
!     1.5     2.5     5.0       .08944272
!     2.5     2.5     5.0       .01239387

!     a       b       x        U(a,b,x)
!     --------------------------------------
!     -2.5     5.0    10.0     -2.31982196
!     -1.5     5.0    10.0      8.65747115
!     -.5     5.0    10.0      2.37997143
!     .0     5.0    10.0      1.00000000
!     .5     5.0    10.0       .38329536
!     1.5     5.0    10.0       .04582817
!     2.5     5.0    10.0       .00444535
!     =======================================================

IMPLICIT DOUBLE PRECISION (a-h,o-z)
WRITE(*,*)'Please enter a, b and x '
!     READ(*,*)A,B,X
a=-2.5
b=2.5
x=5.0
WRITE(*,*)'   a       b       x        U(a,b,x)'
WRITE(*,*)'--------------------------------------'
CALL chgu(a,b,x,hu,md)
WRITE(*,10)a,b,x,hu
10   FORMAT(1X,f5.1,3X,f5.1,3X,f5.1,e15.8)
END PROGRAM mchgu


SUBROUTINE chgu(a,b,x,hu,md)

!     =======================================================
!     Purpose: Compute the confluent hypergeometric function
!     U(a,b,x)
!     Input  : a  --- Parameter
!     b  --- Parameter
!     x  --- Argument  ( x > 0 )
!     Output:  HU --- U(a,b,x)
!     MD --- Method code
!     Routines called:
!     (1) CHGUS for small x ( MD=1 )
!     (2) CHGUL for large x ( MD=2 )
!     (3) CHGUBI for integer b ( MD=3 )
!     (4) CHGUIT for numerical integration ( MD=4 )
!     =======================================================


DOUBLE PRECISION, INTENT(IN OUT)         :: a
DOUBLE PRECISION, INTENT(IN OUT)         :: b
DOUBLE PRECISION, INTENT(IN)             :: x
DOUBLE PRECISION, INTENT(IN OUT)         :: hu
INTEGER, INTENT(OUT)                     :: md
IMPLICIT DOUBLE PRECISION (a-h,o-z)
LOGICAL :: il1,il2,il3,bl1,bl2,bl3,bn

aa=a-b+1.0D0
il1=a == INT(a).AND.a <= 0.0
il2=aa == INT(aa).AND.aa <= 0.0
il3=ABS(a*(a-b+1.0))/x <= 2.0
bl1=x <= 5.0.OR.(x <= 10.0.AND.a <= 2.0)
bl2=(x > 5.0.AND.x <= 12.5).AND.(a >= 1.0.AND.b >= a+4.0)
bl3=x > 12.5.AND.a >= 5.0.AND.b >= a+5.0
bn=b == INT(b).AND.b /= 0.0
id1=-100
IF (b /= INT(b)) THEN
  CALL chgus(a,b,x,hu,id1)
  md=1
  IF (id1 >= 6) RETURN
  hu1=hu
END IF
IF (il1.OR.il2.OR.il3) THEN
  CALL chgul(a,b,x,hu,id)
  md=2
  IF (id >= 6) RETURN
  IF (id1 > id) THEN
    md=1
    id=id1
    hu=hu1
  END IF
END IF
IF (a >= 0.0) THEN
  IF (bn.AND.(bl1.OR.bl2.OR.bl3)) THEN
    CALL chgubi(a,b,x,hu,id)
    md=3
  ELSE
    CALL chguit(a,b,x,hu,id)
    md=4
  END IF
ELSE
  IF (b <= a) THEN
    a00=a
    b00=b
    a=a-b+1.0D0
    b=2.0D0-b
    CALL chguit(a,b,x,hu,id)
    hu=x**(1.0D0-b00)*hu
    a=a00
    b=b00
    md=4
  ELSE IF (bn.AND.(.NOT.il1)) THEN
    CALL chgubi(a,b,x,hu,id)
    md=3
  END IF
END IF
IF (id < 6) WRITE(*,*)'No accurate result obtained'
RETURN
END SUBROUTINE chgu


SUBROUTINE chgus(a,b,x,hu,id)

!     ======================================================
!     Purpose: Compute confluent hypergeometric function
!     U(a,b,x) for small argument x
!     Input  : a  --- Parameter
!     b  --- Parameter ( b <> 0,-1,-2,...)
!     x  --- Argument
!     Output:  HU --- U(a,b,x)
!     ID --- Estimated number of significant digits
!     Routine called: GAMMA for computing gamma function
!     ======================================================



DOUBLE PRECISION, INTENT(IN)             :: a
DOUBLE PRECISION, INTENT(IN)             :: b
DOUBLE PRECISION, INTENT(IN)             :: x
DOUBLE PRECISION, INTENT(OUT)            :: hu
INTEGER, INTENT(OUT)                     :: id
IMPLICIT DOUBLE PRECISION (a-h,o-z)
h0=0.0
id=-100
pi=3.141592653589793D0
CALL gamma(a,ga)
CALL gamma(b,gb)
xg1=1.0D0+a-b
CALL gamma(xg1,gab)
xg2=2.0D0-b
CALL gamma(xg2,gb2)
hu0=pi/DSIN(pi*b)
r1=hu0/(gab*gb)
r2=hu0*x**(1.0D0-b)/(ga*gb2)
hu=r1-r2
hmax=0.0D0
hmin=1.0D+300
DO  j=1,150
  r1=r1*(a+j-1.0D0)/(j*(b+j-1.0D0))*x
  r2=r2*(a-b+j)/(j*(1.0D0-b+j))*x
  hu=hu+r1-r2
  hua=DABS(hu)
  IF (hua > hmax) hmax=hua
  IF (hua < hmin) hmin=hua
  IF (DABS(hu-h0) < DABS(hu)*1.0D-15) EXIT
  h0=hu
END DO
15   d1=LOG10(hmax)
IF (hmin /= 0.0) d2=LOG10(hmin)
id=15-ABS(d1-d2)
RETURN
END SUBROUTINE chgus


SUBROUTINE chgul(a,b,x,hu,id)

!     =======================================================
!     Purpose: Compute the confluent hypergeometric function
!     U(a,b,x) for large argument x
!     Input  : a  --- Parameter
!     b  --- Parameter
!     x  --- Argument
!     Output:  HU --- U(a,b,x)
!     ID --- Estimated number of significant digits
!     =======================================================


DOUBLE PRECISION, INTENT(IN)             :: a
DOUBLE PRECISION, INTENT(IN)             :: b
DOUBLE PRECISION, INTENT(IN)             :: x
DOUBLE PRECISION, INTENT(OUT)            :: hu
INTEGER, INTENT(OUT)                     :: id
IMPLICIT DOUBLE PRECISION (a-h,o-z)
LOGICAL :: il1,il2

id=-100
aa=a-b+1.0D0
il1=a == INT(a).AND.a <= 0.0
il2=aa == INT(aa).AND.aa <= 0.0
IF (il1) nm=ABS(a)
IF (il2) nm=ABS(aa)
IF (il1.OR.il2) THEN
  hu=1.0D0
  r=1.0D0
  DO  k=1,nm
    r=-r*(a+k-1.0D0)*(a-b+k)/(k*x)
    hu=hu+r
  END DO
  hu=x**(-a)*hu
  id=10
ELSE
  hu=1.0D0
  r=1.0D0
  DO  k=1,25
    r=-r*(a+k-1.0D0)*(a-b+k)/(k*x)
    ra=DABS(r)
    IF (k > 5.AND.ra >= r0.OR.ra < 1.0D-15) EXIT
    r0=ra
    hu=hu+r
  END DO
  20    id=ABS(LOG10(ra))
  hu=x**(-a)*hu
END IF
RETURN
END SUBROUTINE chgul


SUBROUTINE chgubi(a,b,x,hu,id)

!     ======================================================
!     Purpose: Compute confluent hypergeometric function
!     U(a,b,x) with integer b ( b = ס1,ס2,... )
!     Input  : a  --- Parameter
!     b  --- Parameter
!     x  --- Argument
!     Output:  HU --- U(a,b,x)
!     ID --- Estimated number of significant digits
!     Routines called:
!     (1) GAMMA for computing gamma function ג(x)
!     (2) PSI for computing psi function
!     ======================================================



DOUBLE PRECISION, INTENT(IN)             :: a
DOUBLE PRECISION, INTENT(IN)             :: b
DOUBLE PRECISION, INTENT(IN)             :: x
DOUBLE PRECISION, INTENT(OUT)            :: hu
INTEGER, INTENT(OUT)                     :: id
IMPLICIT DOUBLE PRECISION (a-h,o-z)
h0=0
id=-100
el=0.5772156649015329D0
n=ABS(b-1)
rn1=1.0D0
rn=1.0D0
DO  j=1,n
  rn=rn*j
  IF (j == n-1) rn1=rn
END DO
CALL psi(a,ps)
CALL gamma(a,ga)
IF (b > 0.0) THEN
  a0=a
  a1=a-n
  a2=a1
  CALL gamma(a1,ga1)
  ua=(-1)**(n-1)/(rn*ga1)
  ub=rn1/ga*x**(-n)
ELSE
  a0=a+n
  a1=a0
  a2=a
  CALL gamma(a1,ga1)
  ua=(-1)**(n-1)/(rn*ga)*x**n
  ub=rn1/ga1
END IF
hm1=1.0D0
r=1.0D0
hmax=0.0D0
hmin=1.0D+300
DO  k=1,150
  r=r*(a0+k-1.0D0)*x/((n+k)*k)
  hm1=hm1+r
  hu1=DABS(hm1)
  IF (hu1 > hmax) hmax=hu1
  IF (hu1 < hmin) hmin=hu1
  IF (DABS(hm1-h0) < DABS(hm1)*1.0D-15) EXIT
  h0=hm1
END DO
20   da1=LOG10(hmax)
IF (hmin /= 0.0) da2=LOG10(hmin)
id=15-ABS(da1-da2)
hm1=hm1*DLOG(x)
s0=0.0D0
DO  m=1,n
  IF (b >= 0.0) s0=s0-1.0D0/m
  IF (b < 0.0) s0=s0+(1.0D0-a)/(m*(a+m-1.0D0))
END DO
hm2=ps+2.0D0*el+s0
r=1.0D0
hmax=0.0D0
hmin=1.0D+300
DO  k=1,150
  s1=0.0D0
  s2=0.0D0
  IF (b > 0.0) THEN
    DO  m=1,k
      s1=s1-(m+2.0D0*a-2.0D0)/(m*(m+a-1.0D0))
    END DO
    DO  m=1,n
      s2=s2+1.0D0/(k+m)
    END DO
  ELSE
    DO  m=1,k+n
      s1=s1+(1.0D0-a)/(m*(m+a-1.0D0))
    END DO
    DO  m=1,k
      s2=s2+1.0D0/m
    END DO
  END IF
  hw=2.0D0*el+ps+s1-s2
  r=r*(a0+k-1.0D0)*x/((n+k)*k)
  hm2=hm2+r*hw
  hu2=DABS(hm2)
  IF (hu2 > hmax) hmax=hu2
  IF (hu2 < hmin) hmin=hu2
  IF (DABS((hm2-h0)/hm2) < 1.0D-15) EXIT
  h0=hm2
END DO
55   db1=LOG10(hmax)
IF (hmin /= 0.0) db2=LOG10(hmin)
id1=15-ABS(db1-db2)
IF (id1 < id) id=id1
hm3=1.0D0
IF (n == 0) hm3=0.0D0
r=1.0D0
DO  k=1,n-1
  r=r*(a2+k-1.0D0)/((k-n)*k)*x
  hm3=hm3+r
END DO
sa=ua*(hm1+hm2)
sb=ub*hm3
hu=sa+sb
IF (sa /= 0.0) id1=INT(LOG10(ABS(sa)))
IF (hu /= 0.0) id2=INT(LOG10(ABS(hu)))
IF (sa*sb < 0.0) id=id-ABS(id1-id2)
RETURN
END SUBROUTINE chgubi


SUBROUTINE chguit(a,b,x,hu,id)

!     ======================================================
!     Purpose: Compute hypergeometric function U(a,b,x) by
!     using Gaussian-Legendre integration (n=60)
!     Input  : a  --- Parameter ( a > 0 )
!     b  --- Parameter
!     x  --- Argument ( x > 0 )
!     Output:  HU --- U(a,b,z)
!     ID --- Estimated number of significant digits
!     Routine called: GAMMA for computing ג(x)
!     ======================================================


DOUBLE PRECISION, INTENT(IN)             :: a
DOUBLE PRECISION, INTENT(IN)             :: b
DOUBLE PRECISION, INTENT(IN)             :: x
DOUBLE PRECISION, INTENT(OUT)            :: hu
INTEGER, INTENT(OUT)                     :: id
IMPLICIT DOUBLE PRECISION (a-h,o-z)
DIMENSION t(30),w(30)
DATA t/ .259597723012478D-01, .778093339495366D-01,  &
    .129449135396945D+00, .180739964873425D+00,  &
    .231543551376029D+00, .281722937423262D+00,  &
    .331142848268448D+00, .379670056576798D+00,  &
    .427173741583078D+00, .473525841761707D+00,  &
    .518601400058570D+00, .562278900753945D+00,  &
    .604440597048510D+00, .644972828489477D+00,  &
    .683766327381356D+00, .720716513355730D+00,  &
    .755723775306586D+00, .788693739932264D+00,  &
    .819537526162146D+00, .848171984785930D+00,  &
    .874519922646898D+00, .898510310810046D+00,  &
    .920078476177628D+00, .939166276116423D+00,  &
    .955722255839996D+00, .969701788765053D+00,  &
    .981067201752598D+00, .989787895222222D+00,  &
    .995840525118838D+00, .999210123227436D+00/
DATA w/ .519078776312206D-01, .517679431749102D-01,  &
    .514884515009810D-01, .510701560698557D-01,  &
    .505141845325094D-01, .498220356905502D-01,  &
    .489955754557568D-01, .480370318199712D-01,  &
    .469489888489122D-01, .457343797161145D-01,  &
    .443964787957872D-01, .429388928359356D-01,  &
    .413655512355848D-01, .396806954523808D-01,  &
    .378888675692434D-01, .359948980510845D-01,  &
    .340038927249464D-01, .319212190192963D-01,  &
    .297524915007890D-01, .275035567499248D-01,  &
    .251804776215213D-01, .227895169439978D-01,  &
    .203371207294572D-01, .178299010142074D-01,  &
    .152746185967848D-01, .126781664768159D-01,  &
    .100475571822880D-01, .738993116334531D-02,  &
    .471272992695363D-02, .202681196887362D-02/

hu0=0
id=7
a1=a-1.0D0
b1=b-a-1.0D0
c=12.0/x
DO  m=10,100,5
  hu1=0.0D0
  g=0.5D0*c/m
  d=g
  DO  j=1,m
    s=0.0D0
    DO  k=1,30
      t1=d+g*t(k)
      t2=d-g*t(k)
      f1=DEXP(-x*t1)*t1**a1*(1.0D0+t1)**b1
      f2=DEXP(-x*t2)*t2**a1*(1.0D0+t2)**b1
      s=s+w(k)*(f1+f2)
    END DO
    hu1=hu1+s*g
    d=d+2.0D0*g
  END DO
  IF (DABS(1.0D0-hu0/hu1) < 1.0D-7) EXIT
  hu0=hu1
END DO
25   CALL gamma(a,ga)
hu1=hu1/ga
DO  m=2,10,2
  hu2=0.0D0
  g=0.5D0/m
  d=g
  DO  j=1,m
    s=0.0D0
    DO  k=1,30
      t1=d+g*t(k)
      t2=d-g*t(k)
      t3=c/(1.0D0-t1)
      t4=c/(1.0D0-t2)
      f1=t3*t3/c*DEXP(-x*t3)*t3**a1*(1.0D0+t3)**b1
      f2=t4*t4/c*DEXP(-x*t4)*t4**a1*(1.0D0+t4)**b1
      s=s+w(k)*(f1+f2)
    END DO
    hu2=hu2+s*g
    d=d+2.0D0*g
  END DO
  IF (DABS(1.0D0-hu0/hu2) < 1.0D-7) EXIT
  hu0=hu2
END DO
45   CALL gamma(a,ga)
hu2=hu2/ga
hu=hu1+hu2
RETURN
END SUBROUTINE chguit


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
