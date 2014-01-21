PROGRAM mairyzo
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:09

!       =========================================================
!       Purpose: This program computes the first NT zeros of Airy
!                functions Ai(x) and Ai'(x), and the associated
!                values of Ai(a') and Ai'(a), and the first NT
!                zeros of Airy functions Bi(x) and Bi'(x), and
!                the associated values of Bi(b') and Bi'(b) using
!                subroutine AIRYZO
!       Input :  NT    --- Total number of zeros
!                KF    --- Function code
!                          KF=1 for Ai(x) and Ai'(x)
!                          KF=2 for Bi(x) and Bi'(x)
!       Output:  XA(m) --- a, the m-th zero of Ai(x) or
!                          b, the m-th zero of Bi(x)
!                XB(m) --- a', the m-th zero of Ai'(x) or
!                          b', the m-th zero of Bi'(x)
!                XC(m) --- Ai(a') or Bi(b')
!                XD(m) --- Ai'(a) or Bi'(b)
!                          ( m --- Serial number of zeros )
!       Example: NT=5

!       m         a            Ai'(a)         a'          Ai(a')
!      -----------------------------------------------------------
!       1    -2.33810741     .70121082   -1.01879297    .53565666
!       2    -4.08794944    -.80311137   -3.24819758   -.41901548
!       3    -5.52055983     .86520403   -4.82009921    .38040647
!       4    -6.78670809    -.91085074   -6.16330736   -.35790794
!       5    -7.94413359     .94733571   -7.37217726    .34230124

!       m         b            Bi'(b)         b'          Bi(b')
!      -----------------------------------------------------------
!       1    -1.17371322     .60195789   -2.29443968   -.45494438
!       2    -3.27109330    -.76031014   -4.07315509    .39652284
!       3    -4.83073784     .83699101   -5.51239573   -.36796916
!       4    -6.16985213    -.88947990   -6.78129445    .34949912
!       5    -7.37676208     .92998364   -7.94017869   -.33602624
!       ==========================================================

IMPLICIT DOUBLE PRECISION (a-h,o-z)
DIMENSION xa(50),xb(50),xc(50),xd(50)
WRITE(*,35)
WRITE(*,40)
WRITE(*,*)'Please enter KF,NT '
!        READ(*,*)KF,NT
kf=1
nt=5
WRITE(*,30)kf,nt
IF (kf == 1) THEN
  WRITE(*,*)'  m        a             Ai''(A)        A''',  &
      '           Ai(a'')'
ELSE IF (kf == 2) THEN
  WRITE(*,*)'  m        b             Bi''(B)        B''',  &
      '           Bi(b'')'
END IF
WRITE(*,*)'---------------------------------',   &
    '---------------------------'
CALL airyzo(nt,kf,xa,xb,xc,xd)
DO  k=1,nt
  WRITE(*,20)k,xa(k),xd(k),xb(k),xc(k)
END DO
20      FORMAT(1X,i3,1X,3F14.8,f13.8)
30      FORMAT(1X,3HKF=,i2,',     ',3HNT=,i3)
35      FORMAT(10X,'KF=1 for Ai(x) and Ai''(X); KF=2 FOR BI(X)',  &
    ' and Bi''(X)')
40      FORMAT(10X,'NT is the number of the zeros')
END PROGRAM mairyzo


SUBROUTINE airyzo(nt,kf,xa,xb,xc,xd)

!       ========================================================
!       Purpose: Compute the first NT zeros of Airy functions
!                Ai(x) and Ai'(x), a and a', and the associated
!                values of Ai(a') and Ai'(a); and the first NT
!                zeros of Airy functions Bi(x) and Bi'(x), b and
!                b', and the associated values of Bi(b') and
!                Bi'(b)
!       Input :  NT    --- Total number of zeros
!                KF    --- Function code
!                          KF=1 for Ai(x) and Ai'(x)
!                          KF=2 for Bi(x) and Bi'(x)
!       Output:  XA(m) --- a, the m-th zero of Ai(x) or
!                          b, the m-th zero of Bi(x)
!                XB(m) --- a', the m-th zero of Ai'(x) or
!                          b', the m-th zero of Bi'(x)
!                XC(m) --- Ai(a') or Bi(b')
!                XD(m) --- Ai'(a) or Bi'(b)
!                          ( m --- Serial number of zeros )
!       Routine called: AIRYB for computing Airy functions and
!                       their derivatives
!       =======================================================


IMPLICIT DOUBLE PRECISION (a-h,o-z)
INTEGER, INTENT(IN)                      :: nt
INTEGER, INTENT(IN)                      :: kf
DOUBLE PRECISION, INTENT(OUT)            :: xa(nt)
DOUBLE PRECISION, INTENT(OUT)            :: xb(nt)
DOUBLE PRECISION, INTENT(OUT)            :: xc(nt)
DOUBLE PRECISION, INTENT(OUT)            :: xd(nt)


pi=3.141592653589793D0
DO  i=1,nt
  IF (kf == 1) THEN
    u=3.0*pi*(4.0*i-1)/8.0D0
    u1=1/(u*u)
    rt0=-(u*u)**(1.0/3.0)*((((-15.5902*u1+.929844)*u1  &
        -.138889)*u1+.10416667D0)*u1+1.0D0)
  ELSE IF (kf == 2) THEN
    IF (i == 1) THEN
      rt0=-1.17371
    ELSE
      u=3.0*pi*(4.0*i-3.0)/8.0
      u1=1.0D0/(u*u)
      rt0=-(u*u)**(1.0/3.0)*((((-15.5902*u1+.929844)*u1  &
          -.138889)*u1+.10416667)*u1+1.0)
    END IF
  END IF
  rt=1.0E300
  DO WHILE (DABS((rt-rt0)/rt) > 1.d-9)
    x=rt0
    CALL airyb(x,ai,bi,ad,bd)
    IF (kf == 1) rt=rt0-ai/ad
    IF (kf == 2) rt=rt0-bi/bd
    IF (DABS((rt-rt0)/rt) > 1.d-9) THEN
      rt0=rt
    END IF
  END DO
  xa(i)=rt
  IF (kf == 1) xd(i)=ad
  IF (kf == 2) xd(i)=bd
END DO
DO  i=1,nt
  IF (kf == 1) THEN
    IF (i == 1) THEN
      rt0=-1.01879
    ELSE
      u=3.0*pi*(4.0*i-3.0)/8.0
      u1=1/(u*u)
      rt0=-(u*u)**(1.0/3.0)*((((15.0168*u1-.873954)  &
          *u1+.121528)*u1-.145833D0)*u1+1.0D0)
    END IF
  ELSE IF (kf == 2) THEN
    IF (i == 1) THEN
      rt0=-2.29444
    ELSE
      u=3.0*pi*(4.0*i-1.0)/8.0
      u1=1.0/(u*u)
      rt0=-(u*u)**(1.0/3.0)*((((15.0168*u1-.873954)  &
          *u1+.121528)*u1-.145833)*u1+1.0)
    END IF
  END IF
  rt=1.0E300
  DO WHILE (DABS((rt-rt0)/rt) > 1.0D-9)
    x=rt0
    CALL airyb(x,ai,bi,ad,bd)
    IF (kf == 1) rt=rt0-ad/(ai*x)
    IF (kf == 2) rt=rt0-bd/(bi*x)
    IF (DABS((rt-rt0)/rt) > 1.0D-9) THEN
      rt0=rt
    END IF
  END DO
  xb(i)=rt
  IF (kf == 1) xc(i)=ai
  IF (kf == 2) xc(i)=bi
END DO
RETURN
END SUBROUTINE airyzo


SUBROUTINE airyb(x,ai,bi,ad,bd)

!       =======================================================
!       Purpose: Compute Airy functions and their derivatives
!       Input:   x  --- Argument of Airy function
!       Output:  AI --- Ai(x)
!                BI --- Bi(x)
!                AD --- Ai'(x)
!                BD --- Bi'(x)
!       =======================================================


IMPLICIT DOUBLE PRECISION (a-h,o-z)
DOUBLE PRECISION, INTENT(IN)             :: x
DOUBLE PRECISION, INTENT(OUT)            :: ai
DOUBLE PRECISION, INTENT(OUT)            :: bi
DOUBLE PRECISION, INTENT(OUT)            :: ad
DOUBLE PRECISION, INTENT(OUT)            :: bd
DIMENSION ck(41),dk(41)

eps=1.0D-15
pi=3.141592653589793D0
c1=0.355028053887817D0
c2=0.258819403792807D0
sr3=1.732050807568877D0
xa=DABS(x)
xq=DSQRT(xa)
IF (x > 0.0D0) xm=5.0
IF (x <= 0.0D0) xm=8.0
IF (x == 0.0D0) THEN
  ai=c1
  bi=sr3*c1
  ad=-c2
  bd=sr3*c2
  RETURN
END IF
IF (xa <= xm) THEN
  fx=1.0D0
  r=1.0D0
  DO  k=1,40
    r=r*x/(3.0D0*k)*x/(3.0D0*k-1.0D0)*x
    fx=fx+r
    IF (DABS(r/fx) < eps) EXIT
  END DO
  15         gx=x
  r=x
  DO  k=1,40
    r=r*x/(3.0D0*k)*x/(3.0D0*k+1.0D0)*x
    gx=gx+r
    IF (DABS(r/gx) < eps) EXIT
  END DO
  25         ai=c1*fx-c2*gx
  bi=sr3*(c1*fx+c2*gx)
  df=.5D0*x*x
  r=df
  DO  k=1,40
    r=r*x/(3.0D0*k)*x/(3.0D0*k+2.0D0)*x
    df=df+r
    IF (DABS(r/df) < eps) EXIT
  END DO
  35         dg=1.0D0
  r=1.0D0
  DO  k=1,40
    r=r*x/(3.0D0*k)*x/(3.0D0*k-2.0D0)*x
    dg=dg+r
    IF (DABS(r/dg) < eps) EXIT
  END DO
  45         ad=c1*df-c2*dg
  bd=sr3*(c1*df+c2*dg)
ELSE
  xe=xa*xq/1.5D0
  xr1=1.0D0/xe
  xar=1.0D0/xq
  xf=DSQRT(xar)
  rp=.5641895835477563D0
  r=1.0D0
  DO  k=1,40
    r=r*(6.0D0*k-1.0D0)/216.0D0*(6.0D0*k-3.0D0)  &
        /k*(6.0D0*k-5.0D0)/(2.0D0*k-1.0D0)
    ck(k)=r
    dk(k)=-(6.0D0*k+1.0D0)/(6.0D0*k-1.0D0)*ck(k)
  END DO
  km=INT(24.5-xa)
  IF (xa < 6.0) km=14
  IF (xa > 15.0) km=10
  IF (x > 0.0D0) THEN
    sai=1.0D0
    sad=1.0D0
    r=1.0D0
    DO  k=1,km
      r=-r*xr1
      sai=sai+ck(k)*r
      sad=sad+dk(k)*r
    END DO
    sbi=1.0D0
    sbd=1.0D0
    r=1.0D0
    DO  k=1,km
      r=r*xr1
      sbi=sbi+ck(k)*r
      sbd=sbd+dk(k)*r
    END DO
    xp1=DEXP(-xe)
    ai=.5D0*rp*xf*xp1*sai
    bi=rp*xf/xp1*sbi
    ad=-.5D0*rp/xf*xp1*sad
    bd=rp/xf/xp1*sbd
  ELSE
    xcs=DCOS(xe+pi/4.0D0)
    xss=DSIN(xe+pi/4.0D0)
    ssa=1.0D0
    sda=1.0D0
    r=1.0D0
    xr2=1.0D0/(xe*xe)
    DO  k=1,km
      r=-r*xr2
      ssa=ssa+ck(2*k)*r
      sda=sda+dk(2*k)*r
    END DO
    ssb=ck(1)*xr1
    sdb=dk(1)*xr1
    r=xr1
    DO  k=1,km
      r=-r*xr2
      ssb=ssb+ck(2*k+1)*r
      sdb=sdb+dk(2*k+1)*r
    END DO
    ai=rp*xf*(xss*ssa-xcs*ssb)
    bi=rp*xf*(xcs*ssa+xss*ssb)
    ad=-rp/xf*(xcs*sda+xss*sdb)
    bd=rp/xf*(xss*sda-xcs*sdb)
  END IF
END IF
RETURN
END SUBROUTINE airyb
