PROGRAM mairyb
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:09

!       ============================================================
!       Purpose: This program computes Airy functions and their
!                derivatives using subroutine AIRYB
!       Input:   x  --- Argument of Airy function
!       Output:  AI --- Ai(x)
!                BI --- Bi(x)
!                AD --- Ai'(x)
!                BD --- Bi'(x)
!       Example:

!   x       Ai(x)          Bi(x)          Ai'(x)         Bi'(x)
!  ----------------------------------------------------------------
!   0   .35502805D+00  .61492663D+00 -.25881940D+00  .44828836D+00
!  10   .11047533D-09  .45564115D+09 -.35206337D-09  .14292361D+10
!  20   .16916729D-26  .21037650D+26 -.75863916D-26  .93818393D+26
!  30   .32082176D-48  .90572885D+47 -.17598766D-47  .49533045D+48

!   x       Ai(-x)         Bi(-x)         Ai'(-x)        Bi'(-x)
!  ----------------------------------------------------------------
!   0       .35502805      .61492663     -.25881940      .44828836
!  10       .04024124     -.31467983      .99626504      .11941411
!  20      -.17640613     -.20013931      .89286286     -.79142903
!  30      -.08796819     -.22444694     1.22862060     -.48369473
!       ============================================================

IMPLICIT DOUBLE PRECISION (a-h,o-z)
WRITE(*,*)'Please enter x '
!        READ(*,*)X
x=20
CALL airyb(x,ai,bi,ad,bd)
WRITE(*,30)
WRITE(*,40)
WRITE(*,10)x,ai,bi,ad,bd
WRITE(*,*)
CALL airyb(-x,ai,bi,ad,bd)
WRITE(*,50)
WRITE(*,40)
WRITE(*,20)x,ai,bi,ad,bd
10      FORMAT(1X,f5.1,4D16.8)
20      FORMAT(1X,f5.1,4D16.8)
30      FORMAT(4X,'x',8X,'Ai(x)',11X,'Bi(x)',11X,'Ai''(X)', 10X,'Bi''(X)')
40      FORMAT(2X,'----------------------------------',  &
    '-----------------------------------')
50      FORMAT(4X,'x',8X,'Ai(-x)',10X,'Bi(-x)',10X, 'Ai''(-X)',9X,'BI''(-X)')
END PROGRAM mairyb


SUBROUTINE airyb(x,ai,bi,ad,bd)

!       =======================================================
!       Purpose: Compute Airy functions and their derivatives
!       Input:   x  --- Argument of Airy function
!       Output:  AI --- Ai(x)
!                BI --- Bi(x)
!                AD --- Ai'(x)
!                BD --- Bi'(x)
!       =======================================================


DOUBLE PRECISION, INTENT(IN)             :: x
DOUBLE PRECISION, INTENT(OUT)            :: ai
DOUBLE PRECISION, INTENT(OUT)            :: bi
DOUBLE PRECISION, INTENT(OUT)            :: ad
DOUBLE PRECISION, INTENT(OUT)            :: bd
IMPLICIT DOUBLE PRECISION (a-h,o-z)
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
    IF (DABS(r) < DABS(fx)*eps) EXIT
  END DO
  15         gx=x
  r=x
  DO  k=1,40
    r=r*x/(3.0D0*k)*x/(3.0D0*k+1.0D0)*x
    gx=gx+r
    IF (DABS(r) < DABS(gx)*eps) EXIT
  END DO
  25         ai=c1*fx-c2*gx
  bi=sr3*(c1*fx+c2*gx)
  df=0.5D0*x*x
  r=df
  DO  k=1,40
    r=r*x/(3.0D0*k)*x/(3.0D0*k+2.0D0)*x
    df=df+r
    IF (DABS(r) < DABS(df)*eps) EXIT
  END DO
  35         dg=1.0D0
  r=1.0D0
  DO  k=1,40
    r=r*x/(3.0D0*k)*x/(3.0D0*k-2.0D0)*x
    dg=dg+r
    IF (DABS(r) < DABS(dg)*eps) EXIT
  END DO
  45         ad=c1*df-c2*dg
  bd=sr3*(c1*df+c2*dg)
ELSE
  xe=xa*xq/1.5D0
  xr1=1.0D0/xe
  xar=1.0D0/xq
  xf=DSQRT(xar)
  rp=0.5641895835477563D0
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
    ai=0.5D0*rp*xf*xp1*sai
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
