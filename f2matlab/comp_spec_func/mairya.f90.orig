PROGRAM mairya
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:09

!       ============================================================
!       Purpose: This program computes Airy functions and their
!                derivatives using subroutine AIRYA
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
x=10
CALL airya(x,ai,bi,ad,bd)
WRITE(*,30)
WRITE(*,40)
WRITE(*,10)x,ai,bi,ad,bd
WRITE(*,*)
CALL airya(-x,ai,bi,ad,bd)
WRITE(*,50)
WRITE(*,40)
WRITE(*,20)x,ai,bi,ad,bd
10      FORMAT(1X,f5.1,4D16.8)
20      FORMAT(1X,f5.1,4D16.8)
30      FORMAT(4X,'x',8X,'Ai(x)',11X,'Bi(x)',11X,'Ai''(X)', 10X,'Bi''(X)')
40      FORMAT(2X,'----------------------------------',  &
    '-----------------------------------')
50      FORMAT(4X,'x',8X,'Ai(-x)',10X,'Bi(-x)',10X, 'Ai''(-X)',9X,'BI''(-X)')
END PROGRAM mairya


SUBROUTINE airya(x,ai,bi,ad,bd)

!       ======================================================
!       Purpose: Compute Airy functions and their derivatives
!       Input:   x  --- Argument of Airy function
!       Output:  AI --- Ai(x)
!                BI --- Bi(x)
!                AD --- Ai'(x)
!                BD --- Bi'(x)
!       Routine called:
!                AJYIK for computing Jv(x), Yv(x), Iv(x) and
!                Kv(x) with v=1/3 and 2/3
!       ======================================================


IMPLICIT DOUBLE PRECISION (a-h,o-z)
DOUBLE PRECISION, INTENT(IN)             :: x
DOUBLE PRECISION, INTENT(OUT)            :: ai
DOUBLE PRECISION, INTENT(OUT)            :: bi
DOUBLE PRECISION, INTENT(OUT)            :: ad
DOUBLE PRECISION, INTENT(OUT)            :: bd
xa=DABS(x)
pir=0.318309886183891D0
c1=0.355028053887817D0
c2=0.258819403792807D0
sr3=1.732050807568877D0
z=xa**1.5/1.5D0
xq=DSQRT(xa)
CALL ajyik(z,vj1,vj2,vy1,vy2,vi1,vi2,vk1,vk2)
IF (x == 0.0D0) THEN
  ai=c1
  bi=sr3*c1
  ad=-c2
  bd=sr3*c2
ELSE IF (x > 0.0D0) THEN
  ai=pir*xq/sr3*vk1
  bi=xq*(pir*vk1+2.0D0/sr3*vi1)
  ad=-xa/sr3*pir*vk2
  bd=xa*(pir*vk2+2.0D0/sr3*vi2)
ELSE
  ai=0.5D0*xq*(vj1-vy1/sr3)
  bi=-0.5D0*xq*(vj1/sr3+vy1)
  ad=0.5D0*xa*(vj2+vy2/sr3)
  bd=0.5D0*xa*(vj2/sr3-vy2)
END IF
RETURN
END SUBROUTINE airya


SUBROUTINE ajyik(x,vj1,vj2,vy1,vy2,vi1,vi2,vk1,vk2)

!       =======================================================
!       Purpose: Compute Bessel functions Jv(x) and Yv(x),
!                and modified Bessel functions Iv(x) and
!                Kv(x), and their derivatives with v=1/3,2/3
!       Input :  x --- Argument of Jv(x),Yv(x),Iv(x) and
!                      Kv(x) ( x ò 0 )
!       Output:  VJ1 --- J1/3(x)
!                VJ2 --- J2/3(x)
!                VY1 --- Y1/3(x)
!                VY2 --- Y2/3(x)
!                VI1 --- I1/3(x)
!                VI2 --- I2/3(x)
!                VK1 --- K1/3(x)
!                VK2 --- K2/3(x)
!       =======================================================


IMPLICIT DOUBLE PRECISION (a-h,o-z)
DOUBLE PRECISION, INTENT(IN)             :: x
DOUBLE PRECISION, INTENT(OUT)            :: vj1
DOUBLE PRECISION, INTENT(OUT)            :: vj2
DOUBLE PRECISION, INTENT(OUT)            :: vy1
DOUBLE PRECISION, INTENT(OUT)            :: vy2
DOUBLE PRECISION, INTENT(OUT)            :: vi1
DOUBLE PRECISION, INTENT(OUT)            :: vi2
DOUBLE PRECISION, INTENT(OUT)            :: vk1
DOUBLE PRECISION, INTENT(OUT)            :: vk2
IF (x == 0.0D0) THEN
  vj1=0.0D0
  vj2=0.0D0
  vy1=-1.0D+300
  vy2=1.0D+300
  vi1=0.0D0
  vi2=0.0D0
  vk1=-1.0D+300
  vk2=-1.0D+300
  RETURN
END IF
pi=3.141592653589793D0
rp2=.63661977236758D0
gp1=.892979511569249D0
gp2=.902745292950934D0
gn1=1.3541179394264D0
gn2=2.678938534707747D0
vv0=0.444444444444444D0
uu0=1.1547005383793D0
x2=x*x
k0=12
IF (x >= 35.0) k0=10
IF (x >= 50.0) k0=8
IF (x <= 12.0) THEN
  DO  l=1,2
    vl=l/3.0D0
    vjl=1.0D0
    r=1.0D0
    DO  k=1,40
      r=-0.25D0*r*x2/(k*(k+vl))
      vjl=vjl+r
      IF (DABS(r) < 1.0D-15) EXIT
    END DO
    20            a0=(0.5D0*x)**vl
    IF (l == 1) vj1=a0/gp1*vjl
    IF (l == 2) vj2=a0/gp2*vjl
  END DO
ELSE
  DO  l=1,2
    vv=vv0*l*l
    px=1.0D0
    rp=1.0D0
    DO  k=1,k0
      rp=-0.78125D-2*rp*(vv-(4.0*k-3.0)**2.0)*(vv-  &
          (4.0*k-1.0)**2.0)/(k*(2.0*k-1.0)*x2)
      px=px+rp
    END DO
    qx=1.0D0
    rq=1.0D0
    DO  k=1,k0
      rq=-0.78125D-2*rq*(vv-(4.0*k-1.0)**2.0)*(vv-  &
          (4.0*k+1.0)**2.0)/(k*(2.0*k+1.0)*x2)
      qx=qx+rq
    END DO
    qx=0.125D0*(vv-1.0)*qx/x
    xk=x-(0.5D0*l/3.0D0+0.25D0)*pi
    a0=DSQRT(rp2/x)
    ck=DCOS(xk)
    sk=DSIN(xk)
    IF (l == 1) THEN
      vj1=a0*(px*ck-qx*sk)
      vy1=a0*(px*sk+qx*ck)
    ELSE IF (l == 2) THEN
      vj2=a0*(px*ck-qx*sk)
      vy2=a0*(px*sk+qx*ck)
    END IF
  END DO
END IF
IF (x <= 12.0D0) THEN
  DO  l=1,2
    vl=l/3.0D0
    vjl=1.0D0
    r=1.0D0
    DO  k=1,40
      r=-0.25D0*r*x2/(k*(k-vl))
      vjl=vjl+r
      IF (DABS(r) < 1.0D-15) EXIT
    END DO
    50            b0=(2.0D0/x)**vl
    IF (l == 1) uj1=b0*vjl/gn1
    IF (l == 2) uj2=b0*vjl/gn2
  END DO
  pv1=pi/3.0D0
  pv2=pi/1.5D0
  vy1=uu0*(vj1*DCOS(pv1)-uj1)
  vy2=uu0*(vj2*DCOS(pv2)-uj2)
END IF
IF (x <= 18.0) THEN
  DO  l=1,2
    vl=l/3.0D0
    vil=1.0D0
    r=1.0D0
    DO  k=1,40
      r=0.25D0*r*x2/(k*(k+vl))
      vil=vil+r
      IF (DABS(r) < 1.0D-15) EXIT
    END DO
    65            a0=(0.5D0*x)**vl
    IF (l == 1) vi1=a0/gp1*vil
    IF (l == 2) vi2=a0/gp2*vil
  END DO
ELSE
  c0=DEXP(x)/DSQRT(2.0D0*pi*x)
  DO  l=1,2
    vv=vv0*l*l
    vsl=1.0D0
    r=1.0D0
    DO  k=1,k0
      r=-0.125D0*r*(vv-(2.0D0*k-1.0D0)**2.0)/(k*x)
      vsl=vsl+r
    END DO
    IF (l == 1) vi1=c0*vsl
    IF (l == 2) vi2=c0*vsl
  END DO
END IF
IF (x <= 9.0D0) THEN
  DO  l=1,2
    vl=l/3.0D0
    IF (l == 1) gn=gn1
    IF (l == 2) gn=gn2
    a0=(2.0D0/x)**vl/gn
    sum=1.0D0
    r=1.0D0
    DO  k=1,60
      r=0.25D0*r*x2/(k*(k-vl))
      sum=sum+r
      IF (DABS(r) < 1.0D-15) EXIT
    END DO
    90            IF (l == 1) vk1=0.5D0*uu0*pi*(sum*a0-vi1)
    IF (l == 2) vk2=0.5D0*uu0*pi*(sum*a0-vi2)
  END DO
ELSE
  c0=DEXP(-x)*DSQRT(0.5D0*pi/x)
  DO  l=1,2
    vv=vv0*l*l
    sum=1.0D0
    r=1.0D0
    DO  k=1,k0
      r=0.125D0*r*(vv-(2.0*k-1.0)**2.0)/(k*x)
      sum=sum+r
    END DO
    IF (l == 1) vk1=c0*sum
    IF (l == 2) vk2=c0*sum
  END DO
END IF
RETURN
END SUBROUTINE ajyik


