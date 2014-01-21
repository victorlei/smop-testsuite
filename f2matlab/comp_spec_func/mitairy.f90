PROGRAM mitairy
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:11

!       ===========================================================
!       Purpose: This program computes the integrals of Airy
!                functions using subroutine ITAIRY
!       Input  : x   --- Upper limit of the integral
!       Output : APT --- Integration of Ai(t) from 0 and x
!                BPT --- Integration of Bi(t) from 0 and x
!                ANT --- Integration of Ai(-t) from 0 and x
!                BNT --- Integration of Bi(-t) from 0 and x
!       Example:

!         x      Ai(t)dt       Bi(t)dt       Ai(-t)dt     Bi(-t)dt
!        ----------------------------------------------------------
!         5    .33328759   .32147832D+03    .71788220    .15873094
!        10    .33333333   .14780980D+09    .76569840    .01504043
!        15    .33333333   .49673090D+16    .68358063    .07202621
!        20    .33333333   .47447423D+25    .71173925   -.03906173
!        25    .33333333   .78920820D+35    .70489539    .03293190
!       ===========================================================

IMPLICIT DOUBLE PRECISION (a-h,o-z)
WRITE(*,*)'Please enter x '
!        READ(*,*)X
x=5.0
WRITE(*,20)
WRITE(*,30)
CALL itairy(x,apt,bpt,ant,bnt)
WRITE(*,10)x,apt,bpt,ant,bnt
10      FORMAT(1X,f5.1,f14.8,2X,d15.8,2F14.8)
20      FORMAT(3X,'x',8X,'Ai(t)dt',7X,'Bi(t)dt',9X, 'Ai(-t)dt',6X,'Bi(-t)dt')
30      FORMAT(2X,'----------------------------------',  &
    '------------------------------')
END PROGRAM mitairy


SUBROUTINE itairy(x,apt,bpt,ant,bnt)

!       ======================================================
!       Purpose: Compute the integrals of Airy fnctions with
!                respect to t from 0 and x ( x ò 0 )
!       Input  : x   --- Upper limit of the integral
!       Output : APT --- Integration of Ai(t) from 0 and x
!                BPT --- Integration of Bi(t) from 0 and x
!                ANT --- Integration of Ai(-t) from 0 and x
!                BNT --- Integration of Bi(-t) from 0 and x
!       ======================================================


DOUBLE PRECISION, INTENT(IN OUT)         :: x
DOUBLE PRECISION, INTENT(OUT)            :: apt
DOUBLE PRECISION, INTENT(OUT)            :: bpt
DOUBLE PRECISION, INTENT(OUT)            :: ant
DOUBLE PRECISION, INTENT(OUT)            :: bnt
IMPLICIT DOUBLE PRECISION (a-h,o-z)
DIMENSION a(16)

eps=1.0D-15
pi=3.141592653589793D0
c1=.355028053887817D0
c2=.258819403792807D0
sr3=1.732050807568877D0
IF (x == 0.0D0) THEN
  apt=0.0D0
  bpt=0.0D0
  ant=0.0D0
  bnt=0.0D0
ELSE
  IF (DABS(x) <= 9.25D0) THEN
    DO  l=0,1
      x=(-1)**l*x
      fx=x
      r=x
      DO  k=1,40
        r=r*(3.0*k-2.0D0)/(3.0*k+1.0D0)*x/(3.0*k) *x/(3.0*k-1.0D0)*x
        fx=fx+r
        IF (DABS(r) < DABS(fx)*eps) EXIT
      END DO
      15               gx=.5D0*x*x
      r=gx
      DO  k=1,40
        r=r*(3.0*k-1.0D0)/(3.0*k+2.0D0)*x/(3.0*k) *x/(3.0*k+1.0D0)*x
        gx=gx+r
        IF (DABS(r) < DABS(gx)*eps) EXIT
      END DO
      25               ant=c1*fx-c2*gx
      bnt=sr3*(c1*fx+c2*gx)
      IF (l == 0) THEN
        apt=ant
        bpt=bnt
      ELSE
        ant=-ant
        bnt=-bnt
        x=-x
      END IF
    END DO
  ELSE
    DATA a/.569444444444444D0,.891300154320988D0,  &
        .226624344493027D+01,.798950124766861D+01,  &
        .360688546785343D+02,.198670292131169D+03,  &
        .129223456582211D+04,.969483869669600D+04,  &
        .824184704952483D+05,.783031092490225D+06,  &
        .822210493622814D+07,.945557399360556D+08,  &
        .118195595640730D+10,.159564653040121D+11,  &
        .231369166433050D+12,.358622522796969D+13/
    q2=1.414213562373095D0
    q0=.3333333333333333D0
    q1=.6666666666666667D0
    xe=x*DSQRT(x)/1.5D0
    xp6=1.0D0/DSQRT(6.0D0*pi*xe)
    su1=1.0D0
    r=1.0D0
    xr1=1.0D0/xe
    DO  k=1,16
      r=-r*xr1
      su1=su1+a(k)*r
    END DO
    su2=1.0D0
    r=1.0D0
    DO  k=1,16
      r=r*xr1
      su2=su2+a(k)*r
    END DO
    apt=q0-DEXP(-xe)*xp6*su1
    bpt=2.0D0*DEXP(xe)*xp6*su2
    su3=1.0D0
    r=1.0D0
    xr2=1.0D0/(xe*xe)
    DO  k=1,8
      r=-r*xr2
      su3=su3+a(2*k)*r
    END DO
    su4=a(1)*xr1
    r=xr1
    DO  k=1,7
      r=-r*xr2
      su4=su4+a(2*k+1)*r
    END DO
    su5=su3+su4
    su6=su3-su4
    ant=q1-q2*xp6*(su5*DCOS(xe)-su6*DSIN(xe))
    bnt=q2*xp6*(su5*DSIN(xe)+su6*DCOS(xe))
  END IF
END IF
RETURN
END SUBROUTINE itairy
