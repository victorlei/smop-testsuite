PROGRAM mffk
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:11

!     ==============================================================
!     Purpose: This program computes the modified Fresnel integrals
!     FÒ(x) and KÒ(x) using subroutine FFK
!     Input :  x   --- Argument of FÒ(x) and KÒ(x)
!     KS  --- Sign code
!     KS=0 for calculating F+(x) and K+(x)
!     KS=1 for calculating F_(x) and K_(x)
!     Output:  FR  --- Re[FÒ(x)]
!     FI  --- Im[FÒ(x)]
!     FM  --- |FÒ(x)|
!     FA  --- Arg[FÒ(x)]  (Degs.)
!     GR  --- Re[KÒ(x)]
!     GI  --- Im[KÒ(x)]
!     GM  --- |KÒ(x)|
!     GA  --- Arg[KÒ(x)]  (Degs.)
!     Example:

!     x     Re[FÒ(x)]   ÒIm[FÒ(x)]    Mod[FÒ(x)]  ÒArg[FÒ(x)]
!     ----------------------------------------------------------
!     0.0    .62665707    .62665707    .88622693    45.000000
!     2.0    .16519561   -.17811942    .24293233   -47.155835
!     4.0    .03219674   -.12047678    .12470479   -75.037684
!     6.0    .08245304   -.01180212    .08329342    -8.145843
!     8.0   -.05729996    .02493542    .06249048   156.482601
!     10.0    .02553188    .04298617    .04999688    59.291561

!     x     Re[KÒ(x)]   ÒIm[KÒ(x)]    Mod[KÒ(x)]  ÒArg[KÒ(x)]
!     ----------------------------------------------------------
!     0.0    .50000000    .00000000    .50000000     0.000000
!     2.0    .10702394    .08562295    .13705989    38.661047
!     4.0    .05126306    .04818949    .07035714    43.229843
!     6.0    .03368650    .03276566    .04699328    44.206095
!     8.0    .02512396    .02473472    .03525648    44.552712
!     10.0    .02004532    .01984592    .02820772    44.713609
!     ===============================================================

IMPLICIT DOUBLE PRECISION (f,g,x)
WRITE(*,*)'Please enter x'
!     READ(*,*) X
x=10.0
WRITE(*,*)
WRITE(*,*)'   x      Re[FÒ(x)]    ÒIm[FÒ(x)]     ', 'Mod[FÒ(x)]   ÒArg[FÒ(x)]'
WRITE(*,*)' ---------------------------------------',  &
    '-----------------------'
CALL ffk(0,x,fr,fi,fm,fa,gr,gi,gm,ga)
WRITE(*,10)x,fr,fi,fm,fa
WRITE(*,*)
WRITE(*,*)'   x      Re[KÒ(x)]    ÒIm[KÒ(x)]     ', 'Mod[KÒ(x)]   ÒArg[KÒ(x)]'
WRITE(*,*)' ---------------------------------------',  &
    '-----------------------'
WRITE(*,10)x,gr,gi,gm,ga
10   FORMAT(1X,f5.1,3F14.8,f14.6)
END PROGRAM mffk


SUBROUTINE ffk(ks,x,fr,fi,fm,fa,gr,gi,gm,ga)

!     =======================================================
!     Purpose: Compute modified Fresnel integrals FÒ(x)
!     and KÒ(x)
!     Input :  x   --- Argument of FÒ(x) and KÒ(x)
!     KS  --- Sign code
!     KS=0 for calculating F+(x) and K+(x)
!     KS=1 for calculating F_(x) and K_(x)
!     Output:  FR  --- Re[FÒ(x)]
!     FI  --- Im[FÒ(x)]
!     FM  --- |FÒ(x)|
!     FA  --- Arg[FÒ(x)]  (Degs.)
!     GR  --- Re[KÒ(x)]
!     GI  --- Im[KÒ(x)]
!     GM  --- |KÒ(x)|
!     GA  --- Arg[KÒ(x)]  (Degs.)
!     ======================================================



INTEGER, INTENT(IN)                      :: ks
DOUBLE PRECISION, INTENT(IN)             :: x
DOUBLE PRECISION, INTENT(OUT)            :: fr
DOUBLE PRECISION, INTENT(OUT)            :: fi
DOUBLE PRECISION, INTENT(OUT)            :: fm
DOUBLE PRECISION, INTENT(OUT)            :: fa
DOUBLE PRECISION, INTENT(OUT)            :: gr
DOUBLE PRECISION, INTENT(OUT)            :: gi
DOUBLE PRECISION, INTENT(OUT)            :: gm
DOUBLE PRECISION, INTENT(OUT)            :: ga
IMPLICIT DOUBLE PRECISION (a-h,o-z)
srd= 57.29577951308233D0
eps=1.0D-15
pi=3.141592653589793D0
pp2=1.2533141373155D0
p2p=.7978845608028654D0
xa=DABS(x)
x2=x*x
x4=x2*x2
IF (x == 0.0D0) THEN
  fr=.5D0*DSQRT(0.5D0*pi)
  fi=(-1)**ks*fr
  fm=DSQRT(0.25D0*pi)
  fa=(-1)**ks*45.0D0
  gr=.5D0
  gi=0.0D0
  gm=.5D0
  ga=0.0D0
ELSE
  IF (xa <= 2.5D0) THEN
    xr=p2p*xa
    c1=xr
    DO  k=1,50
      xr=-.5D0*xr*(4.0D0*k-3.0D0)/k/(2.0D0*k-1.0D0) /(4.0D0*k+1.0D0)*x4
      c1=c1+xr
      IF (DABS(xr/c1) < eps) EXIT
    END DO
    15     s1=p2p*xa*xa*xa/3.0D0
    xr=s1
    DO  k=1,50
      xr=-.5D0*xr*(4.0D0*k-1.0D0)/k/(2.0D0*k+1.0D0) /(4.0D0*k+3.0D0)*x4
      s1=s1+xr
      IF (DABS(xr/s1) < eps) EXIT
    END DO
  ELSE IF (xa < 5.5D0) THEN
    m=INT(42+1.75*x2)
    xsu=0.0D0
    xc=0.0D0
    xs=0.0D0
    xf1=0.0D0
    xf0=1D-100
    DO  k=m,0,-1
      xf=(2.0D0*k+3.0D0)*xf0/x2-xf1
      IF (k == 2*INT(k/2))  THEN
        xc=xc+xf
      ELSE
        xs=xs+xf
      END IF
      xsu=xsu+(2.0D0*k+1.0D0)*xf*xf
      xf1=xf0
      xf0=xf
    END DO
    xq=DSQRT(xsu)
    xw=p2p*xa/xq
    c1=xc*xw
    s1=xs*xw
  ELSE
    xr=1.0D0
    xf=1.0D0
    DO  k=1,12
      xr=-.25D0*xr*(4.0D0*k-1.0D0)*(4.0D0*k-3.0D0)/x4
      xf=xf+xr
    END DO
    xr=1.0D0/(2.0D0*xa*xa)
    xg=xr
    DO  k=1,12
      xr=-.25D0*xr*(4.0D0*k+1.0D0)*(4.0D0*k-1.0D0)/x4
      xg=xg+xr
    END DO
    c1=.5D0+(xf*DSIN(x2)-xg*DCOS(x2))/DSQRT(2.0D0*pi)/xa
    s1=.5D0-(xf*DCOS(x2)+xg*DSIN(x2))/DSQRT(2.0D0*pi)/xa
  END IF
  40    fr=pp2*(.5D0-c1)
  fi0=pp2*(.5D0-s1)
  fi=(-1)**ks*fi0
  fm=DSQRT(fr*fr+fi*fi)
  IF (fr >= 0.0) THEN
    fa=srd*DATAN(fi/fr)
  ELSE IF (fi > 0.0) THEN
    fa=srd*(DATAN(fi/fr)+pi)
  ELSE IF (fi < 0.0) THEN
    fa=srd*(DATAN(fi/fr)-pi)
  END IF
  xp=x*x+pi/4.0D0
  cs=DCOS(xp)
  ss=DSIN(xp)
  xq2=1.0D0/DSQRT(pi)
  gr=xq2*(fr*cs+fi0*ss)
  gi=(-1)**ks*xq2*(fi0*cs-fr*ss)
  gm=DSQRT(gr*gr+gi*gi)
  IF (gr >= 0.0) THEN
    ga=srd*DATAN(gi/gr)
  ELSE IF (gi > 0.0) THEN
    ga=srd*(DATAN(gi/gr)+pi)
  ELSE IF (gi < 0.0) THEN
    ga=srd*(DATAN(gi/gr)-pi)
  END IF
  IF (x < 0.0D0) THEN
    fr=pp2-fr
    fi=(-1)**ks*pp2-fi
    fm=DSQRT(fr*fr+fi*fi)
    fa=srd*DATAN(fi/fr)
    gr=DCOS(x*x)-gr
    gi=-(-1)**ks*DSIN(x*x)-gi
    gm=DSQRT(gr*gr+gi*gi)
    ga=srd*DATAN(gi/gr)
  END IF
END IF
RETURN
END SUBROUTINE ffk
