PROGRAM msegv
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:12

!       ============================================================
!       Purpose: This program computes a sequence of characteristic
!                values of spheroidal prolate and oblate functions
!                using subroutine SEGV
!       Input :  m  --- Mode parameter
!                n  --- Mode parameter
!                c  --- Spheroidal parameter
!                KD --- Function code
!                       KD=1 for Prolate; KD=-1 for Oblate
!       Output:  CV --- Characteristic value for given m, n and c
!                EG(L) --- Characteristic value for mode m and n'
!                          ( L = n' - m + 1 )
!       Examples:
!                Prolate: ( KD = 1 , m = 1, n = 5, c = 5.0 )

!                m      n       c        Lambda mn(c)
!              ---------------------------------------
!                1      1      5.0        5.35042230
!                1      2      5.0       14.64295624
!                1      3      5.0       23.39761312
!                1      4      5.0       32.42194359
!                1      5      5.0       42.65818215

!                Oblate: ( KD = -1 , m = 1, n = 5, c = 5.0 )

!                m      n       c      Lambda mn(-ic)
!               --------------------------------------
!                1      1      5.0       -7.49338828
!                1      2      5.0       -7.12783752
!                1      3      5.0        2.75036721
!                1      4      5.0        8.69495925
!                1      5      5.0       18.43931577
!       =========================================================

IMPLICIT DOUBLE PRECISION (a-h,o-z)
DIMENSION eg(100)
WRITE(*,*)'Please enter KD, m, n and c '
!        READ(*,*)KD,M,N,C
kd=1
m=1
n=5
c=5.0
WRITE(*,15)kd,m,n,c
WRITE(*,*)
CALL segv(m,n,c,kd,cv,eg)
IF (kd == 1) THEN
  WRITE(*,*)'  m      n       c       Lambda mn(c)'
ELSE IF (kd == -1) THEN
  WRITE(*,*)'  m      n       c      Lambda mn(-ic)'
END IF
WRITE(*,*)'---------------------------------------'
DO  l=1,n-m+1
  n1=m+l-1
  WRITE(*,20)m,n1,c,eg(l)
END DO
15      FORMAT(1X,'KD =',i2,',  m =',i3,',   n =',i3,',  c =',f5.1)
20      FORMAT(1X,i3,4X,i3,4X,f5.1,f18.8)
END PROGRAM msegv


SUBROUTINE segv(m,n,c,kd,cv,eg)

!     =========================================================
!     Purpose: Compute the characteristic values of spheroidal
!     wave functions
!     Input :  m  --- Mode parameter
!     n  --- Mode parameter
!     c  --- Spheroidal parameter
!     KD --- Function code
!     KD=1 for Prolate; KD=-1 for Oblate
!     Output:  CV --- Characteristic value for given m, n and c
!     EG(L) --- Characteristic value for mode m and n'
!     ( L = n' - m + 1 )
!     =========================================================


INTEGER, INTENT(IN)                      :: m
INTEGER, INTENT(IN)                      :: n
DOUBLE PRECISION, INTENT(IN)             :: c
INTEGER, INTENT(IN)                      :: kd
DOUBLE PRECISION, INTENT(OUT)            :: cv
DOUBLE PRECISION, INTENT(OUT)            :: eg(200)
IMPLICIT DOUBLE PRECISION (a-h,o-z)
DIMENSION b(100),h(100),d(300),e(300),f(300),cv0(100),a(300) ,g(300)

IF (c < 1.0D-10) THEN
  DO  i=1,n
    eg(i)=(i+m)*(i+m-1.0D0)
  END DO
ELSE
  icm=INT((n-m+2)/2)
  nm=10+INT(0.5*(n-m)+c)
  cs=c*c*kd
  DO  l=0,1
    DO  i=1,nm
      IF (l == 0) k=2*(i-1)
      IF (l == 1) k=2*i-1
      dk0=m+k
      dk1=m+k+1
      dk2=2*(m+k)
      d2k=2*m+k
      a(i)=(d2k+2.0)*(d2k+1.0)/((dk2+3.0)*(dk2+5.0))*cs
      d(i)=dk0*dk1+(2.0*dk0*dk1-2.0*m*m-1.0)/((dk2-1.0) *(dk2+3.0))*cs
      g(i)=k*(k-1.0)/((dk2-3.0)*(dk2-1.0))*cs
    END DO
    DO  k=2,nm
      e(k)=DSQRT(a(k-1)*g(k))
      f(k)=e(k)*e(k)
    END DO
    f(1)=0.0D0
    e(1)=0.0D0
    xa=d(nm)+DABS(e(nm))
    xb=d(nm)-DABS(e(nm))
    nm1=nm-1
    DO  i=1,nm1
      t=DABS(e(i))+DABS(e(i+1))
      t1=d(i)+t
      IF (xa < t1) xa=t1
      t1=d(i)-t
      IF (t1 < xb) xb=t1
    END DO
    DO  i=1,icm
      b(i)=xa
      h(i)=xb
    END DO
    DO  k=1,icm
      DO  k1=k,icm
        IF (b(k1) < b(k)) THEN
          b(k)=b(k1)
          EXIT
        END IF
      END DO
      35      IF (k /= 1.AND.h(k) < h(k-1)) h(k)=h(k-1)
      DO WHILE (1==1)
        40       x1=(b(k)+h(k))/2.0D0
        cv0(k)=x1
        IF (DABS((b(k)-h(k))/x1) < 1.0D-14) EXIT
        j=0
        s=1.0D0
        DO  i=1,nm
          IF (s == 0.0D0) s=s+1.0D-30
          t=f(i)/s
          s=d(i)-t-x1
          IF (s < 0.0D0) j=j+1
        END DO
        IF (j < k) THEN
          h(k)=x1
        ELSE
          b(k)=x1
          IF (j >= icm) THEN
            b(icm)=x1
          ELSE
            IF (h(j+1) < x1) h(j+1)=x1
            IF (x1 < b(j)) b(j)=x1
          END IF
        END IF
      END DO
      50      cv0(k)=x1
      IF (l == 0) eg(2*k-1)=cv0(k)
      IF (l == 1) eg(2*k)=cv0(k)
    END DO
  END DO
END IF
70   cv=eg(n-m+1)
RETURN
END SUBROUTINE segv
