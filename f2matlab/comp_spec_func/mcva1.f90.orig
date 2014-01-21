PROGRAM mcva1
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:10

!     ============================================================
!     Purpose: This program computes a sequence of characteristic
!     values of Mathieu functions using subroutine CVA1
!     Input :  m  --- Order of Mathieu functions
!     q  --- Parameter of Mathieu functions
!     KD --- Case code
!     KD=1 for cem(x,q)  ( m = 0,2,4,...)
!     KD=2 for cem(x,q)  ( m = 1,3,5,...)
!     KD=3 for sem(x,q)  ( m = 1,3,5,...)
!     KD=4 for sem(x,q)  ( m = 2,4,6,...)
!     Output:  CV(I) --- Characteristic values; I = 1,2,3,...
!     For KD=1, CV(1), CV(2), CV(3),..., correspond to
!     the characteristic values of cem for m = 0,2,4,...
!     For KD=2, CV(1), CV(2), CV(3),..., correspond to
!     the characteristic values of cem for m = 1,3,5,...
!     For KD=3, CV(1), CV(2), CV(3),..., correspond to
!     the characteristic values of sem for m = 1,3,5,...
!     For KD=4, CV(1), CV(2), CV(3),..., correspond to
!     the characteristic values of sem for m = 0,2,4,...

!     Example: Mmax = 12,    q = 25.00

!     Characteristic values of Mathieu functions

!     m            a                  b
!     ------------------------------------------
!     0      -40.256779547
!     1      -21.314899691      -40.256778985
!     2       -3.522164727      -21.314860622
!     3       12.964079444       -3.520941527
!     4       27.805240581       12.986489953
!     5       40.050190986       28.062765899
!     6       48.975786716       41.801071292
!     7       57.534689001       55.002957151
!     8       69.524065166       69.057988351
!     9       85.076999882       85.023356505
!     10      103.230204804      103.225680042
!     11      123.643012376      123.642713667
!     12      146.207690643      146.207674647
!     ============================================================

IMPLICIT DOUBLE PRECISION (a-h,o-z)
DIMENSION cv1(200),cv2(200),cve(200),cvs(200)
WRITE(*,*)'Please enter Mmax,q =?'
!     READ(*,*)MMAX,Q
mmax=12
q=25.0
WRITE(*,25)mmax,q
WRITE(*,*)
CALL cva1(1,mmax,q,cv1)
CALL cva1(2,mmax,q,cv2)
DO  j=1,mmax/2+1
  cve(2*j-1)=cv1(j)
  cve(2*j)=cv2(j)
END DO
CALL cva1(3,mmax,q,cv1)
CALL cva1(4,mmax,q,cv2)
DO  j=1,mmax/2+1
  cvs(2*j)=cv1(j)
  cvs(2*j+1)=cv2(j)
END DO
WRITE(*,35)
WRITE(*,*)
WRITE(*,*)'  m            a                  b'
WRITE(*,*)'------------------------------------------'
DO  j=0,mmax
  IF (j == 0) WRITE(*,30)j,cve(j+1)
  IF (j /= 0) WRITE(*,30)j,cve(j+1),cvs(j+1)
END DO
25   FORMAT(3X,6HMMAX =,i3,',    ',3HQ =,f6.2)
30   FORMAT(1X,i3,2F19.9)
35   FORMAT(1X,'Characteristic values of Mathieu functions')
END PROGRAM mcva1


SUBROUTINE cva1(kd,m,q,cv)

!     ============================================================
!     Purpose: Compute a sequence of characteristic values of
!     Mathieu functions
!     Input :  M  --- Maximum order of Mathieu functions
!     q  --- Parameter of Mathieu functions
!     KD --- Case code
!     KD=1 for cem(x,q)  ( m = 0,2,4,תתת )
!     KD=2 for cem(x,q)  ( m = 1,3,5,תתת )
!     KD=3 for sem(x,q)  ( m = 1,3,5,תתת )
!     KD=4 for sem(x,q)  ( m = 2,4,6,תתת )
!     Output:  CV(I) --- Characteristic values; I = 1,2,3,...
!     For KD=1, CV(1), CV(2), CV(3),..., correspond to
!     the characteristic values of cem for m = 0,2,4,...
!     For KD=2, CV(1), CV(2), CV(3),..., correspond to
!     the characteristic values of cem for m = 1,3,5,...
!     For KD=3, CV(1), CV(2), CV(3),..., correspond to
!     the characteristic values of sem for m = 1,3,5,...
!     For KD=4, CV(1), CV(2), CV(3),..., correspond to
!     the characteristic values of sem for m = 0,2,4,...
!     ============================================================


INTEGER, INTENT(IN)                      :: kd
INTEGER, INTENT(IN)                      :: m
DOUBLE PRECISION, INTENT(IN)             :: q
DOUBLE PRECISION, INTENT(OUT)            :: cv(200)
IMPLICIT DOUBLE PRECISION (a-h,o-z)
DIMENSION g(200),h(200),d(500),e(500),f(500)

eps=1.0D-14
icm=INT(m/2)+1
IF (kd == 4) icm=m/2
IF (q == 0.0D0) THEN
  IF (kd == 1) THEN
    DO  ic=1,icm
      cv(ic)=4.0D0*(ic-1.0D0)**2
    END DO
  ELSE IF (kd /= 4) THEN
    DO  ic=1,icm
      cv(ic)=(2.0D0*ic-1.0D0)**2
    END DO
  ELSE
    DO  ic=1,icm
      cv(ic)=4.0D0*ic*ic
    END DO
  END IF
ELSE
  nm=INT(10+1.5*m+0.5*q)
  e(1)=0.0D0
  f(1)=0.0D0
  IF (kd == 1) THEN
    d(1)=0.0D0
    DO  i=2,nm
      d(i)=4.0D0*(i-1.0D0)**2
      e(i)=q
      f(i)=q*q
    END DO
    e(2)=DSQRT(2.0D0)*q
    f(2)=2.0D0*q*q
  ELSE IF (kd /= 4) THEN
    d(1)=1.0D0+(-1)**kd*q
    DO  i=2,nm
      d(i)=(2.0D0*i-1.0D0)**2
      e(i)=q
      f(i)=q*q
    END DO
  ELSE
    d(1)=4.0D0
    DO  i=2,nm
      d(i)=4.0D0*i*i
      e(i)=q
      f(i)=q*q
    END DO
  END IF
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
    g(i)=xa
    h(i)=xb
  END DO
  DO  k=1,icm
    DO  k1=k,icm
      IF (g(k1) < g(k)) THEN
        g(k)=g(k1)
        EXIT
      END IF
    END DO
    55     IF (k /= 1.AND.h(k) < h(k-1)) h(k)=h(k-1)
    DO
      60      x1=(g(k)+h(k))/2.0D0
      cv(k)=x1
      IF (.NOT.(DABS((g(k)-h(k))/x1) < eps)) THEN
        j=0
        s=1.0D0
        DO  i=1,nm
          IF (s == 0.0D0) s=s+1.0D-30
          t=f(i)/s
          s=d(i)-t-x1
          IF (s < 0.0) j=j+1
        END DO
        IF (j < k) THEN
          h(k)=x1
        ELSE
          g(k)=x1
          IF (j >= icm) THEN
            g(icm)=x1
          ELSE
            IF (h(j+1) < x1) h(j+1)=x1
            IF (x1 < g(j)) g(j)=x1
          END IF
        END IF
      ELSE
        cv(k)=x1
        EXIT
      END IF
    END DO
  END DO
END IF
RETURN
END SUBROUTINE cva1
