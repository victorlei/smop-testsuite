PROGRAM mlqmn
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:12

!       ===============================================================
!       Purpose: This program computes the associated Legendre
!                functions Qmn(x) and their derivatives Qmn'(x) using
!                subroutine LQMN
!       Input :  x --- Argument of Qmn(x)
!                m --- Order of Qmn(x)  ( m = 0,1,2,תתת )
!                n --- Degree of Qmn(x) ( n = 0,1,2,תתת )
!       Output:  QM(m,n) --- Qmn(x)
!                QD(m,n) --- Qmn'(x)
!       Examples:

!       Qmn(x):  x = 0.5
!       n\m      0           1           2           3           4
!       ---------------------------------------------------------------
!        0     .549306   -1.154701    1.333333   -5.388603   26.666667
!        1    -.725347   -1.053063    2.666667   -6.158403   32.000000
!        2    -.818663     .729806    4.069272  -12.316806   42.666667
!        3    -.198655    2.491853    -.493486  -23.778868   85.333333
!        4     .440175    1.934087  -11.036781   -9.325204  186.818394

!       Qmn'(x): x = 0.5
!       n\m      0           1           2           3           4
!       ---------------------------------------------------------------
!        0    1.333333    -.769800    4.444444  -20.014809  145.777778
!        1    1.215973   -2.377159    3.555556  -24.633611  156.444444
!        2    -.842707   -5.185328    8.796526  -24.633611  199.111111
!        3   -2.877344   -1.091406   28.115454  -50.976710  227.555556
!        4   -2.233291   11.454786   25.483527 -197.068892  412.039838

!       Qmn(x): x = 2.0
!       n\m      0           1           2           3           4
!       ---------------------------------------------------------------
!        0     .549306    -.577350    1.333333   -5.003702   26.666667
!        1     .098612    -.203274     .666667   -3.079201   18.666667
!        2     .021184    -.064946     .277089   -1.539601   10.666667
!        3     .004871    -.019817     .104220    -.679543    5.333333
!        4     .001161    -.005887     .036816    -.276005    2.427640

!       Qmn'(x): x = 2.0
!       n\m      0           1           2           3           4
!       ---------------------------------------------------------------
!        0    -.333333     .384900   -1.111111    5.388603  -36.444444
!        1    -.117361     .249384    -.888889    4.618802  -32.000000
!        2    -.037496     .116680    -.519437    3.079201  -23.111111
!        3    -.011442     .046960    -.253375    1.720114  -14.222222
!        4    -.003399     .017331    -.110263     .849589   -7.748516
!       ===============================================================

IMPLICIT DOUBLE PRECISION (q,x)
DIMENSION qm(0:100,0:100),qd(0:100,0:100)
WRITE(*,*)'  Please enter m, n and x'
!        READ(*,*) M,N,X
m=0
n=4
x=.5
WRITE(*,*)
WRITE(*,*)'  m     n      x          Qmn(x)         Qmn''(X)'
WRITE(*,*)' ---------------------------------------------------'
CALL lqmn(100,m,n,x,qm,qd)
DO  j=0,n
  WRITE(*,10)m,j,x,qm(m,j),qd(m,j)
END DO
10      FORMAT(1X,i3,3X,i3,3X,f5.1,2D17.8)
END PROGRAM mlqmn


SUBROUTINE lqmn(mm,m,n,x,qm,qd)

!       ==========================================================
!       Purpose: Compute the associated Legendre functions of the
!                second kind, Qmn(x) and Qmn'(x)
!       Input :  x  --- Argument of Qmn(x)
!                m  --- Order of Qmn(x)  ( m = 0,1,2,תתת )
!                n  --- Degree of Qmn(x) ( n = 0,1,2,תתת )
!                mm --- Physical dimension of QM and QD
!       Output:  QM(m,n) --- Qmn(x)
!                QD(m,n) --- Qmn'(x)
!       ==========================================================


INTEGER, INTENT(IN OUT)                  :: mm
INTEGER, INTENT(IN)                      :: m
INTEGER, INTENT(IN)                      :: n
DOUBLE PRECISION, INTENT(IN)             :: x
DOUBLE PRECISION, INTENT(OUT)            :: qm(0:mm,0:n)
DOUBLE PRECISION, INTENT(OUT)            :: qd(0:mm,0:n)
IMPLICIT DOUBLE PRECISION (q,x)


IF (DABS(x) == 1.0D0) THEN
  DO  i=0,m
    DO  j=0,n
      qm(i,j)=1.0D+300
      qd(i,j)=1.0D+300
    END DO
  END DO
  RETURN
END IF
ls=1
IF (DABS(x) > 1.0D0) ls=-1
xs=ls*(1.0D0-x*x)
xq=DSQRT(xs)
q0=0.5D0*DLOG(DABS((x+1.0D0)/(x-1.0D0)))
IF (DABS(x) < 1.0001D0) THEN
  qm(0,0)=q0
  qm(0,1)=x*q0-1.0D0
  qm(1,0)=-1.0D0/xq
  qm(1,1)=-xq*(q0+x/(1.0D0-x*x))
  DO  i=0,1
    DO  j=2,n
      qm(i,j)=((2.0D0*j-1.0D0)*x*qm(i,j-1) -(j+i-1.0D0)*qm(i,j-2))/(j-i)
    END DO
  END DO
  DO  j=0,n
    DO  i=2,m
      qm(i,j)=-2.0D0*(i-1.0D0)*x/xq*qm(i-1,j)-ls*  &
          (j+i-1.0D0)*(j-i+2.0D0)*qm(i-2,j)
    END DO
  END DO
ELSE
  IF (DABS(x) > 1.1D0) THEN
    km=40+m+n
  ELSE
    km=(40+m+n)*INT(-1.0-1.8*LOG(x-1.0))
  END IF
  qf2=0.0D0
  qf1=1.0D0
  DO  k=km,0,-1
    qf0=((2*k+3.0D0)*x*qf1-(k+2.0D0)*qf2)/(k+1.0D0)
    IF (k <= n) qm(0,k)=qf0
    qf2=qf1
    qf1=qf0
  END DO
  DO  k=0,n
    qm(0,k)=q0*qm(0,k)/qf0
  END DO
  qf2=0.0D0
  qf1=1.0D0
  DO  k=km,0,-1
    qf0=((2*k+3.0D0)*x*qf1-(k+1.0D0)*qf2)/(k+2.0D0)
    IF (k <= n) qm(1,k)=qf0
    qf2=qf1
    qf1=qf0
  END DO
  q10=-1.0D0/xq
  DO  k=0,n
    qm(1,k)=q10*qm(1,k)/qf0
  END DO
  DO  j=0,n
    q0=qm(0,j)
    q1=qm(1,j)
    DO  i=0,m-2
      qf=-2.0D0*(i+1)*x/xq*q1+(j-i)*(j+i+1.0D0)*q0
      qm(i+2,j)=qf
      q0=q1
      q1=qf
    END DO
  END DO
END IF
qd(0,0)=ls/xs
DO  j=1,n
  qd(0,j)=ls*j*(qm(0,j-1)-x*qm(0,j))/xs
END DO
DO  j=0,n
  DO  i=1,m
    qd(i,j)=ls*i*x/xs*qm(i,j)+(i+j)*(j-i+1.0D0)/xq*qm(i-1,j)
  END DO
END DO
RETURN
END SUBROUTINE lqmn
