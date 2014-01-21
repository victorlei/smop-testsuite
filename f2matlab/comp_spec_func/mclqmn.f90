PROGRAM mclqmn
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:10

!       ============================================================
!       Purpose: This program computes the associated Legendre
!                functions Qmn(z) and their derivatives Qmn'(z) for
!                a complex argument using subroutine CLQMN
!       Definition: Qmn(z)=(-1)**m*(1-z*z)**(m/2)*dm/dzm[Qn(z)]
!                   Q0(z)=1/2*LOG[(1+z)/(1-z)]     ( for |z|<1 )
!                   Qmn(z)=(z*z-1)**(m/2)*dm/dzm[Qn(z)]
!                   Q0(z)=1/2*LOG[(z+1)/(z-1)]     ( for |z|>1 )
!       Input :  x --- Real part of z
!                y --- Imaginary part of z
!                m --- Order of Qmn(z)  ( m = 0,1,2,תתת )
!                n --- Degree of Qmn(z) ( n = 0,1,2,תתת )
!       Output:  CQM(m,n) --- Qmn(z)
!                CQD(m,n) --- Qmn'(z)
!       Examples:
!                n = 5, x = 0.5, y = 0.2

!       m     Re[Qmn(z)]    Im[Qmn(z)]    Re[Qmn'(z)]   Im[Qmn'(z)]
!      -------------------------------------------------------------
!       0    .987156D+00   .354345D+00    .324023D+01  -.447297D+01
!       1   -.240328D+01   .436861D+01    .281158D+02   .171437D+02
!       2   -.245853D+02  -.138072D+02   -.106283D+03   .913792D+02
!       3    .102723D+03  -.651233D+02   -.362578D+03  -.429802D+03
!       4    .155510D+03   .357712D+03    .196975D+04  -.287414D+02
!       5   -.167357D+04  -.680954D+03   -.193093D+04  -.925757D+03

!                n = 5, x = 2.5, y = 1.0

!       m     Re[Qmn(z)]    Im[Qmn(z)]    Re[Qmn'(z)]   Im[Qmn'(z)]
!      -------------------------------------------------------------
!       0   -.274023D-04  -.227141D-04    .809834D-04   .210884D-04
!       1    .165620D-03   .136108D-03   -.489095D-03  -.124400D-03
!       2   -.118481D-02  -.948832D-03    .349090D-02   .825057D-03
!       3    .982179D-02   .753264D-02   -.288271D-01  -.596384D-02
!       4   -.927915D-01  -.669521D-01    .270840D+00   .451376D-01
!       5    .985601D+00   .656737D+00   -.285567D+01  -.332533D+00
!       ============================================================

IMPLICIT DOUBLE PRECISION (x,y)
IMPLICIT COMPLEX*16 (c,z)
DIMENSION cqm(0:40,0:40),cqd(0:40,0:40)
WRITE(*,*)'  Please enter m, n, x and y '
!        READ(*,*) M,N,X,Y
m=5
n=5
x=.5
y=.2
WRITE(*,30)m,n,x,y
CALL clqmn(40,m,n,x,y,cqm,cqd)
WRITE(*,*)'   m   n   Re[Qmn(z)]    Im[Qmn(z)]    ',  &
    'Re[Qmn''(Z)]   IM[QMN''(Z)]'
WRITE(*,*)' -----------------------------------',  &
    '------------------------------'
DO  j=0,n
  WRITE(*,20)m,j,cqm(m,j),cqd(m,j)
END DO
20      FORMAT(1X,2I4,2D14.6,1X,2D14.6)
30      FORMAT(1X,'m =',i2,', ','n =',i2,', ','x =',f4.1, ', ','y =',f4.1)
END PROGRAM mclqmn


SUBROUTINE clqmn(mm,m,n,x,y,cqm,cqd)

!       =======================================================
!       Purpose: Compute the associated Legendre functions of
!                the second kind, Qmn(z) and Qmn'(z), for a
!                complex argument
!       Input :  x  --- Real part of z
!                y  --- Imaginary part of z
!                m  --- Order of Qmn(z)  ( m = 0,1,2,תתת )
!                n  --- Degree of Qmn(z) ( n = 0,1,2,תתת )
!                mm --- Physical dimension of CQM and CQD
!       Output:  CQM(m,n) --- Qmn(z)
!                CQD(m,n) --- Qmn'(z)
!       =======================================================


INTEGER, INTENT(IN OUT)                  :: mm
INTEGER, INTENT(IN)                      :: m
INTEGER, INTENT(IN)                      :: n
DOUBLE PRECISION, INTENT(IN OUT)         :: x
DOUBLE PRECISION, INTENT(IN OUT)         :: y
COMPLEX, INTENT(OUT)                     :: cqm(0:mm,0:n)
COMPLEX, INTENT(OUT)                     :: cqd(0:mm,0:n)
IMPLICIT DOUBLE PRECISION (x,y)
IMPLICIT COMPLEX*16 (c,z)


z=CMPLX(x,y)
IF (DABS(x) == 1.0D0.AND.y == 0.0D0) THEN
  DO  i=0,m
    DO  j=0,n
      cqm(i,j)=(1.0D+300,0.0D0)
      cqd(i,j)=(1.0D+300,0.0D0)
    END DO
  END DO
  RETURN
END IF
xc=CDABS(z)
IF (DIMAG(z) == 0.0D0.OR.xc < 1.0D0) ls=1
IF (xc > 1.0D0) ls=-1
zq=CDSQRT(ls*(1.0D0-z*z))
zs=ls*(1.0D0-z*z)
cq0=0.5D0*CDLOG(ls*(1.0D0+z)/(1.0D0-z))
IF (xc < 1.0001D0) THEN
  cqm(0,0)=cq0
  cqm(0,1)=z*cq0-1.0D0
  cqm(1,0)=-1.0D0/zq
  cqm(1,1)=-zq*(cq0+z/(1.0D0-z*z))
  DO  i=0,1
    DO  j=2,n
      cqm(i,j)=((2.0D0*j-1.0D0)*z*cqm(i,j-1) -(j+i-1.0D0)*cqm(i,j-2))/(j-i)
    END DO
  END DO
  DO  j=0,n
    DO  i=2,m
      cqm(i,j)=-2.0D0*(i-1.0D0)*z/zq*cqm(i-1,j)-ls*  &
          (j+i-1.0D0)*(j-i+2.0D0)*cqm(i-2,j)
    END DO
  END DO
ELSE
  IF (xc > 1.1) THEN
    km=40+m+n
  ELSE
    km=(40+m+n)*INT(-1.0-1.8*LOG(xc-1.0))
  END IF
  cqf2=(0.0D0,0.0D0)
  cqf1=(1.0D0,0.0D0)
  DO  k=km,0,-1
    cqf0=((2*k+3.0D0)*z*cqf1-(k+2.0D0)*cqf2)/(k+1.0D0)
    IF (k <= n) cqm(0,k)=cqf0
    cqf2=cqf1
    cqf1=cqf0
  END DO
  DO  k=0,n
    cqm(0,k)=cq0*cqm(0,k)/cqf0
  END DO
  cqf2=0.0D0
  cqf1=1.0D0
  DO  k=km,0,-1
    cqf0=((2*k+3.0D0)*z*cqf1-(k+1.0D0)*cqf2)/(k+2.0D0)
    IF (k <= n) cqm(1,k)=cqf0
    cqf2=cqf1
    cqf1=cqf0
  END DO
  cq10=-1.0D0/zq
  DO  k=0,n
    cqm(1,k)=cq10*cqm(1,k)/cqf0
  END DO
  DO  j=0,n
    cq0=cqm(0,j)
    cq1=cqm(1,j)
    DO  i=0,m-2
      cqf=-2.0D0*(i+1)*z/zq*cq1+(j-i)*(j+i+1.0D0)*cq0
      cqm(i+2,j)=cqf
      cq0=cq1
      cq1=cqf
    END DO
  END DO
END IF
cqd(0,0)=ls/zs
DO  j=1,n
  cqd(0,j)=ls*j*(cqm(0,j-1)-z*cqm(0,j))/zs
END DO
DO  j=0,n
  DO  i=1,m
    cqd(i,j)=ls*i*z/zs*cqm(i,j)+(i+j)*(j-i+1.0D0) /zq*cqm(i-1,j)
  END DO
END DO
RETURN
END SUBROUTINE clqmn


