PROGRAM mclpmn
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:10

!       ============================================================
!       Purpose: This program computes the associated Legendre
!                functions Pmn(z) and their derivatives Pmn'(z) for
!                a complex argument using subroutine CLPMN
!       Input :  x --- Real part of z
!                y --- Imaginary part of z
!                m --- Order of Pmn(z),  m = 0,1,2,...,n
!                n --- Degree of Pmn(z), n = 0,1,2,...,N
!       Output:  CPM(m,n) --- Pmn(z)
!                CPD(m,n) --- Pmn'(z)
!       Examples:
!                n = 5, x = 0.5, y = 0.2

!       m     Re[Pmn(z)]    Im[Pmn(z)]    Re[Pmn'(z)]   Im[Pmn'(z)]
!      -------------------------------------------------------------
!       0    .252594D+00  -.530293D+00   -.347606D+01  -.194250D+01
!       1    .333071D+01   .135206D+01    .117643D+02  -.144329D+02
!       2   -.102769D+02   .125759D+02    .765713D+02   .598500D+02
!       3   -.572879D+02  -.522744D+02   -.343414D+03   .147389D+03
!       4    .335711D+03  -.389151D+02   -.226328D+03  -.737100D+03
!       5   -.461125D+03   .329122D+03    .187180D+04   .160494D+02

!                n = 5, x = 2.5, y = 1.0

!       m     Re[Pmn(z)]    Im[Pmn(z)]    Re[Pmn'(z)]   Im[Pmn'(z)]
!      -------------------------------------------------------------
!       0   -.429395D+03   .900336D+03   -.350391D+02   .193594D+04
!       1   -.216303D+04   .446358D+04   -.208935D+03   .964685D+04
!       2   -.883477D+04   .174005D+05   -.123703D+04   .381938D+05
!       3   -.273211D+05   .499684D+05   -.568080D+04   .112614D+06
!       4   -.565523D+05   .938503D+05   -.167147D+05   .219713D+06
!       5   -.584268D+05   .863328D+05   -.233002D+05   .212595D+06
!       ============================================================

IMPLICIT DOUBLE PRECISION (x,y)
IMPLICIT COMPLEX*16 (c,z)
DIMENSION cpm(0:40,0:40),cpd(0:40,0:40)
WRITE(*,*)'  Please enter m, n, x and y '
!        READ(*,*) M,N,X,Y
m=0
n=5
x=.5
y=.2
WRITE(*,30) m,n,x,y
CALL clpmn(40,m,n,x,y,cpm,cpd)
WRITE(*,*)'   m   n    Re[Pmn(z)]    Im[Pmn(z)]    ',  &
    'Re[Pmn''(Z)]   IM[PMN''(Z)]'
WRITE(*,*)' -----------------------------------',  &
    '-------------------------------'
DO  j=0,n
  WRITE(*,20)m,j,cpm(m,j),cpd(m,j)
END DO
20      FORMAT(1X,2I4,1X,2D14.6,1X,2D14.6)
30      FORMAT(1X,'m =',i2,', ','n =',i2,', ','x =',f5.1, ', ','y =',f5.1)
END PROGRAM mclpmn


SUBROUTINE clpmn(mm,m,n,x,y,cpm,cpd)

!       =========================================================
!       Purpose: Compute the associated Legendre functions Pmn(z)
!                and their derivatives Pmn'(z) for a complex
!                argument
!       Input :  x  --- Real part of z
!                y  --- Imaginary part of z
!                m  --- Order of Pmn(z),  m = 0,1,2,...,n
!                n  --- Degree of Pmn(z), n = 0,1,2,...,N
!                mm --- Physical dimension of CPM and CPD
!       Output:  CPM(m,n) --- Pmn(z)
!                CPD(m,n) --- Pmn'(z)
!       =========================================================


INTEGER, INTENT(IN OUT)                  :: mm
INTEGER, INTENT(IN)                      :: m
INTEGER, INTENT(IN)                      :: n
DOUBLE PRECISION, INTENT(IN)             :: x
DOUBLE PRECISION, INTENT(IN OUT)         :: y
COMPLEX, INTENT(OUT)                     :: cpm(0:mm,0:n)
COMPLEX, INTENT(OUT)                     :: cpd(0:mm,0:n)
IMPLICIT DOUBLE PRECISION (x,y)
IMPLICIT COMPLEX*16 (c,z)


z=CMPLX(x,y)
DO  i=0,n
  DO  j=0,m
    cpm(j,i)=(0.0D0,0.0D0)
    cpd(j,i)=(0.0D0,0.0D0)
  END DO
END DO
cpm(0,0)=(1.0D0,0.0D0)
IF (DABS(x) == 1.0D0.AND.y == 0.0D0) THEN
  DO  i=1,n
    cpm(0,i)=x**i
    cpd(0,i)=0.5D0*i*(i)*x**(i)
  END DO
  DO  j=1,n
    DO  i=1,m
      IF (i == 1) THEN
        cpd(i,j)=(1.0D+300,0.0D0)
      ELSE IF (i == 2) THEN
        cpd(i,j)=-0.25D0*(j+2)*(j)*j*(j-1)*x**(j)
      END IF
    END DO
  END DO
  RETURN
END IF
ls=1
IF (CDABS(z) > 1.0D0) ls=-1
zq=CDSQRT(ls*(1.0D0-z*z))
zs=ls*(1.0D0-z*z)
DO  i=1,m
  cpm(i,i)=-ls*(2.0D0*i-1.0D0)*zq*cpm(i-1,i-1)
END DO
DO  i=0,m
  cpm(i,i+1)=(2.0D0*i+1.0D0)*z*cpm(i,i)
END DO
DO  i=0,m
  DO  j=i+2,n
    cpm(i,j)=((2.0D0*j-1.0D0)*z*cpm(i,j-1)-(i+j- 1.0D0)*cpm(i,j-2))/(j-i)
  END DO
END DO
cpd(0,0)=(0.0D0,0.0D0)
DO  j=1,n
  cpd(0,j)=ls*j*(cpm(0,j-1)-z*cpm(0,j))/zs
END DO
DO  i=1,m
  DO  j=i,n
    cpd(i,j)=ls*i*z*cpm(i,j)/zs+(j+i)*(j-i+1.0D0) /zq*cpm(i-1,j)
  END DO
END DO
RETURN
END SUBROUTINE clpmn
