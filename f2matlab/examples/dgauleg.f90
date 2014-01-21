SUBROUTINE dgauleg(x1,x2,x,w,n)
 
! Code converted using TO_F90 by Alan Miller
! Date: 2002-05-14  Time: 13:40:52
 
!----------------------------------------------------------------

REAL*8, INTENT(IN OUT)                   :: x1
REAL*8, INTENT(IN OUT)                   :: x2
REAL*8, INTENT(OUT)                      :: x(n)
REAL*8, INTENT(OUT)                      :: w(n)
INTEGER, INTENT(IN)                      :: n


REAL*8, PARAMETER :: eps=3.d-14
INTEGER :: i,j,m
REAL*8 p1,p2,p3,pp,xl,xm,z,z1

m=(n+1)/2
z1=huge(1.0)
xm=0.5D0*(x2+x1)
xl=0.5D0*(x2-x1)
DO  i=1,m
  z=COS(3.141592654D0*(i-.25D0)/ (n+.5D0))
  DO WHILE (ABS(z-z1) > eps)
    p1=1.d0
    p2=0.d0
    DO  j=1,n
      p3=p2
      p2=p1
      p1=((2.d0*j-1.d0)*z*p2-(j-1.d0)*p3)/j
    END DO
    pp=n*(z*p1-p2)/(z*z-1.d0)
    z1=z
    z=z1-p1/pp
  END DO
  x(i)=xm-xl*z
  x(n+1-i)=xm+xl*z
  w(i)=2.d0*xl/((1.d0-z*z)*pp*pp)
  w(n+1-i)=w(i)
END DO
RETURN
END SUBROUTINE dgauleg
