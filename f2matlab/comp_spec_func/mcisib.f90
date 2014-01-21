PROGRAM mcisib
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:10

!       ========================================================
!       Purpose: This program computes the cosine and sine
!                integrals using subroutine CISIB
!       Input :  x  --- Argument of Ci(x) and Si(x)
!       Output:  CI --- Ci(x)
!                SI --- Si(x)
!       Example:

!                   x        Ci(x)           Si(x)
!                ------------------------------------
!                  0.0    - ì                 0
!                  5.0    -.190030D+00      1.549931
!                 10.0    -.454563D-01      1.658348
!                 20.0     .444201D-01      1.548241
!                 30.0    -.330326D-01      1.566757
!                 40.0     .190201D-01      1.586985
!       ========================================================

IMPLICIT DOUBLE PRECISION (a-h,o-z)
WRITE(*,*)'Please enter x '
!        READ(*,*) X
x=10.0
WRITE(*,*)'   x        Ci(x)           Si(x)'
WRITE(*,*)'------------------------------------'
CALL cisib(x,ci,si)
IF (x /= 0.0D0) WRITE(*,10)x,ci,si
IF (x == 0.0D0) WRITE(*,20)
10      FORMAT(1X,f5.1,d16.6,f14.6)
20      FORMAT(3X,' .0',3X,' - i',17X,'0')
END PROGRAM mcisib


SUBROUTINE cisib(x,ci,si)

!       =============================================
!       Purpose: Compute cosine and sine integrals
!                Si(x) and Ci(x) ( x ò 0 )
!       Input :  x  --- Argument of Ci(x) and Si(x)
!       Output:  CI --- Ci(x)
!                SI --- Si(x)
!       =============================================



DOUBLE PRECISION, INTENT(IN)             :: x
DOUBLE PRECISION, INTENT(OUT)            :: ci
DOUBLE PRECISION, INTENT(OUT)            :: si
IMPLICIT DOUBLE PRECISION (a-h,o-z)
x2=x*x
IF (x == 0.0) THEN
  ci=-1.0D+300
  si=0.0D0
ELSE IF (x <= 1.0D0) THEN
  ci=((((-3.0D-8*x2+3.10D-6)*x2-2.3148D-4)  &
      *x2+1.041667D-2)*x2-0.25)*x2+0.577215665D0+LOG(x)
  si=((((3.1D-7*x2-2.834D-5)*x2+1.66667D-003) *x2-5.555556D-002)*x2+1.0)*x
ELSE
  fx=((((x2+38.027264D0)*x2+265.187033D0)*x2  &
      +335.67732D0)*x2+38.102495D0)/((((x2 +40.021433D0)*x2+322.624911D0)*x2  &
      +570.23628D0)*x2+157.105423D0)
  gx=((((x2+42.242855D0)*x2+302.757865D0)*x2  &
      +352.018498D0)*x2+21.821899D0)/((((x2 +48.196927D0)*x2+482.485984D0)*x2  &
      +1114.978885D0)*x2+449.690326D0)/x
  ci=fx*SIN(x)/x-gx*COS(x)/x
  si=1.570796327D0-fx*COS(x)/x-gx*SIN(x)/x
END IF
RETURN
END SUBROUTINE cisib
