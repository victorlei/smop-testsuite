PROGRAM mcomelp
 
! Code converted using TO_F90 by Alan Miller
! Date: 2004-06-28  Time: 12:58:10

!       ===================================================
!       Purpose: This program computes complete elliptic
!                integrals K(k) and E(k) using subroutine
!                COMELP
!       Input  : K  --- Modulus k ( 0 ó k ó 1 )
!       Output : CK --- K(k)
!                CE --- E(k)
!       Example:
!                  k         K(k)          E(K)
!                ---------------------------------
!                 .00      1.570796      1.570796
!                 .25      1.596242      1.545957
!                 .50      1.685750      1.467462
!                 .75      1.910990      1.318472
!                1.00       ì            1.000000
!       ===================================================

DOUBLE PRECISION :: hk,ck,ce
WRITE(*,*)'Please enter the modulus k '
!        READ(*,*) HK
hk=.5
WRITE(*,*)'    k         K(k)          E(K)'
WRITE(*,*)'  ---------------------------------'
CALL comelp(hk,ck,ce)
IF (hk /= 1.0) WRITE(*,10) hk,ck,ce
IF (hk == 1.0) WRITE(*,20) hk,ce
10      FORMAT(2X,f5.2,2F14.6)
20      FORMAT(2X,f5.2,7X,'i',6X,f14.6)
END PROGRAM mcomelp


SUBROUTINE comelp(hk,ck,ce)

!       ==================================================
!       Purpose: Compute complete elliptic integrals K(k)
!                and E(k)
!       Input  : K  --- Modulus k ( 0 ó k ó 1 )
!       Output : CK --- K(k)
!                CE --- E(k)
!       ==================================================



DOUBLE PRECISION, INTENT(IN)             :: hk
DOUBLE PRECISION, INTENT(OUT)            :: ck
DOUBLE PRECISION, INTENT(OUT)            :: ce
IMPLICIT DOUBLE PRECISION (a-h,o-z)
pk=1.0D0-hk*hk
IF (hk == 1.0) THEN
  ck=1.0D+300
  ce=1.0D0
ELSE
  ak=(((.01451196212D0*pk+.03742563713D0)*pk  &
      +.03590092383D0)*pk+.09666344259D0)*pk+ 1.38629436112D0
  bk=(((.00441787012D0*pk+.03328355346D0)*pk+  &
      .06880248576D0)*pk+.12498593597D0)*pk+.5D0
  ck=ak-bk*DLOG(pk)
  ae=(((.01736506451D0*pk+.04757383546D0)*pk+  &
      .0626060122D0)*pk+.44325141463D0)*pk+1.0D0
  be=(((.00526449639D0*pk+.04069697526D0)*pk+  &
      .09200180037D0)*pk+.2499836831D0)*pk
  ce=ae-be*DLOG(pk)
END IF
RETURN
END SUBROUTINE comelp
