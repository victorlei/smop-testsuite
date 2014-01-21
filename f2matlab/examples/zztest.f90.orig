program zztest
! compile with intel fortran
 integer, parameter :: n=5,m=2
  real ( kind = 8 ) rhs(2*n)
  real ( kind = 8 ) x(n)
  integer :: seed = 123456789,seed2=123

  rhs( : )=1;
  x(1:n) = rhs(1:n)

  print *,'seed=',seed

  print *,'rhs(1:)=',rhs(1:)

  do seed=1,10
   print *,'seed=',seed
  end do
  do seed=1,10,2
   print *,'seed=',seed
  end do

  do seed=1,10
   x(1)=func1(real(seed)**2)
   print *,'x(1)=',x(1)
  end do

  write ( *, * ) '  "Sector1" and "Sector2" are the CIRCLE_SECTOR computations'
end program zztest


FUNCTION func1( a_new )
 REAL :: func1
 REAL a_new
 REAL, SAVE :: a_old   !! saved
 INTEGER ::counter=0,counter2,counter3=1   ! !saved
 INTEGER i1,i2
 real, dimension(10,2) :: r0,r1(10,1),r3(0:10),r2=1,r4(1:10,2)=2,r5(0:3,0:5,-2:5,-10:4)=-1,r6(0:3,0:5,-2:5,-10:4)
 integer, parameter :: p1=3, p2=4
 real, allocatable ::a1(:,:)
 parameter ( p3=3, p4=4 )
 parameter (p5=4)
 double precision, dimension ( 2 ) :: radius1_test = (/ 0.0, 1.0 /)
 real ( kind = 8 ) gam
 double precision g(10,10)
 save g
 logical, parameter :: debug = .false.
 character ( len = 9 ), parameter, dimension(12) :: month = (/ &
      'January  ', 'February ', 'March    ', 'April    ', &
      'May      ', 'June     ', 'July     ', 'August   ', &
      'September', 'October  ', 'November ', 'December ' /)
 real err(0:10,2)
 real ( kind = 8 ), save, dimension ( 2 ) :: fx_vec = (/ &
      real ( 0.87377726306985360531D+00, kind = 8 ), &
      real ( 0.25628287737952698742E-09, kind = 8 ) /)
 integer, parameter :: maxdig = 20
 integer :: seed(1) = 123456789,seed2=123
  integer, parameter :: maxnam = 100
  integer, parameter :: maxcol = 5

  character ( len = 10 ) action
  integer i
  integer, save, dimension ( maxnam ) :: ipoint = (/ ( 0+i, i = 1, maxnam ) /)
  character ( len = 10 ) name
  character ( len = 20 ), save, dimension ( maxnam ) ::  names = &
    (/ ( ' ', i = 1, maxnam ) /)
  integer, save :: numnam = 0
  real ( kind = 8 ) rval
  real ( kind = 8 ), save, dimension ( maxnam, maxcol ) :: rvals = &
    reshape ( (/ ( 0.0D+00, i = 1, maxnam * maxcol ) /), (/ maxnam, maxcol /) )
  integer sum(p1)
  real exp
  integer try,catch
  real ( kind = 8 ) ab1as(0:27)
  data ab1as(0)/  2.13013643429065549448d0/
  data ab1as(1)/  0.6371526795218539933d-1/
  data ab1as(2)/ -0.129334917477510647d-2/
  data ab1as(3)/  0.5678328753228265d-4/
  data ab1as(4)/ -0.279434939177646d-5/
  data ab1as(5)/  0.5600214736787d-7/
  data ab1as(6)/  0.2392009242798d-7/
  data ab1as(7)/ -0.750984865009d-8/
  data ab1as(8)/  0.173015330776d-8/
  data ab1as(9)/ -0.36648877955d-9/
  data ab1as(10)/ 0.7520758307d-10/
  data ab1as(11)/-0.1517990208d-10/
  data ab1as(12)/ 0.301713710d-11/
  data ab1as(13)/-0.58596718d-12/
  data ab1as(14)/ 0.10914455d-12/
  data ab1as(15)/-0.1870536d-13/
  data ab1as(16)/ 0.262542d-14/
  data ab1as(17)/-0.14627d-15/
  data ab1as(18)/-0.9500d-16/
  data ab1as(19)/ 0.5873d-16/
  data ab1as(20)/-0.2420d-16/
  data ab1as(21)/ 0.868d-17/
  data ab1as(22)/-0.290d-17/
  data ab1as(23)/ 0.93d-18/
  data ab1as(24)/-0.29d-18/
  data ab1as(25)/ 0.9d-19/
  data ab1as(26)/-0.3d-19/
  data ab1as(27)/ 0.1d-19/
  data onerpi/ 0.56418958354775628695d0/
integer my0,my1,my2,my3
DATA my0,my1/2*12/,my2,my3/2*8/
CHARACTER*34 fmt , fmtf , fmti 
PARAMETER (N=9) 
CHARACTER*2 xi(9,1)
INTEGER ix(9,1)
!DATA (xi(i,1),i=1,N)/'AC' , 'AZ' , 'AD' , 'AA' , 'AB' , 'ZZ' ,     &
!   &      'ZA' , 'ZX' , 'ZY'/                                         
!DATA (ix(i,1),i=1,N)/4 , 5 , 1 , 3 , 2 , 7 , 8 , 9 , 6/ 

 gam ( y ) = ((((((( &
      0.035868343D+00   * y &
      - 0.193527818D+00 ) * y &
      + 0.482199394D+00 ) * y &
      - 0.756704078D+00 ) * y &
      + 0.918206857D+00 ) * y &
      - 0.897056937D+00 ) * y &
      + 0.988205891D+00 ) * y &
      - 0.577191652D+00 ) * y + 1.0D+00
fmt(1:20) = '(1X, 6X, 4H   (,E30.' 
fmt(23:34) = ',1H,,I8,1H))' 
fmtf(1:20) = '(1X,F6.1,4H   (,E30.' 
fmtf(23:34) = ',1H,,I8,1H))' 
fmti(1:20) = '(1X, I6, 4H   (,E30.' 
fmti(23:34) = ',1H,,I8,1H))' 
 try=10
 catch=try*10
 sum(2:2:-1) = 20
 exp=2.1
 print *,'exp=',exp
 !print *,'exp(2.0)=',exp(2.0)
 if ( lge ( '1', '0' ) .and. lle ( 'a', '9' ) ) then 
 end if
 call random_seed ( )
 !write ( *, '(a,i6)' ) '    The value occurs in index ', index
 write ( *, '(2x,i6,(5g14.6))' ) maxdig, g(1:5,1)
 print *,'names(1)=',names(1)
 err=20
 print *, month(1),month(2)
 write( *,*) month(1),month(2)
 g( 1, 1) =   1.0D+00
 call random_seed ( put = seed )
 call random_seed ( get = seed )

 call random_seed
 a_old = a_old+a_new
 counter = counter+1
 print *,'a_old=',a_old
 func1=counter
 print *,'shape(r1)=',shape(r1)
 print *,'shape(r2)=',shape(r2)
 print *,'shape(r3)=',shape(r3)
 print *,'shape(err)=',shape(err)

END FUNCTION func1


real(kind=8) function d_pi ( )
!*******************************************************************************
!
!! D_PI returns the value of pi as a double precision quantity.
!
!  Modified:
!
!    28 April 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real ( kind = 8 ) D_PI, the value of pi.
!
  implicit none
!  real ( kind = 8 ) d_pi
  d_pi = 3.141592653589793D+00
  return
end
