! this routine downloaded from:
! http://www.psc.edu/~burkardt/src/stroud/stroud.html
! Which gives credit as:
!!! STROUD - N-Dimensional Quadrature
!!!
!!!STROUD includes versions of the quadrature rules for which Arthur Stroud gave
!!!implementations, as well as some that he described but did not implement. A few other rules
!!!have been collected as well, particularly for quadrature over the interior of a triangle,
!!!which is useful in finite element calculations.
!!!
!!!Arthur Stroud published his vast collection of quadrature formulas for multidimensional
!!!regions in 1971. Integration regions included the surface or interior of the circle, sphere,
!!!or N-dimensional sphere, the triangle, tetrahedron and N-dimensional simplex, the square,
!!!cube, and N-dimensional cube, and the torus. In a few cases, he printed sample FORTRAN
!!!programs to compute these integrals.
!!!
!!!Reference:
!!!
!!!    Arthur Stroud,
!!!    Approximate Calculation of Multiple Integrals,
!!!    Prentice Hall, 1971. 
!
!  stroud_prb.f90  19 January 2001
!
program stroud_prb
!
!*******************************************************************************
!
!! STROUD_PRB is a set of tests for the STROUD routines for multiple integrals.
!
  character ( len = 8 ) date
  character ( len = 10 ) time
!
  call date_and_time ( date, time )

  write ( *, * ) ' '
  write ( *, * ) 'STROUD_PRB'
  write ( *, * ) '  STROUD is a set of routines for the approximation '
  write ( *, * ) '  of integrals over N-dimensional regions.'
  write ( *, * ) ' '
  write ( *, * ) '  Today''s date: ', date
  write ( *, * ) '  Today''s time: ', time
!
  call test002
  call test003
  call test005
  call test01
  call test02
  call test024
  call test025
  call test026
  call test03
  call test04
  call test05
  call test06
  call test07
  call test08
  call test10

  call test11
  call test12
  call test13
  call test14
  call test15
  call test16
  call test165
  call test17
  call test18
  call test19
  call test09
  call test095
  call test096
  call test20

  call test21
  call test22
  call test23
  call test24
  call test25
  call test26
  call test27
  call test28
  call test285
  call test29
  call test30

  call test31
  call test32
  call test33

  write ( *, * ) ' '
  write ( *, * ) 'STROUD_PRB'
  write ( *, * ) '  Normal end of STROUD tests.'

!  stop
end
subroutine test002
!
!*******************************************************************************
!
!! TEST002 tests CIRCLE_ANNULUS.
!
  integer, parameter :: ntest = 2
!
  double precision area
  double precision circle_annulus_area_2d
  character ( len = 7 ) fname
  double precision funcd2
  integer i
  integer j
  integer nfunc
  integer nr
  double precision radius1
  double precision, dimension ( ntest ) :: radius1_test = (/ 0.0, 1.0 /)
  double precision radius2
  double precision, dimension ( ntest ) :: radius2_test = (/ 1.0, 2.0 /)
  double precision result
  double precision xc
  double precision, dimension ( ntest ) :: xc_test = (/ 0.0, 0.0 /)
  double precision yc
  double precision, dimension ( ntest ) :: yc_test = (/ 0.0, 0.0 /)
!
  external funcd2
!
  call funcset ( 'COUNT', nfunc )

  write ( *, * ) ' '
  write ( *, * ) 'TEST002'
  write ( *, * ) '  CIRCLE_ANNULUS estimates an integral in a circular annulus.'
  write ( *, * ) ' '
  write ( *, * ) '      F   XC        YC       Radius1   Radius2   NR  Result'
  write ( *, * ) ' '

  do i = 1, ntest

    xc = xc_test(i)
    yc = yc_test(i)
    radius1 = radius1_test(i)
    radius2 = radius2_test(i)

    area = circle_annulus_area_2d ( radius1, radius2 )

    write ( *, * ) ' '
    write ( *, '(a7,4f10.6,2x,f10.6)' ) '   Area', xc, yc, radius1, radius2, area

    do j = 1, nfunc

      call funcset ( 'SET', j )

      do nr = 1, 4
        call circle_annulus ( funcd2, xc, yc, radius1, radius2, nr, result )
        write ( *, '(a7,4f10.6,i2,f10.6)' ) &
          fname(j), xc, yc, radius1, radius2, nr, result
      end do

    end do

  end do

  return
end
subroutine test003
!
!*******************************************************************************
!
!! TEST003 tests CIRCLE_ANNULUS.
!! TEST003 tests CIRCLE_SET_RT.
!! TEST003 tests CIRCLE_SUM_RT.
!
  integer, parameter :: ntest = 3
!
  double precision area
  double precision circle_annulus_area_2d
  character ( len = 7 ) fname
  double precision funcd2
  integer i
  integer j
  integer nfunc
  integer nr
  integer nr2
  integer nt
  double precision ra(5)
  double precision radius1
  double precision, dimension ( ntest ) :: radius1_test = (/ 0.0, 1.0, 1.0 /)
  double precision radius2
  double precision, dimension ( ntest ) :: radius2_test = (/ 1.0, 2.0, 3.0 /)
  double precision result1
  double precision result2
  double precision result3
  double precision rw(5)
  integer rule
  double precision ta(20)
  double precision tw(20)
  double precision xc
  double precision, dimension ( ntest ) :: xc_test = (/ 0.0, 0.0, 0.0 /)
  double precision yc
  double precision, dimension ( ntest ) :: yc_test = (/ 0.0, 0.0, 0.0 /)
  double precision zw
!
  external funcd2
!
  call funcset ( 'COUNT', nfunc )

  write ( *, * ) ' '
  write ( *, * ) 'TEST003'
  write ( *, * ) '  CIRCLE_ANNULUS estimates an integral in a circular annulus.'
  write ( *, * ) '  CIRCLE_SET_RT sets up a rule for a circle;'
  write ( *, * ) '  CIRCLE_SUM_RT applies the rule.'
  write ( *, * ) ' '
  write ( *, * ) '  RESULT1 = CIRCLE_ANNULUS result.'
  write ( *, * ) '  RESULT2 = Difference of CIRCLE_SUM_RT results.'
  write ( *, * ) ' '
  write ( *, * ) '      F   XC        YC       Radius1   Radius2   Result1 Result2'
  write ( *, * ) ' '

  do i = 1, ntest

    xc = xc_test(i)
    yc = yc_test(i)
    radius1 = radius1_test(i)
    radius2 = radius2_test(i)

    area = circle_annulus_area_2d ( radius1, radius2 )

    write ( *, * ) ' '
    write ( *, '(a7,5f11.5)' ) '   Area', xc, yc, radius1, radius2, area

    rule = 9
    call circle_set_rt ( rule, nr2, ra, rw, nt, ta, tw, zw )

    do j = 1, nfunc

      call funcset ( 'SET', j )

      nr = 5
      call circle_annulus ( funcd2, xc, yc, radius1, radius2, nr, result1 )

      call circle_sum_rt ( funcd2, xc, yc, radius1, nr2, ra, rw, nt, ta, tw, &
        zw, result2 )

      call circle_sum_rt ( funcd2, xc, yc, radius2, nr2, ra, rw, nt, ta, tw, &
        zw, result3 )

      write ( *, '(a7,6f11.5)' ) &
          fname(j), xc, yc, radius1, radius2, result1, result3 - result2

    end do

  end do

  return
end
subroutine test005
!
!*******************************************************************************
!
!! TEST005 tests CIRCLE_ANNULUS_SECTOR.
!! TEST005 tests CIRCLE_SET_RT.
!! TEST005 tests CIRCLE_SUM_RT.
!
  integer, parameter :: ntest = 4
!
  double precision as1
  double precision as2
  double precision as3
  double precision as4
  character ( len = 7 ) fname
  double precision funcd2
  integer i
  integer j
  integer nfunc
  integer nr
  integer nr2
  integer nt
  double precision pi
  double precision ra(5)
  double precision radius
  double precision radius1a
  double precision radius2a
  double precision radius1b
  double precision radius2b
  double precision radius1c
  double precision radius2c
  double precision radius1d
  double precision radius2d
  double precision result1
  double precision result2
  integer rule
  double precision rw(5)
  double precision ta(20)
  double precision theta1
  double precision theta2
  double precision theta1a
  double precision theta2a
  double precision theta1b
  double precision theta2b
  double precision theta1c
  double precision theta2c
  double precision theta1d
  double precision theta2d
  double precision tw(20)
  double precision xc
  double precision yc
  double precision zw
!
  external funcd2
!
  call funcset ( 'COUNT', nfunc )

  nr = 5

  rule = 9
  call circle_set_rt ( rule, nr2, ra, rw, nt, ta, tw, zw )

  write ( *, * ) ' '
  write ( *, * ) 'TEST005'
  write ( *, * ) '  CIRCLE_ANNULUS_SECTOR estimates an integral in a '
  write ( *, * ) '  circular annulus sector.'
  write ( *, * ) '  CIRCLE_SET_RT sets an integration rule in a circle.'
  write ( *, * ) '  CIRCLE_SUM_RT uses an integration rule in a circle.'
  write ( *, * ) ' '
  write ( *, * ) '  To test CIRCLE_ANNULUS_SECTOR, we will estimate an integral'
  write ( *, * ) '  over 4 annular sectors that make up the unit circle, '
  write ( *, * ) '  and add to get RESULT1.'
  write ( *, * ) ' '
  write ( *, * ) '  We will also estimate the integral over the unit circle'
  write ( *, * ) '  using CIRCLE_SET_RT and CIRCLE_SUM_RT to get RESULT2.'
  write ( *, * ) ' '
  write ( *, * ) '  We will then compare RESULT1 and RESULT2.'
  write ( *, * ) ' '
  write ( *, * ) '  In this test, CIRCLE_SECTOR computations will use NR = ',nr
  write ( *, * ) '  CIRCLE_SET_RT/CIRCLE_SUM_RT will use rule number ', rule
  write ( *, * ) ' '
  write ( *, * ) '  "RESULT1" is the sum of Annulus Sector calculations.'
  write ( *, * ) '  "RESULT2" is the computation for CIRCLE_SET_RT/CIRCLE_SUM_RT.'
  write ( *, * ) ' '

  xc = 0.0D+00
  yc = 0.0D+00
  radius = 1.0D+00

  radius1a = 0.0D+00
  radius2a = 0.25D+00
  theta1a = 0.0D+00
  theta2a = 0.5D+00 * pi()

  radius1b = 0.0D+00
  radius2b = 0.25D+00
  theta1b = 0.5D+00 * pi()
  theta2b = 2.0D+00 * pi()

  radius1c = 0.25D+00
  radius2c = 1.0D+00
  theta1c = 0.0D+00
  theta2c = 0.25D+00 * pi()

  radius1d = 0.25D+00
  radius2d = 1.0D+00
  theta1d = 0.25D+00 * pi()
  theta2d = 2.0D+00 * pi()

  write ( *, * ) ' '
  write ( *, * ) '     F     Result1  Result2'
  write ( *, * ) ' '

  do j = 1, nfunc

    call funcset ( 'SET', j )

    call circle_annulus_sector ( funcd2, xc, yc, radius1a, radius2a, theta1a, &
      theta2a, nr, as1 )

    call circle_annulus_sector ( funcd2, xc, yc, radius1b, radius2b, theta1b, &
      theta2b, nr, as2 )

    call circle_annulus_sector ( funcd2, xc, yc, radius1c, radius2c, theta1c, &
      theta2c, nr, as3 )

    call circle_annulus_sector ( funcd2, xc, yc, radius1d, radius2d, theta1d, &
      theta2d, nr, as4 )

    result1 = as1 + as2 + as3 + as4

    call circle_sum_rt ( funcd2, xc, yc, radius, nr2, ra, rw, nt, ta, tw, zw, &
      result2 )

    write ( *, '(a7,2g14.6)' ) fname(j), result1, result2

  end do

  return
end
subroutine test01
!
!*******************************************************************************
!
!! TEST01 tests CIRCLE_CUM.
!
  character ( len = 7 ) fname
  double precision funcd2
  integer i
  integer j
  integer nfunc
  integer norder
  double precision pi
  double precision r
  double precision result(4)
  double precision xc
  double precision yc
!
  external funcd2
!
  call funcset ( 'COUNT', nfunc )

  xc = 0.0D+00
  yc = 0.0D+00
  r = 3.0D+00
 
  write ( *, * ) ' '
  write ( *, * ) ' '
  write ( *, * ) 'TEST01'
  write ( *, * ) '  CIRCLE_CUM approximates an integral over a circle.'
  write ( *, * ) ' '
  write ( *, * ) '  We use radius R = ', r
  write ( *, * ) '  and center ', xc, ', ', yc
  write ( *, * ) ' '

  write ( *, * ) ' '
  write ( *, * ) '  Order:      2             4              8       ' // &
    '     16'
  write ( *, * ) '  F(X)'
  write ( *, * ) ' '
 
  do i = 1, nfunc

    call funcset ( 'SET', i )

    do j = 1, 4

      norder = 2**j

      call circle_cum ( funcd2, xc, yc, r, norder, result(j) )

    end do

    write ( *, '(a7,5f14.8)' ) fname(i), result(1:4)
 
  end do

  return
end
subroutine test02
!
!*******************************************************************************
!
!! TEST02 tests CIRCLE_LUNE_AREA_2D.
!! TEST02 tests CIRCLE_SECTOR_AREA_2D.
!! TEST02 tests CIRCLE_TRIANGLE_AREA_2D.
!
  double precision area1
  double precision area2
  double precision area3
  double precision circle_lune_area_2d
  double precision circle_sector_area_2d
  double precision circle_triangle_area_2d
  integer i
  double precision pi
  double precision r
  double precision theta1
  double precision theta2
!
  r = 1.0D+00

  write ( *, * ) ' '
  write ( *, * ) ' '
  write ( *, * ) 'TEST02'
  write ( *, * ) '  CIRCLE_LUNE_AREA_2D computes the area of a'
  write ( *, * ) '    circular lune, defined by joining the endpoints'
  write ( *, * ) '    of a circular arc.'
  write ( *, * ) '  CIRCLE_SECTOR_AREA_2D computes the area of a'
  write ( *, * ) '    circular sector, defined by joining the endpoints'
  write ( *, * ) '    of a circular arc to the center.'
  write ( *, * ) '  CIRCLE_TRIANGLE_AREA_2D computes the signed area of a'
  write ( *, * ) '    triangle, defined by joining the endpoints'
  write ( *, * ) '    of a circular arc and the center.'
  write ( *, * ) ' '
  write ( *, * ) '      R            Theta1      Theta2        ' // &
    'Sector       Triangle     Lune'
  write ( *, * ) ' '

  do i = 0, 12

    theta1 = 0.0D+00
    theta2 = dble ( i ) * 2.0D+00 * pi ( ) / 12.0D+00

    area1 = circle_sector_area_2d ( r, theta1, theta2 )

    area2 = circle_triangle_area_2d ( r, theta1, theta2 )

    area3 = circle_lune_area_2d ( r, theta1, theta2 )

    write ( *, '(6f14.8)' ) r, theta1, theta2, area1, area2, area3

  end do

  return
end
subroutine test024
!
!*******************************************************************************
!
!! TEST024 tests CIRCLE_LUNE_AREA_2D.
!! TEST024 tests CIRCLE_LUNE_H_AREA_2D.
!! TEST024 tests CIRCLE_LUNE_W_AREA_2D.
!
  double precision area1
  double precision area2
  double precision area3
  double precision circle_lune_area_2d
  double precision circle_lune_h_area_2d
  double precision circle_lune_w_area_2d
  double precision h
  integer i
  double precision pi
  double precision r
  double precision theta1
  double precision theta2
  double precision w
!
  r = 50.0D+00

  write ( *, * ) ' '
  write ( *, * ) ' '
  write ( *, * ) 'TEST024'
  write ( *, * ) '  For the area of a circular lune,'
  write ( *, * ) '  CIRCLE_LUNE_AREA_2D uses two angles;'
  write ( *, * ) '  CIRCLE_LUNE_H_AREA_2D works from the height;'
  write ( *, * ) '  CIRCLE_LUNE_W_AREA_2D works from the width.'
  write ( *, * ) ' '
  write ( *, * ) '  The circle has radius R = ', r
  write ( *, * ) ' '
  write ( *, * ) 'THETA1 THETA2  H     W  Area(THETA) Area(H)  Area(W)'
  write ( *, * ) ' '
  do i = 0, 12

    theta1 = 0.0D+00
    theta2 = dble ( i ) * 2.0D+00 * pi ( ) / 12.0D+00
    w = 2.0D+00 * r * sin ( 0.5D+00 * ( theta2 - theta1 ) ) 
    h = r * ( 1.0D+00 - cos ( 0.5D+00 * ( theta2 - theta1 ) ) )

    area1 = circle_lune_area_2d ( r, theta1, theta2 )

    area2 = circle_lune_h_area_2d ( r, h )

    area3 = circle_lune_w_area_2d ( r, w )

    write ( *, '(4f6.2,3f10.4)' ) theta1, theta2, h, w, area1, area2, area3

  end do

  return
end
subroutine test025
!
!*******************************************************************************
!
!! TEST025 tests CIRCLE_SECTOR.
!
  integer, parameter :: nrlo = 1
  integer, parameter :: nrhi = 5
  integer, parameter :: ntest = 4
!
  double precision area
  double precision circle_sector_area_2d
  character ( len = 7 ) fname
  double precision funcd2
  integer i
  integer j
  integer nfunc
  integer nr
  double precision pi
  double precision radius
  double precision, dimension ( ntest ) :: radius_test = (/ 1.0, 2.0, 4.0, 8.0 /)
  double precision result(nrlo:nrhi)
  double precision theta1
  double precision, dimension ( ntest ) :: theta1_test = (/ 0.0, 0.0, 0.0, 0.0 /)
  double precision theta2
  double precision, dimension ( ntest ) :: theta2_test = (/ 2.0, 1.0, 0.5, 0.25 /)
  double precision xc
  double precision, dimension ( ntest ) :: xc_test = (/ 0.0, 0.0, 0.0, 0.0 /)
  double precision yc
  double precision, dimension ( ntest ) :: yc_test = (/ 0.0, 0.0, 0.0, 0.0 /)
!
  external funcd2
!
  call funcset ( 'COUNT', nfunc )

  write ( *, * ) ' '
  write ( *, * ) 'TEST025'
  write ( *, * ) '  CIRCLE_SECTOR estimates an integral in a circular sector.'
  write ( *, * ) ' '
  write ( *, * ) '  The user can specify NR, the number of radial values'
  write ( *, * ) '  used to approximated the integral.'
  write ( *, * ) ' '
  write ( *, * ) '  In this test, computations will use values of NR'
  write ( *, * ) '  from ', nrlo, ' to ', nrhi
  write ( *, * ) ' '

  do i = 1, ntest

    xc = xc_test(i)
    yc = yc_test(i)
    radius = radius_test(i)
    theta1 = theta1_test(i) * pi()
    theta2 = theta2_test(i) * pi()

    area = circle_sector_area_2d ( radius, theta1, theta2 )

    write ( *, * ) ' '
    write ( *, * ) '  XC      YC      RADIUS  THETA1  THETA2  Area'
    write ( *, * ) ' '
    write ( *, '(6f8.4)' ) xc, yc, radius, theta1, theta2, area
    write ( *, * ) ' '
    write ( *, '(a7,14(6x,i2,6x))' ) '   F   ', ( nr, nr = nrlo, nrhi )
    write ( *, * ) ' '

    do j = 1, nfunc

      call funcset ( 'SET', j )

      do nr = nrlo, nrhi
        call circle_sector ( funcd2, xc, yc, radius, theta1, theta2, nr, &
          result(nr) )
      end do

      write ( *, '(a7,5g14.6)' ) fname(j), result(nrlo:nrhi)

    end do

  end do

  return
end
subroutine test026
!
!*******************************************************************************
!
!! TEST026 tests CIRCLE_SECTOR.
!! TEST026 tests CIRCLE_SET_RT.
!! TEST026 tests CIRCLE_SUM_RT.
!
  integer, parameter :: ntest = 4
!
  double precision area1
  double precision area2
  double precision area3
  double precision circle_area_2d
  double precision circle_sector_area_2d
  character ( len = 7 ) fname
  double precision funcd2
  integer i
  integer j
  integer nfunc
  integer nr
  integer nr2
  integer nt
  double precision pi
  double precision ra(5)
  double precision radius
  double precision, dimension ( ntest ) :: radius_test = (/ 1.0, 2.0, 4.0, 8.0 /)
  double precision result1
  double precision result2
  double precision resulta
  double precision resultb
  integer rule
  double precision rw(5)
  double precision ta(20)
  double precision theta1
  double precision, dimension ( ntest ) :: theta1_test = (/ 0.0, 0.0, 0.0, 0.0 /)
  double precision theta2
  double precision, dimension ( ntest ) :: theta2_test = (/ 2.0, 1.0, 0.5, 0.25 /)
  double precision theta3
  double precision tw(20)
  double precision xc
  double precision, dimension ( ntest ) :: xc_test = (/ 0.0, 0.0, 0.0, 0.0 /)
  double precision yc
  double precision, dimension ( ntest ) :: yc_test = (/ 0.0, 0.0, 0.0, 0.0 /)
  double precision zw
!
  external funcd2
!
  call funcset ( 'COUNT', nfunc )

  nr = 5

  rule = 9
  call circle_set_rt ( rule, nr2, ra, rw, nt, ta, tw, zw )

  write ( *, * ) ' '
  write ( *, * ) 'TEST026'
  write ( *, * ) '  CIRCLE_SECTOR estimates an integral in a circular sector.'
  write ( *, * ) '  CIRCLE_SET_RT sets an integration rule in a circle.'
  write ( *, * ) '  CIRCLE_SUM_RT uses an integration rule in a circle.'
  write ( *, * ) ' '
  write ( *, * ) '  To test CIRCLE_SECTOR, we will estimate an integral over'
  write ( *, * ) '  a sector, and over its complement and add the results'
  write ( *, * ) '  to get RESULT1.'
  write ( *, * ) ' '
  write ( *, * ) '  We will also estimate the integral over the whole circle'
  write ( *, * ) '  using CIRCLE_SET_RT and CIRCLE_SUM_RT to get RESULT2.'
  write ( *, * ) ' '
  write ( *, * ) '  We will then compare RESULT1 and RESULT2.'
  write ( *, * ) ' '
  write ( *, * ) '  In this test, CIRCLE_SECTOR computations will use NR = ',nr
  write ( *, * ) '  CIRCLE_SET_RT/CIRCLE_SUM_RT will use rule number ', rule
  write ( *, * ) ' '
  write ( *, * ) '  "Sector1" and "Sector2" are the CIRCLE_SECTOR computations'
  write ( *, * ) '  for the sector and its complement.'
  write ( *, * ) '  "Sum" is the sum of Sector1 and Sector2.'
  write ( *, * ) '  "Circle" is the computation for CIRCLE_SET_RT/CIRCLE_SUM_RT.'
  write ( *, * ) ' '

  do i = 1, ntest

    xc = xc_test(i)
    yc = yc_test(i)
    radius = radius_test(i)

    theta1 = theta1_test(i) * pi()
    theta2 = theta2_test(i) * pi()
    theta3 = theta2 + 2.0 * pi () - ( theta2 - theta1 )

    area1 = circle_sector_area_2d ( radius, theta1, theta2 )
    area2 = circle_sector_area_2d ( radius, theta2, theta3 )
    area3 = circle_area_2d ( radius )

    write ( *, * ) ' '
    write ( *, * ) '  XC       YC       RADIUS   THETA1   THETA2   Area1   Area2  Circle'
    write ( *, * ) ' '
    write ( *, '(8f9.4)' ) xc, yc, radius, theta1, theta2, area1, area2, area3
    write ( *, * ) ' '
    write ( *, * ) '     F   Sector1       Sector2         Sum         Circle'
    write ( *, * ) ' '

    do j = 1, nfunc

      call funcset ( 'SET', j )

      call circle_sector ( funcd2, xc, yc, radius, theta1, theta2, nr, resulta )
      call circle_sector ( funcd2, xc, yc, radius, theta2, theta3, nr, resultb )

      result1 = resulta + resultb

      call circle_sum_rt ( funcd2, xc, yc, radius, nr2, ra, rw, nt, ta, tw, zw, &
        result2 )

      write ( *, '(a7,4g14.6)' ) fname(j), resulta, resultb, &
        resulta + resultb, result2

    end do

  end do

  return
end
subroutine test03
!
!*******************************************************************************
!
!! TEST03 tests CIRCLE_SET_RT.
!! TEST03 tests CIRCLE_SUM_RT.
!
  integer, parameter :: max_r = 10
  integer, parameter :: max_rule = 9
  integer, parameter :: max_t = 20
!
  character ( len = 7 ) fname
  double precision funcd2
  integer i
  integer ihi
  integer ilo
  integer nfunc
  integer nr
  integer nt
  double precision pi
  double precision r
  double precision ra(max_r)
  double precision result(max_rule)
  double precision rw(max_r)
  integer rule
  double precision ta(max_t)
  double precision tw(max_t)
  double precision xc
  double precision yc
  double precision zw
!
  external funcd2
!
  call funcset ( 'COUNT', nfunc )

  xc = 1.0D+00
  yc = 1.0D+00
  r = 1.0D+00
  write ( *, * ) ' '
  write ( *, * ) 'TEST03'
  write ( *, * ) '  For R, Theta product rules on the unit circle,'
  write ( *, * ) '  CIRCLE_SET_RT sets a rule.'
  write ( *, * ) '  CIRCLE_SUM_RT evaluates the rule in an arbitrary circle.'
  write ( *, * ) ' '
  write ( *, * ) '  We use a radius ', r
  write ( *, * ) '  and center ', xc, ', ', yc
  write ( *, * ) ' '

  do ilo = 1, max_rule, 5

    ihi = min ( ilo +  4, max_rule )

    write ( *, * ) ' '
    write ( *, '(a7,7x,5(i7,7x))' ) 'Rule:  ', ( rule, rule = ilo, ihi )
    write ( *, * ) 'Function '

    do i = 1, nfunc

      call funcset ( 'SET', i )

      do rule = ilo, ihi

        call circle_set_rt ( rule, nr, ra, rw, nt, ta, tw, zw )

        call funcset ( 'SET', i )

        call circle_sum_rt ( funcd2, xc, yc, r, nr, ra, rw, nt, ta, tw, zw, &
          result(rule) )

      end do

      write ( *, '(a7,5f14.8)' ) fname(i), result(ilo:ihi)

    end do

  end do

  return
end
subroutine test04
!
!*******************************************************************************
!
!! TEST04 tests CIRCLE_SET_XY.
!! TEST04 tests CIRCLE_SUM_XY.
!
  integer, parameter :: max_order = 64
  integer, parameter :: max_rule = 13
!
  character ( len = 7 ) fname
  double precision funcd2
  integer i
  integer ihi
  integer ilo
  integer nfunc
  integer norder
  double precision r
  double precision result(max_rule)
  integer rule
  double precision weight(max_order)
  double precision xc
  double precision xtab(max_order)
  double precision yc
  double precision ytab(max_order)
!
  external funcd2
!
  call funcset ( 'COUNT', nfunc )

  xc = 1.0D+00
  yc = 1.0D+00
  r = 1.0D+00
  write ( *, * ) ' '
  write ( *, * ) 'TEST04'
  write ( *, * ) '  CIRCLE_SET_XY sets a quadrature rule for the unit circle.'
  write ( *, * ) '  CIRCLE_SUM_XY evaluates the quadrature rule'
  write ( *, * ) '  in an arbitrary circle.'
  write ( *, * ) ' '
  write ( *, * ) '  We use a radius ', r
  write ( *, * ) '  and center ', xc, ', ', yc
  write ( *, * ) ' '

  do ilo = 1, max_rule, 5

    ihi = min ( ilo +  4, max_rule )

    write ( *, * ) ' '
    write ( *, '(a7,7x,5(i7,7x))' ) 'Rule:  ', ( rule, rule = ilo, ihi )
    write ( *, * ) 'Function '

    do i = 1, nfunc

      call funcset ( 'SET', i )

      do rule = ilo, ihi

        call circle_set_xy ( rule, norder, xtab, ytab, weight )

        call funcset ( 'SET', i )

        call circle_sum_xy ( funcd2, xc, yc, r, norder, xtab, ytab, weight, &
          result(rule) )

      end do

      write ( *, '(a7,5f14.8)' ) fname(i), result(ilo:ihi)

    end do

  end do

  return
end
subroutine test05
!
!*******************************************************************************
!
!! TEST05 tests CONE_UNIT_3D.
!
  double precision cone_volume_3d
  character ( len = 7 ) fname
  double precision funcd3
  double precision h
  integer i
  integer nfunc
  double precision r
  double precision result
!
  external funcd3
!
  call funcset ( 'COUNT', nfunc )

  r = 1.0D+00
  h = 1.0D+00

  write ( *, * ) ' '
  write ( *, * ) 'TEST05'
  write ( *, * ) '  CONE_UNIT_3D approximates integrals in a unit cone.'
  write ( *, * ) ' '
  write ( *, * ) '  Volume = ', cone_volume_3d ( r, h )
  write ( *, * ) ' '
  write ( *, * ) '  F(X)    CONE_3D'
  write ( *, * ) ' '

  do i = 1, nfunc

    call funcset ( 'SET', i )

    call cone_unit_3d ( funcd3, result )

    write ( *, '(a7,f14.8)' ) fname(i), result

  end do

  return
end
subroutine test06
!
!*******************************************************************************
!
!! TEST06 tests CUBE_SHELL_ND.
!
  integer, parameter :: max_n = 4
!
  double precision cube_shell_volume_nd
  character ( len = 7 ) fname
  double precision funcdn
  integer i
  integer n
  integer nfunc
  double precision r1
  double precision r2
  double precision result
!
  external funcdn
!
  call funcset ( 'COUNT', nfunc )

  write ( *, * ) ' '
  write ( *, * ) ' '
  write ( *, * ) 'TEST06'
  write ( *, * ) '  CUBE_SHELL_ND approximates integrals in a'
  write ( *, * ) '  cubical shell in ND.'
  write ( *, * ) ' '
  r1 = 0.0D+00
  r2 = 1.0D+00
  write ( *, * ) ' '
  write ( *, * ) '  Inner radius = ', r1
  write ( *, * ) '  Outer radius = ', r2
  write ( *, * ) ' '
 
  do n = 2, max_n
 
    write ( *, * ) ' '
    write ( *, * ) '  Spatial dimension N = ', n
    write ( *, * ) '  Volume = ', cube_shell_volume_nd ( n, r1, r2 )
    write ( *, * ) ' '
    write ( *, * ) '  F(X)      CUBE_SHELL_ND'
    write ( *, * ) ' '
 
    do i = 1, nfunc

      call funcset ( 'SET', i )

      call cube_shell_nd ( funcdn, n, r1, r2, result )
      write ( *, '(a7,2f14.8)' ) fname(i), result

    end do

  end do
 
  r1 = 1.0D+00
  r2 = 2.0D+00
 
  write ( *, * ) ' '
  write ( *, * ) '  Inner radius = ', r1
  write ( *, * ) '  Outer radius = ', r2
  write ( *, * ) ' '
 
  do n = 2,  max_n
 
    write ( *, * ) ' '
    write ( *, * ) '  Spatial dimension N = ', n
    write ( *, * ) '  Volume = ', cube_shell_volume_nd ( n, r1, r2 )
    write ( *, * ) ' '
    write ( *, * ) '  F(X)      CUBE_SHELL_ND'
    write ( *, * ) ' '
 
    do i = 1, nfunc

      call funcset ( 'SET', i )

      call cube_shell_nd ( funcdn, n, r1, r2, result )
      write ( *, '(a7,f14.8)' ) fname(i), result

    end do
 
  end do
 
  return
end
subroutine test07
!
!*******************************************************************************
!
!! TEST07 tests CUBE_UNIT_3D.
!! TEST07 tests CUBE_UNIT_ND.
!! TEST07 tests QMULT_3D.
!! TEST07 tests RECTANGLE_3D.
!
  integer, parameter :: max_k = 10
!
  double precision a1
  double precision a(3)
  double precision b1
  double precision b(3)
  double precision fl18
  double precision fl28
  character ( len = 7 ) fname
  double precision fu18
  double precision fu28
  double precision funcd3
  integer i
  integer k
  integer, parameter :: n = 3
  integer nfunc
  double precision qa(max_k)
  double precision qb(max_k)
  double precision qmult_3d
  double precision result1
  double precision result2
  double precision result3
!
  external funcd3
  external funcdn
  external fl18
  external fl28
  external fu18
  external fu28
!
  call funcset ( 'COUNT', nfunc )
  k = max_k

  a1 = -1.0D+00
  b1 = +1.0D+00

  a(1) = -1.0D+00
  a(2) = -1.0D+00
  a(3) = -1.0D+00
  b(1) = 1.0D+00
  b(2) = 1.0D+00
  b(3) = 1.0D+00

  write ( *, * ) ' '
  write ( *, * ) ' '
  write ( *, * ) 'TEST07'
  write ( *, * ) '  CUBE_UNIT_3D approximates integrals in the unit cube.'
  write ( *, * ) '  CUBE_UNIT_ND approximates integrals in the unit cube in ND.'
  write ( *, * ) '  QMULT_3D approximates triple integrals.'
  write ( *, * ) '  RECTANGLE_3D approximates integrals in a rectangular block.'
  write ( *, * ) ' '
  write ( *, * ) '  F(X)    CUBE_UNIT_3D  CUBE_UNIT_ND  QMULT_3D      ' &
    // 'RECTANGLE_3D'
  write ( *, * ) ' '

  do i = 1, nfunc

    call funcset ( 'SET', i )

    call cube_unit_3d ( funcd3, result1 )
    call cube_unit_nd ( funcdn, qa, qb, n, k )
    result2 = qmult_3d ( funcd3, a1, b1, fu18, fl18, fu28, fl28 )
    call rectangle_3d ( funcd3, a, b, result3 )

    write ( *, '(a7,5f14.8)' ) fname(i), result1, qb(k), result2, result3

  end do

  return
end
subroutine test08
!
!*******************************************************************************
!
!! TEST08 tests CUBE_UNIT_ND.
!
  integer, parameter :: max_k = 10
  integer, parameter :: max_test = 2
!
  character ( len = 7 ) fname
  double precision funcdn
  integer i
  integer i_test
  integer k
  integer khi
  integer klo
  integer, parameter, dimension ( max_test ) :: k_test = (/ 10, 5 /)
  integer n
  integer, parameter, dimension ( max_test ) :: n_test = (/ 2, 3 /)
  integer nfunc
  double precision qa(max_k)
  double precision qb(max_k)
!
  external funcdn
!
  call funcset ( 'COUNT', nfunc )

  write ( *, * ) ' '
  write ( *, * ) ' '
  write ( *, * ) 'TEST08'
  write ( *, * ) '  CUBE_UNIT_ND approximates integrals inside the unit N-cube.'
  write ( *, * ) ' '

  do i_test = 1, max_test
 
    n = n_test(i_test)
    k = k_test(i_test)
    write ( *, * ) ' '
    write ( *, * ) ' '
    write ( *, * ) '  Spatial dimension N = ', n
    write ( *, * ) '  Value of K = ', k
    write ( *, * ) ' '
    write ( *, * ) '  F(X)    CUBE_UNIT_ND'
    write ( *, * ) ' '

    do i = 1, nfunc

      call funcset ( 'SET', i )

      call cube_unit_nd ( funcdn, qa, qb, n, k )

      do klo = 1, k, 5
        khi = min ( klo + 4, k )
        if ( klo == 1 ) then
          write ( *, '(a7,5f14.8)' ) fname(i), qa(klo:khi)
        else
          write ( *, '(7x,5f14.8)' )           qa(klo:khi)
        end if
      end do

      do klo = 1, k, 5
        khi = min ( klo + 4, k )
        write ( *, '(7x,5f14.8)' )           qb(klo:khi)
      end do
                                           
    end do

  end do

  return
end
subroutine test10
!
!*******************************************************************************
!
!! TEST10 tests HEXAGON_UNIT_SET.
!! TEST10 tests HEXAGON_SUM.
!
  integer, parameter :: max_order = 64
  integer, parameter :: max_rule = 4
!
  character ( len = 7 ) fname
  double precision funcd2
  integer i
  integer ihi
  integer ilo
  integer nfunc
  integer norder
  double precision rad
  double precision result(max_rule)
  integer rule
  double precision weight(max_order)
  double precision xc
  double precision xtab(max_order)
  double precision yc
  double precision ytab(max_order)
!
  external funcd2
!
  call funcset ( 'COUNT', nfunc )

  xc = 0.0D+00
  yc = 0.0D+00
  rad = 2.0D+00
 
  write ( *, * ) ' '
  write ( *, * ) ' '
  write ( *, * ) 'TEST10'
  write ( *, * ) '  HEXAGON_UNIT_SET sets a quadrature rule for the unit hexagon.'
  write ( *, * ) '  HEXAGON_SUM evaluates the quadrature rule'
  write ( *, * ) '  in an arbitrary hexagon.'
  write ( *, * ) ' '
  write ( *, * ) '  We use a radius ', rad
  write ( *, * ) '  and center ', xc, ', ', yc
  write ( *, * ) ' '

  do ilo = 1, max_rule, 5

    ihi = min ( ilo + 4, max_rule )

    write ( *, * ) ' '
    write ( *, * ) 'Rule:    ', ( rule, rule = ilo, ihi )
    write ( *, * ) 'Function '
    write ( *, * ) ' '

    do i = 1, nfunc

      call funcset ( 'SET', i )

      do rule = ilo, ihi

        call hexagon_unit_set ( rule, norder, xtab, ytab, weight )

        call hexagon_sum ( funcd2, xc, yc, rad, norder, xtab, ytab, weight, &
          result(rule) )

      end do

      write ( *, '(a7,5f14.8)' ) fname(i), result(ilo:ihi)

    end do

  end do

  return
end
subroutine test11
!
!*******************************************************************************
!
!! TEST11 tests OCTAHEDRON_UNIT_ND.
!
  integer, parameter :: max_n = 3
!
  character ( len = 7 ) fname
  double precision funcdn
  integer i
  integer n
  integer nfunc
  double precision result(3)
!
  external funcdn
!
  call funcset ( 'COUNT', nfunc )

  write ( *, * ) ' '
  write ( *, * ) 'TEST11'
  write ( *, * ) '  OCTAHEDRON_UNIT_ND approximates integrals in a unit' 
  write ( *, * ) '  octahedron in N dimensions.'
  write ( *, * ) ' '
  write ( *, * ) ' '
  write ( *, * ) '  F(X)    N = 1    N = 2   N = 3 '
  write ( *, * ) ' '
 
  do i = 1, nfunc

    call funcset ( 'SET', i )

    do n = 1, max_n
      call octahedron_unit_nd ( funcdn, n, result(n) )
    end do

    write ( *, '(a7,3f14.8)' ) fname(i), result(1:max_n)

  end do

  return
end
subroutine test12
!
!*******************************************************************************
!
!! TEST12 tests PARALLELIPIPED_VOLUME_ND.
!
  integer, parameter :: lda = 5
!
  integer i
  integer n
  double precision parallelipiped_volume_nd
  double precision v(lda,lda)
  double precision volume
!
  write ( *, * ) ' '
  write ( *, * ) ' '
  write ( *, * ) 'TEST12'
  write ( *, * ) '  PARALLELIPIPED_VOLUME_ND computes the volume of a'
  write ( *, * ) '  parallelipiped in N dimensions.'
  write ( *, * ) ' '

  do n = 2, 4

    write ( *, * ) ' '
    write ( *, * ) '  Spatial dimension N = ', n
!
!  Set the values of the parallelipiped.
!
    call setsim ( lda, n, v )

    write ( *, * ) ' '
    write ( *, * ) '  Parallelipiped vertices:'
    write ( *, * ) ' '

    do i = 1, n+1
      write ( *, '(4f4.0)' ) v(i,1:n)
    end do

    volume = parallelipiped_volume_nd ( lda, n, v )

    write ( *, * ) ' '
    write ( *, * ) 'Volume is ', volume

  end do

  return
end
subroutine test13
!
!*******************************************************************************
!
!! TEST13 tests PYRAMID_UNIT_3D.
!
  character ( len = 7 ) fname
  double precision funcd3
  double precision h
  integer i
  integer nfunc
  double precision pyramid_volume_3d
  double precision r
  double precision result
!
  external funcd3
!
  call funcset ( 'COUNT', nfunc )

  r = 1.0D+00
  h = 1.0D+00

  write ( *, * ) ' '
  write ( *, * ) 'TEST13'
  write ( *, * ) '  PYRAMID_UNIT_3D approximates integrals in a unit pyramid.'
  write ( *, * ) ' '
  write ( *, * ) '  Volume = ', pyramid_volume_3d ( r, h )
  write ( *, * ) ' '
  write ( *, * ) '  F(X)    PYRAMID_3D'
  write ( *, * ) ' '

  do i = 1, nfunc

    call funcset ( 'SET', i )

    call pyramid_unit_3d ( funcd3, result )

    write ( *, '(a7,f14.8)' ) fname(i), result

  end do

  return
end
subroutine test14
!
!*******************************************************************************
!
!! TEST14 tests QMULT_1D.
!
  double precision a
  double precision b
  character ( len = 7 ) fname
  double precision funcd1
  integer i
  integer nfunc
  double precision qmult_1d
  double precision result
!
  external funcd1
!
  call funcset ( 'COUNT', nfunc )

  a = -1.0D+00
  b = 1.0D+00
 
  write ( *, * ) ' '
  write ( *, * ) 'TEST14'
  write ( *, * ) '  QMULT_1D approximates an integral on a'
  write ( *, * ) '  one-dimensional interval.'
  write ( *, * ) ' '
  write ( *, * ) '  We use the interval ', a, ' to ', b
  write ( *, * ) ' '
  write ( *, * ) '  F(X)     QMULT_1D'
  write ( *, * ) ' '
 
  do i = 1, nfunc

    call funcset ( 'SET', i )

    result = qmult_1d ( funcd1, a, b )
    write ( *, '(a7,f14.8)' ) fname(i), result
 
  end do

  return
end
subroutine test15
!
!*******************************************************************************
!
!! TEST15 tests SPHERE_UNIT_SURFACE_07_3D.
!! TEST15 tests SPHERE_UNIT_SURFACE_11_3D.
!! TEST15 tests SPHERE_UNIT_SURFACE_14_3D.
!! TEST15 tests SPHERE_UNIT_SURFACE_15_3D.
!
  character ( len = 7 ) fname
  double precision funcd3
  integer i
  integer nfunc
  double precision result1
  double precision result2
  double precision result3
  double precision result4
  double precision sphere_unit_area_nd
!
  external funcd3
!
  call funcset ( 'COUNT', nfunc )

  write ( *, * ) ' '
  write ( *, * ) 'TEST15'
  write ( *, * ) '  For integrals on the surface of the unit sphere in 3D:'
  write ( *, * ) '  SPHERE_UNIT_SURFACE_07_3D uses a formula of degree 7.'
  write ( *, * ) '  SPHERE_UNIT_SURFACE_11_3D uses a formula of degree 11.'
  write ( *, * ) '  SPHERE_UNIT_SURFACE_14_3D uses a formula of degree 14.'
  write ( *, * ) '  SPHERE_UNIT_SURFACE_15_3D uses a formula of degree 15.'
  write ( *, * ) ' '
  write ( *, * ) '  Unit sphere area = ', sphere_unit_area_nd ( 3 )
  write ( *, * ) ' '
  write ( *, * ) '  F(X)    S3S07        S3S11         S3S14         S3S15      '
  write ( *, * ) ' '
 
  do i = 1, nfunc

    call funcset ( 'SET', i )

    call sphere_unit_surface_07_3d ( funcd3, result1 ) 
    call sphere_unit_surface_11_3d ( funcd3, result2 )
    call sphere_unit_surface_14_3d ( funcd3, result3 )
    call sphere_unit_surface_15_3d ( funcd3, result4 )
 
    write ( *, '(a7,4f14.8)' ) fname(i), result1, result2, result3, result4

  end do
 
  return
end
subroutine test16
!
!*******************************************************************************
!
!! TEST16 tests SPHERE_UNIT_SURFACE_3_ND.
!! TEST16 tests SPHERE_UNIT_SURFACE_4_ND.
!! TEST16 tests SPHERE_UNIT_SURFACE_5_ND.
!! TEST16 tests SPHERE_UNIT_SURFACE_7_1_ND.
!! TEST16 tests SPHERE_UNIT_SURFACE_7_2_ND.
!
  character ( len = 7 ) fname
  double precision funcdn
  integer i
  integer n
  integer nfunc
  double precision result1
  double precision result2
  double precision result3
  double precision result4
  double precision result5
  double precision sphere_unit_area_nd
!
  external funcdn
!
  call funcset ( 'COUNT', nfunc )

  write ( *, * ) ' '
  write ( *, * ) 'TEST16'
  write ( *, * ) '  For integrals on the surface of the unit sphere in ND:'
  write ( *, * ) '  SPHERE_UNIT_SURFACE_3_ND uses a formula of degree 3;'
  write ( *, * ) '  SPHERE_UNIT_SURFACE_4_ND uses a formula of degree 4;'
  write ( *, * ) '  SPHERE_UNIT_SURFACE_5_ND uses a formula of degree 5.'
  write ( *, * ) '  SPHERE_UNIT_SURFACE_7_1_ND uses a formula of degree 7.'
  write ( *, * ) '  SPHERE_UNIT_SURFACE_7_2_ND uses a formula of degree 7.'
  write ( *, * ) ' '
 
  do n = 2, 4
 
    write ( *, * ) ' '
    write ( *, * ) '  Spatial dimension N = ', n
    write ( *, * ) '  Unit sphere area = ', sphere_unit_area_nd ( n )
    write ( *, * ) ' '
    write ( *, * ) '  Rule:     #3            #4            #5           #7.1' // &
      '      #7.2'
    write ( *, * ) '  Function'
    write ( *, * ) ' '
 
    do i = 1, nfunc

      call funcset ( 'SET', i )
      call sphere_unit_surface_3_nd ( funcdn, n, result1 )
      call sphere_unit_surface_4_nd ( funcdn, n, result2 )
      call sphere_unit_surface_5_nd ( funcdn, n, result3 )
      call sphere_unit_surface_7_1_nd ( funcdn, n, result4 )
      call sphere_unit_surface_7_2_nd ( funcdn, n, result5 )

      write ( *, '(a7,5f14.8)' ) fname(i), result1, result2, result3, result4, &
        result5

    end do
 
  end do
 
  return
end
subroutine test165
!
!*******************************************************************************
!
!! TEST165 tests SPHERE_SURFACE_5_ND.
!! TEST165 tests SPHERE_SURFACE_7_1_ND.
!
  integer, parameter :: max_n = 5
!
  character ( len = 7 ) fname
  double precision funcdn
  integer i
  integer n
  integer nfunc
  double precision r
  double precision result1
  double precision result2
  double precision sphere_area_nd
  double precision xc(max_n)
!
  external funcdn
!
  call funcset ( 'COUNT', nfunc )

  write ( *, * ) ' '
  write ( *, * ) 'TEST165'
  write ( *, * ) '  For integrals on the surface of a sphere in ND:'
  write ( *, * ) '  SPHERE_SURFACE_5_ND uses a formula of degree 5.'
  write ( *, * ) '  SPHERE_SURFACE_7_1_ND uses a formula of degree 7.'
  write ( *, * ) ' '
  r = 2.0D+00
  xc(1:max_n) = 1.0D+00

  do n = 2, 4

    write ( *, * ) ' '
    write ( *, * ) '  Spatial dimension N = ', n
    write ( *, * ) '  Sphere center = '
    write ( *, * ) xc(1:n)
    write ( *, * ) '  Sphere radius = ', r
    write ( *, * ) '  Sphere area = ', sphere_area_nd ( n, r )
    write ( *, * ) ' '
    write ( *, * ) '  Rule:     #5           #7.1'
    write ( *, * ) '  Function'
    write ( *, * ) ' '

    do i = 1, nfunc

      call funcset ( 'SET', i )

      call sphere_surface_5_nd ( funcdn, n, xc, r, result1 )
      call sphere_surface_7_1_nd ( funcdn, n, xc, r, result2 )

      write ( *, '(a7,2f14.8)' ) fname(i), result1, result2

    end do

  end do

  return
end
subroutine test17
!
!*******************************************************************************
!
!! TEST17 tests SIMPLEX_ND.
!
  integer, parameter :: lda = 5
!
  character ( len = 7 ) fname
  double precision funcdn
  integer i
  integer j
  integer n
  integer nfunc
  double precision result
  double precision v(lda,lda)
!
  external funcdn
!
  call funcset ( 'COUNT', nfunc )

  write ( *, * ) ' '
  write ( *, * ) ' '
  write ( *, * ) 'TEST17'
  write ( *, * ) '  SIMPLEX_ND approximates integrals inside an' 
  write ( *, * ) '  arbitrary simplex in ND.'
  write ( *, * ) ' '
 
  do n = 2, 4
 
    write ( *, * ) ' '
    write ( *, * ) '  Spatial dimension N = ', n
!
!  Restore values of simplex.
!
    call setsim ( lda, n, v )

    write ( *, * ) ' '
    write ( *, * ) '  Simplex vertices:'
    write ( *, * ) ' '
 
    do i = 1, n+1
      write ( *, '(4f4.0)' ) v(i,1:n)
    end do
 
    write ( *, * ) ' '
    write ( *, * ) 'F(X)    SIMPLEX_ND'
    write ( *, * ) ' '

    do i = 1, nfunc

      call funcset ( 'SET', i )

      call simplex_nd ( funcdn, lda, n, v, result )
      write ( *, '(a7,f14.8)' ) fname(i), result
 
      call setsim ( lda, n, v )

    end do

  end do
 
  return
end
subroutine test18
!
!*******************************************************************************
!
!! TEST18 tests SIMPLEX_VOLUME_ND.
!
  integer, parameter :: lda = 5
!
  integer i
  integer n
  double precision simplex_volume_nd
  double precision v(lda,lda)
  double precision volume
!
  write ( *, * ) ' '
  write ( *, * ) ' '
  write ( *, * ) 'TEST18'
  write ( *, * ) '  SIMPLEX_VOLUME_ND computes the volume of a simplex'
  write ( *, * ) '  in N dimensions.'
  write ( *, * ) ' '

  do n = 2, 4

    write ( *, * ) ' '
    write ( *, * ) '  Spatial dimension N = ', n
!
!  Set the values of the simplex.
!
    call setsim ( lda, n, v )

    write ( *, * ) ' '
    write ( *, * ) '  Simplex vertices:'
    write ( *, * ) ' '

    do i = 1, n+1
      write ( *, '(4f4.0)' ) v(i,1:n)
    end do

    volume = simplex_volume_nd ( lda, n, v )

    write ( *, * ) ' '
    write ( *, * ) 'Volume is ', volume

  end do

  return
end
subroutine test19
!
!*******************************************************************************
!
!! TEST19 tests SPHERE_UNIT_07_3D.
!! TEST19 tests SPHERE_UNIT_14_3D.
!! TEST19 tests SPHERE_UNIT_15_3D.
!
  character ( len = 7 ) fname
  double precision funcd3
  integer i
  integer nfunc
  double precision pi
  double precision result1
  double precision result2
  double precision result3
  double precision sphere_unit_volume_nd
!
  external funcd3
!
  call funcset ( 'COUNT', nfunc )

  write ( *, * ) ' '
  write ( *, * ) ' '
  write ( *, * ) 'TEST19'
  write ( *, * ) '  For integrals inside the unit 3 sphere:'
  write ( *, * ) '  SPHERE_UNIT_07_3D uses a formula of degree 7;'
  write ( *, * ) '  SPHERE_UNIT_14_3D uses a formula of degree 14;'
  write ( *, * ) '  SPHERE_UNIT_15_3D uses a formula of degree 15.'
  write ( *, * ) ' '
  write ( *, * ) '  Unit sphere volume = ', sphere_unit_volume_nd ( 3 )
  write ( *, * ) ' '
  write ( *, * ) '  Rule:      #7             #14           #15'
  write ( *, * ) '  F(X)'
  write ( *, * ) ' '
 
  do i = 1, nfunc

    call funcset ( 'SET', i )

    call sphere_unit_07_3d ( funcd3, result1 )
    call sphere_unit_14_3d ( funcd3, result2 )
    call sphere_unit_15_3d ( funcd3, result3 )

    write ( *, '(a7,4f14.8)' ) fname(i), result1, result2, result3

  end do
 
  return
end
subroutine test09
!
!*******************************************************************************
!
!! TEST09 tests SPHERE_UNIT_F1_ND.
!! TEST09 tests SPHERE_UNIT_F3_ND.
!
  integer, parameter :: max_n = 3
!
  character ( len = 7 ) fname
  double precision funcdn
  integer i
  integer n
  integer nfunc
  double precision result1
  double precision result2
  double precision sphere_unit_volume_nd
!
  external funcdn
!
  call funcset ( 'COUNT', nfunc )

  write ( *, * ) ' '
  write ( *, * ) 'TEST09'
  write ( *, * ) '  For integrals inside the unit sphere in ND:'
  write ( *, * ) '  SPHERE_UNIT_F1_ND approximates the integral;'
  write ( *, * ) '  SPHERE_UNIT_F3_ND approximates the integral.'
  write ( *, * ) ' '
 
  do n = 2, max_n
 
    write ( *, * ) ' '
    write ( *, * ) '  Spatial dimension N = ', n
    write ( *, * ) '  Unit sphere volume = ', sphere_unit_volume_nd ( n )
    write ( *, * ) ' '
    write ( *, * ) ' '
    write ( *, * ) '  Rule:      F1          F3'
    write ( *, * ) '  F(X)'
    write ( *, * ) ' '
 
    do i = 1, nfunc

      call funcset ( 'SET', i )

      call sphere_unit_f1_nd ( funcdn, n, result1 )
      call sphere_unit_f3_nd ( funcdn, n, result2 )
      write ( *, '(a7,2f14.8)' ) fname(i), result1, result2
 
    end do
 
  end do
 
  return
end
subroutine test095
!
!*******************************************************************************
!
!! TEST095 tests SPHERE_F1_ND.
!! TEST095 tests SPHERE_F3_ND.
!
  integer, parameter :: max_n = 3
!
  character ( len = 7 ) fname
  double precision funcdn
  integer i
  integer n
  integer nfunc
  double precision result1
  double precision result2
  double precision r
  double precision sphere_volume_nd
  double precision xc(max_n)
!
  external funcdn
!
  call funcset ( 'COUNT', nfunc )

  r = 2.0D+00
  xc(1:max_n) = (/ 1.0D+00, -1.0D+00, 2.0D+00 /)

  write ( *, * ) ' '
  write ( *, * ) 'TEST095'
  write ( *, * ) '  For integrals inside a sphere in ND:'
  write ( *, * ) '  SPHERE_F1_ND approximates the integral;'
  write ( *, * ) '  SPHERE_F3_ND approximates the integral.'
  write ( *, * ) ' '

  do n = 2, max_n

    write ( *, * ) ' '
    write ( *, * ) '  Spatial dimension N = ', n
    write ( *, * ) '  Sphere center:'
    write ( *, '(3f10.4)' ) xc(1:n)
    write ( *, * ) '  Sphere radius = ', r
    write ( *, * ) '  Sphere volume = ', sphere_volume_nd ( n, r )
    write ( *, * ) ' '
    write ( *, * ) ' '
    write ( *, * ) '  Rule:      F1          F3'
    write ( *, * ) '  F(X)'
    write ( *, * ) ' '

    do i = 1, nfunc

      call funcset ( 'SET', i )

      call sphere_f1_nd ( funcdn, n, xc, r, result1 )
      call sphere_f3_nd ( funcdn, n, xc, r, result2 )
      write ( *, '(a7,2f14.8)' ) fname(i), result1, result2

    end do

  end do

  return
end
subroutine test096
!
!*******************************************************************************
!
!! TEST096 tests SPHERE_F1_ND.
!! TEST096 tests SPHERE_F3_ND.
!! TEST096 tests SPHERE_SHELL_03_ND.
!! TEST096 tests SPHERE_SHELL_07_ND.
!
  integer, parameter :: max_n = 3
!
  character ( len = 7 ) fname
  double precision funcdn
  integer i
  integer j
  integer n
  integer nfunc
  double precision result1
  double precision result2
  double precision result3
  double precision result4
  double precision result5
  double precision result6
  double precision r1
  double precision r2
  double precision sphere_shell_volume_nd
  double precision xc(max_n)
!
  external funcdn
!
  call funcset ( 'COUNT', nfunc )

  write ( *, * ) ' '
  write ( *, * ) 'TEST096'
  write ( *, * ) '  For integrals inside a spherical shell in ND:'
  write ( *, * ) '  SPHERE_SHELL_03_ND approximates the integral.'
  write ( *, * ) '  SPHERE_SHELL_07_ND approximates the integral.'
  write ( *, * ) ' '
  write ( *, * ) '  We compare these results with those computed by'
  write ( *, * ) '  from the difference of two spherical integrals.'
  write ( *, * ) '  SPHERE_F1_ND approximates the integral;'
  write ( *, * ) '  SPHERE_F3_ND approximates the integral.'
  write ( *, * ) ' '

  do j = 1, 2

    if ( j == 1 ) then
      r1 = 0.0D+00
      r2 = 1.0D+00
      xc(1:max_n) = 0.0D+00
    else
      r1 = 2.0D+00
      r2 = 3.0D+00
      xc(1:max_n) = (/ 1.0D+00, -1.0D+00, 2.0D+00 /)
    end if

    do n = 2, max_n

      write ( *, * ) ' '
      write ( *, * ) '  Spatial dimension N = ', n
      write ( *, * ) '  Sphere center:'
      write ( *, '(3f10.4)' ) xc(1:n)
      write ( *, * ) '  Inner sphere radius = ', r1
      write ( *, * ) '  Outer sphere radius = ', r2
      write ( *, * ) '  Spherical shell volume = ', &
        sphere_shell_volume_nd ( n, r1, r2 )
      write ( *, * ) ' '
      write ( *, * ) ' '
      write ( *, * ) '  Rule:      #3       #7    F1(R2)-F1(R1)  ' // &      
        'F3(R2)-F3(R1)'
      write ( *, * ) '  F(X)'
      write ( *, * ) ' '

      do i = 1, nfunc

        call funcset ( 'SET', i )

        call sphere_shell_03_nd ( funcdn, n, xc, r1, r2, result1 )
        call sphere_shell_07_nd ( funcdn, n, xc, r1, r2, result2 )

        call sphere_f1_nd ( funcdn, n, xc, r1, result3 )
        call sphere_f1_nd ( funcdn, n, xc, r2, result4 )
      
        call sphere_f3_nd ( funcdn, n, xc, r1, result5 )
        call sphere_f3_nd ( funcdn, n, xc, r2, result6 )

        write ( *, '(a7,4f14.8)' ) fname(i), result1, result2, result4-result3, &
          result6-result5

      end do

    end do

  end do

  return
end
subroutine test20
!
!*******************************************************************************
!
!! TEST20 tests SPHERE_UNIT_AREA_ND.
!! TEST20 tests SPHERE_UNIT_VOLUME_ND.
!
  double precision a
  integer n
  double precision sphere_unit_area_nd
  double precision sphere_unit_volume_nd
  double precision v
!
  write ( *, * ) ' '
  write ( *, * ) 'TEST20'
  write ( *, * ) '  For a unit sphere in N dimensions:'
  write ( *, * ) '  SPHERE_UNIT_AREA_ND computes the area;'
  write ( *, * ) '  SPHERE_UNIT_VOLUME_ND computes the volume.'
  write ( *, * ) ' '
  write ( *, * ) '  N    Area    Volume'
  write ( *, * ) ' '

  do n = 2, 10
    a = sphere_unit_area_nd ( n )
    v = sphere_unit_volume_nd ( n )
    write ( *, '(i3,2f14.8)' ) n, a, v
  end do

  return
end
subroutine test21
!
!*******************************************************************************
!
!! TEST21 tests SQUARE_UNIT_SET.
!! TEST21 tests RECTANGLE_SUB_2D.
!
  integer, parameter :: max_order = 16
!
  character ( len = 7 ) fname
  double precision funcd2
  integer i
  integer j
  integer n
  integer nfunc
  integer norder
  integer nsub(2)
  double precision result
  integer rule
  double precision weight(max_order)
  double precision xtab(max_order)
  double precision xval(2)
  double precision ytab(max_order)
  double precision yval(2)
!
  external funcd2
!
  call funcset ( 'COUNT', nfunc )

  write ( *, * ) ' '
  write ( *, * ) 'TEST21'
  write ( *, * ) '  SQUARE_UNIT_SET sets up a quadrature rule on a unit square.'
  write ( *, * ) '  RECTANGLE_SUB_2D applies it to subrectangles of an'
  write ( *, * ) '  arbitrary rectangle.'
  write ( *, * ) ' '
!
!  Set the location of the square.
!
  xval(1) = 1.0D+00
  yval(1) = 2.0D+00

  xval(2) = 3.0D+00
  yval(2) = 3.0D+00

  write ( *, * ) ' '
  write ( *, * ) '  The corners of the rectangle are:'
  write ( *, * ) ' '
  write ( *, * ) xval(1), yval(1)
  write ( *, * ) xval(2), yval(2)
!
!  Get the quadrature abscissas and weights for a unit square.
!
  rule = 2
  call square_unit_set ( rule, norder, xtab, ytab, weight )

  write ( *, * ) ' '
  write ( *, * ) '  Using unit square integration rule number ', rule
!
!  Set the function.
!
  do i = 1, nfunc

    call funcset ( 'SET', i )
!
!  Try an increasing number of subdivisions.
!
    write ( *, * ) ' '
    write ( *, * ) '  Function  Subdivisions  Integral'
    write ( *, * ) ' '

    do j = 1, 5

      nsub(1) = j
      nsub(2) = 2 * j

      call rectangle_sub_2d ( funcd2, xval, yval, nsub, norder, xtab, ytab, &
        weight, result )

      write ( *, '(a7,2i4, f14.8)' ) fname(i), nsub(1), nsub(2), result

    end do

  end do

  return
end
subroutine test22
!
!*******************************************************************************
!
!! TEST22 tests SQUARE_UNIT_SET.
!! TEST22 tests SQUARE_SUM.
!
  integer, parameter :: max_order = 64
  integer, parameter :: max_rule = 6
!
  character ( len = 7 ) fname
  double precision funcd2
  integer i
  integer ihi
  integer ilo
  integer nfunc
  integer norder
  double precision r
  double precision result(max_rule)
  integer rule
  double precision weight(max_order)
  double precision xc
  double precision xtab(max_order)
  double precision yc
  double precision ytab(max_order)
!
  external funcd2
!
  call funcset ( 'COUNT', nfunc )

  xc = 2.0D+00
  yc = 2.0D+00
  r = 3.0D+00

  write ( *, * ) ' '
  write ( *, * ) 'TEST22'
  write ( *, * ) '  SQUARE_UNIT_SET sets up quadrature on the unit square;'
  write ( *, * ) '  SQUARE_SUM carries it out on an arbitrary square.'
  write ( *, * ) ' '
  write ( *, * ) '  Square center is ', xc, yc
  write ( *, * ) '  Square radius is ', r

  do ilo = 1, max_rule, 5

    ihi = min ( ilo + 4, max_rule )

    write ( *, * ) ' '
    write ( *, * ) 'Rule:    ', ( rule, rule = ilo, ihi )
    write ( *, * ) 'Function '
    write ( *, * ) ' '

    do i = 1, nfunc

      call funcset ( 'SET', i )

      do rule = ilo, ihi

        call square_unit_set ( rule, norder, xtab, ytab, weight )

        call square_sum ( funcd2, xc, yc, r, norder, xtab, ytab, weight, &
          result(rule) )

      end do

      write ( *, '(a7,5f14.8)' ) fname(i), result(ilo:ihi)

    end do

  end do
 
  return
end
subroutine test23
!
!*******************************************************************************
!
!! TEST23 tests SQUARE_UNIT_SET.
!! TEST23 tests SQUARE_UNIT_SUM.
!
  integer, parameter :: max_order = 64
  integer, parameter :: max_rule = 6
!
  character ( len = 7 ) fname
  double precision funcd2
  integer i
  integer ihi
  integer ilo
  integer nfunc
  integer norder
  double precision result(max_rule)
  integer rule
  double precision weight(max_order)
  double precision xtab(max_order)
  double precision ytab(max_order)
!
  external funcd2
!
  call funcset ( 'COUNT', nfunc )

  write ( *, * ) ' '
  write ( *, * ) 'TEST23'
  write ( *, * ) '  SQUARE_UNIT_SET sets up quadrature on the unit square;'
  write ( *, * ) '  SQUARE_UNIT_SUM carries it out on the unit square.'
  write ( *, * ) ' '
 
  do ilo = 1, max_rule, 5

    ihi = min ( ilo + 4, max_rule )

    write ( *, * ) ' '
    write ( *, * ) 'Rule:    ', ( rule, rule = ilo, ihi )
    write ( *, * ) 'Function '
    write ( *, * ) ' '

    do i = 1, nfunc

      call funcset ( 'SET', i )

      do rule = ilo, ihi

        call square_unit_set ( rule, norder, xtab, ytab, weight )

        call square_unit_sum ( funcd2, norder, xtab, ytab, weight, &
          result(rule) )

      end do

      write ( *, '(a7,5f14.8)' ) fname(i), result(ilo:ihi)

    end do

  end do
 
  return
end
subroutine test24
!
!*******************************************************************************
!
!! TEST24 tests TETRA_07.
!! TEST24 tests TETRA_TPRODUCT.
!
  integer, parameter :: max_order = 9
!
  character ( len = 7 ) fname
  double precision funcd3
  integer i
  integer nfunc
  integer norder
  double precision result2
  double precision result3(max_order)
  double precision tetra_unit_volume
  double precision tetra_volume
  double precision x(4)
  double precision y(4)
  double precision z(4)
!
  external funcd3
!
  call funcset ( 'COUNT', nfunc )

  x(1:4) = (/ 1.0D+00, 4.0D+00, 1.0D+00, 1.0D+00 /)
  y(1:4) = (/ 2.0D+00, 2.0D+00, 3.0D+00, 2.0D+00 /)
  z(1:4) = (/ 6.0D+00, 6.0D+00, 6.0D+00, 8.0D+00 /)

  write ( *, * ) ' '
  write ( *, * ) 'TEST24'
  write ( *, * ) '  For integrals inside an arbitrary tetrahedron:'
  write ( *, * ) '  TETRA_07 uses a formula of degree 7;'
  write ( *, * ) '  TETRA_TPRODUCT uses a triangular product formula '
  write ( *, * ) '    of varying degree.'
  write ( *, * ) ' '
  write ( *, * ) '  Tetrahedron vertices:'
  write ( *, * ) ' '
  do i = 1, 4
    write ( *, '(3f4.0)' ) x(i), y(i), z(i)
  end do
  write ( *, * ) ' '
  write ( *, * ) '  Tetrahedron unit volume = ', tetra_unit_volume ( )
  write ( *, * ) '  Tetrahedron Volume = ', tetra_volume ( x, y, z )
  write ( *, * ) ' '
  write ( *, * ) ' '
  write ( *, * ) '  F(X)    TETRA_07'
  write ( *, * ) '          TETRA_TPRODUCT(1:4)'
  write ( *, * ) '          TETRA_TPRODUCT(5:8)'
  write ( *, * ) '          TETRA_TPRODUCT(9)'
  write ( *, * ) ' '

  do i = 1, nfunc

    call funcset ( 'SET', i )

    call tetra_07 ( funcd3, x, y, z, result2 )

    do norder = 1, max_order
      call tetra_tproduct ( funcd3, norder, x, y, z, result3(norder) )
    end do

    write ( *, * ) ' '
    write ( *, '(a7,4f16.10)' ) fname(i), result2
    write ( *, '(7x,4f16.10)' )           result3(1:4)
    write ( *, '(7x,4f16.10)' )           result3(5:8)
    write ( *, '(7x,4f16.10)' )           result3(9)

  end do

  return
end
subroutine test25
!
!*******************************************************************************
!
!! TEST25 tests TETRA_UNIT_SET.
!! TEST25 tests TETRA_UNIT_SUM.
!
  integer, parameter :: max_order = 64
  integer, parameter :: max_rule = 7
!
  character ( len = 7 ) fname
  double precision funcd3
  integer i
  integer ihi
  integer ilo
  integer nfunc
  integer norder
  double precision result(max_rule)
  integer rule
  double precision weight(max_order)
  double precision xtab(max_order)
  double precision ytab(max_order)
  double precision ztab(max_order)
!
  external funcd3
!
  call funcset ( 'COUNT', nfunc )

  write ( *, * ) ' '
  write ( *, * ) 'TEST25'
  write ( *, * ) '  TETRA_UNIT_SET sets quadrature rules for the unit tetrahedron;'
  write ( *, * ) '  TETRA_UNIT_SUM applies them to the unit tetrahedron.'
  write ( *, * ) ' '

  do ilo = 1, max_rule, 5

    ihi = min ( ilo +  4, max_rule )

    write ( *, * ) ' '
    write ( *, * ) 'Rule:   ', ( rule, rule = ilo, ihi )
    write ( *, * ) 'Function'
    write ( *, * ) ' '

    do i = 1, nfunc

      call funcset ( 'SET', i )

      do rule = ilo, ihi

        call tetra_unit_set ( rule, norder, xtab, ytab, ztab, weight )
 
        call tetra_unit_sum ( funcd3, norder, xtab, ytab, ztab, weight, &
          result(rule) )

      end do

      write ( *, '(a7,5f14.8)' ) fname(i), result(ilo:ihi)

    end do

  end do

  return
end
subroutine test26
!
!*******************************************************************************
!
!! TEST26 tests TETRA_UNIT_SET.
!! TEST26 tests TETRA_SUM.
!
  integer, parameter :: max_order = 64
  integer, parameter :: max_rule = 7
!
  character ( len = 7 ) fname
  double precision funcd3
  integer i
  integer ihi
  integer ilo
  integer nfunc
  integer norder
  double precision result(max_rule)
  integer rule
  double precision weight(max_order)
  double precision x(4)
  double precision xtab(max_order)
  double precision y(4)
  double precision ytab(max_order)
  double precision z(4)
  double precision ztab(max_order)
!
  external funcd3
!
  call funcset ( 'COUNT', nfunc )

  x(1:4) = (/ 1.0D+00, 4.0D+00, 1.0D+00, 1.0D+00 /)
  y(1:4) = (/ 2.0D+00, 2.0D+00, 3.0D+00, 2.0D+00 /)
  z(1:4) = (/ 6.0D+00, 6.0D+00, 6.0D+00, 8.0D+00 /)

  write ( *, * ) ' '
  write ( *, * ) 'TEST26'
  write ( *, * ) '  TETRA_UNIT_SET sets quadrature rules for the unit tetrahedron;'
  write ( *, * ) '  TETRA_SUM applies them to an arbitrary tetrahedron.'
  write ( *, * ) ' '
  write ( *, * ) ' '
  write ( *, * ) '  Tetrahedron vertices:'
  write ( *, * ) ' '
  do i = 1, 4
    write ( *, '(3f4.0)' ) x(i), y(i), z(i)
  end do

  do ilo = 1, max_rule, 5

    ihi = min ( ilo +  4, max_rule )

    write ( *, * ) ' '
    write ( *, * ) 'Rule:    ', ( rule, rule = ilo, ihi )
    write ( *, * ) 'Function '
    write ( *, * ) ' '

    do i = 1, nfunc

      call funcset ( 'SET', i )

      do rule = ilo, ihi

        call tetra_unit_set ( rule, norder, xtab, ytab, ztab, weight )
 
        call tetra_sum ( funcd3, x, y, z, norder, xtab, ytab, ztab, weight, &
          result(rule) )

      end do

      write ( *, '(a7,5g14.7)' ) fname(i), result(ilo:ihi)

    end do

  end do

  return
end
subroutine test27
!
!*******************************************************************************
!
!! TEST27 tests TRIANGLE_UNIT_SET.
!! TEST27 tests TRIANGLE_SUB.
!
!  Break up the triangle into NSUB**2 equal subtriangles.  Approximate 
!  the integral over the triangle by the sum of the integrals over each
!  subtriangle.
!
  integer, parameter :: max_order = 16
!
  character ( len = 7 ) fname
  double precision funcd2
  integer i
  integer n
  integer nfunc
  integer norder
  integer nsub
  double precision result
  integer rule
  double precision weight(max_order)
  double precision xtab(max_order)
  double precision xval(3)
  double precision ytab(max_order)
  double precision yval(3)
!
  external funcd2
!
  call funcset ( 'COUNT', nfunc )

  write ( *, * ) ' '
  write ( *, * ) 'TEST27'
  write ( *, * ) '  TRIANGLE_UNIT_SET sets up a quadrature rule on a triangle.'
  write ( *, * ) '  TRIANGLE_SUB applies it to subtriangles of an'
  write ( *, * ) '  arbitrary triangle.'
  write ( *, * ) ' '
!
!  Set the location of the triangle.
!
  xval(1) = 0.0D+00
  yval(1) = 0.0D+00

  xval(2) = 0.0D+00
  yval(2) = 1.0D+00

  xval(3) = 1.0D+00
  yval(3) = 0.0D+00

  write ( *, * ) ' '
  write ( *, * ) 'Triangle vertices:'
  write ( *, * ) ' '
  write ( *, * ) xval(1), yval(1)
  write ( *, * ) xval(2), yval(2)
  write ( *, * ) xval(3), yval(3)
!
!  Get the quadrature abscissas and weights for a unit triangle.
!
  rule = 3
  call triangle_unit_set ( rule, norder, xtab, ytab, weight )
  write ( *, * ) ' '
  write ( *, * ) '  Using unit triangle quadrature rule ', rule
!
!  Set the function.
!
  do i = 1, nfunc

    call funcset ( 'SET', i )
!
!  Try an increasing number of subdivisions.
!
    do nsub = 1, 5

      call triangle_sub ( funcd2, xval, yval, nsub, norder, xtab, ytab,  &
        weight, result )

      write ( *, '(a7,i4, f14.8)' ) fname(i), nsub, result
 
    end do
  
  end do

  return
end
subroutine test28
!
!*******************************************************************************
!
!! TEST28 tests TRIANGLE_UNIT_SET.
!! TEST28 tests TRIANGLE_UNIT_SUM.
!
  integer, parameter :: max_order = 64
  integer, parameter :: max_rule = 19
!
  character ( len = 7 ) fname
  double precision funcd2
  integer i
  integer ihi
  integer ilo
  integer nfunc
  integer norder
  double precision result(max_rule)
  integer rule
  double precision weight(max_order)
  double precision xtab(max_order)
  double precision ytab(max_order)
!
  external funcd2
!
  call funcset ( 'COUNT', nfunc )

  write ( *, * ) ' '
  write ( *, * ) 'TEST28'
  write ( *, * ) '  TRIANGLE_UNIT_SET sets up quadrature in the unit triangle,'
  write ( *, * ) '  TRIANGLE_UNIT_SUM applies it.'
  write ( *, * ) ' '

  do ilo = 1, max_rule, 5

    ihi = min ( ilo +  4, max_rule )

    write ( *, * ) ' '
    write ( *, * ) 'Rule:    ', ( rule, rule = ilo, ihi )
    write ( *, * ) 'Function '
    write ( *, * ) ' '

    do i = 1, nfunc

      call funcset ( 'SET', i )

      do rule = ilo, ihi

        call triangle_unit_set ( rule, norder, xtab, ytab, weight )
 
        call triangle_unit_sum ( funcd2, norder, xtab, ytab, weight, &
          result(rule) )

      end do

      write ( *, '(a7,5f14.8)' ) fname(i), result(ilo:ihi)

    end do

  end do

  return
end
subroutine test285
!
!*******************************************************************************
!
!! TEST285 tests TRIANGLE_UNIT_PRODUCT_SET.
!! TEST285 tests TRIANGLE_UNIT_SUM.
!
  integer, parameter :: max_rule = 8
  integer, parameter :: max_order = max_rule * max_rule
!
  character ( len = 7 ) fname
  double precision funcd2
  integer i
  integer ihi
  integer ilo
  integer nfunc
  integer norder
  double precision result(max_rule)
  integer rule
  double precision weight(max_order)
  double precision xtab(max_order)
  double precision ytab(max_order)
!
  external funcd2
!
  call funcset ( 'COUNT', nfunc )

  write ( *, * ) ' '
  write ( *, * ) 'TEST285'
  write ( *, * ) '  TRIANGLE_UNIT_PRODUCT_SET sets up a product quadrature'
  write ( *, * ) '    rule in the unit triangle,'
  write ( *, * ) '  TRIANGLE_UNIT_SUM applies it.'
  write ( *, * ) ' '

  do ilo = 1, max_rule, 5

    ihi = min ( ilo +  4, max_rule )

    write ( *, * ) ' '
    write ( *, * ) 'Rule Order: ', ( rule, rule = ilo, ihi )
    write ( *, * ) 'Function '
    write ( *, * ) ' '

    do i = 1, nfunc

      call funcset ( 'SET', i )

      do rule = ilo, ihi

        call triangle_unit_product_set ( rule, norder, xtab, ytab, weight )

        call triangle_unit_sum ( funcd2, norder, xtab, ytab, weight, &
          result(rule) )

      end do

      write ( *, '(a7,5f14.8)' ) fname(i), result(ilo:ihi)

    end do

  end do

  return
end
subroutine test29
!
!*******************************************************************************
!
!! TEST29 tests TRIANGLE_UNIT_SET.
!! TEST29 tests TRIANGLE_SUM.
!
  integer, parameter :: max_order = 64
  integer, parameter :: max_rule = 19
!
  character ( len = 7 ) fname
  double precision funcd2
  integer i
  integer ihi
  integer ilo
  integer nfunc
  integer norder
  double precision result(max_rule)
  integer rule
  double precision weight(max_order)
  double precision xtab(max_order)
  double precision xval(3)
  double precision ytab(max_order)
  double precision yval(3)
!
  external funcd2
!
  call funcset ( 'COUNT', nfunc )

  write ( *, * ) ' '
  write ( *, * ) 'TEST29'
  write ( *, * ) '  TRIANGLE_UNIT_SET sets up quadrature in the unit triangle,'
  write ( *, * ) '  TRIANGLE_SUM applies it to an arbitrary triangle.'
  write ( *, * ) ' '

  xval(1) = 1.0D+00
  yval(1) = 1.0D+00

  xval(2) = 3.0D+00
  yval(2) = 1.0D+00

  xval(3) = 1.0D+00
  yval(3) = 4.0D+00

  do ilo = 1, max_rule, 5

    ihi = min ( ilo + 4, max_rule )

    write ( *, * ) ' '
    write ( *, * ) 'Rule:    ', ( rule, rule = ilo, ihi )
    write ( *, * ) 'Function '
    write ( *, * ) ' '

    do i = 1, nfunc

      call funcset ( 'SET', i )

      do rule = ilo, ihi

        call triangle_unit_set ( rule, norder, xtab, ytab, weight )
 
        call triangle_sum ( funcd2, xval, yval, norder, xtab, ytab, weight, &
          result(rule) )

      end do

      write ( *, '(a7,5f14.8)' ) fname(i), result(ilo:ihi)

    end do

  end do

  return
end
subroutine setsim ( lda, n, v )
!
!*******************************************************************************
!
!! SETSIM defines the simplex.
!
  integer lda
  integer n
!
  integer i
  double precision v(lda,n)
!
  v(1:n+1,1:n) = 0.0D+00

  do i = 1, n
    v(i,i) = 1.0D+00
  end do
 
  return
end
subroutine test30
!
!*******************************************************************************
!
!! TEST30 tests TORUS_1.
!
  character ( len = 7 ) fname
  double precision funcd3
  integer i
  integer j
  integer j2
  integer n
  integer nfunc
  double precision result(5)
  double precision r1
  double precision r2
  double precision torus_area_3d
!
  external funcd3
!
  call funcset ( 'COUNT', nfunc )

  r1 = 0.5D+00
  r2 = 1.0D+00
  n = 10
 
  write ( *, * ) ' '
  write ( *, * ) ' '
  write ( *, * ) 'TEST30'
  write ( *, * ) '  TORUS_1 approximates integrals on a torus.'
  write ( *, * ) ' '
  write ( *, * ) '  The degree N will be varied.'
  write ( *, * ) ' '
  write ( *, * ) '  Inner radius = ', r1
  write ( *, * ) '  Outer radius = ', r2
  write ( *, * ) '  Area = ', torus_area_3d ( r1, r2 )
  write ( *, * ) ' '
  write ( *, * ) '  F(X)  ', ( 2**j, j = 0, 8, 2 )
  write ( *, * ) ' '
 
  do i = 1, nfunc

    call funcset ( 'SET', i )

    do j = 1, 5

      j2 = 2 * ( j - 1 )
      n = 2**j2
      call torus_1 ( funcd3, r1, r2, n, result(j) )

    end do

    write ( *, '(a7,5f14.8)' ) fname(i), result(1:5)

  end do
 
  return
end
subroutine test31
!
!*******************************************************************************
!
!! TEST31 tests TORUS_5S2.
!! TEST31 tests TORUS_6S2.
!! TEST31 tests TORUS_14S.
!
  character ( len = 7 ) fname
  double precision funcd3
  integer i
  integer nfunc
  double precision result1
  double precision result2
  double precision result3
  double precision r1
  double precision r2
  double precision torus_volume_3d
!
  external funcd3
!
  call funcset ( 'COUNT', nfunc )

  r1 = 0.5D+00
  r2 = 1.0D+00
 
  write ( *, * ) ' '
  write ( *, * ) ' '
  write ( *, * ) 'TEST31'
  write ( *, * ) '  For the interior of a torus,'
  write ( *, * ) '  TORUS_5S2,'
  write ( *, * ) '  TORUS_6S2, and'
  write ( *, * ) '  TORUS_5S2 approximate integrals.'
  write ( *, * ) ' '
  write ( *, * ) '  Inner radius = ', r1
  write ( *, * ) '  Outer radius = ', r2
  write ( *, * ) '  Volume = ', torus_volume_3d ( r1, r2 )
  write ( *, * ) ' '
  write ( *, * ) '  Rule:        #5S2          #6S2          #14S'
  write ( *, * ) '  F(X)'
  write ( *, * ) ' '
 
  do i = 1, nfunc

    call funcset ( 'SET', i )

    call torus_5s2 ( funcd3, r1, r2, result1 )
    call torus_6s2 ( funcd3, r1, r2, result2 )
    call torus_14s ( funcd3, r1, r2, result3 )

    write ( *, '(a7,3f14.8)' ) fname(i), result1, result2, result3

  end do
 
  return
end
subroutine test32
!
!*******************************************************************************
!
!! TEST32 tests TORUS_SQUARE_5C2.
!! TEST32 tests TORUS_SQUARE_14C.
!
  character ( len = 7 ) fname
  double precision funcd3
  integer i
  integer nfunc
  double precision result1
  double precision result2
  double precision r1
  double precision r2
  double precision torus_square_volume_3d
!
  external funcd3
!
  call funcset ( 'COUNT', nfunc )

  r1 = 1.0D+00
  r2 = 0.125D+00
 
  write ( *, * ) ' '
  write ( *, * ) ' '
  write ( *, * ) 'TEST32'
  write ( *, * ) '  For integrals inside a torus with square cross-section:'
  write ( *, * ) '  TORUS_SQUARE_5C2 approximates the integral;'
  write ( *, * ) '  TORUS_SQUARE_14C approximates the integral.'
  write ( *, * ) ' '
  write ( *, * ) '  Inner radius = ', r1
  write ( *, * ) '  Outer radius = ', r2
  write ( *, * ) '  Volume = ', torus_square_volume_3d ( r1, r2 )
  write ( *, * ) ' '
  write ( *, * ) '  F(X)    5C2           14C'
  write ( *, * ) ' '
 
  do i = 1, nfunc

    call funcset ( 'SET', i )

    call torus_square_5c2 ( funcd3, r1, r2, result1 )
    call torus_square_14c ( funcd3, r1, r2, result2 )

    write ( *, '(a7,2f14.8)' ) fname(i), result1, result2

  end do
 
  return
end
subroutine test33
!
!*******************************************************************************
!
!! TEST33 tests TVEC_EVEN.
!! TEST33 tests TVEC_EVEN2.
!! TEST33 tests TVEC_EVEN3.
!! TEST33 tests TVEC_EVEN_BRACKET.
!! TEST33 tests TVEC_EVEN_BRACKET2.
!! TEST33 tests TVEC_EVEN_BRACKET3.
!
  integer, parameter :: maxt = 5
!
  integer i
  integer nt
  double precision t(maxt)
  double precision theta1
  double precision theta2
!
  write ( *, * ) ' '
  write ( *, * ) 'TEST33'
  write ( *, * ) '  For evenly spaced angles between 0 and 2*PI:'
  write ( *, * ) '  TVEC_EVEN'
  write ( *, * ) '  TVEC_EVEN2'
  write ( *, * ) '  TVEC_EVEN3'
  write ( *, * ) '  For evenly spaced angles between THETA1 and THETA2:'
  write ( *, * ) '  TVEC_EVEN_BRACKET'
  write ( *, * ) '  TVEC_EVEN_BRACKET2.'
  write ( *, * ) '  TVEC_EVEN_BRACKET3.'
  write ( *, * ) ' '

  nt = 4

  call tvec_even ( nt, t )

  write ( *, * ) ' '
  write ( *, * ) '  TVEC_EVEN'
  write ( *, * ) '    NT = ', nt
  do i = 1, nt
    write ( *, * ) t(i)
  end do

  nt = 4

  call tvec_even2 ( nt, t )

  write ( *, * ) ' '
  write ( *, * ) '  TVEC_EVEN2'
  write ( *, * ) '    NT = ', nt
  do i = 1, nt
    write ( *, * ) t(i)
  end do

  nt = 4

  call tvec_even3 ( nt, t )

  write ( *, * ) ' '
  write ( *, * ) '  TVEC_EVEN3'
  write ( *, * ) '    NT = ', nt
  do i = 1, nt
    write ( *, * ) t(i)
  end do

  nt = 4
  theta1 = 30.0D+00
  theta2 = 90.0D+00

  call tvec_even_bracket ( theta1, theta2, nt, t )

  write ( *, * ) ' '
  write ( *, * ) '  TVEC_EVEN_BRACKET'
  write ( *, * ) '    NT = ', nt
  write ( *, * ) '    THETA1 = ', theta1
  write ( *, * ) '    THETA2 = ', theta2
  do i = 1, nt
    write ( *, * ) t(i)
  end do

  nt = 5

  call tvec_even_bracket2 ( theta1, theta2, nt, t )

  write ( *, * ) ' '
  write ( *, * ) '  TVEC_EVEN_BRACKET2'
  write ( *, * ) '    NT = ', nt
  write ( *, * ) '    THETA1 = ', theta1
  write ( *, * ) '    THETA2 = ', theta2
  do i = 1, nt
    write ( *, * ) t(i)
  end do

  nt = 3

  call tvec_even_bracket3 ( theta1, theta2, nt, t )

  write ( *, * ) ' '
  write ( *, * ) '  TVEC_EVEN_BRACKET3'
  write ( *, * ) '    NT = ', nt
  write ( *, * ) '    THETA1 = ', theta1
  write ( *, * ) '    THETA2 = ', theta2
  do i = 1, nt
    write ( *, * ) t(i)
  end do

  return
end
function funcd1 ( x )
!
!*******************************************************************************
!
!! FUNCD1 evaluates a function F(X) of one variable.
!
!
!  Discussion:
!
!    The actual form of the function can be determined by calling FUNCSET.
!
!  Modified:
!
!    21 January 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, double precision X, the value of the variable.
!
!    Output, double precision FUNCD1, the value of the function.
!
  double precision funcd1
  integer ifunc
  double precision x
!
  call funcset ( 'GET', ifunc )

  if ( ifunc == 1 ) then
    funcd1 = 1.0D+00
  else if ( ifunc == 2 ) then
    funcd1 = x
  else if ( ifunc == 3 ) then
    funcd1 = x**2
  else if ( ifunc == 4 ) then
    funcd1 = x**3
  else if ( ifunc == 5 ) then
    funcd1 = x**4
  else if ( ifunc == 6 ) then
    funcd1 = x**5
  else if ( ifunc == 7 ) then
    funcd1 = x**6
  else if ( ifunc == 8 ) then
    funcd1 = abs ( x )
  else if ( ifunc == 9 ) then
    funcd1 = sin ( x )
  else if ( ifunc == 10 ) then
    funcd1 = exp ( x )
  else if ( ifunc == 11 ) then
    funcd1 = 1.0D+00 / ( 1.0D+00 + abs ( x ) )
  else if ( ifunc == 12 ) then
    funcd1 = sqrt ( abs ( x ) )
  else
    funcd1 = 0.0D+00
  end if

  return
end
function funcd2 ( x, y )
!
!*******************************************************************************
!
!! FUNCD2 evaluates a function F(X,Y) of two variables.
!
!
!  Discussion:
!
!    The actual form of the function can be determined by calling FUNCSET.
!
!  Modified:
!
!    21 January 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, double precision X, Y, the value of the variables.
!
!    Output, double precision FUNCD2, the value of the function.
!
  double precision funcd2
  integer ifunc
  double precision x
  double precision y
!
  call funcset ( 'GET', ifunc )

  if ( ifunc == 1 ) then
    funcd2 = 1.0D+00
  else if ( ifunc == 2 ) then
    funcd2 = x
  else if ( ifunc == 3 ) then
    funcd2 = x**2
  else if ( ifunc == 4 ) then
    funcd2 = x**3
  else if ( ifunc == 5 ) then
    funcd2 = x**4
  else if ( ifunc == 6 ) then
    funcd2 = x**5
  else if ( ifunc == 7 ) then
    funcd2 = x**6
  else if ( ifunc == 8 ) then
    funcd2 = sqrt ( x**2 + y**2 )
  else if ( ifunc == 9 ) then
    funcd2 = sin ( x )
  else if ( ifunc == 10 ) then
    funcd2 = exp ( x )
  else if ( ifunc == 11 ) then
    funcd2 = 1.0D+00 / ( 1.0D+00 + sqrt ( 1.0D+00 + x**2 + y**2 ) )
  else if ( ifunc == 12 ) then
    funcd2 = sqrt ( sqrt ( x**2 + y**2 ) )
  else
    funcd2 = 0.0D+00
  end if

  return
end
function funcd3 ( x, y, z )
!
!*******************************************************************************
!
!! FUNCD3 evaluates a function F(X,Y,Z) of 3 variables.
!
!
!  Discussion:
!
!    The actual form of the function can be determined by calling FUNCSET.
!
!  Modified:
!
!    21 January 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, double precision X, Y, Z, the value of the variables.
!
!    Output, double precision FUNCD3, the value of the function.
!
  double precision funcd3
  integer ifunc
  double precision x
  double precision y
  double precision z
!
  call funcset ( 'GET', ifunc )

  if ( ifunc == 1 ) then
    funcd3 = 1.0D+00
  else if ( ifunc == 2 ) then
    funcd3 = x
  else if ( ifunc == 3 ) then
    funcd3 = x**2
  else if ( ifunc == 4 ) then
    funcd3 = x**3
  else if ( ifunc == 5 ) then
    funcd3 = x**4
  else if ( ifunc == 6 ) then
    funcd3 = x**5
  else if ( ifunc == 7 ) then
    funcd3 = x**6
  else if ( ifunc == 8 ) then
    funcd3 = sqrt ( x**2 + y**2 + z**2 )
  else if ( ifunc == 9 ) then
    funcd3 = sin ( x )
  else if ( ifunc == 10 ) then
    funcd3 = exp ( x )
  else if ( ifunc == 11 ) then
    funcd3 = 1.0D+00 / sqrt ( 1.0D+00 + x**2 + y**2 + z**2 )
  else if ( ifunc == 12 ) then
    funcd3 = sqrt ( sqrt ( x**2 + y**2 + z**2 ) )
  else
    funcd3 = 0.0D+00
  end if

  return
end
function funcdn ( n, x )
!
!*******************************************************************************
!
!! FUNCDN evaluates a function of N variables.
!
!
!  Discussion:
!
!    The actual form of the function can be determined by calling FUNCSET.
!
!  Modified:
!
!    21 January 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the number of variables.
!
!    Input, double precision X(N), the value of the variables.
!
!    Output, double precision FUNCDN, the value of the function.
!
  integer n
!
  double precision funcdn
  integer i
  integer ifunc
  double precision temp
  double precision x(n)
!
  call funcset ( 'GET', ifunc )

  if ( ifunc == 1 ) then
    funcdn = 1.0D+00
  else if ( ifunc == 2 ) then
    funcdn = x(1)
  else if ( ifunc == 3 ) then
    funcdn = x(1)**2
  else if ( ifunc == 4 ) then
    funcdn = x(1)**3
  else if ( ifunc == 5 ) then
    funcdn = x(1)**4
  else if ( ifunc == 6 ) then
    funcdn = x(1)**5
  else if ( ifunc == 7 ) then
    funcdn = x(1)**6
  else if ( ifunc == 8 ) then
    funcdn = sqrt ( sum ( x(1:n)**2 ) )
  else if ( ifunc == 9 ) then
    funcdn = sin ( x(1) )
  else if ( ifunc == 10 ) then
    funcdn = exp ( x(1) )
  else if ( ifunc == 11 ) then
    funcdn = 1.0D+00 / ( 1.0D+00 + sqrt ( sum ( x(1:n)**2 ) ) )
  else if ( ifunc == 12 ) then
    funcdn = sqrt ( sqrt ( sum ( x(1:n)**2 ) ) )
  else
    funcdn = 0.0D+00
  end if

  return
end
subroutine funcset ( action, i )
!
!*******************************************************************************
!
!! FUNCSET sets or reports the index of the current function.
!
!
!  Modified:
!
!    21 January 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) ACTION:
!    'COUNT', please return the number of functions.
!    'GET', please return the index of the current function.
!    'SET', please set the function according to the input index.
!
!    Input/output, integer I.
!    If ACTION = 'SET', then I is input, and is the index of the desired
!    function.
!    If ACTION = 'COUNT', then I is output, and is the number of functions
!    available.
!    If ACTION = 'GET', then I is output, and is the index of the current
!    function.
!
  character ( len = * ) action
  integer i
  integer, save :: ival = 0
!
  if ( action == 'SET' ) then
    ival = i
  else if ( action == 'GET' ) then
    i = ival
  else if ( action == 'COUNT' ) then
    i = 12
  end if

  return
end
function fname ( ifunc )
!
!*******************************************************************************
!
!! FNAME returns the name of the current function.
!
!
!  Modified:
!
!    21 January 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer IFUNC, the index of the function.
!
!    Output, character ( len = * ) FNAME, the name of the function.
!
  character ( len = * ) fname
  integer ifunc
!
  if ( ifunc == 1 ) then
    fname = '      1'
  else if ( ifunc == 2 ) then
    fname = '      X'
  else if ( ifunc == 3 ) then
    fname = '   X**2'
  else if ( ifunc == 4 ) then
    fname = '   X**3'
  else if ( ifunc == 5 ) then
    fname = '   X**4'
  else if ( ifunc == 6 ) then
    fname = '   X**5'
  else if ( ifunc == 7 ) then
    fname = '   X**6'
  else if ( ifunc == 8 ) then
    fname = '      R'
  else if ( ifunc == 9 ) then
    fname = ' SIN(X)'
  else if ( ifunc == 10 ) then
    fname = ' EXP(X)'
  else if ( ifunc == 11 ) then
    fname = '1/(1+R)'
  else if ( ifunc == 12 ) then
    fname = 'SQRT(R)'
  else
    fname = '???????'
  end if

  return
end
function fup7 ( x )
!
!*******************************************************************************
!
!! FUP7 is the upper limit function for QMULT_2D.
!
  double precision fup7
  double precision x
!
  fup7 = 1.0D+00

  return
end
function flo7 ( x )
!
!*******************************************************************************
!
!! FLO7 is the lower limit function for QMULT_2D.
!
  double precision flo7
  double precision x
!
  flo7 = -1.0D+00

  return
end
function fu18 ( x )
!
!*******************************************************************************
!
!! FU18 is the upper limit function for problem 18.
!
  double precision fu18
  double precision x
!
  fu18 = 1.0D+00

  return
end
function fl18 ( x )
!
!*******************************************************************************
!
!! FL18 is the lower limit function for problem 18.
!
  double precision fl18
  double precision x
!
  fl18 = -1.0D+00

  return
end
function fu28 ( x, y )
!
!*******************************************************************************
!
!! FU28...
!
  double precision fu28
  double precision x
  double precision y
!
  fu28 = 1.0D+00

  return
end
function fl28 ( x, y )
!
!*******************************************************************************
!
!! FL28...
!
  double precision fl28
  double precision x
  double precision y
!
  fl28 = -1.0D+00

  return
end





















subroutine circle_annulus ( func, xc, yc, radius1, radius2, nr, result )
!
!*******************************************************************************
!
!! CIRCLE_ANNULUS approximates an integral in an annulus.
!
!
!  Discussion:
!
!    An annulus is bounded by two concentric circles.
!
!  Modified:
!
!    17 January 2001
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    William Peirce,
!    Numerical Integration Over the Planar Annulus,
!    Journal of the Society for Industrial and Applied Mathematics,
!    Volume 5, Issue 2, June 1957, pages 66-73.
!
!  Parameters:
!
!    Input, external FUNC, the name of the user supplied function of two
!    variables which is to be integrated, of the form:
!
!      function func ( x, y )
!      double precision func
!      double precision x
!      double precision y
!
!    Input, double precision XC, YC, the center of the circle.
!
!    Input, double precision RADIUS1, RADIUS2, the radii of the circles.
!
!    Input, integer NR, the order of the rule.  This quantity specifies
!    the number of distinct radii to use.  The number of angles used will
!    be 4*NR, for a total of 4*NR**2 points.
!
!    Output, double precision RESULT, the approximation to the integral.
!
  integer nr
!
  double precision a
  double precision area
  double precision b
  double precision c
  double precision circle_annulus_area_2d
  double precision d
  double precision func
  integer i
  integer j
  integer nt
  double precision pi
  double precision quad
  double precision r
  double precision ra(nr)
  double precision radius1
  double precision radius2
  double precision result
  double precision rw(nr)
  double precision t
  double precision tw
  double precision x
  double precision xc
  double precision y
  double precision yc
!
  external func
!
!  Choose radial abscissas and weights.
!
  call legendre_set ( nr, ra, rw )
  a = -1.0D+00
  b = +1.0D+00
  c = radius1**2
  d = radius2**2
  call rule_adjust ( a, b, c, d, nr, ra, rw )
  ra(1:nr) = sqrt ( ra(1:nr) )
  rw(1:nr) = rw(1:nr) / ( radius2**2 - radius1**2 )
!!!  print *,'radius1=',radius1
!!!  print *,'radius2=',radius2
!!!  print *,'rw=',rw
!!!  print *,'nr=',nr
!!!  stop
!
!  Set angular abscissas and weights.
!
  nt = 4 * nr

  tw = 1.0D+00 / dble ( nt )
!
!  Approximate the integral.
!
  quad = 0.0D+00
  do i = 1, nt
    t = 2.0D+00 * pi() * dble ( i - 1 ) / dble ( nt )
    do j = 1, nr
      x = xc + ra(j) * cos ( t )
      y = yc + ra(j) * sin ( t )
      quad = quad + tw * rw(j) * func ( x, y )
    end do
  end do

  area = circle_annulus_area_2d ( radius1, radius2 )
  result = quad * area

  return
end
function circle_annulus_area_2d ( radius1, radius2 )
!
!*******************************************************************************
!
!! CIRCLE_ANNULUS_AREA_2D returns the area of a circular annulus in 2D.
!
!
!  Discussion:
!
!    An annulus comprises the area between two concentric circles.
!
!  Modified:
!
!    14 January 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, double precision RADIUS1, RADIUS2, the radii of the circles.
!
!    Output, double precision CIRCLE_ANNULUS_AREA_2D, the area of the annulus.
!
  double precision circle_annulus_area_2d
  double precision pi
  double precision radius1
  double precision radius2
!
  circle_annulus_area_2d = pi() * ( radius1 + radius2 ) * ( radius2 - radius1 )

  return
end
subroutine circle_annulus_sector ( func, xc, yc, radius1, radius2, theta1, &
  theta2, nr, result )
!
!*******************************************************************************
!
!! CIRCLE_ANNULUS_SECTOR approximates an integral in a circular annulus sector.
!
!
!  Discussion:
!
!    A circular annulus sector comprises the area between two concentric 
!    circles and two concentric rays.
!
!  Modified:
!
!    19 January 2001
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    William Peirce,
!    Numerical Integration Over the Planar Annulus,
!    Journal of the Society for Industrial and Applied Mathematics,
!    Volume 5, Issue 2, June 1957, pages 66-73.
!
!  Parameters:
!
!    Input, external FUNC, the name of the user supplied function of two
!    variables which is to be integrated, of the form:
!
!      function func ( x, y )
!      double precision func
!      double precision x
!      double precision y
!
!    Input, double precision XC, YC, the center of the circle.
!
!    Input, double precision RADIUS1, RADIUS2, the radii of the circles.
!
!    Input, double precision THETA1, THETA2, the angles defining the sector.
!    The sector is measured from THETA1 to THETA2.
!
!    Input, integer NR, the order of the rule.  This quantity specifies
!    the number of distinct radii to use.  The number of angles used will
!    be 4*NR, for a total of 4*NR**2 points.
!
!    Output, double precision RESULT, the approximation to the integral.
!
  integer nr
!
  double precision a
  double precision area
  double precision b
  double precision c
  double precision circle_annulus_sector_area_2d
  double precision d
  double precision func
  integer i
  integer j
  integer nt
  double precision pi
  double precision quad
  double precision r
  double precision ra(nr)
  double precision radius1
  double precision radius2
  double precision result
  double precision rw(nr)
  double precision ta(4*nr)
  double precision theta1
  double precision theta2
  double precision tw(4*nr)
  double precision x
  double precision xc
  double precision y
  double precision yc
!
  external func
!
!  Set the radial abscissas and weights.
!
  call legendre_set ( nr, ra, rw )
  a = -1.0D+00
  b = +1.0D+00
  c = radius1**2
  d = radius2**2
  call rule_adjust ( a, b, c, d, nr, ra, rw )
  ra(1:nr) = sqrt ( ra(1:nr) )
  rw(1:nr) = rw(1:nr) / ( radius2**2 - radius1**2 )
!
!  Pick angles evenly spaced between THETA1 and THETA2, but do not
!  include the endpoints, and use a half interval for the first and last.
!
  nt = 4 * nr

  call tvec_even_bracket3 ( theta1, theta2, nt, ta )
  tw(1:nt) = 1.0D+00 / dble ( nt )
!
!  Approximate the integral.
!
  quad = 0.0D+00
  do i = 1, nt
    do j = 1, nr
      x = xc + ra(j) * cos ( ta(i) )
      y = yc + ra(j) * sin ( ta(i) )
      quad = quad + tw(i) * rw(j) * func ( x, y )
    end do
  end do

  area = circle_annulus_sector_area_2d ( radius1, radius2, theta1, theta2 )
  result = quad * area

  return
end
function circle_annulus_sector_area_2d ( radius1, radius2, theta1, theta2 )
!
!*******************************************************************************
!
!! CIRCLE_ANNULUS_SECTOR_AREA_2D returns the area of a circular annulus sector in 2D.
!
!
!  Discussion:
!
!    A circular annulus sector comprises the area between two concentric 
!    circles and two concentric rays.
!
!  Modified:
!
!    19 January 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, double precision RADIUS1, RADIUS2, the radii of the circles.
!
!    Input, double precision THETA1, THETA2, the angles of the rays.
!    Ordinarily, (THETA2-THETA1) is between 0 and 2*PI.
!
!    Output, double precision CIRCLE_ANNULUS_SECTOR_AREA_2D, the area of the 
!    circulare annulus sector.
!
  double precision circle_annulus_sector_area_2d
  double precision radius1
  double precision radius2
  double precision theta1
  double precision theta2
!
  circle_annulus_sector_area_2d = 0.5 * ( radius1 + radius2 ) &
    * ( radius2 - radius1 ) * ( theta2 - theta1 )

  return
end
function circle_area_2d ( r )
!
!*******************************************************************************
!
!! CIRCLE_AREA_2D returns the area of a circle in 2D.
!
!
!  Modified:
!
!    12 December 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, double precision R, the radius of the circle.
!
!    Output, double precision CIRCLE_AREA_2D, the area of the circle.
!
  double precision circle_area_2d
  double precision pi
  double precision r
!
  circle_area_2d = pi ( ) * r * r

  return
end
subroutine circle_cum ( func, xc, yc, radius, norder, result )
!
!*******************************************************************************
!
!! CIRCLE_CUM approximates an integral on the circumference of a circle in 2D.
!
!
!  Discussion:
!
!    An NORDER point, (NORDER-1)-th degree formula is used, Stroud number U2:M-1.
!
!  Integration region:
!
!    Points (X,Y) such that:
!
!      (X-XC)**2 + (Y-YC)**2 = RADIUS**2.
!
!  Reference:
!
!    Arthur H Stroud,
!    Approximate Calculation of Multiple Integrals,
!    Prentice Hall, 1971.
!
!  Modified:
!
!    07 September 1998
!
!  Parameters:
!
!    Input, external FUNC, the name of the user supplied function of two
!    variables which is to be integrated, of the form:
!
!      function func ( x, y )
!      double precision func
!      double precision x
!      double precision y
!
!    Input, double precision XC, YC, the coordinates of the center of the circle.
!
!    Input, double precision RADIUS, the radius of the circle.
!
!    Input, integer NORDER, the number of points to use.
!
!    Output, double precision RESULT, the approximate integral of the function.
!
  double precision angle
  double precision func
  integer i
  integer norder
  double precision pi
  double precision quad
  double precision radius
  double precision result
  double precision sphere_area_nd
  double precision volume
  double precision x
  double precision xc
  double precision y
  double precision yc
!
  external func
!
  quad = 0.0D+00

  do i = 1, norder
    angle = dble ( 2 * i ) * pi ( ) / dble ( norder )
    x = xc + radius * cos ( angle )
    y = yc + radius * sin ( angle )
    quad = quad + func ( x, y )
  end do

  quad = quad / dble ( norder )

  volume = sphere_area_nd ( 2, radius )
  result = quad * volume

  return
end
function circle_lune_area_2d ( r, theta1, theta2 )
!
!*******************************************************************************
!
!! CIRCLE_LUNE_AREA_2D returns the area of a circular lune in 2D.
!
!
!  Discussion:
!
!    A lune is formed by drawing a circular arc, and joining its endpoints.
!
!  Modified:
!
!    13 January 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, double precision R, the radius of the circle.
!
!    Input, double precision THETA1, THETA2, the angles of the rays
!    that begin and end the arc.
!
!    Output, double precision CIRCLE_LUNE_AREA_2D, the area of the lune.
!
  double precision circle_lune_area_2d
  double precision circle_sector_area_2d
  double precision circle_triangle_area_2d
  double precision r
  double precision sector
  double precision theta1
  double precision theta2
  double precision triangle
!
  sector = circle_sector_area_2d ( r, theta1, theta2 )
  triangle = circle_triangle_area_2d ( r, theta1, theta2 )
  circle_lune_area_2d = sector - triangle

  return
end
function circle_lune_h_area_2d ( r, h )
!
!*******************************************************************************
!
!! CIRCLE_LUNE_H_AREA_2D returns the area of a circular lune in 2D.
!
!
!  Discussion:
!
!    A lune is formed by drawing a circular arc, and joining its endpoints.
!    This lune is described by the "height" of the region.  In other words,
!    the lune is the area that would be submerged if a circle of radius
!    R were standing in water of depth H.
!
!  Modified:
!
!    15 February 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, double precision R, the radius of the circle.
!
!    Input, double precision H, the height of the lune region.
!
!    Output, double precision CIRCLE_LUNE_H_AREA_2D, the area of the lune.
!
  double precision angle
  double precision area
  double precision circle_lune_h_area_2d
  double precision h
  double precision half_width
  double precision pi
  double precision r
  double precision sector
  double precision triangle
!
  if ( h <= 0.0D+00 ) then

    area = 0.0D+00

  else if ( h >= 2.0D+00 * r ) then

    area = pi() * r**2 

  else

    half_width = sqrt ( h * ( 2.0D+00 * r - h ) )
    angle = 2.0D+00 * atan2 ( half_width, r - h )
    sector = r**2 * angle / 2.0D+00
    triangle = ( r - h ) * half_width
    area = sector - triangle

  end if

  circle_lune_h_area_2d = area

  return
end
function circle_lune_w_area_2d ( r, w )
!
!*******************************************************************************
!
!! CIRCLE_LUNE_W_AREA_2D returns the area of a circular lune in 2D.
!
!
!  Discussion:
!
!    A lune is formed by drawing a circular arc, and joining its endpoints.
!    This lune is described by the "width" of the region.  In other words,
!    the lune is the portion of the circle under water if the width
!    of the water surface is W.  There are two possible values for this
!    area, A and (PI*R**2-A).  The routine returns the smaller of the two values.
!
!  Modified:
!
!    15 February 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, double precision R, the radius of the circle.
!
!    Input, double precision W, the width of the lune region.
!
!    Output, double precision CIRCLE_LUNE_W_AREA_2D, the area of the lune.
!
  double precision angle
  double precision area
  double precision circle_lune_w_area_2d
  double precision h
  double precision half_width
  double precision pi
  double precision r
  double precision sector
  double precision triangle
  double precision w
!
  if ( w <= 0.0D+00 ) then

    area = 0.0D+00

  else if ( w >= 2.0D+00 * r ) then

    area = 0.5D+00 * pi() * r**2 

  else

    half_width = 0.5D+00 * w
    h = r - sqrt ( r**2 - half_width**2 )
    angle = 2.0D+00 * atan2 ( half_width, r - h )
    sector = r**2 * angle / 2.0D+00
    triangle = ( r - h ) * half_width
    area = sector - triangle

  end if

  circle_lune_w_area_2d = area

  return
end
subroutine circle_sector ( func, xc, yc, radius, theta1, theta2, nr, result )
!
!*******************************************************************************
!
!! CIRCLE_SECTOR approximates an integral in a circular sector.
!
!
!  Discussion:
!
!    A sector is contained within a circular arc and the lines joining each 
!    endpoint of the arc to the center of the circle.
!
!  Modified:
!
!    19 January 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, external FUNC, the name of the user supplied function of two
!    variables which is to be integrated, of the form:
!
!      function func ( x, y )
!      double precision func
!      double precision x
!      double precision y
!
!    Input, double precision XC, YC, the center of the circle.
!
!    Input, double precision RADIUS, the radius of the circle.
!
!    Input, double precision THETA1, THETA2, the angles defining the sector.
!    The sector is measured from THETA1 to THETA2.
!
!    Input, integer NR, the number of radial values used in the approximation
!    of the integral.  NR must be at least 1.  Higher values improve the
!    accuracy of the integration, at the cost of more function evaluations.
!
!    Output, double precision RESULT, the approximation to the integral.
!
  integer nr
!
  double precision a
  double precision area
  double precision b
  double precision c
  double precision circle_sector_area_2d
  double precision d
  double precision func
  integer i
  integer j
  integer nt
  double precision pi
  double precision quad
  double precision ra(nr)
  double precision radius
  double precision result
  double precision rw(nr)
  double precision t
  double precision ta(4*nr)
  double precision theta1
  double precision theta2
  double precision tw(4*nr)
  double precision x
  double precision xc
  double precision y
  double precision yc
!
  external func
!
!  Set the radial abscissas and weights.
!
  call legendre_set ( nr, ra, rw )
  a = -1.0D+00
  b = +1.0D+00
  c =  0.0D+00
  d =  radius**2
  call rule_adjust ( a, b, c, d, nr, ra, rw )
  ra(1:nr) = sqrt ( ra(1:nr) )
  rw(1:nr) = rw(1:nr) / radius**2
!
!  Pick angles evenly spaced between THETA1 and THETA2, but do not
!  include the endpoints, and use a half interval for the first and last.
!
  nt = 4 * nr

  call tvec_even_bracket3 ( theta1, theta2, nt, ta )
  tw(1:nt) = 1.0D+00 / dble ( nt )
!
!  Approximate the integral.
!
  quad = 0.0D+00

  do i = 1, nr
    do j = 1, nt
      x = xc + ra(i) * cos ( ta(j) )
      y = yc + ra(i) * sin ( ta(j) )
      quad = quad + rw(i) * tw(j) * func ( x, y )
    end do
  end do

  area = circle_sector_area_2d ( radius, theta1, theta2 )
  result = quad * area

  return
end
function circle_sector_area_2d ( r, theta1, theta2 )
!
!*******************************************************************************
!
!! CIRCLE_SECTOR_AREA_2D returns the area of a circular sector in 2D.
!
!
!  Discussion:
!
!    A sector is contained within a circular arc and the lines joining each 
!    endpoint of the arc to the center of the circle.
!
!  Modified:
!
!    12 December 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, double precision R, the radius of the circle.
!
!    Input, double precision THETA1, THETA2, the angles of the rays
!    that delimit the sector.
!
!    Output, double precision CIRCLE_SECTOR_AREA_2D, the area of the sector.
!
  double precision circle_sector_area_2d
  double precision r
  double precision theta1
  double precision theta2
!
  circle_sector_area_2d = 0.50D+00 * r**2 * ( theta2 - theta1 )

  return
end
subroutine circle_set_rt ( rule, nr, ra, rw, nt, ta, tw, zw )
!
!*******************************************************************************
!
!! CIRCLE_SET_RT sets an R, THETA product quadrature rule in the unit circle.
!
!
!  Integration region:
!
!    Points (X,Y) such that:
!
!      X**2 + Y**2 <= 1.0.
!
!  Reference:
!
!    Abramowitz and Stegun,
!    Handbook of Mathematical Functions,
!    National Bureau of Standards, 1964.
!
!    Arthur H Stroud,
!    Approximate Calculation of Multiple Integrals,
!    Prentice Hall, 1971.
!
!  Modified:
!
!    16 December 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer RULE, the rule desired.
!      1, 1 + 0 * 0 points.
!      2, 0 + 1 * 4 points.
!      3, 1 + 1 * 4 points.
!      4, 1 + 1 * 6 points.
!      5, 1 + 2 * 4 points.
!      6, 0 + 3 * 4 points.
!      8, 0 + 4 * 16 points.
!      9, 0 + 5 * 20 points.
!
!    Output, integer NR, the number of R abscissas.
!
!    Output, double precision RA(NR), RW(NR), the R abscissas and weights.
!
!    Output, integer NT, the number of Theta abscissas.
!
!    Output, double precision TA(NT), TW(NT), the THETA abscissas and weights.
!
!    Output, double precision ZW, the weight to use for the center.
!
  double precision a
  double precision b
  double precision c
  double precision d
  integer nr
  integer nt
  double precision ra(*)
  double precision rw(*)
  integer rule
  double precision ta(*)
  double precision tw(*)
  double precision u
  double precision v
  double precision w
  double precision zw
!
  if ( rule == 1 ) then

    nr = 0
    nt = 0
    zw = 1.0D+00

  else if ( rule == 2 ) then

    nr = 1
    ra(1) = 0.5D+00
    rw(1) = 1.0D+00

    nt = 4
    call tvec_even2 ( nt, ta )
    tw(1:nt) = 1.0D+00 / dble ( nt )

    zw = 0.0D+00

  else if ( rule == 3 ) then

    nr = 1
    ra(1) = 1.0D+00
    rw(1) = 1.0D+00

    nt = 4
    call tvec_even ( nt, ta )
    tw(1:4) = 0.125D+00

    zw = 0.5D+00

  else if ( rule == 4 ) then

    nr = 1
    ra(1) = sqrt ( 2.0D+00 / 3.0D+00 )
    rw(1) = 1.0D+00

    nt = 6
    call tvec_even ( nt, ta )
    tw(1:nt) = 0.125D+00

    zw = 0.25D+00

  else if ( rule == 5 ) then

    a = 1.0D+00
    b = sqrt ( 2.0D+00 ) / 2.0D+00
    u = 1.0D+00 / 6.0D+00
    v = 4.0D+00 / 6.0D+00

    nr = 2
    ra(1:nr) = (/ a, b /)
    rw(1:nr) = (/ u, v /)

    nt = 4
    call tvec_even ( nt, ta )
    tw(1:nt) = 1.0D+00 / dble ( nt )

    zw = 4.0D+00 / 24.0D+00

  else if ( rule == 6 ) then

    a = sqrt ( 3.0D+00 ) / 2.0D+00
    b = sqrt ( ( 27.0D+00 - 3.0D+00 * sqrt ( 29.0D+00 ) ) / 52.0D+00 )
    c = sqrt ( ( 27.0D+00 + 3.0D+00 * sqrt ( 29.0D+00 ) ) / 52.0D+00 )

    u = 8.0D+00 / 27.0D+00
    v = ( 551.0D+00 + 41.0D+00 * sqrt ( 29.0D+00 ) ) / 1566.0D+00
    w = ( 551.0D+00 - 41.0D+00 * sqrt ( 29.0D+00 ) ) / 1566.0D+00

    nr = 3
    ra(1:nr) = (/ a, b, c /)
    rw(1:nr) = (/ u, v, w /)

    nt = 4
    call tvec_even ( nt, ta )
    tw(1:nt) = 1.0D+00 / dble ( nt )

    zw = 0.0D+00

  else if ( rule == 7 ) then

    a = sqrt ( ( 6.0D+00 - sqrt ( 6.0D+00 ) ) / 10.0D+00 )
    b = sqrt ( ( 6.0D+00 + sqrt ( 6.0D+00 ) ) / 10.0D+00 )
    u = ( 16.0D+00 + sqrt ( 6.0D+00 ) ) / 36.0D+00
    v = ( 16.0D+00 - sqrt ( 6.0D+00 ) ) / 36.0D+00

    nr = 2
    ra(1:nr) = (/ a, b /)
    rw(1:nr) = (/ u, v /)

    nt = 10
    call tvec_even ( nt, ta )
    tw(1:nt) = 1.0D+00 / dble ( nt )

    zw = 1.0D+00 / 9.0D+00

  else if ( rule == 8 ) then

    nr = 4
    call legendre_set ( nr, ra, rw )
    a = -1.0D+00
    b = +1.0D+00
    c =  0.0D+00
    d = +1.0D+00
    call rule_adjust ( a, b, c, d, nr, ra, rw )
    ra(1:nr) = sqrt ( ra(1:nr) )

    nt = 16
    call tvec_even ( nt, ta )
    tw(1:nt) = 1.0D+00 / dble ( nt )

    zw = 0.0D+00

  else if ( rule == 9 ) then

    nr = 5
    call legendre_set ( nr, ra, rw )
    a = -1.0D+00
    b = +1.0D+00
    c =  0.0D+00
    d = +1.0D+00
    call rule_adjust ( a, b, c, d, nr, ra, rw )
    ra(1:nr) = sqrt ( ra(1:nr) )

    nt = 20
    call tvec_even ( nt, ta )
    tw(1:nt) = 1.0D+00 / dble ( nt )

    zw = 0.0D+00

  else

    write ( *, * ) ' '
    write ( *, * ) 'CIRCLE_SET_RT - Fatal error!'
    write ( *, * ) '  There is no rule of index ', rule
    stop

  end if

  return
end
subroutine circle_set_xy ( rule, norder, xtab, ytab, weight )
!
!*******************************************************************************
!
!! CIRCLE_SET_XY sets an XY quadrature rule inside the unit circle in 2D.
!
!
!  Integration region:
!
!    Points (X,Y) such that:
!
!      X**2 + Y**2 <= 1.0.
!
!  Reference:
!
!    Abramowitz and Stegun,
!    Handbook of Mathematical Functions,
!    National Bureau of Standards, 1964.
!
!    Frank Lether,
!    A Generalized Product Rule for the Circle,
!    SIAM Journal on Numerical Analysis,
!    Volume 8, Number 2, June 1971, pages 249-253.
!
!    Arthur H Stroud,
!    Approximate Calculation of Multiple Integrals,
!    Prentice Hall, 1971.
!
!  Modified:
!
!    16 December 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer RULE, the rule desired.
!      1, 1 point 1-st degree;
!      2, 4 point 3-rd degree, Stroud S2:3-1;
!      3, 4 point 3-rd degree, Lether #1;
!      4, 4 point 3-rd degree, Stroud S2:3-2;
!      5, 5 point 3-rd degree;
!      6, 7 point 5-th degree;
!      7, 9 point 5-th degree;
!      8, 9 point 5-th degree, Lether #2;
!      9, 12 point 7-th degree;
!     10, 16 point 7-th degree, Lether #3;
!     11, 21 point 9-th degree, Stroud S2:9-3;
!     12, 25 point 9-th degree, Lether #4 (after correcting error);
!     13, 64 point 15-th degree Gauss product rule.
!
!    Output, integer NORDER, the order of the desired rule.
!
!    Output, double precision XTAB(*), YTAB(*), the NORDER abscissas of the rule.
!
!    Output, double precision WEIGHT(*), the NORDER weights of the rule.
!
  double precision a
  double precision b
  double precision c
  double precision d
  double precision e
  double precision f
  double precision g
  double precision h
  integer i
  integer j
  integer k
  integer norder
  integer nr
  double precision pi
  double precision r
  double precision ra(4)
  double precision rw(4)
  integer rule
  double precision s
  double precision t
  double precision u
  double precision v
  double precision w
  double precision w1
  double precision w2
  double precision w3
  double precision w4
  double precision w5
  double precision w6
  double precision w7
  double precision w8
  double precision w9
  double precision weight(*)
  double precision xtab(*)
  double precision ytab(*)
  double precision z
!
  if ( rule == 1 ) then

    norder = 1
    xtab(1) = 0.0D+00
    ytab(1) = 0.0D+00
    weight(1) = 1.0D+00

  else if ( rule == 2 ) then

    a = 0.5D+00
    b = 0.25D+00
    z = 0.0D+00

    norder = 4
    xtab(1:4) =   (/  a, -a,  z,  z /)
    ytab(1:4) =   (/  z,  z,  a, -a /)
    weight(1:4) = (/  b,  b,  b,  b /)

  else if ( rule == 3 ) then

    a = 0.5D+00
    b = 0.25D+00

    norder = 4
    xtab(1:4) =   (/  a, -a, -a,  a /)
    ytab(1:4) =   (/  a,  a, -a, -a /)
    weight(1:4) = (/  b,  b,  b,  b /)

  else if ( rule == 4 ) then

    a = sqrt ( 2.0D+00 ) / 2.0D+00
    b = 0.25D+00

    norder = 4
    xtab(1:4) =   (/  a, -a, -a,  a /)
    ytab(1:4) =   (/  a,  a, -a, -a /)
    weight(1:4) = (/  b,  b,  b,  b /)

  else if ( rule == 5 ) then

    a = 1.0D+00
    b = 0.5D+00
    c = 0.125D+00
    z = 0.0D+00

    norder = 5
    xtab(1:5) =   (/ z, a, z, -a,  z /)
    ytab(1:5) =   (/ z, z, a,  z, -a /)
    weight(1:5) = (/ b, c, c,  c,  c /)

  else if ( rule == 6 ) then

    a = sqrt ( 2.0D+00 / 3.0D+00 )
    b = sqrt ( 1.0D+00 / 6.0D+00 )
    c = sqrt ( 2.0D+00 ) / 2.0D+00
    d = 0.125D+00
    e = 0.25D+00
    z = 0.0D+00

    norder = 7
    xtab(1:7) =   (/ z, a, -a,  b, -b,  b, -b /)
    ytab(1:7) =   (/ z, z,  z,  c,  c, -c, -c /)
    weight(1:7) = (/ e, d,  d,  d,  d,  d,  d /)

  else if ( rule == 7 ) then

    a = 0.5D+00
    b = 1.0D+00
    c = 4.0D+00 / 24.0D+00
    d = 1.0D+00 / 24.0D+00
    z = 0.0D+00

    norder = 9
    xtab(1:9) =   (/ z,  b, -b,  z,  z,  a, -a, -a,  a /)
    ytab(1:9) =   (/ z,  z,  z,  b, -b,  a,  a, -a, -a /)
    weight(1:9) = (/ c,  d,  d,  d,  d,  c,  c,  c,  c /)

  else if ( rule == 8 ) then

    a = sqrt ( 2.0D+00 ) / 2.0D+00
    b = sqrt ( 3.0D+00 / 5.0D+00 )
    c = sqrt ( 3.0D+00 / 10.0D+00 )

    w1 = 16.0D+00 / 72.0D+00
    w2 =  8.0D+00 / 72.0D+00
    w3 = 10.0D+00 / 72.0D+00
    w4 =  5.0D+00 / 72.0D+00

    z = 0.0D+00

    norder = 9
    xtab(1:9) =   (/  z,   a,  -a,   z,   z,   a,   a,  -a,  -a /)
    ytab(1:9) =   (/  z,   z,   z,   b,  -b,   c,  -c,   c,  -c /)
    weight(1:9) = (/ w1,  w2,  w2,  w3,  w3,  w4,  w4,  w4,  w4 /)

  else if ( rule == 9 ) then

    a = sqrt ( 3.0D+00 ) / 2.0D+00
    b = sqrt ( ( 27.0D+00 - 3.0D+00 * sqrt ( 29.0D+00 ) ) / 104.0D+00 )
    c = sqrt ( ( 27.0D+00 + 3.0D+00 * sqrt ( 29.0D+00 ) ) / 104.0D+00 )
    u = 2.0D+00 / 27.0D+00
    v = ( 551.0D+00 + 41.0D+00 * sqrt ( 29.0D+00 ) ) / 6264.0D+00
    w = ( 551.0D+00 - 41.0D+00 * sqrt ( 29.0D+00 ) ) / 6264.0D+00
    z = 0.0D+00

    norder = 12
    xtab(1:12) =   (/ a, -a,  z,  z,  b, -b,  b, -b,  c,  c, -c, -c /)
    ytab(1:12) =   (/ z,  z,  a, -a,  b,  b, -b, -b,  c, -c,  c, -c /)
    weight(1:12) = (/ u,  u,  u,  u,  v,  v,  v,  v,  w,  w,  w,  w /)

  else if ( rule == 10 ) then

    a = sqrt ( ( 3.0D+00 - sqrt ( 5.0D+00 ) ) / 8.0D+00 )
    b = sqrt ( ( 15.0D+00 + 3.0D+00 * sqrt ( 5.0D+00 ) &
      - 2.0D+00 * sqrt ( 30.0D+00 ) - 2.0D+00 * sqrt ( 6.0D+00 ) ) / 56.0D+00 )
    c = sqrt ( ( 15.0D+00 + 3.0D+00 * sqrt ( 5.0D+00 ) &
      + 2.0D+00 * sqrt ( 30.0D+00 ) + 2.0D+00 * sqrt ( 6.0D+00 ) ) / 56.0D+00 )
    d = sqrt ( ( 3.0D+00 + sqrt ( 5.0D+00 ) ) / 8.0D+00 )
    e = sqrt ( ( 15.0D+00 - 3.0D+00 * sqrt ( 5.0D+00 ) &
      - 2.0D+00 * sqrt ( 30.0D+00 ) + 2.0D+00 * sqrt ( 6.0D+00 ) ) / 56.0D+00 )
    f = sqrt ( ( 15.0D+00 - 3.0D+00 * sqrt ( 5.0D+00 ) &
      + 2.0D+00 * sqrt ( 30.0D+00 ) - 2.0D+00 * sqrt ( 6.0D+00 ) ) / 56.0D+00 )
    w1 = ( 90.0D+00 + 5.0D+00 * sqrt ( 30.0D+00 ) + 18.0D+00 * sqrt ( 5.0D+00 ) &
       + 5.0D+00 * sqrt ( 6.0D+00 ) ) / 1440.0D+00
    w2 = ( 90.0D+00 - 5.0D+00 * sqrt ( 30.0D+00 ) + 18.0D+00 * sqrt ( 5.0D+00 ) &
       - 5.0D+00 * sqrt ( 6.0D+00 ) ) / 1440.0D+00
    w3 = ( 90.0D+00 + 5.0D+00 * sqrt ( 30.0D+00 ) - 18.0D+00 * sqrt ( 5.0D+00 ) &
       - 5.0D+00 * sqrt ( 6.0D+00 ) ) / 1440.0D+00
    w4 = ( 90.0D+00 - 5.0D+00 * sqrt ( 30.0D+00 ) - 18.0D+00 * sqrt ( 5.0D+00 ) &
       + 5.0D+00 * sqrt ( 6.0D+00 ) ) / 1440.0D+00

    norder = 16
    xtab(1:norder) =   (/  a,  a, -a, -a,  a,  a, -a, -a,  d,  d, -d, -d, &
                           d,  d, -d, -d /)
    ytab(1:norder) =   (/  b, -b,  b, -b,  c, -c,  c, -c,  e, -e,  e, -e, &
                           f, -f,  f, -f /)
    weight(1:norder) = (/ w1, w1, w1, w1, w2, w2, w2, w2, w3, w3, w3, w3, &
                          w4, w4, w4, w4 /)

  else if ( rule == 11 ) then

    norder = 21

    xtab(1) = 0.0D+00
    ytab(1) = 0.0D+00

    weight(1) = 1.0D+00 / 9.0D+00
    weight(2:11) = ( 16.0D+00 + sqrt ( 6.0D+00 ) ) / 360.0D+00
    weight(12:21) = ( 16.0D+00 - sqrt ( 6.0D+00 ) ) / 360.0D+00

    r = sqrt ( ( 6.0D+00 - sqrt ( 6.0D+00 ) ) / 10.0D+00 )

    do i = 1, 10
      a = 2.0D+00 * pi ( ) * dble ( i ) / dble ( 10.0D+00 )
      xtab(1+i) = r * cos ( a )
      ytab(1+i) = r * sin ( a )
    end do

    r = sqrt ( ( 6.0D+00 + sqrt ( 6.0D+00 ) ) / 10.0D+00 )

    do i = 1, 10
      a = 2.0D+00 * pi ( ) * dble ( i ) / dble ( 10.0D+00 )
      xtab(11+i) = r * cos ( a )
      ytab(11+i) = r * sin ( a )
    end do
!
!  There was apparently a misprint in the Lether paper.  The quantity
!  which here reads "322" was printed there as "332".
!
  else if ( rule == 12 ) then

    a = 0.5D+00
    b = sqrt ( 3.0D+00 ) / 2.0D+00
    c = sqrt ( ( 35.0D+00 + 2.0D+00 * sqrt ( 70.0D+00 ) ) / 252.0D+00 )
    d = sqrt ( ( 35.0D+00 - 2.0D+00 * sqrt ( 70.0D+00 ) ) / 252.0D+00 )
    e = sqrt ( ( 35.0D+00 + 2.0D+00 * sqrt ( 70.0D+00 ) ) / 84.0D+00 )
    f = sqrt ( ( 35.0D+00 - 2.0D+00 * sqrt ( 70.0D+00 ) ) / 84.0D+00 )
    g = sqrt ( ( 35.0D+00 + 2.0D+00 * sqrt ( 70.0D+00 ) ) / 63.0D+00 )
    h = sqrt ( ( 35.0D+00 - 2.0D+00 * sqrt ( 70.0D+00 ) ) / 63.0D+00 )

    w1 = 64.0D+00 / 675.0D+00
    w2 = 16.0D+00 / 225.0D+00
    w3 = 16.0D+00 / 675.0D+00
    w4 = ( 322.0D+00 - 13.0D+00 * sqrt ( 70.0D+00 ) ) / 21600.0D+00
    w5 = ( 322.0D+00 + 13.0D+00 * sqrt ( 70.0D+00 ) ) / 21600.0D+00
    w6 = ( 322.0D+00 - 13.0D+00 * sqrt ( 70.0D+00 ) ) / 7200.0D+00
    w7 = ( 322.0D+00 + 13.0D+00 * sqrt ( 70.0D+00 ) ) / 7200.0D+00
    w8 = ( 322.0D+00 - 13.0D+00 * sqrt ( 70.0D+00 ) ) / 5400.0D+00
    w9 = ( 322.0D+00 + 13.0D+00 * sqrt ( 70.0D+00 ) ) / 5400.0D+00
    z = 0.0D+00

    norder = 25
    xtab(1:norder) =   (/  z,  a, -a,  b, -b,  b,  b, -b, -b,  b,  b, -b, -b, &
                           a,  a, -a, -a,  a,  a, -a, -a,  z,  z,  z,  z /)
    ytab(1:norder) =   (/  z,  z,  z,  z,  z,  c, -c,  c, -c,  d, -d,  d, -d, &
                           e, -e,  e, -e,  f, -f,  f, -f,  g, -g,  h, -h /)
    weight(1:norder) = (/ w1, w2, w2, w3, w3, w4, w4, w4, w4, w5, w5, w5, w5, &
                          w6, w6, w6, w6, w7, w7, w7, w7, w8, w8, w9, w9 /)

  else if ( rule == 13 ) then

    nr = 4
    call legendre_set ( nr, ra, rw )
    a = -1.0D+00
    b = +1.0D+00
    c =  0.0D+00
    d = +1.0D+00
    call rule_adjust ( a, b, c, d, nr, ra, rw )
    ra(1:nr) = sqrt ( ra(1:nr) )

    norder = 64

    i = 0

    do j = 1, 16

      c = cos ( pi ( ) * dble ( j ) / 8.0D+00 )
      s = sin ( pi ( ) * dble ( j ) / 8.0D+00 )

      do k = 1, 4

        i = i + 1
        xtab(i) = c * ra(k)
        ytab(i) = s * ra(k)
        weight(i) = rw(k) / 16.0D+00

      end do

    end do

  else

    norder = 0
    write ( *, * ) ' '
    write ( *, * ) 'CIRCLE_SET_XY - Fatal error!'
    write ( *, * ) '  There is no rule of index ', rule
    stop

  end if

  return
end
subroutine circle_sum_rt ( func, xc, yc, radius, nr, ra, rw, nt, ta, tw, zw, &
  result )
!
!*******************************************************************************
!
!! CIRCLE_SUM_RT applies an R, THETA product quadrature rule inside a circle.
!
!
!  Discussion:
!
!    The product rule is assumed to be have the form:
!
!      Integral_Approx = ZW * F(XC,YC) +
!        Sum ( 1 <= IR <= NR ) Sum ( 1 <= IT <= NT )
!        RW(IR) * TW(IT) * F ( XC + R(IR) * RADIUS * Cos ( TA(IT) ),
!                              YC + R(IR) * RADIUS * Sin ( TA(IT) ) )
!
!  Integration region:
!
!    Points (X,Y) such that:
!
!      (X-XC)**2 + (Y-YC)**2 <= RADIUS**2.
!
!  Modified:
!
!    15 December 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, external FUNC, the name of the user supplied
!    function of two variables which is to be integrated,
!    of the form:
!
!      function func ( x, y )
!      double precision func
!      double precision x
!      double precision y
!
!    Input, double precision XC, YC, the coordinates of the center of the circle.
!
!    Input, double precision RADIUS, the radius of the circle.
!
!    Input, integer NR, the number of R abscissas.
!
!    Input, double precision RA(NR), RW(NR), the R abscissas and weights.
!
!    Input, integer NT, the number of Theta abscissas.
!
!    Input, double precision TA(NT), TW(NT), the THETA abscissas and weights.
!
!    Input, double precision ZW, the weight to use for the center.
!
!    Output, double precision RESULT, the approximate integral of the function.
!
  integer nr
  integer nt
!
  double precision circle_area_2d
  double precision func
  integer ir
  integer it
  double precision quad
  double precision ra(nr)
  double precision radius
  double precision rct
  double precision result
  double precision rst
  double precision rw(nr)
  double precision ta(nt)
  double precision tw(nt)
  double precision volume
  double precision x
  double precision xc
  double precision y
  double precision yc
  double precision zw
!
  external func
!
  quad = 0.0D+00

  if ( zw /= 0.0D+00 ) then
    x = xc
    y = yc
    quad = quad + zw * func ( x, y )
  end if

  do it = 1, nt
    rct = radius * cos ( ta(it) )
    rst = radius * sin ( ta(it) )
    do ir = 1, nr
      x = xc + ra(ir) * rct
      y = yc + ra(ir) * rst
      quad = quad + tw(it) * rw(ir) * func ( x, y )
    end do
  end do

  volume = circle_area_2d ( radius )
  result = quad * volume

  return
end
subroutine circle_sum_xy ( func, xc, yc, radius, norder, xtab, ytab, weight, &
  result )
!
!*******************************************************************************
!
!! CIRCLE_SUM_XY applies an XY quadrature rule inside a circle in 2D.
!
!
!  Integration region:
!
!    Points (X,Y) such that:
!
!      (X-XC)**2 + (Y-YC)**2 <= RADIUS**2.
!
!  Modified:
!
!    14 December 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, external FUNC, the name of the user supplied
!    function of two variables which is to be integrated,
!    of the form:
!
!      function func ( x, y )
!      double precision func
!      double precision x
!      double precision y
!
!    Input, double precision XC, YC, the coordinates of the center of the circle.
!
!    Input, double precision RADIUS, the radius of the circle.
!
!    Input, integer NORDER, the order of the rule.  The rule is
!    assumed to be defined on the unit circle.
!
!    Input, double precision XTAB(NORDER), YTAB(NORDER), the XY
!    coordinates of the abscissas of the quadrature rule for a unit circle.
!
!    Input, double precision WEIGHT(NORDER), the weights of the rule.
!
!    Output, double precision RESULT, the approximate integral of the function.
!
  integer norder
!
  double precision circle_area_2d
  double precision func
  integer i
  double precision quad
  double precision radius
  double precision result
  double precision volume
  double precision weight(norder)
  double precision x
  double precision xc
  double precision xtab(norder)
  double precision y
  double precision yc
  double precision ytab(norder)
!
  external func
!
  quad = 0.0D+00
  do i = 1, norder
    x = xc + radius * xtab(i)
    y = yc + radius * ytab(i)
    quad = quad + weight(i) * func ( x, y )
  end do

  volume = circle_area_2d ( radius )
  result = quad * volume

  return
end
function circle_triangle_area_2d ( r, theta1, theta2 )
!
!*******************************************************************************
!
!! CIRCLE_TRIANGLE_AREA_2D returns the area of a circle triangle in 2D.
!
!
!  Discussion:
!
!    A circle triangle is formed by drawing a circular arc, and considering
!    the triangle formed by the endpoints of the arc plus the center of
!    the circle.
!
!    Note that for angles greater than PI, the triangle will actually
!    have NEGATIVE area.
!
!  Modified:
!
!    12 December 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, double precision R, the radius of the circle.
!
!    Input, double precision THETA1, THETA2, the angles of the rays that
!    delimit the arc.
!
!    Output, double precision CIRCLE_TRIANGLE_AREA_2D, the (signed) area
!    of the triangle.
!
  double precision circle_triangle_area_2d
  double precision r
  double precision theta1
  double precision theta2
!
  circle_triangle_area_2d = 0.5D+00 * r**2 * sin ( theta2 - theta1 )

  return
end
subroutine cone_unit_3d ( func, result )
!
!*******************************************************************************
!
!! CONE_UNIT_3D approximates an integral inside a unit cone in 3D.
!
!
!  Integration Region:
!
!    X**2 + Y**2 <= 1 - Z
!    0 <= Z <= 1.
!
!  Discussion:
!
!    An 48 point degree 7 formula, Stroud CN:S2:7-1, is used.
!
!    (There is a typographical error in the S2:7-1 formula for B3.)
!
!  Reference:
!
!    Arthur H Stroud,
!    Approximate Calculation of Multiple Integrals,
!    Prentice Hall, 1971, page 339.
!
!  Modified:
!
!    18 November 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, external FUNC, the name of the user supplied function which
!    evaluates F(X,Y,Z), of the form
!
!      function func ( x, y, z )
!      double precision func
!      double precision x
!      double precision y
!      double precision z
!
!    Output, double precision RESULT, the approximate integral of the function.
!
  double precision a
  double precision b
  double precision c
  double precision cone_volume_3d
  double precision func
  double precision h
  integer i
  double precision pi
  double precision quad
  double precision r
  double precision result
  double precision, save, dimension ( 4 ) :: u = &
    (/ 0.04850054945D+00, 0.2386007376D+00, 0.5170472951D+00,  0.7958514179D+00 /)
  double precision volume
  double precision, save, dimension ( 4 ) :: w1 = &
    (/ 0.1108884156D+00,  0.1434587878D+00, 0.06863388717D+00, 0.01035224075D+00 /)
  double precision w2(3)
  double precision x
  double precision y
  double precision z
!
  external func
!
  a = sqrt ( 3.0D+00 ) / 2.0D+00
  b = sqrt ( ( 27.0D+00 - 3.0D+00 * sqrt ( 29.0D+00 ) ) / 104.0D+00 )
  c = sqrt ( ( 27.0D+00 + 3.0D+00 * sqrt ( 29.0D+00 ) ) / 104.0D+00 )
  w2(1:3) = 3.0D+00 * (/ &
    2.0D+00 / 27.0D+00, &
    ( 551.0D+00 + 4.0D+00 * sqrt ( 29.0D+00 ) ) / 6264.0D+00, &
    ( 551.0D+00 - 4.0D+00 * sqrt ( 29.0D+00 ) ) / 6264.0D+00 /)

  quad = 0.0D+00

  do i = 1, 4

    x = a * ( 1.0D+00 - u(i) )
    y = 0.0D+00
    z = u(i)
    quad = quad + w1(i) * w2(1) * func ( x, y, z )

    x = - a * ( 1.0D+00 - u(i) )
    y = 0.0D+00
    z = u(i)
    quad = quad + w1(i) * w2(1) * func ( x, y, z )

    x = 0.0D+00
    y = a * ( 1.0D+00 - u(i) )
    z = u(i)
    quad = quad + w1(i) * w2(1) * func ( x, y, z )

    x = 0.0D+00
    y = - a * ( 1.0D+00 - u(i) )
    z = u(i)
    quad = quad + w1(i) * w2(1) * func ( x, y, z )

  end do

  do i = 1, 4

    x =   b * ( 1.0D+00 - u(i) )
    y =   b * ( 1.0D+00 - u(i) )
    z =   u(i)
    quad = quad + w1(i) * w2(2) * func ( x, y, z )

    x = - b * ( 1.0D+00 - u(i) )
    y =   b * ( 1.0D+00 - u(i) )
    z =   u(i)
    quad = quad + w1(i) * w2(2) * func ( x, y, z )

    x = - b * ( 1.0D+00 - u(i) )
    y = - b * ( 1.0D+00 - u(i) )
    z =   u(i)
    quad = quad + w1(i) * w2(2) * func ( x, y, z )

    x =   b * ( 1.0D+00 - u(i) )
    y = - b * ( 1.0D+00 - u(i) )
    z =   u(i)
    quad = quad + w1(i) * w2(2) * func ( x, y, z )

    x =   c * ( 1.0D+00 - u(i) )
    y =   c * ( 1.0D+00 - u(i) )
    z =   u(i)
    quad = quad + w1(i) * w2(3) * func ( x, y, z )

    x = - c * ( 1.0D+00 - u(i) )
    y =   c * ( 1.0D+00 - u(i) )
    z =   u(i)
    quad = quad + w1(i) * w2(3) * func ( x, y, z )

    x = - c * ( 1.0D+00 - u(i) )
    y = - c * ( 1.0D+00 - u(i) )
    z =   u(i)
    quad = quad + w1(i) * w2(3) * func ( x, y, z )

    x =   c * ( 1.0D+00 - u(i) )
    y = - c * ( 1.0D+00 - u(i) )
    z =   u(i)
    quad = quad + w1(i) * w2(3) * func ( x, y, z )

  end do

  r = 1.0D+00
  h = 1.0D+00

  volume = cone_volume_3d ( r, h )
  result = quad * volume

  return
end
function cone_volume_3d ( r, h )
!
!*******************************************************************************
!
!! CONE_VOLUME_3D returns the volume of a cone in 3D.
!
!
!  Modified:
!
!    16 November 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, double precision R, the radius of the base of the cone.
!
!    Input, double precision H, the height of the cone.
!
!    Output, double precision CONE_VOLUME_3D, the volume of the cone.
!
  double precision cone_volume_3d
  double precision h
  double precision pi
  double precision r
!
  cone_volume_3d = ( pi ( ) / 3.0D+00 ) * h * r**2

  return
end
subroutine cube_shell_nd ( func, n, r1, r2, result )
!
!*******************************************************************************
!
!! CUBE_SHELL_ND approximates an integral inside a cubic shell in N dimensions.
!
!
!  Discussion:
!
!    An N*2**N point third degree formula is used, Stroud number CNSHELL:3-4.
!
!  Integration region:
!
!    N dimensional points X() such that:
!
!      R1 <= ABS ( X(I) ) <= R2, for I = 1 to N.
!
!  Reference:
!
!    Arthur H Stroud,
!    Approximate Calculation of Multiple Integrals,
!    Prentice Hall, 1971.
!
!  Modified:
!
!    25 August 1998
!
!  Parameters:
!
!    Input, external FUNC, the name of the user supplied
!    function which evaluates F at the N-vector X, of the form
!
!      function func ( n, x )
!      integer n
!      double precision func
!      double precision x(n)
!
!    Input, integer N, the dimension of the space.
!
!    Input, double precision R1, R2, the inner and outer radii of the cubical
!    shell.  The outer cube is of side 2*R2, the inner, missing cube of side
!    2*R1.
!
!    Output, double precision RESULT, the approximate integral of the function.
!
  integer n
!
  double precision cube_shell_volume_nd
  double precision func
  integer i
  integer iadd
  integer ix(n)
  integer j
  integer jhi
  logical more
  integer ncard
  double precision quad
  double precision r1
  double precision r2
  double precision result
  double precision u
  double precision v
  double precision volume
  double precision w
  double precision x(n)
!
  external func
!
  if ( r1 == r2 ) then
    result = 0.0D+00
    return
  end if

  u = sqrt ( dble ( n ) * ( r2**(n+2) - r1**(n+2) ) &
    / ( dble ( n + 2 ) * ( r2**n - r1**n ) ) )
  v = u / sqrt ( 3.0D+00 )

  w = 1.0D+00 / dble ( n * 2**n )

  quad = 0.0D+00
  do i = 1, n

    x(1:n) = - v

    x(i) = - u
    more = .false.
    jhi = 2**n

    do j = 1, jhi

      call subset_next ( n, ix, more, ncard, iadd )

      if ( iadd /= 0 ) then
        x(iadd) = - x(iadd)
      end if

      quad = quad + w * func ( n, x )

    end do

  end do

  volume = cube_shell_volume_nd ( n, r1, r2 )
  result = quad * volume

  return
end
function cube_shell_volume_nd ( n, r1, r2 )
!
!*******************************************************************************
!
!! CUBE_SHELL_VOLUME_ND computes the volume of a cubic shell in ND.
!
!
!  Integration region:
!
!    N dimensional points X() such that:
!
!      R1 <= ABS ( X(I) ) <= R2, for I = 1 to N.
!
!  Modified:
!
!    20 November 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the dimension of the space.
!
!    Input, double precision R1, R2, the inner and outer radii of the cubic
!    shell.  The outer cube is of side 2*R2, the inner, missing cube of side
!    2*R1.
!
!    Output, double precision CUBE_SHELL_VOLUME_ND, the volume of the cubic 
!    shell.
!
  double precision cube_shell_volume_nd
  integer n
  double precision r1
  double precision r2
!
  cube_shell_volume_nd = ( r2**n - r1**n ) * 2**n

  return
end
subroutine cube_unit_3d ( func, result )
!
!*******************************************************************************
!
!! CUBE_UNIT_3D approximates an integral inside the unit cube in 3D.
!
!
!  Discussion:
!
!    An 8 point third degree formula is used.
!
!  Integration region:
!
!    Points (X,Y,Z) such that:
!
!      -1 <= X <= 1,
!      -1 <= Y <= 1,
!      -1 <= Z <= 1.
!
!  Reference:
!
!    Arthur H Stroud,
!    Approximate Calculation of Multiple Integrals,
!    Prentice Hall, 1971.
!
!  Modified:
!
!    15 August 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, external FUNC, the name of the user supplied
!    function which evaluates F(X,Y,Z), of the form
!
!      function func ( x, y, z )
!      double precision func
!      double precision x
!      double precision y
!      double precision z
!
!    Output, double precision RESULT, the approximate integral of the function.
!
  double precision cube_unit_volume_nd
  double precision func
  double precision quad
  double precision result
  double precision s
  double precision volume
  double precision w
  double precision x
  double precision y
  double precision z
!
  external func
!
  s = 1.0D+00 / sqrt ( 3.0D+00 )
  w = 1.0D+00 / 8.0D+00

  x = s
  y = s
  z = s

  quad = w * ( func (  x,  y,  z ) + func (  x,  y, -z ) + &
           func (  x, -y,  z ) + func (  x, -y, -z ) + &
           func ( -x,  y,  z ) + func ( -x,  y, -z ) + &
           func ( -x, -y,  z ) + func ( -x, -y, -z ) )

  volume = cube_unit_volume_nd ( 3 )
  result = quad * volume

  return
end
subroutine cube_unit_nd ( func, qa, qb, n, k )
!
!*******************************************************************************
!
!! CUBE_UNIT_ND approximates an integral inside the unit cube in ND.
!
!
!  Discussion:
!
!    A K**N point product formula is used.
!
!  Integration region:
!
!    N dimensional points X() such that:
!
!      -1 <= X(I) <= 1 for all I.
!
!  Reference:
!
!    Lyness and McHugh,
!    Integration Over Multidimensional Hypercubes, A Progressive Procedure,
!    Computer J, volume 6, 1963, pages 264-270.
!
!    Arthur H Stroud,
!    Approximate Calculation of Multiple Integrals,
!    Prentice Hall, 1971.
!
!  Modified:
!
!    21 November 2000
!
!  Parameters:
!
!    Input, external FUNC, the name of the user supplied
!    function which evaluates the function, of the form
!
!      function func ( n, x )
!      integer n
!      double precision func
!      double precision x(n)
!
!    Output, double precision QA(K), QB(K), two sets of estimates for
!    the integral.  The QB entries are obtained from the
!    QA entries by Richardson extrapolation, and QB(K) is
!    the best estimate for the integral.
!
!    Input, integer N, the dimension of the cube.
!
!    Input, integer K, the highest order of integration, and the order
!    of Richardson extrapolation.  K can be no greater than 10.
!
  integer, parameter :: kmax = 10
!
  integer k
  integer n
!
  double precision func
  double precision g(kmax,kmax)
  integer i
  integer j
  double precision qa(k)
  double precision qb(k)
!
  external func
!
  save g
!
  g(1:kmax,1:kmax) = 0.0D+00

  g( 1, 1) =   1.0D+00
  g( 2, 1) = - 0.3333333333333D+00
  g( 2, 2) =   0.1333333333333D+01
  g( 3, 1) =   0.4166666666667D-01
  g( 3, 2) = - 0.1066666666667D+01
  g( 3, 3) =   0.2025000000000D+01
  g( 4, 1) = - 0.2777777777778D-02
  g( 4, 2) =   0.3555555555556D+00
  g( 4, 3) = - 0.2603571428571D+01
  g( 4, 4) =   0.3250793650794D+01
  g( 5, 1) =   0.1157407407407D-03
  g( 5, 2) = - 0.6772486772487D-01
  g( 5, 3) =   0.1464508928571D+01
  g( 5, 4) = - 0.5779188712522D+01
  g( 5, 5) =   0.5382288910935D+01
  g( 6, 1) = - 0.3306878306878D-05
  g( 6, 2) =   0.8465608465608D-02
  g( 6, 3) = - 0.4881696428571D+00
  g( 6, 4) =   0.4623350970018D+01
  g( 6, 5) = - 0.1223247479758D+02
  g( 6, 6) =   0.9088831168831D+01
  g( 7, 1) =   0.6889329805996D-07
  g( 7, 2) = - 0.7524985302763D-03
  g( 7, 3) =   0.1098381696429D+00
  g( 7, 4) = - 0.2241624712736D+01
  g( 7, 5) =   0.1274216124748D+02
  g( 7, 6) = - 0.2516907092907D+02
  g( 7, 7) =   0.1555944865432D+02
  g( 8, 1) = - 0.1093544413650D-08
  g( 8, 2) =   0.5016656868509D-04
  g( 8, 3) = - 0.1797351866883D-01
  g( 8, 4) =   0.7472082375786D+00
  g( 8, 5) = - 0.8168052081717D+01
  g( 8, 6) =   0.3236023405166D+02
  g( 8, 7) = - 0.5082753227079D+02
  g( 8, 8) =   0.2690606541646D+02
  g( 9, 1) =   0.1366930517063D-10
  g( 9, 2) = - 0.2606055516108D-05
  g( 9, 3) =   0.2246689833604D-02
  g( 9, 4) = - 0.1839281815578D+00
  g( 9, 5) =   0.3646451822195D+01
  g( 9, 6) = - 0.2588818724133D+02
  g( 9, 7) =   0.7782965878964D+02
  g( 9, 8) = - 0.1012934227443D+03
  g( 9, 9) =   0.4688718347156D+02
  g(10, 1) = - 0.1380737896023D-12
  g(10, 2) =   0.1085856465045D-06
  g(10, 3) = - 0.2222000934334D-03
  g(10, 4) =   0.3503393934435D-01
  g(10, 5) = - 0.1215483940732D+01
  g(10, 6) =   0.1456210532325D+02
  g(10, 7) = - 0.7477751530769D+02
  g(10, 8) =   0.1800771959898D+03
  g(10, 9) = - 0.1998874663788D+03
  g(10,10) =   0.8220635246624D+02
!
  if ( k > kmax ) then
    write ( *, * ) ' '
    write ( *, * ) 'CUBE_UNIT_ND - Fatal error!'
    write ( *, * ) '  K must be no greater than KMAX = ', kmax
    write ( *, * ) '  but the input K is ', k
    stop
  end if

  do i = 1, k
    call qmdpt ( func, n, i, qa(i) )
  end do

  qb(1) = qa(1)

  do i = 2, k
    qb(i) = 0.0D+00
    do j = 1, i
      qb(i) = qb(i) + g(i,j) * qa(j)
    end do
  end do

  return
end
function cube_unit_volume_nd ( n )
!
!*******************************************************************************
!
!! CUBE_UNIT_VOLUME_ND returns the volume of the unit cube in ND.
!
!
!  Modified:
!
!    07 November 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the dimension of the space.
!
!    Output, double precision CUBE_UNIT_VOLUME_ND, the volume of the unit
!    cube in ND.
!
  double precision cube_unit_volume_nd
  integer n
!
  cube_unit_volume_nd = 2.0D+00**n

  return
end
subroutine d_swap ( x, y )
!
!*******************************************************************************
!
!! D_SWAP switches two double precision values.
!
!
!  Modified:
!
!    01 May 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, double precision X, Y.  On output, the values of X and
!    Y have been interchanged.
!
  double precision x
  double precision y
  double precision z
!
  z = x
  x = y
  y = z

  return
end
subroutine d_swap3 ( x1, x2, x3 )
!
!*******************************************************************************
!
!! D_SWAP3 swaps three double precision items.
!
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, double precision X1, X2, X3.
!
!    On output, the values have been shifted so that
!
!      X1 := X3;
!      X2 := X1;
!      X3 := X2;
!
  double precision x0
  double precision x1
  double precision x2
  double precision x3
!
  x0 = x1
  x1 = x3
  x3 = x2
  x2 = x0

  return
end
subroutine dge_check ( lda, n, ierror )
!
!*******************************************************************************
!
!! DGE_CHECK checks the dimensions of a general matrix.
!
!
!  Modified:
!
!    16 October 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer LDA, the leading dimension of the array.
!    LDA must be at least N.
!
!    Input, integer N, the order of the matrix.
!    N must be positive.
!
!    Output, integer IERROR, reports whether any errors were detected.
!    IERROR is set to 0 before the checks are made, and then:
!    IERROR = IERROR + 1 if LDA is illegal;
!    IERROR = IERROR + 2 if N is illegal.
!
  integer ierror
  integer lda
  integer n
!
  ierror = 0

  if ( lda < n ) then
    ierror = ierror + 1
    write ( *, * ) ' '
    write ( *, * ) 'DGE_CHECK - Illegal LDA  < N.'
    write ( *, * ) '  LDA = ', lda
    write ( *, * ) '  N =   ', n
  end if

  if ( n < 1 ) then
    ierror = ierror + 2
    write ( *, * ) ' '
    write ( *, * ) 'DGE_CHECK - Illegal N = ', n
  end if

  return
end
subroutine dge_det ( a, lda, n, ipivot, det )
!
!*******************************************************************************
!
!! DGE_DET computes the determinant of a matrix factored by DGE_FA or DGE_TRF.
!
!
!  Modified:
!
!    19 October 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, double precision A(LDA,N), the LU factors computed by DGE_FA
!    or DGE_TRF.
!
!    Input, integer LDA, the leading dimension of the array.
!    LDA must be at least N.
!
!    Input, integer N, the order of the matrix.
!    N must be positive.
!
!    Input, integer IPIVOT(N), as computed by DGE_FA or DGE_TRF.
!
!    Output, double precision DET, the determinant of the matrix.
!
  integer lda
  integer n
!
  double precision a(lda,n)
  double precision det
  integer i
  integer ierror
  integer ipivot(n)
!
!  Check the dimensions.
!
  call dge_check ( lda, n, ierror )

  if ( ierror /= 0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'DGE_DET - Fatal error!'
    write ( *, * ) '  Illegal dimensions.'
    return
  end if

  det = 1.0D+00

  do i = 1, n
    det = det * a(i,i)
  end do

  do i = 1, n
    if ( ipivot(i) /= i ) then
      det = - det
    end if
  end do

  return
end
subroutine dge_fa ( a, lda, n, ipivot, info )
!
!*******************************************************************************
!
!! DGE_FA factors a general matrix.
!
!
!  Note:
!
!    DGE_FA is a simplified version of the LINPACK routine DGEFA.
!
!  Modified:
!
!    04 March 1999
!
!  Parameters:
!
!    Input/output, double precision A(LDA,N), the matrix to be factored.
!    On output, A contains an upper triangular matrix and the multipliers
!    which were used to obtain it.  The factorization can be written
!    A = L * U, where L is a product of permutation and unit lower
!    triangular matrices and U is upper triangular.
!
!    Input, integer LDA, the leading dimension of the array.
!    LDA must be at least N.
!
!    Input, integer N, the order of the matrix.
!    N must be positive.
!
!    Output, integer IPIVOT(N), a vector of pivot indices.
!
!    Output, integer INFO, singularity flag.
!    0, no singularity detected.
!    nonzero, the factorization failed on the INFO-th step.
!
  integer lda
  integer n
!
  double precision a(lda,n)
  integer i
  integer ierror
  integer info
  integer ipivot(n)
  integer j
  integer k
  integer l
  double precision t
!
!  Check the dimensions.
!
  call dge_check ( lda, n, ierror )

  if ( ierror /= 0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'DGE_FA - Fatal error!'
    write ( *, * ) '  Illegal dimensions.'
    return
  end if

  info = 0

  do k = 1, n-1
!
!  Find L, the index of the pivot row.
!
    l = k
    do i = k+1, n
      if ( abs ( a(i,k) ) > abs ( a(l,k) ) ) then
        l = i
      end if
    end do

    ipivot(k) = l
!
!  If the pivot index is zero, the algorithm has failed.
!
    if ( a(l,k) == 0.0D+00 ) then
      info = k
      write ( *, * ) ' '
      write ( *, * ) 'DGE_FA - Fatal error!'
      write ( *, * ) '  Zero pivot on step ', info
      return
    end if
!
!  Interchange rows L and K if necessary.
!
    if ( l /= k ) then
      call d_swap ( a(l,k), a(k,k) )
    end if
!
!  Normalize the values that lie below the pivot entry A(K,K).
!
    a(k+1:n,k) = - a(k+1:n,k) / a(k,k)
!
!  Row elimination with column indexing.
!
    do j = k+1, n

      if ( l /= k ) then
        call d_swap ( a(l,j), a(k,j) )
      end if

      a(k+1:n,j) = a(k+1:n,j) + a(k+1:n,k) * a(k,j)

    end do

  end do

  ipivot(n) = n

  if ( a(n,n) == 0.0D+00 ) then
    info = n
    write ( *, * ) ' '
    write ( *, * ) 'SGE_FA - Fatal error!'
    write ( *, * ) '  Zero pivot on step ', info
  end if

  return
end
subroutine dvec_even_select ( xlo, xhi, n, ival, xval )
!
!*******************************************************************************
!
!! DVEC_EVEN_SELECT returns the I-th of N evenly spaced values in [ XLO, XHI ].
!
!
!  Formula:
!
!    XVAL = ( (N-IVAL) * XLO + (IVAL-1) * XHI ) / dble ( N - 1 )
!
!  Modified:
!
!    31 October 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, double precision XLO, XHI, the low and high values.
!
!    Input, integer N, the number of values.
!
!    Input, integer IVAL, the index of the desired point.
!    IVAL is normally between 1 and N, but may be any
!    integer value.
!
!    Output, double precision XVAL, the IVAL-th of N evenly spaced values
!    between XLO and XHI.
!
!    Unless N = 1, X(1) = XLO and X(N) = XHI.
!
!    If N = 1, then X(1) = 0.5*(XLO+XHI).
!
  integer n
!
  integer ival
  double precision xhi
  double precision xlo
  double precision xval
!
  if ( n == 1 ) then

    xval = 0.5D+00 * ( xlo + xhi )

  else

    xval = ( dble ( n - ival ) * xlo + dble ( ival - 1 ) * xhi ) / dble ( n - 1 )

  end if

  return
end
function ellipse_area_2d ( r1, r2 )
!
!*******************************************************************************
!
!! ELLIPSE_AREA_2D returns the area of an ellipse in 2D.
!
!
!  Modified:
!
!    16 November 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, double precision R1, R2, the major and minor semi-axes.
!
!    Output, double precision ELLIPSE_AREA_2D, the area of the ellipse.
!
  double precision ellipse_area_2d
  double precision pi
  double precision r1
  double precision r2
!
  ellipse_area_2d = pi ( ) * r1 * r2

  return
end
function hexagon_area_2d ( radius )
!
!*******************************************************************************
!
!! HEXAGON_AREA_2D returns the area of a regular hexagon in 2D.
!
!
!  Modified:
!
!    16 November 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, double precision RADIUS, the radius of the hexagon.
!
!    Output, double precision HEXAGON_AREA_2D, the area of the hexagon.
!
  double precision hexagon_area_2d
  double precision hexagon_unit_area_2d
  double precision radius
!
  hexagon_area_2d = radius**2 * hexagon_unit_area_2d ( )

  return
end
subroutine hexagon_sum ( func, xc, yc, radius, norder, xtab, ytab, weight, &
  result )
!
!*******************************************************************************
!
!! HEXAGON_SUM applies a quadrature rule inside a hexagon in 2D.
!
!
!  Integration region:
!
!    The definition is given in terms of the angle in degrees of the
!    vector (X,Y)-(XC,YC):
!
!        0 and  60: Y-YC = - SQRT(3) * X-XC + RADIUS * SQRT(3)
!       60 and 120: Y-YC =                    RADIUS * SQRT(3)/2
!      120 and 180: Y-YC =   SQRT(3) * X-XC + RADIUS * SQRT(3)
!      180 and 240: Y-YC = - SQRT(3) * X-XC - RADIUS * SQRT(3)
!      240 and 300: Y-YC =                  - RADIUS * SQRT(3)/2
!      300 and 360: Y-YC =   SQRT(3) * X-XC - RADIUS * SQRT(3)
!
!  Modified:
!
!    06 November 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, external FUNC, the name of the user supplied
!    function of two variables which is to be integrated,
!    of the form:
!
!      function func ( x, y )
!      double precision func
!      double precision x
!      double precision y
!
!    Input, double precision XC, YC, the coordinates of the center of the hexagon.
!
!    Input, double precision RADIUS, the radius of the hexagon.
!
!    Input, integer NORDER, the order of the rule.
!
!    Input, double precision XTAB(NORDER), YTAB(NORDER), the abscissas.
!
!    Input, double precision WEIGHT(NORDER), the weights of the rule.
!
!    Output, double precision RESULT, the approximate integral of the function.
!
  integer norder
!
  double precision func
  double precision hexagon_area_2d
  integer i
  double precision pi
  double precision quad
  double precision radius
  double precision result
  double precision volume
  double precision weight(norder)
  double precision x
  double precision xc
  double precision xtab(norder)
  double precision y
  double precision yc
  double precision ytab(norder)
!
  external func
!
  quad = 0.0D+00
  do i = 1, norder
    x = xc + radius * xtab(i)
    y = yc + radius * ytab(i)
    quad = quad + weight(i) * func ( x, y )
  end do

  volume = hexagon_area_2d ( radius )
  result = quad * volume

  return
end
function hexagon_unit_area_2d ( )
!
!*******************************************************************************
!
!! HEXAGON_UNIT_AREA_2D returns the area of a unit regular hexagon in 2D.
!
!
!  Modified:
!
!    07 November 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, double precision HEXAGON_UNIT_AREA_2D, the area of the hexagon.
!
  double precision hexagon_unit_area_2d
  double precision rad
!
  hexagon_unit_area_2d = 3.0D+00 * sqrt ( 3.0D+00 ) / 2.0D+00

  return
end
subroutine hexagon_unit_set ( rule, norder, xtab, ytab, weight )
!
!*******************************************************************************
!
!! HEXAGON_UNIT_SET sets a quadrature rule inside the unit hexagon in 2D.
!
!
!  Integration region:
!
!    The definition is given in terms of the angle in degrees of the
!    vector (X,Y):
!
!        0 and  60: Y = - SQRT(3) * X + SQRT(3)
!       60 and 120: Y =                 SQRT(3)/2
!      120 and 180: Y =   SQRT(3) * X + SQRT(3)
!      180 and 240: Y = - SQRT(3) * X - SQRT(3)
!      240 and 300: Y =               - SQRT(3)/2
!      300 and 360: Y =   SQRT(3) * X - SQRT(3)
!
!    or, the convex hull of
!
!      (1,0), (0.5,A), (-0.5,A), (-1,0), (-0.5,-A), (0.5,A)
!
!    where A = SQRT(3)/2.
!
!  Reference:
!
!    Abramowitz and Stegun,
!    Handbook of Mathematical Functions,
!    National Bureau of Standards, 1964.
!
!    Arthur H Stroud,
!    Approximate Calculation of Multiple Integrals,
!    Prentice Hall, 1971.
!
!  Modified:
!
!    09 December 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer RULE, the rule desired.
!      1, 1 point,  degree 1;
!      2, 4 points, degree 3;
!      3, 7 points, degree 3;
!      4, 7 points, degree 5;
!
!    Output, integer NORDER, the order of the desired rule.
!    If RULE is not legal, then NORDER is returned as 0.
!
!    Output, double precision XTAB(*), YTAB(*), the abscissas of the rule.
!
!    Output, double precision WEIGHT(*), the NORDER weights of the rule.
!
  double precision a
  double precision b
  double precision c
  double precision d
  double precision e
  integer norder
  integer rule
  double precision weight(*)
  double precision xtab(*)
  double precision ytab(*)
  double precision z
!
  if ( rule == 1 ) then

    norder = 1
    xtab(1) = 0.0D+00
    ytab(1) = 0.0D+00
    weight(1) = 1.0D+00
!
!  Stroud rule H2:3-1.
!
  else if ( rule == 2 ) then

    a = sqrt ( 5.0D+00 / 12.0D+00 )
    b = 1.0D+00 / 4.0D+00
    z = 0.0D+00

    norder = 4
    xtab(1:4) =   (/  a, -a,  z,  z /)
    ytab(1:4) =   (/  z,  z,  a, -a /)
    weight(1:4) = (/  b,  b,  b,  b /)
!
!  Stroud rule H2:3-2.
!
  else if ( rule == 3 ) then

    a = sqrt ( 3.0D+00 ) / 2.0D+00
    b =  0.5D+00
    c =  1.0D+00
    d =  5.0D+00 / 72.0D+00
    e = 42.0D+00 / 72.0D+00
    z =  0.0D+00

    norder = 7
    xtab(1:7) =   (/  z,  c, -c,  b, -b,  b, -b /)
    ytab(1:7) =   (/  z,  z,  z,  a,  a, -a, -a /)
    weight(1:7) = (/  e,  d,  d,  d,  d,  d,  d /)
!
!  Stroud rule H2:5-1.
!
  else if ( rule == 4 ) then

    a = sqrt ( 14.0D+00 ) / 5.0D+00
    b = sqrt ( 14.0D+00 ) / 10.0D+00
    c = sqrt ( 42.0D+00 ) / 10.0D+00
    d = 125.0D+00 / 1008.0D+00
    e = 258.0D+00 / 1008.0D+00
    z = 0.0D+00

    norder = 7
    xtab(1:7) =   (/ z,  a, -a,  b, -b,  b, -b /)
    ytab(1:7) =   (/ z,  z,  z,  c,  c, -c, -c /)
    weight(1:7) = (/ e,  d,  d,  d,  d,  d,  d /)

  else

    norder = 0

  end if

  return
end
subroutine ksub_next2 ( n, k, iarray, in, iout )
!
!*******************************************************************************
!
!! KSUB_NEXT2 computes the next K subset of an N set.
!
!
!  Discussion:
!
!    This routine uses the revolving door method.  It has no "memory".
!    It simply calculates the successor of the input set,
!    and will start from the beginning after the last set.
!
!  Reference:
!
!    A Nijenhuis and H Wilf,
!    Combinatorial Algorithms,
!    Academic Press, 1978, second edition,
!    ISBN 0-12-519260-6.
!
!  Modified:
!
!    15 April 1999
!
!  Parameters:
!
!    Input, integer N, the size of the set from which subsets are drawn.
!
!    Input, integer K, the size of the desired subset.  K must be
!    between 0 and N.
!
!    Input/output, integer IARRAY(K).  On input, the user must
!    supply a subset of size K in IARRAY.  That is, IARRAY must
!    contain K unique numbers, in order, between 1 and N.  On
!    output, IARRAY(I) is the I-th element of the output subset.
!    The output array is also in sorted order.
!
!    Output, integer IN, the element of the output subset which
!    was not in the input set.  Each new subset differs from the
!    last one by adding one element and deleting another.
!
!    Output, integer IOUT, the element of the input subset which
!    is not in the output subset.
!
  integer k
!
  integer iarray(k)
  integer in
  integer iout
  integer j
  integer m
  integer n
!
  if ( k < 0 .or. k > n ) then
    write ( *, * ) ' '
    write ( *, * ) 'KSUB_NEXT2 - Fatal error!'
    write ( *, * ) '  N = ', n, ' and K = ', k
    write ( *, * ) '  but 0 <= K <= N is required!'
    stop
  end if

  j = 0

  if ( mod ( k, 2 ) /= 0 ) then
    go to 20
  end if

10    continue

  j = j + 1

  if ( j > k ) then
    iarray(k) = k
    in = k
    iout = n
    return
  end if

  if ( iarray(j) /= j ) then

    iout = iarray(j)
    in = iout - 1
    iarray(j) = in

    if ( j /= 1 ) then
      in = j - 1
      iarray(j-1) = in
    end if

    return

  end if

20    continue

  j = j + 1
  m = n

  if ( j < k ) then
    m = iarray(j+1) - 1
  end if

  if ( m == iarray(j) ) then
    go to 10
  end if

  in = iarray(j) + 1
  iarray(j) = in
  iout = in - 1

  if ( j /= 1 ) then
    iarray(j-1) = iout
    iout = j - 1
  end if

  return
end
subroutine legendre_set ( norder, xtab, weight )
!
!*******************************************************************************
!
!! LEGENDRE_SET sets abscissas and weights for Gauss-Legendre quadrature.
!
!
!  Integration interval:
!
!    [ -1, 1 ]
!
!  Weight function:
!
!    1.0D+00
!
!  Integral to approximate:
!
!    INTEGRAL ( -1 <= X <= 1 ) F(X) dX
!
!  Approximate integral:
!
!    SUM ( I = 1 to NORDER ) WEIGHT(I) * F ( XTAB(I) )
!
!  Precision:
!
!    The quadrature rule will integrate exactly all polynomials up to
!    X**(2*NORDER-1).
!
!  Note:
!
!    The abscissas of the rule are the zeroes of the Legendre polynomial
!    P(NORDER)(X).
!
!    The integral produced by a Gauss-Legendre rule is equal to the
!    integral of the unique polynomial of degree NORDER-1 which
!    agrees with the function at the NORDER abscissas of the rule.
!
!  Reference:
!
!    Abramowitz and Stegun,
!    Handbook of Mathematical Functions,
!    National Bureau of Standards, 1964.
!
!    Vladimir Krylov,
!    Approximate Calculation of Integrals,
!    MacMillan, 1962.
!
!    Arthur Stroud and Don Secrest,
!    Gaussian Quadrature Formulas,
!    Prentice Hall, 1966.
!
!    Daniel Zwillinger, editor,
!    Standard Mathematical Tables and Formulae,
!    30th Edition,
!    CRC Press, 1996.
!
!  Modified:
!
!    18 December 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer NORDER, the order of the rule.
!    NORDER must be between 1 and 20, 32 or 64.
!
!    Output, double precision XTAB(NORDER), the abscissas of the rule.
!
!    Output, double precision WEIGHT(NORDER), the weights of the rule.
!    The weights are positive, symmetric and should sum to 2.
!
  integer norder
!
  double precision xtab(norder)
  double precision weight(norder)
!
  if ( norder == 1 ) then

    xtab(1) =   0.0D+00

    weight(1) = 2.0D+00

  else if ( norder == 2 ) then

    xtab(1) = - 0.577350269189625764509148780502D+00
    xtab(2) =   0.577350269189625764509148780502D+00

    weight(1) = 1.0D+00
    weight(2) = 1.0D+00

  else if ( norder == 3 ) then

    xtab(1) = - 0.774596669241483377035853079956D+00
    xtab(2) =   0.0D+00
    xtab(3) =   0.774596669241483377035853079956D+00

    weight(1) = 5.0D+00 / 9.0D+00
    weight(2) = 8.0D+00 / 9.0D+00
    weight(3) = 5.0D+00 / 9.0D+00

  else if ( norder == 4 ) then

    xtab(1) = - 0.861136311594052575223946488893D+00
    xtab(2) = - 0.339981043584856264802665759103D+00
    xtab(3) =   0.339981043584856264802665759103D+00
    xtab(4) =   0.861136311594052575223946488893D+00

    weight(1) = 0.347854845137453857373063949222D+00
    weight(2) = 0.652145154862546142626936050778D+00
    weight(3) = 0.652145154862546142626936050778D+00
    weight(4) = 0.347854845137453857373063949222D+00

  else if ( norder == 5 ) then

    xtab(1) = - 0.906179845938663992797626878299D+00
    xtab(2) = - 0.538469310105683091036314420700D+00
    xtab(3) =   0.0D+00
    xtab(4) =   0.538469310105683091036314420700D+00
    xtab(5) =   0.906179845938663992797626878299D+00

    weight(1) = 0.236926885056189087514264040720D+00
    weight(2) = 0.478628670499366468041291514836D+00
    weight(3) = 0.568888888888888888888888888889D+00
    weight(4) = 0.478628670499366468041291514836D+00
    weight(5) = 0.236926885056189087514264040720D+00

  else if ( norder == 6 ) then

    xtab(1) = - 0.932469514203152027812301554494D+00
    xtab(2) = - 0.661209386466264513661399595020D+00
    xtab(3) = - 0.238619186083196908630501721681D+00
    xtab(4) =   0.238619186083196908630501721681D+00
    xtab(5) =   0.661209386466264513661399595020D+00
    xtab(6) =   0.932469514203152027812301554494D+00

    weight(1) = 0.171324492379170345040296142173D+00
    weight(2) = 0.360761573048138607569833513838D+00
    weight(3) = 0.467913934572691047389870343990D+00
    weight(4) = 0.467913934572691047389870343990D+00
    weight(5) = 0.360761573048138607569833513838D+00
    weight(6) = 0.171324492379170345040296142173D+00

  else if ( norder == 7 ) then

    xtab(1) = - 0.949107912342758524526189684048D+00
    xtab(2) = - 0.741531185599394439863864773281D+00
    xtab(3) = - 0.405845151377397166906606412077D+00
    xtab(4) =   0.0D+00
    xtab(5) =   0.405845151377397166906606412077D+00
    xtab(6) =   0.741531185599394439863864773281D+00
    xtab(7) =   0.949107912342758524526189684048D+00

    weight(1) = 0.129484966168869693270611432679D+00
    weight(2) = 0.279705391489276667901467771424D+00
    weight(3) = 0.381830050505118944950369775489D+00
    weight(4) = 0.417959183673469387755102040816D+00
    weight(5) = 0.381830050505118944950369775489D+00
    weight(6) = 0.279705391489276667901467771424D+00
    weight(7) = 0.129484966168869693270611432679D+00

  else if ( norder == 8 ) then

    xtab(1) = - 0.960289856497536231683560868569D+00
    xtab(2) = - 0.796666477413626739591553936476D+00
    xtab(3) = - 0.525532409916328985817739049189D+00
    xtab(4) = - 0.183434642495649804939476142360D+00
    xtab(5) =   0.183434642495649804939476142360D+00
    xtab(6) =   0.525532409916328985817739049189D+00
    xtab(7) =   0.796666477413626739591553936476D+00
    xtab(8) =   0.960289856497536231683560868569D+00

    weight(1) = 0.101228536290376259152531354310D+00
    weight(2) = 0.222381034453374470544355994426D+00
    weight(3) = 0.313706645877887287337962201987D+00
    weight(4) = 0.362683783378361982965150449277D+00
    weight(5) = 0.362683783378361982965150449277D+00
    weight(6) = 0.313706645877887287337962201987D+00
    weight(7) = 0.222381034453374470544355994426D+00
    weight(8) = 0.101228536290376259152531354310D+00

  else if ( norder == 9 ) then

    xtab(1) = - 0.968160239507626089835576202904D+00
    xtab(2) = - 0.836031107326635794299429788070D+00
    xtab(3) = - 0.613371432700590397308702039341D+00
    xtab(4) = - 0.324253423403808929038538014643D+00
    xtab(5) =   0.0D+00
    xtab(6) =   0.324253423403808929038538014643D+00
    xtab(7) =   0.613371432700590397308702039341D+00
    xtab(8) =   0.836031107326635794299429788070D+00
    xtab(9) =   0.968160239507626089835576202904D+00

    weight(1) = 0.812743883615744119718921581105D-01
    weight(2) = 0.180648160694857404058472031243D+00
    weight(3) = 0.260610696402935462318742869419D+00
    weight(4) = 0.312347077040002840068630406584D+00
    weight(5) = 0.330239355001259763164525069287D+00
    weight(6) = 0.312347077040002840068630406584D+00
    weight(7) = 0.260610696402935462318742869419D+00
    weight(8) = 0.180648160694857404058472031243D+00
    weight(9) = 0.812743883615744119718921581105D-01

  else if ( norder == 10 ) then

    xtab(1) =  - 0.973906528517171720077964012084D+00
    xtab(2) =  - 0.865063366688984510732096688423D+00
    xtab(3) =  - 0.679409568299024406234327365115D+00
    xtab(4) =  - 0.433395394129247190799265943166D+00
    xtab(5) =  - 0.148874338981631210884826001130D+00
    xtab(6) =    0.148874338981631210884826001130D+00
    xtab(7) =    0.433395394129247190799265943166D+00
    xtab(8) =    0.679409568299024406234327365115D+00
    xtab(9) =    0.865063366688984510732096688423D+00
    xtab(10) =   0.973906528517171720077964012084D+00

    weight(1) =  0.666713443086881375935688098933D-01
    weight(2) =  0.149451349150580593145776339658D+00
    weight(3) =  0.219086362515982043995534934228D+00
    weight(4) =  0.269266719309996355091226921569D+00
    weight(5) =  0.295524224714752870173892994651D+00
    weight(6) =  0.295524224714752870173892994651D+00
    weight(7) =  0.269266719309996355091226921569D+00
    weight(8) =  0.219086362515982043995534934228D+00
    weight(9) =  0.149451349150580593145776339658D+00
    weight(10) = 0.666713443086881375935688098933D-01

  else if ( norder == 11 ) then

    xtab(1) =  - 0.978228658146056992803938001123D+00
    xtab(2) =  - 0.887062599768095299075157769304D+00
    xtab(3) =  - 0.730152005574049324093416252031D+00
    xtab(4) =  - 0.519096129206811815925725669459D+00
    xtab(5) =  - 0.269543155952344972331531985401D+00
    xtab(6) =    0.0D+00
    xtab(7) =    0.269543155952344972331531985401D+00
    xtab(8) =    0.519096129206811815925725669459D+00
    xtab(9) =    0.730152005574049324093416252031D+00
    xtab(10) =   0.887062599768095299075157769304D+00
    xtab(11) =   0.978228658146056992803938001123D+00

    weight(1) =  0.556685671161736664827537204425D-01
    weight(2) =  0.125580369464904624634694299224D+00
    weight(3) =  0.186290210927734251426097641432D+00
    weight(4) =  0.233193764591990479918523704843D+00
    weight(5) =  0.262804544510246662180688869891D+00
    weight(6) =  0.272925086777900630714483528336D+00
    weight(7) =  0.262804544510246662180688869891D+00
    weight(8) =  0.233193764591990479918523704843D+00
    weight(9) =  0.186290210927734251426097641432D+00
    weight(10) = 0.125580369464904624634694299224D+00
    weight(11) = 0.556685671161736664827537204425D-01

  else if ( norder == 12 ) then

    xtab(1) =  - 0.981560634246719250690549090149D+00
    xtab(2) =  - 0.904117256370474856678465866119D+00
    xtab(3) =  - 0.769902674194304687036893833213D+00
    xtab(4) =  - 0.587317954286617447296702418941D+00
    xtab(5) =  - 0.367831498998180193752691536644D+00
    xtab(6) =  - 0.125233408511468915472441369464D+00
    xtab(7) =    0.125233408511468915472441369464D+00
    xtab(8) =    0.367831498998180193752691536644D+00
    xtab(9) =    0.587317954286617447296702418941D+00
    xtab(10) =   0.769902674194304687036893833213D+00
    xtab(11) =   0.904117256370474856678465866119D+00
    xtab(12) =   0.981560634246719250690549090149D+00

    weight(1) =  0.471753363865118271946159614850D-01
    weight(2) =  0.106939325995318430960254718194D+00
    weight(3) =  0.160078328543346226334652529543D+00
    weight(4) =  0.203167426723065921749064455810D+00
    weight(5) =  0.233492536538354808760849898925D+00
    weight(6) =  0.249147045813402785000562436043D+00
    weight(7) =  0.249147045813402785000562436043D+00
    weight(8) =  0.233492536538354808760849898925D+00
    weight(9) =  0.203167426723065921749064455810D+00
    weight(10) = 0.160078328543346226334652529543D+00
    weight(11) = 0.106939325995318430960254718194D+00
    weight(12) = 0.471753363865118271946159614850D-01

  else if ( norder == 13 ) then

    xtab(1) =  - 0.984183054718588149472829448807D+00
    xtab(2) =  - 0.917598399222977965206547836501D+00
    xtab(3) =  - 0.801578090733309912794206489583D+00
    xtab(4) =  - 0.642349339440340220643984606996D+00
    xtab(5) =  - 0.448492751036446852877912852128D+00
    xtab(6) =  - 0.230458315955134794065528121098D+00
    xtab(7) =    0.0D+00
    xtab(8) =    0.230458315955134794065528121098D+00
    xtab(9) =    0.448492751036446852877912852128D+00
    xtab(10) =   0.642349339440340220643984606996D+00
    xtab(11) =   0.801578090733309912794206489583D+00
    xtab(12) =   0.917598399222977965206547836501D+00
    xtab(13) =   0.984183054718588149472829448807D+00

    weight(1) =  0.404840047653158795200215922010D-01
    weight(2) =  0.921214998377284479144217759538D-01
    weight(3) =  0.138873510219787238463601776869D+00
    weight(4) =  0.178145980761945738280046691996D+00
    weight(5) =  0.207816047536888502312523219306D+00
    weight(6) =  0.226283180262897238412090186040D+00
    weight(7) =  0.232551553230873910194589515269D+00
    weight(8) =  0.226283180262897238412090186040D+00
    weight(9) =  0.207816047536888502312523219306D+00
    weight(10) = 0.178145980761945738280046691996D+00
    weight(11) = 0.138873510219787238463601776869D+00
    weight(12) = 0.921214998377284479144217759538D-01
    weight(13) = 0.404840047653158795200215922010D-01

  else if ( norder == 14 ) then

    xtab(1) =  - 0.986283808696812338841597266704D+00
    xtab(2) =  - 0.928434883663573517336391139378D+00
    xtab(3) =  - 0.827201315069764993189794742650D+00
    xtab(4) =  - 0.687292904811685470148019803019D+00
    xtab(5) =  - 0.515248636358154091965290718551D+00
    xtab(6) =  - 0.319112368927889760435671824168D+00
    xtab(7) =  - 0.108054948707343662066244650220D+00
    xtab(8) =    0.108054948707343662066244650220D+00
    xtab(9) =    0.319112368927889760435671824168D+00
    xtab(10) =   0.515248636358154091965290718551D+00
    xtab(11) =   0.687292904811685470148019803019D+00
    xtab(12) =   0.827201315069764993189794742650D+00
    xtab(13) =   0.928434883663573517336391139378D+00
    xtab(14) =   0.986283808696812338841597266704D+00

    weight(1) =  0.351194603317518630318328761382D-01
    weight(2) =  0.801580871597602098056332770629D-01
    weight(3) =  0.121518570687903184689414809072D+00
    weight(4) =  0.157203167158193534569601938624D+00
    weight(5) =  0.185538397477937813741716590125D+00
    weight(6) =  0.205198463721295603965924065661D+00
    weight(7) =  0.215263853463157790195876443316D+00
    weight(8) =  0.215263853463157790195876443316D+00
    weight(9) =  0.205198463721295603965924065661D+00
    weight(10) = 0.185538397477937813741716590125D+00
    weight(11) = 0.157203167158193534569601938624D+00
    weight(12) = 0.121518570687903184689414809072D+00
    weight(13) = 0.801580871597602098056332770629D-01
    weight(14) = 0.351194603317518630318328761382D-01

  else if ( norder == 15 ) then

    xtab(1) =  - 0.987992518020485428489565718587D+00
    xtab(2) =  - 0.937273392400705904307758947710D+00
    xtab(3) =  - 0.848206583410427216200648320774D+00
    xtab(4) =  - 0.724417731360170047416186054614D+00
    xtab(5) =  - 0.570972172608538847537226737254D+00
    xtab(6) =  - 0.394151347077563369897207370981D+00
    xtab(7) =  - 0.201194093997434522300628303395D+00
    xtab(8) =    0.0D+00
    xtab(9) =    0.201194093997434522300628303395D+00
    xtab(10) =   0.394151347077563369897207370981D+00
    xtab(11) =   0.570972172608538847537226737254D+00
    xtab(12) =   0.724417731360170047416186054614D+00
    xtab(13) =   0.848206583410427216200648320774D+00
    xtab(14) =   0.937273392400705904307758947710D+00
    xtab(15) =   0.987992518020485428489565718587D+00

    weight(1) =  0.307532419961172683546283935772D-01
    weight(2) =  0.703660474881081247092674164507D-01
    weight(3) =  0.107159220467171935011869546686D+00
    weight(4) =  0.139570677926154314447804794511D+00
    weight(5) =  0.166269205816993933553200860481D+00
    weight(6) =  0.186161000015562211026800561866D+00
    weight(7) =  0.198431485327111576456118326444D+00
    weight(8) =  0.202578241925561272880620199968D+00
    weight(9) =  0.198431485327111576456118326444D+00
    weight(10) = 0.186161000015562211026800561866D+00
    weight(11) = 0.166269205816993933553200860481D+00
    weight(12) = 0.139570677926154314447804794511D+00
    weight(13) = 0.107159220467171935011869546686D+00
    weight(14) = 0.703660474881081247092674164507D-01
    weight(15) = 0.307532419961172683546283935772D-01

  else if ( norder == 16 ) then

    xtab(1) =  - 0.989400934991649932596154173450D+00
    xtab(2) =  - 0.944575023073232576077988415535D+00
    xtab(3) =  - 0.865631202387831743880467897712D+00
    xtab(4) =  - 0.755404408355003033895101194847D+00
    xtab(5) =  - 0.617876244402643748446671764049D+00
    xtab(6) =  - 0.458016777657227386342419442984D+00
    xtab(7) =  - 0.281603550779258913230460501460D+00
    xtab(8) =  - 0.950125098376374401853193354250D-01
    xtab(9) =    0.950125098376374401853193354250D-01
    xtab(10) =   0.281603550779258913230460501460D+00
    xtab(11) =   0.458016777657227386342419442984D+00
    xtab(12) =   0.617876244402643748446671764049D+00
    xtab(13) =   0.755404408355003033895101194847D+00
    xtab(14) =   0.865631202387831743880467897712D+00
    xtab(15) =   0.944575023073232576077988415535D+00
    xtab(16) =   0.989400934991649932596154173450D+00

    weight(1) =  0.271524594117540948517805724560D-01
    weight(2) =  0.622535239386478928628438369944D-01
    weight(3) =  0.951585116824927848099251076022D-01
    weight(4) =  0.124628971255533872052476282192D+00
    weight(5) =  0.149595988816576732081501730547D+00
    weight(6) =  0.169156519395002538189312079030D+00
    weight(7) =  0.182603415044923588866763667969D+00
    weight(8) =  0.189450610455068496285396723208D+00
    weight(9) =  0.189450610455068496285396723208D+00
    weight(10) = 0.182603415044923588866763667969D+00
    weight(11) = 0.169156519395002538189312079030D+00
    weight(12) = 0.149595988816576732081501730547D+00
    weight(13) = 0.124628971255533872052476282192D+00
    weight(14) = 0.951585116824927848099251076022D-01
    weight(15) = 0.622535239386478928628438369944D-01
    weight(16) = 0.271524594117540948517805724560D-01

  else if ( norder == 17 ) then

    xtab(1) =  - 0.990575475314417335675434019941D+00
    xtab(2) =  - 0.950675521768767761222716957896D+00
    xtab(3) =  - 0.880239153726985902122955694488D+00
    xtab(4) =  - 0.781514003896801406925230055520D+00
    xtab(5) =  - 0.657671159216690765850302216643D+00
    xtab(6) =  - 0.512690537086476967886246568630D+00
    xtab(7) =  - 0.351231763453876315297185517095D+00
    xtab(8) =  - 0.178484181495847855850677493654D+00
    xtab(9) =    0.0D+00
    xtab(10) =   0.178484181495847855850677493654D+00
    xtab(11) =   0.351231763453876315297185517095D+00
    xtab(12) =   0.512690537086476967886246568630D+00
    xtab(13) =   0.657671159216690765850302216643D+00
    xtab(14) =   0.781514003896801406925230055520D+00
    xtab(15) =   0.880239153726985902122955694488D+00
    xtab(16) =   0.950675521768767761222716957896D+00
    xtab(17) =   0.990575475314417335675434019941D+00

    weight(1) =  0.241483028685479319601100262876D-01
    weight(2) =  0.554595293739872011294401653582D-01
    weight(3) =  0.850361483171791808835353701911D-01
    weight(4) =  0.111883847193403971094788385626D+00
    weight(5) =  0.135136368468525473286319981702D+00
    weight(6) =  0.154045761076810288081431594802D+00
    weight(7) =  0.168004102156450044509970663788D+00
    weight(8) =  0.176562705366992646325270990113D+00
    weight(9) =  0.179446470356206525458265644262D+00
    weight(10) = 0.176562705366992646325270990113D+00
    weight(11) = 0.168004102156450044509970663788D+00
    weight(12) = 0.154045761076810288081431594802D+00
    weight(13) = 0.135136368468525473286319981702D+00
    weight(14) = 0.111883847193403971094788385626D+00
    weight(15) = 0.850361483171791808835353701911D-01
    weight(16) = 0.554595293739872011294401653582D-01
    weight(17) = 0.241483028685479319601100262876D-01

  else if ( norder == 18 ) then

    xtab(1) =  - 0.991565168420930946730016004706D+00
    xtab(2) =  - 0.955823949571397755181195892930D+00
    xtab(3) =  - 0.892602466497555739206060591127D+00
    xtab(4) =  - 0.803704958972523115682417455015D+00
    xtab(5) =  - 0.691687043060353207874891081289D+00
    xtab(6) =  - 0.559770831073947534607871548525D+00
    xtab(7) =  - 0.411751161462842646035931793833D+00
    xtab(8) =  - 0.251886225691505509588972854878D+00
    xtab(9) =  - 0.847750130417353012422618529358D-01
    xtab(10) =   0.847750130417353012422618529358D-01
    xtab(11) =   0.251886225691505509588972854878D+00
    xtab(12) =   0.411751161462842646035931793833D+00
    xtab(13) =   0.559770831073947534607871548525D+00
    xtab(14) =   0.691687043060353207874891081289D+00
    xtab(15) =   0.803704958972523115682417455015D+00
    xtab(16) =   0.892602466497555739206060591127D+00
    xtab(17) =   0.955823949571397755181195892930D+00
    xtab(18) =   0.991565168420930946730016004706D+00

    weight(1) =  0.216160135264833103133427102665D-01
    weight(2) =  0.497145488949697964533349462026D-01
    weight(3) =  0.764257302548890565291296776166D-01
    weight(4) =  0.100942044106287165562813984925D+00
    weight(5) =  0.122555206711478460184519126800D+00
    weight(6) =  0.140642914670650651204731303752D+00
    weight(7) =  0.154684675126265244925418003836D+00
    weight(8) =  0.164276483745832722986053776466D+00
    weight(9) =  0.169142382963143591840656470135D+00
    weight(10) = 0.169142382963143591840656470135D+00
    weight(11) = 0.164276483745832722986053776466D+00
    weight(12) = 0.154684675126265244925418003836D+00
    weight(13) = 0.140642914670650651204731303752D+00
    weight(14) = 0.122555206711478460184519126800D+00
    weight(15) = 0.100942044106287165562813984925D+00
    weight(16) = 0.764257302548890565291296776166D-01
    weight(17) = 0.497145488949697964533349462026D-01
    weight(18) = 0.216160135264833103133427102665D-01

  else if ( norder == 19 ) then

    xtab(1) =  - 0.992406843843584403189017670253D+00
    xtab(2) =  - 0.960208152134830030852778840688D+00
    xtab(3) =  - 0.903155903614817901642660928532D+00
    xtab(4) =  - 0.822714656537142824978922486713D+00
    xtab(5) =  - 0.720966177335229378617095860824D+00
    xtab(6) =  - 0.600545304661681023469638164946D+00
    xtab(7) =  - 0.464570741375960945717267148104D+00
    xtab(8) =  - 0.316564099963629831990117328850D+00
    xtab(9) =  - 0.160358645640225375868096115741D+00
    xtab(10) =   0.0D+00
    xtab(11) =   0.160358645640225375868096115741D+00
    xtab(12) =   0.316564099963629831990117328850D+00
    xtab(13) =   0.464570741375960945717267148104D+00
    xtab(14) =   0.600545304661681023469638164946D+00
    xtab(15) =   0.720966177335229378617095860824D+00
    xtab(16) =   0.822714656537142824978922486713D+00
    xtab(17) =   0.903155903614817901642660928532D+00
    xtab(18) =   0.960208152134830030852778840688D+00
    xtab(19) =   0.992406843843584403189017670253D+00

    weight(1) =  0.194617882297264770363120414644D-01
    weight(2) =  0.448142267656996003328381574020D-01
    weight(3) =  0.690445427376412265807082580060D-01
    weight(4) =  0.914900216224499994644620941238D-01
    weight(5) =  0.111566645547333994716023901682D+00
    weight(6) =  0.128753962539336227675515784857D+00
    weight(7) =  0.142606702173606611775746109442D+00
    weight(8) =  0.152766042065859666778855400898D+00
    weight(9) =  0.158968843393954347649956439465D+00
    weight(10) = 0.161054449848783695979163625321D+00
    weight(11) = 0.158968843393954347649956439465D+00
    weight(12) = 0.152766042065859666778855400898D+00
    weight(13) = 0.142606702173606611775746109442D+00
    weight(14) = 0.128753962539336227675515784857D+00
    weight(15) = 0.111566645547333994716023901682D+00
    weight(16) = 0.914900216224499994644620941238D-01
    weight(17) = 0.690445427376412265807082580060D-01
    weight(18) = 0.448142267656996003328381574020D-01
    weight(19) = 0.194617882297264770363120414644D-01

  else if ( norder == 20 ) then

    xtab(1) =  - 0.993128599185094924786122388471D+00
    xtab(2) =  - 0.963971927277913791267666131197D+00
    xtab(3) =  - 0.912234428251325905867752441203D+00
    xtab(4) =  - 0.839116971822218823394529061702D+00
    xtab(5) =  - 0.746331906460150792614305070356D+00
    xtab(6) =  - 0.636053680726515025452836696226D+00
    xtab(7) =  - 0.510867001950827098004364050955D+00
    xtab(8) =  - 0.373706088715419560672548177025D+00
    xtab(9) =  - 0.227785851141645078080496195369D+00
    xtab(10) = - 0.765265211334973337546404093988D-01
    xtab(11) =   0.765265211334973337546404093988D-01
    xtab(12) =   0.227785851141645078080496195369D+00
    xtab(13) =   0.373706088715419560672548177025D+00
    xtab(14) =   0.510867001950827098004364050955D+00
    xtab(15) =   0.636053680726515025452836696226D+00
    xtab(16) =   0.746331906460150792614305070356D+00
    xtab(17) =   0.839116971822218823394529061702D+00
    xtab(18) =   0.912234428251325905867752441203D+00
    xtab(19) =   0.963971927277913791267666131197D+00
    xtab(20) =   0.993128599185094924786122388471D+00

    weight(1) =  0.176140071391521183118619623519D-01
    weight(2) =  0.406014298003869413310399522749D-01
    weight(3) =  0.626720483341090635695065351870D-01
    weight(4) =  0.832767415767047487247581432220D-01
    weight(5) =  0.101930119817240435036750135480D+00
    weight(6) =  0.118194531961518417312377377711D+00
    weight(7) =  0.131688638449176626898494499748D+00
    weight(8) =  0.142096109318382051329298325067D+00
    weight(9) =  0.149172986472603746787828737002D+00
    weight(10) = 0.152753387130725850698084331955D+00
    weight(11) = 0.152753387130725850698084331955D+00
    weight(12) = 0.149172986472603746787828737002D+00
    weight(13) = 0.142096109318382051329298325067D+00
    weight(14) = 0.131688638449176626898494499748D+00
    weight(15) = 0.118194531961518417312377377711D+00
    weight(16) = 0.101930119817240435036750135480D+00
    weight(17) = 0.832767415767047487247581432220D-01
    weight(18) = 0.626720483341090635695065351870D-01
    weight(19) = 0.406014298003869413310399522749D-01
    weight(20) = 0.176140071391521183118619623519D-01

  else if ( norder == 32 ) then

    xtab(1) =  - 0.997263861849481563544981128665D+00
    xtab(2) =  - 0.985611511545268335400175044631D+00
    xtab(3) =  - 0.964762255587506430773811928118D+00
    xtab(4) =  - 0.934906075937739689170919134835D+00
    xtab(5) =  - 0.896321155766052123965307243719D+00
    xtab(6) =  - 0.849367613732569970133693004968D+00
    xtab(7) =  - 0.794483795967942406963097298970D+00
    xtab(8) =  - 0.732182118740289680387426665091D+00
    xtab(9) =  - 0.663044266930215200975115168663D+00
    xtab(10) = - 0.587715757240762329040745476402D+00
    xtab(11) = - 0.506899908932229390023747474378D+00
    xtab(12) = - 0.421351276130635345364119436172D+00
    xtab(13) = - 0.331868602282127649779916805730D+00
    xtab(14) = - 0.239287362252137074544603209166D+00
    xtab(15) = - 0.144471961582796493485186373599D+00
    xtab(16) = - 0.483076656877383162348125704405D-01
    xtab(17) =   0.483076656877383162348125704405D-01
    xtab(18) =   0.144471961582796493485186373599D+00
    xtab(19) =   0.239287362252137074544603209166D+00
    xtab(20) =   0.331868602282127649779916805730D+00
    xtab(21) =   0.421351276130635345364119436172D+00
    xtab(22) =   0.506899908932229390023747474378D+00
    xtab(23) =   0.587715757240762329040745476402D+00
    xtab(24) =   0.663044266930215200975115168663D+00
    xtab(25) =   0.732182118740289680387426665091D+00
    xtab(26) =   0.794483795967942406963097298970D+00
    xtab(27) =   0.849367613732569970133693004968D+00
    xtab(28) =   0.896321155766052123965307243719D+00
    xtab(29) =   0.934906075937739689170919134835D+00
    xtab(30) =   0.964762255587506430773811928118D+00
    xtab(31) =   0.985611511545268335400175044631D+00
    xtab(32) =   0.997263861849481563544981128665D+00

    weight(1) =  0.701861000947009660040706373885D-02
    weight(2) =  0.162743947309056706051705622064D-01
    weight(3) =  0.253920653092620594557525897892D-01
    weight(4) =  0.342738629130214331026877322524D-01
    weight(5) =  0.428358980222266806568786466061D-01
    weight(6) =  0.509980592623761761961632446895D-01
    weight(7) =  0.586840934785355471452836373002D-01
    weight(8) =  0.658222227763618468376500637069D-01
    weight(9) =  0.723457941088485062253993564785D-01
    weight(10) = 0.781938957870703064717409188283D-01
    weight(11) = 0.833119242269467552221990746043D-01
    weight(12) = 0.876520930044038111427714627518D-01
    weight(13) = 0.911738786957638847128685771116D-01
    weight(14) = 0.938443990808045656391802376681D-01
    weight(15) = 0.956387200792748594190820022041D-01
    weight(16) = 0.965400885147278005667648300636D-01
    weight(17) = 0.965400885147278005667648300636D-01
    weight(18) = 0.956387200792748594190820022041D-01
    weight(19) = 0.938443990808045656391802376681D-01
    weight(20) = 0.911738786957638847128685771116D-01
    weight(21) = 0.876520930044038111427714627518D-01
    weight(22) = 0.833119242269467552221990746043D-01
    weight(23) = 0.781938957870703064717409188283D-01
    weight(24) = 0.723457941088485062253993564785D-01
    weight(25) = 0.658222227763618468376500637069D-01
    weight(26) = 0.586840934785355471452836373002D-01
    weight(27) = 0.509980592623761761961632446895D-01
    weight(28) = 0.428358980222266806568786466061D-01
    weight(29) = 0.342738629130214331026877322524D-01
    weight(30) = 0.253920653092620594557525897892D-01
    weight(31) = 0.162743947309056706051705622064D-01
    weight(32) = 0.701861000947009660040706373885D-02

  else if ( norder == 64 ) then

    xtab(1) =  - 0.999305041735772139456905624346D+00
    xtab(2) =  - 0.996340116771955279346924500676D+00
    xtab(3) =  - 0.991013371476744320739382383443D+00
    xtab(4) =  - 0.983336253884625956931299302157D+00
    xtab(5) =  - 0.973326827789910963741853507352D+00
    xtab(6) =  - 0.961008799652053718918614121897D+00
    xtab(7) =  - 0.946411374858402816062481491347D+00
    xtab(8) =  - 0.929569172131939575821490154559D+00
    xtab(9) =  - 0.910522137078502805756380668008D+00
    xtab(10) = - 0.889315445995114105853404038273D+00
    xtab(11) = - 0.865999398154092819760783385070D+00
    xtab(12) = - 0.840629296252580362751691544696D+00
    xtab(13) = - 0.813265315122797559741923338086D+00
    xtab(14) = - 0.783972358943341407610220525214D+00
    xtab(15) = - 0.752819907260531896611863774886D+00
    xtab(16) = - 0.719881850171610826848940217832D+00
    xtab(17) = - 0.685236313054233242563558371031D+00
    xtab(18) = - 0.648965471254657339857761231993D+00
    xtab(19) = - 0.611155355172393250248852971019D+00
    xtab(20) = - 0.571895646202634034283878116659D+00
    xtab(21) = - 0.531279464019894545658013903544D+00
    xtab(22) = - 0.489403145707052957478526307022D+00
    xtab(23) = - 0.446366017253464087984947714759D+00
    xtab(24) = - 0.402270157963991603695766771260D+00
    xtab(25) = - 0.357220158337668115950442615046D+00
    xtab(26) = - 0.311322871990210956157512698560D+00
    xtab(27) = - 0.264687162208767416373964172510D+00
    xtab(28) = - 0.217423643740007084149648748989D+00
    xtab(29) = - 0.169644420423992818037313629748D+00
    xtab(30) = - 0.121462819296120554470376463492D+00
    xtab(31) = - 0.729931217877990394495429419403D-01
    xtab(32) = - 0.243502926634244325089558428537D-01
    xtab(33) =   0.243502926634244325089558428537D-01
    xtab(34) =   0.729931217877990394495429419403D-01
    xtab(35) =   0.121462819296120554470376463492D+00
    xtab(36) =   0.169644420423992818037313629748D+00
    xtab(37) =   0.217423643740007084149648748989D+00
    xtab(38) =   0.264687162208767416373964172510D+00
    xtab(39) =   0.311322871990210956157512698560D+00
    xtab(40) =   0.357220158337668115950442615046D+00
    xtab(41) =   0.402270157963991603695766771260D+00
    xtab(42) =   0.446366017253464087984947714759D+00
    xtab(43) =   0.489403145707052957478526307022D+00
    xtab(44) =   0.531279464019894545658013903544D+00
    xtab(45) =   0.571895646202634034283878116659D+00
    xtab(46) =   0.611155355172393250248852971019D+00
    xtab(47) =   0.648965471254657339857761231993D+00
    xtab(48) =   0.685236313054233242563558371031D+00
    xtab(49) =   0.719881850171610826848940217832D+00
    xtab(50) =   0.752819907260531896611863774886D+00
    xtab(51) =   0.783972358943341407610220525214D+00
    xtab(52) =   0.813265315122797559741923338086D+00
    xtab(53) =   0.840629296252580362751691544696D+00
    xtab(54) =   0.865999398154092819760783385070D+00
    xtab(55) =   0.889315445995114105853404038273D+00
    xtab(56) =   0.910522137078502805756380668008D+00
    xtab(57) =   0.929569172131939575821490154559D+00
    xtab(58) =   0.946411374858402816062481491347D+00
    xtab(59) =   0.961008799652053718918614121897D+00
    xtab(60) =   0.973326827789910963741853507352D+00
    xtab(61) =   0.983336253884625956931299302157D+00
    xtab(62) =   0.991013371476744320739382383443D+00
    xtab(63) =   0.996340116771955279346924500676D+00
    xtab(64) =   0.999305041735772139456905624346D+00

    weight(1) =  0.178328072169643294729607914497D-02
    weight(2) =  0.414703326056246763528753572855D-02
    weight(3) =  0.650445796897836285611736039998D-02
    weight(4) =  0.884675982636394772303091465973D-02
    weight(5) =  0.111681394601311288185904930192D-01
    weight(6) =  0.134630478967186425980607666860D-01
    weight(7) =  0.157260304760247193219659952975D-01
    weight(8) =  0.179517157756973430850453020011D-01
    weight(9) =  0.201348231535302093723403167285D-01
    weight(10) = 0.222701738083832541592983303842D-01
    weight(11) = 0.243527025687108733381775504091D-01
    weight(12) = 0.263774697150546586716917926252D-01
    weight(13) = 0.283396726142594832275113052002D-01
    weight(14) = 0.302346570724024788679740598195D-01
    weight(15) = 0.320579283548515535854675043479D-01
    weight(16) = 0.338051618371416093915654821107D-01
    weight(17) = 0.354722132568823838106931467152D-01
    weight(18) = 0.370551285402400460404151018096D-01
    weight(19) = 0.385501531786156291289624969468D-01
    weight(20) = 0.399537411327203413866569261283D-01
    weight(21) = 0.412625632426235286101562974736D-01
    weight(22) = 0.424735151236535890073397679088D-01
    weight(23) = 0.435837245293234533768278609737D-01
    weight(24) = 0.445905581637565630601347100309D-01
    weight(25) = 0.454916279274181444797709969713D-01
    weight(26) = 0.462847965813144172959532492323D-01
    weight(27) = 0.469681828162100173253262857546D-01
    weight(28) = 0.475401657148303086622822069442D-01
    weight(29) = 0.479993885964583077281261798713D-01
    weight(30) = 0.483447622348029571697695271580D-01
    weight(31) = 0.485754674415034269347990667840D-01
    weight(32) = 0.486909570091397203833653907347D-01
    weight(33) = 0.486909570091397203833653907347D-01
    weight(34) = 0.485754674415034269347990667840D-01
    weight(35) = 0.483447622348029571697695271580D-01
    weight(36) = 0.479993885964583077281261798713D-01
    weight(37) = 0.475401657148303086622822069442D-01
    weight(38) = 0.469681828162100173253262857546D-01
    weight(39) = 0.462847965813144172959532492323D-01
    weight(40) = 0.454916279274181444797709969713D-01
    weight(41) = 0.445905581637565630601347100309D-01
    weight(42) = 0.435837245293234533768278609737D-01
    weight(43) = 0.424735151236535890073397679088D-01
    weight(44) = 0.412625632426235286101562974736D-01
    weight(45) = 0.399537411327203413866569261283D-01
    weight(46) = 0.385501531786156291289624969468D-01
    weight(47) = 0.370551285402400460404151018096D-01
    weight(48) = 0.354722132568823838106931467152D-01
    weight(49) = 0.338051618371416093915654821107D-01
    weight(50) = 0.320579283548515535854675043479D-01
    weight(51) = 0.302346570724024788679740598195D-01
    weight(52) = 0.283396726142594832275113052002D-01
    weight(53) = 0.263774697150546586716917926252D-01
    weight(54) = 0.243527025687108733381775504091D-01
    weight(55) = 0.222701738083832541592983303842D-01
    weight(56) = 0.201348231535302093723403167285D-01
    weight(57) = 0.179517157756973430850453020011D-01
    weight(58) = 0.157260304760247193219659952975D-01
    weight(59) = 0.134630478967186425980607666860D-01
    weight(60) = 0.111681394601311288185904930192D-01
    weight(61) = 0.884675982636394772303091465973D-02
    weight(62) = 0.650445796897836285611736039998D-02
    weight(63) = 0.414703326056246763528753572855D-02
    weight(64) = 0.178328072169643294729607914497D-02

  else

    write ( *, * ) ' '
    write ( *, * ) 'LEGENDRE_SET - Fatal error!'
    write ( *, * ) '  Illegal value of NORDER = ', norder
    write ( *, * ) '  Legal values are 1 to 20, 32 or 64.'
    stop

  end if

  return
end
subroutine legendre_set_x1 ( norder, xtab, weight )
!
!*******************************************************************************
!
!! LEGENDRE_SET_X1 sets a Gauss-Legendre rule for ( 1 + X ) * F(X) on [-1,1].
!
!
!  Integration interval:
!
!    [ -1, 1 ]
!
!  Weight function:
!
!    1 + X
!
!  Integral to approximate:
!
!    INTEGRAL ( -1 <= X <= 1 ) ( 1 + X ) * F(X) dX
!
!  Approximate integral:
!
!    SUM ( I = 1 to NORDER ) WEIGHT(I) * F ( XTAB(I) )
!
!  Reference:
!
!    Arthur Stroud and Don Secrest,
!    Gaussian Quadrature Formulas,
!    Prentice Hall, 1966, Table #3.
!
!  Modified:
!
!    18 December 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer NORDER, the order of the rule.
!    NORDER must be between 1 and 9.
!
!    Output, double precision XTAB(NORDER), the abscissas of the rule.
!
!    Output, double precision WEIGHT(NORDER), the weights of the rule.
!
  integer norder
!
  double precision xtab(norder)
  double precision weight(norder)
!
  if ( norder == 1 ) then

    xtab(1) =  0.333333333333333333333333333333D+00

    weight(1) = 2.0D+00

  else if ( norder == 2 ) then

    xtab(1) = -0.289897948556635619639456814941D+00
    xtab(2) =  0.689897948556635619639456814941D+00

    weight(1) =  0.727834473024091322422523991699D+00
    weight(2) =  1.27216552697590867757747600830D+00

  else if ( norder == 3 ) then

    xtab(1) = -0.575318923521694112050483779752D+00
    xtab(2) =  0.181066271118530578270147495862D+00
    xtab(3) =  0.822824080974592105208907712461D+00

    weight(1) =  0.279307919605816490135525088716D+00
    weight(2) =  0.916964425438344986775682378225D+00
    weight(3) =  0.803727654955838523088792533058D+00

  else if ( norder == 4 ) then

    xtab(1) = -0.720480271312438895695825837750D+00
    xtab(2) = -0.167180864737833640113395337326D+00
    xtab(3) =  0.446313972723752344639908004629D+00
    xtab(4) =  0.885791607770964635613757614892D+00

    weight(1) =  0.124723883800032328695500588386D+00
    weight(2) =  0.519390190432929763305824811559D+00
    weight(3) =  0.813858272041085443165617903743D+00
    weight(4) =  0.542027653725952464833056696312D+00

  else if ( norder == 5 ) then

    xtab(1) = -0.802929828402347147753002204224D+00
    xtab(2) = -0.390928546707272189029229647442D+00
    xtab(3) =  0.124050379505227711989974959990D+00
    xtab(4) =  0.603973164252783654928415726409D+00
    xtab(5) =  0.920380285897062515318386619813D+00

    weight(1) =  0.0629916580867691047411692662740D+00
    weight(2) =  0.295635480290466681402532877367D+00
    weight(3) =  0.585547948338679234792151477424D+00
    weight(4) =  0.668698552377478261966702492391D+00
    weight(5) =  0.387126360906606717097443886545D+00

  else if ( norder == 6 ) then

    xtab(1) = -0.853891342639482229703747931639D+00
    xtab(2) = -0.538467724060109001833766720231D+00
    xtab(3) = -0.117343037543100264162786683611D+00
    xtab(4) =  0.326030619437691401805894055838D+00
    xtab(5) =  0.703842800663031416300046295008D+00
    xtab(6) =  0.941367145680430216055899446174D+00

    weight(1) =  0.0349532072544381270240692132496D+00
    weight(2) =  0.175820662202035902032706497222D+00
    weight(3) =  0.394644603562621056482338042193D+00
    weight(4) =  0.563170215152795712476307356284D+00
    weight(5) =  0.542169988926074467362761586552D+00
    weight(6) =  0.289241322902034734621817304499D+00

  else if ( norder == 7 ) then

    xtab(1) = -0.887474878926155707068695617935D+00
    xtab(2) = -0.639518616526215270024840114382D+00
    xtab(3) = -0.294750565773660725252184459658D+00
    xtab(4) =  0.0943072526611107660028971153047D+00
    xtab(5) =  0.468420354430821063046421216613D+00
    xtab(6) =  0.770641893678191536180719525865D+00
    xtab(7) =  0.955041227122575003782349000858D+00

    weight(1) =  0.0208574488112296163587654972151D+00
    weight(2) =  0.109633426887493901777324193433D+00
    weight(3) =  0.265538785861965879934591955055D+00
    weight(4) =  0.428500262783494679963649011999D+00
    weight(5) =  0.509563589198353307674937943100D+00
    weight(6) =  0.442037032763498409684482945478D+00
    weight(7) =  0.223869453693964204606248453720D+00

  else if ( norder == 8 ) then

    xtab(1) = -0.910732089420060298533757956283D+00
    xtab(2) = -0.711267485915708857029562959544D+00
    xtab(3) = -0.426350485711138962102627520502D+00
    xtab(4) = -0.0903733696068532980645444599064D+00
    xtab(5) =  0.256135670833455395138292079035D+00
    xtab(6) =  0.571383041208738483284917464837D+00
    xtab(7) =  0.817352784200412087992517083851D+00
    xtab(8) =  0.964440169705273096373589797925D+00

    weight(1) =  0.0131807657689951954189692640444D+00
    weight(2) =  0.0713716106239448335742111888042D+00
    weight(3) =  0.181757278018795592332221684383D+00
    weight(4) =  0.316798397969276640481632757440D+00
    weight(5) =  0.424189437743720042818124385645D+00
    weight(6) =  0.450023197883549464687088394417D+00
    weight(7) =  0.364476094545494505382889847132D+00
    weight(8) =  0.178203217446223725304862478136D+00

  else if ( norder == 9 ) then

    xtab(1) = -0.927484374233581078117671398464D+00
    xtab(2) = -0.763842042420002599615429776011D+00
    xtab(3) = -0.525646030370079229365386614293D+00
    xtab(4) = -0.236234469390588049278459503207D+00
    xtab(5) =  0.0760591978379781302337137826389D+00
    xtab(6) =  0.380664840144724365880759065541D+00
    xtab(7) =  0.647766687674009436273648507855D+00
    xtab(8) =  0.851225220581607910728163628088D+00
    xtab(9) =  0.971175180702246902734346518378D+00

    weight(1) =  0.00872338834309252349019620448007D+00
    weight(2) =  0.0482400171391415162069086091476D+00
    weight(3) =  0.127219285964216005046760427743D+00
    weight(4) =  0.233604781180660442262926091607D+00
    weight(5) =  0.337433287379681397577000079834D+00
    weight(6) =  0.401235236773473158616600898930D+00
    weight(7) =  0.394134968689382820640692081477D+00
    weight(8) =  0.304297020437232650320317215016D+00
    weight(9) =  0.145112014093119485838598391765D+00

  else

    write ( *, * ) ' '
    write ( *, * ) 'LEGENDRE_SET_X1 - Fatal error!'
    write ( *, * ) '  Illegal input value of NORDER = ', norder
    stop

  end if

  return
end
subroutine legendre_set_x2 ( norder, xtab, weight )
!
!*******************************************************************************
!
!! LEGENDRE_SET_X2 sets a Gauss-Legendre rule for ( 1 + X )**2 * F(X) on [-1,1].
!
!
!  Integration interval:
!
!    [ -1, 1 ]
!
!  Weight function:
!
!    ( 1 + X )**2
!
!  Integral to approximate:
!
!    INTEGRAL ( -1 <= X <= 1 ) ( 1 + X )**2 * F(X) dX
!
!  Approximate integral:
!
!    SUM ( I = 1 to NORDER ) WEIGHT(I) * F ( XTAB(I) )
!
!  Reference:
!
!    Arthur Stroud and Don Secrest,
!    Gaussian Quadrature Formulas,
!    Prentice Hall, 1966, Table #3.
!
!  Modified:
!
!    18 December 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer NORDER, the order of the rule.
!    NORDER must be between 1 and 9.
!
!    Output, double precision XTAB(NORDER), the abscissas of the rule.
!
!    Output, double precision WEIGHT(NORDER), the weights of the rule.
!
  integer norder
!
  double precision xtab(norder)
  double precision weight(norder)
!
  if ( norder == 1 ) then

    xtab(1) =  0.5D+00

    weight(1) =  2.66666666666666666666666666666D+00

  else if ( norder == 2 ) then

    xtab(1) = -0.0883036880224505775998524725910D+00
    xtab(2) =  0.754970354689117244266519139258D+00

    weight(1) =  0.806287056638603444666851075928D+00
    weight(2) =  1.86037961002806322199981559074D+00

  else if ( norder == 3 ) then

    xtab(1) = -0.410004419776996766244796955168D+00
    xtab(2) =  0.305992467923296230556472913192D+00
    xtab(3) =  0.854011951853700535688324041976D+00

    weight(1) =  0.239605624068645584091811926047D+00
    weight(2) =  1.16997015407892817602809616291D+00
    weight(3) =  1.25709088851909290654675857771D+00

  else if ( norder == 4 ) then

    xtab(1) = -0.591702835793545726606755921586D+00
    xtab(2) = -0.0340945902087350046811467387661D+00
    xtab(3) =  0.522798524896275389882037174551D+00
    xtab(4) =  0.902998901106005341405865485802D+00

    weight(1) =  0.0828179259993445222751812523731D+00
    weight(2) =  0.549071097383384602539010760334D+00
    weight(3) =  1.14767031839371367238662411421D+00
    weight(4) =  0.887107324890223869465850539752D+00

  else if ( norder == 5 ) then

    xtab(1) = -0.702108425894032836232448374820D+00
    xtab(2) = -0.268666945261773544694327777841D+00
    xtab(3) =  0.220227225868961343518209179230D+00
    xtab(4) =  0.653039358456608553790815164028D+00
    xtab(5) =  0.930842120163569816951085142737D+00

    weight(1) =  0.0329106016247920636689299329544D+00
    weight(2) =  0.256444805783695354037991444453D+00
    weight(3) =  0.713601289772720001490035944563D+00
    weight(4) =  1.00959169519929190423066348132D+00
    weight(5) =  0.654118274286167343239045863379D+00

  else if ( norder == 6 ) then

    xtab(1) = -0.773611232355123732602532012021D+00
    xtab(2) = -0.431362254623427837535325249187D+00
    xtab(3) = -0.0180728263295041680220798103354D+00
    xtab(4) =  0.395126163954217534500188844163D+00
    xtab(5) =  0.736872116684029732026178298518D+00
    xtab(6) =  0.948190889812665614490712786006D+00

    weight(1) =  0.0146486064549543818622276447204D+00
    weight(2) =  0.125762377479560410622810097040D+00
    weight(3) =  0.410316569036929681761034600615D+00
    weight(4) =  0.756617493988329628546336413760D+00
    weight(5) =  0.859011997894245060846045458784D+00
    weight(6) =  0.500309621812647503028212451747D+00

  else if ( norder == 7 ) then

    xtab(1) = -0.822366333126005527278634734418D+00
    xtab(2) = -0.547034493182875002223997992852D+00
    xtab(3) = -0.200043026557985860387937545780D+00
    xtab(4) =  0.171995710805880507163425502299D+00
    xtab(5) =  0.518891747903884926692601716998D+00
    xtab(6) =  0.793821941703901970495546427988D+00
    xtab(7) =  0.959734452453198985538996625765D+00

    weight(1) =  0.00714150426951365443207221475404D+00
    weight(2) =  0.0653034050584375560578544725498D+00
    weight(3) =  0.235377690316228918725962815880D+00
    weight(4) =  0.505171029671130381676271523850D+00
    weight(5) =  0.733870426238362032891332767175D+00
    weight(6) =  0.725590596901489156295739839779D+00
    weight(7) =  0.394212014211504966587433032679D+00

  else if ( norder == 8 ) then

    xtab(1) = -0.857017929919813794402037235698D+00
    xtab(2) = -0.631543407166567521509503573952D+00
    xtab(3) = -0.339104543648722903660229021109D+00
    xtab(4) = -0.0111941563689783438801237300122D+00
    xtab(5) =  0.316696017045595559454075475675D+00
    xtab(6) =  0.609049663022520165351466780939D+00
    xtab(7) =  0.834198765028697794599267293239D+00
    xtab(8) =  0.967804480896157932935972899807D+00

    weight(1) =  0.00374814227227757804631954025851D+00
    weight(2) =  0.0357961737041152639660521680263D+00
    weight(3) =  0.137974910241879862433949246199D+00
    weight(4) =  0.326515411108352185491692769217D+00
    weight(5) =  0.547577467373226177976217604887D+00
    weight(6) =  0.682278153375510121675529810121D+00
    weight(7) =  0.614544746137780998436053880546D+00
    weight(8) =  0.318231662453524478640851647411D+00

  else if ( norder == 9 ) then

    xtab(1) = -0.882491728426548422828684254270D+00
    xtab(2) = -0.694873684026474640346360850039D+00
    xtab(3) = -0.446537143480670863635920316400D+00
    xtab(4) = -0.159388112702326252531544826624D+00
    xtab(5) =  0.141092709224374414981503995427D+00
    xtab(6) =  0.428217823321559204544020866175D+00
    xtab(7) =  0.676480966471850715860378175342D+00
    xtab(8) =  0.863830940812464825046988286026D+00
    xtab(9) =  0.973668228805771018909618924364D+00

    weight(1) =  0.00209009877215570354392734918986D+00
    weight(2) =  0.0205951891648697848186537272448D+00
    weight(3) =  0.0832489326348178964194106978875D+00
    weight(4) =  0.210746247220398685903797568021D+00
    weight(5) =  0.388325022916052063676224499399D+00
    weight(6) =  0.554275165518437673725822282791D+00
    weight(7) =  0.621388553284444032628761363828D+00
    weight(8) =  0.523916296267173054255512857631D+00
    weight(9) =  0.262081160888317771694556320674D+00

  else

    write ( *, * ) ' '
    write ( *, * ) 'LEGENDRE_SET_X2 - Fatal error!'
    write ( *, * ) '  Illegal input value of NORDER = ', norder
    stop

  end if

  return
end
function n_factorial ( n )
!
!*******************************************************************************
!
!! N_FACTORIAL computes N! (for small values of N).
!
!
!  Modified:
!
!    08 December 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the argument.
!
!    Output, integer N_FACTORIAL, the value of N!.
!
  integer i
  integer n
  integer n_factorial
!
  n_factorial = 1
  do i = 1, n
    n_factorial = n_factorial * i
  end do

  return
end
subroutine octahedron_unit_nd ( func, n, result )
!
!*******************************************************************************
!
!! OCTAHEDRON_UNIT_ND approximates integrals in a unit octahedron in ND.
!
!
!  Discussion:
!
!    A 2*N point 3rd degree formula is used, Stroud number GN:3-1.
!
!  Integration region:
!
!    N dimensional points X() such that:
!
!      SUM ( I = 1 to N ) ABS ( X(I) ) <= 1.
!
!  Reference:
!
!    Arthur H Stroud,
!    Approximate Calculation of Multiple Integrals,
!    Prentice Hall, 1971.
!
!  Modified:
!
!    21 November 2000
!
!  Parameters:
!
!    Input, external FUNC, the name of the user supplied
!    function of which is to be integrated, of the form:
!
!      function func ( n, x )
!      integer n
!      double precision func
!      double precision x(n)
!
!    Input, integer N, the dimension of the octahedron.
!
!    Output, double precision RESULT, the approximate integral of the function.
!
  integer n
!
  double precision func
  integer i
  integer j
  double precision octahedron_unit_volume_nd
  double precision quad
  double precision r
  double precision result
  double precision volume
  double precision w
  double precision x(n)
!
  external func
!
  w = 1.0D+00 / dble ( 2 * n )
  r = sqrt ( dble ( 2 * n ) / dble ( ( n + 1 ) * ( n + 2 ) ) )

  x(1:n) = 0.0D+00

  quad = 0.0D+00
  do i = 1, n
    x(i) = r
    do j = 1, 2
      quad = quad + w * func ( n, x )
      x(i) = - x(i)
    end do
    x(i) = 0.0D+00
  end do

  volume = octahedron_unit_volume_nd ( n )
  result = quad * volume

  return
end
function octahedron_unit_volume_nd ( n )
!
!*******************************************************************************
!
!! OCTAHEDRON_UNIT_VOLUME_ND returns the volume of the unit octahedron in ND.
!
!
!  Modified:
!
!    06 November 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the dimension of the space.
!
!    Output, double precision OCTAHEDRON_UNIT_VOLUME_ND, the volume of
!    the unit octahedron.
!
  integer i
  integer n
  double precision octahedron_unit_volume_nd
  double precision volume
!
  volume = 1.0D+00
  do i = 1, n
    volume = volume * 2.0D+00 / dble ( i )
  end do

  octahedron_unit_volume_nd = volume

  return
end
function parallelipiped_volume_3d ( x, y, z )
!
!*******************************************************************************
!
!! PARALLELIPIPED_VOLUME_3D returns the volume of a parallelipiped in 3D.
!
!
!  Modified:
!
!    09 December 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, double precision X(4), Y(4), Z(4), the coordinates of one corner
!    of the parallelipiped, and its 3 immediate neighbors.
!
!    Output, double precision PARALLELIPIPED_VOLUME_3D, the volume of
!    the parallelipiped.
!
  double precision parallelipiped_volume_3d
  double precision x(4)
  double precision y(4)
  double precision z(4)
!
  parallelipiped_volume_3d = abs ( &
    ( z(2) - z(1) ) * ( y(4) * x(3) - y(3) * x(4) ) + &
    ( z(3) - z(1) ) * ( x(4) * y(2) - x(2) * y(4) ) + &
    ( z(4) - z(1) ) * ( x(2) * y(3) - x(3) * y(2) ) + &
    ( z(3) - z(2) ) * ( y(4) * x(1) - y(1) * x(4) ) + &
    ( z(4) - z(2) ) * ( x(3) * y(1) - x(1) * y(3) ) + &
    ( z(4) - z(3) ) * ( x(1) * y(2) - x(2) * y(1) ) )

  return
end
function parallelipiped_volume_nd ( lda, n, v )
!
!*******************************************************************************
!
!! PARALLELIPIPED_VOLUME_ND returns the volume of a parallelipiped in ND.
!
!
!  Modified:
!
!    09 December 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer LDA, the leading dimension of the array V.
!    LDA must be at least N.
!
!    Input, integer N, the dimension of the space.
!
!    Input/output, double precision V(LDA,N+1).  On input, each of the
!    N+1 rows of V contains the N coordinates of one of the
!    "corners" of the parallelipiped in entries 1 through N, with
!    the last column being left free.
!    On output, V has been overwritten.
!
!    Output, double precision PARALLELIPIPED_VOLUME_ND, the volume of
!    the parallelipiped.
!
  integer lda
  integer n
!
  double precision det(2),det1
  integer info
  double precision parallelipiped_volume_nd
  integer pivot(n+1)
  double precision v(lda,n+1)
!
!  Compute the volume of the N-dimensional parallelipiped.
!
  v(1:n+1,n+1) = 1.0D+00

  call dge_fa ( v, lda, n+1, pivot, info )

  if ( info /= 0 ) then
    parallelipiped_volume_nd = 0.0D+00
    return
  end if

  call dge_det ( v, lda, n+1, pivot, det1 )
  det(1)=det1
  parallelipiped_volume_nd = abs ( det(1) ) * 10.0D+00**det(2)

  return
end
function pi ( )
!
!*******************************************************************************
!
!! PI returns the value of pi.
!
!
!  Modified:
!
!    04 December 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, double precision PI, the value of pi.
!
  double precision pi
!
  pi = 3.14159265358979323846264338327950288419716939937510D+00

  return
end
subroutine pyramid_unit_3d ( func, result )
!
!*******************************************************************************
!
!! PYRAMID_UNIT_3D approximates an integral inside a unit pyramid in 3D.
!
!
!  Integration Region:
!
!    Z - 1 <= X <= 1 - Z
!    Z - 1 <= Y <= 1 - Z
!    0 <= Z <= 1.
!
!  Discussion:
!
!    An 48 point degree 7 formula, Stroud CN:C2:7-1, is used.
!
!  Reference:
!
!    Arthur H Stroud,
!    Approximate Calculation of Multiple Integrals,
!    Prentice Hall, 1971, page 339.
!
!  Modified:
!
!    22 November 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, external FUNC, the name of the user supplied function which
!    evaluates F(X,Y,Z), of the form
!
!      function func ( x, y, z )
!      double precision func
!      double precision x
!      double precision y
!      double precision z
!
!    Output, double precision RESULT, the approximate integral of the function.
!
  double precision a
  double precision b
  double precision c
  double precision func
  double precision h
  integer i
  double precision pi
  double precision pyramid_volume_3d
  double precision quad
  double precision r
  double precision result
  double precision, save, dimension ( 4 ) :: u = &
    (/ 0.04850054945D+00, 0.2386007376D+00, 0.5170472951D+00,  0.7958514179D+00 /)
  double precision volume
  double precision, save, dimension ( 4 ) :: w1 = &
    (/ 0.1108884156D+00,  0.1434587878D+00, 0.06863388717D+00, 0.01035224075D+00 /)
  double precision w2(3)
  double precision x
  double precision y
  double precision z
!
  external func
!
  a = sqrt ( 6.0D+00 ) / 7.0D+00
  b = sqrt ( ( 114.0D+00 - 3.0D+00 * sqrt ( 583.0D+00 ) ) / 287.0D+00 )
  c = sqrt ( ( 114.0D+00 + 3.0D+00 * sqrt ( 583.0D+00 ) ) / 287.0D+00 )
  w2(1:3) = (/ &
    49.0D+00 / 270.0D+00, &
    ( 178981.0D+00 + 2769.0D+00 * sqrt ( 583.0D+00 ) ) / 629640.0D+00, &
    ( 178981.0D+00 - 2769.0D+00 * sqrt ( 583.0D+00 ) ) / 629640.0D+00 /)

  quad = 0.0D+00

  do i = 1, 4

    x = a * ( 1.0D+00 - u(i) )
    y = 0.0D+00
    z = u(i)
    quad = quad + w1(i) * w2(1) * func ( x, y, z )

    x = - a * ( 1.0D+00 - u(i) )
    y = 0.0D+00
    z = u(i)
    quad = quad + w1(i) * w2(1) * func ( x, y, z )

    x = 0.0D+00
    y = a * ( 1.0D+00 - u(i) )
    z = u(i)
    quad = quad + w1(i) * w2(1) * func ( x, y, z )

    x = 0.0D+00
    y = - a * ( 1.0D+00 - u(i) )
    z = u(i)
    quad = quad + w1(i) * w2(1) * func ( x, y, z )

  end do

  do i = 1, 4

    x =   b * ( 1.0D+00 - u(i) )
    y =   b * ( 1.0D+00 - u(i) )
    z =   u(i)
    quad = quad + w1(i) * w2(2) * func ( x, y, z )

    x = - b * ( 1.0D+00 - u(i) )
    y =   b * ( 1.0D+00 - u(i) )
    z =   u(i)
    quad = quad + w1(i) * w2(2) * func ( x, y, z )

    x = - b * ( 1.0D+00 - u(i) )
    y = - b * ( 1.0D+00 - u(i) )
    z =   u(i)
    quad = quad + w1(i) * w2(2) * func ( x, y, z )

    x =   b * ( 1.0D+00 - u(i) )
    y = - b * ( 1.0D+00 - u(i) )
    z =   u(i)
    quad = quad + w1(i) * w2(2) * func ( x, y, z )

    x =   c * ( 1.0D+00 - u(i) )
    y =   c * ( 1.0D+00 - u(i) )
    z =   u(i)
    quad = quad + w1(i) * w2(3) * func ( x, y, z )

    x = - c * ( 1.0D+00 - u(i) )
    y =   c * ( 1.0D+00 - u(i) )
    z =   u(i)
    quad = quad + w1(i) * w2(3) * func ( x, y, z )

    x = - c * ( 1.0D+00 - u(i) )
    y = - c * ( 1.0D+00 - u(i) )
    z =   u(i)
    quad = quad + w1(i) * w2(3) * func ( x, y, z )

    x =   c * ( 1.0D+00 - u(i) )
    y = - c * ( 1.0D+00 - u(i) )
    z =   u(i)
    quad = quad + w1(i) * w2(3) * func ( x, y, z )

  end do

  r = 1.0D+00
  h = 1.0D+00

  volume = pyramid_volume_3d ( r, h )
  result = quad * volume

  return
end
function pyramid_volume_3d ( r, h )
!
!*******************************************************************************
!
!! PYRAMID_VOLUME_3D returns the volume of a pyramid with square base in 3D.
!
!
!  Discussion:
!
!    A pyramid with square base can be regarded as the upper half of a
!    3D octahedron.
!
!  Modified:
!
!    16 November 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, double precision R, the "radius" of the pyramid, that is, half the
!    length of one of the sides of the square base.
!
!    Input, double precision H, the height of the pyramid.
!
!    Output, double precision PYRAMID_VOLUME_3D, the volume of the pyramid.
!
  double precision h
  double precision pyramid_volume_3d
  double precision r
!
  pyramid_volume_3d = ( 4.0D+00 / 3.0D+00 ) * h * r**2

  return
end
subroutine qmdpt ( func, n, nsub, result )
!
!*******************************************************************************
!
!! QMDPT carries out product midpoint quadrature for the unit cube in ND.
!
!
!  Integration region:
!
!    N dimensional points X() such that:
!
!      -1 <= X(I) <= 1 for all I.
!
!  Reference:
!
!    Arthur H Stroud,
!    Approximate Calculation of Multiple Integrals,
!    Prentice Hall, 1971.
!
!  Modified:
!
!    21 November 2000
!
!  Parameters:
!
!    Input, external FUNC, the name of the user supplied
!    function which evaluates the function, of the form
!
!      function func ( n, x )
!      integer n
!      double precision func
!      double precision x(n)
!
!    Output, double precision QA(K), QB(K), two sets of estimates for
!    the integral.  The QB entries are obtained from the
!    QA entries by Richardson extrapolation, and QB(K) is
!    the best estimate for the integral.
!
!    Input, integer N, the dimension of the cube.
!
!    Input, integer NSUB, the number of subdivisions (in each dimension).
!
!    Output, double precision RESULT, the approximate integral of the function.
!
  integer n
!
  double precision func
  integer i
  integer ihi
  integer ix(n)
  integer j
  logical more
  integer nsub
  double precision quad
  double precision result
  double precision volume
  double precision w
  double precision x(n)
!
  external func
!
  w = 1.0D+00 / dble ( nsub**n )
  quad = 0.0D+00

  more = .false.
  ihi = nsub**n

  do i = 1, ihi

    call vec_next ( n, ix, more, nsub )

    x(1:n) = dble ( 2 * ix(1:n) + 1 - nsub ) / dble ( nsub )

    quad = quad + w * func ( n, x )

  end do

  volume = 2.0D+00**n
  result = quad * volume

  return
end
function qmult_1d ( func, a, b )
!
!*******************************************************************************
!
!! QMULT_1D approximates an integral over an interval in 1D.
!
!
!  Discussion:
!
!    A 16 point 31-st degree Gauss-Legendre formula is used.
!
!  Integration region:
!
!    Points X such that:
!
!      A <= X <= B.
!
!  Reference:
!
!    Arthur H Stroud,
!    Approximate Calculation of Multiple Integrals,
!    Prentice Hall, 1971.
!
!  Modified:
!
!    21 November 2000
!
!  Parameters:
!
!    Input, external FUNC, the name of the user supplied
!    function which evaluates F(X), of the form
!
!      function func ( x )
!
!      double precision func
!      double precision x
!
!    Input, double precision A, B, the lower and upper limits of integration.
!
!    Output, double precision QMULT_1D, the approximate integral of the function.
!
  integer, parameter :: norder = 16
!
  double precision a
  double precision b
  double precision func
  integer i
  double precision quad
  double precision qmult_1d
  double precision volume
  double precision weight(norder)
  double precision x
  double precision xtab(norder)
!
  external func
!
  call legendre_set ( norder, xtab, weight )

  quad = 0.0D+00
  do i = 1, norder
    x = 0.5D+00 * ( b - a ) * xtab(i) + 0.5D+00 * ( a + b )
    quad = quad + 0.5D+00 * weight(i) * func ( x )
  end do

  volume = b - a
  qmult_1d = quad * volume

  return
end
function qmult_2d ( func, a, b, fup, flo )
!
!*******************************************************************************
!
!! QMULT_2D approximates an integral with varying Y dimension in 2D.
!
!
!  Discussion:
!
!    A 256 point product of two 16 point 31-st degree Gauss-Legendre
!    quadrature formulas is used.
!
!  Integration region:
!
!    Points (X,Y) such that:
!
!      A <= X <= B,
!      FLO(X) <= Y <= FHI(X).
!
!  Reference:
!
!    Arthur H Stroud,
!    Approximate Calculation of Multiple Integrals,
!    Prentice Hall, 1971.
!
!  Modified:
!
!    21 November 2000
!
!  Parameters:
!
!    Input, external FUNC, the name of the user supplied
!    function which evaluates F(X,Y), of the form
!
!      function func ( x, y )
!      double precision func
!      double precision x
!      double precision y
!
!    Input, double precision A, B, the lower and upper limits of X integration.
!
!    Input, external FUP, FLO, the names of the user
!    supplied functions which evaluate the upper and lower
!    limits of the Y integration, of the form
!
!      function fup(x)
!      double precision fup
!      double precision x
!
!    and
!
!      function flo(x)
!      double precision flo
!      double precision x
!
!    Output, double precision QMULT_2D, the approximate integral of the function.
!
  integer, parameter :: norder = 16
!
  double precision a
  double precision b
  double precision c
  double precision d
  double precision func
  double precision flo
  double precision fup
  integer i
  integer j
  double precision quad
  double precision qmult_2d
  double precision w1
  double precision w2
  double precision weight(norder)
  double precision x
  double precision xtab(norder)
  double precision y
!
  external func
  external flo
  external fup
!
  call legendre_set ( norder, xtab, weight )

  quad = 0.0D+00

  do i = 1, norder

    w1 = 0.5D+00 * ( b - a ) * weight(i)
    x = 0.5D+00 * ( b - a ) * xtab(i) + 0.5D+00 * ( b + a )
    c = flo ( x )
    d = fup ( x )

    do j = 1, norder

      w2 = 0.5D+00 * ( d - c ) * weight(j)
      y = 0.5D+00 * ( d - c ) * xtab(j) + 0.5D+00 * ( d + c )
      quad = quad + w1 * w2 * func ( x, y )

    end do

  end do

  qmult_2d = quad

  return
end
function qmult_3d ( func, a, b, fup1, flo1, fup2, flo2 )
!
!*******************************************************************************
!
!! QMULT_3D approximates an integral with varying Y and Z dimension in 3D.
!
!
!  Discussion:
!
!    A 4096 point product of three 16 point 31-st degree Gauss-Legendre
!    quadrature formulas is used.
!
!  Integration region:
!
!    Points (X,Y,Z) such that:
!
!      A         <= X <= B,
!      FLO(X)    <= Y <= FHI(X),
!      FLO2(X,Y) <= Z <= FHI2(X,Y).
!
!  Reference:
!
!    Arthur H Stroud,
!    Approximate Calculation of Multiple Integrals,
!    Prentice Hall, 1971.
!
!  Modified:
!
!    21 November 2000
!
!  Parameters:
!
!    Input, external FUNC, the name of the user supplied
!    function which evaluates F(X,Y,Z), of the form
!
!      function func ( x, y, z )
!      double precision func
!      double precision x
!      double precision y
!      double precision z
!
!    Input, double precision A, B, the lower and upper limits of X integration.
!
!    Input, external FUP1, FLO1, the names of the user
!    supplied functions which evaluate the upper and lower
!    limits of the Y integration, of the form
!
!      function fup1(x)
!      double precision fup1
!      double precision x
!
!    and
!
!      function flo1(x)
!      double precision flo1
!      double precision x
!
!    Input, external FUP2, FLO2, the names of the user
!    supplied functions which evaluate the upper and lower
!    limits of the Z integration, of the form
!
!      function fup2(x,y)
!      double precision fup2
!      double precision x
!      double precision y
!
!    and
!
!      function flo2(x,y)
!      double precision flo2
!      double precision x
!      double precision y
!
!    Output, double precision QMULT_3D, the approximate integral of the function.
!
  integer, parameter :: norder = 16
!
  double precision a
  double precision b
  double precision c
  double precision d
  double precision e
  double precision f
  double precision func
  double precision flo1
  double precision flo2
  double precision fup1
  double precision fup2
  integer i
  integer j
  integer k
  double precision qmult_3d
  double precision quad
  double precision volume
  double precision w1
  double precision w2
  double precision w3
  double precision weight(norder)
  double precision x
  double precision xtab(norder)
  double precision y
  double precision z
!
  external func
  external flo1
  external flo2
  external fup1
  external fup2
!
  call legendre_set ( norder, xtab, weight )

  quad = 0.0D+00

  do i = 1, norder

    x = 0.5D+00 * ( b - a ) * xtab(i) + 0.5D+00 * ( b + a )
    w1 = 0.5D+00 * weight(i)
    c = flo1 ( x )
    d = fup1 ( x )

    do j = 1, norder

      w2 = 0.5D+00 * ( d - c ) * weight(j)
      y = 0.5D+00 * ( d - c ) * xtab(j) + 0.5D+00 * ( d + c )
      e = flo2 ( x, y )
      f = fup2 ( x, y )

      do k = 1, norder

        w3 = 0.5D+00 * ( f - e ) * weight(k)
        z = 0.5D+00 * ( f - e ) * xtab(k) + 0.5D+00 * ( f + e )
        quad = quad + w1 * w2 * w3 * func ( x, y, z )

      end do

    end do

  end do

  volume = b - a
  qmult_3d = quad * volume

  return
end
subroutine rectangle_3d ( func, a, b, result )
!
!*******************************************************************************
!
!! RECTANGLE_3D approximates an integral inside a rectangular block in 3D.
!
!
!  Discussion:
!
!    An 8 point third degree formula is used.
!
!  Integration region:
!
!    Points (X,Y,Z) such that:
!
!      A(1) <= X <= B(1),
!      A(2) <= Y <= B(2),
!      A(3) <= Z <= B(3).
!
!  Reference:
!
!    Arthur H Stroud,
!    Approximate Calculation of Multiple Integrals,
!    Prentice Hall, 1971.
!
!  Modified:
!
!    21 November 2000
!
!  Parameters:
!
!    Input, external FUNC, the name of the user supplied function which
!    evaluates F(X,Y,Z), of the form
!
!      function func ( x, y, z )
!      double precision func
!      double precision x
!      double precision y
!      double precision z
!
!    Input, double precision A(3), B(3), the lower and upper limits
!    for X, Y and Z.
!
!    Output, double precision RESULT, the approximate integral of the function.
!
  double precision a(3)
  double precision b(3)
  double precision func
  integer i
  integer j
  integer k
  double precision quad
  double precision result
  double precision sqr3
  double precision volume
  double precision w
  double precision x
  double precision y
  double precision z
!
  external func
!
  sqr3 = 1.0D+00 / sqrt ( 3.0D+00 )
  w = 1.0D+00 / 8.0D+00

  quad = 0.0D+00

  do i = 1, 2

    x = sqr3 * ( -1 )**i
    x = 0.5D+00 * ( ( 1.0D+00 - x ) * b(1) + ( 1.0D+00 + x ) * a(1) )

    do j = 1, 2

      y = sqr3 * (  -1 )**j
      y = 0.5D+00 * ( ( 1.0D+00 - y ) * b(2) + ( 1.0D+00 + y ) * a(2) )

      do k = 1, 2

        z = sqr3 * ( -1 )**k
        z = 0.5D+00 * ( ( 1.0D+00 - z ) * b(3) + ( 1.0D+00 + z ) * a(3) )

        quad = quad + w * func ( x, y, z )

      end do

    end do

  end do

  volume = ( b(1) - a(1) ) * ( b(2) - a(2) ) * ( b(3) - a(3) )
  result = volume * quad

  return
end
subroutine rectangle_sub_2d ( func, xval, yval, nsub, norder, xtab, ytab, &
  weight, result )
!
!*******************************************************************************
!
!! RECTANGLE_SUB_2D carries out a composite quadrature over a rectangle in 2D.
!
!
!  Integration interval:
!
!    XVAL(1) <= X <= XVAL(2),
!    YVAL(1) <= Y <= YVAL(2).
!
!  Modified:
!
!    21 November 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, EXTERNAL FUNC, the name of the function to be
!    integrated.  The user must declare the name an EXTERNAL
!    parameter in the calling program, pass the name of the
!    function in FUNC, and write a function of the form
!
!      function func ( x, y )
!
!    which evaluates the function at the point (X,Y).
!
!    Input, double precision XVAL(2), the left and right X coordinates.
!
!    Input, double precision YVAL(2), the lower and upper Y coordinates.
!
!    Input, integer NSUB(2).
!    NSUB(1) is the number of subintervals to use in the X direction,
!    and NSUB(2) is the same thing for Y.
!
!    Input, integer NORDER, the order of the rule.
!
!    Input, double precision XTAB(NORDER), YTAB(NORDER), the abscissas.
!
!    Input, double precision WEIGHT(NORDER), the weights of the rule.
!
!    Output, double precision RESULT, the approximate integral of the function.
!
  integer norder
!
  double precision a(2)
  double precision b(2)
  double precision func
  integer i
  integer j
  integer k
  integer nsub(2)
  double precision quad_sub
  double precision result
  double precision result_sub
  double precision volume
  double precision volume_sub
  double precision weight(norder)
  double precision x
  double precision xhi
  double precision xlo
  double precision xtab(norder)
  double precision xval(2)
  double precision y
  double precision yhi
  double precision ylo
  double precision ytab(norder)
  double precision yval(2)
!
  external func
!
  a(1) = xval(1)
  a(2) = yval(1)
  b(1) = xval(2)
  b(2) = yval(2)

  do i = 1, 2
    if ( a(i) == b(i) ) then
      result = 0.0D+00
      return
    end if
  end do

  do i = 1, 2
    if ( nsub(i) < 1 ) then
      write ( *, * ) ' '
      write ( *, * ) 'RECTANGLE_SUB_2D - Fatal error!'
      write ( *, * ) '  Nonpositive value of NSUB(I) = ', nsub(i)
      write ( *, * ) '  for index I = ', i
      stop
    end if
  end do
!
!  Break up the X interval into NSUB(1) subintervals.
!
  volume = 0.0D+00
  result = 0.0D+00

  do i = 1, nsub(1)

    call dvec_even_select ( a(1), b(1), nsub(1)+1, i, xlo )
    call dvec_even_select ( a(1), b(1), nsub(1)+1, i+1, xhi )
!
!  Break up the Y interval into NSUB(2) subintervals.
!
    do j = 1, nsub(2)

      call dvec_even_select ( a(2), b(2), nsub(2)+1, j,   ylo )
      call dvec_even_select ( a(2), b(2), nsub(2)+1, j+1, yhi )

      quad_sub = 0.0D+00
      do k = 1, norder

        x = xlo + 0.5D+00 * ( xtab(k) + 1.0D+00 ) * ( xhi - xlo )
        y = ylo + 0.5D+00 * ( ytab(k) + 1.0D+00 ) * ( yhi - ylo )

        quad_sub = quad_sub + weight(k) * func ( x, y ) / 4.0D+00

      end do

      volume_sub = ( xhi - xlo ) * ( yhi - ylo )
      result_sub = quad_sub * volume_sub

      volume = volume + volume_sub
      result = result + result_sub

    end do

  end do

  return
end
subroutine rule_adjust ( a, b, c, d, norder, x, w )
!
!*******************************************************************************
!
!! RULE_ADJUST maps a quadrature rule from [A,B] to [C,D].
!
!
!  Discussion:
!
!    Most quadrature rules are defined on a special interval, like
!    [-1,1] or [0,1].  To integrate over an interval, the abscissas
!    and weights must be adjusted.  This can be done on the fly,
!    or by calling this routine.
!
!    If the weight function W(X) is not 1, then the W vector will
!    require further adjustment by the user.
!
!  Modified:
!
!    06 December 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, double precision A, B, the endpoints of the definition interval.
!
!    Input, double precision C, D, the endpoints of the integration interval.
!
!    Input, integer NORDER, the number of abscissas and weights.
!
!    Input/output, double precision X(NORDER), W(NORDER), the abscissas
!    and weights.
!
  integer norder
!
  double precision a
  double precision b
  double precision c
  double precision d
  double precision w(norder)
  double precision x(norder)
!
  x(1:norder) = ( ( b - x(1:norder) ) * c + ( x(1:norder) - a ) * d ) / ( b - a )

  w(1:norder) = ( ( d - c ) / ( b - a ) ) * w(1:norder)

  return
end
subroutine simplex_nd ( func, lda, n, v, result )
!
!*******************************************************************************
!
!! SIMPLEX_ND approximates an integral inside a simplex in ND.
!
!
!  Discussion:
!
!    An N+1 point second degree formula is used.
!
!  Integration region:
!
!    The simplex bounded by the origin and a convex combination of N points.
!
!  Reference:
!
!    Arthur H Stroud,
!    Approximate Calculation of Multiple Integrals,
!    Prentice Hall, 1971.
!
!  Modified:
!
!    21 November 2000
!
!  Parameters:
!
!    Input, external FUNC, the name of the user supplied
!    function which evaluates F(X) at the N-dimensional point
!    X, of the form
!
!      function func ( n, x )
!      integer n
!      double precision func
!      double precision x(n)
!
!    Input, integer LDA, the leading dimension of the array V.
!    LDA must be at least N.
!
!    Input, integer N, the dimension of the space.
!
!    Input/output, double precision V(LDA,N+1).  On input, each of the
!    N+1 rows of V contains the N coordinates of one of the
!    "corners" of the simplex in entries 1 through N, with
!    the last column being left free.
!
!    On output, V has been overwritten in the process of
!    computing the volume of the simplex.
!
!    Output, double precision RESULT, the approximate integral of the function.
!
  integer lda
  integer n
!
  double precision c
  double precision func
  integer i
  double precision quad
  double precision result
  double precision simplex_volume_nd
  double precision v(lda,n+1)
  double precision volume
  double precision w
  double precision x(n)
!
  external func
!
  c = 1.0D+00 / sqrt ( dble ( n + 2 ) )
  w = 1.0D+00 / dble ( n + 1 )

  do i = 1, n
    x(i) = w * ( 1.0D+00 - c ) * sum ( v(1:n+1,i) )
  end do

  quad = 0.0D+00

  do i = 1, n+1

    x(1:n) = x(1:n) + c * v(i,1:n)

    quad = quad + w * func ( n, x )

    x(1:n) = x(1:n) - c * v(i,1:n)

  end do

  volume = simplex_volume_nd ( lda, n, v )
  result = quad * volume

  return
end
function simplex_unit_volume_nd ( n )
!
!*******************************************************************************
!
!! SIMPLEX_UNIT_VOLUME_ND returns the volume of the unit simplex in ND.
!
!
!  Modified:
!
!    27 October 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the dimension of the space.
!
!    Output, double precision SIMPLEX_UNIT_VOLUME_ND, the volume of the
!    unit simplex.
!
  integer n
  integer n_factorial
  double precision simplex_unit_volume_nd
!
  simplex_unit_volume_nd = 1.0D+00 / dble ( n_factorial ( n ) )

  return
end
function simplex_volume_nd ( lda, n, v )
!
!*******************************************************************************
!
!! SIMPLEX_VOLUME_ND returns the volume of a simplex in ND.
!
!
!  Modified:
!
!    09 December 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:]
!
!    Input, integer LDA, the leading dimension of the array V.
!    LDA must be at least N.
!
!    Input, integer N, the dimension of the space.
!
!    Input/output, double precision V(LDA,N+1).  On input, each of the
!    N+1 rows of V contains the N coordinates of one of the
!    "corners" of the simplex in entries 1 through N, with
!    the last column being left free.
!
!    On output, V has been overwritten in the process of
!    computing the volume of the simplex.
!
!    Output, double precision SIMPLEX_VOLUME_ND, the volume of the unit simplex.
!
  integer lda
  integer n
!
  double precision parallelipiped_volume_nd
  double precision simplex_unit_volume_nd
  double precision simplex_volume_nd
  double precision v(lda,n+1)
  double precision volume
!
!  Compute the volume of the parallelipiped.
!
  volume = parallelipiped_volume_nd ( lda, n, v )
!
!  Multiply by the volume of the unit simplex, which serves as a
!  conversion factor between a parallelipiped and the simplex.
!
  simplex_volume_nd = volume * simplex_unit_volume_nd ( n )

  return
end
function sphere_area_3d ( r )
!
!*******************************************************************************
!
!! SPHERE_AREA_3D computes the surface area of a sphere in 3D.
!
!
!  Modified:
!
!    12 December 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, double precision R, the radius of the sphere.
!
!    Output, double precision SPHERE_AREA_3D, the area of the sphere.
!
  double precision pi
  double precision r
  double precision sphere_area_3d
!
  sphere_area_3d = 4.0D+00 * pi ( ) * r**2

  return
end
function sphere_area_nd ( n, r )
!
!*******************************************************************************
!
!! SPHERE_AREA_ND computes the surface area of a sphere in ND.
!
!
!  Discussion:
!
!    N   Area
!
!    2   2       * PI    * R
!    3   4       * PI    * R**2
!    4   2       * PI**2 * R**3
!    5   (8/3)   * PI**2 * R**4
!    6             PI**3 * R**5
!    7   (16/15) * PI**3 * R**6
!
!  Modified:
!
!    26 October 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the dimension of the space.
!
!    Input, double precision R, the radius of the sphere.
!
!    Output, double precision SPHERE_AREA_ND, the area of the sphere.
!
  integer n
  double precision r
  double precision sphere_area_nd
  double precision sphere_unit_area_nd
!
  sphere_area_nd = sphere_unit_area_nd ( n ) * r**(n-1)

  return
end
subroutine sphere_f1_nd ( func, n, xc, r, result )
!
!*******************************************************************************
!
!! SPHERE_F1_ND approximates an integral inside a sphere in ND.
!
!
!  Discussion:
!
!    An (N+1)*2**N point 5-th degree formula is used, Stroud number SN:5-6.
!
!  Integration region:
!
!    N dimensional points X() such that:
!
!      SUM ( I = 1 to N ) ( X(I) - XC(I) )**2 <= R**2.
!
!  Reference:
!
!    Arthur H Stroud,
!    Approximate Calculation of Multiple Integrals,
!    Prentice Hall, 1971.
!
!  Modified:
!
!    19 December 2000
!
!  Parameters:
!
!    Input, external FUNC, the name of the user supplied
!    function which evaluates F at the N-vector X, of the form
!
!      function func ( n, x )
!      integer n
!      double precision func
!      double precision x(n)
!
!    Input, integer N, the dimension of the sphere.
!
!    Input, double precision XC(N), the center of the sphere.
!
!    Input, double precision R, the radius of the sphere.
!
!    Output, double precision RESULT, the approximate integral of the function.
!
  integer n
!
  double precision func
  integer i
  integer ihi
  integer itemp
  integer j
  integer k
  integer khi
  integer ktemp
  double precision pi
  double precision quad
  double precision r
  double precision result
  double precision sphere_volume_nd
  double precision t
  double precision temp
  double precision u
  double precision u2
  double precision v
  double precision volume
  double precision w
  double precision x(n)
  double precision xc(n)
  double precision y
!
  external func
!
  if ( r == 0.0D+00 ) then
    result = 0.0D+00
    return
  end if

  u2 = ( 1.0D+00 - 2.0D+00 * sqrt ( 1.0D+00 / dble ( n + 4 ) ) ) / dble ( n + 2 )
  u = sqrt ( u2 )
  x(1:n) = xc(1:n) - r * u

  w = 1.0D+00 / dble ( ( n + 1 ) * 2**n )

  quad = 0.0D+00
  ihi = 2**n

  do i = 1, ihi

    itemp = i - 1

    do j = 1, n

      u = ( xc(j) - x(j) ) / r

      if ( mod ( itemp, 2 ) == 1 ) then
        x(j) = xc(j) - abs ( x(j) - xc(j) )
      else
        x(j) = xc(j) + abs ( x(j) - xc(j) )
      end if

      itemp = itemp / 2

    end do

    quad = quad + w * func ( n, x )

  end do

  temp = sqrt ( dble ( n + 4 ) )

  t = sqrt ( 2.0D+00 * dble ( n + 1 ) / dble ( n + 2 ) ) / ( dble ( n ) * temp )

  y = ( 1.0D+00 + 2.0D+00 / ( dble ( n ) * temp ) ) / dble ( n + 2 )
  v = sqrt ( y - t )
  u = sqrt ( y + dble ( n - 1 ) * t )

  khi = 2**n

  do i = 1, n

    x(1:n) = xc(1:n) - r * v

    x(i) = xc(i) - r * u

    do k = 1, khi

      ktemp = k - 1

      do j = 1, n

        if ( mod ( ktemp, 2 ) == 1 ) then
          x(j) = xc(j) - abs ( x(j) - xc(j) )
        else
          x(j) = xc(j) + abs ( x(j) - xc(j) )
        end if

        ktemp = ktemp / 2

      end do

      quad = quad + w * func ( n, x )

    end do

    x(i) = xc(i) - r * v

  end do

  volume = sphere_volume_nd ( n, r )
  result = quad * volume

  return
end
subroutine sphere_f3_nd ( func, n, xc, r, result )
!
!*******************************************************************************
!
!! SPHERE_F3_ND approximates an integral inside a sphere in ND.
!
!
!  Discussion:
!
!    A 2**(N+1)-1 point 5-th degree formula is used, Stroud number SN:5-4.
!
!  Integration region:
!
!    N dimensional points X() such that:
!
!      SUM ( I = 1 to N ) ( X(I) - XC(I) )**2 <= R**2.
!
!  Reference:
!
!    Arthur H Stroud,
!    Approximate Calculation of Multiple Integrals,
!    Prentice Hall, 1971.
!
!  Modified:
!
!    21 November 2000
!
!  Parameters:
!
!    Input, external FUNC, the name of the user supplied
!    function which evaluates F at the N-vector X, of the form
!
!      function func ( n, x )
!      integer n
!      double precision func
!      double precision x(n)
!
!    Input, integer N, the dimension of the sphere.
!
!    Input, double precision XC(N), the center of the sphere.
!
!    Input, double precision R, the radius of the sphere.
!
!    Output, double precision RESULT, the approximate integral of the function.
!
  integer n
!
  double precision func
  integer i
  integer j
  integer jtemp
  integer k
  double precision pi
  double precision quad
  double precision r
  double precision result
  double precision ri
  double precision s
  double precision sphere_volume_nd
  double precision volume
  double precision weight
  double precision x(n)
  double precision xc(n)
!
  external func
!
  if ( r == 0.0D+00 ) then
    result = 0.0D+00
    return
  end if

  quad = 0.0D+00
!
!  The first point is the center of the sphere.
!
  x(1:n) = xc(1:n)

  weight = 4.0D+00 / dble ( n + 2 )**2
  quad = quad + weight * func ( n, x )

  s = 1.0D+00 / sqrt ( dble ( n + 4 ) )

  do i = 1, n

    ri = sqrt ( dble ( i + 2 ) / dble ( n + 4 ) )
!
!  Set up the first point, with (I-1) zeroes, RI, and then N-I S's.
!
    do j = 1, n

      if ( j < i ) then
        x(j) = xc(j)
      else if ( j == i ) then
        x(j) = xc(j) + r * ri
      else
        x(j) = xc(j) + r * s
      end if

    end do

    weight = 2.0D+00**( i - n ) * dble ( n + 4 ) &
      / dble ( ( i + 1 ) * ( i + 2 ) * ( n + 2 ) )
!
!  Now go through all sign permutations of the basic point.
!
    do j = 1, 2**(n+1-i)

      jtemp = j - 1

      do k = i, n

        if ( mod ( jtemp, 2 ) == 1 ) then
          x(k) = xc(k) - abs ( x(k) - xc(k) )
        else
          x(k) = xc(k) + abs ( x(k) - xc(k) )
        end if

        jtemp = jtemp / 2

      end do

      quad = quad + weight * func ( n, x )

    end do

  end do

  volume = sphere_volume_nd ( n, r )
  result = quad * volume

  return
end
subroutine sphere_shell_03_nd ( func, n, xc, r1, r2, result )
!
!*******************************************************************************
!
!! SPHERE_SHELL_03_ND approximates an integral inside a spherical shell in ND.
!
!
!  Discussion:
!
!    An 2*N point 3-rd degree formula is used, Stroud number SN-Shell:3-1.
!
!  Integration region:
!
!    N dimensional points X() such that:
!
!      R1**2 <= SUM ( I = 1 to N ) ( X(I) - XC(I) )**2 <= R2**2.
!
!  Reference:
!
!    Arthur H Stroud,
!    Approximate Calculation of Multiple Integrals,
!    Prentice Hall, 1971.
!
!  Modified:
!
!    21 December 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, external FUNC, the name of the user supplied
!    function which evaluates F at the N-vector X, of the form
!
!      function func ( n, x )
!      integer n
!      double precision func
!      double precision x(n)
!
!    Input, integer N, the dimension of the space.
!
!    Input, double precision XC(N), the center of the spheres.
!
!    Input, double precision R1, R2, the inner and outer radiuses that
!    define the spherical shell.
!
!    Output, double precision RESULT, the approximate integral of the function.
!
  integer n
!
  double precision func
  integer i
  double precision quad
  double precision r
  double precision r1
  double precision r2
  double precision result
  double precision rho
  double precision sphere_shell_volume_nd
  double precision volume
  double precision w
  double precision x(n)
  double precision xc(n)
!
  external func
!
  if ( r1 == r2 ) then
    result = 0.0D+00
    return
  end if

  rho = r1 / r2

  r = dble ( n ) * ( 1.0D+00 - rho**(n+2) ) &
    / ( dble ( n + 2 ) * ( 1.0D+00 - rho**n ) )
  r = sqrt ( r )
  w = 1.0D+00 / dble ( 2 * n )

  x(1:n) = xc(1:n)

  quad = 0.0D+00
  do i = 1, n
    x(i) = xc(i) + r * r2
    quad = quad + w * func ( n, x )
    x(i) = xc(i) - r * r2
    quad = quad + w * func ( n, x )
    x(i) = xc(i)
  end do

  volume = sphere_shell_volume_nd ( n, r1, r2 )
  result = quad * volume

  return
end
subroutine sphere_shell_07_nd ( func, n, xc, r1, r2, result )
!
!*******************************************************************************
!
!! SPHERE_SHELL_07_ND approximates an integral inside a spherical shell in ND.
!
!
!  Discussion:
!
!    An 2*M point 7-th degree formula is used, Stroud number SN-Shell:7-1.
!
!    Here M is the number of points in some formula of degree 7 for the
!    surface of the sphere in ND.
!
!    For the formula we have chosen, M is 2**N + 2*N**2
!
!  Integration region:
!
!    N dimensional points X() such that:
!
!      R1**2 <= SUM ( I = 1 to N ) ( X(I) - XC(I) )**2 <= R2**2.
!
!  Reference:
!
!    Arthur H Stroud,
!    Approximate Calculation of Multiple Integrals,
!    Prentice Hall, 1971.
!
!  Modified:
!
!    21 December 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, external FUNC, the name of the user supplied
!    function which evaluates F at the N-vector X, of the form
!
!      function func ( n, x )
!      integer n
!      double precision func
!      double precision x(n)
!
!    Input, integer N, the dimension of the space.
!
!    Input, double precision XC(N), the center of the spheres.
!
!    Input, double precision R1, R2, the inner and outer radiuses that
!    define the spherical shell.
!
!    Output, double precision RESULT, the approximate integral of the function.
!
  integer n
!
  double precision a
  double precision b
  double precision c0
  double precision c2
  double precision c4
  double precision c6
  double precision func
  integer i
  integer iadd
  integer ix(n)
  integer j
  integer jhi
  integer k
  logical more
  integer ncard
  double precision quad
  double precision r1
  double precision r2
  double precision ra(2)
  double precision result
  double precision rho
  double precision rw(2)
  double precision sphere_shell_volume_nd
  double precision volume
  double precision w
  double precision w1
  double precision w2
  double precision w3
  double precision x(n)
  double precision x1
  double precision x2
  double precision x3
  double precision xc(n)
!
  external func
!
!  Determine the radii:
!
  rho = r1 / r2
  c0 = ( 1.0D+00 - rho**n     ) / dble ( n     )
  c2 = ( 1.0D+00 - rho**(n+2) ) / dble ( n + 2 )
  c4 = ( 1.0D+00 - rho**(n+4) ) / dble ( n + 4 )
  c6 = ( 1.0D+00 - rho**(n+6) ) / dble ( n + 6 )

  a = ( c4 * c2 - c0 * c6 ) / ( c2 * c2 - c4 * c0 )
  b = ( c4 * c4 - c2 * c6 ) / ( c2 * c2 - c4 * c0 )

  ra(1) = 0.5D+00 * ( a + sqrt ( a**2 - 4.0D+00 * b ) )
  ra(2) = 0.5D+00 * ( a - sqrt ( a**2 - 4.0D+00 * b ) )

  ra(1) = ( 1.0D+00 - ra(1) ) * r1 + ra(1) * r2
  ra(2) = ( 1.0D+00 - ra(2) ) * r1 + ra(2) * r2
!
!  Determine the radial weights
!
  rw(1) = ( c0 * r2**2 - c2 ) / ( ( r2 + r1 ) * ( r2 - r1 ) )
  rw(2) = ( c2 - c0 * r1**2 ) / ( ( r2 + r1 ) * ( r2 - r1 ) )

  rw(1) = rw(1) * dble ( n ) * r2**n / ( r2**n - r1**n )
  rw(2) = rw(2) * dble ( n ) * r2**n / ( r2**n - r1**n )
!
!  Set up the integration rule for a unit spherical surface.
!
  w1 = dble ( 8 - n ) / dble ( n * ( n + 2 ) * ( n + 4 ) )
  w2 = dble ( n**3 ) / dble ( 2**n * n * ( n + 2 ) * ( n + 4 ) )
  w3 = 4.0D+00 / dble ( n * ( n + 2 ) * ( n + 4 ) )

  x1 = 1.0D+00
  x2 = 1.0D+00 / sqrt ( dble ( n ) )
  x3 = 1.0D+00 / sqrt ( 2.0D+00 )
!
!  Carry out the quadrature.
!
  x(1:n) = xc(1:n)

  quad = 0.0D+00
  do k = 1, 2
!
!  First term.
!
    do i = 1, n
      x(i) = xc(i) + ra(k) * x1
      quad = quad + rw(k) * w1 * func ( n, x )
      x(i) = xc(i) - ra(k) * x1
      quad = quad + rw(k) * w1 * func ( n, x )
      x(i) = xc(i)
    end do
!
!  Second term.
!
    x(1:n) = xc(1:n) - ra(k) * x2

    more = .false.
    jhi = 2**n

    do j = 1, jhi

      call subset_next ( n, ix, more, ncard, iadd )

      if ( iadd /= 0 ) then
        x(iadd) = xc(iadd) - ( x(iadd) - xc(iadd) )
      end if

      quad = quad + rw(k) * w2 * func ( n, x )

    end do
!
!  Third term.
!
    x(1:n) = xc(1:n)

    do i = 1, n-1
      do j = i+1, n
        x(i) = xc(i) + ra(k) * x3
        x(j) = xc(j) + ra(k) * x3
        quad = quad + rw(k) * w3 * func ( n, x )
        x(i) = xc(i) - ra(k) * x3
        x(j) = xc(j) + ra(k) * x3
        quad = quad + rw(k) * w3 * func ( n, x )
        x(i) = xc(i) + ra(k) * x3
        x(j) = xc(j) - ra(k) * x3
        quad = quad + rw(k) * w3 * func ( n, x )
        x(i) = xc(i) - ra(k) * x3
        x(j) = xc(j) - ra(k) * x3
        quad = quad + rw(k) * w3 * func ( n, x )
        x(i) = xc(i)
        x(j) = xc(j)
      end do
    end do

  end do

  volume = sphere_shell_volume_nd ( n, r1, r2 )
  result = quad * volume

  return
end
function sphere_shell_volume_nd ( n, r1, r2 )
!
!*******************************************************************************
!
!! SPHERE_SHELL_VOLUME_ND computes the volume of a spherical shell in ND.
!
!
!  Discussion:
!
!    The spherical shell is the volume between two concentric spheres of
!    radius R1 and R2.
!
!  Modified:
!
!    19 December 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the dimension of the space.
!
!    Input, double precision R1, R2, the radiuses of the inner and outer spheres.
!
!    Output, double precision SPHERE_SHELL_VOLUME_ND, the volume of the
!    spherical shell.
!
  integer n
  double precision r1
  double precision r2
  double precision sphere_shell_volume_nd
  double precision sphere_volume_nd
!
  sphere_shell_volume_nd = sphere_volume_nd ( n, r2 ) &
    - sphere_volume_nd ( n, r1 )

  return
end
subroutine sphere_surface_5_nd ( func, n, xc, r, result )
!
!*******************************************************************************
!
!! SPHERE_SURFACE_5_ND approximates surface integrals on a sphere in ND.
!
!
!  Discussion:
!
!    A 2*N+2**N points 5-th degree formula is used, Stroud number UN:5-2.
!
!  Integration region:
!
!    N dimensional points X() such that:
!
!      SUM ( I = 1 to N ) ( X(I) - XC(I) )**2 = R**2.
!
!  Reference:
!
!    Arthur H Stroud,
!    Approximate Calculation of Multiple Integrals,
!    Prentice Hall, 1971.
!
!  Modified:
!
!    20 December 2000
!
!  Parameters:
!
!    Input, external FUNC, the name of the user supplied
!    function which evaluates F(X), at the N dimensional point
!    X, of the form
!
!      function func ( n, x )
!      integer n
!      double precision func
!      double precision x(n)
!
!    Input, integer N, the dimension of the space.
!
!    Input, double precision XC(N), the center of the sphere.
!
!    Input, double precision R, the radius of the sphere.
!
!    Output, double precision RESULT, the approximate integral of the function.
!
  integer n
!
  double precision func
  integer i
  integer iadd
  integer ihi
  integer ix(n)
  logical more
  integer ncard
  double precision quad
  double precision r
  double precision result
  double precision sphere_area_nd
  double precision volume
  double precision w1
  double precision w2
  double precision x(n)
  double precision x1
  double precision x2
  double precision xc(n)
!
  external func
!
  x1 = 1.0D+00
  x2 = 1.0D+00 / sqrt ( dble ( n ) )

  w1 = 1.0D+00 / dble ( n * ( n + 2 ) )
  w2 = dble ( n ) / dble ( ( n + 2 ) * 2**n )

  x(1:n) = xc(1:n)

  quad = 0.0D+00

  do i = 1, n
    x(i) = xc(i) + r * x1
    quad = quad + w1 * func ( n, x )
    x(i) = xc(i) - r * x1
    quad = quad + w1 * func ( n, x )
    x(i) = xc(i)
  end do

  more = .false.
  ihi = 2**n

  x(1:n) = xc(1:n) - r * x2

  do i = 1, ihi

    call subset_next ( n, ix, more, ncard, iadd )

    if ( iadd /= 0 ) then
      x(iadd) = xc(iadd) - ( x(iadd) - xc(iadd) )
    end if

    quad = quad + w2 * func ( n, x )

  end do

  volume = sphere_area_nd ( n, r )
  result = quad * volume

  return
end
subroutine sphere_surface_7_1_nd ( func, n, xc, r, result )
!
!*******************************************************************************
!
!! SPHERE_SURFACE_7_1_ND approximates surface integrals on a sphere in ND.
!
!
!  Discussion:
!
!    A 2**N + 2*N**2 point 7th degree formula is used, Stroud number UN:7-1.
!
!  Integration region:
!
!    N dimensional points X() such that:
!
!      SUM ( I = 1 to N ) ( X(I) - XC(I) )**2 = R**2.
!
!  Reference:
!
!    Arthur H Stroud,
!    Approximate Calculation of Multiple Integrals,
!    Prentice Hall, 1971.
!
!  Modified:
!
!    20 December 2000
!
!  Parameters:
!
!    Input, external FUNC, the name of the user supplied
!    function which evaluates F(X), at the N dimensional point
!    X, of the form
!
!      function func ( n, x )
!      integer n
!      double precision func
!      double precision x(n)
!
!    Input, integer N, the dimension of the space.
!
!    Input, double precision XC(N), the center of the sphere.
!
!    Input, double precision R, the radius of the sphere.
!
!    Output, double precision RESULT, the approximate integral of the function.
!
  integer n
!
  double precision func
  integer i
  integer iadd
  integer ix(n)
  integer j
  integer jhi
  logical more
  integer ncard
  double precision quad
  double precision r
  double precision result
  double precision sphere_area_nd
  double precision volume
  double precision w1
  double precision w2
  double precision w3
  double precision x(n)
  double precision x1
  double precision x2
  double precision x3
  double precision xc(n)
!
  external func
!
  x(1:n) = xc(1:n)

  w1 = dble ( 8 - n ) / dble ( n * ( n + 2 ) * ( n + 4 ) )
  w2 = dble ( n**3 ) / dble ( 2**n * n * ( n + 2 ) * ( n + 4 ) )
  w3 = 4.0D+00 / dble ( n * ( n + 2 ) * ( n + 4 ) )

  x1 = 1.0D+00
  x2 = 1.0D+00 / sqrt ( dble ( n ) )
  x3 = 1.0D+00 / sqrt ( 2.0D+00 )

  quad = 0.0D+00
!
!  First term.
!
  do i = 1, n
    x(i) = xc(i) + r * x1
    quad = quad + w1 * func ( n, x )
    x(i) = xc(i) - r * x1
    quad = quad + w1 * func ( n, x )
    x(i) = xc(i)
  end do
!
!  Second term.
!
  x(1:n) = xc(1:n) - r * x2

  more = .false.
  jhi = 2**n

  do j = 1, jhi

    call subset_next ( n, ix, more, ncard, iadd )

    if ( iadd /= 0 ) then
      x(iadd) = xc(iadd) - ( x(iadd) - xc(iadd) )
    end if

    quad = quad + w2 * func ( n, x )

  end do
!
!  Third term.
!
  x(1:n) = xc(1:n)

  do i = 1, n-1
    do j = i+1, n
      x(i) = xc(i) + r * x3
      x(j) = xc(j) + r * x3
      quad = quad + w3 * func ( n, x )
      x(i) = xc(i) - r * x3
      x(j) = xc(j) + r * x3
      quad = quad + w3 * func ( n, x )
      x(i) = xc(i) + r * x3
      x(j) = xc(j) - r * x3
      quad = quad + w3 * func ( n, x )
      x(i) = xc(i) - r * x3
      x(j) = xc(j) - r * x3
      quad = quad + w3 * func ( n, x )
      x(i) = xc(i)
      x(j) = xc(j)
    end do
  end do

  volume = sphere_area_nd ( n, r )
  result = quad * volume

  return
end
subroutine sphere_unit_07_3d ( func, result )
!
!*******************************************************************************
!
!! SPHERE_UNIT_07_3D approximates an integral inside the unit sphere in 3D.
!
!
!  Discussion:
!
!    A 64 point 7-th degree formula is used.
!
!  Integration region:
!
!    Points (X,Y,Z) such that:
!
!      X**2 + Y**2 + Z**2 <= 1.
!
!  Modified:
!
!    21 November 2000
!
!  Reference:
!
!    Arthur H Stroud,
!    Approximate Calculation of Multiple Integrals,
!    Prentice Hall, 1971.
!
!  Parameters:
!
!    Input, external FUNC, the name of the user supplied
!    function which evaluates F(X,Y,Z), of the form
!
!      function func ( x, y, z )
!      double precision func
!      double precision x
!      double precision y
!      double precision z
!
!    Output, double precision RESULT, the approximate integral of the function.
!
  integer, parameter :: norder = 4
!
  double precision angle
  double precision func
  integer i
  integer j
  integer k
  double precision pi
  double precision quad
  double precision result
  double precision sphere_unit_volume_3d
  double precision volume
  double precision w
  double precision weight1(norder)
  double precision weight2(norder)
  double precision weight3(norder)
  double precision x
  double precision xtab1(norder)
  double precision xtab2(norder)
  double precision xtab3(norder)
  double precision y
  double precision z
!
  external func
!
!  This is the 5 point Gauss-Legendre rule,
!  but with the midpoint deleted, and with different weights.
!
  xtab1(1) = -0.906179845938663992797626878299D+00
  xtab1(2) = -0.538469310105683091036314420700D+00
  xtab1(3) =  0.538469310105683091036314420700D+00
  xtab1(4) =  0.906179845938663992797626878299D+00

  weight1(1) = 0.19455533421780251826D+00
  weight1(2) = 0.13877799911553081506D+00
  weight1(3) = 0.13877799911553081506D+00
  weight1(4) = 0.19455533421780251826D+00
!
!  Set XTAB2 and WEIGHT2.
!
  do j = 1, norder
    angle = pi ( ) * dble ( 2 * j - 1 ) / dble ( 2 * norder )
    xtab2(j) = cos ( angle )
  end do

  weight2(1:norder) = 1.0D+00
!
!  Set XTAB3 and WEIGHT3 for the interval [-1,1].
!
  call legendre_set ( norder, xtab3, weight3 )

  w = 3.0D+00 / 16.0D+00

  quad = 0.0D+00

  do i = 1, norder
    do j = 1, norder
      do k = 1, norder

        x = xtab1(i) * sqrt ( 1.0D+00 - xtab2(j)**2 ) &
                     * sqrt ( 1.0D+00 - xtab3(k)**2 )
        y = xtab1(i) * xtab2(j) * sqrt ( 1.0D+00 - xtab3(k)**2 )
        z = xtab1(i) * xtab3(k)

        quad = quad + w * weight1(i) * weight2(j) * weight3(k) * func ( x, y, z )

      end do
    end do
  end do

  volume = sphere_unit_volume_3d ( )
  result = quad * volume

  return
end
subroutine sphere_unit_14_3d ( func, result )
!
!*******************************************************************************
!
!! SPHERE_UNIT_14_3D approximates an integral inside the unit sphere in 3D.
!
!
!  Discussion:
!
!    A 288 point 14-th degree formula is used, Stroud number S3:14-1.
!
!  Integration region:
!
!    Points (X,Y,Z) such that:
!
!      X**2 + Y**2 + Z**2 <= 1.
!
!  Reference:
!
!    Arthur H Stroud,
!    Approximate Calculation of Multiple Integrals,
!    Prentice Hall, 1971.
!
!  Modified:
!
!    21 November 2000
!
!  Parameters:
!
!    Input, external FUNC, the name of the user supplied
!    function which evaluates F(X,Y,Z), of the form
!
!      function func ( x, y, z )
!      double precision func
!      double precision x
!      double precision y
!      double precision z
!
!    Output, double precision RESULT, the approximate integral of the function.
!
  double precision func
  integer i
  integer j
  integer k
  integer l
  integer m
  integer n
  double precision pi
  double precision quad
  double precision, save, dimension ( 4 ) :: r = (/ &
    0.968160240D+00, 0.836031107D+00, 0.613371433D+00, 0.324253423D+00 /)
  double precision result
  double precision sphere_unit_volume_3d
  double precision temp
  double precision volume
  double precision w1
  double precision w2
  double precision, save, dimension ( 4 ) :: weight = (/ &
    0.076181268D+00, 0.126263673D+00, 0.098048133D+00, 0.032840260D+00 /)
  double precision x
  double precision, save, dimension ( 5 ) :: xtab = (/ &
    -0.151108275D+00, 0.315838353D+00, 0.346307112D+00, -0.101808787D+00, -0.409228403D+00 /)
  double precision y
  double precision, save, dimension ( 5 ) :: ytab = (/ &
    0.155240600D+00, 0.257049387D+00, 0.666277790D+00, 0.817386065D+00, 0.501547712D+00 /)
  double precision z
  double precision, save, dimension ( 5 ) :: ztab = (/ &
    0.976251323D+00, 0.913330032D+00, 0.660412970D+00, 0.567022920D+00, 0.762221757D+00 /)
!
  external func
!
  quad = 0.0D+00

  do m = 1, 4

    w1 = 125.0D+00 * weight(m) / 3360.0D+00
    x = 0.525731112D+00 * r(m)
    y = 0.850650808D+00 * r(m)
    z = 0.0D+00

    do j = 1, 2
      x = - x
      do k = 1, 2
        y = - y
        do l = 1, 3
          call d_swap3 ( x, y, z )
          quad = quad + w1 * func ( x, y, z )
        end do
      end do
    end do

    w2 = 143.0D+00 * weight(m) / 3360.0D+00

    do n = 1, 5

      x = xtab(n) * r(m)
      y = ytab(n) * r(m)
      z = ztab(n) * r(m)

      do i = 1, 3

        temp = x
        x = z
        z = - y
        y = - temp

        do j = 1, 3

          call d_swap3 ( x, y, z )

          quad = quad + w2 * func ( x, y, z )

        end do

        y = - y
        z = - z
        quad = quad + w2 * func ( x, y, z )

      end do

    end do

  end do

  volume = sphere_unit_volume_3d ( )
  result = quad * volume

  return
end
subroutine sphere_unit_15_3d ( func, result )
!
!*******************************************************************************
!
!! SPHERE_UNIT_15_3D approximates an integral inside the unit sphere in 3D.
!
!
!  Discussion:
!
!    A 512 point 15-th degree formula is used.
!
!  Integration region:
!
!    Points (X,Y,Z) such that:
!
!      X**2 + Y**2 + Z**2 <= 1.
!
!  Reference:
!
!    Arthur H Stroud,
!    Approximate Calculation of Multiple Integrals,
!    Prentice Hall, 1971.
!
!  Modified:
!
!    28 October 2000
!
!  Parameters:
!
!    Input, external FUNC, the name of the user supplied
!    function which evaluates F(X,Y,Z), of the form
!
!      function func ( x, y, z )
!      double precision func
!      double precision x
!      double precision y
!      double precision z
!
!    Output, double precision RESULT, the approximate integral of the function.
!
  integer, parameter :: norder1 = 4
  integer, parameter :: norder2 = 8
!
  double precision cj
  double precision ck
  double precision func
  integer i
  integer j
  integer k
  double precision pi
  double precision quad
  double precision result
  double precision sj
  double precision sk
  double precision sphere_unit_volume_3d
  double precision volume
  double precision w
  double precision, save, dimension ( norder1 ) :: weight1 = (/ &
    0.0328402599D+00, 0.0980481327D+00, 0.1262636728D+00, 0.0761812678D+00 /)
  double precision weight2(norder2)
  double precision x
  double precision, save, dimension ( norder1 ) :: xtab1 = (/ &
    0.3242534234D+00, 0.6133714327D+00, 0.8360311073D+00, 0.9681602395D+00 /)
  double precision xtab2(norder2)
  double precision y
  double precision z
!
  external func
!
  call legendre_set ( norder2, xtab2, weight2 )

  w = 3.0D+00 / 32.0D+00

  quad = 0.0D+00

  do i = 1, norder1

    do j = 1, norder2

      sj = xtab2(j)
      cj = sqrt ( 1.0D+00 - sj**2 )

      do k = 1, 16
        sk = sin ( dble ( k ) * pi ( ) / 8.0D+00 )
        ck = cos ( dble ( k ) * pi ( ) / 8.0D+00 )
        x = xtab1(i) * cj * ck
        y = xtab1(i) * cj * sk
        z = xtab1(i) * sj
        quad = quad + w * weight1(i) * weight2(j) * func ( x, y, z )
      end do

    end do

  end do

  volume = sphere_unit_volume_3d ( )
  result = quad * volume

  return
end
function sphere_unit_area_3d ( )
!
!*******************************************************************************
!
!! SPHERE_UNIT_AREA_3D computes the surface area of a unit sphere in 3D.
!
!
!  Modified:
!
!    20 December 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, double precision SPHERE_UNIT_AREA_3D, the area of the sphere.
!
  double precision pi
  double precision sphere_unit_area_3d
!
  sphere_unit_area_3d = 4.0D+00 * pi ( )

  return
end
function sphere_unit_area_nd ( n )
!
!*******************************************************************************
!
!! SPHERE_UNIT_AREA_ND computes the surface area of a unit sphere in ND.
!
!
!  Discussion:
!
!    N   Area
!
!    2   2       * PI
!    3   4       * PI
!    4   2       * PI**2
!    5   (8/3)   * PI**2
!    6             PI**3
!    7   (16/15) * PI**3
!
!  Modified:
!
!    26 October 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the dimension of the space.
!
!    Output, double precision SPHERE_UNIT_AREA_ND, the area of the sphere.
!
  double precision area
  integer i
  integer m
  integer n
  double precision pi
  double precision sphere_unit_area_nd
!
  if ( mod ( n, 2 ) == 0 ) then
    m = n / 2
    area = 2.0D+00 * ( pi ( ) )**m
    do i = 1, m-1
      area = area / dble ( i )
    end do
  else
    m = ( n - 1 ) / 2
    area = 2.0D+00**n * ( pi() )**m
    do i = m+1, 2*m
      area = area / dble ( i )
    end do
  end if

  sphere_unit_area_nd = area

  return
end
subroutine sphere_unit_f1_nd ( func, n, result )
!
!*******************************************************************************
!
!! SPHERE_UNIT_F1_ND approximates an integral inside a unit sphere in ND.
!
!
!  Discussion:
!
!    An (N+1)*2**N point 5-th degree formula is used, Stroud number SN:5-6.
!
!  Integration region:
!
!    N dimensional points X() such that:
!
!      SUM ( I = 1 to N ) X(I)**2 <= 1.
!
!  Reference:
!
!    Arthur H Stroud,
!    Approximate Calculation of Multiple Integrals,
!    Prentice Hall, 1971.
!
!  Modified:
!
!    21 November 2000
!
!  Parameters:
!
!    Input, external FUNC, the name of the user supplied
!    function which evaluates F at the N-vector X, of the form
!
!      function func ( n, x )
!      integer n
!      double precision func
!      double precision x(n)
!
!    Input, integer N, the dimension of the sphere.
!
!    Output, double precision RESULT, the approximate integral of the function.
!
  integer n
!
  double precision func
  integer i
  integer ihi
  integer itemp
  integer j
  integer k
  integer khi
  integer ktemp
  double precision pi
  double precision quad
  double precision result
  double precision sphere_unit_volume_nd
  double precision t
  double precision temp
  double precision u
  double precision u2
  double precision v
  double precision volume
  double precision w
  double precision x(n)
  double precision y
!
  external func
!
  u2 = ( 1.0D+00 - 2.0D+00 * sqrt ( 1.0D+00 / dble ( n + 4 ) ) ) / dble ( n + 2 )
  u = sqrt ( u2 )
  x(1:n) = - u

  w = 1.0D+00 / dble ( ( n + 1 ) * 2**n )

  quad = 0.0D+00
  ihi = 2**n

  do i = 1, ihi

    itemp = i - 1

    do j = 1, n

      if ( mod ( itemp, 2 ) == 1 ) then
        x(j) = - abs ( x(j) )
      else
        x(j) = abs ( x(j) )
      end if

      itemp = itemp / 2

    end do

    quad = quad + w * func ( n, x )

  end do

  temp = sqrt ( dble ( n + 4 ) )

  t = sqrt ( 2.0D+00 * dble ( n + 1 ) / dble ( n + 2 ) ) / ( dble ( n ) * temp )

  y = ( 1.0D+00 + 2.0D+00 / ( dble ( n ) * temp ) ) / dble ( n + 2 )
  v = sqrt ( y - t )
  u = sqrt ( y + dble ( n - 1 ) * t )

  khi = 2**n

  do i = 1, n

    x(1:n) = - v

    x(i) = - u

    do k = 1, khi

      ktemp = k - 1

      do j = 1, n

        if ( mod ( ktemp, 2 ) == 1 ) then
          x(j) = - abs ( x(j) )
        else
          x(j) = abs ( x(j) )
        end if

        ktemp = ktemp / 2

      end do

      quad = quad + w * func ( n, x )

    end do

    x(i) = - v

  end do

  volume = sphere_unit_volume_nd ( n )
  result = quad * volume

  return
end
subroutine sphere_unit_f3_nd ( func, n, result )
!
!*******************************************************************************
!
!! SPHERE_UNIT_F3_ND approximates an integral inside a unit sphere in ND.
!
!
!  Discussion:
!
!    A 2**(N+1)-1 point 5-th degree formula is used, Stroud number SN:5-4.
!
!  Integration region:
!
!    N dimensional points X() such that:
!
!      SUM ( I = 1 to N ) X(I)**2 <= 1.
!
!  Reference:
!
!    Arthur H Stroud,
!    Approximate Calculation of Multiple Integrals,
!    Prentice Hall, 1971.
!
!  Modified:
!
!    21 November 2000
!
!  Parameters:
!
!    Input, external FUNC, the name of the user supplied
!    function which evaluates F at the N-vector X, of the form
!
!      function func ( n, x )
!      integer n
!      double precision func
!      double precision x(n)
!
!    Input, integer N, the dimension of the sphere.
!
!    Output, double precision RESULT, the approximate integral of the function.
!
  integer n
!
  double precision func
  integer i
  integer j
  integer jtemp
  integer k
  double precision pi
  double precision quad
  double precision result
  double precision ri
  double precision s
  double precision sphere_unit_volume_nd
  double precision volume
  double precision weight
  double precision x(n)
!
  external func
!
  quad = 0.0D+00
!
!  The first point is the center of the sphere.
!
  x(1:n) = 0.0D+00

  weight = 4.0D+00 / dble ( n + 2 )**2
  quad = quad + weight * func ( n, x )

  s = 1.0D+00 / sqrt ( dble ( n + 4 ) )

  do i = 1, n

    ri = sqrt ( dble ( i + 2 ) / dble ( n + 4 ) )
!
!  Set up the first point, with (I-1) zeroes, RI, and then N-I S's.
!
    do j = 1, n

      if ( j < i ) then
        x(j) = 0.0D+00
      else if ( j == i ) then
        x(j) = ri
      else
        x(j) = s
      end if

    end do

    weight = 2.0D+00**( i - n ) * dble ( n + 4 ) &
      / dble ( ( i + 1 ) * ( i + 2 ) * ( n + 2 ) )
!
!  Now go through all sign permutations of the basic point.
!
    do j = 1, 2**(n+1-i)

      jtemp = j - 1

      do k = i, n

        if ( mod ( jtemp, 2 ) == 1 ) then
          x(k) = - abs ( x(k) )
        else
          x(k) = abs ( x(k) )
        end if

        jtemp = jtemp / 2

      end do

      quad = quad + weight * func ( n, x )

    end do

  end do

  volume = sphere_unit_volume_nd ( n )
  result = quad * volume

  return
end
subroutine sphere_unit_surface_07_3d ( func, result )
!
!*******************************************************************************
!
!! SPHERE_UNIT_SURFACE_07_3D approximates surface integrals on the unit sphere in 3D.
!
!
!  Discussion:
!
!    A 32 point 7-th degree formula is used.
!
!  Integration region:
!
!    Points (X,Y,Z) such that:
!
!      X**2 + Y**2 + Z**2 = 1.
!
!  Reference:
!
!    Arthur H Stroud,
!    Approximate Calculation of Multiple Integrals,
!    Prentice Hall, 1971.
!
!  Modified:
!
!    21 November 2000
!
!  Parameters:
!
!    Input, external FUNC, the name of the user supplied
!    function which evaluates F(X,Y,Z), of the form
!
!      function func ( x, y, z )
!      double precision func
!      double precision x
!      double precision y
!      double precision z
!
!    Output, double precision RESULT, the approximate integral of the function.
!
  integer, parameter :: norder1 = 2
  integer, parameter :: norder2 = 4
  integer, parameter :: norder3 = 4
!
  double precision angle
  double precision func
  integer i
  integer j
  integer k
  double precision pi
  double precision quad
  double precision result
  double precision sphere_unit_area_3d
  double precision volume
  double precision weight1(norder1)
  double precision weight2(norder2)
  double precision weight3(norder3)
  double precision x
  double precision xtab1(norder1)
  double precision xtab2(norder2)
  double precision xtab3(norder3)
  double precision y
  double precision z
!
  external func
!
!  Set XTAB1 and WATE1.
!
  xtab1(1) = -1.0D+00
  xtab1(2) =  1.0D+00
  weight1(1) = 1.0D+00
  weight1(2) = 1.0D+00
!
!  Set XTAB2 and WATE2.
!
  do j = 1, norder2
    angle = pi ( ) * dble ( 2 * j - 1 ) / dble ( 2 * norder2 )
    xtab2(j) = cos ( angle )
  end do

  weight2(1:norder2) = 1.0D+00 / dble ( 4 * norder2 )
!
!  Set XTAB3 and WATE3.
!
  call legendre_set ( norder3, xtab3, weight3 )

  quad = 0.0D+00
  do i = 1, norder1
    do j = 1, norder2
      do k = 1, norder3

        x = xtab1(i) * sqrt ( 1.0D+00 - xtab2(j)**2 ) &
                     * sqrt ( 1.0D+00 - xtab3(k)**2 )
        y = xtab1(i) * xtab2(j) * sqrt ( 1.0D+00 - xtab3(k)**2 )
        z = xtab1(i) * xtab3(k)

        quad = quad + weight1(i) * weight2(j) * weight3(k) * func ( x, y, z )

      end do
    end do
  end do

  volume = sphere_unit_area_3d ( )
  result = quad * volume

  return
end
subroutine sphere_unit_surface_11_3d ( func, result )
!
!*******************************************************************************
!
!! SPHERE_UNIT_SURFACE_11_3D approximates surface integrals on the unit sphere in 3D.
!
!
!  Discussion:
!
!    A 50 point 11-th degree formula is used, Stroud number U3:11-1.
!
!  Integration region:
!
!    Points (X,Y,Z) such that:
!
!      X**2 + Y**2 + Z**2 = 1.
!
!  Reference:
!
!    A D McLaren,
!    Math. Comp.
!    Volume 17, pages 361-383, 1963.
!
!    Arthur H Stroud,
!    Approximate Calculation of Multiple Integrals,
!    Prentice Hall, 1971.
!
!  Modified:
!
!    07 November 2000
!
!  Parameters:
!
!    Input, external FUNC, the name of the user supplied
!    function which evaluates F(X,Y,Z), of the form
!
!      function func ( x, y, z )
!      double precision func
!      double precision x
!      double precision y
!      double precision z
!
!    Output, double precision RESULT, the approximate integral of the function.
!
  double precision func
  integer i
  integer j
  integer k
  integer l
  double precision pi
  double precision quad
  double precision result
  double precision sphere_unit_area_3d
  double precision volume
  double precision w1
  double precision w2
  double precision w3
  double precision w4
  double precision x
  double precision y
  double precision z
!
  external func
!
  quad = 0.0D+00

  w1 = 9216.0D+00 / 725760.0D+00
  x = 1.0D+00
  y = 0.0D+00
  z = 0.0D+00
  do i = 1, 2
    x = - x
    do j = 1, 3
      call d_swap3 ( x, y, z )
      quad = quad + w1 * func ( x, y, z )
    end do
  end do

  w2 = 16384.0D+00 / 725760.0D+00
  x = sqrt ( 0.5D+00 )
  y = sqrt ( 0.5D+00 )
  z = 0.0D+00
  do i = 1, 2
    x = - x
    do j = 1, 2
      y = - y
      do k = 1, 3
        call d_swap3 ( x, y, z )
        quad = quad + w2 * func ( x, y, z )
      end do
    end do
  end do

  w3 = 15309.0D+00 / 725760.0D+00
  x = sqrt ( 1.0D+00 / 3.0D+00 )
  y = sqrt ( 1.0D+00 / 3.0D+00 )
  z = sqrt ( 1.0D+00 / 3.0D+00 )
  do i = 1, 2
    x = - x
    do j = 1, 2
      y = - y
      do k = 1, 2
        z = - z
        quad = quad + w3 * func ( x, y, z )
      end do
    end do
  end do

  w4 = 14641.0D+00 / 725760.0D+00
  x = sqrt ( 1.0D+00 / 11.0D+00 )
  y = sqrt ( 1.0D+00 / 11.0D+00 )
  z = 3.0D+00 * sqrt ( 1.0D+00 / 11.0D+00 )
  do i = 1, 2
    x = - x
    do j = 1, 2
      y = - y
      do k = 1, 2
        z = - z
        do l = 1, 3
          call d_swap3 ( x, y, z )
          quad = quad + w4 * func ( x, y, z )
        end do
      end do
    end do
  end do

  volume = sphere_unit_area_3d ( )
  result = quad * volume

  return
end
subroutine sphere_unit_surface_14_3d ( func, result )
!
!*******************************************************************************
!
!! SPHERE_UNIT_SURFACE_14_3D approximates surface integrals on the unit sphere in 3D.
!
!
!  Discussion:
!
!    A 72 point 14-th degree formula is used, Stroud number U3:14-1.
!
!  Integration region:
!
!    Points (X,Y,Z) such that:
!
!      X**2 + Y**2 + Z**2 = 1.
!
!  Reference:
!
!    A D McLaren,
!    Math. Comp.
!    Volume 17, pages 361-383, 1963.
!
!    Arthur H Stroud,
!    Approximate Calculation of Multiple Integrals,
!    Prentice Hall, 1971.
!
!  Modified:
!
!    21 November 2000
!
!  Parameters:
!
!    Input, external FUNC, the name of the user supplied
!    function which evaluates F(X,Y,Z), of the form
!
!      function func ( x, y, z )
!      double precision func
!      double precision x
!      double precision y
!      double precision z
!
!    Output, double precision RESULT, the approximate integral of the function.
!
  double precision func
  integer i
  integer j
  integer k
  double precision pi
  double precision quad
  double precision result
  double precision sphere_unit_area_3d
  double precision temp
  double precision volume
  double precision w1
  double precision w2
  double precision x
  double precision, save, dimension ( 5 ) :: xtab = (/ &
    -0.151108275D+00, 0.315838353D+00, 0.346307112D+00, -0.101808787D+00, &
    -0.409228403D+00 /)
  double precision y
  double precision, save, dimension ( 5 ) :: ytab = (/ &
    0.155240600D+00, 0.257049387D+00, 0.666277790D+00,  0.817386065D+00, &
    0.501547712D+00 /)
  double precision z
  double precision, save, dimension ( 5 ) :: ztab = (/ &
    0.976251323D+00, 0.913330032D+00, 0.660412970D+00,  0.567022920D+00, &
    0.762221757D+00 /)
!
  external func
!
  quad = 0.0D+00

  w1 = 125.0D+00 / 10080.0D+00
  x = 0.525731112D+00
  y = 0.850650808D+00
  z = 0.0D+00

  do i = 1, 2
    x = - x
    do j = 1, 2
      y = - y
      do k = 1, 3
        call d_swap3 ( x, y, z )
        quad = quad + w1 * func ( x, y, z )
      end do
    end do
  end do

  w2 = 143.0D+00 / 10080.0D+00

  do i = 1, 5

    x = xtab(i)
    y = ytab(i)
    z = ztab(i)

    do j = 1, 3

      temp = x
      x = z
      z = - y
      y = - temp

      do k = 1, 3
        call d_swap3 ( x, y, z )
        quad = quad + w2 * func ( x, y, z )
      end do

      y = - y
      z = - z
      quad = quad + w2 * func ( x, y, z )

    end do

  end do

  volume = sphere_unit_area_3d ( )
  result = quad * volume

  return
end
subroutine sphere_unit_surface_15_3d ( func, result )
!
!*******************************************************************************
!
!! SPHERE_UNIT_SURFACE_15_3D approximates surface integrals on the unit sphere in 3D.
!
!
!  Discussion:
!
!    A 128 point 15-th degree spherical product Gauss formula is used.
!
!  Integration region:
!
!    Points (X,Y,Z) such that:
!
!      X**2 + Y**2 + Z**2 = 1.
!
!  Reference:
!
!    Arthur H Stroud,
!    Approximate Calculation of Multiple Integrals,
!    Prentice Hall, 1971.
!
!  Modified:
!
!    21 November 2000
!
!  Parameters:
!
!    Input, external FUNC, the name of the user supplied
!    function which evaluates F(X,Y,Z), of the form
!
!      function func ( x, y, z )
!      double precision func
!      double precision x
!      double precision y
!      double precision z
!
!    Output, double precision RESULT, the approximate integral of the function.
!
  integer, parameter :: norder = 8
!
  double precision angle
  double precision func
  integer j
  integer k
  double precision pi
  double precision quad
  double precision result
  double precision sphere_unit_area_3d
  double precision volume
  double precision weight(norder)
  double precision x
  double precision xtab(norder)
  double precision y
  double precision z
!
  external func
!
  call legendre_set ( norder, xtab, weight )

  weight(1:norder) = weight(1:norder) / 32.0D+00

  quad = 0.0D+00

  do j = 1, norder

    do k = 1, 16

      angle = dble ( k ) * pi ( ) / 8.0D+00
      x = sqrt ( 1.0D+00 - xtab(j)**2 ) * cos ( angle )
      y = sqrt ( 1.0D+00 - xtab(j)**2 ) * sin ( angle )
      z = xtab(j)

      quad = quad + weight(j) * func ( x, y, z )

    end do
  end do

  volume = sphere_unit_area_3d ( )
  result = quad * volume

  return
end
subroutine sphere_unit_surface_3_nd ( func, n, result )
!
!*******************************************************************************
!
!! SPHERE_UNIT_SURFACE_3_ND approximates surface integrals on the unit sphere in ND.
!
!
!  Discussion:
!
!    A 2*N point 3rd degree formula is used, Stroud number UN:3-1.
!
!  Integration region:
!
!    N dimensional points X() such that:
!
!      SUM ( I = 1 to N ) X(I)**2 = 1.
!
!  Reference:
!
!    Arthur H Stroud,
!    Approximate Calculation of Multiple Integrals,
!    Prentice Hall, 1971.
!
!  Modified:
!
!    21 November 2000
!
!  Parameters:
!
!    Input, external FUNC, the name of the user supplied
!    function which evaluates F(X), at the N dimensional point
!    X, of the form
!
!      function func ( n, x )
!      integer n
!      double precision func
!      double precision x(n)
!
!    Input, integer N, the dimension of the space.
!
!    Output, double precision RESULT, the approximate integral of the function.
!
  integer n
!
  double precision func
  integer i
  double precision pi
  double precision quad
  double precision result
  double precision sphere_unit_area_nd
  double precision volume
  double precision w
  double precision x(n)
!
  external func
!
  x(1:n) = 0.0D+00

  w = 1.0D+00 / dble ( 2 * n )

  quad = 0.0D+00
  do i = 1, n
    x(i) = 1.0D+00
    quad = quad + w * func ( n, x )
    x(i) = - 1.0D+00
    quad = quad + w * func ( n, x )
    x(i) = 0.0D+00
  end do

  volume = sphere_unit_area_nd ( n )
  result = quad * volume

  return
end
subroutine sphere_unit_surface_4_nd ( func, n, result )
!
!*******************************************************************************
!
!! SPHERE_UNIT_SURFACE_4_ND approximates surface integrals on the unit sphere in ND.
!
!
!  Discussion:
!
!    A 2*N**2 point 5th degree formula is used, Stroud number UN:5-1.
!
!  Integration region:
!
!    N dimensional points X() such that:
!
!      SUM ( I = 1 to N ) X(I)**2 = 1.
!
!  Reference:
!
!    Arthur H Stroud,
!    Approximate Calculation of Multiple Integrals,
!    Prentice Hall, 1971.
!
!  Modified:
!
!    21 November 2000
!
!  Parameters:
!
!    Input, external FUNC, the name of the user supplied
!    function which evaluates F(X), at the N dimensional point
!    X, of the form
!
!      function func ( n, x )
!      integer n
!      double precision func
!      double precision x(n)
!
!    Input, integer N, the dimension of the space.
!
!    Output, double precision RESULT, the approximate integral of the function.
!
  integer n
!
  double precision func
  integer i
  integer j
  double precision pi
  double precision quad
  double precision result
  double precision s
  double precision sphere_unit_area_nd
  double precision volume
  double precision w1
  double precision w2
  double precision x(n)
!
  external func
!
  x(1:n) = 0.0D+00

  w1 = dble ( 4 - n ) / dble ( 2 * n * ( n + 2 ) )

  quad = 0.0D+00

  do i = 1, n
    x(i) = 1.0D+00
    quad = quad + w1 * func ( n, x )
    x(i) = - 1.0D+00
    quad = quad + w1 * func ( n, x )
    x(i) = 0.0D+00
  end do

  s = 1.0D+00 / sqrt ( 2.0D+00 )
  w2 = 1.0D+00 / dble ( n * ( n + 2 ) )

  do i = 1, n

    x(i) = s

    do j = i+1, n
      x(j) = s
      quad = quad + w2 * func ( n, x )
      x(j) = -s
      quad = quad + w2 * func ( n, x )
      x(j) = 0.0D+00
    end do

    x(i) = - s

    do j = i+1, n
      x(j) = s
      quad = quad + w2 * func ( n, x )
      x(j) = - s
      quad = quad + w2 * func ( n, x )
      x(j) = 0.0D+00
    end do

    x(i) = 0.0D+00

  end do

  volume = sphere_unit_area_nd ( n )
  result = quad * volume

  return
end
subroutine sphere_unit_surface_5_nd ( func, n, result )
!
!*******************************************************************************
!
!! SPHERE_UNIT_SURFACE_5_ND approximates surface integrals on the unit sphere in ND.
!
!
!  Discussion:
!
!    A 2*N+2**N points 5-th degree formula is used, Stroud number UN:5-2.
!
!  Integration region:
!
!    N dimensional points X() such that:
!
!      SUM ( I = 1 to N ) X(I)**2 = 1.
!
!  Reference:
!
!    Arthur H Stroud,
!    Approximate Calculation of Multiple Integrals,
!    Prentice Hall, 1971.
!
!  Modified:
!
!    21 November 2000
!
!  Parameters:
!
!    Input, external FUNC, the name of the user supplied
!    function which evaluates F(X), at the N dimensional point
!    X, of the form
!
!      function func ( n, x )
!      integer n
!      double precision func
!      double precision x(n)
!
!    Input, integer N, the dimension of the space.
!
!    Output, double precision RESULT, the approximate integral of the function.
!
  integer n
!
  double precision func
  integer i
  integer iadd
  integer ihi
  integer ix(n)
  logical more
  integer ncard
  double precision quad
  double precision result
  double precision sphere_unit_area_nd
  double precision volume
  double precision w1
  double precision w2
  double precision x(n)
  double precision x1
  double precision x2
!
  external func
!
  x1 = 1.0D+00
  x2 = 1.0D+00 / sqrt ( dble ( n ) )

  w1 = 1.0D+00 / dble ( n * ( n + 2 ) )
  w2 = dble ( n ) / dble ( ( n + 2 ) * 2**n )

  x(1:n) = 0.0D+00

  quad = 0.0D+00

  do i = 1, n
    x(i) = x1
    quad = quad + w1 * func ( n, x )
    x(i) = -x1
    quad = quad + w1 * func ( n, x )
    x(i) = 0.0D+00
  end do

  more = .false.
  ihi = 2**n

  x(1:n) = - x2

  do i = 1, ihi

    call subset_next ( n, ix, more, ncard, iadd )

    if ( iadd /= 0 ) then
      x(iadd) = - x(iadd)
    end if

    quad = quad + w2 * func ( n, x )

  end do

  volume = sphere_unit_area_nd ( n )
  result = quad * volume

  return
end
subroutine sphere_unit_surface_7_1_nd ( func, n, result )
!
!*******************************************************************************
!
!! SPHERE_UNIT_SURFACE_7_1_ND approximates surface integrals on the unit sphere in ND.
!
!
!  Discussion:
!
!    A 2**N + 2*N**2 point 7th degree formula is used, Stroud number UN:7-1.
!
!  Integration region:
!
!    N dimensional points X() such that:
!
!      SUM ( I = 1 to N ) X(I)**2 = 1.
!
!  Reference:
!
!    Arthur H Stroud,
!    Approximate Calculation of Multiple Integrals,
!    Prentice Hall, 1971.
!
!  Modified:
!
!    20 December 2000
!
!  Parameters:
!
!    Input, external FUNC, the name of the user supplied
!    function which evaluates F(X), at the N dimensional point
!    X, of the form
!
!      function func ( n, x )
!      integer n
!      double precision func
!      double precision x(n)
!
!    Input, integer N, the dimension of the space.
!
!    Output, double precision RESULT, the approximate integral of the function.
!
  integer n
!
  double precision func
  integer i
  integer iadd
  integer ix(n)
  integer j
  integer jhi
  logical more
  integer ncard
  double precision quad
  double precision result
  double precision sphere_unit_area_nd
  double precision volume
  double precision w1
  double precision w2
  double precision w3
  double precision x(n)
  double precision x1
  double precision x2
  double precision x3
!
  external func
!
  w1 = dble ( 8 - n ) / dble ( n * ( n + 2 ) * ( n + 4 ) )
  w2 = dble ( n**3 ) / dble ( 2**n * n * ( n + 2 ) * ( n + 4 ) )
  w3 = 4.0D+00 / dble ( n * ( n + 2 ) * ( n + 4 ) )

  x1 = 1.0D+00
  x2 = 1.0D+00 / sqrt ( dble ( n ) )
  x3 = 1.0D+00 / sqrt ( 2.0D+00 )

  x(1:n) = 0.0D+00

  quad = 0.0D+00
!
!  First term.
!
  do i = 1, n
    x(i) = x1
    quad = quad + w1 * func ( n, x )
    x(i) = -x1
    quad = quad + w1 * func ( n, x )
    x(i) = 0.0D+00
  end do
!
!  Second term.
!
  x(1:n) = -x2

  more = .false.
  jhi = 2**n

  do j = 1, jhi

    call subset_next ( n, ix, more, ncard, iadd )

    if ( iadd /= 0 ) then
      x(iadd) = -x(iadd)
    end if

    quad = quad + w2 * func ( n, x )

  end do
!
!  Third term.
!
  x(1:n) = 0.0D+00

  do i = 1, n-1
    do j = i+1, n
      x(i) = x3
      x(j) = x3
      quad = quad + w3 * func ( n, x )
      x(i) = -x3
      x(j) = x3
      quad = quad + w3 * func ( n, x )
      x(i) = x3
      x(j) = -x3
      quad = quad + w3 * func ( n, x )
      x(i) = -x3
      x(j) = -x3
      quad = quad + w3 * func ( n, x )
      x(i) = 0.0D+00
      x(j) = 0.0D+00
    end do
  end do

  volume = sphere_unit_area_nd ( n )
  result = quad * volume

  return
end
subroutine sphere_unit_surface_7_2_nd ( func, n, result )
!
!*******************************************************************************
!
!! SPHERE_UNIT_SURFACE_7_2_ND approximates surface integrals on the unit sphere in ND.
!
!
!  Discussion:
!
!    A 2**N * ( N + 1 ) point 7th degree formula is used, Stroud number UN:7-2.
!
!    Some of the weights in this quadrature formula are negative.
!
!  Integration region:
!
!    N dimensional points X() such that:
!
!      SUM ( I = 1 to N ) X(I)**2 = 1.
!
!  Reference:
!
!    Arthur H Stroud,
!    Approximate Calculation of Multiple Integrals,
!    Prentice Hall, 1971.
!
!  Modified:
!
!    20 December 2000
!
!  Parameters:
!
!    Input, external FUNC, the name of the user supplied
!    function which evaluates F(X), at the N dimensional point
!    X, of the form
!
!      function func ( n, x )
!      integer n
!      double precision func
!      double precision x(n)
!
!    Input, integer N, the dimension of the space.
!
!    Output, double precision RESULT, the approximate integral of the function.
!
  integer n
!
  double precision func
  integer iadd
  integer i
  integer ix(n)
  integer j
  integer jhi
  logical more
  integer ncard
  double precision quad
  double precision result
  double precision sphere_unit_area_nd
  double precision volume
  double precision w1
  double precision w2
  double precision x(n)
  double precision x1
  double precision x2
  double precision x3
!
  external func
!
  x(1:n) = 0.0D+00

  w1 = - dble ( n**2 ) / dble ( 2**(n+3) * ( n + 2 ) )
  w2 = dble ( ( n + 4 )**2 ) / dble ( 2**(n+3) * n * ( n + 2 ) )
  x1 = 1.0D+00 / sqrt ( dble ( n ) )
  x2 = sqrt ( 5.0D+00 / dble ( n + 4 ) )
  x3 = 1.0D+00 / sqrt ( dble ( n + 4 ) )

  quad = 0.0D+00

  x(1:n) = -x1

  more = .false.
  jhi = 2**n

  do j = 1, jhi

    call subset_next ( n, ix, more, ncard, iadd )

    if ( iadd /= 0 ) then
      x(iadd) = -x(iadd)
    end if

    quad = quad + w1 * func ( n, x )

  end do

  do i = 1, n

    x(1:n) = -x3

    x(i) = -x2
    more = .false.

    do j = 1, jhi

      call subset_next ( n, ix, more, ncard, iadd )

      if ( iadd /= 0 ) then
        x(iadd) = - x(iadd)
      end if

      quad = quad + w2 * func ( n, x )

    end do

  end do

  volume = sphere_unit_area_nd ( n )
  result = quad * volume

  return
end
function sphere_unit_volume_3d ( )
!
!*******************************************************************************
!
!! SPHERE_UNIT_VOLUME_3D computes the volume of a unit sphere in 3D.
!
!
!  Modified:
!
!    20 December 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, double precision SPHERE_UNIT_VOLUME_3D, the volume of the sphere.
!
  double precision pi
  double precision sphere_unit_volume_3d
!
  sphere_unit_volume_3d = ( 4.0D+00 / 3.0D+00 ) * pi ( )

  return
end
function sphere_unit_volume_nd ( n )
!
!*******************************************************************************
!
!! SPHERE_UNIT_VOLUME_ND computes the volume of a unit sphere in ND.
!
!
!  Discussion:
!
!    N  Volume
!
!    2             PI
!    3  (4/3)    * PI
!    4  (1/2)    * PI**2
!    5  (8/15)   * PI**2
!    6  (1/6)    * PI**3
!    7  (16/105) * PI**3
!
!  Modified:
!
!    26 October 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the dimension of the space.
!
!    Output, double precision SPHERE_UNIT_VOLUME_ND, the volume of the sphere.
!
  integer i
  integer m
  integer n
  double precision pi
  double precision sphere_unit_volume_nd
  double precision volume
!
  if ( mod ( n, 2 ) == 0 ) then
    m = n / 2
    volume = ( pi() )**m
    do i = 1, m
      volume = volume / dble ( i )
    end do
  else
    m = ( n - 1 ) / 2
    volume = ( pi() )**m * 2.0D+00**n
    do i = m+1, 2*m+1
      volume = volume / dble ( i )
    end do
  end if

  sphere_unit_volume_nd = volume

  return
end
function sphere_volume_3d ( r )
!
!*******************************************************************************
!
!! SPHERE_VOLUME_3D computes the volume of a sphere in 3D.
!
!
!  Modified:
!
!    12 December 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, double precision R, the radius of the sphere.
!
!    Output, double precision SPHERE_VOLUME_3D, the volume of the sphere.
!
  double precision pi
  double precision r
  double precision sphere_volume_3d
!
  sphere_volume_3d = ( 4.0D+00 / 3.0D+00 ) * pi ( ) * r**3

  return
end
function sphere_volume_nd ( n, r )
!
!*******************************************************************************
!
!! SPHERE_VOLUME_ND computes the volume of a sphere in ND.
!
!
!  Discussion:
!
!    N  Volume
!
!    2             PI    * R**2
!    3  (4/3)    * PI    * R**3
!    4  (1/2)    * PI**2 * R**4
!    5  (8/15)   * PI**2 * R**5
!    6  (1/6)    * PI**3 * R**6
!    7  (16/105) * PI**3 * R**7
!
!  Modified:
!
!    26 October 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the dimension of the space.
!
!    Input, double precision R, the radius of the sphere.
!
!    Output, double precision SPHERE_VOLUME_ND, the volume of the sphere.
!
  integer n
  double precision r
  double precision sphere_unit_volume_nd
  double precision sphere_volume_nd
!
  sphere_volume_nd = sphere_unit_volume_nd ( n ) * r**n

  return
end
subroutine square_sum ( func, xc, yc, r, norder, xtab, ytab, weight, result )
!
!*******************************************************************************
!
!! SQUARE_SUM carries out a quadrature rule over a square.
!
!
!  Integration interval:
!
!    abs ( X - XC ) <= R
!    abs ( Y - YC ) <= R
!
!  Modified:
!
!    20 November 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, EXTERNAL FUNC, the name of the function to be
!    integrated.  The user must declare the name an EXTERNAL
!    parameter in the calling program, pass the name of the
!    function in FUNC, and write a function of the form
!
!      FUNCTION FUNC(X,Y)
!
!    which evaluates the function at the point (X,Y).
!
!    Input, integer NORDER, the order of the rule.
!
!    Input, double precision XTAB(NORDER), YTAB(NORDER), the abscissas of the rule.
!
!    Input, double precision WEIGHT(NORDER), the weights of the rule.
!
!    Output, double precision RESULT, the approximate integral of the function.
!
  integer norder
!
  double precision func
  integer i
  double precision quad
  double precision r
  double precision result
  double precision volume
  double precision weight(norder)
  double precision x
  double precision xc
  double precision xtab(norder)
  double precision y
  double precision yc
  double precision ytab(norder)
!
  external func
!
  quad = 0.0D+00
  do i = 1, norder
    x = xc + r * xtab(i)
    y = yc + r * ytab(i)
    quad = quad + 0.25D+00 * weight(i) * func ( x, y )
  end do

  volume = 4.0D+00 * r**2
  result = quad * volume

  return
end
subroutine square_unit_set ( rule, norder, xtab, ytab, weight )
!
!*******************************************************************************
!
!! SQUARE_UNIT_SET sets weights and abscissas for quadrature within a unit square.
!
!
!  Integration interval:
!
!    -1 <= X <= 1,
!    -1 <= Y <= 1.
!
!  References:
!
!    Strang and Fix,
!    An Analysis of the Finite Element Method,
!    Prentice Hall, 1973.
!
!    Arthur H Stroud,
!    Approximate Calculation of Multiple Integrals,
!    Prentice Hall, 1971.
!
!  Modified:
!
!    07 November 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer RULE, the rule number.
!    1, a 1 point 1st degree rule.
!    2, a 4 point 3rd degree rule.
!    3, a 9 point 5th degree rule.
!    4, a 12 point 7-th degree rule, Stroud number C2:7-1.
!    5, a 13 point 7-th degree rule, Stroud number C2:7-3.
!    6, a 64 point 15-th degree product rule.
!
!    Output, integer NORDER, the order of the rule.
!
!    Output, double precision XTAB(*), YTAB(*), the NORDER abscissas of the rule.
!
!    Output, double precision WEIGHT(*), the weights of the rule.
!
  integer, parameter :: norder2 = 8
!
  double precision a
  double precision c
  integer i
  integer j
  integer k
  integer norder
  double precision r
  integer rule
  double precision s
  double precision t
  double precision w
  double precision w1
  double precision w2
  double precision w3
  double precision weight(*)
  double precision weight2(norder2)
  double precision xtab(*)
  double precision xtab2(norder2)
  double precision ytab(*)
  double precision z
!
  if ( rule == 1 ) then

    norder = 1
    weight(1) = 4.0D+00

    xtab(1) = 0.0D+00
    ytab(1) = 0.0D+00

  else if ( rule == 2 ) then

    a = 1.0D+00
    s = 1.0D+00 / sqrt ( 3.0D+00 )

    norder = 4
    xtab(1:4) =   (/ -s, +s, -s, +s /)
    ytab(1:4) =   (/ -s, -s, +s, +s /)
    weight(1:4) = (/  a,  a,  a,  a /)

  else if ( rule == 3 ) then

    s = sqrt ( 0.6D+00 )
    z = 0.0D+00
    w1 = 64.0D+00 / 81.0D+00
    w2 = 25.0D+00 / 81.0D+00
    w3 = 40.0D+00 / 81.0D+00

    norder = 9
    xtab(1:9) =   (/   z,  -s, +s, -s, +s,   z, -s, +s,  z /)
    ytab(1:9) =   (/   z,  -s, -s, +s, +s,  -s,  z,  z, +s /)
    weight(1:9) = (/  w1,  w2, w2, w2, w2,  w3, w3, w3, w3 /)

  else if ( rule == 4 ) then

    r = sqrt ( 6.0D+00 / 7.0D+00 )
    c = 3.0D+00 * sqrt ( 583.0D+00 )
    s = sqrt ( ( 114.0D+00 - c ) / 287.0D+00 )
    t = sqrt ( ( 114.0D+00 + c ) / 287.0D+00 )
    w1 = 4.0D+00 * 49.0D+00 / 810.0D+00
    w2 = 4.0D+00 * ( 178981.0D+00 + 923.0D+00 * c ) / 1888920.0D+00
    w3 = 4.0D+00 * ( 178981.0D+00 - 923.0D+00 * c ) / 1888920.0D+00
    z = 0.0D+00

    norder = 12
    xtab(1:12) =   (/   r,  z, -r,  z,   s, -s, -s,  s,  t, -t, -t,  t /)
    ytab(1:12) =   (/   z,  r,  z,  -r,  s,  s, -s, -s,  t,  t, -t, -t /)
    weight(1:12) = (/  w1, w1,  w1, w1, w2, w2, w2, w2, w3, w3, w3, w3 /)

  else if ( rule == 5 ) then

    r = sqrt ( 12.0D+00 / 35.0D+00 )
    c = 3.0D+00 * sqrt ( 186.0D+00 )
    s = sqrt ( ( 93.0D+00 + c ) / 155.0D+00 )
    t = sqrt ( ( 93.0D+00 - c ) / 155.0D+00 )
    w1 =  8.0D+00 / 162.0D+00
    w2 = 98.0D+00 / 162.0D+00
    w3 = 31.0D+00 / 162.0D+00
    z = 0.0D+00

    norder = 13
    xtab(1:13) =   (/  z,  r, -r,  z,  z,  s,  s, -s, -s,  t,  t, -t, -t /)
    ytab(1:13) =   (/  z,  z,  z,  r, -r,  t, -t,  t, -t,  s, -s,  s, -s /)
    weight(1:13) = (/ w1, w2, w2, w2, w2, w3, w3, w3, w3, w3, w3, w3, w3 /)

  else if ( rule == 6 ) then

    norder = 64
    call legendre_set ( norder2, xtab2, weight2 )

    k = 0

    do i = 1, norder2

      do j = 1, norder2

        k = k + 1
        xtab(k) = xtab2(i)
        ytab(k) = xtab2(j)
        weight(k) = weight2(i) * weight2(j)

      end do

    end do

  else

    write ( *, * ) ' '
    write ( *, * ) 'SQUARE_UNIT_SET - Fatal error!'
    write ( *, * ) '  Illegal value of NORDER = ', norder
    stop

  end if

  return
end
subroutine square_unit_sum ( func, norder, xtab, ytab, weight, result )
!
!*******************************************************************************
!
!! SQUARE_UNIT_SUM carries out a quadrature rule over the unit square.
!
!
!  Integration interval:
!
!    -1 <= X <= 1,
!    -1 <= Y <= 1.
!
!  Modified:
!
!    25 August 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, EXTERNAL FUNC, the name of the function to be
!    integrated.  The user must declare the name an EXTERNAL
!    parameter in the calling program, pass the name of the
!    function in FUNC, and write a function of the form
!
!      FUNCTION FUNC(X,Y)
!
!    which evaluates the function at the point (X,Y).
!
!    Input, integer NORDER, the order of the rule.
!
!    Input, double precision XTAB(NORDER), YTAB(NORDER), the abscissas of the rule.
!
!    Input, double precision WEIGHT(NORDER), the weights of the rule.
!
!    Output, double precision RESULT, the approximate integral of the function.
!
  integer norder
!
  double precision func
  integer i
  double precision quad
  double precision result
  double precision volume
  double precision weight(norder)
  double precision xtab(norder)
  double precision ytab(norder)
!
  external func
!
  quad = 0.0D+00
  do i = 1, norder
    quad = quad + weight(i) * func ( xtab(i), ytab(i) ) / 4.0D+00
  end do

  volume = 1.0D+00
  result = quad * volume

  return
end
subroutine subset_next ( n, a, more, ncard, iadd )
!
!*******************************************************************************
!
!! SUBSET_NEXT generates all subsets of a set of order N, one at a time.
!
!
!  Discussion:
!
!    It generates the subsets one at a time, by adding or subtracting
!    exactly one element on each step.
!
!    The user should set MORE = .FALSE. and the value of N before
!    the first call.  On return, the user may examine A which contains
!    the definition of the new subset, and must check .MORE., because
!    as soon as it is .FALSE. on return, all the subsets have been
!    generated and the user probably should cease calling.
!
!    The first set returned is the empty set.
!
!  Reference:
!
!    A Nijenhuis and H Wilf,
!    Combinatorial Algorithms,
!    Academic Press, 1978, second edition,
!    ISBN 0-12-519260-6.
!
!  Modified:
!
!    15 April 1999
!
!  Parameters:
!
!    Input, integer N, the order of the total set from which
!    subsets will be drawn.
!
!    Output, integer A(N).  On each return, the Gray code for the newly
!    generated subset.  A(I) = 0 if element I is in the subset, 1 otherwise.
!
!    Input/output, logical MORE.  Set this variable .FALSE. before
!    the first call.  Normally, MORE will be returned .TRUE. but once
!    all the subsets have been generated, MORE will be
!    reset .FALSE. on return and you should stop calling the program.
!
!    Output, integer NCARD, the cardinality of the set returned,
!    which may be any value between 0 (the empty set) and N (the
!    whole set).
!
!    Output, integer IADD, the element which was added or removed to the
!    previous subset to generate the current one.  Exception:
!    the empty set is returned on the first call, and IADD is set to 0.
!
  integer n
!
  integer a(n)
  integer i
  integer iadd
  logical more
  integer ncard
!
!  First set returned is the empty set.
!
  if ( .not. more ) then

    a(1:n) = 0

    iadd = 0
    ncard = 0
    more = .true.

  else

    iadd = 1

    if ( mod ( ncard, 2 ) /= 0 ) then

      do

        iadd = iadd + 1
        if ( a(iadd-1) /= 0 ) then
          exit
        end if

      end do

    end if

    a(iadd) = 1 - a(iadd)
    ncard = ncard + 2 * a(iadd) - 1
!
!  Last set returned is the singleton A(N).
!
    if ( ncard == a(n) ) then
      more = .false.
    end if

  end if

  return
end
subroutine tetra_07 ( func, x, y, z, result )
!
!*******************************************************************************
!
!! TETRA_07 approximates an integral inside a tetrahedron in 3D.
!
!
!  Discussion:
!
!    A 64 point 7-th degree conical product Gauss formula is used,
!    Stroud number T3:7-1.
!
!  Integration region:
!
!    Points inside a tetrahedron whose four corners are given.
!
!  Reference:
!
!    Arthur H Stroud,
!    Approximate Calculation of Multiple Integrals,
!    Prentice Hall, 1971.
!
!    Stroud and Secrest,
!    Gaussian Quadrature Formulas,
!    Prentice Hall, 1966, pages 42-43.
!
!  Modified:
!
!    08 December 2000
!
!  Parameters:
!
!    Input, external FUNC, the name of the user supplied
!    function of three variables which is to be integrated,
!    of the form:
!
!      function func ( x, y, z )
!      double precision func
!      double precision x
!      double precision y
!      double precision z
!
!    Input, double precision X(4), Y(4), Z(4), the X, Y and Z coordinates of the vertices.
!
!    Output, double precision RESULT, the approximate integral of the function.
!
  integer, parameter :: norder = 4
!
  double precision a
  double precision b
  double precision c
  double precision d
  double precision func
  integer i
  integer j
  integer k
  double precision quad
  double precision result
  double precision t
  double precision tetra_volume
  double precision u
  double precision v
  double precision volume
  double precision w
  double precision weight1(norder)
  double precision, save, dimension ( norder ) :: weight2 = (/ &
    0.1355069134D+00, 0.2034645680D+00, 0.1298475476D+00, 0.0311809709D+00 /)
  double precision, save, dimension ( norder ) :: weight3 = (/ &
    0.1108884156D+00, 0.1434587898D+00, 0.0686338872D+00, 0.0103522407D+00 /)
  double precision x(4)
  double precision xtab1(norder)
  double precision, save, dimension ( norder ) :: xtab2 = (/ &
    0.0571041961D+00, 0.2768430136D+00, 0.5835904324D+00, 0.8602401357D+00 /)
  double precision, save, dimension ( norder ) :: xtab3 = (/ &
    0.0485005495D+00, 0.2386007376D+00, 0.5170472951D+00, 0.7958514179D+00 /)
  double precision xval
  double precision y(4)
  double precision yval
  double precision z(4)
  double precision zval
!
  external func
!
!  Get the Gauss-Legendre weights and abscissas for [-1,1].
!
  call legendre_set ( norder, xtab1, weight1 )
!
!  Adjust the rule for the interval [0,1].
!
  a = -1.0D+00
  b = +1.0D+00

  c =  0.0D+00
  d =  1.0D+00

  call rule_adjust ( a, b, c, d, norder, xtab1, weight1 )
!
!  Carry out the quadrature.
!
  quad = 0.0D+00

  do i = 1, norder
    do j = 1, norder
      do k = 1, norder
!
!  Compute the barycentric coordinates of the point in the unit triangle.
!
        t =                                                 xtab3(k)
        u =                        xtab2(j)   * ( 1.0D+00 - xtab3(k) )
        v = xtab1(i) * ( 1.0D+00 - xtab2(j) ) * ( 1.0D+00 - xtab3(k) )
        w = 1.0D+00 - t - u - v
!
!  Compute the corresponding point in the triangle.
!
        xval = t * x(1) + u * x(2) + v * x(3) + w * x(4)
        yval = t * y(1) + u * y(2) + v * y(3) + w * y(4)
        zval = t * z(1) + u * z(2) + v * z(3) + w * z(4)

        quad = quad + 6.0D+00 * weight1(i) * weight2(j) * weight3(k) &
          * func ( xval, yval, zval )

      end do
    end do
  end do

  volume = tetra_volume ( x, y, z )
  result = quad * volume

  return
end
subroutine tetra_sum ( func, x, y, z, norder, xtab, ytab, ztab, weight, result )
!
!*******************************************************************************
!
!! TETRA_SUM carries out a quadrature rule in a tetrahedron in 3D.
!
!
!  Integration region:
!
!    A tetrahedron whose vertices are specified.
!
!  Modified:
!
!    19 November 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, external FUNC, name of the function, of the form:
!
!      function func ( x, y, z )
!      double precision func
!      double precision x
!      double precision y
!      double precision z
!
!    Input, double precision X(4), Y(4), Z(4), the coordinates of the vertices.
!
!    Input, integer NORDER, the order of the rule.
!
!    Input, double precision XTAB(NORDER), YTAB(NORDER), ZTAB(NORDER), the abscissas.
!
!    Input, double precision WEIGHT(NORDER), the weights.
!
!    Output, double precision RESULT, the approximate integral of the function.
!
  integer norder
!
  double precision func
  integer i
  double precision quad
  double precision result
  double precision tetra_volume
  double precision volume
  double precision weight(norder)
  double precision x(4)
  double precision xtab(norder)
  double precision xval
  double precision y(4)
  double precision ytab(norder)
  double precision yval
  double precision z(4)
  double precision ztab(norder)
  double precision zval
!
  external func
!
  quad = 0.0D+00

  do i = 1, norder

    xval = xtab(i) * x(1) + ytab(i) * x(2) + ztab(i) * x(3) &
      + ( 1.0D+00 - xtab(i) - ytab(i) - ztab(i) ) * x(4)
    yval = xtab(i) * y(1) + ytab(i) * y(2) + ztab(i) * y(3) &
      + ( 1.0D+00 - xtab(i) - ytab(i) - ztab(i) ) * y(4)
    xval = xtab(i) * z(1) + ytab(i) * z(2) + ztab(i) * z(3) &
      + ( 1.0D+00 - xtab(i) - ytab(i) - ztab(i) ) * z(4)

    quad = quad + weight(i) * func ( xval, yval, zval )

  end do

  volume = tetra_volume ( x, y, z )
  result = quad * volume

  return
end
subroutine tetra_tproduct ( func, norder, x, y, z, result )
!
!*******************************************************************************
!
!! TETRA_TPRODUCT approximates an integral in a tetrahedron in 3D.
!
!
!  Discussion:
!
!    An NORDER**3 point (2*NORDER-1)-th degree triangular product
!    Gauss-Legendre rule is used.
!
!    With NORDER = 8, this routine is equivalent to the routine TETR15
!    in the reference, page 367.
!
!    Thanks to Joerg Behrens, jbehren@gwdg.de, for numerous suggestions
!    and corrections.
!
!  Integration region:
!
!    Points inside a tetrahedron whose four corners are given.
!
!  Reference:
!
!    Arthur H Stroud,
!    Approximate Calculation of Multiple Integrals,
!    Prentice Hall, 1971.
!
!  Modified:
!
!    18 December 2000
!
!  Parameters:
!
!    Input, external FUNC, the name of the user supplied
!    function of three variables which is to be integrated,
!    of the form:
!
!      function func ( x, y, z )
!      double precision func
!      double precision x
!      double precision y
!      double precision z
!
!    Input, integer NORDER, the order of the basic quadrature rules.
!    NORDER should be between 1 and 9.
!
!    Input, double precision X(4), Y(4), Z(4), the coordinates of the vertices
!    of the tetrahedron.
!
!    Output, double precision RESULT, the approximate integral of the function.
!
  integer norder
!
  double precision a
  double precision b
  double precision c
  double precision d
  double precision func
  integer i
  integer j
  integer k
  double precision quad
  double precision result
  double precision tetra_volume
  double precision volume
  double precision, dimension ( norder ) :: weight0
  double precision, dimension ( norder ) :: weight1
  double precision, dimension ( norder ) :: weight2
  double precision x(4)
  double precision, dimension ( norder ) :: xtab0
  double precision, dimension ( norder ) :: xtab1
  double precision, dimension ( norder ) :: xtab2
  double precision xval
  double precision y(4)
  double precision yval
  double precision z(4)
  double precision zval
!
  external func
!
  if ( norder < 1 .or. norder > 9 ) then
    write ( *, * ) ' '
    write ( *, * ) 'TETRA_TPRODUCT - Fatal error!'
    write ( *, * ) '  The quadrature rule orders must be between 1 and 9.'
    write ( *, * ) '  The input value was NORDER = ', norder
    stop
  end if
!
!  Get the Gauss-Legendre NORDER point rules on [-1,1] for integrating
!    F(X),
!    X * F(X),
!    X * X * F(X).
!
  call legendre_set ( norder, xtab0, weight0 )
  call legendre_set_x1 ( norder, xtab1, weight1 )
  call legendre_set_x2 ( norder, xtab2, weight2 )
!
!  Adjust the rules from [-1,1] to [0,1].
!
  a = -1.0D+00
  b = +1.0D+00
  c =  0.0D+00
  d =  1.0D+00

  call rule_adjust ( a, b, c, d, norder, xtab0, weight0 )

  call rule_adjust ( a, b, c, d, norder, xtab1, weight1 )

  call rule_adjust ( a, b, c, d, norder, xtab2, weight2 )
!
!  For rules with a weight function that is not 1, the weight vectors
!  require further adjustment.
!
  weight1(1:norder) = weight1(1:norder) / 2.0D+00
  weight2(1:norder) = weight2(1:norder) / 4.0D+00
!
!  Carry out the quadrature.
!
  quad = 0.0D+00

  do k = 1, norder
    do j = 1, norder
      do i = 1, norder

        xval = x(1) + ( ( ( x(4) - x(3) )   * xtab0(i) &
                        + ( x(3) - x(2) ) ) * xtab1(j) &
                        + ( x(2) - x(1) ) ) * xtab2(k)

        yval = y(1) + ( ( ( y(4) - y(3) )   * xtab0(i) &
                        + ( y(3) - y(2) ) ) * xtab1(j) &
                        + ( y(2) - y(1) ) ) * xtab2(k)

        zval = z(1) + ( ( ( z(4) - z(3) )   * xtab0(i) &
                        + ( z(3) - z(2) ) ) * xtab1(j) &
                        + ( z(2) - z(1) ) ) * xtab2(k)

        quad = quad + 6.0D+00 * weight0(i) * weight1(j) * weight2(k) &
          * func ( xval, yval, zval )

      end do

    end do

  end do
!
!  Compute the volume of the tetrahedron.
!
  volume = tetra_volume ( x, y, z )
  result = quad * volume

  return
end
subroutine tetra_unit_set ( rule, norder, xtab, ytab, ztab, weight )
!
!*******************************************************************************
!
!! TETRA_UNIT_SET sets weights and abscissas for quadrature on a unit tetrahedron.
!
!
!  Integration region:
!
!    0 <= X
!    0 <= Y
!    0 <= Z
!    X + Y + Z <= 1.
!
!  References:
!
!    H Engels,
!    Numerical Quadrature and Cubature,
!    Academic Press, 1980.
!
!    O C Zienkiewicz,
!    The Finite Element Method,
!    McGraw Hill, Third Edition, 1977, page 202.
!
!  Modified:
!
!    11 December 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer RULE, the index of the rule.
!
!     1, NORDER =  1, precision 0, Newton Cotes formula, Zienkiewicz #1.
!     2, NORDER =  4, precision 1, Newton Cotes formula.
!     3, NORDER =  4, Zienkiewicz #2.
!     4, NORDER =  10, precision 2, Newton Cotes formula.
!     5, NORDER =  5, Zienkiewicz #3.
!     6, NORDER =  8, precision 3, Newton Cotes formula.
!     7, NORDER =  35, precision 4, Newton Cotes formula.
!
!    Output, integer NORDER, the order of the rule.
!
!    Output, double precision XTAB(NORDER), YTAB(NORDER), ZTAB(NORDER),
!    the abscissas.
!
!    Output, double precision WEIGHT(NORDER), the weights.
!
  double precision a
  double precision b
  double precision c
  double precision d
  double precision e
  double precision f
  double precision g
  double precision h
  integer norder
  integer rule
  double precision weight(*)
  double precision xtab(*)
  double precision ytab(*)
  double precision z
  double precision ztab(*)
!
!  Newton Cotes.
!
  if ( rule == 1 ) then

    norder = 1
    xtab(1) = 1.0D+00 / 3.0D+00
    ytab(1) = 1.0D+00 / 3.0D+00
    ztab(1) = 1.0D+00 / 3.0D+00
    weight(1) = 1.0D+00
!
!  Newton Cotes.
!
  else if ( rule == 2 ) then

    a = 1.0D+00
    b = 1.0D+00 / 4.0D+00
    z = 0.0D+00

    norder = 4
    xtab(1:4) =   (/ z, a, z, z /)
    ytab(1:4) =   (/ z, z, a, z /)
    ztab(1:4) =   (/ z, z, z, a /)
    weight(1:4) = (/ b, b, b, b /)
!
!  Zienkiewicz #2.
!
  else if ( rule == 3 ) then

    a =  0.58541020D+00
    b =  0.13819660D+00
    c =  0.25D+00

    norder = 4
    xtab(1:4) =   (/ a, b, b, b /)
    ytab(1:4) =   (/ b, a, b, b /)
    ztab(1:4) =   (/ b, b, a, b /)
    weight(1:4) = (/ c, c, c, c /)
!
!  Newton Cotes.
!
  else if ( rule == 4 ) then

    a =  1.0D+00
    b =  0.5D+00
    c = -1.0D+00 / 20.0D+00
    d =  4.0D+00 / 20.0D+00
    z =  0.0D+00

    norder = 10
    xtab(1:10) =   (/ z, a, z, z, b, z, z, b, b, z /)
    ytab(1:10) =   (/ z, z, a, z, z, b, z, b, z, b /)
    ztab(1:10) =   (/ z, z, z, a, z, z, b, z, b, b /)
    weight(1:10) = (/ c, c, c, c, d, d, d, d, d, d /)
!
!  Zienkiewicz #3.
!
  else if ( rule == 5 ) then

    a =   1.0D+00 / 6.0D+00
    b =   0.25D+00
    c =   0.5D+00
    d = - 0.8D+00
    e =   0.45D+00

    norder = 5
    xtab(1:5) =   (/ b, c, a, a, a /)
    ytab(1:5) =   (/ b, a, c, a, a /)
    ztab(1:5) =   (/ b, a, a, c, a /)
    weight(1:5) = (/ d, e, e, e, e /)
!
!  Newton Cotes.
!
  else if ( rule == 6 ) then

    a = 1.0D+00
    b = 1.0D+00 / 40.0D+00
    c = 1.0D+00 /  3.0D+00
    d = 9.0D+00 / 40.0D+00
    z = 0.0D+00

    norder = 8
    xtab(1:8) =   (/ z, a, z, z, c, c, z, c /)
    ytab(1:8) =   (/ z, z, a, z, c, z, c, c /)
    ztab(1:8) =   (/ z, z, z, a, z, c, c, c /)
    weight(1:8) = (/ b, b, b, b, d, d, d, d /)
!
!  Newton Cotes.
!
  else if ( rule == 7 ) then

    a =   0.25D+00
    b =   0.50D+00
    c =   0.75D+00
    d =   1.00D+00
    e =  -5.0D+00 / 420.0D+00
    f = -12.0D+00 / 420.0D+00
    g =  16.0D+00 / 420.0D+00
    h = 128.0D+00 / 420.0D+00
    z =   0.0D+00

    norder = 35

    xtab(1:35) =   (/ z, d, z, z, a, z, z, c, c, c, z, a, z, z, a, z, b, z, z, &
                      b, b, z, a, b, a, a, b, z, b, z, a, a, z, a, a /)
    ytab(1:35) =   (/ z, z, d, z, z, a, z, z, a, z, c, c, c, z, z, a, z, b, z, &
                      b, z, b, a, a, b, z, z, a, a, b, b, z, a, a, a /)
    ztab(1:35) =   (/ z, z, z, d, z, z, a, z, z, a, z, z, a, c, c, c, z, z, b, &
                      z, b, b, z, z, z, a, a, a, a, a, a, b, b, b, a /)

    weight(1:35) = (/ e, e, e, e, g, g, g, g, g, g, g, g, g, g, g, g, f, f, f, &
                      f, f, f, g, g, g, g, g, g, g, g, g, g, g, g, h /)

  end if

  return
end
subroutine tetra_unit_sum ( func, norder, xtab, ytab, ztab, weight, result )
!
!*******************************************************************************
!
!! TETRA_UNIT_SUM carries out a quadrature rule in the unit tetrahedron in 3D.
!
!
!  Integration region:
!
!    0 <= X
!    0 <= Y
!    0 <= Z
!    X + Y + Z <= 1.
!
!  Modified:
!
!    26 October 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, external FUNC, the name of the user supplied
!    function of three variables which is to be integrated,
!    of the form:
!
!      function func ( x, y, z )
!      double precision func
!      double precision x
!      double precision y
!      double precision z
!
!    Input, integer NORDER, the order of the rule.
!
!    Input, double precision XTAB(NORDER), YTAB(NORDER), ZTAB(NORDER), the abscissas.
!
!    Input, double precision WEIGHT(NORDER), the weights.
!
!    Output, double precision RESULT, the approximate integral of the function.
!
  integer norder
!
  double precision func
  integer i
  double precision quad
  double precision result
  double precision tetra_unit_volume
  double precision volume
  double precision weight(norder)
  double precision xtab(norder)
  double precision ytab(norder)
  double precision ztab(norder)
!
  external func
!
  quad = 0.0D+00

  do i = 1, norder
    quad = quad + weight(i) * func ( xtab(i), ytab(i), ztab(i) )
  end do

  volume = tetra_unit_volume ( )
  result = quad * volume

  return
end
function tetra_unit_volume ( )
!
!*******************************************************************************
!
!! TETRA_UNIT_VOLUME returns the volume of the unit tetrahedron in 3D.
!
!
!  Modified:
!
!    27 October 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, double precision TETRA_UNIT_VOLUME, the volume of the unit tetrahedron.
!
  double precision tetra_unit_volume
!
  tetra_unit_volume = 1.0D+00 / 6.0D+00

  return
end
function tetra_volume ( x, y, z )
!
!*******************************************************************************
!
!! TETRA_VOLUME computes the volume of a tetrahedron in 3D.
!
!
!  Integration region:
!
!    Points inside a tetrahedron whose four vertices are given.
!
!  Modified:
!
!    19 November 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, double precision X(4), Y(4), Z(4), the vertices.
!
!    Output, double precision TETRA_VOLUME, the volume of the tetrahedron.
!
  double precision parallelipiped_volume_3d
  double precision tetra_unit_volume
  double precision tetra_volume
  double precision volume
  double precision x(4)
  double precision y(4)
  double precision z(4)
!
  volume = parallelipiped_volume_3d ( x, y, z )

  tetra_volume = volume * tetra_unit_volume ( )

  return
end
subroutine torus_1 ( func, r1, r2, n, result )
!
!*******************************************************************************
!
!! TORUS_1 approximates an integral on the surface of a torus in 3D.
!
!
!  Discussion:
!
!    An (N+1)*(N+2) point N-th degree formula is used.
!
!  Integration region:
!
!    Points (X,Y,Z) such that:
!
!      ( SQRT ( X**2 + Y**2 ) - R1 )**2 + Z**2 = R2**2.
!
!  Reference:
!
!    Arthur H Stroud,
!    Approximate Calculation of Multiple Integrals,
!    Prentice Hall, 1971.
!
!  Modified:
!
!    07 November 2000
!
!  Parameters:
!
!    Input, external FUNC, the name of the user supplied
!    function of three variables which is to be integrated,
!    of the form:
!
!      function func ( x, y, z )
!      double precision x
!      double precision y
!      double precision z
!
!    Input, double precision R1, R2, the two radii that define the torus.
!
!    Input, integer N, defines the degree of the formula
!    used to approximate the integral.
!
!    Output, double precision RESULT, the approximate integral of the function.
!
  double precision angle
  double precision ct1
  double precision func
  integer i
  integer j
  integer n
  double precision pi
  double precision quad
  double precision r1
  double precision r2
  double precision result
  double precision st1
  double precision torus_area_3d
  double precision u
  double precision volume
  double precision w
  double precision x
  double precision y
  double precision z
!
  external func
!
  w = 1.0D+00 / ( r1 * dble ( ( n + 1 ) * ( n + 2 ) ) )
  quad = 0.0D+00

  do i = 1, n+1

    angle = 2.0D+00 * pi ( ) * dble ( i ) / dble ( n + 1 )
    ct1 = cos ( angle )
    st1 = sin ( angle )

    do j = 1, n+2

      angle = 2.0D+00 * pi ( ) * dble ( j ) / dble ( n + 2 )
      u = r1 + r2 * cos ( angle )
      x = u * ct1
      y = u * st1
      z = r2 * sin ( angle )

      quad = quad + w * u * func ( x, y, z )

    end do

  end do

  volume = torus_area_3d ( r1, r2 )
  result = quad * volume

  return
end
subroutine torus_14s ( func, r1, r2, result )
!
!*******************************************************************************
!
!! TORUS_14S approximates an integral inside a torus in 3D.
!
!
!  Discussion:
!
!    A 960 point 14-th degree formula is used.
!
!  Integration region:
!
!    Points (X,Y,Z) such that:
!
!      ( SQRT ( X**2 + Y**2 ) - R1 )**2 + Z**2 <= R2**2.
!
!  Reference:
!
!    Arthur H Stroud,
!    Approximate Calculation of Multiple Integrals,
!    Prentice Hall, 1971.
!
!  Modified:
!
!    07 November 2000
!
!  Parameters:
!
!    Input, external FUNC, the name of the user supplied
!    function of three variables which is to be integrated,
!    of the form:
!
!      function func ( x, y, z )
!      double precision func
!      double precision x
!      double precision y
!      double precision z
!
!    Input, double precision R1, R2, the two radii that define the torus.
!
!    Output, double precision RESULT, the approximate integral of the function.
!
  integer, parameter :: norder = 4
!
  double precision angle
  double precision ct
  double precision cth
  double precision func
  integer i
  integer j
  integer n
  double precision pi
  double precision quad
  double precision, save, dimension ( norder ) :: r = (/ &
    0.263499230D+00, 0.574464514D+00, 0.818529487D+00, 0.964659606D+00 /)
  double precision r1
  double precision r2
  double precision result
  double precision st
  double precision sth
  double precision torus_volume_3d
  double precision u
  double precision volume
  double precision, save, dimension ( norder ) :: weight = (/ &
    0.086963711D+00, 0.163036289D+00, 0.163036289D+00, 0.086963711D+00 /)
  double precision x
  double precision y
  double precision z
!
  external func
!
  quad = 0.0D+00

  do n = 1, 15

    angle = 2.0D+00 * pi ( ) * dble ( n ) / 15.0D+00
    cth = cos ( angle )
    sth = sin ( angle )

    do i = 1, 16

      angle = pi ( ) * dble ( i ) / 8.0D+00
      ct = cos ( angle )
      st = sin ( angle )

      do j = 1, norder
        u = r1 + r(j) * ct * r2
        x = u * cth
        y = u * sth
        z = r(j) * st * r2
        quad = quad + u * weight(j) * func ( x, y, z ) / ( 120.0D+00 * r1 )
      end do

    end do

  end do

  volume = torus_volume_3d ( r1, r2 )
  result = quad * volume

  return
end
subroutine torus_5s2 ( func, r1, r2, result )
!
!*******************************************************************************
!
!! TORUS_5S2 approximates an integral inside a torus in 3D.
!
!
!  Discussion:
!
!    A 24 point, 5-th degree formula is used, Stroud number TOR3-S2:5-1.
!
!  Integration region:
!
!    Points (X,Y,Z) such that:
!
!      ( SQRT ( X**2 + Y**2 ) - R1 )**2 + Z**2 <= R2**2.
!
!  Reference:
!
!    Arthur H Stroud,
!    Approximate Calculation of Multiple Integrals,
!    Prentice Hall, 1971.
!
!  Modified:
!
!    21 November 2000
!
!  Parameters:
!
!    Input, external FUNC, the name of the user supplied
!    function of three variables which is to be integrated,
!    of the form:
!
!      function func ( x, y, z )
!      double precision func
!      double precision x
!      double precision y
!      double precision z
!
!    Input, double precision R1, R2, the two radii that define the torus.
!
!    Output, double precision RESULT, the approximate integral of the function.
!
  double precision angle
  double precision cs
  double precision func
  integer i
  double precision pi
  double precision quad
  double precision r1
  double precision r2
  double precision result
  double precision sn
  double precision torus_volume_3d
  double precision u1
  double precision u2
  double precision u3
  double precision volume
  double precision w
  double precision x
  double precision y
  double precision z
!
  external func
!
  w = 1.0D+00 / 24.0D+00

  quad = 0.0D+00

  u1 = sqrt ( r1**2 + 0.5D+00 * r2**2 )
  u2 = sqrt ( r1**2 + sqrt ( 2.0D+00 ) * r1 * r2 + r2**2 )
  u3 = sqrt ( r1**2 - sqrt ( 2.0D+00 ) * r1 * r2 + r2**2 )

  do i = 1, 6

    angle = pi ( ) * dble ( i ) / 3.0D+00
    cs = cos ( angle )
    sn = sin ( angle )

    x = u1 * cs
    y = u1 * sn
    z = r2 / sqrt ( 2.0D+00 )
    quad = quad + w * func ( x, y, z )

    x = u1 * cs
    y = u1 * sn
    z = - r2 / sqrt ( 2.0D+00 )
    quad = quad + w * func ( x, y, z )

    x = u2 * cs
    y = u2 * sn
    z = 0.0D+00
    quad = quad + w * func ( x, y, z )

    x = u3 * cs
    y = u3 * sn
    z = 0.0D+00
    quad = quad + w * func ( x, y, z )

  end do

  volume = torus_volume_3d ( r1, r2 )
  result = quad * volume

  return
end
subroutine torus_6s2 ( func, r1, r2, result )
!
!*******************************************************************************
!
!! TORUS_6S2 approximates an integral inside a torus in 3D.
!
!
!  Discussion:
!
!    An 84 point 6-th degree formula is used, Stroud number TOR3-S2:6-1.
!
!  Integration region:
!
!    Points (X,Y,Z) such that:
!
!      ( SQRT ( X**2 + Y**2 ) - R1 )**2 + Z**2 <= R2**2.
!
!  Reference:
!
!    Arthur H Stroud,
!    Approximate Calculation of Multiple Integrals,
!    Prentice Hall, 1971.
!
!  Modified:
!
!    07 November 2000
!
!  Parameters:
!
!    Input, external FUNC, the name of the user supplied
!    function of three variables which is to be integrated,
!    of the form:
!
!      function func ( x, y, z )
!      double precision func
!      double precision x
!      double precision y
!      double precision z
!
!    Input, double precision R1, R2, the two radii that define the torus.
!
!    Output, double precision RESULT, the approximate integral of the function.
!
  integer, parameter :: norder = 2
!
  double precision cth
  double precision func
  integer i
  integer j
  integer k
  integer n
  double precision pi
  double precision quad
  double precision r1
  double precision r2
  double precision result
  double precision, save, dimension ( norder ) :: s = (/ 0.322914992D+00, 0.644171310D+00 /)
  double precision sth
  double precision torus_volume_3d
  double precision u
  double precision v
  double precision volume
  double precision w
  double precision, save, dimension ( norder ) :: weight = (/ 0.387077796D+00, 0.165609800D+00 /)
  double precision x
  double precision y
  double precision z
!
  external func
!
  w = 1.0D+00 / ( 7.0D+00 * r1 * pi() )

  quad = 0.0D+00

  do n = 1, 7

    u = 0.5D+00 * sqrt ( 3.0D+00 ) * r2
    cth = cos ( 2.0D+00 * pi ( ) * dble ( n ) / 7.0D+00 )
    sth = sin ( 2.0D+00 * pi ( ) * dble ( n ) / 7.0D+00 )

    do i = 1, 2

      u = - u

      x = ( r1 + u ) * cth
      y = ( r1 + u ) * sth
      z = 0.0D+00
      quad = quad + 0.232710567D+00 * w * ( r1 + u ) * func ( x, y, z )

      x = r1 * cth
      y = r1 * sth
      z = u
      quad = quad + 0.232710567D+00 * w * r1 * func ( x, y, z )

    end do

    do k = 1, norder

      u = s(k) * r2
      v = u

      do i = 1, 2

        u = - u

        do j = 1, 2

          v = - v

          x = ( r1 + u ) * cth
          y = ( r1 + u ) * sth
          z = v
          quad = quad + weight(k) * w * ( r1 + u ) * func ( x, y, z )

        end do
      end do
    end do
  end do

  volume = torus_volume_3d ( r1, r2 )
  result = quad * volume

  return
end
function torus_area_3d ( r1, r2 )
!
!*******************************************************************************
!
!! TORUS_AREA_3D returns the area of a torus in 3D.
!
!
!  Integration region:
!
!    Points (X,Y,Z) such that:
!
!      ( SQRT ( X**2 + Y**2 ) - R1 )**2 + Z**2 = R2**2.
!
!  Modified:
!
!    07 November 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, double precision R1, R2, the two radii that define the torus.
!
!    Output, double precision TORUS_AREA_3D, the area of the torus.
!
  double precision pi
  double precision r1
  double precision r2
  double precision torus_area_3d
!
  torus_area_3d = 4.0D+00 * pi()**2 * r1 * r2

  return
end
subroutine torus_square_14c ( func, r1, r2, result )
!
!*******************************************************************************
!
!! TORUS_SQUARE_14C approximates an integral in a "square" torus in 3D.
!
!
!  Discussion:
!
!    A 14-th degree 960 point formula is used.
!
!  Integration region:
!
!    Points (X,Y,Z) such that:
!
!      R1 - R2 <= SQRT ( X**2 + Y**2 ) <= R1 + R2,
!       -R2 <= Z <= R2.
!
!  Reference:
!
!    Arthur H Stroud,
!    Approximate Calculation of Multiple Integrals,
!    Prentice Hall, 1971.
!
!  Modified:
!
!    08 November 2000
!
!  Parameters:
!
!    Input, external FUNC, the name of the user supplied
!    function of three variables which is to be integrated, of the form:
!
!      function func ( x, y, z )
!      double precision func
!      double precision x
!      double precision y
!      double precision z
!
!    Input, double precision R1, R2, the radii that define the torus.
!
!    Output, double precision RESULT, the approximate integral of the function.
!
  integer, parameter :: norder = 8
!
  double precision angle
  double precision cth
  double precision func
  integer i
  integer j
  integer n
  double precision pi
  double precision quad
  double precision r1
  double precision r2
  double precision result
  double precision rtab(norder)
  double precision sth
  double precision torus_square_volume_3d
  double precision u
  double precision volume
  double precision w
  double precision weight(norder)
  double precision x
  double precision y
  double precision z
!
  external func
!
  call legendre_set ( norder, rtab, weight )

  w = 1.0D+00 / ( 60.0D+00 * r1 )
  quad = 0.0D+00

  do n = 1, 15

    angle = 2.0D+00 * pi ( ) * dble ( n ) / 15.0D+00
    cth = cos ( angle )
    sth = sin ( angle )

    do i = 1, norder

      u = r1 + rtab(i) * r2
      x = u * cth
      y = u * sth

      do j = 1, norder
        z = rtab(j) * r2
        quad = quad + u * w * weight(i) * weight(j) * func ( x, y, z )
      end do

    end do

  end do

  volume = torus_square_volume_3d ( r1, r2 )
  result = quad * volume

  return
end
subroutine torus_square_5c2 ( func, r1, r2, result )
!
!*******************************************************************************
!
!! TORUS_SQUARE_5C2 approximates an integral in a "square" torus in 3D.
!
!
!  Discussion:
!
!    A 24 point 5-th degree formula is used, Stroud number TOR3-C2:5-1.
!
!  Integration region:
!
!    Points (X,Y,Z) such that:
!
!      R1 - R2 <= SQRT ( X**2 + Y**2 ) <= R1 + R2,
!      -R2 <= Z <= R2.
!
!  Reference:
!
!    Arthur H Stroud,
!    Approximate Calculation of Multiple Integrals,
!    Prentice Hall, 1971.
!
!  Modified:
!
!    08 November 2000
!
!  Parameters:
!
!    Input, external FUNC, the name of the user supplied
!    function of three variables which is to be integrated,
!    of the form:
!
!      function func ( x, y, z )
!      double precision func
!      double precision x
!      double precision y
!      double precision z
!
!    Input, double precision R1, the primary radius of the torus.
!
!    Input, double precision R2, one-half the length of a side of the
!    square cross-section.
!
!    Output, double precision RESULT, the approximate integral of the function.
!
  double precision, parameter :: b1 = 5.0D+00 / 108.0D+00
  double precision, parameter :: b2 = 4.0D+00 / 108.0D+00
  double precision cs
  double precision func
  integer i
  double precision pi
  double precision quad
  double precision r1
  double precision r2
  double precision result
  double precision sn
  double precision torus_square_volume_3d
  double precision u1
  double precision u2
  double precision u3
  double precision v
  double precision volume
  double precision x
  double precision y
  double precision z
!
  external func
!
  quad = 0.0D+00

  u1 = sqrt ( r1**2 + r2**2 )

  v = r2 * sqrt ( 0.6D+00 )

  u2 = sqrt ( r1**2 - sqrt ( 3.0D+00 ) * r1 * r2 + r2**2 )

  u3 = sqrt ( r1**2 + sqrt ( 3.0D+00 ) * r1 * r2 + r2**2 )

  do i = 1, 6

    cs = cos ( dble ( i ) * pi ( ) / 3.0D+00 )
    sn = sin ( dble ( i ) * pi ( ) / 3.0D+00 )

    x = u1 * cs
    y = u1 * sn
    z = v
    quad = quad + b1 * func ( x, y, z )

    z = -v
    quad = quad + b1 * func ( x, y, z )

    x = u2 * cs
    y = u2 * sn
    z = 0.0D+00
    quad = quad + b2 * func ( x, y, z )

    x = u3 * cs
    y = u3 * sn
    z = 0.0D+00
    quad = quad + b2 * func ( x, y, z )

  end do

  volume = torus_square_volume_3d ( r1, r2 )
  result = quad * volume

  return
end
function torus_square_area_3d ( r1, r2 )
!
!*******************************************************************************
!
!! TORUS_SQUARE_AREA_3D returns the area of a square torus in 3D.
!
!
!  Integration region:
!
!    Points (X,Y,Z) such that:
!
!      R1 - R2 <= SQRT ( X**2 + Y**2 ) <= R1 + R2,
!      -R2 <= Z <= R2.
!
!  Modified:
!
!    07 November 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, double precision R1, R2, the two radii that define the torus.
!
!    Output, double precision TORUS_SQUARE_AREA_3D, the area of the torus.
!
  double precision pi
  double precision r1
  double precision r2
  double precision torus_square_area_3d
!
  torus_square_area_3d = 16.0D+00 * pi ( ) * r1 * r2

  return
end
function torus_square_volume_3d ( r1, r2 )
!
!*******************************************************************************
!
!! TORUS_SQUARE_VOLUME_3D returns the volume of a square torus in 3D.
!
!
!  Integration region:
!
!    Points (X,Y,Z) such that:
!
!      R1 - R2 <= SQRT ( X**2 + Y**2 ) <= R1 + R2,
!      -R2 <= Z <= R2.
!
!  Modified:
!
!    07 November 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, double precision R1, R2, the two radii that define the torus.
!
!    Output, double precision TORUS_SQUARE_VOLUME_3D, the volume of the torus.
!
  double precision pi
  double precision r1
  double precision r2
  double precision torus_square_volume_3d
!
  torus_square_volume_3d = 8.0D+00 * pi ( ) * r1 * r2**2

  return
end
function torus_volume_3d ( r1, r2 )
!
!*******************************************************************************
!
!! TORUS_VOLUME_3D returns the volume of a torus in 3D.
!
!
!  Integration region:
!
!    Points (X,Y,Z) such that:
!
!      ( SQRT ( X**2 + Y**2 ) - R1 )**2 + Z**2 = R2**2.
!
!  Modified:
!
!    07 November 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, double precision R1, R2, the two radii that define the torus.
!
!    Output, double precision TORUS_VOLUME_3D, the volume of the torus.
!
  double precision pi
  double precision r1
  double precision r2
  double precision torus_volume_3d
!
  torus_volume_3d = 2.0D+00 * pi()**2 * r1 * r2**2

  return
end
subroutine triangle_rule_adjust ( xval, yval, norder, xtab, ytab, weight, &
  xtab2, ytab2, weight2 )
!
!*******************************************************************************
!
!! TRIANGLE_RULE_ADJUST adjusts a unit quadrature rule to an arbitrary triangle.
!
!
!  Discussion:
!
!    This routine accepts as input abscissas and weights appropriate for
!    quadrature in a unit triangle, and returns abscissas and weights
!    appropriate for quadrature in a given triangle.
!
!    Once this routine has been called, an integral over the given triangle
!    can be approximated as:
!
!      QUAD = Sum ( 1 <= I <= NORDER ) WTAB2(I) * FUNC ( XTAB2(I), YTAB2(I) )
!
!  Integration region:
!
!    Points (X,Y) such that:
!
!      (X,Y) = ALPHA * (X1,Y1) + BETA * (X2,Y2) + ( 1 - ALPHA - BETA ) * (X3,Y3)
!      0 <= ALPHA <= 1 - BETA
!      0 <= BETA <= 1 - ALPHA
!
!  Modified:
!
!    21 January 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, double precision XVAL(3), YVAL(3), the coordinates of the nodes.
!
!    Input, integer NORDER, the order of the rule.
!
!    Input, double precision XTAB(NORDER), YTAB(NORDER), the abscissas for
!    a unit triangle.
!
!    Input, double precision WEIGHT(NORDER), the weights for a unit triangle.
!
!    Output, double precision XTAB2(NORDER), YTAB2(NORDER), the adjusted abscissas.
!
!    Output, double precision WEIGHT2(NORDER), the adjusted weights.
!
  integer norder
!
  integer i
  double precision triangle_volume
  double precision volume
  double precision weight(norder)
  double precision weight2(norder)
  double precision xtab(norder)
  double precision xtab2(norder)
  double precision xval(3)
  double precision ytab(norder)
  double precision ytab2(norder)
  double precision yval(3)
!
  volume = triangle_volume ( xval, yval )

  do i = 1, norder

    xtab2(i) = xtab(i) * xval(1) + ytab(i) * xval(2) &
      + ( 1.0D+00 - xtab(i) - ytab(i) ) * xval(3)

    ytab2(i) = xtab(i) * yval(1) + ytab(i) * yval(2) &
      + ( 1.0D+00 - xtab(i) - ytab(i) ) * yval(3)

    weight2(i) = weight(i) * 2.0D+00 * volume

  end do

  return
end
subroutine triangle_sub ( func, xval, yval, nsub, norder, xtab, ytab, weight, &
  result )
!
!*******************************************************************************
!
!! TRIANGLE_SUB carries out quadrature over subdivisions of a triangular region.
!
!
!  Integration region:
!
!    Points (X,Y) such that:
!
!      (X,Y) =
!          ALPHA                * ( XVAL(1), YVAL(1) )
!        + BETA                 * ( XVAL(2), YVAL(2) )
!        + ( 1 - ALPHA - BETA ) * ( XVAL(3), YVAL(3) )
!      0 <= ALPHA <= 1 - BETA
!      0 <= BETA <= 1 - ALPHA
!
!  Modified:
!
!    12 September 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, external FUNC, the name of the user supplied function of
!    two variables which is to be integrated, of the form:
!
!      function func ( x, y )
!      double precision func
!      double precision x
!      double precision y
!
!    Input, XVAL(3), YVAL(3), the coordinates of the triangle vertices.
!
!    Input, integer NSUB, the number of subdivisions of each side of the
!    input triangle to be made.  NSUB = 1 means no subdivisions are made.
!    NSUB = 3 means that each side of the triangle is subdivided into
!    three portions, and that the original triangle is subdivided into
!    NSUB**2 triangles.  NSUB must be at least 1.
!
!    Input, integer NORDER, the order of the rule.
!
!    Input, double precision XTAB(NORDER), YTAB(NORDER), the abscissas.
!
!    Input, double precision WEIGHT(NORDER), the weights of the rule.
!
!    Output, double precision RESULT, the approximate integral of the function.
!
  integer norder
!
  double precision area
  double precision func
  integer i
  integer j
  integer k
  integer nsub
  double precision quad
  double precision result
  double precision temp1
  double precision temp2
  double precision triangle_volume
  double precision volume
  double precision weight(norder)
  double precision x
  double precision x1
  double precision x2
  double precision x3
  double precision xtab(norder)
  double precision xval(3)
  double precision y
  double precision y1
  double precision y2
  double precision y3
  double precision ytab(norder)
  double precision yval(3)
!
  external func
!
!  Initialize RESULT, the approximate integral.
!
  result = 0.0D+00
!
!  NSUB must be positive.
!
  if ( nsub <= 0 ) then
    return
  end if
!
!  Initialize QUAD, the quadrature sum.
!
  quad = 0.0D+00
!
!  The sub-triangles can be grouped into NSUB strips.
!
  do i = 1, nsub

    temp1 = 0.0D+00
    temp2 = dble ( i ) / dble ( nsub )

    x2 = xval(2) + temp1 * ( xval(3) - xval(2) ) &
                 + temp2 * ( xval(1) - xval(2) )

    y2 = yval(2) + temp1 * ( yval(3) - yval(2) ) &
                 + temp2 * ( yval(1) - yval(2) )

    temp1 = 0.0D+00
    temp2 = dble ( i - 1 ) / dble ( nsub )

    x3 = xval(2) + temp1 * ( xval(3) - xval(2) ) &
                 + temp2 * ( xval(1) - xval(2) )

    y3 = yval(2) + temp1 * ( yval(3) - yval(2) ) &
                 + temp2 * ( yval(1) - yval(2) )
!
!  There are 2*I-1 triangles in strip number I.
!  The next triangle in the strip shares two nodes with the previous one.
!  Compute its corners, (X1,Y1), (X2,Y2), (X3,Y3).
!
    do j = 1, 2*i-1

      x1 = x2
      y1 = y2
      x2 = x3
      y2 = y3
      temp1 = dble ( ( j + 1 ) / 2 ) / dble ( nsub )
      temp2 = dble ( i - 1 - ( j / 2 ) ) / dble ( nsub )

      x3 = xval(2) + temp1 * ( xval(3) - xval(2) ) &
                   + temp2 * ( xval(1) - xval(2) )

      y3 = yval(2) + temp1 * ( yval(3) - yval(2) ) &
                   + temp2 * ( yval(1) - yval(2) )
!
!  Now integrate over the triangle, mapping the points ( XTAB(K), YTAB(K) )
!  into the triangle.
!
      do k = 1, norder

        x = x2 + xtab(k) * ( x3 - x2 ) + ytab(k) * ( x1 - x2 )
        y = y2 + xtab(k) * ( y3 - y2 ) + ytab(k) * ( y1 - y2 )
        quad = quad + weight(k) * func ( x, y )

       end do

    end do

  end do

  volume = triangle_volume ( xval, yval ) / dble ( nsub**2 )
  result = quad * volume

  return
end
subroutine triangle_sum ( func, xval, yval, norder, xtab, ytab, weight, result )
!
!*******************************************************************************
!
!! TRIANGLE_SUM carries out a unit quadrature rule in an arbitrary triangle.
!
!
!  Integration region:
!
!    Points (X,Y) such that:
!
!      (X,Y) = ALPHA * (X1,Y1) + BETA * (X2,Y2) + ( 1 - ALPHA - BETA ) * (X3,Y3)
!      0 <= ALPHA <= 1 - BETA
!      0 <= BETA <= 1 - ALPHA
!
!  Modified:
!
!    25 August 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, external FUNC, the name of the user supplied function of
!    two variables which is to be integrated, of the form:
!
!      function func ( x, y )
!      double precision func
!      double precision x
!      double precision y
!
!    Input, double precision XVAL(3), YVAL(3), the coordinates of the nodes.
!
!    Input, integer NORDER, the order of the rule.
!
!    Input, double precision XTAB(NORDER), YTAB(NORDER), the abscissas.
!
!    Input, double precision WEIGHT(NORDER), the weights of the rule.
!
!    Output, double precision RESULT, the approximate integral of the function.
!
  integer norder
!
  double precision func
  integer i
  double precision quad
  double precision result
  double precision triangle_volume
  double precision volume
  double precision weight(norder)
  double precision x
  double precision xtab(norder)
  double precision xval(3)
  double precision y
  double precision ytab(norder)
  double precision yval(3)
!
  external func
!
  quad = 0.0D+00

  do i = 1, norder

    x = xtab(i) * xval(1) + ytab(i) * xval(2) &
      + ( 1.0D+00 - xtab(i) - ytab(i) ) * xval(3)

    y = xtab(i) * yval(1) + ytab(i) * yval(2) &
      + ( 1.0D+00 - xtab(i) - ytab(i) ) * yval(3)

    quad = quad + weight(i) * func ( x, y )

  end do

  volume = triangle_volume ( xval, yval )
  result = quad * volume

  return
end
subroutine triangle_sum_adjusted ( func, norder, xtab, ytab, weight, result )
!
!*******************************************************************************
!
!! TRIANGLE_SUM_ADJUSTED carries out an adjusted quadrature rule in a triangle.
!
!
!  Discussion:
!
!    It is assumed that a quadrature rule approprate for a unit triangle
!    was generated, and then adjusted to a particular triangle by calling
!    TRIANGLE_RULE_ADJUST.
!
!  Integration region:
!
!    Points (X,Y) such that:
!
!      (X,Y) = ALPHA * (X1,Y1) + BETA * (X2,Y2) + ( 1 - ALPHA - BETA ) * (X3,Y3)
!      0 <= ALPHA <= 1 - BETA
!      0 <= BETA <= 1 - ALPHA
!
!  Modified:
!
!    21 January 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, external FUNC, the name of the user supplied function of
!    two variables which is to be integrated, of the form:
!
!      function func ( x, y )
!      double precision func
!      double precision x
!      double precision y
!
!    Input, integer NORDER, the order of the rule.
!
!    Input, double precision XTAB(NORDER), YTAB(NORDER), the abscissas.
!
!    Input, double precision WEIGHT(NORDER), the weights of the rule.
!
!    Output, double precision RESULT, the approximate integral of the function.
!
  integer norder
!
  double precision func
  integer i
  double precision result
  double precision weight(norder)
  double precision xtab(norder)
  double precision ytab(norder)
!
  external func
!
  result = 0.0D+00

  do i = 1, norder
    result = result + weight(i) * func ( xtab(i), ytab(i) )
  end do

  return
end
subroutine triangle_unit_product_set ( rule, norder, xtab, ytab, weight )
!
!*******************************************************************************
!
!! TRIANGLE_UNIT_PRODUCT_SET sets a product quadrature rule on a unit triangle.
!
!
!  Integration region:
!
!    0 <= X <= 1 - Y,
!    0 <= Y <= 1 - X.
!
!  References:
!
!    Arthur H Stroud,
!    Approximate Calculation of Multiple Integrals,
!    Prentice Hall, 1971.
!
!  Modified:
!
!    21 January 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer RULE, the rule number, or the order of the 1D rule.
!
!    Output, integer NORDER, the order of the rule.  NORDER = RULE**2.
!
!    Output, double precision XTAB(NORDER), YTAB(NORDER), the abscissas.
!
!    Output, double precision WEIGHT(NORDER), the weights of the rule.
!
  integer rule
!
  double precision a
  double precision b
  double precision c
  double precision d
  integer i
  integer j
  integer k
  integer norder
  integer norder0
  integer norder1
  double precision weight(rule*rule)
  double precision weight0(rule)
  double precision weight1(rule)
  double precision xtab(rule*rule)
  double precision xtab0(rule)
  double precision xtab1(rule)
  double precision ytab(rule*rule)
  double precision ytab0(rule)
  double precision ytab1(rule)
!
  norder = rule * rule 

  a = -1.0D+00
  b = +1.0D+00
  c =  0.0D+00
  d = +1.0D+00

  norder0 = rule
  call legendre_set ( norder0, xtab0, weight0 )
  call rule_adjust ( a, b, c, d, norder0, xtab0, weight0 )

  norder1 = rule
  call legendre_set_x1 ( norder1, xtab1, weight1 )
  call rule_adjust ( a, b, c, d, norder1, xtab1, weight1 )

  k = 0
  do j = 1, norder1
    do i = 1, norder0
      k = k + 1
      xtab(k) = 1.0D+00 - xtab1(j)
      ytab(k) = xtab0(i) * xtab1(j)
      weight(k) = weight0(i) * weight1(j)
    end do
  end do

  return
end
subroutine triangle_unit_set ( rule, norder, xtab, ytab, weight )
!
!*******************************************************************************
!
!! TRIANGLE_UNIT_SET sets weights and abscissas for quadrature on a unit triangle.
!
!
!  Integration region:
!
!    0 <= X <= 1 - Y,
!    0 <= Y <= 1 - X.
!
!  References:
!
!    H R Schwarz,
!    Methode der Finiten Elemente,
!    Teubner Studienbuecher, 1980.
!
!    Strang and Fix,
!    An Analysis of the Finite Element Method,
!    Prentice Hall, 1973, page 184.
!
!    Arthur H Stroud,
!    Approximate Calculation of Multiple Integrals,
!    Prentice Hall, 1971.
!
!    O C Zienkiewicz,
!    The Finite Element Method,
!    McGraw Hill, Third Edition, 1977, page 201.
!
!  Modified:
!
!    10 December 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer RULE, the index of the rule.
!
!     1, NORDER =  1, precision 1, Zienkiewicz #1.
!     2, NORDER =  3, precision 2, Strang and Fix formula #1.
!     3, NORDER =  3, precision 2, Strang and Fix formula #2, Zienkiewicz #2.
!     4, NORDER =  4, precision 3, Strang and Fix formula #3, Zienkiewicz #3.
!     5, NORDER =  6, precision 3, Strang and Fix formula #4.
!     6, NORDER =  6, precision 3, Stroud formula T2:3-1.
!     7, NORDER =  6, precision 4, Strang and Fix formula #5.
!     8, NORDER =  7, precision 4, Strang and Fix formula #6.
!     9, NORDER =  7, precision 5, Strang and Fix formula #7,
!        Stroud formula T2:5-1, Zienkiewicz #4, Schwarz Table 2.2.
!    10, NORDER =  9, precision 6, Strang and Fix formula #8.
!    11, NORDER = 12, precision 6, Strang and Fix formula #9.
!    12, NORDER = 13, precision 7, Strang and Fix formula #10.
!    13, NORDER =  7, precision ?.
!    14, NORDER = 16, precision 7, conical product Gauss, Stroud formula T2:7-1.
!    15, NORDER = 64, precision 15, triangular product Gauss rule.
!    16, NORDER = 19, precision 8, from CUBTRI, ACM TOMS #584.
!    17, NORDER = 19, precision 9, from TRIEX, Lyness and Jespersen.
!    18, NORDER = 28, precision 11, from TRIEX, Lyness and Jespersen.
!    19, NORDER = 37, precision 13, from ACM TOMS #706.
!
!    Output, integer NORDER, the order of the rule.
!
!    Output, double precision XTAB(NORDER), YTAB(NORDER), the abscissas.
!
!    Output, double precision WEIGHT(NORDER), the weights of the rule.
!
  double precision a
  double precision b
  double precision c
  double precision d
  double precision e
  double precision f
  double precision g
  double precision h
  integer i
  integer rule
  integer j
  integer k
  integer norder
  integer norder2
  double precision p
  double precision q
  double precision r
  double precision s
  double precision t
  double precision u
  double precision v
  double precision w
  double precision w1
  double precision w2
  double precision w3
  double precision w4
  double precision w5
  double precision w6
  double precision w7
  double precision w8
  double precision w9
  double precision weight(*)
  double precision weight1(8)
  double precision weight2(8)
  double precision wx
  double precision x
  double precision xtab(*)
  double precision xtab1(8)
  double precision xtab2(8)
  double precision y
  double precision ytab(*)
  double precision z
!
!  1 point, precision 1.
!
  if ( rule == 1 ) then

    a = 1.0D+00 / 3.0D+00
    w = 1.0D+00

    norder = 1
    xtab(1) = a
    ytab(1) = a
    weight(1) = w
!
!  3 points, precision 2, Strang and Fix formula #1.
!
  else if ( rule == 2 ) then

    a = 1.0D+00
    b = 3.0D+00
    c = 4.0D+00
    d = 6.0D+00

    norder = 3
    xtab(1:3) =   (/ c, a, a /) / d
    ytab(1:3) =   (/ a, c, a /) / d
    weight(1:3) = (/ a, a, a /) / b
!
!  3 points, precision 2, Strang and Fix formula #2.
!
  else if ( rule == 3 ) then

    a = 0.5D+00
    b = 1.0D+00
    c = 1.0D+00 / 3.0D+00
    z = 0.0D+00

    norder = 3
    xtab(1:3) =   (/ z, a, a /)
    ytab(1:3) =   (/ a, z, a /)
    weight(1:3) = (/ c, c, c /)
!
!  4 points, precision 3, Strang and Fix formula #3.
!
  else if ( rule == 4 ) then

    a =   6.0D+00
    b =  10.0D+00
    c =  18.0D+00
    d =  25.0D+00
    e = -27.0D+00
    f =  30.0D+00
    g =  48.0D+00

    norder = 4
    xtab(1:4) =   (/ b, c, a, a /) / f
    ytab(1:4) =   (/ b, a, c, a /) / f
    weight(1:4) = (/ e, d, d, d /) / g
!
!  6 points, precision 3, Strang and Fix formula #4.
!
  else if ( rule == 5 ) then

    a = 0.659027622374092D+00
    b = 0.231933368553031D+00
    c = 0.109039009072877D+00
    w = 1.0D+00 / 6.0D+00

    norder = 6
    xtab(1:6) =   (/ a, a, b, b, c, c /)
    ytab(1:6) =   (/ b, c, a, c, a, b /)
    weight(1:6) = (/ w, w, w, w, w, w /)
!
!  6 points, precision 3, Stroud T2:3-1.
!
  else if ( rule == 6 ) then

    a = 0.0D+00
    b = 0.5D+00
    c = 2.0D+00 /  3.0D+00
    d = 1.0D+00 /  6.0D+00
    v = 1.0D+00 / 30.0D+00
    w = 3.0D+00 / 10.0D+00

    norder = 6
    xtab(1:6) =   (/ a, b, b, c, d, d /)
    ytab(1:6) =   (/ b, a, b, d, c, d /)
    weight(1:6) = (/ v, v, v, w, w, w /)
!
!  6 points, precision 4, Strang and Fix, formula #5.
!
  else if ( rule == 7 ) then

    a = 0.816847572980459D+00
    b = 0.091576213509771D+00
    c = 0.108103018168070D+00
    d = 0.445948490915965D+00
    v = 0.109951743655322D+00
    w = 0.223381589678011D+00

    norder = 6
    xtab(1:6) =   (/ a, b, b, c, d, d /)
    ytab(1:6) =   (/ b, a, b, d, c, d /)
    weight(1:6) = (/ v, v, v, w, w, w /)
!
!  7 points, precision 4, Strang and Fix formula #6.
!
  else if ( rule == 8 ) then

    a = 1.0D+00 / 3.0D+00
    c = 0.736712498968435D+00
    d = 0.237932366472434D+00
    e = 0.025355134551932D+00
    v = 0.375D+00
    w = 0.104166666666667D+00

    norder = 7
    xtab(1:7) =   (/ a, c, c, d, d, e, e /)
    ytab(1:7) =   (/ a, d, e, c, e, c, d /)
    weight(1:7) = (/ v, w, w, w, w, w, w /)
!
!  7 points, precision 5, Strang and Fix formula #7, Stroud T2:5-1
!
  else if ( rule == 9 ) then

    a = 1.0D+00 / 3.0D+00
    b = ( 9.0D+00 + 2.0D+00 * sqrt ( 15.0D+00 ) ) / 21.0D+00
    c = ( 6.0D+00 -           sqrt ( 15.0D+00 ) ) / 21.0D+00
    d = ( 9.0D+00 - 2.0D+00 * sqrt ( 15.0D+00 ) ) / 21.0D+00
    e = ( 6.0D+00 +           sqrt ( 15.0D+00 ) ) / 21.0D+00
    u = 0.225D+00
    v = ( 155.0D+00 - sqrt ( 15.0D+00 ) ) / 1200.0D+00
    w = ( 155.0D+00 + sqrt ( 15.0D+00 ) ) / 1200.0D+00

    norder = 7
    xtab(1:7) =   (/ a, b, c, c, d, e, e /)
    ytab(1:7) =   (/ a, c, b, c, e, d, e /)
    weight(1:7) = (/ u, v, v, v, w, w, w /)
!
!  9 points, precision 6, Strang and Fix formula #8.
!
  else if ( rule == 10 ) then

    a = 0.124949503233232D+00
    b = 0.437525248383384D+00
    c = 0.797112651860071D+00
    d = 0.165409927389841D+00
    e = 0.037477420750088D+00

    u = 0.205950504760887D+00
    v = 0.063691414286223D+00

    norder = 9
    xtab(1:9) =   (/ a, b, b, c, c, d, d, e, e /)
    ytab(1:9) =   (/ b, a, b, d, e, c, e, c, d /)
    weight(1:9) = (/ u, u, u, v, v, v, v, v, v /)
!
!  12 points, precision 6, Strang and Fix, formula #9.
!
  else if ( rule == 11 ) then

    a = 0.873821971016996D+00
    b = 0.063089014491502D+00
    c = 0.501426509658179D+00
    d = 0.249286745170910D+00
    e = 0.636502499121399D+00
    f = 0.310352451033785D+00
    g = 0.053145049844816D+00

    u = 0.050844906370207D+00
    v = 0.116786275726379D+00
    w = 0.082851075618374D+00

    norder = 12
    xtab(1:12) =   (/ a, b, b, d, c, d, e, e, f, f, g, g /)
    ytab(1:12) =   (/ b, a, b, c, d, d, f, g, e, g, e, f /)
    weight(1:12) = (/ u, u, u, v, v, v, w, w, w, w, w, w /)
!
!  13 points, precision 7, Strang and Fix, formula #10.
!
  else if ( rule == 12 ) then

    a = 0.479308067841923D+00
    b = 0.260345966079038D+00
    c = 0.869739794195568D+00
    d = 0.065130102902216D+00
    e = 0.638444188569809D+00
    f = 0.312865496004875D+00
    g = 0.048690315425316D+00
    h = 1.0D+00 / 3.0D+00
    t = 0.175615257433204D+00
    u = 0.053347235608839D+00
    v = 0.077113760890257D+00
    w = -0.149570044467670D+00

    norder = 13
    xtab(1:13) =   (/ a, b, b, c, d, d, e, e, f, f, g, g, h /)
    ytab(1:13) =   (/ b, a, b, d, c, d, f, g, e, g, e, f, h /)
    weight(1:13) = (/ t, t, t, u, u, u, v, v, v, v, v, v, w /)
!
!  7 points, precision ?.
!
  else if ( rule == 13 ) then

    a = 1.0D+00 / 3.0D+00
    b = 1.0D+00
    c = 0.5D+00
    z = 0.0D+00

    u = 27.0D+00 / 60.0D+00
    v =  3.0D+00 / 60.0D+00
    w =  8.0D+00 / 60.0D+00

    norder = 7
    xtab(1:7) =   (/ a, b, z, z, z, c, c /)
    ytab(1:7) =   (/ a, z, b, z, c, z, c /)
    weight(1:7) = (/ u, v, v, v, w, w, w /)
!
!  16 points.
!
  else if ( rule == 14 ) then

    norder = 16

    norder2 = 4

    call legendre_set ( norder2, xtab1, weight1 )

    xtab1(1:norder2) = 0.5D+00 * ( xtab1(1:norder2) + 1.0D+00 )

    weight2(1) = 0.1355069134D+00
    weight2(2) = 0.2034645680D+00
    weight2(3) = 0.1298475476D+00
    weight2(4) = 0.0311809709D+00

    xtab2(1) = 0.0571041961D+00
    xtab2(2) = 0.2768430136D+00
    xtab2(3) = 0.5835904324D+00
    xtab2(4) = 0.8602401357D+00

    k = 0
    do i = 1, norder2
      do j = 1, norder2
        k = k + 1
        xtab(k) = xtab2(j)
        ytab(k) = xtab1(i) * ( 1.0D+00 - xtab2(j) )
        weight(k) = weight1(i) * weight2(j)
      end do
    end do
!
!  64 points, precision 15.
!
  else if ( rule == 15 ) then

    norder = 64

    weight2(1) = 0.00329519144D+00
    weight2(2) = 0.01784290266D+00
    weight2(3) = 0.04543931950D+00
    weight2(4) = 0.07919959949D+00
    weight2(5) = 0.10604735944D+00
    weight2(6) = 0.11250579947D+00
    weight2(7) = 0.09111902364D+00
    weight2(8) = 0.04455080436D+00

    xtab2(1) = 0.04463395529D+00
    xtab2(2) = 0.14436625704D+00
    xtab2(3) = 0.28682475714D+00
    xtab2(4) = 0.45481331520D+00
    xtab2(5) = 0.62806783542D+00
    xtab2(6) = 0.78569152060D+00
    xtab2(7) = 0.90867639210D+00
    xtab2(8) = 0.98222008485D+00

    norder2 = 8
    call legendre_set ( norder2, xtab1, weight1 )

    k = 0
    do j = 1, norder2
      do i = 1, norder2
        k = k + 1
        xtab(k) = 1.0D+00 - xtab2(j)
        ytab(k) = 0.5D+00 * ( 1.0D+00 + xtab1(i) ) * xtab2(j)
        weight(k) = weight1(i) * weight2(j)
      end do
    end do
!
!  19 points, precision 8.
!
  else if ( rule == 16 ) then

    a = 1.0D+00 / 3.0D+00
    b = ( 9.0D+00 + 2.0D+00 * sqrt ( 15.0D+00 ) ) / 21.0D+00
    c = ( 6.0D+00 -       sqrt ( 15.0D+00 ) ) / 21.0D+00
    d = ( 9.0D+00 - 2.0D+00 * sqrt ( 15.0D+00 ) ) / 21.0D+00
    e = ( 6.0D+00 +       sqrt ( 15.0D+00 ) ) / 21.0D+00
    f = ( 40.0D+00 - 10.0D+00 * sqrt ( 15.0D+00 ) &
      + 10.0D+00 * sqrt ( 7.0D+00 ) + 2.0D+00 * sqrt ( 105.0D+00 ) ) / 90.0D+00
    g = ( 25.0D+00 +  5.0D+00 * sqrt ( 15.0D+00 ) &
      -  5.0D+00 * sqrt ( 7.0D+00 ) - sqrt ( 105.0D+00 ) ) / 90.0D+00
    p = ( 40.0D+00 + 10.0D+00 * sqrt ( 15.0D+00 ) &
      + 10.0D+00 * sqrt ( 7.0D+00 ) - 2.0D+00 * sqrt ( 105.0D+00 ) ) / 90.0D+00
    q = ( 25.0D+00 -  5.0D+00 * sqrt ( 15.0D+00 ) &
      -  5.0D+00 * sqrt ( 7.0D+00 ) + sqrt ( 105.0D+00 ) ) / 90.0D+00
    r = ( 40.0D+00 + 10.0D+00 * sqrt ( 7.0D+00 ) ) / 90.0D+00
    s = ( 25.0D+00 +  5.0D+00 * sqrt ( 15.0D+00 ) - 5.0D+00 * sqrt ( 7.0D+00 ) &
      - sqrt ( 105.0D+00 ) ) / 90.0D+00
    t = ( 25.0D+00 -  5.0D+00 * sqrt ( 15.0D+00 ) - 5.0D+00 * sqrt ( 7.0D+00 ) &
      + sqrt ( 105.0D+00 ) ) / 90.0D+00

    w1 = ( 7137.0D+00 - 1800.0D+00 * sqrt ( 7.0D+00 ) ) / 62720.0D+00
    w2 = - 9301697.0D+00 / 4695040.0D+00 - 13517313.0D+00 * sqrt ( 15.0D+00 ) &
      / 23475200.0D+00 + 764885.0D+00 * sqrt ( 7.0D+00 ) / 939008.0D+00 &
      + 198763.0D+00 * sqrt ( 105.0D+00 ) / 939008.0D+00
    w2 = w2 / 3.0D+00
    w3 = -9301697.0D+00 / 4695040.0D+00 + 13517313.0D+00 * sqrt ( 15.0D+00 ) &
      / 23475200.0D+00 &
      + 764885.0D+00 * sqrt ( 7.0D+00 ) / 939008.0D+00 &
      - 198763.0D+00 * sqrt ( 105.0D+00 ) / 939008.0D+00
    w3 = w3 / 3.0D+00
    w4 = ( 102791225.0D+00 - 23876225.0D+00 * sqrt ( 15.0D+00 ) &
      - 34500875.0D+00 * sqrt ( 7.0D+00 ) &
      + 9914825.0D+00 * sqrt ( 105.0D+00 ) ) / 59157504.0D+00
    w4 = w4 / 3.0D+00
    w5 = ( 102791225.0D+00 + 23876225.0D+00 * sqrt ( 15.0D+00 ) &
      - 34500875.0D+00 * sqrt ( 7.0D+00 ) &
      - 9914825D+00 * sqrt ( 105.0D+00 ) ) / 59157504.0D+00
    w5 = w5 / 3.0D+00
    w6 = ( 11075.0D+00 - 3500.0D+00 * sqrt ( 7.0D+00 ) ) / 8064.0D+00
    w6 = w6 / 6.0D+00

    norder = 19
    xtab(1:19) =   (/  a,  b,  c,  c,  d,  e,  e,  f,  g,  g,  p,  q,  q, &
                       r,  r,  s,  s,  t,  t /)
    ytab(1:19) =   (/  a,  c,  b,  c,  e,  d,  e,  g,  f,  g,  q,  p,  q, &
                       s,  t,  r,  t,  r,  s /)
    weight(1:19) = (/ w1, w2, w2, w2, w3, w3, w3, w4, w4, w4, w5, w5, w5, &
      w6, w6, w6, w6, w6, w6 /)
!
!  19 points, precision 9.
!
  else if ( rule == 17 ) then

    norder = 19

    a = 1.0D+00 / 3.0D+00
    b = 0.02063496160252593D+00
    c = 0.4896825191987370D+00
    d = 0.1258208170141290D+00
    e = 0.4370895914929355D+00
    f = 0.6235929287619356D+00
    g = 0.1882035356190322D+00
    r = 0.9105409732110941D+00
    s = 0.04472951339445297D+00
    t = 0.7411985987844980D+00
    u = 0.03683841205473626D+00
    v = 0.22196288916076574D+00

    w1 = 0.09713579628279610D+00
    w2 = 0.03133470022713983D+00
    w3 = 0.07782754100477543D+00
    w4 = 0.07964773892720910D+00
    w5 = 0.02557767565869810D+00
    w6 = 0.04328353937728940D+00

    xtab(1:19) =   (/  a,  b,  c,  c,  d,  e,  e,  f,  g,  g,  r,  s,  s, &
      t, t, u, u, v, v /)
    ytab(1:19) =   (/  a,  c,  b,  c,  e,  d,  e,  g,  f,  g,  s,  r,  s, &
      u, v, t, v, t, u /)
    weight(1:19) = (/ w1, w2, w2, w2, w3, w3, w3, w4, w4, w4, w5, w5, w5, &
      w6, w6, w6, w6, w6, w6 /)
!
!  28 points, precision 11.
!
  else if ( rule == 18 ) then

    a = 1.0D+00 / 3.0D+00
    b = 0.9480217181434233D+00
    c = 0.02598914092828833D+00
    d = 0.8114249947041546D+00
    e = 0.09428750264792270D+00
    f = 0.01072644996557060D+00
    g = 0.4946367750172147D+00
    p = 0.5853132347709715D+00
    q = 0.2073433826145142D+00
    r = 0.1221843885990187D+00
    s = 0.4389078057004907D+00
    t = 0.6779376548825902D+00
    u = 0.04484167758913055D+00
    v = 0.27722066752827925D+00
    w = 0.8588702812826364D+00
    x = 0.0D+00
    y = 0.1411297187173636D+00

    w1 = 0.08797730116222190D+00
    w2 = 0.008744311553736190D+00
    w3 = 0.03808157199393533D+00
    w4 = 0.01885544805613125D+00
    w5 = 0.07215969754474100D+00
    w6 = 0.06932913870553720D+00
    w7 = 0.04105631542928860D+00
    w8 = 0.007362383783300573D+00

    norder = 28
    xtab(1:28) =   (/  a,  b,  c,  c,  d,  e,  e,  f,  g,  g,  p,  q,  q, &
       r,  s,  s,  t,  t,  u,  u,  v,  v,  w,  w,  x,  x,  y,  y /)
    ytab(1:28) =   (/  a,  c,  b,  c,  e,  d,  e,  g,  f,  g,  q,  p,  q, &
       s,  r,  s,  u,  v,  t,  v,  t,  u,  x,  y,  w,  y,  w,  x /)
    weight(1:28) = (/ w1, w2, w2, w2, w3, w3, w3, w4, w4, w4, w5, w5, w5, &
      w6, w6, w6, w7, w7, w7, w7, w7, w7, w8, w8, w8, w8, w8, w8 /)
!
!  37 points, precision 13.
!
  else if ( rule == 19 ) then

    a = 1.0D+00 / 3.0D+00
    b = 0.950275662924105565450352089520D+00
    c = 0.024862168537947217274823955239D+00
    d = 0.171614914923835347556304795551D+00
    e = 0.414192542538082326221847602214D+00
    f = 0.539412243677190440263092985511D+00
    g = 0.230293878161404779868453507244D+00

    w1 = 0.051739766065744133555179145422D+00
    w2 = 0.008007799555564801597804123460D+00
    w3 = 0.046868898981821644823226732071D+00
    w4 = 0.046590940183976487960361770070D+00
    w5 = 0.031016943313796381407646220131D+00
    w6 = 0.010791612736631273623178240136D+00
    w7 = 0.032195534242431618819414482205D+00
    w8 = 0.015445834210701583817692900053D+00
    w9 = 0.017822989923178661888748319485D+00
    wx = 0.037038683681384627918546472190D+00

    norder = 37
    xtab(1:10) =   (/ a, b, c, c, d, e, e, f, g, g /)
    ytab(1:10) =   (/ a, c, b, c, e, d, e, g, f, g /)
    weight(1:37) = (/ w1, w2, w2, w2, w3, w3, w3, w4, w4, w4, w5, w5, w5, &
                      w6, w6, w6, w7, w7, w7, w8, w8, w8, w8, w8, w8, w9, &
                      w9, w9, w9, w9, w9, wx, wx, wx, wx, wx, wx /)

    a = 0.772160036676532561750285570113D+00
    b = 0.113919981661733719124857214943D+00

    xtab(11) = a
    ytab(11) = b

    xtab(12) = b
    ytab(12) = a

    xtab(13) = b
    ytab(13) = b

    a = 0.009085399949835353883572964740D+00
    b = 0.495457300025082323058213517632D+00

    xtab(14) = a
    ytab(14) = b

    xtab(15) = b
    ytab(15) = a

    xtab(16) = b
    ytab(16) = b

    a = 0.062277290305886993497083640527D+00
    b = 0.468861354847056503251458179727D+00

    xtab(17) = a
    ytab(17) = b

    xtab(18) = b
    ytab(18) = a

    xtab(19) = b
    ytab(19) = b

    a = 0.022076289653624405142446876931D+00
    b = 0.851306504174348550389457672223D+00
    c = 1.0D+00 - a - b

    xtab(20) = a
    ytab(20) = b

    xtab(21) = a
    ytab(21) = c

    xtab(22) = b
    ytab(22) = a

    xtab(23) = b
    ytab(23) = c

    xtab(24) = c
    ytab(24) = a

    xtab(25) = c
    ytab(25) = b

    a = 0.018620522802520968955913511549D+00
    b = 0.689441970728591295496647976487D+00
    c = 1.0D+00 - a - b

    xtab(26) = a
    ytab(26) = b

    xtab(27) = a
    ytab(27) = c

    xtab(28) = b
    ytab(28) = a

    xtab(29) = b
    ytab(29) = c

    xtab(30) = c
    ytab(30) = a

    xtab(31) = c
    ytab(31) = b

    a = 0.096506481292159228736516560903D+00
    b = 0.635867859433872768286976979827D+00
    c = 1.0D+00 - a - b

    xtab(32) = a
    ytab(32) = b

    xtab(33) = a
    ytab(33) = c

    xtab(34) = b
    ytab(34) = a

    xtab(35) = b
    ytab(35) = c

    xtab(36) = c
    ytab(36) = a

    xtab(37) = c
    ytab(37) = b

  else

    write ( *, * ) ' '
    write ( *, * ) 'TRIANGLE_UNIT_SET - Fatal error!'
    write ( *, * ) '  Illegal value of RULE = ', rule
    stop

  end if

  return
end
subroutine triangle_unit_sum ( func, norder, xtab, ytab, weight, result )
!
!*******************************************************************************
!
!! TRIANGLE_UNIT_SUM carries out a quadrature rule in the unit triangle.
!
!
!  Integration region:
!
!    Points (X,Y) such that:
!
!      0 <= X <= 1 - Y,
!      0 <= Y <= 1 - X.
!
!  Modified:
!
!    25 August 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, external FUNC, the name of the user supplied
!    function of two variables which is to be integrated,
!    of the form:
!
!      function func ( x, y )
!      double precision func
!      double precision x
!      double precision y
!
!    Input, integer NORDER, the order of the rule.
!
!    Input, double precision XTAB(NORDER), YTAB(NORDER), the abscissas.
!
!    Input, double precision WEIGHT(NORDER), the weights of the rule.
!
!    Output, double precision RESULT, the approximate integral of the function.
!
  integer norder
!
  double precision func
  integer i
  double precision quad
  double precision result
  double precision triangle_unit_volume
  double precision volume
  double precision weight(norder)
  double precision xtab(norder)
  double precision ytab(norder)
!
  external func
!
  quad = 0.0D+00

  do i = 1, norder
    quad = quad + weight(i) * func ( xtab(i), ytab(i) )
  end do

  volume = triangle_unit_volume ( )
  result = quad * volume

  return
end
function triangle_unit_volume ( )
!
!*******************************************************************************
!
!! TRIANGLE_UNIT_VOLUME returns the "volume" of the unit triangle in 2D.
!
!
!  Discussion:
!
!    The "volume" of a triangle is usually called its area.
!
!  Modified:
!
!    27 October 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, double precision TRIANGLE_UNIT_VOLUME, the volume of the unit
!    triangle.
!
  double precision triangle_unit_volume
!
  triangle_unit_volume = 1.0D+00 / 2.0D+00

  return
end
function triangle_volume ( x, y )
!
!*******************************************************************************
!
!! TRIANGLE_VOLUME returns the "volume" of a triangle in 2D.
!
!
!  Modified:
!
!    19 November 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, double precision X(3), Y(3), the vertices of the triangle.
!
!    Output, double precision TRIANGLE_VOLUME, the volume of the triangle.
!
  double precision triangle_volume
  double precision x(3)
  double precision y(3)
!
  triangle_volume = 0.5D+00 * abs ( &
    x(1) * ( y(2) - y(3) ) + &
    x(2) * ( y(3) - y(1) ) + &
    x(3) * ( y(1) - y(2) ) )

  return
end
subroutine tvec_even ( nt, t )
!
!*******************************************************************************
!
!! TVEC_EVEN computes an evenly spaced set of angles between 0 and 2*PI.
!
!
!  Discussion:
!
!    The computation realizes that 0 = 2 * PI, and does not include that value.
!
!  Example:
!
!    NT = 4
!
!    T = ( 0, PI/2, PI, 3*PI/2 )
!
!  Modified:
!
!    14 December 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer NT, the number of values to compute.
!
!    Output, double precision TVEC(NT), the evenly spaced angles, in radians.
!
  integer nt
!
  integer i
  double precision, parameter :: pi = &
    3.14159265358979323846264338327950288419716939937510D+00
  double precision t(nt)
!
  do i = 1, nt
    t(i) = dble ( 2 * ( i - 1 ) ) * pi / dble ( nt )
  end do

  return
end
subroutine tvec_even2 ( nt, t )
!
!*******************************************************************************
!
!! TVEC_EVEN2 computes an evenly spaced set of angles between 0 and 2*PI.
!
!
!  Discussion:
!
!    The computation realizes that 0 = 2 * PI.  The values are equally
!    spaced in the circle, do not include 0, and are symmetric about 0.
!
!  Example:
!
!    NT = 4
!
!    T = ( PI/4, 3*PI/4, 5*PI/4, 7*PI/4 )
!
!  Modified:
!
!    15 December 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer NT, the number of values to compute.
!
!    Output, double precision TVEC(NT), the evenly spaced angles, in radians.
!
  integer nt
!
  integer i
  double precision, parameter :: pi = &
    3.14159265358979323846264338327950288419716939937510D+00
  double precision t(nt)
!
  do i = 1, nt
    t(i) = dble ( 2 * i - 1 ) * pi / dble ( nt )
  end do

  return
end
subroutine tvec_even3 ( nt, t )
!
!*******************************************************************************
!
!! TVEC_EVEN3 computes an evenly spaced set of angles between 0 and 2*PI.
!
!
!  Discussion:
!
!    The angles begin with 0 and end with 2*PI.
!
!  Example:
!
!    NT = 4
!
!    T = ( 0, 2*PI/3, 4*PI/3 2*PI )
!
!  Modified:
!
!    13 January 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer NT, the number of values to compute.
!
!    Output, double precision TVEC(NT), the evenly spaced angles, in radians.
!
  integer nt
!
  integer i
  double precision, parameter :: pi = &
    3.14159265358979323846264338327950288419716939937510D+00
  double precision t(nt)
!
  if ( nt == 1 ) then
    t(1) = pi
  else
    do i = 1, nt
      t(i) = dble ( 2 * ( i - 1 ) ) * pi / dble ( nt - 1 )
    end do
  end if

  return
end
subroutine tvec_even_bracket ( theta1, theta2, nt, t )
!
!*******************************************************************************
!
!! TVEC_EVEN_BRACKET computes an evenly spaced set of angles between THETA1 and THETA2.
!
!
!  Example:
!
!    NT = 4
!    THETA1 = 30
!    THETA2 = 90
!
!    T = ( 30, 50, 70, 90 )
!
!  Modified:
!
!    13 January 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, double precision THETA1, THETA2, the limiting angles.
!
!    Input, integer NT, the number of values to compute.
!
!    Output, double precision TVEC(NT), the evenly spaced angles.
!
  integer nt
!
  integer i
  double precision t(nt)
  double precision theta1
  double precision theta2
!
  if ( nt == 1 ) then
    t(i) = ( theta1 + theta2 ) / 2.0D+00
  else
    do i = 1, nt
      t(i) = dble ( dble ( nt - i ) * theta1 &
                  + dble ( i - 1 ) * theta2 ) / dble ( nt - 1 )
    end do
  end if

  return
end
subroutine tvec_even_bracket2 ( theta1, theta2, nt, t )
!
!*******************************************************************************
!
!! TVEC_EVEN_BRACKET2 computes an evenly spaced set of angles between THETA1 and THETA2.
!
!
!  Example:
!
!    NT = 5
!    THETA1 = 30
!    THETA2 = 90
!
!    T = ( 40, 50, 60, 70, 80 )
!
!  Modified:
!
!    13 January 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, double precision THETA1, THETA2, the limiting angles.
!
!    Input, integer NT, the number of values to compute.
!
!    Output, double precision TVEC(NT), the evenly spaced angles.
!
  integer nt
!
  integer i
  double precision t(nt)
  double precision theta1
  double precision theta2
!
  do i = 1, nt
    t(i) = dble ( dble ( nt + 1 - i ) * theta1 &
                  + dble ( i ) * theta2 ) / dble ( nt + 1 )
  end do

  return
end
subroutine tvec_even_bracket3 ( theta1, theta2, nt, t )
!
!*******************************************************************************
!
!! TVEC_EVEN_BRACKET3 computes an evenly spaced set of angles between THETA1 and THETA2.
!
!
!  Example:
!
!    NT = 3
!    THETA1 = 30
!    THETA2 = 90
!
!    T = ( 40, 60, 80 )
!
!  Modified:
!
!    13 January 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, double precision THETA1, THETA2, the limiting angles.
!
!    Input, integer NT, the number of values to compute.
!
!    Output, double precision TVEC(NT), the evenly spaced angles.
!
  integer nt
!
  integer i
  double precision t(nt)
  double precision theta1
  double precision theta2
!
  do i = 1, nt
    t(i) = dble ( dble ( 2 * nt + 1 - 2 * i ) * theta1 &
                  + dble ( 2 * i - 1 ) * theta2 ) / dble ( 2 * nt )
  end do

  return
end
subroutine vec_next ( n, iarray, more, ibase )
!
!*******************************************************************************
!
!! VEC_NEXT generates all N-vectors of integers modulo a given base.
!
!
!  Examples:
!
!    N = 2, IBASE = 3
!
!    0   0
!    0   1
!    0   2
!    1   0
!    1   1
!    1   2
!    2   0
!    2   1
!    2   2
!
!  Comment:
!
!    The vectors are produced in lexical order, starting with
!    (0,0,...,0), (0,0,...,1), ... through (IBASE-1,IBASE-1,...,IBASE-1).
!
!  Modified:
!
!    15 April 1999
!
!  Parameters:
!
!    Input, integer N, the size of the vectors to be used.
!
!    Output, integer IARRAY(N).  On each return, IARRAY
!    will contain entries in the range 0 to IBASE-1.
!
!    Input/output, logical MORE.  Set this variable .FALSE. before
!    the first call.  Normally, MORE will be returned .TRUE. but
!    once all the vectors have been generated, MORE will be
!    reset .FALSE. and you should stop calling the program.
!
!    Input, integer IBASE, the base to be used.  IBASE = 2 will
!    give vectors of 0's and 1's, for instance.
!
  integer n
!
  integer i
  integer iarray(n)
  integer ibase
  integer, save :: kount = 0
  integer, save :: last = 0
  logical more
  integer nn
!
  if ( .not. more ) then

    kount = 1
    last = ibase**n
    more = .true.
    iarray(1:n) = 0

  else

    kount = kount + 1

    if ( kount == last ) then
      more = .false.
    end if

    iarray(n) = iarray(n) + 1

    do i = 1, n

      nn = n - i

      if ( iarray(nn+1) < ibase ) then
        return
      end if

      iarray(nn+1) = 0

      if ( nn /= 0 ) then
        iarray(nn) = iarray(nn) + 1
      end if

    end do

  end if

  return
end
