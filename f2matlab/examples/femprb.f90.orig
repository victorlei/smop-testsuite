!  femprb.f90  02 June 2000
!
program femprb
!
!*******************************************************************************
!
!! FEMPRB calls the various FEMPACK tests.
!
  write ( *, * ) ' '
  write ( *, * ) 'FEMPRB'
  write ( *, * ) '  Begin FEMPACK tests.'
  call test01
  call test02
  call test03
  call test04
  write ( *, * ) ' '
  write ( *, * ) 'FEMPRB'
  write ( *, * ) '  Normal end of FEMPACK tests.'
!  stop
end
subroutine test01
!
!*******************************************************************************
!
!! TEST01 tests the shape routines.
!
  write ( *, * ) ' '
  write ( *, * ) 'TEST01'
  write ( *, * ) '  SHAPE_TEST tests the shape routines.'
  call shape_test ( 'Q4' )
  call shape_test ( 'Q8' )
  call shape_test ( 'Q9' )
  call shape_test ( 'Q12' )
  call shape_test ( 'Q16' )
  call shape_test ( 'QL' )
  call shape_test ( 'T3' )
  call shape_test ( 'T6' )
  call shape_test ( 'T10' )
  return
end
subroutine test02
!
!*******************************************************************************
!
!! TEST02 tests the grid routines.
!
  write ( *, * ) ' '
  write ( *, * ) 'TEST02'
  write ( *, * ) '  Test the grid routines.'
  call grid_test ( 'Q4' )
  call grid_test ( 'Q8' )
  call grid_test ( 'Q9' )
  call grid_test ( 'Q12' )
  call grid_test ( 'Q16' )
  call grid_test ( 'QL' )
  call grid_test ( 'T3' )
  call grid_test ( 'T6' )
  call grid_test ( 'T10' )
  return
end
subroutine test03
!
!*******************************************************************************
!
!! TEST03 tests the map routines.
!
  write ( *, * ) ' '
  write ( *, * ) 'TEST03'
  write ( *, * ) '  Test the map routines.'
  call map_test ( 'Q4' )
  call map_test ( 'Q8' )
  call map_test ( 'Q9' )
  call map_test ( 'Q12' )
  call map_test ( 'Q16' )
  call map_test ( 'QL' )
  call map_test ( 'T3' )
  call map_test ( 'T6' )
  call map_test ( 'T10' )
  return
end
subroutine test04
!
!*******************************************************************************
!
!! TEST04 tests DIV_BIL.
!
  integer, parameter :: m = 21
  integer, parameter :: n = 13
  integer, parameter :: maxm = m
!
  real diff
  real div(maxm, n-1)
  real dudx
  real dudy
  real dvdx
  real dvdy
  integer i
  integer j
  real temp
  real u(maxm, n)
  real v(maxm, n)
  real vort(maxm, n-1)
  real x
  real xhi
  real xlo
  real xm
  real y
  real yhi
  real ylo
  real ym
!
  write ( *, * ) ' '
  write ( *, * ) 'TEST04'
  write ( *, * ) '  DIV_BIL estimates divergence and vorticity.'
  write ( *, * ) ' '
  write ( *, * ) '  Original U, V data forms ', m, ' rows and '
  write ( *, * ) '  ', n, ' columns.'
!
!  Set limits of the data points.
!
  xlo = 0.0
  xhi = 1.0
  ylo = 0.0
  yhi = 2.0
!
!  Put dummy data into U and V at the data nodes.
!
  write ( *, * ) ' '
  write ( *, * ) '  I, J, X, Y, U(I,J), V(I,J)'
  write ( *, * ) ' '
  do i = 1, m
    y = ( real ( m - i ) * ylo + real ( i - 1 ) * yhi ) / real ( m - 1 )
    do j = 1, n
      x = ( real ( n - j ) * xlo + real ( j - 1 ) * xhi ) / real ( n - 1 )
      u(i,j) = x * y
      v(i,j) = sin ( x**2 + y**2 )
      write ( *, '(2i4,4g14.6)' ) i, j, x, y, u(i,j), v(i, j)
    end do
    write ( *, * ) ' '
  end do
!
!  Get DIV and VORT.
!
  call div_bil ( div, m, maxm, n, u, v, vort, xhi, xlo, yhi, ylo )
!
!  Compare computed and known values at the centers of the elements.
!
  write ( *, * ) ' '
  write ( *, * ) 'I, J, XM(I, j), YM(I, j)'
  write ( *, * ) ' '
  do i = 1, m-1
    ym = ( real(2*m-2*i-1) * ylo + real(2*i-1) * yhi ) / real ( 2 * m - 2 )
    do j = 1, n-1
      xm = ( real(2*n-2*j-1) * xlo + real(2*j-1) * xhi ) / real( 2 * n - 2 )
      write ( *, '(2i4,3g14.6)' ) i, j, xm, ym
    end do
    write ( *, * ) ' '
  end do
  write ( *, * ) ' '
  write ( *, * ) '  I, J, DIV(I, j), Exact Divergence, Difference'
  write ( *, * ) ' '
  do i = 1, m - 1
    ym = ( real(2*m-2*i-1) * ylo + real(2*i-1) * yhi ) / real ( 2 * m - 2 )
    do j = 1, n - 1
      xm = ( real(2*n-2*j-1) * xlo + real(2*j-1) * xhi ) / real ( 2 * n - 2 )
      dudx = ym
      dudy = xm
      dvdx = 2.0 * xm * cos ( xm**2 + ym**2 )
      dvdy = 2.0 * ym * cos ( xm**2 + ym**2 )
      temp = dudx + dvdy
      diff = div(i, j) - temp
      write ( *, '(2i4,3g14.6)' ) i, j, div(i,j), temp, diff
    end do
    write ( *, * ) ' '
  end do
  write ( *, * ) ' '
  write ( *, * ) '  I, J, VORT(I, j), Exact Vorticity, Difference'
  write ( *, * ) ' '
  do i = 1, m - 1
    y = ( real(2*m-2*i-1) * ylo + real(2*i-1) * yhi ) / real ( 2 * m - 2 )
    do j = 1, n-1
      x = ( real(2*n-2*j-1) * xlo + real(2*j-1) * xhi ) / real ( 2 * n - 2 )
      dudx = y
      dudy = x
      dvdx = 2.0 * x * cos ( x**2 + y**2 )
      dvdy = 2.0 * y * cos ( x**2 + y**2 )
      temp = dvdx - dudy
      diff = vort(i, j) - temp
      write ( *, '(2i4,3g14.6)' ) i, j, vort(i, j), temp, diff
    end do
    write ( *, * ) ' '
  end do
  return
end










!  fempack.f90  02 June 2000
!
subroutine base_bil ( dx11, dx12, dx21, dx22, dy11, dy12, dy21, dy22, psi11, &
  psi12, psi21, psi22, xl, xm, xr, yb, ym, yt )
!
!*******************************************************************************
!
!! BASE_BIL evalutes basis functions for a rectangular bilinear element.
!
!
!  Discussion:
!
!    The routine is given the corners of a rectangular bilinear element.
!    It then evaluates the basis functions associated with each corner,
!    and their derivatives with respect to X and Y.
!
!    The "local node" numbering is as follows:
!
!      (2,1)---(2,2)   <-- Y = YT
!        |       |
!        |       |
!      (1,1)---(1,2)   <-- Y = YB
!
!        ^       ^
!        |       |
!       x = XL    x = XR
!
!  Modified:
!
!    02 March 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real DX11, DX12, DX21, DX22, the derivatives of the basis
!    functions with respect to X, associated with local nodes (1,1), (1,2),
!    (2,1) and (2,2), and evaluated at the point (XM,YM).
!
!    Output, real DY11, DY12, DY21, DY22.
!    The derivatives of the basis functions with respect to Y,
!    associated with local nodes (1,1), (1,2), (2,1) and (2,2),
!    and evaluated at the point (XM,YM).
!
!    Output, real PSI11, PSI12, PSI21, PSI22.
!    The basis functions associated with local nodes (1,1), (1,2),
!    (2,1) and (2,2), and evaluated at the point (XM,YM).
!
!    Input, real XL, the X coordinate of the left boundary of the element.
!
!    Input, real XM.
!    XM is the X coordinate of the point at which the basis functions
!    and their derivatives should be evaluated.  It need not be the
!    center point, but generally should lie within the element.
!
!    Input, real XR, the X coordinate of the right boundary of the element.
!
!    Input, real YB, the Y coordinate of the bottom boundary of the element.
!
!    Input, real YM.
!    YM is the Y coordinate of the point at which the basis functions
!    and their derivatives should be evaluated.  It need not be the
!    center point, but generally should lie within the element.
!
!    Input, real YT, the Y coordinate of the top boundary of the element.
!
  real dx11
  real dx12
  real dx21
  real dx22
  real dy11
  real dy12
  real dy21
  real dy22
  real psi11
  real psi12
  real psi21
  real psi22
  real xl
  real xm
  real xr
  real yb
  real ym
  real yt
!
  psi11 = ( xm - xr ) * ( ym - yt ) / ( ( xl - xr ) * ( yb - yt ) )
  psi12 = ( xm - xl ) * ( ym - yt ) / ( ( xr - xl ) * ( yb - yt ) )
  psi21 = ( xm - xr ) * ( ym - yb ) / ( ( xl - xr ) * ( yt - yb ) )
  psi22 = ( xm - xl ) * ( ym - yb ) / ( ( xr - xl ) * ( yt - yb ) )
  dx11 = ( ym - yt ) / ( ( xl - xr ) * ( yb - yt ) )
  dx12 = ( ym - yt ) / ( ( xr - xl ) * ( yb - yt ) )
  dx21 = ( ym - yb ) / ( ( xl - xr ) * ( yt - yb ) )
  dx22 = ( ym - yb ) / ( ( xr - xl ) * ( yt - yb ) )
  dy11 = ( xm - xr ) / ( ( xl - xr ) * ( yb - yt ) )
  dy12 = ( xm - xl ) / ( ( xr - xl ) * ( yb - yt ) )
  dy21 = ( xm - xr ) / ( ( xl - xr ) * ( yt - yb ) )
  dy22 = ( xm - xl ) / ( ( xr - xl ) * ( yt - yb ) )
  return
end
subroutine div_bil ( div, m, maxm, n, u, v, vort, xhi, xlo, yhi, ylo )
!
!*******************************************************************************
!
!! DIV_BIL estimates the divergence and vorticity of a discrete field.
!
!
!  Discussion:
!
!    The routine is given the values of a vector field ( U(X,Y), V(X,Y) ) at
!    an array of points ( X(I), Y(J) ) for I = 1,M and J = 1,N.
!
!    The routine models the vector field over the interior of this region using
!    a bilinear interpolant.  It then uses the interpolant to estimate the
!    value of the divergence:
!
!      DIV(X,Y) = dU/dX + dV/dY
!
!    and the vorticity:
!
!      VORT(X,Y) = dV/dX - dU/dY
!
!    at the INTERIORS of each of the bilinear elements.
!
!        |       |       |
!      (3,1)---(3,2)---(3,3)---
!        |       |       |
!        | [2,1] | [2,2] |
!        |       |       |
!      (2,1)---(2,2)---(2,3)---
!        |       |       |
!        | [1,1] | [1,2] |
!        |       |       |
!      (1,1)---(1,2)---(1,3)---
!
!    Here, the nodes labeled with parentheses represent the points at
!    which the original (U,V) data is given, while the nodes labeled
!    with square brackets represent the centers of the bilinear
!    elements, where the approximations to the divergence and vorticity
!    are made.
!
!    The reason for evaluating the divergence and vorticity in this way
!    is that the bilinear interpolant to the (U,V) data is not
!    differentiable at the boundaries of the elements, nor especially at
!    the nodes, but is an (infinitely differentiable) bilinear function
!    in the interior of each element.  If a value at the original nodes
!    is strongly desired, then the average at the four surrounding
!    central nodes may be taken.
!
!  Modified:
!
!    01 March 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real DIV(MAXM,N-1).
!    For I = 1 to M-1, J = 1 to N-1, DIV(I,J) contains an estimate for
!    the divergence in the bilinear element that lies between
!    data rows I and I+1, and data columns J and J+1.
!
!    Input, integer M, is the number of data rows.  M must be at
!    least 2.
!
!    Input, integer MAXM, is the leading dimension to use for DIV, U,
!    V, and VORT.  MAXM must be at least M.
!
!    Input, integer N, the number of data columns.  N must be at least 2.
!
!    Input, real U(MAXM,N).
!    For I = 1 to M, J = 1 to N, U(I,J) contains the value of the
!    first component of a vector quantity whose divergence and
!    vorticity are desired.  A common example would be that U
!    represents the horizontal velocity component of a flow field.
!
!    Input, real V(MAXM,N).
!    For I = 1 to M, J = 1 to N, V(I,J) contains the value of the
!    second component of a vector quantity whose divergence and
!    vorticity are desired.  A common example would be that V
!    represents the vertical velocity component of a flow field.
!
!    Output, real VORT(MAXM,N-1).
!    For I = 1 to M-1, J = 1 to N-1, VORT(I,J) contains an estimate for
!    the vorticity in the bilinear element that lies between
!    data rows I and I+1, and data columns J and J+1.
!
!    Input, real XHI, the X coordinate of the rightmost data column.
!
!    Input, real XLO, the X coordinate of the leftmost data column.
!    XHI and XLO must be distinct.
!
!    Input, real YHI, the Y coordinate of the uppermost data row.
!
!    Input, real YLO, the Y coordinate of the lowermost data row.
!    YHI and YLO must be distinct.
!
  integer maxm
  integer n
!
  real div(maxm,n-1)
  real dx11
  real dx12
  real dx21
  real dx22
  real dy11
  real dy12
  real dy21
  real dy22
  integer i
  integer j
  integer m
  real psi11
  real psi12
  real psi21
  real psi22
  real u(maxm,n)
  real v(maxm,n)
  real vort(maxm,n-1)
  real xhi
  real xl
  real xlo
  real xm
  real xr
  real yhi
  real yb
  real ylo
  real ym
  real yt
!
  if ( m <= 1 ) then
    write ( *, * ) ' '
    write ( *, * ) 'DIV_BIL - Fatal error!'
    write ( *, * ) '  M must be at least 2,'
    write ( *, * ) '  but the input value of M is ', m
    stop
  end if
  if ( maxm < m ) then
    write ( *, * ) ' '
    write ( *, * ) 'DIV_BIL - Fatal error!'
    write ( *, * ) '  MAXM must be at least M,'
    write ( *, * ) '  but the input value of MAXM is ', maxm
    write ( *, * ) '  and the input value of M is ', m
    stop
  end if
  if ( n <= 1 ) then
    write ( *, * ) ' '
    write ( *, * ) 'DIV_BIL - Fatal error!'
    write ( *, * ) '  N must be at least 2,'
    write ( *, * ) '  but the input value of N is ', n
    stop
  end if
  if ( xhi == xlo ) then
    write ( *, * ) ' '
    write ( *, * ) 'DIV_BIL - Fatal error!'
    write ( *, * ) '  XHI and XLO must be distinct,'
    write ( *, * ) '  but the input value of XLO is ', xlo
    write ( *, * ) '  and the input value of XHI is ', xhi
    stop
  end if
  if ( yhi == ylo ) then
    write ( *, * ) ' '
    write ( *, * ) 'DIV_BIL - Fatal error!'
    write ( *, * ) '  YHI and YLO must be distinct,'
    write ( *, * ) '  but the input value of YLO is ', ylo
    write ( *, * ) '  and the input value of YHI is ', yhi
    stop
  end if
!
  do i = 1, m-1
    yb = ( (2*m-2*i  ) * ylo + (2*i-2) * yhi ) / real ( 2*m-2 )
    ym = ( (2*m-2*i-1) * ylo + (2*i-1) * yhi ) / real ( 2*m-2 )
    yt = ( (2*m-2*i-2) * ylo + (2*i  ) * yhi ) / real ( 2*m-2 )
    do j = 1, n-1
      xl = ( (2*n-2*j  ) * xlo + (2*j-2) * xhi ) / real ( 2*n-2 )
      xm = ( (2*n-2*j-1) * xlo + (2*j-1) * xhi ) / real ( 2*n-2 )
      xr = ( (2*n-2*j-2) * xlo + (2*j  ) * xhi ) / real ( 2*n-2 )
      call base_bil ( dx11, dx12, dx21, dx22, dy11, dy12, dy21, &
        dy22, psi11, psi12, psi21, psi22, xl, xm, xr, yb, ym, yt )
!
!  Note the following formula for the value of U and V at the same
!  point that the divergence and vorticity are being evaluated.
!
!         umid =  u(i  ,j  ) * psi11 + u(i  ,j+1) * psi12 &
!               + u(i+1,j  ) * psi21 + u(i+1,j+1) * psi22
!
!         vmid =  v(i  ,j  ) * psi11 + v(i  ,j+1) * psi12 &
!               + v(i+1,j  ) * psi21 + v(i+1,j+1) * psi22
!
      div(i,j) =  u(i  ,j  ) * dx11 + u(i  ,j+1) * dx12 &
                + u(i+1,j  ) * dx21 + u(i+1,j+1) * dx22 &
                + v(i  ,j  ) * dy11 + v(i  ,j+1) * dy12 &
                + v(i+1,j  ) * dy21 + v(i+1,j+1) * dy22
      vort(i,j) =  v(i  ,j  ) * dx11 + v(i  ,j+1) * dx12 &
                 + v(i+1,j  ) * dx21 + v(i+1,j+1) * dx22 &
                 - u(i  ,j  ) * dy11 - u(i  ,j+1) * dy12 &
                 - u(i+1,j  ) * dy21 - u(i+1,j+1) * dy22
    end do
  end do
  return
end
function element_code ( i )
!
!*******************************************************************************
!
!! ELEMENT_CODE returns the code for each element.
!
!
!  List:
!
!    I  ELEMENT_CODE   Definition
!    -  ------------   ----------
!    1  Q4             4 node linear Lagrange/serendipity quadrilateral;
!    2  Q8             8 node quadratic serendipity quadrilateral;
!    3  Q9             9 node quadratic Lagrange quadrilateral;
!    4  Q12            12 node cubic serendipity quadrilateral;
!    5  Q16            16 node cubic Lagrange quadrilateral;
!    6  QL             6 node linear/quadratic quadrilateral;
!    7  T3             3 node linear triangle;
!    8  T6             6 node quadratic triangle;
!    9  T10            10 node cubic triangle.
! 
!  Modified:
!
!    04 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer I, the number of the element.  
!
!    Output, character ( len = 4 ) ELEMENT_CODE, the code for the element.
!
  character ( len = 4 ) element_code
  integer i
!
  if ( i == 1 ) then
    element_code = 'Q4'
  else if ( i == 2 ) then
    element_code = 'Q8'
  else if ( i == 3 ) then
    element_code = 'Q9'
  else if ( i == 4 ) then
    element_code = 'Q12'
  else if ( i == 5 ) then
    element_code = 'Q16'
  else if ( i == 6 ) then
    element_code = 'QL'
  else if ( i == 7 ) then
    element_code = 'T3'
  else if ( i == 8 ) then
    element_code = 'T6'
  else if ( i == 9 ) then
    element_code = 'T10'
  else
    element_code = '????'
  end if
  return
end
function order_code ( code )
!
!*******************************************************************************
!
!! ORDER_CODE returns the order for each element.
!
!
!  List:
!
!    CODE  Order  Definition
!    ----  -----  ----------
!    Q4     4     4 node linear Lagrange/serendipity quadrilateral;
!    Q8     8     8 node quadratic serendipity quadrilateral;
!    Q9     9     9 node quadratic Lagrange quadrilateral;
!    Q12   12     12 node cubic serendipity quadrilateral;
!    Q16   16     16 node cubic Lagrange quadrilateral;
!    QL     6     6 node linear/quadratic quadrilateral;
!    T3     3     3 node linear triangle;
!    T6     6     6 node quadratic triangle;
!    T10   10     10 node cubic triangle.
! 
!  Modified:
!
!    05 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) CODE, the code for the element.
!
!    Output, integer ORDER_CODE, the order of the element.
!
  character ( len = * ) code
  integer order_code
!
  if ( code == 'Q4' ) then
    order_code = 4
  else if ( code == 'Q8' ) then
    order_code = 8
  else if ( code == 'Q9' ) then
    order_code = 9
  else if ( code == 'Q12' ) then
    order_code = 12
  else if ( code == 'Q16' ) then
    order_code = 16
  else if ( code == 'QL' ) then
    order_code = 6
  else if ( code == 'T3' ) then
    order_code = 3
  else if ( code == 'T6' ) then
    order_code = 6
  else if ( code == 'T10' ) then
    order_code = 10
  else
    order_code = 0
  end if
  return
end
subroutine grid ( code, maxelem, n, nelemx, nelemy, nodes )
!
!*******************************************************************************
!
!! GRID returns the grid associated with any available element.
!
!
!  Modified:
!
!    05 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) CODE, identifies the element desired.
!    Legal values include 'Q4', 'Q8', 'Q9', 'Q12', 'Q16', 'QL', 'T3', 
!    'T6' and 'T10'.
!
!    Input, integer MAXELEM, the maximum number of elements for which
!    storage is provided, which must be at least NELEMX * NELEMY.
!
!    Input, integer N, the order of the element.
!
!    Input, integer NELEMX, NELEMY, the number of quadrilaterals along the
!    X and Y directions.  The number of elements generated will be
!    NELEMX * NELEMY.
!
!    Output, integer NODES(N,MAXELEM); NODES(I,J) contains the index
!    of the I-th node of the J-th element.  
!
  integer maxelem
  integer n
!
  character ( len = * ) code
  integer nelemx
  integer nelemy
  integer nodes(n,maxelem)
!
  if ( code == 'Q4' ) then
    call grid_q4 ( maxelem, nelemx, nelemy, nodes )
  else if ( code == 'Q8' ) then
    call grid_q8 ( maxelem, nelemx, nelemy, nodes )
  else if ( code == 'Q9' ) then
    call grid_q9 ( maxelem, nelemx, nelemy, nodes )
  else if ( code == 'Q12' ) then
    call grid_q12 ( maxelem, nelemx, nelemy, nodes )
  else if ( code == 'Q16' ) then
    call grid_q16 ( maxelem, nelemx, nelemy, nodes )
  else if ( code == 'QL' ) then
    call grid_ql ( maxelem, nelemx, nelemy, nodes )
  else if ( code == 'T3' ) then
    call grid_t3 ( maxelem, nelemx, nelemy, nodes )
  else if ( code == 'T6' ) then
    call grid_t6 ( maxelem, nelemx, nelemy, nodes )
  else if ( code == 'T10' ) then
    call grid_t10 ( maxelem, nelemx, nelemy, nodes )
  else
    write ( *, * ) ' '
    write ( *, * ) 'GRID - Fatal error!'
    write ( *, '(a,a)' ) '  Illegal value of CODE = ', code
    stop
  end if
  return
end
subroutine grid_test ( code )
!
!*******************************************************************************
!
!! GRID_TEST tests the grid routines.
!
!
!  Modified:
!
!    05 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) CODE, the code for the element.
!    Legal values include 'Q4', 'Q8', 'Q9', 'Q12', 'Q16', 'QL', 
!    'T3', 'T6' and 'T10'.
!
  integer, parameter :: maxelem = 12
!
  character ( len = * ) code
  integer i
  integer ielem
  integer n
  integer nelem
  integer nelemx
  integer nelemy
  integer nodes(16*maxelem)
  integer order_code
  integer width
!
!  NODES is defined as a vector rather than a two dimensional array,
!  so that we can handle the various cases using a single array.
!
  write ( *, * ) ' '
  write ( *, * ) 'GRID_TEST'
  write ( *, '(a,a)' ) '  Test the grid routine for element ', code
  nelemx = 3
  nelemy = 2
  nelem = nelemx * nelemy
  n = order_code ( code )
  call grid ( code, maxelem, n, nelemx, nelemy, nodes )
  do ielem = 1, nelem
    write ( *, '(i3,3x,20i3)' ) ielem, ( nodes((ielem-1)*n+i), i = 1, n )
  end do
  call grid_width ( maxelem, n, nelem, nodes, width )
  write ( *, * ) ' '
  write ( *, * ) 'Grid width is ', width
  return
end
subroutine grid_width ( maxelm, n, nelem, node, width )
!
!*******************************************************************************
!
!! GRID_WIDTH computes the width of a given grid.
!
!
!  Definition:
!
!    The grid width is defined to be 1 plus the maximum absolute
!    difference of global indices of nodes in the same element.
!
!  Example:
!
!   For the following simple grid, the grid width is 14.
!
!   23---24---25---26---27---28---29
!    |         |         |         |
!    |         |         |         |
!   19        20        21        22
!    |         |         |         |
!    | 4       | 5       | 6       |
!   12---13---14---15---16---17---18
!    |         |         |         |
!    |         |         |         |
!    8         9        10        11
!    |         |         |         |
!    | 1       | 2       | 3       |
!    1----2----3----4----5----6----7
!
!  Modified:
!
!    12 October 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer MAXELM, the maximum number of elements.
!
!    Input, integer N, the order of the elements.
!
!    Input, integer NELEM, the number of elements.
! 
!    Input, integer NODE(N,MAXELM), the nodes that make up each element.
!
!    Output, integer WIDTH, the grid width.
!
  integer maxelm
  integer n
!
  integer ielem
  integer inode1
  integer inode2
  integer ip1
  integer ip2
  integer nelem
  integer node(n,maxelm)
  integer width
!
  width = 0
 
  do ielem = 1, nelem
    do inode1 = 1, n
      ip1 = node(inode1,ielem)
      do inode2 = 1, n
        ip2 = node(inode2,ielem)
        width = max ( width, abs ( ip1 - ip2 ) )
      end do
    end do
  end do
 
  return
end
subroutine grid_q4 ( maxelem, nelemx, nelemy, nodes )
!
!*******************************************************************************
!
!! GRID_Q4 produces a grid of 4 node quadrilaterals.
!
!
!  Example:
!
!    Input:
!
!      NELEMX = 3, NELEMY = 2
!
!    Output:
!
!      NODES = 
!         1, 2,  5,  6;
!         2, 3,  6,  7;
!         3, 4,  7,  8;
!         5, 6,  9, 10;
!         6, 7, 10, 11;
!         7, 8, 11, 12.
!
!  Diagram:
!
!    9---10---11---12
!    |    |    |    |
!    |    |    |    |
!    |  4 |  5 |  6 |
!    |    |    |    |
!    5----6----7----8
!    |    |    |    |
!    |    |    |    |
!    |  1 |  2 |  3 |
!    |    |    |    |
!    1----2----3----4
!
!  Modified:
!
!    05 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer MAXELEM, the maximum number of elements for which
!    storage is provided, which must be at least NELEMX * NELEMY.
!
!    Input, integer NELEMX, NELEMY, the number of quadrilaterals along the
!    X and Y directions.  The number of elements generated will be
!    NELEMX * NELEMY.
!
!    Output, integer NODES(4,MAXELEM); NODES(I,J) contains the index
!    of the I-th node of the J-th element.  
!
  integer maxelem
!
  integer ielem
  integer i
  integer j
  integer nelem
  integer nelemx
  integer nelemy
  integer nodes(4,maxelem)
!
  nelem = nelemx * nelemy
  if ( nelem > maxelem ) then
    write ( *, * ) ' '
    write ( *, * ) 'GRID_Q4 - Fatal error!'
    write ( *, * ) '  Not enough storage for NODE array.'
    write ( *, * ) '  Increase MAXELEM to ', nelem
    stop
  end if
  ielem = 0
 
  do j = 1, nelemy
    do i = 1, nelemx
      ielem = ielem + 1
      nodes(1,ielem) = ( j - 1 ) * ( nelemx + 1 ) + i
      nodes(2,ielem) = ( j - 1 ) * ( nelemx + 1 ) + i + 1
      nodes(3,ielem) =   j       * ( nelemx + 1 ) + i
      nodes(4,ielem) =   j       * ( nelemx + 1 ) + i + 1
    end do
  end do
  return
end
subroutine grid_q8 ( maxelem, nelemx, nelemy, nodes )
!
!*******************************************************************************
!
!! GRID_Q8 produces a grid of 8 node quadrilaterals.
!
!
!  Example:
!
!    Input:
!
!      NELEMX = 3, NELEMY = 2
!
!    Output:
!
!      NODES =
!         3, 14, 12,  1,  2,  9, 13,  8;
!         5, 16, 14,  3,  4, 10, 15,  9;
!         7, 18, 16,  5,  6, 11, 17, 10;
!        14, 25, 23, 12, 13, 20, 24, 19;
!        16, 27, 25, 14, 15, 21, 26, 20;
!        18, 29, 27, 16, 17, 22, 28, 21.
!
!  Diagram:
!
!   23---24---25---26---27---28---29
!    |         |         |         |
!    |         |         |         |
!   19        20        21        22
!    |         |         |         |
!    | 4       | 5       | 6       |
!   12---13---14---15---16---17---18
!    |         |         |         |
!    |         |         |         |
!    8         9        10        11
!    |         |         |         |
!    | 1       | 2       | 3       |
!    1----2----3----4----5----6----7
!
!  Modified:
!
!    12 October 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer MAXELEM, the maximum number of elements for which
!    storage is provided, which must be at least NELEMX * NELEMY.
!
!    Input, integer NELEMX, NELEMY, the number of quadrilaterals along the
!    X and Y directions.  The number of elements generated will be
!    NELEMX * NELEMY.
!
!    Output, integer NODES(8,MAXELEM); NODES(I,J) contains the index
!    of the I-th node of the J-th element.
!
  integer maxelem
!
  integer base
  integer ielem
  integer i
  integer j
  integer nelem
  integer nelemx
  integer nelemy
  integer nodes(8,maxelem)
!
  nelem = nelemx * nelemy
  if ( nelem > maxelem ) then
    write ( *, * ) ' '
    write ( *, * ) 'GRID_Q8 - Fatal error!'
    write ( *, * ) '  Not enough storage for NODE array.'
    write ( *, * ) '  Increase MAXELEM to ', nelem
    stop
  end if
  ielem = 0
  do j = 1, nelemy
    do i = 1, nelemx
      ielem = ielem + 1
      base = ( j - 1 )  * ( 3 * nelemx + 2 ) + 2 * i - 1
      nodes(1,ielem) = base + 2
      nodes(2,ielem) = base + ( 3 * nelemx + 2 ) + 2
      nodes(3,ielem) = base + ( 3 * nelemx + 2 )
      nodes(4,ielem) = base
      nodes(5,ielem) = base + 1
      nodes(6,ielem) = base + 2 * nelemx + 2 - i + 1
      nodes(7,ielem) = base + ( 3 * nelemx + 2 ) + 1
      nodes(8,ielem) = base +  2 * nelemx + 2 - i
    end do
  end do
  return
end
subroutine grid_q9 ( maxelem, nelemx, nelemy, nodes )
!
!*******************************************************************************
!
!! GRID_Q9 produces a grid of 9 node quadrilaterals.
!
!
!  Example:
!
!    Input:
!
!      NELEMX = 3, NELEMY = 2
!
!    Output:
!
!      NODES = 
!         3, 17, 15,  1,  2, 10, 16,  8,  9;
!         5, 19, 17,  3,  4, 12, 18, 10, 11;
!         7, 21, 19,  5,  6, 14, 20, 12, 13;
!        17, 31, 29, 15, 16, 24, 30, 22, 23;
!        19, 33, 31, 17, 18, 26, 32, 24, 25;
!        21, 35, 33, 19, 20, 28, 34, 26, 27.
!
!  Diagram:
!
!   29---30---31---32---33---34---35
!    |    .    |    .    |    .    |
!    |    .    |    .    |    .    |
!   22 . 23 . 24 . 25 . 26 . 27 . 28
!    |    .    |    .    |    .    |
!    | 4  .    | 5  .    | 6  .    |
!   15---16---17---18---19---20---21
!    |    .    |    .    |    .    |
!    |    .    |    .    |    .    |
!    8 .  9 . 10 . 11 . 12 . 13 . 14
!    |    .    |    .    |    .    |
!    | 1  .    | 2  .    | 3  .    |
!    1----2----3----4----5----6----7
!
!  Modified:
!
!    11 October 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer MAXELEM, the maximum number of elements for which
!    storage is provided, which must be at least NELEMX * NELEMY.
!
!    Input, integer NELEMX, NELEMY, the number of quadrilaterals along the
!    X and Y directions.  The number of elements generated will be
!    NELEMX * NELEMY.
!
!    Output, integer NODES(9,MAXELEM); NODES(I,J) contains the index
!    of the I-th node of the J-th element.  
!
  integer maxelem
!
  integer base
  integer ielem
  integer i
  integer j
  integer nelem
  integer nelemx
  integer nelemy
  integer nodes(9,maxelem)
!
  nelem = nelemx * nelemy
  if ( nelem > maxelem ) then
    write ( *, * ) ' '
    write ( *, * ) 'GRID_Q9 - Fatal error!'
    write ( *, * ) '  Not enough storage for NODE array.'
    write ( *, * ) '  Increase MAXELEM to ', nelem
    stop
  end if
  ielem = 0
 
  do j = 1, nelemy
    do i = 1, nelemx
      ielem = ielem + 1
      base = ( 2 * j - 2 )  * ( 2 * nelemx + 1 ) + 2 * i - 1
      nodes(1,ielem) = base + 2
      nodes(2,ielem) = base + 2 * ( 2 * nelemx + 1 ) + 2
      nodes(3,ielem) = base + 2 * ( 2 * nelemx + 1 )
      nodes(4,ielem) = base
      nodes(5,ielem) = base + 1
      nodes(6,ielem) = base +     ( 2 * nelemx + 1 ) + 2
      nodes(7,ielem) = base + 2 * ( 2 * nelemx + 1 ) + 1
      nodes(8,ielem) = base +     ( 2 * nelemx + 1 )
      nodes(9,ielem) = base +     ( 2 * nelemx + 1 ) + 1
    end do
  end do
  return
end
subroutine grid_q12 ( maxelem, nelemx, nelemy, nodes )
!
!*******************************************************************************
!
!! GRID_Q12 produces a grid of 12 node quadrilaterals.
!
!
!  Example:
!
!    Input:
!
!      NELEMX = 3, NELEMY = 2
!
!    Output:
!
!      NODES =
!         1,  2,  3,  4, 11, 12, 15, 16, 19, 20, 21, 22;
!         4,  5,  6,  7, 12, 13, 16, 17, 22, 23, 24, 25;
!         7,  8,  9, 10, 13, 14, 17, 18, 25, 26, 27, 28;
!        19, 20, 21, 22, 29, 30, 33, 34, 37, 38, 39, 40;
!        22, 23, 24, 25, 30, 31, 34, 35, 40, 41, 42, 43;
!        25, 26, 27, 28, 31, 32, 35, 36, 43, 44, 45, 46.
!
!  Diagram:
!
!   37-38-39-40-41-42-43-44-45-46
!    |        |        |        |
!   33       34       35       36
!    |        |        |        |
!   29       30       31       32
!    | 4      | 5      | 6      |
!   19-20-21-22-23-24-25-26-27-28
!    |        |        |        |
!   15       16       17       18
!    |        |        |        |
!   11       12       13       14
!    | 1      | 2      | 3      |
!    1--2--3--4--5--6--7--8--9-10
!
!  Modified:
!
!    07 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer MAXELEM, the maximum number of elements for which
!    storage is provided, which must be at least NELEMX * NELEMY.
!
!    Input, integer NELEMX, NELEMY, the number of quadrilaterals along the
!    X and Y directions.  The number of elements generated will be
!    NELEMX * NELEMY.
!
!    Output, integer NODES(12,MAXELEM); NODES(I,J) contains the index
!    of the I-th node of the J-th element.
!
  integer maxelem
!
  integer base
  integer ielem
  integer i
  integer j
  integer nelem
  integer nelemx
  integer nelemy
  integer nodes(12,maxelem)
!
  nelem = nelemx * nelemy
  if ( nelem > maxelem ) then
    write ( *, * ) ' '
    write ( *, * ) 'GRID_Q12 - Fatal error!'
    write ( *, * ) '  Not enough storage for NODE array.'
    write ( *, * ) '  Increase MAXELEM to ', nelem
    stop
  end if
  ielem = 0
  do j = 1, nelemy
    do i = 1, nelemx
      ielem = ielem + 1
      base = ( j - 1 )  * ( 5 * nelemx + 3 ) + 1
      nodes(1,ielem) =  base + ( i - 1 ) * 3
      nodes(2,ielem) =  base + ( i - 1 ) * 3 + 1
      nodes(3,ielem) =  base + ( i - 1 ) * 3 + 2
      nodes(4,ielem) =  base + ( i - 1 ) * 3 + 3
      nodes(5,ielem) =  base + 3 * nelemx + i
      nodes(6,ielem) =  base + 3 * nelemx + i + 1
      nodes(7,ielem) =  base + 4 * nelemx + i + 1
      nodes(8,ielem) =  base + 4 * nelemx + i + 2
      nodes(9,ielem) =  base + 5 * nelemx + 3 * i
      nodes(10,ielem) = base + 5 * nelemx + 3 * i + 1
      nodes(11,ielem) = base + 5 * nelemx + 3 * i + 2
      nodes(12,ielem) = base + 5 * nelemx + 3 * i + 3
    end do
  end do
  return
end
subroutine grid_q16 ( maxelem, nelemx, nelemy, nodes )
!
!*******************************************************************************
!
!! GRID_Q16 produces a grid of 16 node quadrilaterals.
!
!
!  Example:
!
!    Input:
!
!      NELEMX = 2, NELEMY = 2
!
!    Output:
!
!      NODES = 
!         1,  2,  3,  4,  8,  9, 10, 11, 15, 16, 17, 18, 22, 23, 24, 25;
!         4,  5,  6,  7, 11, 12, 13, 14, 18, 19, 20, 21, 25, 26, 27, 28;
!        22, 23, 24, 25, 29, 30, 31, 32, 36, 37, 38, 39, 43, 44, 45, 46;
!        25, 26, 27, 28, 32, 33, 34, 35, 39, 40, 41, 42, 46, 47, 48, 49. 
!        
!
!  Diagram:
!
!   43-44-45-46-47-48-49
!    |        |        |
!    |        |        |
!   36 37 38 39 40 41 42
!    |        |        |
!    |        |        |
!   29 30 31 32 33 34 35
!    |        |        |
!    | 3      | 4      |
!   22-23-24-25-26-27-28
!    |        |        |
!    |        |        |
!   15 16 17 18 19 20 21
!    |        |        |
!    |        |        |
!    8  9 10 11 12 13 14
!    |        |        |
!    | 1      | 2      |
!    1--2--3--4--5--6--7
!
!  Modified:
!
!    08 December 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer MAXELEM, the maximum number of elements for which
!    storage is provided, which must be at least NELEMX * NELEMY.
!
!    Input, integer NELEMX, NELEMY, the number of triangles along the
!    X and Y directions.  The number of elements generated will be
!    NELEMX * NELEMY.
!
!    Output, integer NODES(16,MAXELEM); NODES(I,J) contains the index
!    of the I-th node of the J-th element.  
!
  integer maxelem
!
  integer base
  integer ielem
  integer i
  integer j
  integer nelem
  integer nelemx
  integer nelemy
  integer nodes(16,maxelem)
!
  nelem = nelemx * nelemy
  if ( nelem > maxelem ) then
    write ( *, * ) ' '
    write ( *, * ) 'GRID_Q16 - Fatal error!'
    write ( *, * ) '  Not enough storage for NODE array.'
    write ( *, * ) '  Increase MAXELEM to ', nelem
    stop
  end if
  ielem = 0
 
  do j = 1, nelemy
    do i = 1, nelemx
      base = ( j - 1 ) * 3 * ( 3 * nelemx + 1 ) + 3 * i - 2
      ielem = ielem + 1
      nodes( 1,ielem) = base
      nodes( 2,ielem) = base                          + 1
      nodes( 3,ielem) = base                          + 2
      nodes( 4,ielem) = base                          + 3
      nodes( 5,ielem) = base +     ( 3 * nelemx + 1 )
      nodes( 6,ielem) = base +     ( 3 * nelemx + 1 ) + 1
      nodes( 7,ielem) = base +     ( 3 * nelemx + 1 ) + 2
      nodes( 8,ielem) = base +     ( 3 * nelemx + 1 ) + 3
      nodes( 9,ielem) = base + 2 * ( 3 * nelemx + 1 )
      nodes(10,ielem) = base + 2 * ( 3 * nelemx + 1 ) + 1
      nodes(11,ielem) = base + 2 * ( 3 * nelemx + 1 ) + 2
      nodes(12,ielem) = base + 2 * ( 3 * nelemx + 1 ) + 3
      nodes(13,ielem) = base + 3 * ( 3 * nelemx + 1 )
      nodes(14,ielem) = base + 3 * ( 3 * nelemx + 1 ) + 1
      nodes(15,ielem) = base + 3 * ( 3 * nelemx + 1 ) + 2
      nodes(16,ielem) = base + 3 * ( 3 * nelemx + 1 ) + 3
    end do
  end do
  return
end
subroutine grid_ql ( maxelem, nelemx, nelemy, nodes )
!
!*******************************************************************************
!
!! GRID_QL produces a grid of 6 node quadratics/linears.
!
!
!  Example:
!
!    Input:
!
!      NELEMX = 3, NELEMY = 2
!
!    Output:
!
!      NODES = 
!         1,  2,  3,  8,  9, 10;
!         3,  4,  5, 10, 11, 12;
!         5,  6,  7, 12, 13, 14;
!         8,  9, 10, 15, 16, 17;
!        10, 11, 12, 17, 18, 19;
!        12, 13, 14, 19, 20, 21.
!
!  Diagram:
!
!   15---16---17---18---19---20---21
!    |         |         |         |
!    |         |         |         |
!    |    4    |    5    |    6    |
!    |         |         |         |
!    |         |         |         |
!    8----9---10---11---12---13---14
!    |         |         |         |
!    |         |         |         |
!    |    1    |    2    |    3    |
!    |         |         |         |
!    |         |         |         |
!    1----2----3----4----5----6----7
!
!  Modified:
!
!    05 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer MAXELEM, the maximum number of elements for which
!    storage is provided, which must be at least NELEMX * NELEMY.
!
!    Input, integer NELEMX, NELEMY, the number of quadrilaterals along the
!    X and Y directions.  The number of elements generated will be
!    NELEMX * NELEMY.  X will the the "quadratic direction", and
!    Y will be the "linear direction".
!
!    Output, integer NODES(6,MAXELEM); NODES(I,J) contains the index
!    of the I-th node of the J-th element.  
!
  integer maxelem
!
  integer base
  integer ielem
  integer i
  integer j
  integer nelem
  integer nelemx
  integer nelemy
  integer nodes(6,maxelem)
!
  nelem = nelemx * nelemy
  if ( nelem > maxelem ) then
    write ( *, * ) ' '
    write ( *, * ) 'GRID_QL - Fatal error!'
    write ( *, * ) '  Not enough storage for NODE array.'
    write ( *, * ) '  Increase MAXELEM to ', nelem
    stop
  end if
  ielem = 0
 
  do j = 1, nelemy
    do i = 1, nelemx
      ielem = ielem + 1
      base = ( j - 1 )  * ( 2 * nelemx + 1 ) + 2 * i - 1
      nodes(1,ielem) = base
      nodes(2,ielem) = base + 1
      nodes(3,ielem) = base + 2
      nodes(4,ielem) = base + ( 2 * nelemx + 1 )
      nodes(5,ielem) = base + ( 2 * nelemx + 1 ) + 1
      nodes(6,ielem) = base + ( 2 * nelemx + 1 ) + 2

    end do
  end do
  return
end
subroutine grid_t3 ( maxelem, nelemx, nelemy, nodes )
!
!*******************************************************************************
!
!! GRID_T3 produces a grid of pairs of 3 node triangles.
!
!
!  Example:
!
!    Input:
!
!      NELEMX = 3, NELEMY = 2
!
!    Output:
!
!      NODES = 
!         1,  2,  5;
!         6,  5,  2;
!         2,  3,  6;
!         7,  6,  3;
!         3,  4,  7;
!         8,  7,  4;
!         5,  6,  9;
!        10,  9,  6;
!         6,  7, 10;
!        11, 10,  7;
!         7,  8, 11;
!        12, 11,  8.
!
!  Diagram:
!
!    9---10---11---12
!    |\ 8 |\10 |\12 |
!    | \  | \  | \  |
!    |  \ |  \ |  \ |
!    |  7\|  9\| 11\|
!    5----6----7----8
!    |\ 2 |\ 4 |\ 6 |
!    | \  | \  | \  |
!    |  \ |  \ |  \ |
!    |  1\|  3\|  5\|
!    1----2----3----4
!
!  Modified:
!
!    05 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer MAXELEM, the maximum number of elements for which
!    storage is provided, which must be at least 2 * NELEMX * NELEMY.
!
!    Input, integer NELEMX, NELEMY, the number of triangles along the
!    X and Y directions.  The number of elements generated will be
!    2 * NELEMX * NELEMY.
!
!    Output, integer NODES(3,MAXELEM); NODES(I,J) contains the index
!    of the I-th node of the J-th element.  
!
  integer maxelem
!
  integer ielem
  integer i
  integer j
  integer nelem
  integer nelemx
  integer nelemy
  integer nodes(3,maxelem)
!
  nelem = 2 * nelemx * nelemy
  if ( nelem > maxelem ) then
    write ( *, * ) ' '
    write ( *, * ) 'GRID_T3 - Fatal error!'
    write ( *, * ) '  Not enough storage for NODE array.'
    write ( *, * ) '  Increase MAXELEM to ', nelem
    stop
  end if
  ielem = 0
 
  do j = 1, nelemy
    do i = 1, nelemx
      ielem = ielem + 1
      nodes(1,ielem) = ( j - 1 ) * ( nelemx + 1 ) + i
      nodes(2,ielem) = ( j - 1 ) * ( nelemx + 1 ) + i + 1
      nodes(3,ielem) =   j       * ( nelemx + 1 ) + i
      ielem = ielem + 1
      nodes(1,ielem) =   j       * ( nelemx + 1 ) + i + 1
      nodes(2,ielem) =   j       * ( nelemx + 1 ) + i
      nodes(3,ielem) = ( j - 1 ) * ( nelemx + 1 ) + i + 1
    end do
  end do
  return
end
subroutine grid_t6 ( maxelem, nelemx, nelemy, nodes )
!
!*******************************************************************************
!
!! GRID_T6 produces a grid of pairs of 6 node triangles.
!
!
!  Example:
!
!    Input:
!
!      NELEMX = 3, NELEMY = 2
!
!    Output:
!
!      NODES = 
!         3, 15,  1,  2,  9,  8;
!        15,  3, 17, 16,  9, 10;
!         5, 17,  3   4, 11, 10;
!        17,  5, 19, 18, 11, 12;
!         7, 19,  5,  6, 13, 12;
!        19,  7, 21, 20, 13, 14;
!        17, 29, 15, 16, 23, 22;
!        29, 17, 31, 30, 23, 24;
!        19, 31, 17, 18, 25, 24;
!        31, 19, 33, 32, 25, 26;
!        21, 33, 19, 20, 27, 26;
!        33, 21, 35, 34, 27, 28.
!
!  Diagram:
!
!   29-30-31-32-33-34-35
!    |\ 8  |\10  |\12  |
!    | \   | \   | \   |
!   22 23 24 25 26 27 28
!    |   \ |   \ |   \ |
!    |  7 \|  9 \| 11 \|
!   15-16-17-18-19-20-21
!    |\ 2  |\ 4  |\ 6  |
!    | \   | \   | \   |
!    8  9 10 11 12 13 14
!    |   \ |   \ |   \ |
!    |  1 \|  3 \|  5 \|
!    1--2--3--4--5--6--7
!
!  Modified:
!
!    12 October 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer MAXELEM, the maximum number of elements for which
!    storage is provided, which must be at least 2 * NELEMX * NELEMY.
!
!    Input, integer NELEMX, NELEMY, the number of triangles along the
!    X and Y directions.  The number of elements generated will be
!    2 * NELEMX * NELEMY.
!
!    Output, integer NODES(6,MAXELEM); NODES(I,J) contains the index
!    of the I-th node of the J-th element.  
!
  integer maxelem
!
  integer base
  integer ielem
  integer i
  integer j
  integer nelem
  integer nelemx
  integer nelemy
  integer nodes(6,maxelem)
!
  nelem = 2 * nelemx * nelemy
  if ( nelem > maxelem ) then
    write ( *, * ) ' '
    write ( *, * ) 'GRID_T6 - Fatal error!'
    write ( *, * ) '  Not enough storage for NODE array.'
    write ( *, * ) '  Increase MAXELEM to ', nelem
    stop
  end if
  ielem = 0
 
  do j = 1, nelemy
    do i = 1, nelemx
      base = ( j - 1 ) * 2 * ( 2 * nelemx + 1 ) + 2 * i - 1
      ielem = ielem + 1
      nodes(1,ielem) = base + 2
      nodes(2,ielem) = base + 2 * ( 2 * nelemx + 1 )
      nodes(3,ielem) = base
      nodes(4,ielem) = base + 1
      nodes(5,ielem) = base +     ( 2 * nelemx + 1 ) + 1
      nodes(6,ielem) = base +     ( 2 * nelemx + 1 )
      ielem = ielem + 1
      nodes(1,ielem) = base + 2 * ( 2 * nelemx + 1 )
      nodes(2,ielem) = base + 2
      nodes(3,ielem) = base + 2 * ( 2 * nelemx + 1 ) + 2
      nodes(4,ielem) = base + 2 * ( 2 * nelemx + 1 ) + 1
      nodes(5,ielem) = base +     ( 2 * nelemx + 1 ) + 1
      nodes(6,ielem) = base +     ( 2 * nelemx + 1 ) + 2
    end do
  end do
  return
end
subroutine grid_t10 ( maxelem, nelemx, nelemy, nodes )
!
!*******************************************************************************
!
!! GRID_T10 produces a grid of pairs of 10 node triangles.
!
!
!  Example:
!
!    Input:
!
!      NELEMX = 2, NELEMY = 2
!
!    Output:
!
!      NODES = 
!         1,  2,  3,  4, 10, 16, 22, 15,  8,  9;
!        25, 24, 23, 22, 16, 10,  4, 11, 18, 17;
!         4,  5,  6,  7, 13, 19, 25, 18, 11, 12;
!        28, 27, 26, 25, 19, 13,  7, 14, 21, 20;
!        22, 23, 24, 25, 31, 37, 43, 36, 29, 30;
!        46, 45, 44, 43, 37, 31, 25, 32, 39, 38;
!        25, 26, 27, 28, 34, 40, 46, 39, 31, 33;
!        49, 48, 47, 46, 40, 34, 28, 35, 42, 41.
!        
!
!  Diagram:
!
!   43-44-45-46-47-48-49
!    |\     6 |\     8 |
!    | \      | \      |
!   36 37 38 39 40 41 42
!    |   \    |   \    |
!    |    \   |    \   |
!   29 30 31 32 33 34 35
!    |      \ |      \ |
!    | 5     \| 7     \|
!   22-23-24-25-26-27-28
!    |\     2 |\     4 |
!    | \      | \      |
!   15 16 17 18 19 20 21
!    |   \    |   \    |
!    |    \   |    \   |
!    8  9 10 11 12 13 14
!    |      \ |      \ |
!    | 1     \| 3     \|
!    1--2--3--4--5--6--7
!
!  Modified:
!
!    09 December 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer MAXELEM, the maximum number of elements for which
!    storage is provided, which must be at least 2 * NELEMX * NELEMY.
!
!    Input, integer NELEMX, NELEMY, the number of triangles along the
!    X and Y directions.  The number of elements generated will be
!    2 * NELEMX * NELEMY.
!
!    Output, integer NODES(10,MAXELEM); NODES(I,J) contains the index
!    of the I-th node of the J-th element.  
!
  integer maxelem
!
  integer base
  integer ielem
  integer i
  integer j
  integer nelem
  integer nelemx
  integer nelemy
  integer nodes(10,maxelem)
!
  nelem = 2 * nelemx * nelemy
  if ( nelem > maxelem ) then
    write ( *, * ) ' '
    write ( *, * ) 'GRID_T10 - Fatal error!'
    write ( *, * ) '  Not enough storage for NODE array.'
    write ( *, * ) '  Increase MAXELEM to ', nelem
    stop
  end if
  ielem = 0
 
  do j = 1, nelemy
    do i = 1, nelemx
      base = ( j - 1 ) * 3 * ( 3 * nelemx + 1 ) + 3 * i - 2
      ielem = ielem + 1
      nodes( 1,ielem) = base
      nodes( 2,ielem) = base                          + 1
      nodes( 3,ielem) = base                          + 2
      nodes( 4,ielem) = base                          + 3
      nodes( 5,ielem) = base +     ( 3 * nelemx + 1 ) + 2
      nodes( 6,ielem) = base + 2 * ( 3 * nelemx + 1 ) + 1
      nodes( 7,ielem) = base + 3 * ( 3 * nelemx + 1 )
      nodes( 8,ielem) = base + 2 * ( 3 * nelemx + 1 )
      nodes( 9,ielem) = base +     ( 2 * nelemx + 1 ) + 2
      nodes(10,ielem) = base +     ( 2 * nelemx + 1 ) + 3
      ielem = ielem + 1
      nodes( 1,ielem) = base + 3 * ( 3 * nelemx + 1 ) + 3
      nodes( 2,ielem) = base + 3 * ( 3 * nelemx + 1 ) + 2
      nodes( 3,ielem) = base + 3 * ( 3 * nelemx + 1 ) + 1
      nodes( 4,ielem) = base + 3 * ( 3 * nelemx + 1 )
      nodes( 5,ielem) = base + 2 * ( 3 * nelemx + 1 ) + 1
      nodes( 6,ielem) = base +     ( 3 * nelemx + 1 ) + 2
      nodes( 7,ielem) = base                          + 3
      nodes( 8,ielem) = base +     ( 3 * nelemx + 1 ) + 3
      nodes( 9,ielem) = base + 2 * ( 3 * nelemx + 1 ) + 3
      nodes(10,ielem) = base + 2 * ( 3 * nelemx + 1 ) + 2
    end do
  end do
  return
end
subroutine interp ( code, dtdr, dtds, maxn, n, r, s, t, ubase, u, dudr, duds )
!
!*******************************************************************************
!
!! INTERP interpolates a quantity in an element from basis node values.
!
!
!  Modified:
!
!    09 December 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) CODE, identifies the element.
!    Legal values include 'Q4', 'Q8', 'Q9', 'Q12', 'Q16', 'QL', 
!    'T3', 'T6' and 'T10'.
!
!    Output, real DTDR(MAXN), DTDS(MAXN), the derivatives of the 
!    basis functions at (R,S).
!
!    Input, integer MAXN, the maximum value of N.
!
!    Output, integer N, the order of the element.
!
!    Input, real R, S, the reference coordinates of a point.
!
!    Output, real T(MAXN), the value of the basis functions at (R,S).
!
!    Input, real UBASE(MAXN), the value of the quantity at the basis nodes.
!
!    Output, real U, DUDR, DUDS, the interpolated value of the
!    quantity and its derivatives at the point (R,S).
!
  integer maxn
!
  character ( len = * ) code
  real dtdr(maxn)
  real dtds(maxn)
  real dudr
  real duds
  integer i
  integer n
  real r
  real s
  real t(maxn)
  real u
  real ubase(maxn)
!
  call shape ( code, n, r, s, t, dtdr, dtds )
 
  u = 0.0
  dudr = 0.0
  duds = 0.0
  do i = 1, n
    u = u + ubase(i) * t(i)
    dudr = dudr + ubase(i) * dtdr(i)
    duds = duds + ubase(i) * dtds(i)
  end do
 
  return
end
subroutine map ( code, w, lda, n )
!
!*******************************************************************************
!
!! MAP returns the interpolation matrix for any available element.
!
!
!  Formula:
!
!    Given data Q(J) associated with the nodes, the coefficients of
!    the interpolating polynomial are
!
!      A(I) = W(I,J) * Q(J)
!
!   In other words, if we let PHI(I)(R,S) be the I-th basis function,
!   evaluated at the point (R,S), and we let REXP(I) and SEXP(I)
!   be the exponents of R and S in the I-th associated polynomial,
!   then the interpolating polynomial P(R,S) has two forms:
!
!     P(R,S) =
!
!       = SUM ( I = 1 to N ) Q(I) * PHI(I)(R,S)
!       = SUM ( I = 1 to N ) A(I) * R**REXP(I) * S**SEXP(I)
!
!  Modified:
!
!    04 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) CODE, identifies the element.
!    Legal values include 'Q4', 'Q8', 'Q9', 'Q12', 'Q16', 'QL', 
!    'T3', 'T6' and 'T10'.
!
!    Output, real W(LDA,N), the interpolation matrix.
!
!    Input, integer LDA, the leading dimension of the W array.
!    LDA must be at least N.
!
!    Output, integer N, the order of the element.
!
  integer, parameter :: maxn = 20
!
  integer lda
  integer n
!
  real area
  character ( len = * ) code
  integer i
  integer info
  integer ipivot(maxn)
  integer j
  real r(maxn)
  integer rexp(maxn)
  real s(maxn)
  integer sexp(maxn)
  real w(lda,n)
  real work(maxn)
!
!  Get the (R,S) location of the nodes.
!
  call node ( code, n, r, s, area )
  if ( lda < n ) then
    write ( *, * ) ' '
    write ( *, * ) 'MAP - Fatal error!'
    write ( *, * ) '  LDA < N.'
    stop
  end if
  if ( maxn < n ) then
    write ( *, * ) ' '
    write ( *, * ) 'MAP - Fatal error!'
    write ( *, * ) '  Internal parameter MAXN exceeded.'
    write ( *, * ) '  MAXN < N.'
    stop
  end if
!
!  Get the associated polynomials.
!
  call poly ( code, n, rexp, sexp )
!
!  Set up the Vandermonde matrix.
!
  do i = 1, n
    do j = 1, n
      w(i,j) = r(i)**rexp(j) * s(i)**sexp(j)
    end do
  end do
!
!  Factor the Vandermonde matrix.
!
  call sge_fa ( w, lda, n, ipivot, info )
print *,'-----------------------------------------------------------------------'
  print *,'w=',w
  print *,'lda=',lda
  print *,'n=',n
  print *,'ipivot=',ipivot
  print *,'info=',info
  if ( info /= 0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'MAP - Fatal error!'
    write ( *, * ) '  The Vandermonde matrix is singular.'
    stop
  end if
!
!  Invert the Vandermonde matrix.
!
  call sge_inv ( w, lda, n, ipivot, work )
  return
end
subroutine map_test ( code )
!
!*******************************************************************************
!
!! MAP_TEST tests the map routines.
!
!
!  Modified:
!
!    05 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = 4 ) CODE, the code for the element.
!    Legal values include 'Q4', 'Q8', 'Q9', 'Q12', 'Q16', 'QL', 
!    'T3', 'T6' and 'T10'.
!
  integer, parameter :: maxn = 20
  integer, parameter :: lda = maxn
!
  character ( len = * ) code
  integer order_code
  integer i
  integer j
  integer n
  real w(maxn,maxn)
!
  write ( *, * ) ' '
  write ( *, * ) 'MAP_TEST'
  write ( *, '(a,a)' ) '  The interpolation matrix for element ', code
  write ( *, * ) ' '
  n = order_code ( code )
  call map ( code, w, lda, n )
  do i = 1, n
    write ( *, '(7f9.3)' ) ( w(i,j), j = 1, n )
  end do
  return
end
subroutine node ( code, n, r, s, area )
!
!*******************************************************************************
!
!! NODE returns the basis nodes for any available element.
!
!
!  Modified:
!
!    04 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) CODE, identifies the element desired.
!    Legal values include 'Q4', 'Q8', 'Q9', 'Q12', 'Q16', 'QL', 
!    'T3', 'T6' and 'T10'.
!
!    Output, integer N, the number of nodes in the element.
!
!    Output, real R(N), S(N), the coordinates of the basis nodes.
!
!    Output, real AREA, the area of the element.
!
  real area
  character ( len = * ) code
  integer n
  real r(*)
  real s(*)
!
  if ( code == 'Q4' ) then
    call node_q4 ( n, r, s, area )
  else if ( code == 'Q8' ) then
    call node_q8 ( n, r, s, area )
  else if ( code == 'Q9' ) then
    call node_q9 ( n, r, s, area )
  else if ( code == 'Q12' ) then
    call node_q12 ( n, r, s, area )
  else if ( code == 'Q16' ) then
    call node_q16 ( n, r, s, area )
  else if ( code == 'QL' ) then
    call node_ql ( n, r, s, area )
  else if ( code == 'T3' ) then
    call node_t3 ( n, r, s, area )
  else if ( code == 'T6' ) then
    call node_t6 ( n, r, s, area )
  else if ( code == 'T10' ) then
    call node_t10 ( n, r, s, area )
  else
    write ( *, * ) ' '
    write ( *, * ) 'NODE - Fatal error!'
    write ( *, '(a,a)' ) '  Illegal value of CODE = ', code
    stop
  end if
  return
end
subroutine node_q4 ( n, r, s, area )
!
!*******************************************************************************
!
!! NODE_Q4 returns the basis nodes for a 4 node quadrilateral.
!
!
!  Diagram:
!
!    |
!    1  3-------4
!    |  |       |
!    |  |       |
!    S  |       |
!    |  |       |
!    |  |       |
!    0  1-------2
!    |
!    +--0---R---1-->
!
!  Modified:
!
!    05 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, integer N, the number of nodes in the element.
!
!    Output, real R(4), S(4), the coordinates of the basis nodes.
!
!    Output, real AREA, the area of the element.
!
  real area
  integer n
  real r(4)
  real s(4)
!
  n = 4
  r(1) = 0.0
  s(1) = 0.0
  r(2) = 1.0
  s(2) = 0.0
  r(3) = 0.0
  s(3) = 1.0
  r(4) = 1.0
  s(4) = 1.0
  area = 1.0
  return
end
subroutine node_q8 ( n, r, s, area )
!
!*******************************************************************************
!
!! NODE_Q8 returns the basis nodes for an 8 node quadrilateral.
!
!
!  Comment:
!
!    This element is known as the quadratic "serendipity" element.
!
!  Diagram:
!
!    |
!    1  6---7---8
!    |  |       |
!    |  |       |
!    S  4       5
!    |  |       |
!    |  |       |
!    0  1---2---3
!    |
!    +--0---R---1-->
!
!  Modified:
!
!    05 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, integer N, the number of nodes in the element.
!
!    Output, real R(8), S(8), the coordinates of the basis nodes.
!
!    Output, real AREA, the area of the element.
!
  real area
  integer n
  real r(8)
  real s(8)
!
  n = 8
  r(1) = 0.0
  s(1) = 0.0
  r(2) = 0.5
  s(2) = 0.0
  r(3) = 1.0
  s(3) = 0.0
  r(4) = 0.0
  s(4) = 0.5
  r(5) = 1.0
  s(5) = 0.5
  r(6) = 0.0
  s(6) = 1.0
  r(7) = 0.5
  s(7) = 1.0
  r(8) = 1.0
  s(8) = 1.0
  area = 1.0
  return
end
subroutine node_q9 ( n, r, s, area )
!
!*******************************************************************************
!
!! NODE_Q9 returns the basis nodes for a 9 node quadrilateral.
!
!
!  Diagram:
!
!    |
!    1  7---8---9
!    |  |   :   |
!    |  |   :   |
!    S  4...5...6
!    |  |   :   |
!    |  |   :   |
!    0  1---2---3
!    |
!    +--0--R--1-->
!
!  Modified:
!
!    05 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, integer N, the number of nodes in the element.
!
!    Input, real R(9), S(9), the coordinates of the basis nodes.
!
!    Output, real AREA, the area of the element.
!
  real area
  integer n
  real r(9)
  real s(9)
!
  n = 9
  r(1) = 0.0
  s(1) = 0.0
  r(2) = 0.5
  s(2) = 0.0
  r(3) = 1.0
  s(3) = 0.0
  r(4) = 0.0
  s(4) = 0.5
  r(5) = 0.5
  s(5) = 0.5
  r(6) = 1.0
  s(6) = 0.5
  r(7) = 0.0
  s(7) = 1.0
  r(8) = 0.5
  s(8) = 1.0
  r(9) = 1.0
  s(9) = 1.0
  area = 1.0
  return
end
subroutine node_q12 ( n, r, s, area )
!
!*******************************************************************************
!
!! NODE_Q12 returns the basis nodes for a 12 node quadrilateral.
!
!
!  Comment:
!
!    This element is known as the cubic "serendipity" element.
!
!  Diagram:
!
!    |
!    1  9-10-11-12
!    |  |        |
!    |  7        8
!    S  |        |
!    |  5        6
!    |  |        |
!    0  1--2--3--4
!    |
!    +--0---R---1-->
!
!  Modified:
!
!    04 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, integer N, the number of nodes in the element.
!
!    Output, real R(12), S(12), the coordinates of the basis nodes.
!
!    Output, real AREA, the area of the element.
!
  real a
  real area
  real b
  real c
  real d
  integer n
  real r(12)
  real s(12)
!
  n = 12
  a = 0.0
  b = 1.0 / 3.0
  c = 2.0 / 3.0
  d = 1.0
  r(1) = a
  s(1) = a
  r(2) = b
  s(2) = a
  r(3) = c
  s(3) = a
  r(4) = d
  s(4) = a

  r(5) = a
  s(5) = b
  r(6) = d
  s(6) = b
  r(7) = a
  s(7) = c
  r(8) = d
  s(8) = c
  r(9) = a
  s(9) = d
  r(10) = b
  s(10) = d
  r(11) = c
  s(11) = d
  r(12) = d
  s(12) = d
  area = 1.0
  return
end
subroutine node_q16 ( n, r, s, area )
!
!*******************************************************************************
!
!! NODE_Q16 returns the basis nodes for a 16 node quadrilateral.
!
!
!  Diagram:
!
!    |
!    1 13--14--15--16
!    |  |   :   :   |
!    |  |   :   :   |
!    |  9..10..11..12
!    S  |   :   :   |
!    |  |   :   :   |
!    |  5...6...7...8
!    |  |   :   :   |
!    |  |   :   :   |  
!    0  1---2---3---4
!    |
!    +--0-----R-----1-->
!
!  Modified:
!
!    14 October 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, integer N, the number of nodes in the element.
!
!    Output, real R(16), S(16), the coordinates of the basis nodes.
!
!    Output, real AREA, the area of the element.
!
  real area
  integer i
  integer j
  integer k
  integer n
  real r(16)
  real s(16)
!
  n = 16
  k = 0
  do i = 0, 3
    do j = 0, 3
      k = k + 1
      r(k) = real ( j ) / 3.0 
      s(k) = real ( i ) / 3.0
    end do
  end do
  area = 1.0
  return
end
subroutine node_ql ( n, r, s, area )
!
!*******************************************************************************
!
!! NODE_QL returns the basis nodes for a quadratic/linear.
!
!
!  Diagram:
!
!    |
!    1  4---5---6
!    |  |       |
!    |  |       |
!    S  |       |
!    |  |       |
!    |  |       |
!    0  1---2---3
!    |
!    +--0---R---1-->
!
!  Modified:
!
!    05 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, integer N, the number of nodes in the element.
!
!    Output, real R(6), S(6), the coordinates of the basis nodes.
!
!    Output, real AREA, the area of the element.
!
  real area
  integer n
  real r(6)
  real s(6)
!
  n = 6
  r(1) = 0.0
  s(1) = 0.0
  r(2) = 0.5
  s(2) = 0.0
  r(3) = 1.0
  s(3) = 0.0
  r(4) = 0.0
  s(4) = 1.0
  r(5) = 0.5
  s(5) = 1.0
  r(6) = 1.0
  s(6) = 1.0
  area = 1.0
  return
end
subroutine node_t3 ( n, r, s, area )
!
!*******************************************************************************
!
!! NODE_T3 returns the basis nodes for the 3 node triangle.
!
!
!  Diagram:
!
!    |
!    1  3
!    |  |\
!    |  | \
!    S  |  \
!    |  |   \
!    |  |    \
!    0  1-----2
!    |
!    +--0--R--1-->
!
!  Modified:
!
!    05 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, integer N, the number of nodes in the element.
!
!    Output, real R(3), S(3), the coordinates of the basis nodes.
!
!    Output, real AREA, the area of the element.
!
  real area
  integer n
  real r(3)
  real s(3)
  n = 3
  r(1) = 0.0
  s(1) = 0.0
  r(2) = 1.0
  s(2) = 0.0
  r(3) = 0.0
  s(3) = 1.0
  area = 0.5
  return
end
subroutine node_t6 ( n, r, s, area )
!
!*******************************************************************************
!
!! NODE_T6 returns the basis nodes for a 6 node triangle.
!
!
!  Diagram:
!
!    |
!    1  6
!    |  |\
!    |  | \
!    S  4  5
!    |  |   \
!    |  |    \
!    0  1--2--3
!    |
!    +--0--R--1-->
!
!  Modified:
!
!    05 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, integer N, the number of nodes in the element.
!
!    Output, real R(6), S(6), the coordinates of the basis nodes.
!
!    Output, real AREA, the area of the element.
!
  real area
  integer n
  real r(6)
  real s(6)
!
  n = 6
  r(1) = 0.0
  s(1) = 0.0
  r(2) = 0.5
  s(2) = 0.0
  r(3) = 1.0
  s(3) = 0.0
  r(4) = 0.0
  s(4) = 0.5
  r(5) = 0.5
  s(5) = 0.5
  r(6) = 0.0
  s(6) = 1.0
  area = 0.5
  return
end
subroutine node_t10 ( n, r, s, area )
!
!*******************************************************************************
!
!! NODE_T10 returns the basis nodes for a 10 node triangle.
!
!
!  Diagram:
!
!    |
!    1  10
!    |  |\
!    |  | \
!    |  8  9
!    |  |   \
!    S  |    \
!    |  5  6  7
!    |  |      \
!    |  |       \
!    0  1--2--3--4
!    |
!    +--0----R---1-->
!
!  Modified:
!
!    05 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, integer N, the number of nodes in the element.
!
!    Output, real R(10), S(10), the coordinates of the basis nodes.
!
!    Output, real AREA, the area of the element.
!
  real area
  integer n
  real r(10)
  real s(10)
!
  n = 10
  r(1) = 0.0
  s(1) = 0.0
  r(2) = 1.0 / 3.0
  s(2) = 0.0
  r(3) = 2.0 / 3.0
  s(3) = 0.0
  r(4) = 1.0
  s(4) = 0.0
  r(5) = 0.0
  s(5) = 1.0 / 3.0
  r(6) = 1.0 / 3.0
  s(6) = 1.0 / 3.0
  r(7) = 2.0 / 3.0
  s(7) = 1.0 / 3.0
  r(8) = 0.0
  s(8) = 2.0 / 3.0
  r(9) = 1.0 / 3.0
  s(9) = 2.0 / 3.0
  r(10) = 0.0
  s(10) = 1.0
  area = 0.5
  return
end
subroutine poly ( code, n, rexp, sexp )
!
!*******************************************************************************
!
!! POLY returns the polynomials associated with any available element.
!
!
!  Modified:
!
!    04 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) CODE, identifies the element desired.
!    Legal values include 'Q4', 'Q8', 'Q9', 'Q12', 'Q16', 'QL', 'T3', 
!    'T6' and 'T10'.
!
!    Output, integer N, the number of polynomials.
!
!    Output, integer REXP(N), SEXP(N), the powers of R and S associated
!    with each polynomial.
!
  character ( len = * ) code
  integer n
  integer rexp(*)
  integer sexp(*)
!
  if ( code == 'Q4' ) then
    call poly_q4 ( n, rexp, sexp )
  else if ( code == 'Q8' ) then
    call poly_q8 ( n, rexp, sexp )
  else if ( code == 'Q9' ) then
    call poly_q9 ( n, rexp, sexp )
  else if ( code == 'Q12' ) then
    call poly_q12 ( n, rexp, sexp )
  else if ( code == 'Q16' ) then
    call poly_q16 ( n, rexp, sexp )
  else if ( code == 'QL' ) then
    call poly_ql ( n, rexp, sexp )
  else if ( code == 'T3' ) then
    call poly_t3 ( n, rexp, sexp )
  else if ( code == 'T6' ) then
    call poly_t6 ( n, rexp, sexp )
  else if ( code == 'T10' ) then
    call poly_t10 ( n, rexp, sexp )
  else
    write ( *, * ) ' '
    write ( *, * ) 'POLY - Fatal error!'
    write ( *, '(a,a)' ) '  Illegal value of CODE = ', code
    stop
  end if
  return
end
subroutine poly_q4 ( n, rexp, sexp )
!
!*******************************************************************************
!
!! POLY_Q4 returns the polynomials associated with a 4 node quadrilateral.
!
!
!  Diagram:
!
!    |
!    1  3-----4
!    |  |     |
!    |  |     |
!    S  |     |
!    |  |     |
!    |  |     |
!    0  1-----2
!    |
!    +--0--R--1-->
!
!  Formula:
!
!    Given coefficients A(I), the polynomial interpolant at (R,S) is
!
!      P(R,S) = SUM ( I = 1 to N ) A(I) * R**REXP(I) * S**SEXP(I) 
!
!  Modified:
!
!    04 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, integer N, the number of polynomials.
!
!    Output, integer REXP(N), SEXP(N), the powers of R and S associated
!    with each polynomial.
!
  integer, parameter :: n_internal = 4
!
  integer n
  integer rexp(n_internal)
  integer sexp(n_internal)
!
  n = n_internal
  rexp(1) = 0
  sexp(1) = 0
  rexp(2) = 0
  sexp(2) = 1
  rexp(3) = 1
  sexp(3) = 0
  rexp(4) = 1
  sexp(4) = 1
  return
end
subroutine poly_q8 ( n, rexp, sexp )
!
!*******************************************************************************
!
!! POLY_Q8 returns the polynomials associated with an 8 node quadrilateral.
!
!
!  Diagram:
!
!    |
!    1  6---7---8
!    |  |       |
!    |  |       |
!    S  4       5
!    |  |       |
!    |  |       |
!    0  1---2---3
!    |
!    +--0--R--1-->
!
!  Formula:
!
!    Given coefficients A(I), the polynomial interpolant at (R,S) is
!
!      P(R,S) = SUM ( I = 1 to N ) A(I) * R**REXP(I) * S**SEXP(I) 
!
!  Modified:
!
!    04 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, integer N, the number of polynomials.
!
!    Output, integer REXP(N), SEXP(N), the powers of R and S associated
!    with each polynomial.
!
  integer, parameter :: n_internal = 8
!
  integer n
  integer rexp(n_internal)
  integer sexp(n_internal)
!
  n = n_internal
  rexp(1) = 0
  sexp(1) = 0
  rexp(2) = 0
  sexp(2) = 1
  rexp(3) = 0
  sexp(3) = 2
  rexp(4) = 1
  sexp(4) = 0
  rexp(5) = 1
  sexp(5) = 1
  rexp(6) = 1
  sexp(6) = 2
  rexp(7) = 2
  sexp(7) = 0
  rexp(8) = 2
  sexp(8) = 1
  return
end
subroutine poly_q9 ( n, rexp, sexp )
!
!*******************************************************************************
!
!! POLY_Q9 returns the polynomials associated with a 9 node quadrilateral.
!
!
!  Diagram:
!
!    |
!    1  7---8---9
!    |  |   :   |
!    |  |   :   |
!    S  4...5...6
!    |  |   :   |
!    |  |   :   |
!    0  1---2---3
!    |
!    +--0--R--1-->
!
!  Formula:
!
!    Given coefficients A(I), the polynomial interpolant at (R,S) is
!
!      P(R,S) = SUM ( I = 1 to N ) A(I) * R**REXP(I) * S**SEXP(I) 
!
!  Modified:
!
!    05 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, integer N, the number of polynomials.
!
!    Output, integer REXP(N), SEXP(N), the powers of R and S associated
!    with each polynomial.
!
  integer, parameter :: n_internal = 9
!
  integer n
  integer rexp(n_internal)
  integer sexp(n_internal)
!
  n = n_internal
  rexp(1) = 0
  sexp(1) = 0
  rexp(2) = 0
  sexp(2) = 1
  rexp(3) = 0
  sexp(3) = 2
  rexp(4) = 1
  sexp(4) = 0
  rexp(5) = 1
  sexp(5) = 1
  rexp(6) = 1
  sexp(6) = 2
  rexp(7) = 2
  sexp(7) = 0
  rexp(8) = 2
  sexp(8) = 1
  rexp(9) = 2
  sexp(9) = 2
  return
end
subroutine poly_q12 ( n, rexp, sexp )
!
!*******************************************************************************
!
!! POLY_Q12 returns the polynomials associated with a 12 node quadrilateral.
!
!
!  Diagram:
!
!    |
!    1  9--10--11--12
!    |  |           |
!    |  |           |
!    |  7           8
!    S  |           |
!    |  |           |
!    |  5           6
!    |  |           |
!    |  |           |  
!    0  1---2---3---4
!    |
!    +--0-----R-----1-->
!
!  Formula:
!
!    Given coefficients A(I), the polynomial interpolant at (R,S) is
!
!      P(R,S) = SUM ( I = 1 to N ) A(I) * R**REXP(I) * S**SEXP(I) 
!
!  Modified:
!
!    04 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, integer N, the number of polynomials.
!
!    Output, integer REXP(N), SEXP(N), the powers of R and S associated
!    with each polynomial.
!
  integer, parameter :: n_internal = 12
!
  integer n
  integer rexp(n_internal)
  integer sexp(n_internal)
!
  n = n_internal
  rexp(1) = 0
  sexp(1) = 0
  rexp(2) = 0
  sexp(2) = 1
  rexp(3) = 0
  sexp(3) = 2
  rexp(4) = 0
  sexp(4) = 3
  rexp(5) = 1
  sexp(5) = 0
  rexp(6) = 1
  sexp(6) = 1
  rexp(7) = 1
  sexp(7) = 2
  rexp(8) = 1
  sexp(8) = 3
  rexp(9) = 2
  sexp(9) = 0
  rexp(10) = 2
  sexp(10) = 1
  rexp(11) = 3
  sexp(11) = 0
  rexp(12) = 3
  sexp(12) = 1
  return
end
subroutine poly_q16 ( n, rexp, sexp )
!
!*******************************************************************************
!
!! POLY_Q16 returns the polynomials associated with a 16 node quadrilateral.
!
!
!  Diagram:
!
!    |
!    1 13--14--15--16
!    |  |   :   :   |
!    |  |   :   :   |
!    |  9..10..11..12
!    S  |   :   :   |
!    |  |   :   :   |
!    |  5...6...7...8
!    |  |   :   :   |
!    |  |   :   :   |  
!    0  1---2---3---4
!    |
!    +--0-----R-----1-->
!
!  Formula:
!
!    Given coefficients A(I), the polynomial interpolant at (R,S) is
!
!      P(R,S) = SUM ( I = 1 to N ) A(I) * R**REXP(I) * S**SEXP(I) 
!
!  Modified:
!
!    04 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, integer N, the number of polynomials.
!
!    Output, integer REXP(N), SEXP(N), the powers of R and S associated
!    with each polynomial.
!
  integer, parameter :: n_internal = 16
!
  integer n
  integer rexp(n_internal)
  integer sexp(n_internal)
!
  n = n_internal
  rexp(1) = 0
  sexp(1) = 0
  rexp(2) = 0
  sexp(2) = 1
  rexp(3) = 0
  sexp(3) = 2
  rexp(4) = 0
  sexp(4) = 3
  rexp(5) = 1
  sexp(5) = 0
  rexp(6) = 1
  sexp(6) = 1
  rexp(7) = 1
  sexp(7) = 2
  rexp(8) = 1
  sexp(8) = 3
  rexp(9) = 2
  sexp(9) = 0
  rexp(10) = 2
  sexp(10) = 1
  rexp(11) = 2
  sexp(11) = 2
  rexp(12) = 2
  sexp(12) = 3
  rexp(13) = 3
  sexp(13) = 0
  rexp(14) = 3
  sexp(14) = 1
  rexp(15) = 3
  sexp(15) = 2
  rexp(16) = 3
  sexp(16) = 3
  return
end
subroutine poly_ql ( n, rexp, sexp )
!
!*******************************************************************************
!
!! POLY_QL returns the polynomials for a quadratic/linear quadrilateral.
!
!
!  Diagram:
!
!    |
!    1  4---5---6
!    |  |       |
!    |  |       |
!    S  |       |
!    |  |       |
!    |  |       |
!    0  1---2---3
!    |
!    +--0---R---1-->
!
!  Formula:
!
!    Given coefficients A(I), the polynomial interpolant at (R,S) is
!
!      P(R,S) = SUM ( I = 1 to N ) A(I) * R**REXP(I) * S**SEXP(I) 
!
!  Modified:
!
!    04 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, integer N, the number of polynomials.
!
!    Output, integer REXP(N), SEXP(N), the powers of R and S associated
!    with each polynomial.
!
  integer, parameter :: n_internal = 6
!
  integer n
  integer rexp(n_internal)
  integer sexp(n_internal)
!
  n = n_internal
  rexp(1) = 0
  sexp(1) = 0
  rexp(2) = 0
  sexp(2) = 1
  rexp(3) = 1
  sexp(3) = 0
  rexp(4) = 1
  sexp(4) = 1
  rexp(5) = 2
  sexp(5) = 0
  rexp(6) = 2
  sexp(6) = 1
  return
end
subroutine poly_t3 ( n, rexp, sexp )
!
!*******************************************************************************
!
!! POLY_T3 returns the polynomials associated with a 3 node triangle.
!
!
!  Diagram:
!
!    |
!    1  3
!    |  |\
!    |  | \
!    S  |  \
!    |  |   \
!    |  |    \
!    0  1-----2
!    |
!    +--0--R--1-->
!
!  Formula:
!
!    Given coefficients A(I), the polynomial interpolant at (R,S) is
!
!      P(R,S) = SUM ( I = 1 to N ) A(I) * R**REXP(I) * S**SEXP(I) 
!
!  Modified:
!
!    04 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, integer N, the number of polynomials.
!
!    Output, integer REXP(N), SEXP(N), the powers of R and S associated
!    with each polynomial.
!
  integer, parameter :: n_internal = 3
!
  integer n
  integer rexp(n_internal)
  integer sexp(n_internal)
!
  n = n_internal
  rexp(1) = 0
  sexp(1) = 0
  rexp(2) = 0
  sexp(2) = 1
  rexp(3) = 1
  sexp(3) = 0
  return
end
subroutine poly_t6 ( n, rexp, sexp )
!
!*******************************************************************************
!
!! POLY_T6 returns the polynomials associated with a 6 node triangle.
!
!
!  Diagram:
!
!    |
!    1  6
!    |  |\
!    |  | \
!    S  4  5
!    |  |   \
!    |  |    \
!    0  1--2--3
!    |
!    +--0--R--1-->
!
!  Formula:
!
!    Given coefficients A(I), the polynomial interpolant at (R,S) is
!
!      P(R,S) = SUM ( I = 1 to N ) A(I) * R**REXP(I) * S**SEXP(I) 
!
!  Modified:
!
!    05 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, integer N, the number of polynomials.
!
!    Output, integer REXP(N), SEXP(N), the powers of R and S associated
!    with each polynomial.
!
  integer, parameter :: n_internal = 6
!
  integer n
  integer rexp(n_internal)
  integer sexp(n_internal)
!
  n = n_internal
  rexp(1) = 0
  sexp(1) = 0
  rexp(2) = 0
  sexp(2) = 1
  rexp(3) = 0
  sexp(3) = 2
  rexp(4) = 1
  sexp(4) = 0
  rexp(5) = 1
  sexp(5) = 1
  rexp(6) = 2
  sexp(6) = 0
  return
end
subroutine poly_t10 ( n, rexp, sexp )
!
!*******************************************************************************
!
!! POLY_T10 returns the polynomials associated with a 10 node triangle.
!
!
!  Diagram:
!
!    |
!    1  10
!    |  |\
!    |  | \
!    |  8  9
!    |  |   \
!    S  |    \
!    |  5  6  7
!    |  |      \
!    |  |       \
!    0  1--2--3--4
!    |
!    +--0----R---1-->
!
!  Formula:
!
!    Given coefficients A(I), the polynomial interpolant at (R,S) is
!
!      P(R,S) = SUM ( I = 1 to N ) A(I) * R**REXP(I) * S**SEXP(I) 
!
!  Modified:
!
!    05 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, integer N, the number of polynomials.
!
!    Output, integer REXP(N), SEXP(N), the powers of R and S associated
!    with each polynomial.
!
  integer, parameter :: n_internal = 10
!
  integer n
  integer rexp(n_internal)
  integer sexp(n_internal)
!
  n = n_internal
  rexp(1) = 0
  sexp(1) = 0
  rexp(2) = 0
  sexp(2) = 1
  rexp(3) = 0
  sexp(3) = 2
  rexp(4) = 0
  sexp(4) = 3
  rexp(5) = 1
  sexp(5) = 0
  rexp(6) = 1
  sexp(6) = 1
  rexp(7) = 1
  sexp(7) = 2
  rexp(8) = 2
  sexp(8) = 0
  rexp(9) = 2
  sexp(9) = 1
  rexp(10) = 3
  sexp(10) = 0
  return
end
subroutine quad_1d ( norder, xtab, weight )
!
!*******************************************************************************
!
!! QUAD_1D computes abscissas and weights for Gauss-Legendre quadrature.
!
!
!  Integration interval:
!
!    [ -1, 1 ]
!
!  Weight function:
!
!    1.
!
!  Integral to approximate:
!
!    INTEGRAL ( -1 <= X <= 1 ) F(X) dX.
!
!  Approximate integral:
!
!    SUM ( I = 1 to NORDER ) WEIGHT(I) * F ( XTAB(I) ).
!
!  Modified:
!
!    04 November 1998
!
!  Parameters:
!
!    Input, integer NORDER, the order of the rule.
!    NORDER must be greater than 0.
!
!    Output, real XTAB(NORDER), the abscissas of the rule.
!
!    Output, real WEIGHT(NORDER), the weights of the rule.
!    The weights are positive, symmetric, and should sum to 2.
!
  real, parameter :: pi = 3.14159265358979323846264338327950288419716939937510 
!
  integer norder
!
  real d1
  real d2pn
  real d3pn
  real d4pn
  real dp
  real dpn
  real e1
  real fx
  real h
  integer i
  integer iback
  integer k
  integer m
  integer mp1mi
  integer ncopy
  integer nmove
  real p
  real pk
  real pkm1
  real pkp1
  real t
  real u
  real v
  real x0
  real xtab(norder)
  real xtemp
  real weight(norder)
!
  if ( norder < 1 ) then
    write ( *, * ) ' '
    write ( *, * ) 'QUAD1D - Fatal error!'
    write ( *, * ) '  Illegal value of NORDER = ', norder
    stop
  end if
 
  e1 = real ( norder * ( norder + 1 ) )
 
  m = ( norder + 1 ) / 2
 
  do i = 1, ( norder + 1 ) / 2
 
    mp1mi = m + 1 - i
    t = real ( 4 * i - 1 ) * pi / real ( 4 * norder + 2 )
    x0 = cos(t) * ( 1.0E+00 - ( 1.0E+00 - 1.0E+00 / &
      real ( norder ) ) / real ( 8 * norder**2) )
 
    pkm1 = 1.0E+00
    pk = x0
    do k = 2, norder
      pkp1 = 2.0E+00 * x0 * pk - pkm1 - ( x0 * pk - pkm1 ) / real ( k )
      pkm1 = pk
      pk = pkp1
    end do
 
    d1 = real ( norder ) * ( pkm1 - x0 * pk )
    dpn = d1 / ( 1.0E+00 - x0**2 )
    d2pn = ( 2.0E+00 * x0 * dpn - e1 * pk ) / ( 1.0E+00 - x0**2 )
    d3pn = ( 4.0E+00 * x0 * d2pn + ( 2.0E+00 - e1 ) * dpn ) / ( 1.0E+00 - x0**2 )
    d4pn = ( 6.0E+00 * x0 * d3pn + ( 6.0E+00 - e1 ) * d2pn ) / ( 1.0E+00 - x0**2 )
    u = pk / dpn
    v = d2pn / dpn
!
!  Initial approximation H:
!
    h = - u * ( 1.0E+00 + 0.5E+00 * u * ( v + u * ( v**2 - d3pn &
      / ( 3.0E+00 * dpn ) ) ) )
!
!  Refine H using one step of Newton's method:
!
    p = pk + h * ( dpn + 0.5E+00 * h * ( d2pn + h / 3.0E+00 &
      * ( d3pn + 0.25E+00 * h * d4pn ) ) )
    dp = dpn + h * ( d2pn + 0.5E+00 * h * ( d3pn + h * d4pn / 3.0E+00 ) )
    h = h - p / dp
 
    xtemp = x0 + h
    xtab(mp1mi) = xtemp
 
    fx = d1 - h * e1 * ( pk + 0.5E+00 * h * ( dpn + h / 3.0E+00 &
      * ( d2pn + 0.25E+00 * h * ( d3pn + 0.2E+00 * h * d4pn ) ) ) )
    weight(mp1mi) = 2.0E+00 * ( 1.0E+00 - xtemp**2 ) / fx**2
 
  end do
 
  if ( mod ( norder, 2 ) == 1 ) then
    xtab(1) = 0.0E+00
  end if
!
!  Shift the data up.
!
  nmove = ( norder + 1 ) / 2
  ncopy = norder - nmove
  do i = 1, nmove
    iback = norder + 1 - i
    xtab(iback) = xtab(iback-ncopy)
    weight(iback) = weight(iback-ncopy)
  end do
!
!  Reflect values for the negative abscissas.
!
  do i = 1, norder - nmove
    xtab(i) = - xtab(norder+1-i)
    weight(i) = weight(norder+1-i)
  end do
 
  return
end
subroutine quad_t ( nrule, maxorder, norder, area, rquad, squad, wquad )
!
!*******************************************************************************
!
!! QUAD_T returns a quadrature rule for a triangle.
!
!
!  Formula:
!
!    The quadrature rule approximates
!
!      Integral ( 0 <= R <= 1, 0 <= S <= 1 - R ) F(R,S) dR dS 
!
!    by
!
!      AREA * Sum ( I = 1 to NORDER ) WQUAD(I) * F(RQUAD(I),SQUAD(I)).
!
!  Modified:
!
!    06 October 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer NRULE, the desired Gauss rule.
!      1, NORDER = 1;
!      2, NORDER = 3.
!
!    Input, integer MAXORDER, the maximum order for which storage is
!    available.
!
!    Output, integer NORDER, the order of the Gauss rule.
!
!    Output, real AREA, the area of the element.
!
!    Output, real RQUAD(NORDER), SQUAD(NORDER), the quadrature abscissas.
!
!    Output, real WQUAD(NORDER), the quadrature weights.
!
  integer maxorder
!
  real area
  integer norder
  integer nrule
  real rquad(maxorder)
  real squad(maxorder)
  real wquad(maxorder)
!
  area = 0.5
  if ( nrule == 1 ) then
    norder = 1
    rquad(1) = 1.0 / 3.0
    squad(1) = 1.0 / 3.0
    wquad(1) = 1.0
  else if ( nrule == 2 ) then
    rquad(1) = 1.0
    squad(1) = 0.0
    wquad(1) = 1.0 / 3.0
    rquad(2) = 0.0
    squad(2) = 1.0
    wquad(2) = 1.0 / 3.0
    rquad(3) = 0.0
    squad(3) = 0.0
    wquad(3) = 1.0 / 3.0
  else
    write ( *, * ) ' '
    write ( *, * ) 'QUADT - Fatal error!'
    write ( *, * ) '  Unacceptable value of NRULE = ', nrule
    stop
  end if
 
  return
end
subroutine serene ( type, ve, vn, vne, vnw, vs, vse, vsw, vw, vterp )
!
!*******************************************************************************
!
!! SERENE interpolates data using a serendipity quadrilateral.
!
!
!  Modified:
!
!    02 March 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = 2 ) TYPE, tells SERENE the geometry of the
!    finite element that surrounds the point of interest.  The options
!    are displayed in the following table, which suggests the meaning
!    of each option by its position:
!
!        |   |
!      NW* N * NE
!        |   |
!     -*-*-*-*-*-
!        |   |
!      W * C * E
!        |   |
!     -*-*-*-*-*-
!        |   |
!      SW* S * SE
!        |   |
!
!    Input, real VE, VN, VNE, VNW, VS, VSE, VSW, VW,
!    are the values of the function at the nodes to the east,
!    north, northeast, northwest, south, southeast, southwest and
!    west of the point of interest.  If the finite element is of
!    type 'C', then all 8 values are needed.  However, if the
!    finite element is of type 'SE', for instance, then only three
!    values are needed, namely VE, VN, and VNW, since these are
!    the only node positions defined in such a finite element.
!
!    Output, real VTERP, the interpolated value of the
!    function at the point of interest.
!
  real eta
  real pe
  real pn
  real pne
  real pnw
  real ps
  real pse
  real psw
  real pw
  character ( len = 2 ) type
  real ve
  real vn
  real vne
  real vnw
  real vs
  real vse
  real vsw
  real vw
  real vterp
  real xsi
!
!  To make this routine more general, simply pass in the values of XSI
!  and ETA at which the interpolated value is desired.
!
!  By setting XSI = ETA = 0, we are asking for the interpolated value
!  at the center of the finite element.
!
  xsi = 0.0
  eta = 0.0
!
!  8 node center
!
!  Polynomial space is spanned by:
!         1
!       x    y
!    x^2  xy  y^2
!      x^2y xy^2
!
!
!    ^   1    4--7--3
!    |        !     !
!    E        !     !
!    T   0    8  X  6
!    A        !     !
!    |        !     !
!    V  -1    1--5--2
!
!            -1  0  1
!
!           <---XSI--->
!
  if ( type == 'C' ) then
    psw = - 0.25 * ( 1.0 - xsi ) * ( 1.0 - eta ) * ( 1.0 + xsi + eta )
    pse = - 0.25 * ( 1.0 + xsi ) * ( 1.0 - eta ) * ( 1.0 - xsi + eta )
    pne = - 0.25 * ( 1.0 + xsi ) * ( 1.0 + eta ) * ( 1.0 - xsi - eta )
    pnw = - 0.25 * ( 1.0 - xsi ) * ( 1.0 + eta ) * ( 1.0 + xsi - eta )
    ps =    0.50 * ( 1.0 - xsi ) * ( 1.0 + xsi ) * ( 1.0 - eta )
    pe =    0.50 * ( 1.0 + xsi ) * ( 1.0 + eta ) * ( 1.0 - eta )
    pn =    0.50 * ( 1.0 - xsi ) * ( 1.0 + xsi ) * ( 1.0 + eta )
    pw =    0.50 * ( 1.0 - xsi ) * ( 1.0 + eta ) * ( 1.0 - eta )
!
!  5 node side
!
!    ^   1
!    |
!    E
!    T   0    8  X  6
!    A        !     !
!    |        !     !
!    V  -1    1--5--2
!
!            -1  0  1
!
!           <---XSI--->
!
  else if ( type == 'N' ) then
    psw =  0.5 * ( xsi - 1.0 ) * ( 1.0 + xsi + eta )
    pse = -0.5 * ( xsi + 1.0 ) * ( 1.0 - xsi + eta )
    ps =  -      ( xsi + 1.0 ) * ( xsi - 1.0 )
    pe =   0.5 * ( xsi + 1.0 ) * ( eta + 1.0 )
    pw =  -0.5 * ( xsi - 1.0 ) * ( eta + 1.0 )
!
!    ^   1    4--7
!    |        !
!    E        !
!    T   0    8  X
!    A        !
!    |        !
!    V  -1    1--5
!
!            -1  0  1
!
!           <---XSI--->
!
  else if ( type == 'E' ) then
    pse =  0.5 * ( eta - 1.0 ) * ( 1.0 + xsi + eta )
    pne = -0.5 * ( eta + 1.0 ) * ( 1.0 + xsi - eta )
    ps =  -0.5 * ( xsi + 1.0 ) * ( eta - 1.0 )
    pn =   0.5 * ( xsi + 1.0 ) * ( eta + 1.0 )
    pw =  -      ( eta + 1.0 ) * ( eta - 1.0 )
!
!  5 node side
!
!    ^   1       7--3
!    |              !
!    E              !
!    T   0       X  6
!    A              !
!    |              !
!    V  -1       5--2
!
!            -1  0  1
!
!           <---XSI--->
!
  else if ( type == 'W' ) then
    pse =   0.5 * ( eta - 1.0 ) * ( 1.0 - xsi + eta )
    pne = - 0.5 * ( eta + 1.0 ) * ( 1.0 - xsi - eta )
    ps =    0.5 * ( xsi - 1.0 ) * ( eta - 1.0 )
    pe =  -       ( eta - 1.0 ) * ( eta + 1.0 )
    pn =  - 0.5 * ( xsi - 1.0 ) * ( eta + 1.0 )
!
!  5 node side
!
!    ^   1    4--7--3
!    |        !     !
!    E        !     !
!    T   0    8  X  6
!    A
!    |
!    V  -1
!
!            -1  0  1
!
!           <---XSI--->
!
  else if ( type == 'S' ) then
    pne = - 0.5 * ( xsi + 1.0 ) * ( 1.0 - xsi - eta )
    pnw =   0.5 * ( xsi - 1.0 ) * ( 1.0 + xsi - eta )
    pe =  - 0.5 * ( eta - 1.0 ) * ( xsi + 1.0 )
    pn =  -       ( xsi + 1.0 ) * ( xsi - 1.0 )
    pw =    0.5 * ( eta - 1.0 ) * ( xsi - 1.0 )
!
!  3 node corner
!
!  Polynomial space is spanned by:
!         1
!       x    y
!
!
!    ^   1
!    |
!    E
!    T   0    8  X
!    A        !
!    |        !
!    V  -1    1--5
!
!            -1  0  1
!
!           <---XSI--->
!
  else if ( type == 'NE' ) then
    psw = - 1.0 - xsi - eta
    ps =    1.0 + xsi
    pw =    1.0       + eta
!
!  3 node corner
!
!  Polynomial space is spanned by:
!         1
!       x    y
!
!    ^   1
!    |
!    E
!    T   0       X  6
!    A              !
!    |              !
!    V  -1       5--2
!
!            -1  0  1
!
!           <---XSI--->
!
  else if ( type == 'NW' ) then
    pse = 1.0 + xsi - eta
    ps =  1.0 - xsi
    pe =  1.0       + eta
!
!  3 node corner
!
!  Polynomial space is spanned by:
!         1
!       x    y
!
!
!    ^   1    4--7
!    |        !
!    E        !
!    T   0    8  X
!    A
!    |
!    V  -1
!
!            -1  0  1
!
!           <---XSI--->
!
  else if ( type == 'SE' ) then
    pnw = - 1.0 - xsi + eta
    pn =    1.0 + xsi
    pw =    1.0       - eta
!
!  3 node corner
!
!  Polynomial space is spanned by:
!         1
!       x    y
!
!    ^   1       7--3
!    |              !
!    E              !
!    T   0       X  6
!    A
!    |
!    V  -1
!
!            -1  0  1
!
!           <---XSI--->
!
  else if ( type == 'SW' ) then
    pne = - 1.0 + xsi + eta
    pe =    1.0       - eta
    pn =    1.0 - xsi
  end if
  vterp = vsw * psw + vse * pse + vne * pne + vnw * pnw &
    + vs * ps + ve * pe + vn * pn + vw * pw
  return
end
subroutine sge_fa ( a, lda, n, ipivot, info )
!
!*******************************************************************************
!
!! SGE_FA factors a general matrix.
!
!
!  Note:
!
!    SGE_FA is a simplified version of the LINPACK routine SGEFA.
!
!  Parameters:
!
!    Input/output, real A(LDA,N), the matrix to be factored.
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
  real a(lda,n)
  integer i
  integer info
  integer ipivot(n)
  integer j
  integer k
  integer l
  real t
!
  info = 0
!
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
    if ( a(l,k) == 0.0 ) then
      info = k
      write ( *, * ) ' '
      write ( *, * ) 'SGE_FA - Fatal error!'
      write ( *, * ) '  Zero pivot on step ', info
      return
    end if
!
!  Interchange rows L and K if necessary.
!
    if ( l /= k ) then
      t = a(l,k)
      a(l,k) = a(k,k)
      a(k,k) = t
    end if
!
!  Normalize the values that lie below the pivot entry A(K,K).
!
    do i = k+1, n
      a(i,k) = - a(i,k) / a(k,k)
    end do
!
!  Row elimination with column indexing.
!
    do j = k+1, n
      if ( l /= k ) then
        t = a(l,j)
        a(l,j) = a(k,j)
        a(k,j) = t
      end if
      do i = k+1, n
        a(i,j) = a(i,j) + a(i,k) * a(k,j)
      end do
    end do
  end do
  ipivot(n) = n
  if ( a(n,n) == 0.0 ) then
    info = n
    write ( *, * ) ' '
    write ( *, * ) 'SGE_FA - Fatal error!'
    write ( *, * ) '  Zero pivot on step ', info
  end if
  return
end
subroutine sge_inv ( a, lda, n, ipivot, work )
!
!*******************************************************************************
!
!! SGE_INV computes the inverse of a matrix factored by SGE_FA.
!
!
!  Note:
!
!    SGE_INV is a simplified version of the LINPACK routine SGEDI.
!
!  Modified:
!
!    04 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, real A(LDA,N).
!    On input, the factor information computed by SGE_FA.
!    On output, the inverse matrix.
!
!    Input, integer LDA, the leading dimension of the array A,
!    which must be at least N.
!
!    Input, integer N, the order of the matrix A.
!
!    Input, integer IPIVOT(N), the pivot vector from SGE_FA.
!
!    Workspace, real WORK(N).
!
  integer lda
  integer n
!
  real a(lda,n)
  integer i
  integer ipivot(n)
  integer j
  integer k
  real temp
  real work(n)
!
!  Compute Inverse(U).
!
  do k = 1, n
    a(k,k) = 1.0 / a(k,k)
    do i = 1, k-1
      a(i,k) = - a(i,k) * a(k,k)
    end do
    do j = k + 1, n
      temp = a(k,j)
      a(k,j) = 0.0
      do i = 1, k
        a(i,j) = a(i,j) + temp * a(i,k)
      end do
    end do
  end do
!
!  Form Inverse(U) * Inverse(L).
!
  do k = n - 1, 1, -1
    do i = k + 1, n
      work(i) = a(i,k)
      a(i,k) = 0.0
    end do
    do j = k + 1, n
      do i = 1, n
        a(i,k) = a(i,k) + work(j) * a(i,j)
      end do
    end do
    if ( ipivot(k) /= k ) then
      do i = 1, n
        temp = a(i,k)
        a(i,k) = a(i,ipivot(k))
        a(i,ipivot(k)) = temp
      end do
    end if
  end do
  return
end
subroutine shape ( code, n, r, s, t, dtdr, dtds )
!
!*******************************************************************************
!
!! SHAPE evaluates shape functions for any available element.
!
!
!  Modified:
!
!    10 December 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) CODE, identifies the element.
!    Legal values include 'Q4', 'Q8', 'Q9', 'Q12', 'Q16', 'QL', 
!    'T3', 'T6' and 'T10'.
!
!    Input, integer N, the number of nodes in the element.
!
!    Input, real R, S, the reference coordinates of a point.
!
!    Output, real T(N), the basis functions at the point.
!
!    Output, real DTDR(N), the R basis derivatives at the point.
!
!    Output, real DTDS(N), the S basis derivatives at the point.
!
  integer n
!
  character ( len = * ) code
  real dtdr(n)
  real dtds(n)
  real r
  real s
  real t(n)
!
  if ( code == 'Q4' ) then
    call shape_q4 ( r, s, t, dtdr, dtds )
  else if( code == 'Q8' ) then
    call shape_q8 ( r, s, t, dtdr, dtds )
  else if ( code == 'Q9' ) then
    call shape_q9 ( r, s, t, dtdr, dtds )
  else if ( code == 'Q12' ) then
    call shape_q12 ( r, s, t, dtdr, dtds )
  else if ( code == 'Q16' ) then
    call shape_q16 ( r, s, t, dtdr, dtds )
  else if ( code == 'QL' ) then
    call shape_ql ( r, s, t, dtdr, dtds )
  else if ( code == 'T3' ) then
    call shape_t3 ( r, s, t, dtdr, dtds )
  else if ( code == 'T6' ) then
    call shape_t6 ( r, s, t, dtdr, dtds )
  else if ( code == 'T10' ) then
    call shape_t10 ( r, s, t, dtdr, dtds )
  else
    write ( *, * ) ' '
    write ( *, * ) 'SHAPE - Fatal error!'
    write ( *, '(a,a)' ) '  Unrecognized code = ', code
    stop
  end if
  return
end
subroutine shape_test ( code )
!
!*******************************************************************************
!
!! SHAPE_TEST verifies the shape function values at the basis nodes.
!
!
!  Modified:
!
!    09 December 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) CODE, identifies the element to be used.
!    Legal values include 'Q4', 'Q8', 'Q9', 'Q12', 'Q16', 'QL', 
!    'T3', 'T6' and 'T10'.
!
  integer, parameter :: maxn = 16
!
  real area
  character ( len = * ) code
  real dtdr(maxn)
  real dtds(maxn)
  integer i
  integer j
  integer n
  real r(maxn)
  real rsum
  real s(maxn)
  real ssum
  real t(maxn)
!
  write ( *, * ) ' '
  write ( *, * ) 'SHAPE_TEST'
  write ( *, '(a,a)' ) '  Verify shape functions of type ', code
  call node ( code, n, r, s, area )
  write ( *, * ) ' '
  write ( *, * ) '  Number of nodes = ', n
  write ( *, * ) '  Basis function values at basis nodes'
  write ( *, * ) '  should form the identity matrix.'
  write ( *, * ) ' '
  do i = 1, n
    call shape ( code, n, r(i), s(i), t, dtdr, dtds )
    write ( *, '(10f7.3)' ) ( t(j), j = 1, n )
  end do
  write ( *, * ) ' '
  write ( *, * ) '  R and S derivatives should sum to 0.'
  write ( *, * ) ' '
  write ( *, * ) '  dTdR sum, dTdS sum:'
  write ( *, * ) ' '
  do i = 1, n
    call shape ( code, n, r(i), s(i), t, dtdr, dtds )
    rsum = 0.0
    ssum = 0.0
    do j = 1, n
      rsum = rsum + dtdr(j)
      ssum = ssum + dtds(j)
    end do
    write ( *, '(2f14.8)' ) rsum, ssum
  end do
  return
end
subroutine shape_q4 ( r, s, t, dtdr, dtds )
!
!*******************************************************************************
!
!! SHAPE_Q4 evaluates shape functions for a 4 node quadrilateral.
!
!
!  Diagram:
!
!    |
!    1  3-----4
!    |  |     |
!    |  |     |
!    S  |     |
!    |  |     |
!    |  |     |
!    0  1-----2
!    |
!    +--0--R--1-->
!
!  Modified:
!
!    05 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real R, S, the reference coordinates of a point.
!
!    Output, real T(4), the basis functions at the point.
!
!    Output, real DTDR(4), the R basis derivatives at the point.
!
!    Output, real DTDS(4), the S basis derivatives at the point.
!
  real dtdr(4)
  real dtds(4)
  real r
  real s
  real t(4)
!
  t(1) = ( 1.0 - r ) * ( 1.0 - s )
  t(2) =         r   * ( 1.0 - s )
  t(3) = ( 1.0 - r ) *         s
  t(4) =         r   *         s
  dtdr(1) = - 1.0 + s
  dtdr(2) =   1.0 - s     
  dtdr(3) =       - s
  dtdr(4) =         s
  dtds(1) = - 1.0 + r
  dtds(2) =       - r
  dtds(3) =   1.0 - r
  dtds(4) =         r
  return
end
subroutine shape_q8 ( r, s, t, dtdr, dtds )
!
!*******************************************************************************
!
!! SHAPE_Q8 evaluates shape functions for an 8 node quadrilateral.
!
!
!  Comment:
!
!    This element is known as the "serendipity" element.
!
!  Diagram:
!
!    |
!    1  6--7--8
!    |  |     |
!    |  |     |
!    S  4     5
!    |  |     |
!    |  |     |
!    0  1--2--3
!    |
!    +--0--R--1-->
!
!  Modified:
!
!    05 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real R, S, the reference coordinates of a point.
!
!    Output, real T(8), the basis functions at the point.
!
!    Output, real DTDR(8), the R basis derivatives at the point.
!
!    Output, real DTDS(8), the S basis derivatives at the point.
!
  real dtdr(8)
  real dtds(8)
  real r
  real s
  real t(8)
!
  t(1) =             ( r - 1.0 )     * ( s - 1.0 ) * ( 1.0 - 2.0 * r - 2.0 * s )
  t(2) =   4.0 * r * ( r - 1.0 )     * ( s - 1.0 )
  t(3) =         r                   * ( s - 1.0 ) * ( 1.0 - 2.0 * r + 2.0 * s )
  t(4) =   4.0 *     ( r - 1.0 ) * s * ( s - 1.0 )
  t(5) = - 4.0 * r               * s * ( s - 1.0 )
  t(6) =             ( r - 1.0 ) * s               * ( 2.0 * r - 2.0 * s + 1.0 )
  t(7) = - 4.0 * r * ( r - 1.0 ) * s
  t(8) =         r               * s               * ( 2.0 * r + 2.0 * s - 3.0 )
  dtdr(1) = ( s - 1.0 ) * ( - 4.0 * r - 2.0 * s + 3.0 )
  dtdr(2) =   4.0 * ( 2.0 * r - 1.0 )     * ( s - 1.0 )
  dtdr(3) = ( s - 1.0 ) * ( - 4.0 * r + 2.0 * s + 1.0 )
  dtdr(4) =   4.0 *                     s * ( s - 1.0 )
  dtdr(5) = - 4.0 *                     s * ( s - 1.0 )
  dtdr(6) =   s         * (   4.0 * r - 2.0 * s - 1.0 )
  dtdr(7) = - 4.0 * ( 2.0 * r - 1.0 ) * s
  dtdr(8) =   s         * (   4.0 * r + 2.0 * s - 3.0 )
  dtds(1) = ( r - 1.0 ) * ( - 4.0 * s - 2.0 * r + 3.0 )
  dtds(2) =   4.0 * r * ( r - 1.0 )
  dtds(3) =   r *       (   4.0 * s - 2.0 * r - 1.0 )
  dtds(4) =   4.0 *     ( r - 1.0 ) * ( 2.0 * s - 1.0 )
  dtds(5) = - 4.0 * r               * ( 2.0 * s - 1.0 )
  dtds(6) = ( r - 1.0 ) * ( - 4.0 * s + 2.0 * r + 1.0 )
  dtds(7) = - 4.0 * r * ( r - 1.0 )
  dtds(8) =   r *       (   4.0 * s + 2.0 * r - 3.0 )
  return
end
subroutine shape_q9 ( r, s, t, dtdr, dtds )
!
!*******************************************************************************
!
!! SHAPE_Q9 evaluates shape functions for a 9 node quadrilateral.
!
!
!  Diagram:
!
!    |
!    1  7--8--9
!    |  |  :  |
!    |  |  :  |
!    S  4..5..6
!    |  |  :  |
!    |  |  :  |
!    0  1--2--3
!    |
!    +--0--R--1-->
!
!  Modified:
!
!    05 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real R, S, the reference coordinates of a point.
!
!    Output, real T(9), the basis functions at the point.
!
!    Output, real DTDR(9), the R basis derivatives at the point.
!
!    Output, real DTDS(9), the S basis derivatives at the point.
!
  real dtdr(9)
  real dtds(9)
  real r
  real s
  real t(9)
!
  t(1) =    4.0 * ( r - 1.0 ) * ( r - 0.5 ) * ( s - 1.0 ) * ( s - 0.5 )
  t(2) = -  8.0 * r * ( r - 1.0 ) * ( s - 1.0 ) * ( s - 0.5 )
  t(3) =    4.0 * r * ( r - 0.5 ) * ( s - 1.0 ) * ( s - 0.5 )
  t(4) = -  8.0 * ( r - 1.0 ) * ( r - 0.5 ) * s * ( s - 1.0 )
  t(5) =   16.0 * r * ( r - 1.0 ) * s * ( s - 1.0 )
  t(6) = -  8.0 * r * ( r - 0.5 ) * s * ( s - 1.0 )
  t(7) =    4.0 * ( r - 1.0 ) * ( r - 0.5 ) * s * ( s - 0.5 )
  t(8) = -  8.0 * r * ( r - 1.0 ) * s * ( s - 0.5 )
  t(9) =    4.0 * r * ( r - 0.5 ) * s * ( s - 0.5 )
  dtdr(1) = 4.0 * ( 2.0 * r - 1.5 ) * ( s - 1.0 ) * ( s - 0.5 )
  dtdr(2) = - 8.0 * ( 2.0 * r - 1.0 ) * ( s - 1.0 ) * ( s - 0.5 )
  dtdr(3) = 4.0 * ( 2.0 * r - 0.5 ) * ( s - 1.0 ) * ( s - 0.5 )
  dtdr(4) = - 8.0 * ( 2.0 * r - 1.5 ) * s * ( s - 1.0 )
  dtdr(5) = 16.0 * ( 2.0 * r - 1.0 ) * s * ( s - 1.0 )
  dtdr(6) = - 8.0 * ( 2.0 * r - 0.5 ) * s * ( s - 1.0 )
  dtdr(7) = 4.0 * ( 2.0 * r - 1.5 ) * s * ( s - 0.5 )
  dtdr(8) = - 8.0 * ( 2.0 * r - 1.0 ) * s * ( s - 0.5 )
  dtdr(9) = 4.0 * ( 2.0 * r - 0.5 ) * s * ( s - 0.5 )
  dtds(1) = 4.0 * ( r - 1.0 ) * ( r - 0.5 ) * ( 2.0 * s - 1.5 )
  dtds(2) = - 8.0 * r * ( r - 1.0 ) * ( 2.0 * s - 1.5 )
  dtds(3) = 4.0 * r * ( r - 0.5 ) * ( 2.0 * s - 1.5 )
  dtds(4) = - 8.0 * ( r - 1.0 ) * ( r - 0.5 ) * ( 2.0 * s - 1.0 )
  dtds(5) =  16.0 * r * ( r - 1.0 ) * ( 2.0 * s - 1.0 )
  dtds(6) = - 8.0 * r * ( r - 0.5 ) * ( 2.0 * s - 1.0 )
  dtds(7) = 4.0 * ( r - 1.0 ) * ( r - 0.5 ) * ( 2.0 * s - 0.5 )
  dtds(8) = - 8.0 * r * ( r - 1.0 ) * ( 2.0 * s - 0.5 ) 
  dtds(9) = 4.0 * r * ( r - 0.5 ) * ( 2.0 * s - 0.5 )
  return
end
subroutine shape_q12 ( r, s, t, dtdr, dtds )
!
!*******************************************************************************
!
!! SHAPE_Q12 evaluates shape functions for a 12 node quadrilateral.
!
!
!  Note:
!
!    This routine is being worked on.
!
!  Diagram:
!
!    |
!    1  9-10-11-12
!    |  |        |
!    |  7        8
!    S  |        |
!    |  5        6
!    |  |        |
!    0  1--2--3--4
!    |
!    +--0---R---1-->
!
!  Modified:
!
!    12 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real R, S, the reference coordinates of a point.
!
!    Output, real T(12), the basis functions at the point.
!
!    Output, real DTDR(12), the R basis derivatives at the point.
!
!    Output, real DTDS(12), the S basis derivatives at the point.
!
  real a
  real b
  real c
  real corner
  real d
  real dcdr
  real dcds
  real dtdr(12)
  real dtds(12)
  real r
  real s
  real t(12)
!
  a = 0.0
  b = 1.0 / 3.0
  c = 2.0 / 3.0
  d = 1.0
  corner = 9.0 * ( ( 2.0 * r - 1.0 )**2 + ( 2.0 * s - 1.0 )**2 ) - 10.0
  t(1) =     0.125  * ( r - d ) * ( s - d ) * corner
  t(2) =  - 13.5    * ( r - a ) * ( r - c ) * ( r - d ) * ( s - d )
  t(3) =    13.5    * ( r - a ) * ( r - b ) * ( r - d ) * ( s - d )
  t(4) =   - 0.125  * ( r - a ) * ( s - d ) * corner
  t(5) =  - 13.5    * ( r - d ) * ( s - a ) * ( s - c ) * ( s - d ) 
  t(6) =    13.5    * ( r - a ) * ( s - a ) * ( s - c ) * ( s - d )
  t(7) =    13.5    * ( r - d ) * ( s - a ) * ( s - b ) * ( s - d )
  t(8) =  - 13.5    * ( r - a ) * ( s - a ) * ( s - b ) * ( s - d )
  t(9) =   - 0.125  * ( r - d ) * ( s - a ) * corner
  t(10) =   13.5    * ( r - a ) * ( r - c ) * ( r - d ) * ( s - a )
  t(11) = - 13.5    * ( r - a ) * ( r - b ) * ( r - d ) * ( s - a )
  t(12) =    0.125  * ( r - a ) * ( s - a ) * corner
 
  dcdr = 36.0 * ( 2.0 * r - 1.0 )
  dtdr(1) =  0.125 * ( s - d ) * ( ( r - d ) * dcdr + corner )
  dtdr(2) =  - 13.5 * ( s - d ) * ( 3.0 * r**2 &
    - 2.0 * ( a + c + d ) * r + a * c + c * d + d * a ) 
  dtdr(3) =    13.5 * ( s - d ) * ( 3.0 * r**2 &
    - 2.0 * ( a + b + d ) * r + a * b + b * d + d * a )
  dtdr(4) = - 0.125 * ( s - d ) * ( ( r - a ) * dcdr + corner )
  dtdr(5) = - 13.5 * ( s - a ) * ( s - c ) * ( s - d ) 
  dtdr(6) =   13.5 * ( s - a ) * ( s - c ) * ( s - d )
  dtdr(7) =   13.5 * ( s - a ) * ( s - b ) * ( s - d )
  dtdr(8) = - 13.5 * ( s - a ) * ( s - b ) * ( s - d )
  dtdr(9) = - 0.125 * ( s - a ) * ( ( r - d ) * dcdr + corner )
  dtdr(10) =   13.5 * ( s - a ) * ( 3.0 * r**2 &
    - 2.0 * ( a + c + d ) * r + a * c + c * d + d * a ) 
  dtdr(11) = - 13.5 * ( s - a ) * ( 3.0 * r**2 &
    - 2.0 * ( a + b + d ) * r + a * b + b * d + d * a )
  dtdr(12) = 0.125 * ( s - a ) * ( ( r - a ) * dcdr + corner )
  dcds = 36.0 * ( 2.0 * s - 1.0 )
  dtds(1) =  0.125 * ( r - d ) * ( corner + ( s - d ) * dcds )
  dtds(2) =  - 13.5 * ( r - a ) * ( r - c ) * ( r - d ) 
  dtds(3) =  13.5 * ( r - a ) * ( r - b ) * ( r - d )
  dtds(4) = - 0.125  * ( r - a ) * ( corner + ( s - d ) * dcds )
  dtds(5) =  - 13.5 * ( r - d ) * ( 3.0 * s**2 &
    - 2.0 * ( a + c + d ) * s + a * c + c * d + d * a )
  dtds(6) =  13.5 * ( r - a ) * ( 3.0 * s**2 &
    - 2.0 * ( a + c + d ) * s + a * c + c * d + d * a )
  dtds(7) =  13.5 * ( r - d ) * ( 3.0 * s**2 &
    - 2.0 * ( a + b + d ) * s + a * b + b * d + d * a )
  dtds(8) =  - 13.5 * ( r - a ) * ( 3.0 * s**2 &
    - 2.0 * ( a + b + d ) * s + a * b + b * d + d * a )
  dtds(9) =  - 0.125 * ( r - d ) * ( corner + ( s - a ) * dcds )
  dtds(10) = 13.5 * ( r - a ) * ( r - c ) * ( r - d ) 
  dtds(11) = - 13.5 * ( r - a ) * ( r - b ) * ( r - d ) 
  dtds(12) = 0.125 * ( r - a ) * ( corner + ( s - a ) * dcds )
  return
end
subroutine shape_q16 ( r, s, t, dtdr, dtds )
!
!*******************************************************************************
!
!! SHAPE_Q16 evaluates shape functions for a 16 node quadrilateral.
!
!
!  Diagram:
!
!    |
!    1 13--14--15--16
!    |  |   :   :   |
!    |  |   :   :   |
!    |  9..10..11..12
!    S  |   :   :   |
!    |  |   :   :   |
!    |  5...6...7...8
!    |  |   :   :   |
!    |  |   :   :   |  
!    0  1---2---3---4
!    |
!    +--0-----R-----1-->
!
!  Modified:
!
!    12 December 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real R, S, the reference coordinates of a point.
!
!    Output, real T(16), the basis functions at the point.
!
!    Output, real DTDR(16), the R basis derivatives at the point.
!
!    Output, real DTDS(16), the S basis derivatives at the point.
!
  real dabc
  real dabd
  real dacd
  real dbcd
  real dtdr(16)
  real dtds(16)
  real r
  real ra
  real rb
  real rc
  real rd
  real s
  real sa
  real sb
  real sc
  real sd
  real t(16)
!
  ra = r - 0.0
  rb = r - 1.0 / 3.0
  rc = r - 2.0 / 3.0
  rd = r - 1.0
  sa = s - 0.0
  sb = s - 1.0 / 3.0
  sc = s - 2.0 / 3.0
  sd = s - 1.0
  t(1)  =   (  81.0 / 4.0 ) * rb * rc * rd * sb * sc * sd
  t(2)  = - ( 243.0 / 4.0 ) * ra * rc * rd * sb * sc * sd
  t(3)  =   ( 243.0 / 4.0 ) * ra * rb * rd * sb * sc * sd
  t(4)  = - (  81.0 / 4.0 ) * ra * rb * rc * sb * sc * sd
  t(5)  = - ( 243.0 / 4.0 ) * rb * rc * rd * sa * sc * sd
  t(6)  =   ( 729.0 / 4.0 ) * ra * rc * rd * sa * sc * sd
  t(7)  = - ( 729.0 / 4.0 ) * ra * rb * rd * sa * sc * sd
  t(8)  =   ( 243.0 / 4.0 ) * ra * rb * rc * sa * sc * sd
  t(9)  =   ( 243.0 / 4.0 ) * rb * rc * rd * sa * sb * sd
  t(10) = - ( 729.0 / 4.0 ) * ra * rc * rd * sa * sb * sd
  t(11) =   ( 729.0 / 4.0 ) * ra * rb * rd * sa * sb * sd
  t(12) = - ( 243.0 / 4.0 ) * ra * rb * rc * sa * sb * sd
  t(13) = - (  81.0 / 4.0 ) * rb * rc * rd * sa * sb * sc
  t(14) =   ( 243.0 / 4.0 ) * ra * rc * rd * sa * sb * sc
  t(15) = - ( 243.0 / 4.0 ) * ra * rb * rd * sa * sb * sc
  t(16) =   (  81.0 / 4.0 ) * ra * rb * rc * sa * sb * sc
  dbcd = 3.0 * r**2 -  4.0 * r       + 11.0 / 9.0
  dacd = 3.0 * r**2 - 10.0 * r / 3.0 +  2.0 / 3.0
  dabd = 3.0 * r**2 -  8.0 * r / 3.0 +  1.0 / 3.0
  dabc = 3.0 * r**2 -  2.0 * r       +  2.0 / 9.0
  dtdr( 1) =   (  81.0 / 4.0 ) * dbcd * sb * sc * sd
  dtdr( 2) = - ( 243.0 / 4.0 ) * dacd * sb * sc * sd
  dtdr( 3) =   ( 243.0 / 4.0 ) * dabd * sb * sc * sd
  dtdr( 4) = - (  81.0 / 4.0 ) * dabc * sb * sc * sd
  dtdr( 5) = - ( 243.0 / 4.0 ) * dbcd * sa * sc * sd
  dtdr( 6) =   ( 729.0 / 4.0 ) * dacd * sa * sc * sd
  dtdr( 7) = - ( 729.0 / 4.0 ) * dabd * sa * sc * sd
  dtdr( 8) =   ( 243.0 / 4.0 ) * dabc * sa * sc * sd
  dtdr( 9) =   ( 243.0 / 4.0 ) * dbcd * sa * sb * sd
  dtdr(10) = - ( 729.0 / 4.0 ) * dacd * sa * sb * sd
  dtdr(11) =   ( 729.0 / 4.0 ) * dabd * sa * sb * sd
  dtdr(12) = - ( 243.0 / 4.0 ) * dabc * sa * sb * sd
  dtdr(13) = - (  81.0 / 4.0 ) * dbcd * sa * sb * sc
  dtdr(14) =   ( 243.0 / 4.0 ) * dacd * sa * sb * sc
  dtdr(15) = - ( 243.0 / 4.0 ) * dabd * sa * sb * sc
  dtdr(16) =   (  81.0 / 4.0 ) * dabc * sa * sb * sc
  dbcd = 3.0 * s**2 -  4.0 * s       + 11.0 / 9.0
  dacd = 3.0 * s**2 - 10.0 * s / 3.0 +  2.0 / 3.0
  dabd = 3.0 * s**2 -  8.0 * s / 3.0 +  1.0 / 3.0
  dabc = 3.0 * s**2 -  2.0 * s       +  2.0 / 9.0
  dtds( 1) =   (  81.0 / 4.0 ) * rb * rc * rd * dbcd
  dtds( 2) = - ( 243.0 / 4.0 ) * ra * rc * rd * dbcd
  dtds( 3) =   ( 243.0 / 4.0 ) * ra * rb * rd * dbcd
  dtds( 4) = - (  81.0 / 4.0 ) * ra * rb * rc * dbcd
  dtds( 5) = - ( 243.0 / 4.0 ) * rb * rc * rd * dacd
  dtds( 6) =   ( 729.0 / 4.0 ) * ra * rc * rd * dacd
  dtds( 7) = - ( 729.0 / 4.0 ) * ra * rb * rd * dacd
  dtds( 8) =   ( 243.0 / 4.0 ) * ra * rb * rc * dacd
  dtds( 9) =   ( 243.0 / 4.0 ) * rb * rc * rd * dabd
  dtds(10) = - ( 729.0 / 4.0 ) * ra * rc * rd * dabd
  dtds(11) =   ( 729.0 / 4.0 ) * ra * rb * rd * dabd
  dtds(12) = - ( 243.0 / 4.0 ) * ra * rb * rc * dabd
  dtds(13) = - (  81.0 / 4.0 ) * rb * rc * rd * dabc
  dtds(14) =   ( 243.0 / 4.0 ) * ra * rc * rd * dabc
  dtds(15) = - ( 243.0 / 4.0 ) * ra * rb * rd * dabc
  dtds(16) =   (  81.0 / 4.0 ) * ra * rb * rc * dabc
  
  return
end
subroutine shape_ql ( r, s, t, dtdr, dtds )
!
!*******************************************************************************
!
!! SHAPE_QL evaluates shape functions for a 6 node quadratic/linear.
!
!
!  Diagram:
!
!    |
!    1  4--5--6
!    |  |     |
!    |  |     |
!    S  |     |
!    |  |     |
!    |  |     |
!    0  1--2--3
!    |
!    +--0--R--1-->
!
!  Modified:
!
!    05 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real R, S, the reference coordinates of a point.
!
!    Output, real T(6), the basis functions at the point.
!
!    Output, real DTDR(6), the R basis derivatives at the point.
!
!    Output, real DTDS(6), the S basis derivatives at the point.
!
  real dtdr(6)
  real dtds(6)
  real r
  real s
  real t(6)
!
  t(1) = - 2.0 *     ( r - 0.5 ) * ( r - 1.0 )     * ( s - 1.0 )
  t(2) =   4.0 * r               * ( r - 1.0 )     * ( s - 1.0 )
  t(3) = - 2.0 * r * ( r - 0.5 )                   * ( s - 1.0 )
  t(4) =   2.0 *     ( r - 0.5 ) * ( r - 1.0 ) * s
  t(5) = - 4.0 * r               * ( r - 1.0 ) * s
  t(6) =   2.0 * r * ( r - 0.5 )               * s
  dtdr(1) = 2.0 * ( - 2.0 * r + 1.5 )     * ( s - 1.0 )
  dtdr(2) = 4.0 * (   2.0 * r - 1.0 )     * ( s - 1.0 )
  dtdr(3) = 2.0 * ( - 2.0 * r + 0.5 )     * ( s - 1.0 ) 
  dtdr(4) = 2.0 * (   2.0 * r - 1.5 ) * s
  dtdr(5) = 4.0 * ( - 2.0 * r + 1.0 ) * s
  dtdr(6) = 2.0 * (   2.0 * r - 0.5 ) * s
  dtds(1) = - 2.0 *     ( r - 0.5 ) * ( r - 1.0 )
  dtds(2) =   4.0 * r               * ( r - 1.0 )
  dtds(3) = - 2.0 * r * ( r - 0.5 )
  dtds(4) =   2.0 *     ( r - 0.5 ) * ( r - 1.0 )
  dtds(5) = - 4.0 * r               * ( r - 1.0 )
  dtds(6) =   2.0 * r * ( r - 0.5 )
  return
end
subroutine shape_t3 ( r, s, t, dtdr, dtds )
!
!*******************************************************************************
!
!! SHAPE_T3 evaluates shape functions for a 3 node triangle.
!
!
!  Diagram:
!
!    |
!    1  3
!    |  |\
!    |  | \
!    S  |  \
!    |  |   \
!    |  |    \
!    0  1-----2
!    |
!    +--0--R--1-->
!
!  Modified:
!
!    05 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real R, S, the reference coordinates of a point.
!
!    Output, real T(3), the basis functions at the point.
!
!    Output, real DTDR(3), the R basis derivatives at the point.
!
!    Output, real DTDS(3), the S basis derivatives at the point.
!
  real dtdr(3)
  real dtds(3)
  real r
  real s
  real t(3)
!
  t(1) = 1.0 - r - s
  t(2) =       r
  t(3) =           s
  dtdr(1) = -1.0
  dtdr(2) =  1.0
  dtdr(3) =  0.0
  dtds(1) = -1.0
  dtds(2) =  0.0
  dtds(3) =  1.0
  return
end
subroutine shape_t6 ( r, s, t, dtdr, dtds )
!
!*******************************************************************************
!
!! SHAPE_T6 evaluates shape functions for a 6 node triangle.
!
!
!  Diagram:
!
!    |
!    1  6
!    |  |\
!    |  | \
!    S  4  5
!    |  |   \
!    |  |    \
!    0  1--2--3
!    |
!    +--0--R--1-->
!
!  Modified:
!
!    05 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real R, S, the reference coordinates of a point.
!
!    Output, real T(6), the basis functions at the point.
!
!    Output, real DTDR(6), the R basis derivatives at the point.
!
!    Output, real DTDS(6), the S basis derivatives at the point.
!
  real dtdr(6)
  real dtds(6)
  real r
  real s
  real t(6)
! 
  t(1) = 2.0 * ( 1.0 - r - s ) * ( 0.5 - r - s )
  t(2) = 4.0 * r * ( 1.0 - r - s )
  t(3) = 2.0 * r * ( r - 0.5 )
  t(4) = 4.0 * s * ( 1.0 - r - s )
  t(5) = 4.0 * r * s
  t(6) = 2.0 * s * ( s - 0.5 )
  dtdr(1) = - 3.0 + 4.0 * r + 4.0 * s
  dtdr(2) =   4.0 - 8.0 * r - 4.0 * s
  dtdr(3) = - 1.0 + 4.0 * r
  dtdr(4) =                 - 4.0 * s
  dtdr(5) =                   4.0 * s
  dtdr(6) =   0.0
  dtds(1) = - 3.0 + 4.0 * r + 4.0 * s
  dtds(2) =       - 4.0 * r
  dtds(3) =   0.0
  dtds(4) =   4.0 - 4.0 * r - 8.0 * s
  dtds(5) =         4.0 * r
  dtds(6) = - 1.0           + 4.0 * s
  return
end
subroutine shape_t10 ( r, s, t, dtdr, dtds )
!
!*******************************************************************************
!
!! SHAPE_T10 evaluates shape functions for a 10 node triangle.
!
!
!  Diagram:
!
!    |
!    1  10
!    |  |\
!    |  | \
!    |  8  9
!    |  |   \
!    S  |    \
!    |  5  6  7
!    |  |      \
!    |  |       \
!    0  1--2--3--4
!    |
!    +--0----R---1-->
!
!  Modified:
!
!    05 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real R, S, the reference coordinates of a point.
!
!    Output, real T(10), the basis functions at the point.
!
!    Output, real DTDR(10), the R basis derivatives at the point.
!
!    Output, real DTDS(10), the S basis derivatives at the point.
!
  real a 
  real b
  real c
  real dtdr(10)
  real dtds(10)
  real r
  real s
  real t(10)
!
  a = 1.0 / 3.0
  b = 2.0 / 3.0
  c = 1.0
  t( 1) = 4.5 * ( a - r - s ) * ( b - r - s ) * ( c - r - s )
  t( 2) = 13.5 * r * ( b - r - s ) * ( c - r - s ) 
  t( 3) = - 13.5 * r * ( a - r ) * ( c - r - s )
  t( 4) = 4.5 * r * ( a - r ) * ( b - r )
  t( 5) = 13.5 * s * ( b - r - s ) * ( c - r - s )
  t( 6) = 27.0 * r * s * ( c - r - s )
  t( 7) = 13.5 * r * s * ( r - a )
  t( 8) = 13.5 * s * ( s - a ) * ( c - r - s )
  t( 9) = 13.5 * r * s * ( s - a )
  t(10) = 4.5 * s * ( a - s ) * ( b - s )
  dtdr( 1) = 4.5 * ( ( a - s ) * ( 2.0 * r - c - b + 2.0 * s ) &
    - ( s - b ) * ( s - c ) - 2.0 * ( 2.0 * s - b - c ) * r - 3.0 * r**2 )
  dtdr( 2) = 13.5 * ( &
    ( s - b ) * ( s - c ) + 2.0 * ( 2.0 * s - b - c ) * r + 3.0 * r**2 )
  dtdr( 3) = - 13.5 * ( a * ( c - s ) + 2.0 * ( s - a - c ) * r + 3.0 * r**2 )
  dtdr( 4) = 4.5 * ( a * b - 2.0 * ( a + b ) * r + 3.0 * r**2 )
  dtdr( 5) = 13.5 * s * ( 2.0 * s - b - c + 2.0 * r )
  dtdr( 6) = 27.0 * s * ( c - s - 2.0 * r )
  dtdr( 7) = 13.5 * s * ( 2.0 * r - a )
  dtdr( 8) = - 13.5 * s * ( s - a )
  dtdr( 9) = 13.5 * s * ( s - a)
  dtdr(10) = 0.0
  dtds( 1) = 4.5 * ( ( a - r ) * ( 2.0 * s - c - b + 2.0 * r ) &
    - ( r - b ) * ( r - c ) - 2.0 * ( 2.0 * r - b - c ) * s - 3.0 * s**2 )
  dtds( 2) = 13.5 * r * ( 2.0 * s + 2.0 * r - b - c )
  dtds( 3) = 13.5 * r * ( a - r )
  dtds( 4) = 0.0
  dtds( 5) = 13.5 * ( ( r - b ) * ( r - c ) + &
    2.0 * ( 2.0 * r - b - c ) * s + 3.0 * s**2 )
  dtds( 6) = 27.0 * r * ( c - r - 2.0 * s )
  dtds( 7) = 13.5 * r * ( r - a )
  dtds( 8) = - 13.5 * ( a * ( c - r ) + 2.0 * ( r - c - a ) * s + 3.0 * s**2 )
  dtds( 9) = 13.5 * r * ( 2.0 * s - a)
  dtds(10) = 4.5 * ( a * b - 2.0 * ( a + b ) * s + 3.0 * s**2 )
  return
end
