function femprb(varargin)
%
%*******************************************************************************
%
%! FEMPRB calls the various FEMPACK tests.
%
clear global; clear functions;


writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'FEMPRB');
writef(1,['%s \n'], '  Begin FEMPACK tests.');
test01;
test02;
test03;
test04;
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'FEMPRB');
writef(1,['%s \n'], '  Normal end of FEMPACK tests.');
%  stop
end
function test01(varargin)
%
%*******************************************************************************
%
%! TEST01 tests the shape routines.
%

writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'TEST01');
writef(1,['%s \n'], '  SHAPE_TEST tests the shape routines.');
shape_test ( 'Q4' );
shape_test ( 'Q8' );
shape_test ( 'Q9' );
shape_test ( 'Q12' );
shape_test ( 'Q16' );
shape_test ( 'QL' );
shape_test ( 'T3' );
shape_test ( 'T6' );
shape_test ( 'T10' );
return;
end
function test02(varargin)
%
%*******************************************************************************
%
%! TEST02 tests the grid routines.
%

writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'TEST02');
writef(1,['%s \n'], '  Test the grid routines.');
grid_test ( 'Q4' );
grid_test ( 'Q8' );
grid_test ( 'Q9' );
grid_test ( 'Q12' );
grid_test ( 'Q16' );
grid_test ( 'QL' );
grid_test ( 'T3' );
grid_test ( 'T6' );
grid_test ( 'T10' );
return;
end
function test03(varargin)
%
%*******************************************************************************
%
%! TEST03 tests the map routines.
%

writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'TEST03');
writef(1,['%s \n'], '  Test the map routines.');
map_test ( 'Q4' );
map_test ( 'Q8' );
map_test ( 'Q9' );
map_test ( 'Q12' );
map_test ( 'Q16' );
map_test ( 'QL' );
map_test ( 'T3' );
map_test ( 'T6' );
map_test ( 'T10' );
return;
end
function test04(varargin)
%
%*******************************************************************************
%
%! TEST04 tests DIV_BIL.
%
persistent diff div dudx dudy dvdx dvdy i j m maxm n temp u v vort x xhi xlo xm y yhi ylo ym ; 

if isempty(m), m = 21; end;
if isempty(n), n = 13; end;
if isempty(maxm), maxm = m; end;
%
if isempty(diff), diff=0; end;
if isempty(div), div=zeros(maxm, n-1); end;
if isempty(dudx), dudx=0; end;
if isempty(dudy), dudy=0; end;
if isempty(dvdx), dvdx=0; end;
if isempty(dvdy), dvdy=0; end;
if isempty(i), i=0; end;
if isempty(j), j=0; end;
if isempty(temp), temp=0; end;
if isempty(u), u=zeros(maxm, n); end;
if isempty(v), v=zeros(maxm, n); end;
if isempty(vort), vort=zeros(maxm, n-1); end;
if isempty(x), x=0; end;
if isempty(xhi), xhi=0; end;
if isempty(xlo), xlo=0; end;
if isempty(xm), xm=0; end;
if isempty(y), y=0; end;
if isempty(yhi), yhi=0; end;
if isempty(ylo), ylo=0; end;
if isempty(ym), ym=0; end;
%
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'TEST04');
writef(1,['%s \n'], '  DIV_BIL estimates divergence and vorticity.');
writef(1,['%s \n'], ' ');
writef(1,['%s %0.15g %s \n'], '  Original U, V data forms ', m, ' rows and ');
writef(1,['%s %0.15g %s \n'], '  ', n, ' columns.');
%
%  Set limits of the data points.
%
xlo = 0.0;
xhi = 1.0;
ylo = 0.0;
yhi = 2.0;
%
%  Put dummy data into U and V at the data nodes.
%
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], '  I, J, X, Y, U(I,J), V(I,J)');
writef(1,['%s \n'], ' ');
for i = 1: m;
y =( real( m - i ) .* ylo + real( i - 1 ) .* yhi ) ./ real( m - 1 );
for j = 1: n;
x =( real( n - j ) .* xlo + real( j - 1 ) .* xhi ) ./ real( n - 1 );
u(i,j) = x .* y;
v(i,j) = sin( x.^2 + y.^2 );
writef(1,[repmat('%4i',1,2),repmat('%14.6f',1,4),'\n'], i, j, x, y, u(i,j), v(i, j));
end; j = fix(n+1);
writef(1,['%s \n'], ' ');
end; i = fix(m+1);
%
%  Get DIV and VORT.
%
[ div, m, maxm, n, u, v, vort, xhi, xlo, yhi, ylo ]=div_bil( div, m, maxm, n, u, v, vort, xhi, xlo, yhi, ylo );
%
%  Compare computed and known values at the centers of the elements.
%
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'I, J, XM(I, j), YM(I, j)');
writef(1,['%s \n'], ' ');
for i = 1: m-1;
ym =( real(2.*m-2.*i-1) .* ylo + real(2.*i-1) .* yhi ) ./ real( 2 .* m - 2 );
for j = 1: n-1;
xm =( real(2.*n-2.*j-1) .* xlo + real(2.*j-1) .* xhi ) ./ real( 2 .* n - 2 );
writef(1,[repmat('%4i',1,2),repmat('%14.6f',1,3),'\n'], i, j, xm, ym);
end; j = fix(n-1+1);
writef(1,['%s \n'], ' ');
end; i = fix(m-1+1);
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], '  I, J, DIV(I, j), Exact Divergence, Difference');
writef(1,['%s \n'], ' ');
for i = 1: m - 1;
ym =( real(2.*m-2.*i-1) .* ylo + real(2.*i-1) .* yhi ) ./ real( 2 .* m - 2 );
for j = 1: n - 1;
xm =( real(2.*n-2.*j-1) .* xlo + real(2.*j-1) .* xhi ) ./ real( 2 .* n - 2 );
dudx = ym;
dudy = xm;
dvdx = 2.0 .* xm .* cos( xm.^2 + ym.^2 );
dvdy = 2.0 .* ym .* cos( xm.^2 + ym.^2 );
temp = dudx + dvdy;
diff = div(i, j) - temp;
writef(1,[repmat('%4i',1,2),repmat('%14.6f',1,3),'\n'], i, j, div(i,j), temp, diff);
end; j = fix(n - 1+1);
writef(1,['%s \n'], ' ');
end; i = fix(m - 1+1);
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], '  I, J, VORT(I, j), Exact Vorticity, Difference');
writef(1,['%s \n'], ' ');
for i = 1: m - 1;
y =( real(2.*m-2.*i-1) .* ylo + real(2.*i-1) .* yhi ) ./ real( 2 .* m - 2 );
for j = 1: n-1;
x =( real(2.*n-2.*j-1) .* xlo + real(2.*j-1) .* xhi ) ./ real( 2 .* n - 2 );
dudx = y;
dudy = x;
dvdx = 2.0 .* x .* cos( x.^2 + y.^2 );
dvdy = 2.0 .* y .* cos( x.^2 + y.^2 );
temp = dvdx - dudy;
diff = vort(i, j) - temp;
writef(1,[repmat('%4i',1,2),repmat('%14.6f',1,3),'\n'], i, j, vort(i, j), temp, diff);
end; j = fix(n-1+1);
writef(1,['%s \n'], ' ');
end; i = fix(m - 1+1);
return;
end
%  fempack.f90  02 June 2000
%
function [dx11, dx12, dx21, dx22, dy11, dy12, dy21, dy22, psi11,psi12, psi21, psi22, xl, xm, xr, yb, ym, yt]=base_bil( dx11, dx12, dx21, dx22, dy11, dy12, dy21, dy22, psi11,psi12, psi21, psi22, xl, xm, xr, yb, ym, yt );
%
%*******************************************************************************
%
%! BASE_BIL evalutes basis functions for a rectangular bilinear element.
%
%
%  Discussion:
%
%    The routine is given the corners of a rectangular bilinear element.
%    It then evaluates the basis functions associated with each corner,
%    and their derivatives with respect to X and Y.
%
%    The 'local node' numbering is as follows:
%
%      (2,1)---(2,2)   <-- Y = YT
%        |       |
%        |       |
%      (1,1)---(1,2)   <-- Y = YB
%
%        ^       ^
%        |       |
%       x = XL    x = XR
%
%  Modified:
%
%    02 March 1999
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Output, real DX11, DX12, DX21, DX22, the derivatives of the basis
%    functions with respect to X, associated with local nodes (1,1), (1,2),
%    (2,1) and (2,2), and evaluated at the point (XM,YM).
%
%    Output, real DY11, DY12, DY21, DY22.
%    The derivatives of the basis functions with respect to Y,
%    associated with local nodes (1,1), (1,2), (2,1) and (2,2),
%    and evaluated at the point (XM,YM).
%
%    Output, real PSI11, PSI12, PSI21, PSI22.
%    The basis functions associated with local nodes (1,1), (1,2),
%    (2,1) and (2,2), and evaluated at the point (XM,YM).
%
%    Input, real XL, the X coordinate of the left boundary of the element.
%
%    Input, real XM.
%    XM is the X coordinate of the point at which the basis functions
%    and their derivatives should be evaluated.  It need not be the
%    center point, but generally should lie within the element.
%
%    Input, real XR, the X coordinate of the right boundary of the element.
%
%    Input, real YB, the Y coordinate of the bottom boundary of the element.
%
%    Input, real YM.
%    YM is the Y coordinate of the point at which the basis functions
%    and their derivatives should be evaluated.  It need not be the
%    center point, but generally should lie within the element.
%
%    Input, real YT, the Y coordinate of the top boundary of the element.
%
%

psi11 =( xm - xr ) .*( ym - yt ) ./(( xl - xr ) .*( yb - yt ) );
psi12 =( xm - xl ) .*( ym - yt ) ./(( xr - xl ) .*( yb - yt ) );
psi21 =( xm - xr ) .*( ym - yb ) ./(( xl - xr ) .*( yt - yb ) );
psi22 =( xm - xl ) .*( ym - yb ) ./(( xr - xl ) .*( yt - yb ) );
dx11 =( ym - yt ) ./(( xl - xr ) .*( yb - yt ) );
dx12 =( ym - yt ) ./(( xr - xl ) .*( yb - yt ) );
dx21 =( ym - yb ) ./(( xl - xr ) .*( yt - yb ) );
dx22 =( ym - yb ) ./(( xr - xl ) .*( yt - yb ) );
dy11 =( xm - xr ) ./(( xl - xr ) .*( yb - yt ) );
dy12 =( xm - xl ) ./(( xr - xl ) .*( yb - yt ) );
dy21 =( xm - xr ) ./(( xl - xr ) .*( yt - yb ) );
dy22 =( xm - xl ) ./(( xr - xl ) .*( yt - yb ) );
return;
end
function [div, m, maxm, n, u, v, vort, xhi, xlo, yhi, ylo]=div_bil( div, m, maxm, n, u, v, vort, xhi, xlo, yhi, ylo );
%
%*******************************************************************************
%
%! DIV_BIL estimates the divergence and vorticity of a discrete field.
%
%
%  Discussion:
%
%    The routine is given the values of a vector field ( U(X,Y), V(X,Y) ) at
%    an array of points ( X(I), Y(J) ) for I = 1,M and J = 1,N.
%
%    The routine models the vector field over the interior of this region using
%    a bilinear interpolant.  It then uses the interpolant to estimate the
%    value of the divergence:
%
%      DIV(X,Y) = dU/dX + dV/dY
%
%    and the vorticity:
%
%      VORT(X,Y) = dV/dX - dU/dY
%
%    at the INTERIORS of each of the bilinear elements.
%
%        |       |       |
%      (3,1)---(3,2)---(3,3)---
%        |       |       |
%        | [2,1] | [2,2] |
%        |       |       |
%      (2,1)---(2,2)---(2,3)---
%        |       |       |
%        | [1,1] | [1,2] |
%        |       |       |
%      (1,1)---(1,2)---(1,3)---
%
%    Here, the nodes labeled with parentheses represent the points at
%    which the original (U,V) data is given, while the nodes labeled
%    with square brackets represent the centers of the bilinear
%    elements, where the approximations to the divergence and vorticity
%    are made.
%
%    The reason for evaluating the divergence and vorticity in this way
%    is that the bilinear interpolant to the (U,V) data is not
%    differentiable at the boundaries of the elements, nor especially at
%    the nodes, but is an (infinitely differentiable) bilinear function
%    in the interior of each element.  If a value at the original nodes
%    is strongly desired, then the average at the four surrounding
%    central nodes may be taken.
%
%  Modified:
%
%    01 March 1999
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Output, real DIV(MAXM,N-1).
%    For I = 1 to M-1, J = 1 to N-1, DIV(I,J) contains an estimate for
%    the divergence in the bilinear element that lies between
%    data rows I and I+1, and data columns J and J+1.
%
%    Input, integer M, is the number of data rows.  M must be at
%    least 2.
%
%    Input, integer MAXM, is the leading dimension to use for DIV, U,
%    V, and VORT.  MAXM must be at least M.
%
%    Input, integer N, the number of data columns.  N must be at least 2.
%
%    Input, real U(MAXM,N).
%    For I = 1 to M, J = 1 to N, U(I,J) contains the value of the
%    first component of a vector quantity whose divergence and
%    vorticity are desired.  A common example would be that U
%    represents the horizontal velocity component of a flow field.
%
%    Input, real V(MAXM,N).
%    For I = 1 to M, J = 1 to N, V(I,J) contains the value of the
%    second component of a vector quantity whose divergence and
%    vorticity are desired.  A common example would be that V
%    represents the vertical velocity component of a flow field.
%
%    Output, real VORT(MAXM,N-1).
%    For I = 1 to M-1, J = 1 to N-1, VORT(I,J) contains an estimate for
%    the vorticity in the bilinear element that lies between
%    data rows I and I+1, and data columns J and J+1.
%
%    Input, real XHI, the X coordinate of the rightmost data column.
%
%    Input, real XLO, the X coordinate of the leftmost data column.
%    XHI and XLO must be distinct.
%
%    Input, real YHI, the Y coordinate of the uppermost data row.
%
%    Input, real YLO, the Y coordinate of the lowermost data row.
%    YHI and YLO must be distinct.
%
%
persistent dx11 dx12 dx21 dx22 dy11 dy12 dy21 dy22 i j psi11 psi12 psi21 psi22 xl xm xr yb ym yt ; 

div_orig=div;div_shape=[maxm,n-1];div=reshape([div_orig(1:min(prod(div_shape),numel(div_orig))),zeros(1,max(0,prod(div_shape)-numel(div_orig)))],div_shape);
if isempty(dx11), dx11=0; end;
if isempty(dx12), dx12=0; end;
if isempty(dx21), dx21=0; end;
if isempty(dx22), dx22=0; end;
if isempty(dy11), dy11=0; end;
if isempty(dy12), dy12=0; end;
if isempty(dy21), dy21=0; end;
if isempty(dy22), dy22=0; end;
if isempty(i), i=0; end;
if isempty(j), j=0; end;
if isempty(psi11), psi11=0; end;
if isempty(psi12), psi12=0; end;
if isempty(psi21), psi21=0; end;
if isempty(psi22), psi22=0; end;
u_orig=u;u_shape=[maxm,n];u=reshape([u_orig(1:min(prod(u_shape),numel(u_orig))),zeros(1,max(0,prod(u_shape)-numel(u_orig)))],u_shape);
v_orig=v;v_shape=[maxm,n];v=reshape([v_orig(1:min(prod(v_shape),numel(v_orig))),zeros(1,max(0,prod(v_shape)-numel(v_orig)))],v_shape);
vort_orig=vort;vort_shape=[maxm,n-1];vort=reshape([vort_orig(1:min(prod(vort_shape),numel(vort_orig))),zeros(1,max(0,prod(vort_shape)-numel(vort_orig)))],vort_shape);
if isempty(xl), xl=0; end;
if isempty(xm), xm=0; end;
if isempty(xr), xr=0; end;
if isempty(yb), yb=0; end;
if isempty(ym), ym=0; end;
if isempty(yt), yt=0; end;
%
if( m <= 1 )
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'DIV_BIL - Fatal error!');
writef(1,['%s \n'], '  M must be at least 2,');
writef(1,['%s %0.15g \n'], '  but the input value of M is ', m);
error(['stop encountered in original fortran code  ',char(10),';']);
end;
if( maxm < m )
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'DIV_BIL - Fatal error!');
writef(1,['%s \n'], '  MAXM must be at least M,');
writef(1,['%s %0.15g \n'], '  but the input value of MAXM is ', maxm);
writef(1,['%s %0.15g \n'], '  and the input value of M is ', m);
error(['stop encountered in original fortran code  ',char(10),';']);
end;
if( n <= 1 )
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'DIV_BIL - Fatal error!');
writef(1,['%s \n'], '  N must be at least 2,');
writef(1,['%s %0.15g \n'], '  but the input value of N is ', n);
error(['stop encountered in original fortran code  ',char(10),';']);
end;
if( xhi == xlo )
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'DIV_BIL - Fatal error!');
writef(1,['%s \n'], '  XHI and XLO must be distinct,');
writef(1,['%s %0.15g \n'], '  but the input value of XLO is ', xlo);
writef(1,['%s %0.15g \n'], '  and the input value of XHI is ', xhi);
error(['stop encountered in original fortran code  ',char(10),';']);
end;
if( yhi == ylo )
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'DIV_BIL - Fatal error!');
writef(1,['%s \n'], '  YHI and YLO must be distinct,');
writef(1,['%s %0.15g \n'], '  but the input value of YLO is ', ylo);
writef(1,['%s %0.15g \n'], '  and the input value of YHI is ', yhi);
error(['stop encountered in original fortran code  ',char(10),';']);
end;
%
for i = 1: m-1;
yb =((2.*m-2.*i  ) .* ylo +(2.*i-2) .* yhi ) ./ real( 2.*m-2 );
ym =((2.*m-2.*i-1) .* ylo +(2.*i-1) .* yhi ) ./ real( 2.*m-2 );
yt =((2.*m-2.*i-2) .* ylo +(2.*i  ) .* yhi ) ./ real( 2.*m-2 );
for j = 1: n-1;
xl =((2.*n-2.*j  ) .* xlo +(2.*j-2) .* xhi ) ./ real( 2.*n-2 );
xm =((2.*n-2.*j-1) .* xlo +(2.*j-1) .* xhi ) ./ real( 2.*n-2 );
xr =((2.*n-2.*j-2) .* xlo +(2.*j  ) .* xhi ) ./ real( 2.*n-2 );
[ dx11, dx12, dx21, dx22, dy11, dy12, dy21,dy22, psi11, psi12, psi21, psi22, xl, xm, xr, yb, ym, yt ]=base_bil( dx11, dx12, dx21, dx22, dy11, dy12, dy21,dy22, psi11, psi12, psi21, psi22, xl, xm, xr, yb, ym, yt );
%
%  Note the following formula for the value of U and V at the same
%  point that the divergence and vorticity are being evaluated.
%
%         umid =  u(i  ,j  ) * psi11 + u(i  ,j+1) * psi12%               + u(i+1,j  ) * psi21 + u(i+1,j+1) * psi22
%
%         vmid =  v(i  ,j  ) * psi11 + v(i  ,j+1) * psi12%               + v(i+1,j  ) * psi21 + v(i+1,j+1) * psi22
%
div(i,j) =  u(i  ,j  ) .* dx11 + u(i  ,j+1) .* dx12+ u(i+1,j  ) .* dx21 + u(i+1,j+1) .* dx22+ v(i  ,j  ) .* dy11 + v(i  ,j+1) .* dy12+ v(i+1,j  ) .* dy21 + v(i+1,j+1) .* dy22;
vort(i,j) =  v(i  ,j  ) .* dx11 + v(i  ,j+1) .* dx12+ v(i+1,j  ) .* dx21 + v(i+1,j+1) .* dx22- u(i  ,j  ) .* dy11 - u(i  ,j+1) .* dy12- u(i+1,j  ) .* dy21 - u(i+1,j+1) .* dy22;
end; j = fix(n-1+1);
end; i = fix(m-1+1);
div_orig(1:prod(div_shape))=div;div=div_orig;
u_orig(1:prod(u_shape))=u;u=u_orig;
v_orig(1:prod(v_shape))=v;v=v_orig;
vort_orig(1:prod(vort_shape))=vort;vort=vort_orig;
return;
end
function [element_coderesult, i ]=element_code( i );
%
%*******************************************************************************
%
%! ELEMENT_CODE returns the code for each element.
%
%
%  List:
%
%    I  ELEMENT_CODE   Definition
%    -  ------------   ----------
%    1  Q4             4 node linear Lagrange/serendipity quadrilateral;
%    2  Q8             8 node quadratic serendipity quadrilateral;
%    3  Q9             9 node quadratic Lagrange quadrilateral;
%    4  Q12            12 node cubic serendipity quadrilateral;
%    5  Q16            16 node cubic Lagrange quadrilateral;
%    6  QL             6 node linear/quadratic quadrilateral;
%    7  T3             3 node linear triangle;
%    8  T6             6 node quadratic triangle;
%    9  T10            10 node cubic triangle.
%
%  Modified:
%
%    04 January 1999
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, integer I, the number of the element.
%
%    Output, character ( len = 4 ) ELEMENT_CODE, the code for the element.
%
element_coderesult=[];
persistent element_code ; 

if isempty(element_coderesult), element_coderesult=repmat(' ',1,4); end;
%
if( i == 1 )
element_coderesult = 'Q4';
elseif( i == 2 ) ;
element_coderesult = 'Q8';
elseif( i == 3 ) ;
element_coderesult = 'Q9';
elseif( i == 4 ) ;
element_coderesult = 'Q12';
elseif( i == 5 ) ;
element_coderesult = 'Q16';
elseif( i == 6 ) ;
element_coderesult = 'QL';
elseif( i == 7 ) ;
element_coderesult = 'T3';
elseif( i == 8 ) ;
element_coderesult = 'T6';
elseif( i == 9 ) ;
element_coderesult = 'T10';
else;
element_coderesult = '????';
end;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',i); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [order_coderesult, code ]=order_code( code );
%
%*******************************************************************************
%
%! ORDER_CODE returns the order for each element.
%
%
%  List:
%
%    CODE  Order  Definition
%    ----  -----  ----------
%    Q4     4     4 node linear Lagrange/serendipity quadrilateral;
%    Q8     8     8 node quadratic serendipity quadrilateral;
%    Q9     9     9 node quadratic Lagrange quadrilateral;
%    Q12   12     12 node cubic serendipity quadrilateral;
%    Q16   16     16 node cubic Lagrange quadrilateral;
%    QL     6     6 node linear/quadratic quadrilateral;
%    T3     3     3 node linear triangle;
%    T6     6     6 node quadratic triangle;
%    T10   10     10 node cubic triangle.
%
%  Modified:
%
%    05 January 1999
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, character ( len = * ) CODE, the code for the element.
%
%    Output, integer ORDER_CODE, the order of the element.
%
order_coderesult=[];
persistent order_code ; 

if isempty(order_coderesult), order_coderesult=0; end;
%
if( strcmp(deblank(code),deblank('Q4')) )
order_coderesult = 4;
elseif( strcmp(deblank(code),deblank('Q8')) ) ;
order_coderesult = 8;
elseif( strcmp(deblank(code),deblank('Q9')) ) ;
order_coderesult = 9;
elseif( strcmp(deblank(code),deblank('Q12')) ) ;
order_coderesult = 12;
elseif( strcmp(deblank(code),deblank('Q16')) ) ;
order_coderesult = 16;
elseif( strcmp(deblank(code),deblank('QL')) ) ;
order_coderesult = 6;
elseif( strcmp(deblank(code),deblank('T3')) ) ;
order_coderesult = 3;
elseif( strcmp(deblank(code),deblank('T6')) ) ;
order_coderesult = 6;
elseif( strcmp(deblank(code),deblank('T10')) ) ;
order_coderesult = 10;
else;
order_coderesult = 0;
end;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',code); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [code, maxelem, n, nelemx, nelemy, nodes]=grid( code, maxelem, n, nelemx, nelemy, nodes );
%
%*******************************************************************************
%
%! GRID returns the grid associated with any available element.
%
%
%  Modified:
%
%    05 January 1999
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, character ( len = * ) CODE, identifies the element desired.
%    Legal values include 'Q4', 'Q8', 'Q9', 'Q12', 'Q16', 'QL', 'T3',
%    'T6' and 'T10'.
%
%    Input, integer MAXELEM, the maximum number of elements for which
%    storage is provided, which must be at least NELEMX * NELEMY.
%
%    Input, integer N, the order of the element.
%
%    Input, integer NELEMX, NELEMY, the number of quadrilaterals along the
%    X and Y directions.  The number of elements generated will be
%    NELEMX * NELEMY.
%
%    Output, integer NODES(N,MAXELEM); NODES(I,J) contains the index
%    of the I-th node of the J-th element.
%
%

nodes_orig=nodes;nodes_shape=[n,maxelem];nodes=reshape([nodes_orig(1:min(prod(nodes_shape),numel(nodes_orig))),zeros(1,max(0,prod(nodes_shape)-numel(nodes_orig)))],nodes_shape);
%
if( strcmp(deblank(code),deblank('Q4')) )
[ maxelem, nelemx, nelemy, nodes ]=grid_q4( maxelem, nelemx, nelemy, nodes );
elseif( strcmp(deblank(code),deblank('Q8')) ) ;
[ maxelem, nelemx, nelemy, nodes ]=grid_q8( maxelem, nelemx, nelemy, nodes );
elseif( strcmp(deblank(code),deblank('Q9')) ) ;
[ maxelem, nelemx, nelemy, nodes ]=grid_q9( maxelem, nelemx, nelemy, nodes );
elseif( strcmp(deblank(code),deblank('Q12')) ) ;
[ maxelem, nelemx, nelemy, nodes ]=grid_q12( maxelem, nelemx, nelemy, nodes );
elseif( strcmp(deblank(code),deblank('Q16')) ) ;
[ maxelem, nelemx, nelemy, nodes ]=grid_q16( maxelem, nelemx, nelemy, nodes );
elseif( strcmp(deblank(code),deblank('QL')) ) ;
[ maxelem, nelemx, nelemy, nodes ]=grid_ql( maxelem, nelemx, nelemy, nodes );
elseif( strcmp(deblank(code),deblank('T3')) ) ;
[ maxelem, nelemx, nelemy, nodes ]=grid_t3( maxelem, nelemx, nelemy, nodes );
elseif( strcmp(deblank(code),deblank('T6')) ) ;
[ maxelem, nelemx, nelemy, nodes ]=grid_t6( maxelem, nelemx, nelemy, nodes );
elseif( strcmp(deblank(code),deblank('T10')) ) ;
[ maxelem, nelemx, nelemy, nodes ]=grid_t10( maxelem, nelemx, nelemy, nodes );
else;
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'GRID - Fatal error!');
writef(1,['%s','%s','\n'], '  Illegal value of CODE = ', code);
error(['stop encountered in original fortran code  ',char(10),';']);
end;
nodes_orig(1:prod(nodes_shape))=nodes;nodes=nodes_orig;
return;
end
function [code]=grid_test( code );
%
%*******************************************************************************
%
%! GRID_TEST tests the grid routines.
%
%
%  Modified:
%
%    05 January 1999
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, character ( len = * ) CODE, the code for the element.
%    Legal values include 'Q4', 'Q8', 'Q9', 'Q12', 'Q16', 'QL',
%    'T3', 'T6' and 'T10'.
%
persistent i ielem maxelem n nelem nelemx nelemy nodes width ; 

if isempty(maxelem), maxelem = 12; end;
%
if isempty(i), i=0; end;
if isempty(ielem), ielem=0; end;
if isempty(n), n=0; end;
if isempty(nelem), nelem=0; end;
if isempty(nelemx), nelemx=0; end;
if isempty(nelemy), nelemy=0; end;
if isempty(nodes), nodes=zeros(1,16.*maxelem); end;
if isempty(width), width=0; end;
%
%  NODES is defined as a vector rather than a two dimensional array,
%  so that we can handle the various cases using a single array.
%
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'GRID_TEST');
writef(1,['%s','%s','\n'], '  Test the grid routine for element ', code);
nelemx = 3;
nelemy = 2;
nelem = fix(nelemx .* nelemy);
[n , code ]=order_code( code );
[ code, maxelem, n, nelemx, nelemy, nodes ]=grid( code, maxelem, n, nelemx, nelemy, nodes );
for ielem = 1: nelem;
for  i =( 1):( n ), writef(1,['%3i',repmat(' ',1,3),repmat('%3i',1,20),'\n'], ielem, nodes((ielem-1).*n+i)); end;
end; ielem = fix(nelem+1);
[ maxelem, n, nelem, nodes, width ]=grid_width( maxelem, n, nelem, nodes, width );
writef(1,['%s \n'], ' ');
writef(1,['%s %0.15g \n'], 'Grid width is ', width);
return;
end
function [maxelm, n, nelem, node, width]=grid_width( maxelm, n, nelem, node, width );
%
%*******************************************************************************
%
%! GRID_WIDTH computes the width of a given grid.
%
%
%  Definition:
%
%    The grid width is defined to be 1 plus the maximum absolute
%    difference of global indices of nodes in the same element.
%
%  Example:
%
%   For the following simple grid, the grid width is 14.
%
%   23---24---25---26---27---28---29
%    |         |         |         |
%    |         |         |         |
%   19        20        21        22
%    |         |         |         |
%    | 4       | 5       | 6       |
%   12---13---14---15---16---17---18
%    |         |         |         |
%    |         |         |         |
%    8         9        10        11
%    |         |         |         |
%    | 1       | 2       | 3       |
%    1----2----3----4----5----6----7
%
%  Modified:
%
%    12 October 1998
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, integer MAXELM, the maximum number of elements.
%
%    Input, integer N, the order of the elements.
%
%    Input, integer NELEM, the number of elements.
%
%    Input, integer NODE(N,MAXELM), the nodes that make up each element.
%
%    Output, integer WIDTH, the grid width.
%
%
persistent ielem inode1 inode2 ip1 ip2 ; 

if isempty(ielem), ielem=0; end;
if isempty(inode1), inode1=0; end;
if isempty(inode2), inode2=0; end;
if isempty(ip1), ip1=0; end;
if isempty(ip2), ip2=0; end;
node_orig=node;node_shape=[n,maxelm];node=reshape([node_orig(1:min(prod(node_shape),numel(node_orig))),zeros(1,max(0,prod(node_shape)-numel(node_orig)))],node_shape);
%
width = 0;

for ielem = 1: nelem;
for inode1 = 1: n;
ip1 = fix(node(inode1,ielem));
for inode2 = 1: n;
ip2 = fix(node(inode2,ielem));
width = fix(max( width, abs( ip1 - ip2 ) ));
end; inode2 = fix(n+1);
end; inode1 = fix(n+1);
end; ielem = fix(nelem+1);

node_orig(1:prod(node_shape))=node;node=node_orig;
return;
end
function [maxelem, nelemx, nelemy, nodes]=grid_q4( maxelem, nelemx, nelemy, nodes );
%
%*******************************************************************************
%
%! GRID_Q4 produces a grid of 4 node quadrilaterals.
%
%
%  Example:
%
%    Input:
%
%      NELEMX = 3, NELEMY = 2
%
%    Output:
%
%      NODES =
%         1, 2,  5,  6;
%         2, 3,  6,  7;
%         3, 4,  7,  8;
%         5, 6,  9, 10;
%         6, 7, 10, 11;
%         7, 8, 11, 12.
%
%  Diagram:
%
%    9---10---11---12
%    |    |    |    |
%    |    |    |    |
%    |  4 |  5 |  6 |
%    |    |    |    |
%    5----6----7----8
%    |    |    |    |
%    |    |    |    |
%    |  1 |  2 |  3 |
%    |    |    |    |
%    1----2----3----4
%
%  Modified:
%
%    05 January 1999
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, integer MAXELEM, the maximum number of elements for which
%    storage is provided, which must be at least NELEMX * NELEMY.
%
%    Input, integer NELEMX, NELEMY, the number of quadrilaterals along the
%    X and Y directions.  The number of elements generated will be
%    NELEMX * NELEMY.
%
%    Output, integer NODES(4,MAXELEM); NODES(I,J) contains the index
%    of the I-th node of the J-th element.
%
%
persistent i ielem j nelem ; 

if isempty(ielem), ielem=0; end;
if isempty(i), i=0; end;
if isempty(j), j=0; end;
if isempty(nelem), nelem=0; end;
nodes_orig=nodes;nodes_shape=[4,maxelem];nodes=reshape([nodes_orig(1:min(prod(nodes_shape),numel(nodes_orig))),zeros(1,max(0,prod(nodes_shape)-numel(nodes_orig)))],nodes_shape);
%
nelem = fix(nelemx .* nelemy);
if( nelem > maxelem )
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'GRID_Q4 - Fatal error!');
writef(1,['%s \n'], '  Not enough storage for NODE array.');
writef(1,['%s %0.15g \n'], '  Increase MAXELEM to ', nelem);
error(['stop encountered in original fortran code  ',char(10),';']);
end;
ielem = 0;

for j = 1: nelemy;
for i = 1: nelemx;
ielem = fix(ielem + 1);
nodes(1,ielem) =fix(( j - 1 ) .*( nelemx + 1 ) + i);
nodes(2,ielem) =fix(( j - 1 ) .*( nelemx + 1 ) + i + 1);
nodes(3,ielem) =   fix(j       .*( nelemx + 1 ) + i);
nodes(4,ielem) =   fix(j       .*( nelemx + 1 ) + i + 1);
end; i = fix(nelemx+1);
end; j = fix(nelemy+1);
nodes_orig(1:prod(nodes_shape))=nodes;nodes=nodes_orig;
return;
end
function [maxelem, nelemx, nelemy, nodes]=grid_q8( maxelem, nelemx, nelemy, nodes );
%
%*******************************************************************************
%
%! GRID_Q8 produces a grid of 8 node quadrilaterals.
%
%
%  Example:
%
%    Input:
%
%      NELEMX = 3, NELEMY = 2
%
%    Output:
%
%      NODES =
%         3, 14, 12,  1,  2,  9, 13,  8;
%         5, 16, 14,  3,  4, 10, 15,  9;
%         7, 18, 16,  5,  6, 11, 17, 10;
%        14, 25, 23, 12, 13, 20, 24, 19;
%        16, 27, 25, 14, 15, 21, 26, 20;
%        18, 29, 27, 16, 17, 22, 28, 21.
%
%  Diagram:
%
%   23---24---25---26---27---28---29
%    |         |         |         |
%    |         |         |         |
%   19        20        21        22
%    |         |         |         |
%    | 4       | 5       | 6       |
%   12---13---14---15---16---17---18
%    |         |         |         |
%    |         |         |         |
%    8         9        10        11
%    |         |         |         |
%    | 1       | 2       | 3       |
%    1----2----3----4----5----6----7
%
%  Modified:
%
%    12 October 1998
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, integer MAXELEM, the maximum number of elements for which
%    storage is provided, which must be at least NELEMX * NELEMY.
%
%    Input, integer NELEMX, NELEMY, the number of quadrilaterals along the
%    X and Y directions.  The number of elements generated will be
%    NELEMX * NELEMY.
%
%    Output, integer NODES(8,MAXELEM); NODES(I,J) contains the index
%    of the I-th node of the J-th element.
%
%
persistent base i ielem j nelem ; 

if isempty(base), base=0; end;
if isempty(ielem), ielem=0; end;
if isempty(i), i=0; end;
if isempty(j), j=0; end;
if isempty(nelem), nelem=0; end;
nodes_orig=nodes;nodes_shape=[8,maxelem];nodes=reshape([nodes_orig(1:min(prod(nodes_shape),numel(nodes_orig))),zeros(1,max(0,prod(nodes_shape)-numel(nodes_orig)))],nodes_shape);
%
nelem = fix(nelemx .* nelemy);
if( nelem > maxelem )
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'GRID_Q8 - Fatal error!');
writef(1,['%s \n'], '  Not enough storage for NODE array.');
writef(1,['%s %0.15g \n'], '  Increase MAXELEM to ', nelem);
error(['stop encountered in original fortran code  ',char(10),';']);
end;
ielem = 0;
for j = 1: nelemy;
for i = 1: nelemx;
ielem = fix(ielem + 1);
base =fix(( j - 1 )  .*( 3 .* nelemx + 2 ) + 2 .* i - 1);
nodes(1,ielem) = fix(base + 2);
nodes(2,ielem) = fix(base +( 3 .* nelemx + 2 ) + 2);
nodes(3,ielem) = fix(base +( 3 .* nelemx + 2 ));
nodes(4,ielem) = fix(base);
nodes(5,ielem) = fix(base + 1);
nodes(6,ielem) = fix(base + 2 .* nelemx + 2 - i + 1);
nodes(7,ielem) = fix(base +( 3 .* nelemx + 2 ) + 1);
nodes(8,ielem) = fix(base +  2 .* nelemx + 2 - i);
end; i = fix(nelemx+1);
end; j = fix(nelemy+1);
nodes_orig(1:prod(nodes_shape))=nodes;nodes=nodes_orig;
return;
end
function [maxelem, nelemx, nelemy, nodes]=grid_q9( maxelem, nelemx, nelemy, nodes );
%
%*******************************************************************************
%
%! GRID_Q9 produces a grid of 9 node quadrilaterals.
%
%
%  Example:
%
%    Input:
%
%      NELEMX = 3, NELEMY = 2
%
%    Output:
%
%      NODES =
%         3, 17, 15,  1,  2, 10, 16,  8,  9;
%         5, 19, 17,  3,  4, 12, 18, 10, 11;
%         7, 21, 19,  5,  6, 14, 20, 12, 13;
%        17, 31, 29, 15, 16, 24, 30, 22, 23;
%        19, 33, 31, 17, 18, 26, 32, 24, 25;
%        21, 35, 33, 19, 20, 28, 34, 26, 27.
%
%  Diagram:
%
%   29---30---31---32---33---34---35
%    |    .    |    .    |    .    |
%    |    .    |    .    |    .    |
%   22 . 23 . 24 . 25 . 26 . 27 . 28
%    |    .    |    .    |    .    |
%    | 4  .    | 5  .    | 6  .    |
%   15---16---17---18---19---20---21
%    |    .    |    .    |    .    |
%    |    .    |    .    |    .    |
%    8 .  9 . 10 . 11 . 12 . 13 . 14
%    |    .    |    .    |    .    |
%    | 1  .    | 2  .    | 3  .    |
%    1----2----3----4----5----6----7
%
%  Modified:
%
%    11 October 1998
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, integer MAXELEM, the maximum number of elements for which
%    storage is provided, which must be at least NELEMX * NELEMY.
%
%    Input, integer NELEMX, NELEMY, the number of quadrilaterals along the
%    X and Y directions.  The number of elements generated will be
%    NELEMX * NELEMY.
%
%    Output, integer NODES(9,MAXELEM); NODES(I,J) contains the index
%    of the I-th node of the J-th element.
%
%
persistent base i ielem j nelem ; 

if isempty(base), base=0; end;
if isempty(ielem), ielem=0; end;
if isempty(i), i=0; end;
if isempty(j), j=0; end;
if isempty(nelem), nelem=0; end;
nodes_orig=nodes;nodes_shape=[9,maxelem];nodes=reshape([nodes_orig(1:min(prod(nodes_shape),numel(nodes_orig))),zeros(1,max(0,prod(nodes_shape)-numel(nodes_orig)))],nodes_shape);
%
nelem = fix(nelemx .* nelemy);
if( nelem > maxelem )
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'GRID_Q9 - Fatal error!');
writef(1,['%s \n'], '  Not enough storage for NODE array.');
writef(1,['%s %0.15g \n'], '  Increase MAXELEM to ', nelem);
error(['stop encountered in original fortran code  ',char(10),';']);
end;
ielem = 0;

for j = 1: nelemy;
for i = 1: nelemx;
ielem = fix(ielem + 1);
base =fix(( 2 .* j - 2 )  .*( 2 .* nelemx + 1 ) + 2 .* i - 1);
nodes(1,ielem) = fix(base + 2);
nodes(2,ielem) = fix(base + 2 .*( 2 .* nelemx + 1 ) + 2);
nodes(3,ielem) = fix(base + 2 .*( 2 .* nelemx + 1 ));
nodes(4,ielem) = fix(base);
nodes(5,ielem) = fix(base + 1);
nodes(6,ielem) = fix(base +( 2 .* nelemx + 1 ) + 2);
nodes(7,ielem) = fix(base + 2 .*( 2 .* nelemx + 1 ) + 1);
nodes(8,ielem) = fix(base +( 2 .* nelemx + 1 ));
nodes(9,ielem) = fix(base +( 2 .* nelemx + 1 ) + 1);
end; i = fix(nelemx+1);
end; j = fix(nelemy+1);
nodes_orig(1:prod(nodes_shape))=nodes;nodes=nodes_orig;
return;
end
function [maxelem, nelemx, nelemy, nodes]=grid_q12( maxelem, nelemx, nelemy, nodes );
%
%*******************************************************************************
%
%! GRID_Q12 produces a grid of 12 node quadrilaterals.
%
%
%  Example:
%
%    Input:
%
%      NELEMX = 3, NELEMY = 2
%
%    Output:
%
%      NODES =
%         1,  2,  3,  4, 11, 12, 15, 16, 19, 20, 21, 22;
%         4,  5,  6,  7, 12, 13, 16, 17, 22, 23, 24, 25;
%         7,  8,  9, 10, 13, 14, 17, 18, 25, 26, 27, 28;
%        19, 20, 21, 22, 29, 30, 33, 34, 37, 38, 39, 40;
%        22, 23, 24, 25, 30, 31, 34, 35, 40, 41, 42, 43;
%        25, 26, 27, 28, 31, 32, 35, 36, 43, 44, 45, 46.
%
%  Diagram:
%
%   37-38-39-40-41-42-43-44-45-46
%    |        |        |        |
%   33       34       35       36
%    |        |        |        |
%   29       30       31       32
%    | 4      | 5      | 6      |
%   19-20-21-22-23-24-25-26-27-28
%    |        |        |        |
%   15       16       17       18
%    |        |        |        |
%   11       12       13       14
%    | 1      | 2      | 3      |
%    1--2--3--4--5--6--7--8--9-10
%
%  Modified:
%
%    07 January 1999
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, integer MAXELEM, the maximum number of elements for which
%    storage is provided, which must be at least NELEMX * NELEMY.
%
%    Input, integer NELEMX, NELEMY, the number of quadrilaterals along the
%    X and Y directions.  The number of elements generated will be
%    NELEMX * NELEMY.
%
%    Output, integer NODES(12,MAXELEM); NODES(I,J) contains the index
%    of the I-th node of the J-th element.
%
%
persistent base i ielem j nelem ; 

if isempty(base), base=0; end;
if isempty(ielem), ielem=0; end;
if isempty(i), i=0; end;
if isempty(j), j=0; end;
if isempty(nelem), nelem=0; end;
nodes_orig=nodes;nodes_shape=[12,maxelem];nodes=reshape([nodes_orig(1:min(prod(nodes_shape),numel(nodes_orig))),zeros(1,max(0,prod(nodes_shape)-numel(nodes_orig)))],nodes_shape);
%
nelem = fix(nelemx .* nelemy);
if( nelem > maxelem )
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'GRID_Q12 - Fatal error!');
writef(1,['%s \n'], '  Not enough storage for NODE array.');
writef(1,['%s %0.15g \n'], '  Increase MAXELEM to ', nelem);
error(['stop encountered in original fortran code  ',char(10),';']);
end;
ielem = 0;
for j = 1: nelemy;
for i = 1: nelemx;
ielem = fix(ielem + 1);
base =fix(( j - 1 )  .*( 5 .* nelemx + 3 ) + 1);
nodes(1,ielem) =  fix(base +( i - 1 ) .* 3);
nodes(2,ielem) =  fix(base +( i - 1 ) .* 3 + 1);
nodes(3,ielem) =  fix(base +( i - 1 ) .* 3 + 2);
nodes(4,ielem) =  fix(base +( i - 1 ) .* 3 + 3);
nodes(5,ielem) =  fix(base + 3 .* nelemx + i);
nodes(6,ielem) =  fix(base + 3 .* nelemx + i + 1);
nodes(7,ielem) =  fix(base + 4 .* nelemx + i + 1);
nodes(8,ielem) =  fix(base + 4 .* nelemx + i + 2);
nodes(9,ielem) =  fix(base + 5 .* nelemx + 3 .* i);
nodes(10,ielem) = fix(base + 5 .* nelemx + 3 .* i + 1);
nodes(11,ielem) = fix(base + 5 .* nelemx + 3 .* i + 2);
nodes(12,ielem) = fix(base + 5 .* nelemx + 3 .* i + 3);
end; i = fix(nelemx+1);
end; j = fix(nelemy+1);
nodes_orig(1:prod(nodes_shape))=nodes;nodes=nodes_orig;
return;
end
function [maxelem, nelemx, nelemy, nodes]=grid_q16( maxelem, nelemx, nelemy, nodes );
%
%*******************************************************************************
%
%! GRID_Q16 produces a grid of 16 node quadrilaterals.
%
%
%  Example:
%
%    Input:
%
%      NELEMX = 2, NELEMY = 2
%
%    Output:
%
%      NODES =
%         1,  2,  3,  4,  8,  9, 10, 11, 15, 16, 17, 18, 22, 23, 24, 25;
%         4,  5,  6,  7, 11, 12, 13, 14, 18, 19, 20, 21, 25, 26, 27, 28;
%        22, 23, 24, 25, 29, 30, 31, 32, 36, 37, 38, 39, 43, 44, 45, 46;
%        25, 26, 27, 28, 32, 33, 34, 35, 39, 40, 41, 42, 46, 47, 48, 49.
%
%
%  Diagram:
%
%   43-44-45-46-47-48-49
%    |        |        |
%    |        |        |
%   36 37 38 39 40 41 42
%    |        |        |
%    |        |        |
%   29 30 31 32 33 34 35
%    |        |        |
%    | 3      | 4      |
%   22-23-24-25-26-27-28
%    |        |        |
%    |        |        |
%   15 16 17 18 19 20 21
%    |        |        |
%    |        |        |
%    8  9 10 11 12 13 14
%    |        |        |
%    | 1      | 2      |
%    1--2--3--4--5--6--7
%
%  Modified:
%
%    08 December 1998
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, integer MAXELEM, the maximum number of elements for which
%    storage is provided, which must be at least NELEMX * NELEMY.
%
%    Input, integer NELEMX, NELEMY, the number of triangles along the
%    X and Y directions.  The number of elements generated will be
%    NELEMX * NELEMY.
%
%    Output, integer NODES(16,MAXELEM); NODES(I,J) contains the index
%    of the I-th node of the J-th element.
%
%
persistent base i ielem j nelem ; 

if isempty(base), base=0; end;
if isempty(ielem), ielem=0; end;
if isempty(i), i=0; end;
if isempty(j), j=0; end;
if isempty(nelem), nelem=0; end;
nodes_orig=nodes;nodes_shape=[16,maxelem];nodes=reshape([nodes_orig(1:min(prod(nodes_shape),numel(nodes_orig))),zeros(1,max(0,prod(nodes_shape)-numel(nodes_orig)))],nodes_shape);
%
nelem = fix(nelemx .* nelemy);
if( nelem > maxelem )
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'GRID_Q16 - Fatal error!');
writef(1,['%s \n'], '  Not enough storage for NODE array.');
writef(1,['%s %0.15g \n'], '  Increase MAXELEM to ', nelem);
error(['stop encountered in original fortran code  ',char(10),';']);
end;
ielem = 0;

for j = 1: nelemy;
for i = 1: nelemx;
base =fix(( j - 1 ) .* 3 .*( 3 .* nelemx + 1 ) + 3 .* i - 2);
ielem = fix(ielem + 1);
nodes( 1,ielem) = fix(base);
nodes( 2,ielem) = fix(base                          + 1);
nodes( 3,ielem) = fix(base                          + 2);
nodes( 4,ielem) = fix(base                          + 3);
nodes( 5,ielem) = fix(base +( 3 .* nelemx + 1 ));
nodes( 6,ielem) = fix(base +( 3 .* nelemx + 1 ) + 1);
nodes( 7,ielem) = fix(base +( 3 .* nelemx + 1 ) + 2);
nodes( 8,ielem) = fix(base +( 3 .* nelemx + 1 ) + 3);
nodes( 9,ielem) = fix(base + 2 .*( 3 .* nelemx + 1 ));
nodes(10,ielem) = fix(base + 2 .*( 3 .* nelemx + 1 ) + 1);
nodes(11,ielem) = fix(base + 2 .*( 3 .* nelemx + 1 ) + 2);
nodes(12,ielem) = fix(base + 2 .*( 3 .* nelemx + 1 ) + 3);
nodes(13,ielem) = fix(base + 3 .*( 3 .* nelemx + 1 ));
nodes(14,ielem) = fix(base + 3 .*( 3 .* nelemx + 1 ) + 1);
nodes(15,ielem) = fix(base + 3 .*( 3 .* nelemx + 1 ) + 2);
nodes(16,ielem) = fix(base + 3 .*( 3 .* nelemx + 1 ) + 3);
end; i = fix(nelemx+1);
end; j = fix(nelemy+1);
nodes_orig(1:prod(nodes_shape))=nodes;nodes=nodes_orig;
return;
end
function [maxelem, nelemx, nelemy, nodes]=grid_ql( maxelem, nelemx, nelemy, nodes );
%
%*******************************************************************************
%
%! GRID_QL produces a grid of 6 node quadratics/linears.
%
%
%  Example:
%
%    Input:
%
%      NELEMX = 3, NELEMY = 2
%
%    Output:
%
%      NODES =
%         1,  2,  3,  8,  9, 10;
%         3,  4,  5, 10, 11, 12;
%         5,  6,  7, 12, 13, 14;
%         8,  9, 10, 15, 16, 17;
%        10, 11, 12, 17, 18, 19;
%        12, 13, 14, 19, 20, 21.
%
%  Diagram:
%
%   15---16---17---18---19---20---21
%    |         |         |         |
%    |         |         |         |
%    |    4    |    5    |    6    |
%    |         |         |         |
%    |         |         |         |
%    8----9---10---11---12---13---14
%    |         |         |         |
%    |         |         |         |
%    |    1    |    2    |    3    |
%    |         |         |         |
%    |         |         |         |
%    1----2----3----4----5----6----7
%
%  Modified:
%
%    05 January 1999
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, integer MAXELEM, the maximum number of elements for which
%    storage is provided, which must be at least NELEMX * NELEMY.
%
%    Input, integer NELEMX, NELEMY, the number of quadrilaterals along the
%    X and Y directions.  The number of elements generated will be
%    NELEMX * NELEMY.  X will the the 'quadratic direction', and
%    Y will be the 'linear direction'.
%
%    Output, integer NODES(6,MAXELEM); NODES(I,J) contains the index
%    of the I-th node of the J-th element.
%
%
persistent base i ielem j nelem ; 

if isempty(base), base=0; end;
if isempty(ielem), ielem=0; end;
if isempty(i), i=0; end;
if isempty(j), j=0; end;
if isempty(nelem), nelem=0; end;
nodes_orig=nodes;nodes_shape=[6,maxelem];nodes=reshape([nodes_orig(1:min(prod(nodes_shape),numel(nodes_orig))),zeros(1,max(0,prod(nodes_shape)-numel(nodes_orig)))],nodes_shape);
%
nelem = fix(nelemx .* nelemy);
if( nelem > maxelem )
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'GRID_QL - Fatal error!');
writef(1,['%s \n'], '  Not enough storage for NODE array.');
writef(1,['%s %0.15g \n'], '  Increase MAXELEM to ', nelem);
error(['stop encountered in original fortran code  ',char(10),';']);
end;
ielem = 0;

for j = 1: nelemy;
for i = 1: nelemx;
ielem = fix(ielem + 1);
base =fix(( j - 1 )  .*( 2 .* nelemx + 1 ) + 2 .* i - 1);
nodes(1,ielem) = fix(base);
nodes(2,ielem) = fix(base + 1);
nodes(3,ielem) = fix(base + 2);
nodes(4,ielem) = fix(base +( 2 .* nelemx + 1 ));
nodes(5,ielem) = fix(base +( 2 .* nelemx + 1 ) + 1);
nodes(6,ielem) = fix(base +( 2 .* nelemx + 1 ) + 2);
end; i = fix(nelemx+1);
end; j = fix(nelemy+1);
nodes_orig(1:prod(nodes_shape))=nodes;nodes=nodes_orig;
return;
end
function [maxelem, nelemx, nelemy, nodes]=grid_t3( maxelem, nelemx, nelemy, nodes );
%
%*******************************************************************************
%
%! GRID_T3 produces a grid of pairs of 3 node triangles.
%
%
%  Example:
%
%    Input:
%
%      NELEMX = 3, NELEMY = 2
%
%    Output:
%
%      NODES =
%         1,  2,  5;
%         6,  5,  2;
%         2,  3,  6;
%         7,  6,  3;
%         3,  4,  7;
%         8,  7,  4;
%         5,  6,  9;
%        10,  9,  6;
%         6,  7, 10;
%        11, 10,  7;
%         7,  8, 11;
%        12, 11,  8.
%
%  Diagram:
%
%    9---10---11---12
%    |\ 8 |\10 |\12 |
%    | \  | \  | \  |
%    |  \ |  \ |  \ |
%    |  7\|  9\| 11\|
%    5----6----7----8
%    |\ 2 |\ 4 |\ 6 |
%    | \  | \  | \  |
%    |  \ |  \ |  \ |
%    |  1\|  3\|  5\|
%    1----2----3----4
%
%  Modified:
%
%    05 January 1999
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, integer MAXELEM, the maximum number of elements for which
%    storage is provided, which must be at least 2 * NELEMX * NELEMY.
%
%    Input, integer NELEMX, NELEMY, the number of triangles along the
%    X and Y directions.  The number of elements generated will be
%    2 * NELEMX * NELEMY.
%
%    Output, integer NODES(3,MAXELEM); NODES(I,J) contains the index
%    of the I-th node of the J-th element.
%
%
persistent i ielem j nelem ; 

if isempty(ielem), ielem=0; end;
if isempty(i), i=0; end;
if isempty(j), j=0; end;
if isempty(nelem), nelem=0; end;
nodes_orig=nodes;nodes_shape=[3,maxelem];nodes=reshape([nodes_orig(1:min(prod(nodes_shape),numel(nodes_orig))),zeros(1,max(0,prod(nodes_shape)-numel(nodes_orig)))],nodes_shape);
%
nelem = fix(2 .* nelemx .* nelemy);
if( nelem > maxelem )
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'GRID_T3 - Fatal error!');
writef(1,['%s \n'], '  Not enough storage for NODE array.');
writef(1,['%s %0.15g \n'], '  Increase MAXELEM to ', nelem);
error(['stop encountered in original fortran code  ',char(10),';']);
end;
ielem = 0;

for j = 1: nelemy;
for i = 1: nelemx;
ielem = fix(ielem + 1);
nodes(1,ielem) =fix(( j - 1 ) .*( nelemx + 1 ) + i);
nodes(2,ielem) =fix(( j - 1 ) .*( nelemx + 1 ) + i + 1);
nodes(3,ielem) =   fix(j       .*( nelemx + 1 ) + i);
ielem = fix(ielem + 1);
nodes(1,ielem) =   fix(j       .*( nelemx + 1 ) + i + 1);
nodes(2,ielem) =   fix(j       .*( nelemx + 1 ) + i);
nodes(3,ielem) =fix(( j - 1 ) .*( nelemx + 1 ) + i + 1);
end; i = fix(nelemx+1);
end; j = fix(nelemy+1);
nodes_orig(1:prod(nodes_shape))=nodes;nodes=nodes_orig;
return;
end
function [maxelem, nelemx, nelemy, nodes]=grid_t6( maxelem, nelemx, nelemy, nodes );
%
%*******************************************************************************
%
%! GRID_T6 produces a grid of pairs of 6 node triangles.
%
%
%  Example:
%
%    Input:
%
%      NELEMX = 3, NELEMY = 2
%
%    Output:
%
%      NODES =
%         3, 15,  1,  2,  9,  8;
%        15,  3, 17, 16,  9, 10;
%         5, 17,  3   4, 11, 10;
%        17,  5, 19, 18, 11, 12;
%         7, 19,  5,  6, 13, 12;
%        19,  7, 21, 20, 13, 14;
%        17, 29, 15, 16, 23, 22;
%        29, 17, 31, 30, 23, 24;
%        19, 31, 17, 18, 25, 24;
%        31, 19, 33, 32, 25, 26;
%        21, 33, 19, 20, 27, 26;
%        33, 21, 35, 34, 27, 28.
%
%  Diagram:
%
%   29-30-31-32-33-34-35
%    |\ 8  |\10  |\12  |
%    | \   | \   | \   |
%   22 23 24 25 26 27 28
%    |   \ |   \ |   \ |
%    |  7 \|  9 \| 11 \|
%   15-16-17-18-19-20-21
%    |\ 2  |\ 4  |\ 6  |
%    | \   | \   | \   |
%    8  9 10 11 12 13 14
%    |   \ |   \ |   \ |
%    |  1 \|  3 \|  5 \|
%    1--2--3--4--5--6--7
%
%  Modified:
%
%    12 October 1998
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, integer MAXELEM, the maximum number of elements for which
%    storage is provided, which must be at least 2 * NELEMX * NELEMY.
%
%    Input, integer NELEMX, NELEMY, the number of triangles along the
%    X and Y directions.  The number of elements generated will be
%    2 * NELEMX * NELEMY.
%
%    Output, integer NODES(6,MAXELEM); NODES(I,J) contains the index
%    of the I-th node of the J-th element.
%
%
persistent base i ielem j nelem ; 

if isempty(base), base=0; end;
if isempty(ielem), ielem=0; end;
if isempty(i), i=0; end;
if isempty(j), j=0; end;
if isempty(nelem), nelem=0; end;
nodes_orig=nodes;nodes_shape=[6,maxelem];nodes=reshape([nodes_orig(1:min(prod(nodes_shape),numel(nodes_orig))),zeros(1,max(0,prod(nodes_shape)-numel(nodes_orig)))],nodes_shape);
%
nelem = fix(2 .* nelemx .* nelemy);
if( nelem > maxelem )
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'GRID_T6 - Fatal error!');
writef(1,['%s \n'], '  Not enough storage for NODE array.');
writef(1,['%s %0.15g \n'], '  Increase MAXELEM to ', nelem);
error(['stop encountered in original fortran code  ',char(10),';']);
end;
ielem = 0;

for j = 1: nelemy;
for i = 1: nelemx;
base =fix(( j - 1 ) .* 2 .*( 2 .* nelemx + 1 ) + 2 .* i - 1);
ielem = fix(ielem + 1);
nodes(1,ielem) = fix(base + 2);
nodes(2,ielem) = fix(base + 2 .*( 2 .* nelemx + 1 ));
nodes(3,ielem) = fix(base);
nodes(4,ielem) = fix(base + 1);
nodes(5,ielem) = fix(base +( 2 .* nelemx + 1 ) + 1);
nodes(6,ielem) = fix(base +( 2 .* nelemx + 1 ));
ielem = fix(ielem + 1);
nodes(1,ielem) = fix(base + 2 .*( 2 .* nelemx + 1 ));
nodes(2,ielem) = fix(base + 2);
nodes(3,ielem) = fix(base + 2 .*( 2 .* nelemx + 1 ) + 2);
nodes(4,ielem) = fix(base + 2 .*( 2 .* nelemx + 1 ) + 1);
nodes(5,ielem) = fix(base +( 2 .* nelemx + 1 ) + 1);
nodes(6,ielem) = fix(base +( 2 .* nelemx + 1 ) + 2);
end; i = fix(nelemx+1);
end; j = fix(nelemy+1);
nodes_orig(1:prod(nodes_shape))=nodes;nodes=nodes_orig;
return;
end
function [maxelem, nelemx, nelemy, nodes]=grid_t10( maxelem, nelemx, nelemy, nodes );
%
%*******************************************************************************
%
%! GRID_T10 produces a grid of pairs of 10 node triangles.
%
%
%  Example:
%
%    Input:
%
%      NELEMX = 2, NELEMY = 2
%
%    Output:
%
%      NODES =
%         1,  2,  3,  4, 10, 16, 22, 15,  8,  9;
%        25, 24, 23, 22, 16, 10,  4, 11, 18, 17;
%         4,  5,  6,  7, 13, 19, 25, 18, 11, 12;
%        28, 27, 26, 25, 19, 13,  7, 14, 21, 20;
%        22, 23, 24, 25, 31, 37, 43, 36, 29, 30;
%        46, 45, 44, 43, 37, 31, 25, 32, 39, 38;
%        25, 26, 27, 28, 34, 40, 46, 39, 31, 33;
%        49, 48, 47, 46, 40, 34, 28, 35, 42, 41.
%
%
%  Diagram:
%
%   43-44-45-46-47-48-49
%    |\     6 |\     8 |
%    | \      | \      |
%   36 37 38 39 40 41 42
%    |   \    |   \    |
%    |    \   |    \   |
%   29 30 31 32 33 34 35
%    |      \ |      \ |
%    | 5     \| 7     \|
%   22-23-24-25-26-27-28
%    |\     2 |\     4 |
%    | \      | \      |
%   15 16 17 18 19 20 21
%    |   \    |   \    |
%    |    \   |    \   |
%    8  9 10 11 12 13 14
%    |      \ |      \ |
%    | 1     \| 3     \|
%    1--2--3--4--5--6--7
%
%  Modified:
%
%    09 December 1998
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, integer MAXELEM, the maximum number of elements for which
%    storage is provided, which must be at least 2 * NELEMX * NELEMY.
%
%    Input, integer NELEMX, NELEMY, the number of triangles along the
%    X and Y directions.  The number of elements generated will be
%    2 * NELEMX * NELEMY.
%
%    Output, integer NODES(10,MAXELEM); NODES(I,J) contains the index
%    of the I-th node of the J-th element.
%
%
persistent base i ielem j nelem ; 

if isempty(base), base=0; end;
if isempty(ielem), ielem=0; end;
if isempty(i), i=0; end;
if isempty(j), j=0; end;
if isempty(nelem), nelem=0; end;
nodes_orig=nodes;nodes_shape=[10,maxelem];nodes=reshape([nodes_orig(1:min(prod(nodes_shape),numel(nodes_orig))),zeros(1,max(0,prod(nodes_shape)-numel(nodes_orig)))],nodes_shape);
%
nelem = fix(2 .* nelemx .* nelemy);
if( nelem > maxelem )
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'GRID_T10 - Fatal error!');
writef(1,['%s \n'], '  Not enough storage for NODE array.');
writef(1,['%s %0.15g \n'], '  Increase MAXELEM to ', nelem);
error(['stop encountered in original fortran code  ',char(10),';']);
end;
ielem = 0;

for j = 1: nelemy;
for i = 1: nelemx;
base =fix(( j - 1 ) .* 3 .*( 3 .* nelemx + 1 ) + 3 .* i - 2);
ielem = fix(ielem + 1);
nodes( 1,ielem) = fix(base);
nodes( 2,ielem) = fix(base                          + 1);
nodes( 3,ielem) = fix(base                          + 2);
nodes( 4,ielem) = fix(base                          + 3);
nodes( 5,ielem) = fix(base +( 3 .* nelemx + 1 ) + 2);
nodes( 6,ielem) = fix(base + 2 .*( 3 .* nelemx + 1 ) + 1);
nodes( 7,ielem) = fix(base + 3 .*( 3 .* nelemx + 1 ));
nodes( 8,ielem) = fix(base + 2 .*( 3 .* nelemx + 1 ));
nodes( 9,ielem) = fix(base +( 2 .* nelemx + 1 ) + 2);
nodes(10,ielem) = fix(base +( 2 .* nelemx + 1 ) + 3);
ielem = fix(ielem + 1);
nodes( 1,ielem) = fix(base + 3 .*( 3 .* nelemx + 1 ) + 3);
nodes( 2,ielem) = fix(base + 3 .*( 3 .* nelemx + 1 ) + 2);
nodes( 3,ielem) = fix(base + 3 .*( 3 .* nelemx + 1 ) + 1);
nodes( 4,ielem) = fix(base + 3 .*( 3 .* nelemx + 1 ));
nodes( 5,ielem) = fix(base + 2 .*( 3 .* nelemx + 1 ) + 1);
nodes( 6,ielem) = fix(base +( 3 .* nelemx + 1 ) + 2);
nodes( 7,ielem) = fix(base                          + 3);
nodes( 8,ielem) = fix(base +( 3 .* nelemx + 1 ) + 3);
nodes( 9,ielem) = fix(base + 2 .*( 3 .* nelemx + 1 ) + 3);
nodes(10,ielem) = fix(base + 2 .*( 3 .* nelemx + 1 ) + 2);
end; i = fix(nelemx+1);
end; j = fix(nelemy+1);
nodes_orig(1:prod(nodes_shape))=nodes;nodes=nodes_orig;
return;
end
function [code, dtdr, dtds, maxn, n, r, s, t, ubase, u, dudr, duds]=interp( code, dtdr, dtds, maxn, n, r, s, t, ubase, u, dudr, duds );
%
%*******************************************************************************
%
%! INTERP interpolates a quantity in an element from basis node values.
%
%
%  Modified:
%
%    09 December 1998
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, character ( len = * ) CODE, identifies the element.
%    Legal values include 'Q4', 'Q8', 'Q9', 'Q12', 'Q16', 'QL',
%    'T3', 'T6' and 'T10'.
%
%    Output, real DTDR(MAXN), DTDS(MAXN), the derivatives of the
%    basis functions at (R,S).
%
%    Input, integer MAXN, the maximum value of N.
%
%    Output, integer N, the order of the element.
%
%    Input, real R, S, the reference coordinates of a point.
%
%    Output, real T(MAXN), the value of the basis functions at (R,S).
%
%    Input, real UBASE(MAXN), the value of the quantity at the basis nodes.
%
%    Output, real U, DUDR, DUDS, the interpolated value of the
%    quantity and its derivatives at the point (R,S).
%
%
persistent i ; 

if isempty(i), i=0; end;
%
[ code, n, r, s, t, dtdr, dtds ]=shape( code, n, r, s, t, dtdr, dtds );

u = 0.0;
dudr = 0.0;
duds = 0.0;
for i = 1: n;
u = u + ubase(i) .* t(i);
dudr = dudr + ubase(i) .* dtdr(i);
duds = duds + ubase(i) .* dtds(i);
end; i = fix(n+1);

return;
end
function [code, w, lda, n]=map( code, w, lda, n );
%
%*******************************************************************************
%
%! MAP returns the interpolation matrix for any available element.
%
%
%  Formula:
%
%    Given data Q(J) associated with the nodes, the coefficients of
%    the interpolating polynomial are
%
%      A(I) = W(I,J) * Q(J)
%
%   In other words, if we let PHI(I,R,S) be the I-th basis function,
%   evaluated at the point (R,S), and we let REXP(I) and SEXP(I)
%   be the exponents of R and S in the I-th associated polynomial,
%   then the interpolating polynomial P(R,S) has two forms:
%
%     P(R,S) =
%
%       = SUM ( I = 1 to N ) Q(I) * PHI(I,R,S)
%       = SUM ( I = 1 to N ) A(I) * R**REXP(I) * S**SEXP(I)
%
%  Modified:
%
%    04 January 1999
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, character ( len = * ) CODE, identifies the element.
%    Legal values include 'Q4', 'Q8', 'Q9', 'Q12', 'Q16', 'QL',
%    'T3', 'T6' and 'T10'.
%
%    Output, real W(LDA,N), the interpolation matrix.
%
%    Input, integer LDA, the leading dimension of the W array.
%    LDA must be at least N.
%
%    Output, integer N, the order of the element.
%
persistent area i info ipivot j maxn r rexp s sexp work ; 

if isempty(maxn), maxn = 20; end;
%
%
if isempty(area), area=0; end;
if isempty(i), i=0; end;
if isempty(info), info=0; end;
if isempty(ipivot), ipivot=zeros(1,maxn); end;
if isempty(j), j=0; end;
if isempty(r), r=zeros(1,maxn); end;
if isempty(rexp), rexp=zeros(1,maxn); end;
if isempty(s), s=zeros(1,maxn); end;
if isempty(sexp), sexp=zeros(1,maxn); end;
w_orig=w;w_shape=[lda,n];w=reshape([w_orig(1:min(prod(w_shape),numel(w_orig))),zeros(1,max(0,prod(w_shape)-numel(w_orig)))],w_shape);
if isempty(work), work=zeros(1,maxn); end;
%
%  Get the (R,S) location of the nodes.
%
[ code, n, r, s, area ]=node( code, n, r, s, area );
if( lda < n )
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'MAP - Fatal error!');
writef(1,['%s \n'], '  LDA < N.');
error(['stop encountered in original fortran code  ',char(10),';']);
end;
if( maxn < n )
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'MAP - Fatal error!');
writef(1,['%s \n'], '  Internal parameter MAXN exceeded.');
writef(1,['%s \n'], '  MAXN < N.');
error(['stop encountered in original fortran code  ',char(10),';']);
end;
%
%  Get the associated polynomials.
%
[ code, n, rexp, sexp ]=poly( code, n, rexp, sexp );
%
%  Set up the Vandermonde matrix.
%
for i = 1: n;
for j = 1: n;
w(i,j) = r(i).^rexp(j) .* s(i).^sexp(j);
end; j = fix(n+1);
end; i = fix(n+1);
%
%  Factor the Vandermonde matrix.
%
[ w, lda, n, ipivot, info ]=sge_fa( w, lda, n, ipivot, info );
writef(1,['%s \n'], '-----------------------------------------------------------------------');
writef(1,['%s %0.15g \n'], 'w=',w);
writef(1,['%s %0.15g \n'], 'lda=',lda);
writef(1,['%s %0.15g \n'], 'n=',n);
writef(1,['%s %0.15g \n'], 'ipivot=',ipivot);
writef(1,['%s %0.15g \n'], 'info=',info);
if( info ~= 0 )
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'MAP - Fatal error!');
writef(1,['%s \n'], '  The Vandermonde matrix is singular.');
error(['stop encountered in original fortran code  ',char(10),';']);
end;
%
%  Invert the Vandermonde matrix.
%
[ w, lda, n, ipivot, work ]=sge_inv( w, lda, n, ipivot, work );
w_orig(1:prod(w_shape))=w;w=w_orig;
return;
end
function [code]=map_test( code );
%
%*******************************************************************************
%
%! MAP_TEST tests the map routines.
%
%
%  Modified:
%
%    05 January 1999
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, character ( len = 4 ) CODE, the code for the element.
%    Legal values include 'Q4', 'Q8', 'Q9', 'Q12', 'Q16', 'QL',
%    'T3', 'T6' and 'T10'.
%
persistent i j lda maxn n w ; 

if isempty(maxn), maxn = 20; end;
if isempty(lda), lda = maxn; end;
%
if isempty(i), i=0; end;
if isempty(j), j=0; end;
if isempty(n), n=0; end;
if isempty(w), w=zeros(maxn,maxn); end;
%
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'MAP_TEST');
writef(1,['%s','%s','\n'], '  The interpolation matrix for element ', code);
writef(1,['%s \n'], ' ');
[n , code ]=order_code( code );
[ code, w, lda, n ]=map( code, w, lda, n );
for i = 1: n;
for  j =( 1):( n ), writef(1,[repmat('%9.3f',1,7),'\n'], w(i,j)); end;
end; i = fix(n+1);
return;
end
function [code, n, r, s, area]=node( code, n, r, s, area );
%
%*******************************************************************************
%
%! NODE returns the basis nodes for any available element.
%
%
%  Modified:
%
%    04 January 1999
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, character ( len = * ) CODE, identifies the element desired.
%    Legal values include 'Q4', 'Q8', 'Q9', 'Q12', 'Q16', 'QL',
%    'T3', 'T6' and 'T10'.
%
%    Output, integer N, the number of nodes in the element.
%
%    Output, real R(N), S(N), the coordinates of the basis nodes.
%
%    Output, real AREA, the area of the element.
%

r_shape=size(r);r=reshape(r,1,[]);
s_shape=size(s);s=reshape(s,1,[]);
%
if( strcmp(deblank(code),deblank('Q4')) )
[ n, r, s, area ]=node_q4( n, r, s, area );
elseif( strcmp(deblank(code),deblank('Q8')) ) ;
[ n, r, s, area ]=node_q8( n, r, s, area );
elseif( strcmp(deblank(code),deblank('Q9')) ) ;
[ n, r, s, area ]=node_q9( n, r, s, area );
elseif( strcmp(deblank(code),deblank('Q12')) ) ;
[ n, r, s, area ]=node_q12( n, r, s, area );
elseif( strcmp(deblank(code),deblank('Q16')) ) ;
[ n, r, s, area ]=node_q16( n, r, s, area );
elseif( strcmp(deblank(code),deblank('QL')) ) ;
[ n, r, s, area ]=node_ql( n, r, s, area );
elseif( strcmp(deblank(code),deblank('T3')) ) ;
[ n, r, s, area ]=node_t3( n, r, s, area );
elseif( strcmp(deblank(code),deblank('T6')) ) ;
[ n, r, s, area ]=node_t6( n, r, s, area );
elseif( strcmp(deblank(code),deblank('T10')) ) ;
[ n, r, s, area ]=node_t10( n, r, s, area );
else;
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'NODE - Fatal error!');
writef(1,['%s','%s','\n'], '  Illegal value of CODE = ', code);
error(['stop encountered in original fortran code  ',char(10),';']);
end;
r_shape=zeros(r_shape);r_shape(:)=r(1:numel(r_shape));r=r_shape;
s_shape=zeros(s_shape);s_shape(:)=s(1:numel(s_shape));s=s_shape;
return;
end
function [n, r, s, area]=node_q4( n, r, s, area );
%
%*******************************************************************************
%
%! NODE_Q4 returns the basis nodes for a 4 node quadrilateral.
%
%
%  Diagram:
%
%    |
%    1  3-------4
%    |  |       |
%    |  |       |
%    S  |       |
%    |  |       |
%    |  |       |
%    0  1-------2
%    |
%    +--0---R---1-->
%
%  Modified:
%
%    05 January 1999
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Output, integer N, the number of nodes in the element.
%
%    Output, real R(4), S(4), the coordinates of the basis nodes.
%
%    Output, real AREA, the area of the element.
%
%

n = 4;
r(1) = 0.0;
s(1) = 0.0;
r(2) = 1.0;
s(2) = 0.0;
r(3) = 0.0;
s(3) = 1.0;
r(4) = 1.0;
s(4) = 1.0;
area = 1.0;
return;
end
function [n, r, s, area]=node_q8( n, r, s, area );
%
%*******************************************************************************
%
%! NODE_Q8 returns the basis nodes for an 8 node quadrilateral.
%
%
%  Comment:
%
%    This element is known as the quadratic 'serendipity' element.
%
%  Diagram:
%
%    |
%    1  6---7---8
%    |  |       |
%    |  |       |
%    S  4       5
%    |  |       |
%    |  |       |
%    0  1---2---3
%    |
%    +--0---R---1-->
%
%  Modified:
%
%    05 January 1999
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Output, integer N, the number of nodes in the element.
%
%    Output, real R(8), S(8), the coordinates of the basis nodes.
%
%    Output, real AREA, the area of the element.
%
%

n = 8;
r(1) = 0.0;
s(1) = 0.0;
r(2) = 0.5;
s(2) = 0.0;
r(3) = 1.0;
s(3) = 0.0;
r(4) = 0.0;
s(4) = 0.5;
r(5) = 1.0;
s(5) = 0.5;
r(6) = 0.0;
s(6) = 1.0;
r(7) = 0.5;
s(7) = 1.0;
r(8) = 1.0;
s(8) = 1.0;
area = 1.0;
return;
end
function [n, r, s, area]=node_q9( n, r, s, area );
%
%*******************************************************************************
%
%! NODE_Q9 returns the basis nodes for a 9 node quadrilateral.
%
%
%  Diagram:
%
%    |
%    1  7---8---9
%    |  |   :   |
%    |  |   :   |
%    S  4...5...6
%    |  |   :   |
%    |  |   :   |
%    0  1---2---3
%    |
%    +--0--R--1-->
%
%  Modified:
%
%    05 January 1999
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Output, integer N, the number of nodes in the element.
%
%    Input, real R(9), S(9), the coordinates of the basis nodes.
%
%    Output, real AREA, the area of the element.
%
%

n = 9;
r(1) = 0.0;
s(1) = 0.0;
r(2) = 0.5;
s(2) = 0.0;
r(3) = 1.0;
s(3) = 0.0;
r(4) = 0.0;
s(4) = 0.5;
r(5) = 0.5;
s(5) = 0.5;
r(6) = 1.0;
s(6) = 0.5;
r(7) = 0.0;
s(7) = 1.0;
r(8) = 0.5;
s(8) = 1.0;
r(9) = 1.0;
s(9) = 1.0;
area = 1.0;
return;
end
function [n, r, s, area]=node_q12( n, r, s, area );
%
%*******************************************************************************
%
%! NODE_Q12 returns the basis nodes for a 12 node quadrilateral.
%
%
%  Comment:
%
%    This element is known as the cubic 'serendipity' element.
%
%  Diagram:
%
%    |
%    1  9-10-11-12
%    |  |        |
%    |  7        8
%    S  |        |
%    |  5        6
%    |  |        |
%    0  1--2--3--4
%    |
%    +--0---R---1-->
%
%  Modified:
%
%    04 January 1999
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Output, integer N, the number of nodes in the element.
%
%    Output, real R(12), S(12), the coordinates of the basis nodes.
%
%    Output, real AREA, the area of the element.
%
persistent a b c d ; 

if isempty(a), a=0; end;
if isempty(b), b=0; end;
if isempty(c), c=0; end;
if isempty(d), d=0; end;
%
n = 12;
a = 0.0;
b = 1.0 ./ 3.0;
c = 2.0 ./ 3.0;
d = 1.0;
r(1) = a;
s(1) = a;
r(2) = b;
s(2) = a;
r(3) = c;
s(3) = a;
r(4) = d;
s(4) = a;
r(5) = a;
s(5) = b;
r(6) = d;
s(6) = b;
r(7) = a;
s(7) = c;
r(8) = d;
s(8) = c;
r(9) = a;
s(9) = d;
r(10) = b;
s(10) = d;
r(11) = c;
s(11) = d;
r(12) = d;
s(12) = d;
area = 1.0;
return;
end
function [n, r, s, area]=node_q16( n, r, s, area );
%
%*******************************************************************************
%
%! NODE_Q16 returns the basis nodes for a 16 node quadrilateral.
%
%
%  Diagram:
%
%    |
%    1 13--14--15--16
%    |  |   :   :   |
%    |  |   :   :   |
%    |  9..10..11..12
%    S  |   :   :   |
%    |  |   :   :   |
%    |  5...6...7...8
%    |  |   :   :   |
%    |  |   :   :   |
%    0  1---2---3---4
%    |
%    +--0-----R-----1-->
%
%  Modified:
%
%    14 October 1998
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Output, integer N, the number of nodes in the element.
%
%    Output, real R(16), S(16), the coordinates of the basis nodes.
%
%    Output, real AREA, the area of the element.
%
persistent i j k ; 

if isempty(i), i=0; end;
if isempty(j), j=0; end;
if isempty(k), k=0; end;
%
n = 16;
k = 0;
for i = 0: 3;
for j = 0: 3;
k = fix(k + 1);
r(k) = real( j ) ./ 3.0;
s(k) = real( i ) ./ 3.0;
end; j = fix(3+1);
end; i = fix(3+1);
area = 1.0;
return;
end
function [n, r, s, area]=node_ql( n, r, s, area );
%
%*******************************************************************************
%
%! NODE_QL returns the basis nodes for a quadratic/linear.
%
%
%  Diagram:
%
%    |
%    1  4---5---6
%    |  |       |
%    |  |       |
%    S  |       |
%    |  |       |
%    |  |       |
%    0  1---2---3
%    |
%    +--0---R---1-->
%
%  Modified:
%
%    05 January 1999
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Output, integer N, the number of nodes in the element.
%
%    Output, real R(6), S(6), the coordinates of the basis nodes.
%
%    Output, real AREA, the area of the element.
%
%

n = 6;
r(1) = 0.0;
s(1) = 0.0;
r(2) = 0.5;
s(2) = 0.0;
r(3) = 1.0;
s(3) = 0.0;
r(4) = 0.0;
s(4) = 1.0;
r(5) = 0.5;
s(5) = 1.0;
r(6) = 1.0;
s(6) = 1.0;
area = 1.0;
return;
end
function [n, r, s, area]=node_t3( n, r, s, area );
%
%*******************************************************************************
%
%! NODE_T3 returns the basis nodes for the 3 node triangle.
%
%
%  Diagram:
%
%    |
%    1  3
%    |  |\
%    |  | \
%    S  |  \
%    |  |   \
%    |  |    \
%    0  1-----2
%    |
%    +--0--R--1-->
%
%  Modified:
%
%    05 January 1999
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Output, integer N, the number of nodes in the element.
%
%    Output, real R(3), S(3), the coordinates of the basis nodes.
%
%    Output, real AREA, the area of the element.
%

n = 3;
r(1) = 0.0;
s(1) = 0.0;
r(2) = 1.0;
s(2) = 0.0;
r(3) = 0.0;
s(3) = 1.0;
area = 0.5;
return;
end
function [n, r, s, area]=node_t6( n, r, s, area );
%
%*******************************************************************************
%
%! NODE_T6 returns the basis nodes for a 6 node triangle.
%
%
%  Diagram:
%
%    |
%    1  6
%    |  |\
%    |  | \
%    S  4  5
%    |  |   \
%    |  |    \
%    0  1--2--3
%    |
%    +--0--R--1-->
%
%  Modified:
%
%    05 January 1999
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Output, integer N, the number of nodes in the element.
%
%    Output, real R(6), S(6), the coordinates of the basis nodes.
%
%    Output, real AREA, the area of the element.
%
%

n = 6;
r(1) = 0.0;
s(1) = 0.0;
r(2) = 0.5;
s(2) = 0.0;
r(3) = 1.0;
s(3) = 0.0;
r(4) = 0.0;
s(4) = 0.5;
r(5) = 0.5;
s(5) = 0.5;
r(6) = 0.0;
s(6) = 1.0;
area = 0.5;
return;
end
function [n, r, s, area]=node_t10( n, r, s, area );
%
%*******************************************************************************
%
%! NODE_T10 returns the basis nodes for a 10 node triangle.
%
%
%  Diagram:
%
%    |
%    1  10
%    |  |\
%    |  | \
%    |  8  9
%    |  |   \
%    S  |    \
%    |  5  6  7
%    |  |      \
%    |  |       \
%    0  1--2--3--4
%    |
%    +--0----R---1-->
%
%  Modified:
%
%    05 January 1999
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Output, integer N, the number of nodes in the element.
%
%    Output, real R(10), S(10), the coordinates of the basis nodes.
%
%    Output, real AREA, the area of the element.
%
%

n = 10;
r(1) = 0.0;
s(1) = 0.0;
r(2) = 1.0 ./ 3.0;
s(2) = 0.0;
r(3) = 2.0 ./ 3.0;
s(3) = 0.0;
r(4) = 1.0;
s(4) = 0.0;
r(5) = 0.0;
s(5) = 1.0 ./ 3.0;
r(6) = 1.0 ./ 3.0;
s(6) = 1.0 ./ 3.0;
r(7) = 2.0 ./ 3.0;
s(7) = 1.0 ./ 3.0;
r(8) = 0.0;
s(8) = 2.0 ./ 3.0;
r(9) = 1.0 ./ 3.0;
s(9) = 2.0 ./ 3.0;
r(10) = 0.0;
s(10) = 1.0;
area = 0.5;
return;
end
function [code, n, rexp, sexp]=poly( code, n, rexp, sexp );
%
%*******************************************************************************
%
%! POLY returns the polynomials associated with any available element.
%
%
%  Modified:
%
%    04 January 1999
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, character ( len = * ) CODE, identifies the element desired.
%    Legal values include 'Q4', 'Q8', 'Q9', 'Q12', 'Q16', 'QL', 'T3',
%    'T6' and 'T10'.
%
%    Output, integer N, the number of polynomials.
%
%    Output, integer REXP(N), SEXP(N), the powers of R and S associated
%    with each polynomial.
%

rexp_shape=size(rexp);rexp=reshape(rexp,1,[]);
sexp_shape=size(sexp);sexp=reshape(sexp,1,[]);
%
if( strcmp(deblank(code),deblank('Q4')) )
[ n, rexp, sexp ]=poly_q4( n, rexp, sexp );
elseif( strcmp(deblank(code),deblank('Q8')) ) ;
[ n, rexp, sexp ]=poly_q8( n, rexp, sexp );
elseif( strcmp(deblank(code),deblank('Q9')) ) ;
[ n, rexp, sexp ]=poly_q9( n, rexp, sexp );
elseif( strcmp(deblank(code),deblank('Q12')) ) ;
[ n, rexp, sexp ]=poly_q12( n, rexp, sexp );
elseif( strcmp(deblank(code),deblank('Q16')) ) ;
[ n, rexp, sexp ]=poly_q16( n, rexp, sexp );
elseif( strcmp(deblank(code),deblank('QL')) ) ;
[ n, rexp, sexp ]=poly_ql( n, rexp, sexp );
elseif( strcmp(deblank(code),deblank('T3')) ) ;
[ n, rexp, sexp ]=poly_t3( n, rexp, sexp );
elseif( strcmp(deblank(code),deblank('T6')) ) ;
[ n, rexp, sexp ]=poly_t6( n, rexp, sexp );
elseif( strcmp(deblank(code),deblank('T10')) ) ;
[ n, rexp, sexp ]=poly_t10( n, rexp, sexp );
else;
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'POLY - Fatal error!');
writef(1,['%s','%s','\n'], '  Illegal value of CODE = ', code);
error(['stop encountered in original fortran code  ',char(10),';']);
end;
rexp_shape=zeros(rexp_shape);rexp_shape(:)=rexp(1:numel(rexp_shape));rexp=rexp_shape;
sexp_shape=zeros(sexp_shape);sexp_shape(:)=sexp(1:numel(sexp_shape));sexp=sexp_shape;
return;
end
function [n, rexp, sexp]=poly_q4( n, rexp, sexp );
%
%*******************************************************************************
%
%! POLY_Q4 returns the polynomials associated with a 4 node quadrilateral.
%
%
%  Diagram:
%
%    |
%    1  3-----4
%    |  |     |
%    |  |     |
%    S  |     |
%    |  |     |
%    |  |     |
%    0  1-----2
%    |
%    +--0--R--1-->
%
%  Formula:
%
%    Given coefficients A(I), the polynomial interpolant at (R,S) is
%
%      P(R,S) = SUM ( I = 1 to N ) A(I) * R**REXP(I) * S**SEXP(I)
%
%  Modified:
%
%    04 January 1999
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Output, integer N, the number of polynomials.
%
%    Output, integer REXP(N), SEXP(N), the powers of R and S associated
%    with each polynomial.
%
persistent n_internal ; 

if isempty(n_internal), n_internal = 4; end;
%
%
n = fix(n_internal);
rexp(1) = 0;
sexp(1) = 0;
rexp(2) = 0;
sexp(2) = 1;
rexp(3) = 1;
sexp(3) = 0;
rexp(4) = 1;
sexp(4) = 1;
return;
end
function [n, rexp, sexp]=poly_q8( n, rexp, sexp );
%
%*******************************************************************************
%
%! POLY_Q8 returns the polynomials associated with an 8 node quadrilateral.
%
%
%  Diagram:
%
%    |
%    1  6---7---8
%    |  |       |
%    |  |       |
%    S  4       5
%    |  |       |
%    |  |       |
%    0  1---2---3
%    |
%    +--0--R--1-->
%
%  Formula:
%
%    Given coefficients A(I), the polynomial interpolant at (R,S) is
%
%      P(R,S) = SUM ( I = 1 to N ) A(I) * R**REXP(I) * S**SEXP(I)
%
%  Modified:
%
%    04 January 1999
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Output, integer N, the number of polynomials.
%
%    Output, integer REXP(N), SEXP(N), the powers of R and S associated
%    with each polynomial.
%
persistent n_internal ; 

if isempty(n_internal), n_internal = 8; end;
%
%
n = fix(n_internal);
rexp(1) = 0;
sexp(1) = 0;
rexp(2) = 0;
sexp(2) = 1;
rexp(3) = 0;
sexp(3) = 2;
rexp(4) = 1;
sexp(4) = 0;
rexp(5) = 1;
sexp(5) = 1;
rexp(6) = 1;
sexp(6) = 2;
rexp(7) = 2;
sexp(7) = 0;
rexp(8) = 2;
sexp(8) = 1;
return;
end
function [n, rexp, sexp]=poly_q9( n, rexp, sexp );
%
%*******************************************************************************
%
%! POLY_Q9 returns the polynomials associated with a 9 node quadrilateral.
%
%
%  Diagram:
%
%    |
%    1  7---8---9
%    |  |   :   |
%    |  |   :   |
%    S  4...5...6
%    |  |   :   |
%    |  |   :   |
%    0  1---2---3
%    |
%    +--0--R--1-->
%
%  Formula:
%
%    Given coefficients A(I), the polynomial interpolant at (R,S) is
%
%      P(R,S) = SUM ( I = 1 to N ) A(I) * R**REXP(I) * S**SEXP(I)
%
%  Modified:
%
%    05 January 1999
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Output, integer N, the number of polynomials.
%
%    Output, integer REXP(N), SEXP(N), the powers of R and S associated
%    with each polynomial.
%
persistent n_internal ; 

if isempty(n_internal), n_internal = 9; end;
%
%
n = fix(n_internal);
rexp(1) = 0;
sexp(1) = 0;
rexp(2) = 0;
sexp(2) = 1;
rexp(3) = 0;
sexp(3) = 2;
rexp(4) = 1;
sexp(4) = 0;
rexp(5) = 1;
sexp(5) = 1;
rexp(6) = 1;
sexp(6) = 2;
rexp(7) = 2;
sexp(7) = 0;
rexp(8) = 2;
sexp(8) = 1;
rexp(9) = 2;
sexp(9) = 2;
return;
end
function [n, rexp, sexp]=poly_q12( n, rexp, sexp );
%
%*******************************************************************************
%
%! POLY_Q12 returns the polynomials associated with a 12 node quadrilateral.
%
%
%  Diagram:
%
%    |
%    1  9--10--11--12
%    |  |           |
%    |  |           |
%    |  7           8
%    S  |           |
%    |  |           |
%    |  5           6
%    |  |           |
%    |  |           |
%    0  1---2---3---4
%    |
%    +--0-----R-----1-->
%
%  Formula:
%
%    Given coefficients A(I), the polynomial interpolant at (R,S) is
%
%      P(R,S) = SUM ( I = 1 to N ) A(I) * R**REXP(I) * S**SEXP(I)
%
%  Modified:
%
%    04 January 1999
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Output, integer N, the number of polynomials.
%
%    Output, integer REXP(N), SEXP(N), the powers of R and S associated
%    with each polynomial.
%
persistent n_internal ; 

if isempty(n_internal), n_internal = 12; end;
%
%
n = fix(n_internal);
rexp(1) = 0;
sexp(1) = 0;
rexp(2) = 0;
sexp(2) = 1;
rexp(3) = 0;
sexp(3) = 2;
rexp(4) = 0;
sexp(4) = 3;
rexp(5) = 1;
sexp(5) = 0;
rexp(6) = 1;
sexp(6) = 1;
rexp(7) = 1;
sexp(7) = 2;
rexp(8) = 1;
sexp(8) = 3;
rexp(9) = 2;
sexp(9) = 0;
rexp(10) = 2;
sexp(10) = 1;
rexp(11) = 3;
sexp(11) = 0;
rexp(12) = 3;
sexp(12) = 1;
return;
end
function [n, rexp, sexp]=poly_q16( n, rexp, sexp );
%
%*******************************************************************************
%
%! POLY_Q16 returns the polynomials associated with a 16 node quadrilateral.
%
%
%  Diagram:
%
%    |
%    1 13--14--15--16
%    |  |   :   :   |
%    |  |   :   :   |
%    |  9..10..11..12
%    S  |   :   :   |
%    |  |   :   :   |
%    |  5...6...7...8
%    |  |   :   :   |
%    |  |   :   :   |
%    0  1---2---3---4
%    |
%    +--0-----R-----1-->
%
%  Formula:
%
%    Given coefficients A(I), the polynomial interpolant at (R,S) is
%
%      P(R,S) = SUM ( I = 1 to N ) A(I) * R**REXP(I) * S**SEXP(I)
%
%  Modified:
%
%    04 January 1999
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Output, integer N, the number of polynomials.
%
%    Output, integer REXP(N), SEXP(N), the powers of R and S associated
%    with each polynomial.
%
persistent n_internal ; 

if isempty(n_internal), n_internal = 16; end;
%
%
n = fix(n_internal);
rexp(1) = 0;
sexp(1) = 0;
rexp(2) = 0;
sexp(2) = 1;
rexp(3) = 0;
sexp(3) = 2;
rexp(4) = 0;
sexp(4) = 3;
rexp(5) = 1;
sexp(5) = 0;
rexp(6) = 1;
sexp(6) = 1;
rexp(7) = 1;
sexp(7) = 2;
rexp(8) = 1;
sexp(8) = 3;
rexp(9) = 2;
sexp(9) = 0;
rexp(10) = 2;
sexp(10) = 1;
rexp(11) = 2;
sexp(11) = 2;
rexp(12) = 2;
sexp(12) = 3;
rexp(13) = 3;
sexp(13) = 0;
rexp(14) = 3;
sexp(14) = 1;
rexp(15) = 3;
sexp(15) = 2;
rexp(16) = 3;
sexp(16) = 3;
return;
end
function [n, rexp, sexp]=poly_ql( n, rexp, sexp );
%
%*******************************************************************************
%
%! POLY_QL returns the polynomials for a quadratic/linear quadrilateral.
%
%
%  Diagram:
%
%    |
%    1  4---5---6
%    |  |       |
%    |  |       |
%    S  |       |
%    |  |       |
%    |  |       |
%    0  1---2---3
%    |
%    +--0---R---1-->
%
%  Formula:
%
%    Given coefficients A(I), the polynomial interpolant at (R,S) is
%
%      P(R,S) = SUM ( I = 1 to N ) A(I) * R**REXP(I) * S**SEXP(I)
%
%  Modified:
%
%    04 January 1999
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Output, integer N, the number of polynomials.
%
%    Output, integer REXP(N), SEXP(N), the powers of R and S associated
%    with each polynomial.
%
persistent n_internal ; 

if isempty(n_internal), n_internal = 6; end;
%
%
n = fix(n_internal);
rexp(1) = 0;
sexp(1) = 0;
rexp(2) = 0;
sexp(2) = 1;
rexp(3) = 1;
sexp(3) = 0;
rexp(4) = 1;
sexp(4) = 1;
rexp(5) = 2;
sexp(5) = 0;
rexp(6) = 2;
sexp(6) = 1;
return;
end
function [n, rexp, sexp]=poly_t3( n, rexp, sexp );
%
%*******************************************************************************
%
%! POLY_T3 returns the polynomials associated with a 3 node triangle.
%
%
%  Diagram:
%
%    |
%    1  3
%    |  |\
%    |  | \
%    S  |  \
%    |  |   \
%    |  |    \
%    0  1-----2
%    |
%    +--0--R--1-->
%
%  Formula:
%
%    Given coefficients A(I), the polynomial interpolant at (R,S) is
%
%      P(R,S) = SUM ( I = 1 to N ) A(I) * R**REXP(I) * S**SEXP(I)
%
%  Modified:
%
%    04 January 1999
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Output, integer N, the number of polynomials.
%
%    Output, integer REXP(N), SEXP(N), the powers of R and S associated
%    with each polynomial.
%
persistent n_internal ; 

if isempty(n_internal), n_internal = 3; end;
%
%
n = fix(n_internal);
rexp(1) = 0;
sexp(1) = 0;
rexp(2) = 0;
sexp(2) = 1;
rexp(3) = 1;
sexp(3) = 0;
return;
end
function [n, rexp, sexp]=poly_t6( n, rexp, sexp );
%
%*******************************************************************************
%
%! POLY_T6 returns the polynomials associated with a 6 node triangle.
%
%
%  Diagram:
%
%    |
%    1  6
%    |  |\
%    |  | \
%    S  4  5
%    |  |   \
%    |  |    \
%    0  1--2--3
%    |
%    +--0--R--1-->
%
%  Formula:
%
%    Given coefficients A(I), the polynomial interpolant at (R,S) is
%
%      P(R,S) = SUM ( I = 1 to N ) A(I) * R**REXP(I) * S**SEXP(I)
%
%  Modified:
%
%    05 January 1999
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Output, integer N, the number of polynomials.
%
%    Output, integer REXP(N), SEXP(N), the powers of R and S associated
%    with each polynomial.
%
persistent n_internal ; 

if isempty(n_internal), n_internal = 6; end;
%
%
n = fix(n_internal);
rexp(1) = 0;
sexp(1) = 0;
rexp(2) = 0;
sexp(2) = 1;
rexp(3) = 0;
sexp(3) = 2;
rexp(4) = 1;
sexp(4) = 0;
rexp(5) = 1;
sexp(5) = 1;
rexp(6) = 2;
sexp(6) = 0;
return;
end
function [n, rexp, sexp]=poly_t10( n, rexp, sexp );
%
%*******************************************************************************
%
%! POLY_T10 returns the polynomials associated with a 10 node triangle.
%
%
%  Diagram:
%
%    |
%    1  10
%    |  |\
%    |  | \
%    |  8  9
%    |  |   \
%    S  |    \
%    |  5  6  7
%    |  |      \
%    |  |       \
%    0  1--2--3--4
%    |
%    +--0----R---1-->
%
%  Formula:
%
%    Given coefficients A(I), the polynomial interpolant at (R,S) is
%
%      P(R,S) = SUM ( I = 1 to N ) A(I) * R**REXP(I) * S**SEXP(I)
%
%  Modified:
%
%    05 January 1999
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Output, integer N, the number of polynomials.
%
%    Output, integer REXP(N), SEXP(N), the powers of R and S associated
%    with each polynomial.
%
persistent n_internal ; 

if isempty(n_internal), n_internal = 10; end;
%
%
n = fix(n_internal);
rexp(1) = 0;
sexp(1) = 0;
rexp(2) = 0;
sexp(2) = 1;
rexp(3) = 0;
sexp(3) = 2;
rexp(4) = 0;
sexp(4) = 3;
rexp(5) = 1;
sexp(5) = 0;
rexp(6) = 1;
sexp(6) = 1;
rexp(7) = 1;
sexp(7) = 2;
rexp(8) = 2;
sexp(8) = 0;
rexp(9) = 2;
sexp(9) = 1;
rexp(10) = 3;
sexp(10) = 0;
return;
end
function [norder, xtab, weight]=quad_1d( norder, xtab, weight );
%
%*******************************************************************************
%
%! QUAD_1D computes abscissas and weights for Gauss-Legendre quadrature.
%
%
%  Integration interval:
%
%    [ -1, 1 ]
%
%  Weight function:
%
%    1.
%
%  Integral to approximate:
%
%    INTEGRAL ( -1 <= X <= 1 ) F(X) dX.
%
%  Approximate integral:
%
%    SUM ( I = 1 to NORDER ) WEIGHT(I) * F ( XTAB(I) ).
%
%  Modified:
%
%    04 November 1998
%
%  Parameters:
%
%    Input, integer NORDER, the order of the rule.
%    NORDER must be greater than 0.
%
%    Output, real XTAB(NORDER), the abscissas of the rule.
%
%    Output, real WEIGHT(NORDER), the weights of the rule.
%    The weights are positive, symmetric, and should sum to 2.
%
persistent d1 d2pn d3pn d4pn dp dpn e1 fx h i iback k m mp1mi ncopy nmove p pi pk pkm1 pkp1 t u v x0 xtemp ; 

if isempty(pi), pi = 3.14159265358979323846264338327950288419716939937510; end;
%
%
if isempty(d1), d1=0; end;
if isempty(d2pn), d2pn=0; end;
if isempty(d3pn), d3pn=0; end;
if isempty(d4pn), d4pn=0; end;
if isempty(dp), dp=0; end;
if isempty(dpn), dpn=0; end;
if isempty(e1), e1=0; end;
if isempty(fx), fx=0; end;
if isempty(h), h=0; end;
if isempty(i), i=0; end;
if isempty(iback), iback=0; end;
if isempty(k), k=0; end;
if isempty(m), m=0; end;
if isempty(mp1mi), mp1mi=0; end;
if isempty(ncopy), ncopy=0; end;
if isempty(nmove), nmove=0; end;
if isempty(p), p=0; end;
if isempty(pk), pk=0; end;
if isempty(pkm1), pkm1=0; end;
if isempty(pkp1), pkp1=0; end;
if isempty(t), t=0; end;
if isempty(u), u=0; end;
if isempty(v), v=0; end;
if isempty(x0), x0=0; end;
if isempty(xtemp), xtemp=0; end;
%
if( norder < 1 )
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'QUAD1D - Fatal error!');
writef(1,['%s %0.15g \n'], '  Illegal value of NORDER = ', norder);
error(['stop encountered in original fortran code  ',char(10),';']);
end;

e1 = real( norder .*( norder + 1 ) );

m =fix(fix(( norder + 1 ) ./ 2));

for i = 1:fix(( norder + 1 ) ./ 2);

mp1mi = fix(m + 1 - i);
t = real( 4 .* i - 1 ) .* pi ./ real( 4 .* norder + 2 );
x0 = cos(t) .*( 1.0e+00 -( 1.0e+00 - 1.0e+00 ./real( norder ) ) ./ real( 8 .* norder.^2) );

pkm1 = 1.0e+00;
pk = x0;
for k = 2: norder;
pkp1 = 2.0e+00 .* x0 .* pk - pkm1 -( x0 .* pk - pkm1 ) ./ real( k );
pkm1 = pk;
pk = pkp1;
end; k = fix(norder+1);

d1 = real( norder ) .*( pkm1 - x0 .* pk );
dpn = d1 ./( 1.0e+00 - x0.^2 );
d2pn =( 2.0e+00 .* x0 .* dpn - e1 .* pk ) ./( 1.0e+00 - x0.^2 );
d3pn =( 4.0e+00 .* x0 .* d2pn +( 2.0e+00 - e1 ) .* dpn ) ./( 1.0e+00 - x0.^2 );
d4pn =( 6.0e+00 .* x0 .* d3pn +( 6.0e+00 - e1 ) .* d2pn ) ./( 1.0e+00 - x0.^2 );
u = pk ./ dpn;
v = d2pn ./ dpn;
%
%  Initial approximation H:
%
h = - u .*( 1.0e+00 + 0.5e+00 .* u .*( v + u .*( v.^2 - d3pn./( 3.0e+00 .* dpn ) ) ) );
%
%  Refine H using one step of Newton's method:
%
p = pk + h .*( dpn + 0.5e+00 .* h .*( d2pn + h ./ 3.0e+00.*( d3pn + 0.25e+00 .* h .* d4pn ) ) );
dp = dpn + h .*( d2pn + 0.5e+00 .* h .*( d3pn + h .* d4pn ./ 3.0e+00 ) );
h = h - p ./ dp;

xtemp = x0 + h;
xtab(mp1mi) = xtemp;

fx = d1 - h .* e1 .*( pk + 0.5e+00 .* h .*( dpn + h ./ 3.0e+00.*( d2pn + 0.25e+00 .* h .*( d3pn + 0.2e+00 .* h .* d4pn ) ) ) );
weight(mp1mi) = 2.0e+00 .*( 1.0e+00 - xtemp.^2 ) ./ fx.^2;

end; i =fix(fix(( norder + 1 ) ./ 2)+1);

if( rem( norder, 2 ) == 1 )
xtab(1) = 0.0e+00;
end;
%
%  Shift the data up.
%
nmove =fix(fix(( norder + 1 ) ./ 2));
ncopy = fix(norder - nmove);
for i = 1: nmove;
iback = fix(norder + 1 - i);
xtab(iback) = xtab(iback-ncopy);
weight(iback) = weight(iback-ncopy);
end; i = fix(nmove+1);
%
%  Reflect values for the negative abscissas.
%
for i = 1: norder - nmove;
xtab(i) = - xtab(norder+1-i);
weight(i) = weight(norder+1-i);
end; i = fix(norder - nmove+1);

return;
end
function [nrule, maxorder, norder, area, rquad, squad, wquad]=quad_t( nrule, maxorder, norder, area, rquad, squad, wquad );
%
%*******************************************************************************
%
%! QUAD_T returns a quadrature rule for a triangle.
%
%
%  Formula:
%
%    The quadrature rule approximates
%
%      Integral ( 0 <= R <= 1, 0 <= S <= 1 - R ) F(R,S) dR dS
%
%    by
%
%      AREA * Sum ( I = 1 to NORDER ) WQUAD(I) * F(RQUAD(I),SQUAD(I)).
%
%  Modified:
%
%    06 October 1998
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, integer NRULE, the desired Gauss rule.
%      1, NORDER = 1;
%      2, NORDER = 3.
%
%    Input, integer MAXORDER, the maximum order for which storage is
%    available.
%
%    Output, integer NORDER, the order of the Gauss rule.
%
%    Output, real AREA, the area of the element.
%
%    Output, real RQUAD(NORDER), SQUAD(NORDER), the quadrature abscissas.
%
%    Output, real WQUAD(NORDER), the quadrature weights.
%
%
%

area = 0.5;
if( nrule == 1 )
norder = 1;
rquad(1) = 1.0 ./ 3.0;
squad(1) = 1.0 ./ 3.0;
wquad(1) = 1.0;
elseif( nrule == 2 ) ;
rquad(1) = 1.0;
squad(1) = 0.0;
wquad(1) = 1.0 ./ 3.0;
rquad(2) = 0.0;
squad(2) = 1.0;
wquad(2) = 1.0 ./ 3.0;
rquad(3) = 0.0;
squad(3) = 0.0;
wquad(3) = 1.0 ./ 3.0;
else;
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'QUADT - Fatal error!');
writef(1,['%s %0.15g \n'], '  Unacceptable value of NRULE = ', nrule);
error(['stop encountered in original fortran code  ',char(10),';']);
end;

return;
end
function [type, ve, vn, vne, vnw, vs, vse, vsw, vw, vterp]=serene( type, ve, vn, vne, vnw, vs, vse, vsw, vw, vterp );
%
%*******************************************************************************
%
%! SERENE interpolates data using a serendipity quadrilateral.
%
%
%  Modified:
%
%    02 March 1999
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, character ( len = 2 ) TYPE, tells SERENE the geometry of the
%    finite element that surrounds the point of interest.  The options
%    are displayed in the following table, which suggests the meaning
%    of each option by its position:
%
%        |   |
%      NW* N * NE
%        |   |
%     -*-*-*-*-*-
%        |   |
%      W * C * E
%        |   |
%     -*-*-*-*-*-
%        |   |
%      SW* S * SE
%        |   |
%
%    Input, real VE, VN, VNE, VNW, VS, VSE, VSW, VW,
%    are the values of the function at the nodes to the east,
%    north, northeast, northwest, south, southeast, southwest and
%    west of the point of interest.  If the finite element is of
%    type 'C', then all 8 values are needed.  However, if the
%    finite element is of type 'SE', for instance, then only three
%    values are needed, namely VE, VN, and VNW, since these are
%    the only node positions defined in such a finite element.
%
%    Output, real VTERP, the interpolated value of the
%    function at the point of interest.
%
persistent eta pe pn pne pnw ps pse psw pw xsi ; 

if isempty(eta), eta=0; end;
if isempty(pe), pe=0; end;
if isempty(pn), pn=0; end;
if isempty(pne), pne=0; end;
if isempty(pnw), pnw=0; end;
if isempty(ps), ps=0; end;
if isempty(pse), pse=0; end;
if isempty(psw), psw=0; end;
if isempty(pw), pw=0; end;
if isempty(xsi), xsi=0; end;
%
%  To make this routine more general, simply pass in the values of XSI
%  and ETA at which the interpolated value is desired.
%
%  By setting XSI = ETA = 0, we are asking for the interpolated value
%  at the center of the finite element.
%
xsi = 0.0;
eta = 0.0;
%
%  8 node center
%
%  Polynomial space is spanned by:
%         1
%       x    y
%    x^2  xy  y^2
%      x^2y xy^2
%
%
%    ^   1    4--7--3
%    |        !     !
%    E        !     !
%    T   0    8  X  6
%    A        !     !
%    |        !     !
%    V  -1    1--5--2
%
%            -1  0  1
%
%           <---XSI--->
%
if( strcmp(deblank(type),deblank('C')) )
psw = - 0.25 .*( 1.0 - xsi ) .*( 1.0 - eta ) .*( 1.0 + xsi + eta );
pse = - 0.25 .*( 1.0 + xsi ) .*( 1.0 - eta ) .*( 1.0 - xsi + eta );
pne = - 0.25 .*( 1.0 + xsi ) .*( 1.0 + eta ) .*( 1.0 - xsi - eta );
pnw = - 0.25 .*( 1.0 - xsi ) .*( 1.0 + eta ) .*( 1.0 + xsi - eta );
ps =    0.50 .*( 1.0 - xsi ) .*( 1.0 + xsi ) .*( 1.0 - eta );
pe =    0.50 .*( 1.0 + xsi ) .*( 1.0 + eta ) .*( 1.0 - eta );
pn =    0.50 .*( 1.0 - xsi ) .*( 1.0 + xsi ) .*( 1.0 + eta );
pw =    0.50 .*( 1.0 - xsi ) .*( 1.0 + eta ) .*( 1.0 - eta );
%
%  5 node side
%
%    ^   1
%    |
%    E
%    T   0    8  X  6
%    A        !     !
%    |        !     !
%    V  -1    1--5--2
%
%            -1  0  1
%
%           <---XSI--->
%
elseif( strcmp(deblank(type),deblank('N')) ) ;
psw =  0.5 .*( xsi - 1.0 ) .*( 1.0 + xsi + eta );
pse = -0.5 .*( xsi + 1.0 ) .*( 1.0 - xsi + eta );
ps =  -( xsi + 1.0 ) .*( xsi - 1.0 );
pe =   0.5 .*( xsi + 1.0 ) .*( eta + 1.0 );
pw =  -0.5 .*( xsi - 1.0 ) .*( eta + 1.0 );
%
%    ^   1    4--7
%    |        !
%    E        !
%    T   0    8  X
%    A        !
%    |        !
%    V  -1    1--5
%
%            -1  0  1
%
%           <---XSI--->
%
elseif( strcmp(deblank(type),deblank('E')) ) ;
pse =  0.5 .*( eta - 1.0 ) .*( 1.0 + xsi + eta );
pne = -0.5 .*( eta + 1.0 ) .*( 1.0 + xsi - eta );
ps =  -0.5 .*( xsi + 1.0 ) .*( eta - 1.0 );
pn =   0.5 .*( xsi + 1.0 ) .*( eta + 1.0 );
pw =  -( eta + 1.0 ) .*( eta - 1.0 );
%
%  5 node side
%
%    ^   1       7--3
%    |              !
%    E              !
%    T   0       X  6
%    A              !
%    |              !
%    V  -1       5--2
%
%            -1  0  1
%
%           <---XSI--->
%
elseif( strcmp(deblank(type),deblank('W')) ) ;
pse =   0.5 .*( eta - 1.0 ) .*( 1.0 - xsi + eta );
pne = - 0.5 .*( eta + 1.0 ) .*( 1.0 - xsi - eta );
ps =    0.5 .*( xsi - 1.0 ) .*( eta - 1.0 );
pe =  -( eta - 1.0 ) .*( eta + 1.0 );
pn =  - 0.5 .*( xsi - 1.0 ) .*( eta + 1.0 );
%
%  5 node side
%
%    ^   1    4--7--3
%    |        !     !
%    E        !     !
%    T   0    8  X  6
%    A
%    |
%    V  -1
%
%            -1  0  1
%
%           <---XSI--->
%
elseif( strcmp(deblank(type),deblank('S')) ) ;
pne = - 0.5 .*( xsi + 1.0 ) .*( 1.0 - xsi - eta );
pnw =   0.5 .*( xsi - 1.0 ) .*( 1.0 + xsi - eta );
pe =  - 0.5 .*( eta - 1.0 ) .*( xsi + 1.0 );
pn =  -( xsi + 1.0 ) .*( xsi - 1.0 );
pw =    0.5 .*( eta - 1.0 ) .*( xsi - 1.0 );
%
%  3 node corner
%
%  Polynomial space is spanned by:
%         1
%       x    y
%
%
%    ^   1
%    |
%    E
%    T   0    8  X
%    A        !
%    |        !
%    V  -1    1--5
%
%            -1  0  1
%
%           <---XSI--->
%
elseif( strcmp(deblank(type),deblank('NE')) ) ;
psw = - 1.0 - xsi - eta;
ps =    1.0 + xsi;
pw =    1.0       + eta;
%
%  3 node corner
%
%  Polynomial space is spanned by:
%         1
%       x    y
%
%    ^   1
%    |
%    E
%    T   0       X  6
%    A              !
%    |              !
%    V  -1       5--2
%
%            -1  0  1
%
%           <---XSI--->
%
elseif( strcmp(deblank(type),deblank('NW')) ) ;
pse = 1.0 + xsi - eta;
ps =  1.0 - xsi;
pe =  1.0       + eta;
%
%  3 node corner
%
%  Polynomial space is spanned by:
%         1
%       x    y
%
%
%    ^   1    4--7
%    |        !
%    E        !
%    T   0    8  X
%    A
%    |
%    V  -1
%
%            -1  0  1
%
%           <---XSI--->
%
elseif( strcmp(deblank(type),deblank('SE')) ) ;
pnw = - 1.0 - xsi + eta;
pn =    1.0 + xsi;
pw =    1.0       - eta;
%
%  3 node corner
%
%  Polynomial space is spanned by:
%         1
%       x    y
%
%    ^   1       7--3
%    |              !
%    E              !
%    T   0       X  6
%    A
%    |
%    V  -1
%
%            -1  0  1
%
%           <---XSI--->
%
elseif( strcmp(deblank(type),deblank('SW')) ) ;
pne = - 1.0 + xsi + eta;
pe =    1.0       - eta;
pn =    1.0 - xsi;
end;
vterp = vsw .* psw + vse .* pse + vne .* pne + vnw .* pnw+ vs .* ps + ve .* pe + vn .* pn + vw .* pw;
return;
end
function [a, lda, n, ipivot, info]=sge_fa( a, lda, n, ipivot, info );
%
%*******************************************************************************
%
%! SGE_FA factors a general matrix.
%
%
%  Note:
%
%    SGE_FA is a simplified version of the LINPACK routine SGEFA.
%
%  Parameters:
%
%    Input/output, real A(LDA,N), the matrix to be factored.
%    On output, A contains an upper triangular matrix and the multipliers
%    which were used to obtain it.  The factorization can be written
%    A = L * U, where L is a product of permutation and unit lower
%    triangular matrices and U is upper triangular.
%
%    Input, integer LDA, the leading dimension of the array.
%    LDA must be at least N.
%
%    Input, integer N, the order of the matrix.
%    N must be positive.
%
%    Output, integer IPIVOT(N), a vector of pivot indices.
%
%    Output, integer INFO, singularity flag.
%    0, no singularity detected.
%    nonzero, the factorization failed on the INFO-th step.
%
%
persistent i j k l t ; 

a_orig=a;a_shape=[lda,n];a=reshape([a_orig(1:min(prod(a_shape),numel(a_orig))),zeros(1,max(0,prod(a_shape)-numel(a_orig)))],a_shape);
if isempty(i), i=0; end;
if isempty(j), j=0; end;
if isempty(k), k=0; end;
if isempty(l), l=0; end;
if isempty(t), t=0; end;
%
info = 0;
%
for k = 1: n-1;
%
%  Find L, the index of the pivot row.
%
l = fix(k);
for i = k+1: n;
if( abs( a(i,k) ) > abs( a(l,k) ) )
l = fix(i);
end;
end; i = fix(n+1);
ipivot(k) = fix(l);
%
%  If the pivot index is zero, the algorithm has failed.
%
if( a(l,k) == 0.0 )
info = fix(k);
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'SGE_FA - Fatal error!');
writef(1,['%s %0.15g \n'], '  Zero pivot on step ', info);
a_orig(1:prod(a_shape))=a;a=a_orig;
return;
end;
%
%  Interchange rows L and K if necessary.
%
if( l ~= k )
t = a(l,k);
a(l,k) = a(k,k);
a(k,k) = t;
end;
%
%  Normalize the values that lie below the pivot entry A(K,K).
%
for i = k+1: n;
a(i,k) = - a(i,k) ./ a(k,k);
end; i = fix(n+1);
%
%  Row elimination with column indexing.
%
for j = k+1: n;
if( l ~= k )
t = a(l,j);
a(l,j) = a(k,j);
a(k,j) = t;
end;
for i = k+1: n;
a(i,j) = a(i,j) + a(i,k) .* a(k,j);
end; i = fix(n+1);
end; j = fix(n+1);
end; k = fix(n-1+1);
ipivot(n) = fix(n);
if( a(n,n) == 0.0 )
info = fix(n);
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'SGE_FA - Fatal error!');
writef(1,['%s %0.15g \n'], '  Zero pivot on step ', info);
end;
a_orig(1:prod(a_shape))=a;a=a_orig;
return;
end
function [a, lda, n, ipivot, work]=sge_inv( a, lda, n, ipivot, work );
%
%*******************************************************************************
%
%! SGE_INV computes the inverse of a matrix factored by SGE_FA.
%
%
%  Note:
%
%    SGE_INV is a simplified version of the LINPACK routine SGEDI.
%
%  Modified:
%
%    04 January 1999
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input/output, real A(LDA,N).
%    On input, the factor information computed by SGE_FA.
%    On output, the inverse matrix.
%
%    Input, integer LDA, the leading dimension of the array A,
%    which must be at least N.
%
%    Input, integer N, the order of the matrix A.
%
%    Input, integer IPIVOT(N), the pivot vector from SGE_FA.
%
%    Workspace, real WORK(N).
%
%
persistent i j k temp ; 

a_orig=a;a_shape=[lda,n];a=reshape([a_orig(1:min(prod(a_shape),numel(a_orig))),zeros(1,max(0,prod(a_shape)-numel(a_orig)))],a_shape);
if isempty(i), i=0; end;
if isempty(j), j=0; end;
if isempty(k), k=0; end;
if isempty(temp), temp=0; end;
%
%  Compute Inverse(U).
%
for k = 1: n;
a(k,k) = 1.0 ./ a(k,k);
for i = 1: k-1;
a(i,k) = - a(i,k) .* a(k,k);
end; i = fix(k-1+1);
for j = k + 1: n;
temp = a(k,j);
a(k,j) = 0.0;
for i = 1: k;
a(i,j) = a(i,j) + temp .* a(i,k);
end; i = fix(k+1);
end; j = fix(n+1);
end; k = fix(n+1);
%
%  Form Inverse(U) * Inverse(L).
%
for k = n - 1: -1: 1;
for i = k + 1: n;
work(i) = a(i,k);
a(i,k) = 0.0;
end; i = fix(n+1);
for j = k + 1: n;
for i = 1: n;
a(i,k) = a(i,k) + work(j) .* a(i,j);
end; i = fix(n+1);
end; j = fix(n+1);
if( ipivot(k) ~= k )
for i = 1: n;
temp = a(i,k);
a(i,k) = a(i,ipivot(k));
a(i,ipivot(k)) = temp;
end; i = fix(n+1);
end;
end; k = fix(1-1);
a_orig(1:prod(a_shape))=a;a=a_orig;
return;
end
function [code, n, r, s, t, dtdr, dtds]=shape( code, n, r, s, t, dtdr, dtds );
%
%*******************************************************************************
%
%! SHAPE evaluates shape functions for any available element.
%
%
%  Modified:
%
%    10 December 1998
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, character ( len = * ) CODE, identifies the element.
%    Legal values include 'Q4', 'Q8', 'Q9', 'Q12', 'Q16', 'QL',
%    'T3', 'T6' and 'T10'.
%
%    Input, integer N, the number of nodes in the element.
%
%    Input, real R, S, the reference coordinates of a point.
%
%    Output, real T(N), the basis functions at the point.
%
%    Output, real DTDR(N), the R basis derivatives at the point.
%
%    Output, real DTDS(N), the S basis derivatives at the point.
%
%
%

if( strcmp(deblank(code),deblank('Q4')) )
[ r, s, t, dtdr, dtds ]=shape_q4( r, s, t, dtdr, dtds );
elseif( strcmp(deblank(code),deblank('Q8')) ) ;
[ r, s, t, dtdr, dtds ]=shape_q8( r, s, t, dtdr, dtds );
elseif( strcmp(deblank(code),deblank('Q9')) ) ;
[ r, s, t, dtdr, dtds ]=shape_q9( r, s, t, dtdr, dtds );
elseif( strcmp(deblank(code),deblank('Q12')) ) ;
[ r, s, t, dtdr, dtds ]=shape_q12( r, s, t, dtdr, dtds );
elseif( strcmp(deblank(code),deblank('Q16')) ) ;
[ r, s, t, dtdr, dtds ]=shape_q16( r, s, t, dtdr, dtds );
elseif( strcmp(deblank(code),deblank('QL')) ) ;
[ r, s, t, dtdr, dtds ]=shape_ql( r, s, t, dtdr, dtds );
elseif( strcmp(deblank(code),deblank('T3')) ) ;
[ r, s, t, dtdr, dtds ]=shape_t3( r, s, t, dtdr, dtds );
elseif( strcmp(deblank(code),deblank('T6')) ) ;
[ r, s, t, dtdr, dtds ]=shape_t6( r, s, t, dtdr, dtds );
elseif( strcmp(deblank(code),deblank('T10')) ) ;
[ r, s, t, dtdr, dtds ]=shape_t10( r, s, t, dtdr, dtds );
else;
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'SHAPE - Fatal error!');
writef(1,['%s','%s','\n'], '  Unrecognized code = ', code);
error(['stop encountered in original fortran code  ',char(10),';']);
end;
return;
end
function [code]=shape_test( code );
%
%*******************************************************************************
%
%! SHAPE_TEST verifies the shape function values at the basis nodes.
%
%
%  Modified:
%
%    09 December 1998
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, character ( len = * ) CODE, identifies the element to be used.
%    Legal values include 'Q4', 'Q8', 'Q9', 'Q12', 'Q16', 'QL',
%    'T3', 'T6' and 'T10'.
%
persistent area dtdr dtds i j maxn n r rsum s ssum t ; 

if isempty(maxn), maxn = 16; end;
%
if isempty(area), area=0; end;
if isempty(dtdr), dtdr=zeros(1,maxn); end;
if isempty(dtds), dtds=zeros(1,maxn); end;
if isempty(i), i=0; end;
if isempty(j), j=0; end;
if isempty(n), n=0; end;
if isempty(r), r=zeros(1,maxn); end;
if isempty(rsum), rsum=0; end;
if isempty(s), s=zeros(1,maxn); end;
if isempty(ssum), ssum=0; end;
if isempty(t), t=zeros(1,maxn); end;
%
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'SHAPE_TEST');
writef(1,['%s','%s','\n'], '  Verify shape functions of type ', code);
[ code, n, r, s, area ]=node( code, n, r, s, area );
writef(1,['%s \n'], ' ');
writef(1,['%s %0.15g \n'], '  Number of nodes = ', n);
writef(1,['%s \n'], '  Basis function values at basis nodes');
writef(1,['%s \n'], '  should form the identity matrix.');
writef(1,['%s \n'], ' ');
for i = 1: n;
[ code, n, r(i), s(i), t, dtdr, dtds ]=shape( code, n, r(i), s(i), t, dtdr, dtds );
for  j =( 1):( n ), writef(1,[repmat('%7.3f',1,10),'\n'], t(j)); end;
end; i = fix(n+1);
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], '  R and S derivatives should sum to 0.');
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], '  dTdR sum, dTdS sum:');
writef(1,['%s \n'], ' ');
for i = 1: n;
[ code, n, r(i), s(i), t, dtdr, dtds ]=shape( code, n, r(i), s(i), t, dtdr, dtds );
rsum = 0.0;
ssum = 0.0;
for j = 1: n;
rsum = rsum + dtdr(j);
ssum = ssum + dtds(j);
end; j = fix(n+1);
writef(1,[repmat('%14.8f',1,2),'\n'], rsum, ssum);
end; i = fix(n+1);
return;
end
function [r, s, t, dtdr, dtds]=shape_q4( r, s, t, dtdr, dtds );
%
%*******************************************************************************
%
%! SHAPE_Q4 evaluates shape functions for a 4 node quadrilateral.
%
%
%  Diagram:
%
%    |
%    1  3-----4
%    |  |     |
%    |  |     |
%    S  |     |
%    |  |     |
%    |  |     |
%    0  1-----2
%    |
%    +--0--R--1-->
%
%  Modified:
%
%    05 January 1999
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, real R, S, the reference coordinates of a point.
%
%    Output, real T(4), the basis functions at the point.
%
%    Output, real DTDR(4), the R basis derivatives at the point.
%
%    Output, real DTDS(4), the S basis derivatives at the point.
%
%

t(1) =( 1.0 - r ) .*( 1.0 - s );
t(2) =         r   .*( 1.0 - s );
t(3) =( 1.0 - r ) .*         s;
t(4) =         r   .*         s;
dtdr(1) = - 1.0 + s;
dtdr(2) =   1.0 - s;
dtdr(3) =       - s;
dtdr(4) =         s;
dtds(1) = - 1.0 + r;
dtds(2) =       - r;
dtds(3) =   1.0 - r;
dtds(4) =         r;
return;
end
function [r, s, t, dtdr, dtds]=shape_q8( r, s, t, dtdr, dtds );
%
%*******************************************************************************
%
%! SHAPE_Q8 evaluates shape functions for an 8 node quadrilateral.
%
%
%  Comment:
%
%    This element is known as the 'serendipity' element.
%
%  Diagram:
%
%    |
%    1  6--7--8
%    |  |     |
%    |  |     |
%    S  4     5
%    |  |     |
%    |  |     |
%    0  1--2--3
%    |
%    +--0--R--1-->
%
%  Modified:
%
%    05 January 1999
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, real R, S, the reference coordinates of a point.
%
%    Output, real T(8), the basis functions at the point.
%
%    Output, real DTDR(8), the R basis derivatives at the point.
%
%    Output, real DTDS(8), the S basis derivatives at the point.
%
%

t(1) =( r - 1.0 )     .*( s - 1.0 ) .*( 1.0 - 2.0 .* r - 2.0 .* s );
t(2) =   4.0 .* r .*( r - 1.0 )     .*( s - 1.0 );
t(3) =         r                   .*( s - 1.0 ) .*( 1.0 - 2.0 .* r + 2.0 .* s );
t(4) =   4.0 .*( r - 1.0 ) .* s .*( s - 1.0 );
t(5) = - 4.0 .* r               .* s .*( s - 1.0 );
t(6) =( r - 1.0 ) .* s               .*( 2.0 .* r - 2.0 .* s + 1.0 );
t(7) = - 4.0 .* r .*( r - 1.0 ) .* s;
t(8) =         r               .* s               .*( 2.0 .* r + 2.0 .* s - 3.0 );
dtdr(1) =( s - 1.0 ) .*( - 4.0 .* r - 2.0 .* s + 3.0 );
dtdr(2) =   4.0 .*( 2.0 .* r - 1.0 )     .*( s - 1.0 );
dtdr(3) =( s - 1.0 ) .*( - 4.0 .* r + 2.0 .* s + 1.0 );
dtdr(4) =   4.0 .*                     s .*( s - 1.0 );
dtdr(5) = - 4.0 .*                     s .*( s - 1.0 );
dtdr(6) =   s         .*(   4.0 .* r - 2.0 .* s - 1.0 );
dtdr(7) = - 4.0 .*( 2.0 .* r - 1.0 ) .* s;
dtdr(8) =   s         .*(   4.0 .* r + 2.0 .* s - 3.0 );
dtds(1) =( r - 1.0 ) .*( - 4.0 .* s - 2.0 .* r + 3.0 );
dtds(2) =   4.0 .* r .*( r - 1.0 );
dtds(3) =   r .*(   4.0 .* s - 2.0 .* r - 1.0 );
dtds(4) =   4.0 .*( r - 1.0 ) .*( 2.0 .* s - 1.0 );
dtds(5) = - 4.0 .* r               .*( 2.0 .* s - 1.0 );
dtds(6) =( r - 1.0 ) .*( - 4.0 .* s + 2.0 .* r + 1.0 );
dtds(7) = - 4.0 .* r .*( r - 1.0 );
dtds(8) =   r .*(   4.0 .* s + 2.0 .* r - 3.0 );
return;
end
function [r, s, t, dtdr, dtds]=shape_q9( r, s, t, dtdr, dtds );
%
%*******************************************************************************
%
%! SHAPE_Q9 evaluates shape functions for a 9 node quadrilateral.
%
%
%  Diagram:
%
%    |
%    1  7--8--9
%    |  |  :  |
%    |  |  :  |
%    S  4..5..6
%    |  |  :  |
%    |  |  :  |
%    0  1--2--3
%    |
%    +--0--R--1-->
%
%  Modified:
%
%    05 January 1999
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, real R, S, the reference coordinates of a point.
%
%    Output, real T(9), the basis functions at the point.
%
%    Output, real DTDR(9), the R basis derivatives at the point.
%
%    Output, real DTDS(9), the S basis derivatives at the point.
%
%

t(1) =    4.0 .*( r - 1.0 ) .*( r - 0.5 ) .*( s - 1.0 ) .*( s - 0.5 );
t(2) = -  8.0 .* r .*( r - 1.0 ) .*( s - 1.0 ) .*( s - 0.5 );
t(3) =    4.0 .* r .*( r - 0.5 ) .*( s - 1.0 ) .*( s - 0.5 );
t(4) = -  8.0 .*( r - 1.0 ) .*( r - 0.5 ) .* s .*( s - 1.0 );
t(5) =   16.0 .* r .*( r - 1.0 ) .* s .*( s - 1.0 );
t(6) = -  8.0 .* r .*( r - 0.5 ) .* s .*( s - 1.0 );
t(7) =    4.0 .*( r - 1.0 ) .*( r - 0.5 ) .* s .*( s - 0.5 );
t(8) = -  8.0 .* r .*( r - 1.0 ) .* s .*( s - 0.5 );
t(9) =    4.0 .* r .*( r - 0.5 ) .* s .*( s - 0.5 );
dtdr(1) = 4.0 .*( 2.0 .* r - 1.5 ) .*( s - 1.0 ) .*( s - 0.5 );
dtdr(2) = - 8.0 .*( 2.0 .* r - 1.0 ) .*( s - 1.0 ) .*( s - 0.5 );
dtdr(3) = 4.0 .*( 2.0 .* r - 0.5 ) .*( s - 1.0 ) .*( s - 0.5 );
dtdr(4) = - 8.0 .*( 2.0 .* r - 1.5 ) .* s .*( s - 1.0 );
dtdr(5) = 16.0 .*( 2.0 .* r - 1.0 ) .* s .*( s - 1.0 );
dtdr(6) = - 8.0 .*( 2.0 .* r - 0.5 ) .* s .*( s - 1.0 );
dtdr(7) = 4.0 .*( 2.0 .* r - 1.5 ) .* s .*( s - 0.5 );
dtdr(8) = - 8.0 .*( 2.0 .* r - 1.0 ) .* s .*( s - 0.5 );
dtdr(9) = 4.0 .*( 2.0 .* r - 0.5 ) .* s .*( s - 0.5 );
dtds(1) = 4.0 .*( r - 1.0 ) .*( r - 0.5 ) .*( 2.0 .* s - 1.5 );
dtds(2) = - 8.0 .* r .*( r - 1.0 ) .*( 2.0 .* s - 1.5 );
dtds(3) = 4.0 .* r .*( r - 0.5 ) .*( 2.0 .* s - 1.5 );
dtds(4) = - 8.0 .*( r - 1.0 ) .*( r - 0.5 ) .*( 2.0 .* s - 1.0 );
dtds(5) =  16.0 .* r .*( r - 1.0 ) .*( 2.0 .* s - 1.0 );
dtds(6) = - 8.0 .* r .*( r - 0.5 ) .*( 2.0 .* s - 1.0 );
dtds(7) = 4.0 .*( r - 1.0 ) .*( r - 0.5 ) .*( 2.0 .* s - 0.5 );
dtds(8) = - 8.0 .* r .*( r - 1.0 ) .*( 2.0 .* s - 0.5 );
dtds(9) = 4.0 .* r .*( r - 0.5 ) .*( 2.0 .* s - 0.5 );
return;
end
function [r, s, t, dtdr, dtds]=shape_q12( r, s, t, dtdr, dtds );
%
%*******************************************************************************
%
%! SHAPE_Q12 evaluates shape functions for a 12 node quadrilateral.
%
%
%  Note:
%
%    This routine is being worked on.
%
%  Diagram:
%
%    |
%    1  9-10-11-12
%    |  |        |
%    |  7        8
%    S  |        |
%    |  5        6
%    |  |        |
%    0  1--2--3--4
%    |
%    +--0---R---1-->
%
%  Modified:
%
%    12 January 1999
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, real R, S, the reference coordinates of a point.
%
%    Output, real T(12), the basis functions at the point.
%
%    Output, real DTDR(12), the R basis derivatives at the point.
%
%    Output, real DTDS(12), the S basis derivatives at the point.
%
persistent a b c corner d dcdr dcds ; 

if isempty(a), a=0; end;
if isempty(b), b=0; end;
if isempty(c), c=0; end;
if isempty(corner), corner=0; end;
if isempty(d), d=0; end;
if isempty(dcdr), dcdr=0; end;
if isempty(dcds), dcds=0; end;
%
a = 0.0;
b = 1.0 ./ 3.0;
c = 2.0 ./ 3.0;
d = 1.0;
corner = 9.0 .*(( 2.0 .* r - 1.0 ).^2 +( 2.0 .* s - 1.0 ).^2 ) - 10.0;
t(1) =     0.125  .*( r - d ) .*( s - d ) .* corner;
t(2) =  - 13.5    .*( r - a ) .*( r - c ) .*( r - d ) .*( s - d );
t(3) =    13.5    .*( r - a ) .*( r - b ) .*( r - d ) .*( s - d );
t(4) =   - 0.125  .*( r - a ) .*( s - d ) .* corner;
t(5) =  - 13.5    .*( r - d ) .*( s - a ) .*( s - c ) .*( s - d );
t(6) =    13.5    .*( r - a ) .*( s - a ) .*( s - c ) .*( s - d );
t(7) =    13.5    .*( r - d ) .*( s - a ) .*( s - b ) .*( s - d );
t(8) =  - 13.5    .*( r - a ) .*( s - a ) .*( s - b ) .*( s - d );
t(9) =   - 0.125  .*( r - d ) .*( s - a ) .* corner;
t(10) =   13.5    .*( r - a ) .*( r - c ) .*( r - d ) .*( s - a );
t(11) = - 13.5    .*( r - a ) .*( r - b ) .*( r - d ) .*( s - a );
t(12) =    0.125  .*( r - a ) .*( s - a ) .* corner;

dcdr = 36.0 .*( 2.0 .* r - 1.0 );
dtdr(1) =  0.125 .*( s - d ) .*(( r - d ) .* dcdr + corner );
dtdr(2) =  - 13.5 .*( s - d ) .*( 3.0 .* r.^2- 2.0 .*( a + c + d ) .* r + a .* c + c .* d + d .* a );
dtdr(3) =    13.5 .*( s - d ) .*( 3.0 .* r.^2- 2.0 .*( a + b + d ) .* r + a .* b + b .* d + d .* a );
dtdr(4) = - 0.125 .*( s - d ) .*(( r - a ) .* dcdr + corner );
dtdr(5) = - 13.5 .*( s - a ) .*( s - c ) .*( s - d );
dtdr(6) =   13.5 .*( s - a ) .*( s - c ) .*( s - d );
dtdr(7) =   13.5 .*( s - a ) .*( s - b ) .*( s - d );
dtdr(8) = - 13.5 .*( s - a ) .*( s - b ) .*( s - d );
dtdr(9) = - 0.125 .*( s - a ) .*(( r - d ) .* dcdr + corner );
dtdr(10) =   13.5 .*( s - a ) .*( 3.0 .* r.^2- 2.0 .*( a + c + d ) .* r + a .* c + c .* d + d .* a );
dtdr(11) = - 13.5 .*( s - a ) .*( 3.0 .* r.^2- 2.0 .*( a + b + d ) .* r + a .* b + b .* d + d .* a );
dtdr(12) = 0.125 .*( s - a ) .*(( r - a ) .* dcdr + corner );
dcds = 36.0 .*( 2.0 .* s - 1.0 );
dtds(1) =  0.125 .*( r - d ) .*( corner +( s - d ) .* dcds );
dtds(2) =  - 13.5 .*( r - a ) .*( r - c ) .*( r - d );
dtds(3) =  13.5 .*( r - a ) .*( r - b ) .*( r - d );
dtds(4) = - 0.125  .*( r - a ) .*( corner +( s - d ) .* dcds );
dtds(5) =  - 13.5 .*( r - d ) .*( 3.0 .* s.^2- 2.0 .*( a + c + d ) .* s + a .* c + c .* d + d .* a );
dtds(6) =  13.5 .*( r - a ) .*( 3.0 .* s.^2- 2.0 .*( a + c + d ) .* s + a .* c + c .* d + d .* a );
dtds(7) =  13.5 .*( r - d ) .*( 3.0 .* s.^2- 2.0 .*( a + b + d ) .* s + a .* b + b .* d + d .* a );
dtds(8) =  - 13.5 .*( r - a ) .*( 3.0 .* s.^2- 2.0 .*( a + b + d ) .* s + a .* b + b .* d + d .* a );
dtds(9) =  - 0.125 .*( r - d ) .*( corner +( s - a ) .* dcds );
dtds(10) = 13.5 .*( r - a ) .*( r - c ) .*( r - d );
dtds(11) = - 13.5 .*( r - a ) .*( r - b ) .*( r - d );
dtds(12) = 0.125 .*( r - a ) .*( corner +( s - a ) .* dcds );
return;
end
function [r, s, t, dtdr, dtds]=shape_q16( r, s, t, dtdr, dtds );
%
%*******************************************************************************
%
%! SHAPE_Q16 evaluates shape functions for a 16 node quadrilateral.
%
%
%  Diagram:
%
%    |
%    1 13--14--15--16
%    |  |   :   :   |
%    |  |   :   :   |
%    |  9..10..11..12
%    S  |   :   :   |
%    |  |   :   :   |
%    |  5...6...7...8
%    |  |   :   :   |
%    |  |   :   :   |
%    0  1---2---3---4
%    |
%    +--0-----R-----1-->
%
%  Modified:
%
%    12 December 1998
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, real R, S, the reference coordinates of a point.
%
%    Output, real T(16), the basis functions at the point.
%
%    Output, real DTDR(16), the R basis derivatives at the point.
%
%    Output, real DTDS(16), the S basis derivatives at the point.
%
persistent dabc dabd dacd dbcd ra rb rc rd sa sb sc sd ; 

if isempty(dabc), dabc=0; end;
if isempty(dabd), dabd=0; end;
if isempty(dacd), dacd=0; end;
if isempty(dbcd), dbcd=0; end;
if isempty(ra), ra=0; end;
if isempty(rb), rb=0; end;
if isempty(rc), rc=0; end;
if isempty(rd), rd=0; end;
if isempty(sa), sa=0; end;
if isempty(sb), sb=0; end;
if isempty(sc), sc=0; end;
if isempty(sd), sd=0; end;
%
ra = r - 0.0;
rb = r - 1.0 ./ 3.0;
rc = r - 2.0 ./ 3.0;
rd = r - 1.0;
sa = s - 0.0;
sb = s - 1.0 ./ 3.0;
sc = s - 2.0 ./ 3.0;
sd = s - 1.0;
t(1)  =(  81.0 ./ 4.0 ) .* rb .* rc .* rd .* sb .* sc .* sd;
t(2)  = -( 243.0 ./ 4.0 ) .* ra .* rc .* rd .* sb .* sc .* sd;
t(3)  =( 243.0 ./ 4.0 ) .* ra .* rb .* rd .* sb .* sc .* sd;
t(4)  = -(  81.0 ./ 4.0 ) .* ra .* rb .* rc .* sb .* sc .* sd;
t(5)  = -( 243.0 ./ 4.0 ) .* rb .* rc .* rd .* sa .* sc .* sd;
t(6)  =( 729.0 ./ 4.0 ) .* ra .* rc .* rd .* sa .* sc .* sd;
t(7)  = -( 729.0 ./ 4.0 ) .* ra .* rb .* rd .* sa .* sc .* sd;
t(8)  =( 243.0 ./ 4.0 ) .* ra .* rb .* rc .* sa .* sc .* sd;
t(9)  =( 243.0 ./ 4.0 ) .* rb .* rc .* rd .* sa .* sb .* sd;
t(10) = -( 729.0 ./ 4.0 ) .* ra .* rc .* rd .* sa .* sb .* sd;
t(11) =( 729.0 ./ 4.0 ) .* ra .* rb .* rd .* sa .* sb .* sd;
t(12) = -( 243.0 ./ 4.0 ) .* ra .* rb .* rc .* sa .* sb .* sd;
t(13) = -(  81.0 ./ 4.0 ) .* rb .* rc .* rd .* sa .* sb .* sc;
t(14) =( 243.0 ./ 4.0 ) .* ra .* rc .* rd .* sa .* sb .* sc;
t(15) = -( 243.0 ./ 4.0 ) .* ra .* rb .* rd .* sa .* sb .* sc;
t(16) =(  81.0 ./ 4.0 ) .* ra .* rb .* rc .* sa .* sb .* sc;
dbcd = 3.0 .* r.^2 -  4.0 .* r       + 11.0 ./ 9.0;
dacd = 3.0 .* r.^2 - 10.0 .* r ./ 3.0 +  2.0 ./ 3.0;
dabd = 3.0 .* r.^2 -  8.0 .* r ./ 3.0 +  1.0 ./ 3.0;
dabc = 3.0 .* r.^2 -  2.0 .* r       +  2.0 ./ 9.0;
dtdr( 1) =(  81.0 ./ 4.0 ) .* dbcd .* sb .* sc .* sd;
dtdr( 2) = -( 243.0 ./ 4.0 ) .* dacd .* sb .* sc .* sd;
dtdr( 3) =( 243.0 ./ 4.0 ) .* dabd .* sb .* sc .* sd;
dtdr( 4) = -(  81.0 ./ 4.0 ) .* dabc .* sb .* sc .* sd;
dtdr( 5) = -( 243.0 ./ 4.0 ) .* dbcd .* sa .* sc .* sd;
dtdr( 6) =( 729.0 ./ 4.0 ) .* dacd .* sa .* sc .* sd;
dtdr( 7) = -( 729.0 ./ 4.0 ) .* dabd .* sa .* sc .* sd;
dtdr( 8) =( 243.0 ./ 4.0 ) .* dabc .* sa .* sc .* sd;
dtdr( 9) =( 243.0 ./ 4.0 ) .* dbcd .* sa .* sb .* sd;
dtdr(10) = -( 729.0 ./ 4.0 ) .* dacd .* sa .* sb .* sd;
dtdr(11) =( 729.0 ./ 4.0 ) .* dabd .* sa .* sb .* sd;
dtdr(12) = -( 243.0 ./ 4.0 ) .* dabc .* sa .* sb .* sd;
dtdr(13) = -(  81.0 ./ 4.0 ) .* dbcd .* sa .* sb .* sc;
dtdr(14) =( 243.0 ./ 4.0 ) .* dacd .* sa .* sb .* sc;
dtdr(15) = -( 243.0 ./ 4.0 ) .* dabd .* sa .* sb .* sc;
dtdr(16) =(  81.0 ./ 4.0 ) .* dabc .* sa .* sb .* sc;
dbcd = 3.0 .* s.^2 -  4.0 .* s       + 11.0 ./ 9.0;
dacd = 3.0 .* s.^2 - 10.0 .* s ./ 3.0 +  2.0 ./ 3.0;
dabd = 3.0 .* s.^2 -  8.0 .* s ./ 3.0 +  1.0 ./ 3.0;
dabc = 3.0 .* s.^2 -  2.0 .* s       +  2.0 ./ 9.0;
dtds( 1) =(  81.0 ./ 4.0 ) .* rb .* rc .* rd .* dbcd;
dtds( 2) = -( 243.0 ./ 4.0 ) .* ra .* rc .* rd .* dbcd;
dtds( 3) =( 243.0 ./ 4.0 ) .* ra .* rb .* rd .* dbcd;
dtds( 4) = -(  81.0 ./ 4.0 ) .* ra .* rb .* rc .* dbcd;
dtds( 5) = -( 243.0 ./ 4.0 ) .* rb .* rc .* rd .* dacd;
dtds( 6) =( 729.0 ./ 4.0 ) .* ra .* rc .* rd .* dacd;
dtds( 7) = -( 729.0 ./ 4.0 ) .* ra .* rb .* rd .* dacd;
dtds( 8) =( 243.0 ./ 4.0 ) .* ra .* rb .* rc .* dacd;
dtds( 9) =( 243.0 ./ 4.0 ) .* rb .* rc .* rd .* dabd;
dtds(10) = -( 729.0 ./ 4.0 ) .* ra .* rc .* rd .* dabd;
dtds(11) =( 729.0 ./ 4.0 ) .* ra .* rb .* rd .* dabd;
dtds(12) = -( 243.0 ./ 4.0 ) .* ra .* rb .* rc .* dabd;
dtds(13) = -(  81.0 ./ 4.0 ) .* rb .* rc .* rd .* dabc;
dtds(14) =( 243.0 ./ 4.0 ) .* ra .* rc .* rd .* dabc;
dtds(15) = -( 243.0 ./ 4.0 ) .* ra .* rb .* rd .* dabc;
dtds(16) =(  81.0 ./ 4.0 ) .* ra .* rb .* rc .* dabc;

return;
end
function [r, s, t, dtdr, dtds]=shape_ql( r, s, t, dtdr, dtds );
%
%*******************************************************************************
%
%! SHAPE_QL evaluates shape functions for a 6 node quadratic/linear.
%
%
%  Diagram:
%
%    |
%    1  4--5--6
%    |  |     |
%    |  |     |
%    S  |     |
%    |  |     |
%    |  |     |
%    0  1--2--3
%    |
%    +--0--R--1-->
%
%  Modified:
%
%    05 January 1999
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, real R, S, the reference coordinates of a point.
%
%    Output, real T(6), the basis functions at the point.
%
%    Output, real DTDR(6), the R basis derivatives at the point.
%
%    Output, real DTDS(6), the S basis derivatives at the point.
%
%

t(1) = - 2.0 .*( r - 0.5 ) .*( r - 1.0 )     .*( s - 1.0 );
t(2) =   4.0 .* r               .*( r - 1.0 )     .*( s - 1.0 );
t(3) = - 2.0 .* r .*( r - 0.5 )                   .*( s - 1.0 );
t(4) =   2.0 .*( r - 0.5 ) .*( r - 1.0 ) .* s;
t(5) = - 4.0 .* r               .*( r - 1.0 ) .* s;
t(6) =   2.0 .* r .*( r - 0.5 )               .* s;
dtdr(1) = 2.0 .*( - 2.0 .* r + 1.5 )     .*( s - 1.0 );
dtdr(2) = 4.0 .*(   2.0 .* r - 1.0 )     .*( s - 1.0 );
dtdr(3) = 2.0 .*( - 2.0 .* r + 0.5 )     .*( s - 1.0 );
dtdr(4) = 2.0 .*(   2.0 .* r - 1.5 ) .* s;
dtdr(5) = 4.0 .*( - 2.0 .* r + 1.0 ) .* s;
dtdr(6) = 2.0 .*(   2.0 .* r - 0.5 ) .* s;
dtds(1) = - 2.0 .*( r - 0.5 ) .*( r - 1.0 );
dtds(2) =   4.0 .* r               .*( r - 1.0 );
dtds(3) = - 2.0 .* r .*( r - 0.5 );
dtds(4) =   2.0 .*( r - 0.5 ) .*( r - 1.0 );
dtds(5) = - 4.0 .* r               .*( r - 1.0 );
dtds(6) =   2.0 .* r .*( r - 0.5 );
return;
end
function [r, s, t, dtdr, dtds]=shape_t3( r, s, t, dtdr, dtds );
%
%*******************************************************************************
%
%! SHAPE_T3 evaluates shape functions for a 3 node triangle.
%
%
%  Diagram:
%
%    |
%    1  3
%    |  |\
%    |  | \
%    S  |  \
%    |  |   \
%    |  |    \
%    0  1-----2
%    |
%    +--0--R--1-->
%
%  Modified:
%
%    05 January 1999
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, real R, S, the reference coordinates of a point.
%
%    Output, real T(3), the basis functions at the point.
%
%    Output, real DTDR(3), the R basis derivatives at the point.
%
%    Output, real DTDS(3), the S basis derivatives at the point.
%
%

t(1) = 1.0 - r - s;
t(2) =       r;
t(3) =           s;
dtdr(1) = -1.0;
dtdr(2) =  1.0;
dtdr(3) =  0.0;
dtds(1) = -1.0;
dtds(2) =  0.0;
dtds(3) =  1.0;
return;
end
function [r, s, t, dtdr, dtds]=shape_t6( r, s, t, dtdr, dtds );
%
%*******************************************************************************
%
%! SHAPE_T6 evaluates shape functions for a 6 node triangle.
%
%
%  Diagram:
%
%    |
%    1  6
%    |  |\
%    |  | \
%    S  4  5
%    |  |   \
%    |  |    \
%    0  1--2--3
%    |
%    +--0--R--1-->
%
%  Modified:
%
%    05 January 1999
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, real R, S, the reference coordinates of a point.
%
%    Output, real T(6), the basis functions at the point.
%
%    Output, real DTDR(6), the R basis derivatives at the point.
%
%    Output, real DTDS(6), the S basis derivatives at the point.
%
%

t(1) = 2.0 .*( 1.0 - r - s ) .*( 0.5 - r - s );
t(2) = 4.0 .* r .*( 1.0 - r - s );
t(3) = 2.0 .* r .*( r - 0.5 );
t(4) = 4.0 .* s .*( 1.0 - r - s );
t(5) = 4.0 .* r .* s;
t(6) = 2.0 .* s .*( s - 0.5 );
dtdr(1) = - 3.0 + 4.0 .* r + 4.0 .* s;
dtdr(2) =   4.0 - 8.0 .* r - 4.0 .* s;
dtdr(3) = - 1.0 + 4.0 .* r;
dtdr(4) =                 - 4.0 .* s;
dtdr(5) =                   4.0 .* s;
dtdr(6) =   0.0;
dtds(1) = - 3.0 + 4.0 .* r + 4.0 .* s;
dtds(2) =       - 4.0 .* r;
dtds(3) =   0.0;
dtds(4) =   4.0 - 4.0 .* r - 8.0 .* s;
dtds(5) =         4.0 .* r;
dtds(6) = - 1.0           + 4.0 .* s;
return;
end
function [r, s, t, dtdr, dtds]=shape_t10( r, s, t, dtdr, dtds );
%
%*******************************************************************************
%
%! SHAPE_T10 evaluates shape functions for a 10 node triangle.
%
%
%  Diagram:
%
%    |
%    1  10
%    |  |\
%    |  | \
%    |  8  9
%    |  |   \
%    S  |    \
%    |  5  6  7
%    |  |      \
%    |  |       \
%    0  1--2--3--4
%    |
%    +--0----R---1-->
%
%  Modified:
%
%    05 January 1999
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, real R, S, the reference coordinates of a point.
%
%    Output, real T(10), the basis functions at the point.
%
%    Output, real DTDR(10), the R basis derivatives at the point.
%
%    Output, real DTDS(10), the S basis derivatives at the point.
%
persistent a b c ; 

if isempty(a), a=0; end;
if isempty(b), b=0; end;
if isempty(c), c=0; end;
%
a = 1.0 ./ 3.0;
b = 2.0 ./ 3.0;
c = 1.0;
t( 1) = 4.5 .*( a - r - s ) .*( b - r - s ) .*( c - r - s );
t( 2) = 13.5 .* r .*( b - r - s ) .*( c - r - s );
t( 3) = - 13.5 .* r .*( a - r ) .*( c - r - s );
t( 4) = 4.5 .* r .*( a - r ) .*( b - r );
t( 5) = 13.5 .* s .*( b - r - s ) .*( c - r - s );
t( 6) = 27.0 .* r .* s .*( c - r - s );
t( 7) = 13.5 .* r .* s .*( r - a );
t( 8) = 13.5 .* s .*( s - a ) .*( c - r - s );
t( 9) = 13.5 .* r .* s .*( s - a );
t(10) = 4.5 .* s .*( a - s ) .*( b - s );
dtdr( 1) = 4.5 .*(( a - s ) .*( 2.0 .* r - c - b + 2.0 .* s )-( s - b ) .*( s - c ) - 2.0 .*( 2.0 .* s - b - c ) .* r - 3.0 .* r.^2 );
dtdr( 2) = 13.5 .*(( s - b ) .*( s - c ) + 2.0 .*( 2.0 .* s - b - c ) .* r + 3.0 .* r.^2 );
dtdr( 3) = - 13.5 .*( a .*( c - s ) + 2.0 .*( s - a - c ) .* r + 3.0 .* r.^2 );
dtdr( 4) = 4.5 .*( a .* b - 2.0 .*( a + b ) .* r + 3.0 .* r.^2 );
dtdr( 5) = 13.5 .* s .*( 2.0 .* s - b - c + 2.0 .* r );
dtdr( 6) = 27.0 .* s .*( c - s - 2.0 .* r );
dtdr( 7) = 13.5 .* s .*( 2.0 .* r - a );
dtdr( 8) = - 13.5 .* s .*( s - a );
dtdr( 9) = 13.5 .* s .*( s - a);
dtdr(10) = 0.0;
dtds( 1) = 4.5 .*(( a - r ) .*( 2.0 .* s - c - b + 2.0 .* r )-( r - b ) .*( r - c ) - 2.0 .*( 2.0 .* r - b - c ) .* s - 3.0 .* s.^2 );
dtds( 2) = 13.5 .* r .*( 2.0 .* s + 2.0 .* r - b - c );
dtds( 3) = 13.5 .* r .*( a - r );
dtds( 4) = 0.0;
dtds( 5) = 13.5 .*(( r - b ) .*( r - c ) +2.0 .*( 2.0 .* r - b - c ) .* s + 3.0 .* s.^2 );
dtds( 6) = 27.0 .* r .*( c - r - 2.0 .* s );
dtds( 7) = 13.5 .* r .*( r - a );
dtds( 8) = - 13.5 .*( a .*( c - r ) + 2.0 .*( r - c - a ) .* s + 3.0 .* s.^2 );
dtds( 9) = 13.5 .* r .*( 2.0 .* s - a);
dtds(10) = 4.5 .*( a .* b - 2.0 .*( a + b ) .* s + 3.0 .* s.^2 );
return;
end




function out=writef(fid,varargin)
% function out=writef(fid,varargin)
%  Catches fortran stdout (6) and reroutes in to Matlab's stdout (1)
%  Catches fortran stderr (0) and reroutes in to Matlab's stderr (2)
if isnumeric(fid)
 if fid==6,      out=fprintf(1,varargin{:});
 elseif fid==0,  out=fprintf(2,varargin{:});
 elseif isempty(fid) %% treat empty array like a string array [sethg 2008-03-03]
  out=sprintf(varargin{:});
  if nargin>2 %set the calling var to out
   if ~isempty(inputname(1)), assignin('caller',inputname(1),out); end
  end
 else,           out=fprintf(fid,varargin{:});
 end
elseif ischar(fid)
 out=sprintf(varargin{:});
 if nargin>2 %set the calling var to out
  if ~isempty(inputname(1)), assignin('caller',inputname(1),out); end
 end
else,            out=fprintf(fid,varargin{:});
end
end