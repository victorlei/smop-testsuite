function stroud_prb(varargin)
%
%*******************************************************************************
%
%! STROUD_PRB is a set of tests for the STROUD routines for multiple integrals.
%
clear global; clear functions;

persistent datemlv timemlv ; 

if isempty(datemlv), datemlv=repmat(' ',1,8); end;
if isempty(timemlv), timemlv=repmat(' ',1,10); end;
%
datemlv=datestr(now,'yyyymmdd');
timemlv =[datestr(now,'HHMMSS'),'.',num2str(round((sum(clock)-fix(sum(clock))).*1000),3)];
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'STROUD_PRB');
writef(1,['%s \n'], '  STROUD is a set of routines for the approximation ');
writef(1,['%s \n'], '  of integrals over N-dimensional regions.');
writef(1,['%s \n'], ' ');
writef(1,['%s %0.15g \n'], '  Today''s date: ', datemlv);
writef(1,['%s %0.15g \n'], '  Today''s time: ', timemlv);
%
test002;
test003;
test005;
test01;
test02;
test024;
test025;
test026;
test03;
test04;
test05;
test06;
test07;
test08;
test10;
test11;
test12;
test13;
test14;
test15;
test16;
test165;
test17;
test18;
test19;
test09;
test095;
test096;
test20;
test21;
test22;
test23;
test24;
test25;
test26;
test27;
test28;
test285;
test29;
test30;
test31;
test32;
test33;
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'STROUD_PRB');
writef(1,['%s \n'], '  Normal end of STROUD tests.');
%  stop
end
function test002(varargin)
%
%*******************************************************************************
%
%! TEST002 tests CIRCLE_ANNULUS.
%
persistent area i j nfunc nr ntest radius1 radius1_test radius2 radius2_test result xc xc_test yc yc_test ; 

if isempty(ntest), ntest = 2; end;
%
if isempty(area), area=0; end;
if isempty(i), i=0; end;
if isempty(j), j=0; end;
if isempty(nfunc), nfunc=0; end;
if isempty(nr), nr=0; end;
if isempty(radius1), radius1=0; end;
if isempty(radius1_test), radius1_test([1:ntest]) =[ 0.0, 1.0 ]; end;
if isempty(radius2), radius2=0; end;
if isempty(radius2_test), radius2_test([1:ntest]) =[ 1.0, 2.0 ]; end;
if isempty(result), result=0; end;
if isempty(xc), xc=0; end;
if isempty(xc_test), xc_test([1:ntest]) =[ 0.0, 0.0 ]; end;
if isempty(yc), yc=0; end;
if isempty(yc_test), yc_test([1:ntest]) =[ 0.0, 0.0 ]; end;
%
%
[dumvar1, nfunc ]=funcset ( 'COUNT', nfunc );
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'TEST002');
writef(1,['%s \n'], '  CIRCLE_ANNULUS estimates an integral in a circular annulus.');
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], '      F   XC        YC       Radius1   Radius2   NR  Result');
writef(1,['%s \n'], ' ');
for i = 1: ntest;
xc = xc_test(i);
yc = yc_test(i);
radius1 = radius1_test(i);
radius2 = radius2_test(i);
[area , radius1, radius2 ]=circle_annulus_area_2d( radius1, radius2 );
writef(1,['%s \n'], ' ');
writef(1,['%7s',repmat('%10.6f',1,4),repmat(' ',1,2),'%10.6f','\n'], '   Area', xc, yc, radius1, radius2, area);
for j = 1: nfunc;
[dumvar1, j ]=funcset ( 'SET', j );
for nr = 1: 4;
[dumvar1, xc, yc, radius1, radius2, nr, result ]=circle_annulus( @funcd2, xc, yc, radius1, radius2, nr, result );
writef(1,['%7s',repmat('%10.6f',1,4),'%2i','%10.6f','\n'],fname(j), xc, yc, radius1, radius2, nr, result);
end; nr = fix(4+1);
end; j = fix(nfunc+1);
end; i = fix(ntest+1);
return;
end
function test003(varargin)
%
%*******************************************************************************
%
%! TEST003 tests CIRCLE_ANNULUS.
%! TEST003 tests CIRCLE_SET_RT.
%! TEST003 tests CIRCLE_SUM_RT.
%
persistent area i j nfunc nr nr2 nt ntest ra radius1 radius1_test radius2 radius2_test result1 result2 result3 rule rw ta tw xc xc_test yc yc_test zw ; 

if isempty(ntest), ntest = 3; end;
%
if isempty(area), area=0; end;
if isempty(i), i=0; end;
if isempty(j), j=0; end;
if isempty(nfunc), nfunc=0; end;
if isempty(nr), nr=0; end;
if isempty(nr2), nr2=0; end;
if isempty(nt), nt=0; end;
if isempty(ra), ra=zeros(1,5); end;
if isempty(radius1), radius1=0; end;
if isempty(radius1_test), radius1_test([1:ntest]) =[ 0.0, 1.0, 1.0 ]; end;
if isempty(radius2), radius2=0; end;
if isempty(radius2_test), radius2_test([1:ntest]) =[ 1.0, 2.0, 3.0 ]; end;
if isempty(result1), result1=0; end;
if isempty(result2), result2=0; end;
if isempty(result3), result3=0; end;
if isempty(rw), rw=zeros(1,5); end;
if isempty(rule), rule=0; end;
if isempty(ta), ta=zeros(1,20); end;
if isempty(tw), tw=zeros(1,20); end;
if isempty(xc), xc=0; end;
if isempty(xc_test), xc_test([1:ntest]) =[ 0.0, 0.0, 0.0 ]; end;
if isempty(yc), yc=0; end;
if isempty(yc_test), yc_test([1:ntest]) =[ 0.0, 0.0, 0.0 ]; end;
if isempty(zw), zw=0; end;
%
%
[dumvar1, nfunc ]=funcset ( 'COUNT', nfunc );
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'TEST003');
writef(1,['%s \n'], '  CIRCLE_ANNULUS estimates an integral in a circular annulus.');
writef(1,['%s \n'], '  CIRCLE_SET_RT sets up a rule for a circle;');
writef(1,['%s \n'], '  CIRCLE_SUM_RT applies the rule.');
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], '  RESULT1 = CIRCLE_ANNULUS result.');
writef(1,['%s \n'], '  RESULT2 = Difference of CIRCLE_SUM_RT results.');
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], '      F   XC        YC       Radius1   Radius2   Result1 Result2');
writef(1,['%s \n'], ' ');
for i = 1: ntest;
xc = xc_test(i);
yc = yc_test(i);
radius1 = radius1_test(i);
radius2 = radius2_test(i);
[area , radius1, radius2 ]=circle_annulus_area_2d( radius1, radius2 );
writef(1,['%s \n'], ' ');
writef(1,['%7s',repmat('%11.5f',1,5),'\n'], '   Area', xc, yc, radius1, radius2, area);
rule = 9;
[ rule, nr2, ra, rw, nt, ta, tw, zw ]=circle_set_rt( rule, nr2, ra, rw, nt, ta, tw, zw );
for j = 1: nfunc;
[dumvar1, j ]=funcset ( 'SET', j );
nr = 5;
[dumvar1, xc, yc, radius1, radius2, nr, result1 ]=circle_annulus( @funcd2, xc, yc, radius1, radius2, nr, result1 );
[dumvar1, xc, yc, radius1, nr2, ra, rw, nt, ta, tw,zw, result2 ]=circle_sum_rt( @funcd2, xc, yc, radius1, nr2, ra, rw, nt, ta, tw,zw, result2 );
[dumvar1, xc, yc, radius2, nr2, ra, rw, nt, ta, tw,zw, result3 ]=circle_sum_rt( @funcd2, xc, yc, radius2, nr2, ra, rw, nt, ta, tw,zw, result3 );
writef(1,['%7s',repmat('%11.5f',1,6),'\n'],fname(j), xc, yc, radius1, radius2, result1, result3 - result2);
end; j = fix(nfunc+1);
end; i = fix(ntest+1);
return;
end
function test005(varargin)
%
%*******************************************************************************
%
%! TEST005 tests CIRCLE_ANNULUS_SECTOR.
%! TEST005 tests CIRCLE_SET_RT.
%! TEST005 tests CIRCLE_SUM_RT.
%
persistent as1 as2 as3 as4 i j nfunc nr nr2 nt ntest ra radius radius1a radius1b radius1c radius1d radius2a radius2b radius2c radius2d result1 result2 rule rw ta theta1 theta1a theta1b theta1c theta1d theta2 theta2a theta2b theta2c theta2d tw xc yc zw ; 

if isempty(ntest), ntest = 4; end;
%
if isempty(as1), as1=0; end;
if isempty(as2), as2=0; end;
if isempty(as3), as3=0; end;
if isempty(as4), as4=0; end;
if isempty(i), i=0; end;
if isempty(j), j=0; end;
if isempty(nfunc), nfunc=0; end;
if isempty(nr), nr=0; end;
if isempty(nr2), nr2=0; end;
if isempty(nt), nt=0; end;
if isempty(ra), ra=zeros(1,5); end;
if isempty(radius), radius=0; end;
if isempty(radius1a), radius1a=0; end;
if isempty(radius2a), radius2a=0; end;
if isempty(radius1b), radius1b=0; end;
if isempty(radius2b), radius2b=0; end;
if isempty(radius1c), radius1c=0; end;
if isempty(radius2c), radius2c=0; end;
if isempty(radius1d), radius1d=0; end;
if isempty(radius2d), radius2d=0; end;
if isempty(result1), result1=0; end;
if isempty(result2), result2=0; end;
if isempty(rule), rule=0; end;
if isempty(rw), rw=zeros(1,5); end;
if isempty(ta), ta=zeros(1,20); end;
if isempty(theta1), theta1=0; end;
if isempty(theta2), theta2=0; end;
if isempty(theta1a), theta1a=0; end;
if isempty(theta2a), theta2a=0; end;
if isempty(theta1b), theta1b=0; end;
if isempty(theta2b), theta2b=0; end;
if isempty(theta1c), theta1c=0; end;
if isempty(theta2c), theta2c=0; end;
if isempty(theta1d), theta1d=0; end;
if isempty(theta2d), theta2d=0; end;
if isempty(tw), tw=zeros(1,20); end;
if isempty(xc), xc=0; end;
if isempty(yc), yc=0; end;
if isempty(zw), zw=0; end;
%
%
[dumvar1, nfunc ]=funcset ( 'COUNT', nfunc );
nr = 5;
rule = 9;
[ rule, nr2, ra, rw, nt, ta, tw, zw ]=circle_set_rt( rule, nr2, ra, rw, nt, ta, tw, zw );
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'TEST005');
writef(1,['%s \n'], '  CIRCLE_ANNULUS_SECTOR estimates an integral in a ');
writef(1,['%s \n'], '  circular annulus sector.');
writef(1,['%s \n'], '  CIRCLE_SET_RT sets an integration rule in a circle.');
writef(1,['%s \n'], '  CIRCLE_SUM_RT uses an integration rule in a circle.');
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], '  To test CIRCLE_ANNULUS_SECTOR, we will estimate an integral');
writef(1,['%s \n'], '  over 4 annular sectors that make up the unit circle, ');
writef(1,['%s \n'], '  and add to get RESULT1.');
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], '  We will also estimate the integral over the unit circle');
writef(1,['%s \n'], '  using CIRCLE_SET_RT and CIRCLE_SUM_RT to get RESULT2.');
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], '  We will then compare RESULT1 and RESULT2.');
writef(1,['%s \n'], ' ');
writef(1,['%s %0.15g \n'], '  In this test, CIRCLE_SECTOR computations will use NR = ',nr);
writef(1,['%s %0.15g \n'], '  CIRCLE_SET_RT/CIRCLE_SUM_RT will use rule number ', rule);
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], '  "RESULT1" is the sum of Annulus Sector calculations.');
writef(1,['%s \n'], '  "RESULT2" is the computation for CIRCLE_SET_RT/CIRCLE_SUM_RT.');
writef(1,['%s \n'], ' ');
xc = 0.0d+00;
yc = 0.0d+00;
radius = 1.0d+00;
radius1a = 0.0d+00;
radius2a = 0.25d+00;
theta1a = 0.0d+00;
theta2a = 0.5d+00 .* pi();
radius1b = 0.0d+00;
radius2b = 0.25d+00;
theta1b = 0.5d+00 .* pi();
theta2b = 2.0d+00 .* pi();
radius1c = 0.25d+00;
radius2c = 1.0d+00;
theta1c = 0.0d+00;
theta2c = 0.25d+00 .* pi();
radius1d = 0.25d+00;
radius2d = 1.0d+00;
theta1d = 0.25d+00 .* pi();
theta2d = 2.0d+00 .* pi();
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], '     F     Result1  Result2');
writef(1,['%s \n'], ' ');
for j = 1: nfunc;
[dumvar1, j ]=funcset ( 'SET', j );
[dumvar1, xc, yc, radius1a, radius2a, theta1a,theta2a, nr, as1 ]=circle_annulus_sector( @funcd2, xc, yc, radius1a, radius2a, theta1a,theta2a, nr, as1 );
[dumvar1, xc, yc, radius1b, radius2b, theta1b,theta2b, nr, as2 ]=circle_annulus_sector( @funcd2, xc, yc, radius1b, radius2b, theta1b,theta2b, nr, as2 );
[dumvar1, xc, yc, radius1c, radius2c, theta1c,theta2c, nr, as3 ]=circle_annulus_sector( @funcd2, xc, yc, radius1c, radius2c, theta1c,theta2c, nr, as3 );
[dumvar1, xc, yc, radius1d, radius2d, theta1d,theta2d, nr, as4 ]=circle_annulus_sector( @funcd2, xc, yc, radius1d, radius2d, theta1d,theta2d, nr, as4 );
result1 = as1 + as2 + as3 + as4;
[dumvar1, xc, yc, radius, nr2, ra, rw, nt, ta, tw, zw,result2 ]=circle_sum_rt( @funcd2, xc, yc, radius, nr2, ra, rw, nt, ta, tw, zw,result2 );
writef(1,['%7s',repmat('%14.6f',1,2),'\n'], fname(j), result1, result2);
end; j = fix(nfunc+1);
return;
end
function test01(varargin)
%
%*******************************************************************************
%
%! TEST01 tests CIRCLE_CUM.
%
persistent i j nfunc norder pi r result xc yc ; 

if isempty(i), i=0; end;
if isempty(j), j=0; end;
if isempty(nfunc), nfunc=0; end;
if isempty(norder), norder=0; end;
if isempty(pi), pi=0; end;
if isempty(r), r=0; end;
if isempty(result), result=zeros(1,4); end;
if isempty(xc), xc=0; end;
if isempty(yc), yc=0; end;
%
%
[dumvar1, nfunc ]=funcset ( 'COUNT', nfunc );
xc = 0.0d+00;
yc = 0.0d+00;
r = 3.0d+00;

writef(1,['%s \n'], ' ');
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'TEST01');
writef(1,['%s \n'], '  CIRCLE_CUM approximates an integral over a circle.');
writef(1,['%s \n'], ' ');
writef(1,['%s %0.15g \n'], '  We use radius R = ', r);
writef(1,['%s %0.15g %s %0.15g \n'], '  and center ', xc, ', ', yc);
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], ['  Order:      2             4              8       ','     16']);
writef(1,['%s \n'], '  F(X)');
writef(1,['%s \n'], ' ');

for i = 1: nfunc;
[dumvar1, i ]=funcset ( 'SET', i );
for j = 1: 4;
norder = fix(2.^j);
[dumvar1, xc, yc, r, norder, result(j) ]=circle_cum( @funcd2, xc, yc, r, norder, result(j) );
end; j = fix(4+1);
writef(1,['%7s',repmat('%14.8f',1,5),'\n'], fname(i), result([1:4]));

end; i = fix(nfunc+1);
return;
end
function test02(varargin)
%
%*******************************************************************************
%
%! TEST02 tests CIRCLE_LUNE_AREA_2D.
%! TEST02 tests CIRCLE_SECTOR_AREA_2D.
%! TEST02 tests CIRCLE_TRIANGLE_AREA_2D.
%
persistent area1 area2 area3 i r theta1 theta2 ; 

if isempty(area1), area1=0; end;
if isempty(area2), area2=0; end;
if isempty(area3), area3=0; end;
if isempty(i), i=0; end;
if isempty(r), r=0; end;
if isempty(theta1), theta1=0; end;
if isempty(theta2), theta2=0; end;
%
r = 1.0d+00;
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'TEST02');
writef(1,['%s \n'], '  CIRCLE_LUNE_AREA_2D computes the area of a');
writef(1,['%s \n'], '    circular lune, defined by joining the endpoints');
writef(1,['%s \n'], '    of a circular arc.');
writef(1,['%s \n'], '  CIRCLE_SECTOR_AREA_2D computes the area of a');
writef(1,['%s \n'], '    circular sector, defined by joining the endpoints');
writef(1,['%s \n'], '    of a circular arc to the center.');
writef(1,['%s \n'], '  CIRCLE_TRIANGLE_AREA_2D computes the signed area of a');
writef(1,['%s \n'], '    triangle, defined by joining the endpoints');
writef(1,['%s \n'], '    of a circular arc and the center.');
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], ['      R            Theta1      Theta2        ','Sector       Triangle     Lune']);
writef(1,['%s \n'], ' ');
for i = 0: 12;
theta1 = 0.0d+00;
theta2 = ( i ) .* 2.0d+00 .* pi( ) ./ 12.0d+00;
[area1 , r, theta1, theta2 ]=circle_sector_area_2d( r, theta1, theta2 );
[area2 , r, theta1, theta2 ]=circle_triangle_area_2d( r, theta1, theta2 );
[area3 , r, theta1, theta2 ]=circle_lune_area_2d( r, theta1, theta2 );
writef(1,[repmat('%14.8f',1,6),'\n'], r, theta1, theta2, area1, area2, area3);
end; i = fix(12+1);
return;
end
function test024(varargin)
%
%*******************************************************************************
%
%! TEST024 tests CIRCLE_LUNE_AREA_2D.
%! TEST024 tests CIRCLE_LUNE_H_AREA_2D.
%! TEST024 tests CIRCLE_LUNE_W_AREA_2D.
%
persistent area1 area2 area3 h i r theta1 theta2 w ; 

if isempty(area1), area1=0; end;
if isempty(area2), area2=0; end;
if isempty(area3), area3=0; end;
if isempty(h), h=0; end;
if isempty(i), i=0; end;
if isempty(r), r=0; end;
if isempty(theta1), theta1=0; end;
if isempty(theta2), theta2=0; end;
if isempty(w), w=0; end;
%
r = 50.0d+00;
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'TEST024');
writef(1,['%s \n'], '  For the area of a circular lune,');
writef(1,['%s \n'], '  CIRCLE_LUNE_AREA_2D uses two angles;');
writef(1,['%s \n'], '  CIRCLE_LUNE_H_AREA_2D works from the height;');
writef(1,['%s \n'], '  CIRCLE_LUNE_W_AREA_2D works from the width.');
writef(1,['%s \n'], ' ');
writef(1,['%s %0.15g \n'], '  The circle has radius R = ', r);
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'THETA1 THETA2  H     W  Area(THETA) Area(H)  Area(W)');
writef(1,['%s \n'], ' ');
for i = 0: 12;
theta1 = 0.0d+00;
theta2 = ( i ) .* 2.0d+00 .* pi( ) ./ 12.0d+00;
w = 2.0d+00 .* r .* sin( 0.5d+00 .*( theta2 - theta1 ) );
h = r .*( 1.0d+00 - cos( 0.5d+00 .*( theta2 - theta1 ) ) );
[area1 , r, theta1, theta2 ]=circle_lune_area_2d( r, theta1, theta2 );
[area2 , r, h ]=circle_lune_h_area_2d( r, h );
[area3 , r, w ]=circle_lune_w_area_2d( r, w );
writef(1,[repmat('%6.2f',1,4),repmat('%10.4f',1,3),'\n'], theta1, theta2, h, w, area1, area2, area3);
end; i = fix(12+1);
return;
end
function test025(varargin)
%
%*******************************************************************************
%
%! TEST025 tests CIRCLE_SECTOR.
%
persistent area i j nfunc nr nrhi nrlo ntest radius radius_test result theta1 theta1_test theta2 theta2_test xc xc_test yc yc_test ; 

if isempty(nrlo), nrlo = 1; end;
if isempty(nrhi), nrhi = 5; end;
if isempty(ntest), ntest = 4; end;
%
if isempty(area), area=0; end;
if isempty(i), i=0; end;
if isempty(j), j=0; end;
if isempty(nfunc), nfunc=0; end;
if isempty(nr), nr=0; end;
if isempty(radius), radius=0; end;
if isempty(radius_test), radius_test([1:ntest]) =[ 1.0, 2.0, 4.0, 8.0 ]; end;
if isempty(result), result=zeros(1,nrhi); end;
if isempty(theta1), theta1=0; end;
if isempty(theta1_test), theta1_test([1:ntest]) =[ 0.0, 0.0, 0.0, 0.0 ]; end;
if isempty(theta2), theta2=0; end;
if isempty(theta2_test), theta2_test([1:ntest]) =[ 2.0, 1.0, 0.5, 0.25 ]; end;
if isempty(xc), xc=0; end;
if isempty(xc_test), xc_test([1:ntest]) =[ 0.0, 0.0, 0.0, 0.0 ]; end;
if isempty(yc), yc=0; end;
if isempty(yc_test), yc_test([1:ntest]) =[ 0.0, 0.0, 0.0, 0.0 ]; end;
%
%
[dumvar1, nfunc ]=funcset ( 'COUNT', nfunc );
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'TEST025');
writef(1,['%s \n'], '  CIRCLE_SECTOR estimates an integral in a circular sector.');
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], '  The user can specify NR, the number of radial values');
writef(1,['%s \n'], '  used to approximated the integral.');
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], '  In this test, computations will use values of NR');
writef(1,['%s %0.15g %s %0.15g \n'], '  from ', nrlo, ' to ', nrhi);
writef(1,['%s \n'], ' ');
for i = 1: ntest;
xc = xc_test(i);
yc = yc_test(i);
radius = radius_test(i);
theta1 = theta1_test(i) .* pi();
theta2 = theta2_test(i) .* pi();
[area , radius, theta1, theta2 ]=circle_sector_area_2d( radius, theta1, theta2 );
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], '  XC      YC      RADIUS  THETA1  THETA2  Area');
writef(1,['%s \n'], ' ');
writef(1,[repmat('%8.4f',1,6),'\n'], xc, yc, radius, theta1, theta2, area);
writef(1,['%s \n'], ' ');
for  nr =( nrlo):( nrhi ), writef(1,['%7s',repmat([repmat(' ',1,6),'%2i',repmat(' ',1,6)] ,1,14),'\n'], '   F   ',  nr); end;
writef(1,['%s \n'], ' ');
for j = 1: nfunc;
[dumvar1, j ]=funcset ( 'SET', j );
for nr = nrlo: nrhi;
[dumvar1, xc, yc, radius, theta1, theta2, nr,result(nr) ]=circle_sector( @funcd2, xc, yc, radius, theta1, theta2, nr,result(nr) );
end; nr = fix(nrhi+1);
writef(1,['%7s',repmat('%14.6f',1,5),'\n'], fname(j), result([nrlo:nrhi]));
end; j = fix(nfunc+1);
end; i = fix(ntest+1);
return;
end
function test026(varargin)
%
%*******************************************************************************
%
%! TEST026 tests CIRCLE_SECTOR.
%! TEST026 tests CIRCLE_SET_RT.
%! TEST026 tests CIRCLE_SUM_RT.
%
persistent area1 area2 area3 i j nfunc nr nr2 nt ntest ra radius radius_test result1 result2 resulta resultb rule rw ta theta1 theta1_test theta2 theta2_test theta3 tw xc xc_test yc yc_test zw ; 

if isempty(ntest), ntest = 4; end;
%
if isempty(area1), area1=0; end;
if isempty(area2), area2=0; end;
if isempty(area3), area3=0; end;
if isempty(i), i=0; end;
if isempty(j), j=0; end;
if isempty(nfunc), nfunc=0; end;
if isempty(nr), nr=0; end;
if isempty(nr2), nr2=0; end;
if isempty(nt), nt=0; end;
if isempty(ra), ra=zeros(1,5); end;
if isempty(radius), radius=0; end;
if isempty(radius_test), radius_test([1:ntest]) =[ 1.0, 2.0, 4.0, 8.0 ]; end;
if isempty(result1), result1=0; end;
if isempty(result2), result2=0; end;
if isempty(resulta), resulta=0; end;
if isempty(resultb), resultb=0; end;
if isempty(rule), rule=0; end;
if isempty(rw), rw=zeros(1,5); end;
if isempty(ta), ta=zeros(1,20); end;
if isempty(theta1), theta1=0; end;
if isempty(theta1_test), theta1_test([1:ntest]) =[ 0.0, 0.0, 0.0, 0.0 ]; end;
if isempty(theta2), theta2=0; end;
if isempty(theta2_test), theta2_test([1:ntest]) =[ 2.0, 1.0, 0.5, 0.25 ]; end;
if isempty(theta3), theta3=0; end;
if isempty(tw), tw=zeros(1,20); end;
if isempty(xc), xc=0; end;
if isempty(xc_test), xc_test([1:ntest]) =[ 0.0, 0.0, 0.0, 0.0 ]; end;
if isempty(yc), yc=0; end;
if isempty(yc_test), yc_test([1:ntest]) =[ 0.0, 0.0, 0.0, 0.0 ]; end;
if isempty(zw), zw=0; end;
%
%
[dumvar1, nfunc ]=funcset ( 'COUNT', nfunc );
nr = 5;
rule = 9;
[ rule, nr2, ra, rw, nt, ta, tw, zw ]=circle_set_rt( rule, nr2, ra, rw, nt, ta, tw, zw );
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'TEST026');
writef(1,['%s \n'], '  CIRCLE_SECTOR estimates an integral in a circular sector.');
writef(1,['%s \n'], '  CIRCLE_SET_RT sets an integration rule in a circle.');
writef(1,['%s \n'], '  CIRCLE_SUM_RT uses an integration rule in a circle.');
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], '  To test CIRCLE_SECTOR, we will estimate an integral over');
writef(1,['%s \n'], '  a sector, and over its complement and add the results');
writef(1,['%s \n'], '  to get RESULT1.');
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], '  We will also estimate the integral over the whole circle');
writef(1,['%s \n'], '  using CIRCLE_SET_RT and CIRCLE_SUM_RT to get RESULT2.');
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], '  We will then compare RESULT1 and RESULT2.');
writef(1,['%s \n'], ' ');
writef(1,['%s %0.15g \n'], '  In this test, CIRCLE_SECTOR computations will use NR = ',nr);
writef(1,['%s %0.15g \n'], '  CIRCLE_SET_RT/CIRCLE_SUM_RT will use rule number ', rule);
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], '  "Sector1" and "Sector2" are the CIRCLE_SECTOR computations');
writef(1,['%s \n'], '  for the sector and its complement.');
writef(1,['%s \n'], '  "Sum" is the sum of Sector1 and Sector2.');
writef(1,['%s \n'], '  "Circle" is the computation for CIRCLE_SET_RT/CIRCLE_SUM_RT.');
writef(1,['%s \n'], ' ');
for i = 1: ntest;
xc = xc_test(i);
yc = yc_test(i);
radius = radius_test(i);
theta1 = theta1_test(i) .* pi();
theta2 = theta2_test(i) .* pi();
theta3 = theta2 + 2.0 .* pi() -( theta2 - theta1 );
[area1 , radius, theta1, theta2 ]=circle_sector_area_2d( radius, theta1, theta2 );
[area2 , radius, theta2, theta3 ]=circle_sector_area_2d( radius, theta2, theta3 );
[area3 , radius ]=circle_area_2d( radius );
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], '  XC       YC       RADIUS   THETA1   THETA2   Area1   Area2  Circle');
writef(1,['%s \n'], ' ');
writef(1,[repmat('%9.4f',1,8),'\n'], xc, yc, radius, theta1, theta2, area1, area2, area3);
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], '     F   Sector1       Sector2         Sum         Circle');
writef(1,['%s \n'], ' ');
for j = 1: nfunc;
[dumvar1, j ]=funcset ( 'SET', j );
[dumvar1, xc, yc, radius, theta1, theta2, nr, resulta ]=circle_sector( @funcd2, xc, yc, radius, theta1, theta2, nr, resulta );
[dumvar1, xc, yc, radius, theta2, theta3, nr, resultb ]=circle_sector( @funcd2, xc, yc, radius, theta2, theta3, nr, resultb );
result1 = resulta + resultb;
[dumvar1, xc, yc, radius, nr2, ra, rw, nt, ta, tw, zw,result2 ]=circle_sum_rt( @funcd2, xc, yc, radius, nr2, ra, rw, nt, ta, tw, zw,result2 );
writef(1,['%7s',repmat('%14.6f',1,4),'\n'], fname(j), resulta, resultb,resulta + resultb, result2);
end; j = fix(nfunc+1);
end; i = fix(ntest+1);
return;
end
function test03(varargin)
%
%*******************************************************************************
%
%! TEST03 tests CIRCLE_SET_RT.
%! TEST03 tests CIRCLE_SUM_RT.
%
persistent i ihi ilo max_r max_rule max_t nfunc nr nt pi r ra result rule rw ta tw xc yc zw ; 

if isempty(max_r), max_r = 10; end;
if isempty(max_rule), max_rule = 9; end;
if isempty(max_t), max_t = 20; end;
%
if isempty(i), i=0; end;
if isempty(ihi), ihi=0; end;
if isempty(ilo), ilo=0; end;
if isempty(nfunc), nfunc=0; end;
if isempty(nr), nr=0; end;
if isempty(nt), nt=0; end;
if isempty(pi), pi=0; end;
if isempty(r), r=0; end;
if isempty(ra), ra=zeros(1,max_r); end;
if isempty(result), result=zeros(1,max_rule); end;
if isempty(rw), rw=zeros(1,max_r); end;
if isempty(rule), rule=0; end;
if isempty(ta), ta=zeros(1,max_t); end;
if isempty(tw), tw=zeros(1,max_t); end;
if isempty(xc), xc=0; end;
if isempty(yc), yc=0; end;
if isempty(zw), zw=0; end;
%
%
[dumvar1, nfunc ]=funcset ( 'COUNT', nfunc );
xc = 1.0d+00;
yc = 1.0d+00;
r = 1.0d+00;
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'TEST03');
writef(1,['%s \n'], '  For R, Theta product rules on the unit circle,');
writef(1,['%s \n'], '  CIRCLE_SET_RT sets a rule.');
writef(1,['%s \n'], '  CIRCLE_SUM_RT evaluates the rule in an arbitrary circle.');
writef(1,['%s \n'], ' ');
writef(1,['%s %0.15g \n'], '  We use a radius ', r);
writef(1,['%s %0.15g %s %0.15g \n'], '  and center ', xc, ', ', yc);
writef(1,['%s \n'], ' ');
for ilo = 1: 5: max_rule;
ihi = fix(min( ilo +  4, max_rule ));
writef(1,['%s \n'], ' ');
for  rule =( ilo):( ihi ), writef(1,['%7s',repmat(' ',1,7),repmat(['%7i',repmat(' ',1,7)] ,1,5),'\n'], 'Rule:  ',  rule); end;
writef(1,['%s \n'], 'Function ');
for i = 1: nfunc;
[dumvar1, i ]=funcset ( 'SET', i );
for rule = ilo: ihi;
[ rule, nr, ra, rw, nt, ta, tw, zw ]=circle_set_rt( rule, nr, ra, rw, nt, ta, tw, zw );
[dumvar1, i ]=funcset ( 'SET', i );
[dumvar1, xc, yc, r, nr, ra, rw, nt, ta, tw, zw,result(rule) ]=circle_sum_rt( @funcd2, xc, yc, r, nr, ra, rw, nt, ta, tw, zw,result(rule) );
end; rule = fix(ihi+1);
writef(1,['%7s',repmat('%14.8f',1,5),'\n'], fname(i), result([ilo:ihi]));
end; i = fix(nfunc+1);
end; ilo = fix(max_rule+1);
return;
end
function test04(varargin)
%
%*******************************************************************************
%
%! TEST04 tests CIRCLE_SET_XY.
%! TEST04 tests CIRCLE_SUM_XY.
%
persistent i ihi ilo max_order max_rule nfunc norder r result rule weight xc xtab yc ytab ; 

if isempty(max_order), max_order = 64; end;
if isempty(max_rule), max_rule = 13; end;
%
if isempty(i), i=0; end;
if isempty(ihi), ihi=0; end;
if isempty(ilo), ilo=0; end;
if isempty(nfunc), nfunc=0; end;
if isempty(norder), norder=0; end;
if isempty(r), r=0; end;
if isempty(result), result=zeros(1,max_rule); end;
if isempty(rule), rule=0; end;
if isempty(weight), weight=zeros(1,max_order); end;
if isempty(xc), xc=0; end;
if isempty(xtab), xtab=zeros(1,max_order); end;
if isempty(yc), yc=0; end;
if isempty(ytab), ytab=zeros(1,max_order); end;
%
%
[dumvar1, nfunc ]=funcset ( 'COUNT', nfunc );
xc = 1.0d+00;
yc = 1.0d+00;
r = 1.0d+00;
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'TEST04');
writef(1,['%s \n'], '  CIRCLE_SET_XY sets a quadrature rule for the unit circle.');
writef(1,['%s \n'], '  CIRCLE_SUM_XY evaluates the quadrature rule');
writef(1,['%s \n'], '  in an arbitrary circle.');
writef(1,['%s \n'], ' ');
writef(1,['%s %0.15g \n'], '  We use a radius ', r);
writef(1,['%s %0.15g %s %0.15g \n'], '  and center ', xc, ', ', yc);
writef(1,['%s \n'], ' ');
for ilo = 1: 5: max_rule;
ihi = fix(min( ilo +  4, max_rule ));
writef(1,['%s \n'], ' ');
for  rule =( ilo):( ihi ), writef(1,['%7s',repmat(' ',1,7),repmat(['%7i',repmat(' ',1,7)] ,1,5),'\n'], 'Rule:  ',  rule); end;
writef(1,['%s \n'], 'Function ');
for i = 1: nfunc;
[dumvar1, i ]=funcset ( 'SET', i );
for rule = ilo: ihi;
[ rule, norder, xtab, ytab, weight ]=circle_set_xy( rule, norder, xtab, ytab, weight );
[dumvar1, i ]=funcset ( 'SET', i );
[dumvar1, xc, yc, r, norder, xtab, ytab, weight,result(rule) ]=circle_sum_xy( @funcd2, xc, yc, r, norder, xtab, ytab, weight,result(rule) );
end; rule = fix(ihi+1);
writef(1,['%7s',repmat('%14.8f',1,5),'\n'], fname(i), result([ilo:ihi]));
end; i = fix(nfunc+1);
end; ilo = fix(max_rule+1);
return;
end
function test05(varargin)
%
%*******************************************************************************
%
%! TEST05 tests CONE_UNIT_3D.
%
persistent h i nfunc r result ; 

if isempty(h), h=0; end;
if isempty(i), i=0; end;
if isempty(nfunc), nfunc=0; end;
if isempty(r), r=0; end;
if isempty(result), result=0; end;
%
%
[dumvar1, nfunc ]=funcset ( 'COUNT', nfunc );
r = 1.0d+00;
h = 1.0d+00;
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'TEST05');
writef(1,['%s \n'], '  CONE_UNIT_3D approximates integrals in a unit cone.');
writef(1,['%s \n'], ' ');
writef(1,['%s %0.15g \n'], '  Volume = ', cone_volume_3d ( r, h ));
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], '  F(X)    CONE_3D');
writef(1,['%s \n'], ' ');
for i = 1: nfunc;
[dumvar1, i ]=funcset ( 'SET', i );
[dumvar1, result ]=cone_unit_3d( @funcd3, result );
writef(1,['%7s','%14.8f','\n'], fname(i), result);
end; i = fix(nfunc+1);
return;
end
function test06(varargin)
%
%*******************************************************************************
%
%! TEST06 tests CUBE_SHELL_ND.
%
persistent i max_n n nfunc r1 r2 result ; 

if isempty(max_n), max_n = 4; end;
%
if isempty(i), i=0; end;
if isempty(n), n=0; end;
if isempty(nfunc), nfunc=0; end;
if isempty(r1), r1=0; end;
if isempty(r2), r2=0; end;
if isempty(result), result=0; end;
%
%
[dumvar1, nfunc ]=funcset ( 'COUNT', nfunc );
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'TEST06');
writef(1,['%s \n'], '  CUBE_SHELL_ND approximates integrals in a');
writef(1,['%s \n'], '  cubical shell in ND.');
writef(1,['%s \n'], ' ');
r1 = 0.0d+00;
r2 = 1.0d+00;
writef(1,['%s \n'], ' ');
writef(1,['%s %0.15g \n'], '  Inner radius = ', r1);
writef(1,['%s %0.15g \n'], '  Outer radius = ', r2);
writef(1,['%s \n'], ' ');

for n = 2: max_n;

writef(1,['%s \n'], ' ');
writef(1,['%s %0.15g \n'], '  Spatial dimension N = ', n);
writef(1,['%s %0.15g \n'], '  Volume = ', cube_shell_volume_nd ( n, r1, r2 ));
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], '  F(X)      CUBE_SHELL_ND');
writef(1,['%s \n'], ' ');

for i = 1: nfunc;
[dumvar1, i ]=funcset ( 'SET', i );
[dumvar1, n, r1, r2, result ]=cube_shell_nd( @funcdn, n, r1, r2, result );
writef(1,['%7s',repmat('%14.8f',1,2),'\n'], fname(i), result);
end; i = fix(nfunc+1);
end; n = fix(max_n+1);

r1 = 1.0d+00;
r2 = 2.0d+00;

writef(1,['%s \n'], ' ');
writef(1,['%s %0.15g \n'], '  Inner radius = ', r1);
writef(1,['%s %0.15g \n'], '  Outer radius = ', r2);
writef(1,['%s \n'], ' ');

for n = 2:  max_n;

writef(1,['%s \n'], ' ');
writef(1,['%s %0.15g \n'], '  Spatial dimension N = ', n);
writef(1,['%s %0.15g \n'], '  Volume = ', cube_shell_volume_nd ( n, r1, r2 ));
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], '  F(X)      CUBE_SHELL_ND');
writef(1,['%s \n'], ' ');

for i = 1: nfunc;
[dumvar1, i ]=funcset ( 'SET', i );
[dumvar1, n, r1, r2, result ]=cube_shell_nd( @funcdn, n, r1, r2, result );
writef(1,['%7s','%14.8f','\n'], fname(i), result);
end; i = fix(nfunc+1);

end; n =  fix(max_n+1);

return;
end
function test07(varargin)
%
%*******************************************************************************
%
%! TEST07 tests CUBE_UNIT_3D.
%! TEST07 tests CUBE_UNIT_ND.
%! TEST07 tests QMULT_3D.
%! TEST07 tests RECTANGLE_3D.
%
persistent a a1 b b1 i k max_k n nfunc qa qb result1 result2 result3 ; 

if isempty(max_k), max_k = 10; end;
%
if isempty(a1), a1=0; end;
if isempty(a), a=zeros(1,3); end;
if isempty(b1), b1=0; end;
if isempty(b), b=zeros(1,3); end;
if isempty(i), i=0; end;
if isempty(k), k=0; end;
if isempty(n), n = 3; end;
if isempty(nfunc), nfunc=0; end;
if isempty(qa), qa=zeros(1,max_k); end;
if isempty(qb), qb=zeros(1,max_k); end;
if isempty(result1), result1=0; end;
if isempty(result2), result2=0; end;
if isempty(result3), result3=0; end;
%
%
[dumvar1, nfunc ]=funcset ( 'COUNT', nfunc );
k = fix(max_k);
a1 = -1.0d+00;
b1 = +1.0d+00;
a(1) = -1.0d+00;
a(2) = -1.0d+00;
a(3) = -1.0d+00;
b(1) = 1.0d+00;
b(2) = 1.0d+00;
b(3) = 1.0d+00;
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'TEST07');
writef(1,['%s \n'], '  CUBE_UNIT_3D approximates integrals in the unit cube.');
writef(1,['%s \n'], '  CUBE_UNIT_ND approximates integrals in the unit cube in ND.');
writef(1,['%s \n'], '  QMULT_3D approximates triple integrals.');
writef(1,['%s \n'], '  RECTANGLE_3D approximates integrals in a rectangular block.');
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], ['  F(X)    CUBE_UNIT_3D  CUBE_UNIT_ND  QMULT_3D      ','RECTANGLE_3D']);
writef(1,['%s \n'], ' ');
for i = 1: nfunc;
[dumvar1, i ]=funcset ( 'SET', i );
[dumvar1, result1 ]=cube_unit_3d( @funcd3, result1 );
[dumvar1, qa, qb, n, k ]=cube_unit_nd( @funcdn, qa, qb, n, k );
result2 = qmult_3d( @funcd3, a1, b1, @fu18, @fl18, @fu28, @fl28 );
[dumvar1, a, b, result3 ]=rectangle_3d( @funcd3, a, b, result3 );
writef(1,['%7s',repmat('%14.8f',1,5),'\n'], fname(i), result1, qb(k), result2, result3);
end; i = fix(nfunc+1);
return;
end
function test08(varargin)
%
%*******************************************************************************
%
%! TEST08 tests CUBE_UNIT_ND.
%
persistent i i_test k k_test khi klo max_k max_test n n_test nfunc qa qb ; 

if isempty(max_k), max_k = 10; end;
if isempty(max_test), max_test = 2; end;
%
if isempty(i), i=0; end;
if isempty(i_test), i_test=0; end;
if isempty(k), k=0; end;
if isempty(khi), khi=0; end;
if isempty(klo), klo=0; end;
if isempty(k_test), k_test([1:max_test]) =[ 10, 5 ]; end;
if isempty(n), n=0; end;
if isempty(n_test), n_test([1:max_test]) =[ 2, 3 ]; end;
if isempty(nfunc), nfunc=0; end;
if isempty(qa), qa=zeros(1,max_k); end;
if isempty(qb), qb=zeros(1,max_k); end;
%
%
[dumvar1, nfunc ]=funcset ( 'COUNT', nfunc );
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'TEST08');
writef(1,['%s \n'], '  CUBE_UNIT_ND approximates integrals inside the unit N-cube.');
writef(1,['%s \n'], ' ');
for i_test = 1: max_test;

n = fix(n_test(i_test));
k = fix(k_test(i_test));
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], ' ');
writef(1,['%s %0.15g \n'], '  Spatial dimension N = ', n);
writef(1,['%s %0.15g \n'], '  Value of K = ', k);
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], '  F(X)    CUBE_UNIT_ND');
writef(1,['%s \n'], ' ');
for i = 1: nfunc;
[dumvar1, i ]=funcset ( 'SET', i );
[dumvar1, qa, qb, n, k ]=cube_unit_nd( @funcdn, qa, qb, n, k );
for klo = 1: 5: k;
khi = fix(min( klo + 4, k ));
if( klo == 1 )
writef(1,['%7s',repmat('%14.8f',1,5),'\n'], fname(i), qa([klo:khi]));
else;
writef(1,[repmat(' ',1,7),repmat('%14.8f',1,5),'\n'],           qa([klo:khi]));
end;
end; klo = fix(k+1);
for klo = 1: 5: k;
khi = fix(min( klo + 4, k ));
writef(1,[repmat(' ',1,7),repmat('%14.8f',1,5),'\n'],           qb([klo:khi]));
end; klo = fix(k+1);

end; i = fix(nfunc+1);
end; i_test = fix(max_test+1);
return;
end
function test10(varargin)
%
%*******************************************************************************
%
%! TEST10 tests HEXAGON_UNIT_SET.
%! TEST10 tests HEXAGON_SUM.
%
persistent i ihi ilo max_order max_rule nfunc norder rad result rule weight xc xtab yc ytab ; 

if isempty(max_order), max_order = 64; end;
if isempty(max_rule), max_rule = 4; end;
%
if isempty(i), i=0; end;
if isempty(ihi), ihi=0; end;
if isempty(ilo), ilo=0; end;
if isempty(nfunc), nfunc=0; end;
if isempty(norder), norder=0; end;
if isempty(rad), rad=0; end;
if isempty(result), result=zeros(1,max_rule); end;
if isempty(rule), rule=0; end;
if isempty(weight), weight=zeros(1,max_order); end;
if isempty(xc), xc=0; end;
if isempty(xtab), xtab=zeros(1,max_order); end;
if isempty(yc), yc=0; end;
if isempty(ytab), ytab=zeros(1,max_order); end;
%
%
[dumvar1, nfunc ]=funcset ( 'COUNT', nfunc );
xc = 0.0d+00;
yc = 0.0d+00;
rad = 2.0d+00;

writef(1,['%s \n'], ' ');
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'TEST10');
writef(1,['%s \n'], '  HEXAGON_UNIT_SET sets a quadrature rule for the unit hexagon.');
writef(1,['%s \n'], '  HEXAGON_SUM evaluates the quadrature rule');
writef(1,['%s \n'], '  in an arbitrary hexagon.');
writef(1,['%s \n'], ' ');
writef(1,['%s %0.15g \n'], '  We use a radius ', rad);
writef(1,['%s %0.15g %s %0.15g \n'], '  and center ', xc, ', ', yc);
writef(1,['%s \n'], ' ');
for ilo = 1: 5: max_rule;
ihi = fix(min( ilo + 4, max_rule ));
writef(1,['%s \n'], ' ');
for  rule =( ilo):( ihi ), writef(1,['%s %0.15g \n'], 'Rule:    ',  rule); end;
writef(1,['%s \n'], 'Function ');
writef(1,['%s \n'], ' ');
for i = 1: nfunc;
[dumvar1, i ]=funcset ( 'SET', i );
for rule = ilo: ihi;
[ rule, norder, xtab, ytab, weight ]=hexagon_unit_set( rule, norder, xtab, ytab, weight );
[dumvar1, xc, yc, rad, norder, xtab, ytab, weight,result(rule) ]=hexagon_sum( @funcd2, xc, yc, rad, norder, xtab, ytab, weight,result(rule) );
end; rule = fix(ihi+1);
writef(1,['%7s',repmat('%14.8f',1,5),'\n'], fname(i), result([ilo:ihi]));
end; i = fix(nfunc+1);
end; ilo = fix(max_rule+1);
return;
end
function test11(varargin)
%
%*******************************************************************************
%
%! TEST11 tests OCTAHEDRON_UNIT_ND.
%
persistent i max_n n nfunc result ; 

if isempty(max_n), max_n = 3; end;
%
if isempty(i), i=0; end;
if isempty(n), n=0; end;
if isempty(nfunc), nfunc=0; end;
if isempty(result), result=zeros(1,3); end;
%
%
[dumvar1, nfunc ]=funcset ( 'COUNT', nfunc );
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'TEST11');
writef(1,['%s \n'], '  OCTAHEDRON_UNIT_ND approximates integrals in a unit');
writef(1,['%s \n'], '  octahedron in N dimensions.');
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], '  F(X)    N = 1    N = 2   N = 3 ');
writef(1,['%s \n'], ' ');

for i = 1: nfunc;
[dumvar1, i ]=funcset ( 'SET', i );
for n = 1: max_n;
[dumvar1, n, result(n) ]=octahedron_unit_nd( @funcdn, n, result(n) );
end; n = fix(max_n+1);
writef(1,['%7s',repmat('%14.8f',1,3),'\n'], fname(i), result([1:max_n]));
end; i = fix(nfunc+1);
return;
end
function test12(varargin)
%
%*******************************************************************************
%
%! TEST12 tests PARALLELIPIPED_VOLUME_ND.
%
persistent i lda n v volume ; 

if isempty(lda), lda = 5; end;
%
if isempty(i), i=0; end;
if isempty(n), n=0; end;
if isempty(v), v=zeros(lda,lda); end;
if isempty(volume), volume=0; end;
%
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'TEST12');
writef(1,['%s \n'], '  PARALLELIPIPED_VOLUME_ND computes the volume of a');
writef(1,['%s \n'], '  parallelipiped in N dimensions.');
writef(1,['%s \n'], ' ');
for n = 2: 4;
writef(1,['%s \n'], ' ');
writef(1,['%s %0.15g \n'], '  Spatial dimension N = ', n);
%
%  Set the values of the parallelipiped.
%
[ lda, n, v ]=setsim( lda, n, v );
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], '  Parallelipiped vertices:');
writef(1,['%s \n'], ' ');
for i = 1: n+1;
writef(1,[repmat('%4.0f',1,4),'\n'], v(i,[1:n]));
end; i = fix(n+1+1);
[volume , lda, n, v ]=parallelipiped_volume_nd( lda, n, v );
writef(1,['%s \n'], ' ');
writef(1,['%s %0.15g \n'], 'Volume is ', volume);
end; n = fix(4+1);
return;
end
function test13(varargin)
%
%*******************************************************************************
%
%! TEST13 tests PYRAMID_UNIT_3D.
%
persistent h i nfunc r result ; 

if isempty(h), h=0; end;
if isempty(i), i=0; end;
if isempty(nfunc), nfunc=0; end;
if isempty(r), r=0; end;
if isempty(result), result=0; end;
%
%
[dumvar1, nfunc ]=funcset ( 'COUNT', nfunc );
r = 1.0d+00;
h = 1.0d+00;
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'TEST13');
writef(1,['%s \n'], '  PYRAMID_UNIT_3D approximates integrals in a unit pyramid.');
writef(1,['%s \n'], ' ');
writef(1,['%s %0.15g \n'], '  Volume = ', pyramid_volume_3d ( r, h ));
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], '  F(X)    PYRAMID_3D');
writef(1,['%s \n'], ' ');
for i = 1: nfunc;
[dumvar1, i ]=funcset ( 'SET', i );
[dumvar1, result ]=pyramid_unit_3d( @funcd3, result );
writef(1,['%7s','%14.8f','\n'], fname(i), result);
end; i = fix(nfunc+1);
return;
end
function test14(varargin)
%
%*******************************************************************************
%
%! TEST14 tests QMULT_1D.
%
persistent a b i nfunc result ; 

if isempty(a), a=0; end;
if isempty(b), b=0; end;
if isempty(i), i=0; end;
if isempty(nfunc), nfunc=0; end;
if isempty(result), result=0; end;
%
%
[dumvar1, nfunc ]=funcset ( 'COUNT', nfunc );
a = -1.0d+00;
b = 1.0d+00;

writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'TEST14');
writef(1,['%s \n'], '  QMULT_1D approximates an integral on a');
writef(1,['%s \n'], '  one-dimensional interval.');
writef(1,['%s \n'], ' ');
writef(1,['%s %0.15g %s %0.15g \n'], '  We use the interval ', a, ' to ', b);
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], '  F(X)     QMULT_1D');
writef(1,['%s \n'], ' ');

for i = 1: nfunc;
[dumvar1, i ]=funcset ( 'SET', i );
result = qmult_1d( @funcd1, a, b );
writef(1,['%7s','%14.8f','\n'], fname(i), result);

end; i = fix(nfunc+1);
return;
end
function test15(varargin)
%
%*******************************************************************************
%
%! TEST15 tests SPHERE_UNIT_SURFACE_07_3D.
%! TEST15 tests SPHERE_UNIT_SURFACE_11_3D.
%! TEST15 tests SPHERE_UNIT_SURFACE_14_3D.
%! TEST15 tests SPHERE_UNIT_SURFACE_15_3D.
%
persistent i nfunc result1 result2 result3 result4 ; 

if isempty(i), i=0; end;
if isempty(nfunc), nfunc=0; end;
if isempty(result1), result1=0; end;
if isempty(result2), result2=0; end;
if isempty(result3), result3=0; end;
if isempty(result4), result4=0; end;
%
%
[dumvar1, nfunc ]=funcset ( 'COUNT', nfunc );
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'TEST15');
writef(1,['%s \n'], '  For integrals on the surface of the unit sphere in 3D:');
writef(1,['%s \n'], '  SPHERE_UNIT_SURFACE_07_3D uses a formula of degree 7.');
writef(1,['%s \n'], '  SPHERE_UNIT_SURFACE_11_3D uses a formula of degree 11.');
writef(1,['%s \n'], '  SPHERE_UNIT_SURFACE_14_3D uses a formula of degree 14.');
writef(1,['%s \n'], '  SPHERE_UNIT_SURFACE_15_3D uses a formula of degree 15.');
writef(1,['%s \n'], ' ');
writef(1,['%s %0.15g \n'], '  Unit sphere area = ', sphere_unit_area_nd ( 3 ));
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], '  F(X)    S3S07        S3S11         S3S14         S3S15      ');
writef(1,['%s \n'], ' ');

for i = 1: nfunc;
[dumvar1, i ]=funcset ( 'SET', i );
[dumvar1, result1 ]=sphere_unit_surface_07_3d( @funcd3, result1 );
[dumvar1, result2 ]=sphere_unit_surface_11_3d( @funcd3, result2 );
[dumvar1, result3 ]=sphere_unit_surface_14_3d( @funcd3, result3 );
[dumvar1, result4 ]=sphere_unit_surface_15_3d( @funcd3, result4 );

writef(1,['%7s',repmat('%14.8f',1,4),'\n'], fname(i), result1, result2, result3, result4);
end; i = fix(nfunc+1);

return;
end
function test16(varargin)
%
%*******************************************************************************
%
%! TEST16 tests SPHERE_UNIT_SURFACE_3_ND.
%! TEST16 tests SPHERE_UNIT_SURFACE_4_ND.
%! TEST16 tests SPHERE_UNIT_SURFACE_5_ND.
%! TEST16 tests SPHERE_UNIT_SURFACE_7_1_ND.
%! TEST16 tests SPHERE_UNIT_SURFACE_7_2_ND.
%
persistent i n nfunc result1 result2 result3 result4 result5 ; 

if isempty(i), i=0; end;
if isempty(n), n=0; end;
if isempty(nfunc), nfunc=0; end;
if isempty(result1), result1=0; end;
if isempty(result2), result2=0; end;
if isempty(result3), result3=0; end;
if isempty(result4), result4=0; end;
if isempty(result5), result5=0; end;
%
%
[dumvar1, nfunc ]=funcset ( 'COUNT', nfunc );
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'TEST16');
writef(1,['%s \n'], '  For integrals on the surface of the unit sphere in ND:');
writef(1,['%s \n'], '  SPHERE_UNIT_SURFACE_3_ND uses a formula of degree 3;');
writef(1,['%s \n'], '  SPHERE_UNIT_SURFACE_4_ND uses a formula of degree 4;');
writef(1,['%s \n'], '  SPHERE_UNIT_SURFACE_5_ND uses a formula of degree 5.');
writef(1,['%s \n'], '  SPHERE_UNIT_SURFACE_7_1_ND uses a formula of degree 7.');
writef(1,['%s \n'], '  SPHERE_UNIT_SURFACE_7_2_ND uses a formula of degree 7.');
writef(1,['%s \n'], ' ');

for n = 2: 4;

writef(1,['%s \n'], ' ');
writef(1,['%s %0.15g \n'], '  Spatial dimension N = ', n);
writef(1,['%s %0.15g \n'], '  Unit sphere area = ', sphere_unit_area_nd ( n ));
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], ['  Rule:     #3            #4            #5           #7.1','      #7.2']);
writef(1,['%s \n'], '  Function');
writef(1,['%s \n'], ' ');

for i = 1: nfunc;
[dumvar1, i ]=funcset ( 'SET', i );
[dumvar1, n, result1 ]=sphere_unit_surface_3_nd( @funcdn, n, result1 );
[dumvar1, n, result2 ]=sphere_unit_surface_4_nd( @funcdn, n, result2 );
[dumvar1, n, result3 ]=sphere_unit_surface_5_nd( @funcdn, n, result3 );
[dumvar1, n, result4 ]=sphere_unit_surface_7_1_nd( @funcdn, n, result4 );
[dumvar1, n, result5 ]=sphere_unit_surface_7_2_nd( @funcdn, n, result5 );
writef(1,['%7s',repmat('%14.8f',1,5),'\n'], fname(i), result1, result2, result3, result4,result5);
end; i = fix(nfunc+1);

end; n = fix(4+1);

return;
end
function test165(varargin)
%
%*******************************************************************************
%
%! TEST165 tests SPHERE_SURFACE_5_ND.
%! TEST165 tests SPHERE_SURFACE_7_1_ND.
%
persistent i max_n n nfunc r result1 result2 xc ; 

if isempty(max_n), max_n = 5; end;
%
if isempty(i), i=0; end;
if isempty(n), n=0; end;
if isempty(nfunc), nfunc=0; end;
if isempty(r), r=0; end;
if isempty(result1), result1=0; end;
if isempty(result2), result2=0; end;
if isempty(xc), xc=zeros(1,max_n); end;
%
%
[dumvar1, nfunc ]=funcset ( 'COUNT', nfunc );
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'TEST165');
writef(1,['%s \n'], '  For integrals on the surface of a sphere in ND:');
writef(1,['%s \n'], '  SPHERE_SURFACE_5_ND uses a formula of degree 5.');
writef(1,['%s \n'], '  SPHERE_SURFACE_7_1_ND uses a formula of degree 7.');
writef(1,['%s \n'], ' ');
r = 2.0d+00;
xc([1:max_n]) = 1.0d+00;
for n = 2: 4;
writef(1,['%s \n'], ' ');
writef(1,['%s %0.15g \n'], '  Spatial dimension N = ', n);
writef(1,['%s \n'], '  Sphere center = ');
writef(1,['%0.15g \n'], xc([1:n]));
writef(1,['%s %0.15g \n'], '  Sphere radius = ', r);
writef(1,['%s %0.15g \n'], '  Sphere area = ', sphere_area_nd ( n, r ));
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], '  Rule:     #5           #7.1');
writef(1,['%s \n'], '  Function');
writef(1,['%s \n'], ' ');
for i = 1: nfunc;
[dumvar1, i ]=funcset ( 'SET', i );
[dumvar1, n, xc, r, result1 ]=sphere_surface_5_nd( @funcdn, n, xc, r, result1 );
[dumvar1, n, xc, r, result2 ]=sphere_surface_7_1_nd( @funcdn, n, xc, r, result2 );
writef(1,['%7s',repmat('%14.8f',1,2),'\n'], fname(i), result1, result2);
end; i = fix(nfunc+1);
end; n = fix(4+1);
return;
end
function test17(varargin)
%
%*******************************************************************************
%
%! TEST17 tests SIMPLEX_ND.
%
persistent i j lda n nfunc result v ; 

if isempty(lda), lda = 5; end;
%
if isempty(i), i=0; end;
if isempty(j), j=0; end;
if isempty(n), n=0; end;
if isempty(nfunc), nfunc=0; end;
if isempty(result), result=0; end;
if isempty(v), v=zeros(lda,lda); end;
%
%
[dumvar1, nfunc ]=funcset ( 'COUNT', nfunc );
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'TEST17');
writef(1,['%s \n'], '  SIMPLEX_ND approximates integrals inside an');
writef(1,['%s \n'], '  arbitrary simplex in ND.');
writef(1,['%s \n'], ' ');

for n = 2: 4;

writef(1,['%s \n'], ' ');
writef(1,['%s %0.15g \n'], '  Spatial dimension N = ', n);
%
%  Restore values of simplex.
%
[ lda, n, v ]=setsim( lda, n, v );
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], '  Simplex vertices:');
writef(1,['%s \n'], ' ');

for i = 1: n+1;
writef(1,[repmat('%4.0f',1,4),'\n'], v(i,[1:n]));
end; i = fix(n+1+1);

writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'F(X)    SIMPLEX_ND');
writef(1,['%s \n'], ' ');
for i = 1: nfunc;
[dumvar1, i ]=funcset ( 'SET', i );
[dumvar1, lda, n, v, result ]=simplex_nd( @funcdn, lda, n, v, result );
writef(1,['%7s','%14.8f','\n'], fname(i), result);

[ lda, n, v ]=setsim( lda, n, v );
end; i = fix(nfunc+1);
end; n = fix(4+1);

return;
end
function test18(varargin)
%
%*******************************************************************************
%
%! TEST18 tests SIMPLEX_VOLUME_ND.
%
persistent i lda n v volume ; 

if isempty(lda), lda = 5; end;
%
if isempty(i), i=0; end;
if isempty(n), n=0; end;
if isempty(v), v=zeros(lda,lda); end;
if isempty(volume), volume=0; end;
%
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'TEST18');
writef(1,['%s \n'], '  SIMPLEX_VOLUME_ND computes the volume of a simplex');
writef(1,['%s \n'], '  in N dimensions.');
writef(1,['%s \n'], ' ');
for n = 2: 4;
writef(1,['%s \n'], ' ');
writef(1,['%s %0.15g \n'], '  Spatial dimension N = ', n);
%
%  Set the values of the simplex.
%
[ lda, n, v ]=setsim( lda, n, v );
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], '  Simplex vertices:');
writef(1,['%s \n'], ' ');
for i = 1: n+1;
writef(1,[repmat('%4.0f',1,4),'\n'], v(i,[1:n]));
end; i = fix(n+1+1);
[volume , lda, n, v ]=simplex_volume_nd( lda, n, v );
writef(1,['%s \n'], ' ');
writef(1,['%s %0.15g \n'], 'Volume is ', volume);
end; n = fix(4+1);
return;
end
function test19(varargin)
%
%*******************************************************************************
%
%! TEST19 tests SPHERE_UNIT_07_3D.
%! TEST19 tests SPHERE_UNIT_14_3D.
%! TEST19 tests SPHERE_UNIT_15_3D.
%
persistent i nfunc pi result1 result2 result3 ; 

if isempty(i), i=0; end;
if isempty(nfunc), nfunc=0; end;
if isempty(pi), pi=0; end;
if isempty(result1), result1=0; end;
if isempty(result2), result2=0; end;
if isempty(result3), result3=0; end;
%
%
[dumvar1, nfunc ]=funcset ( 'COUNT', nfunc );
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'TEST19');
writef(1,['%s \n'], '  For integrals inside the unit 3 sphere:');
writef(1,['%s \n'], '  SPHERE_UNIT_07_3D uses a formula of degree 7;');
writef(1,['%s \n'], '  SPHERE_UNIT_14_3D uses a formula of degree 14;');
writef(1,['%s \n'], '  SPHERE_UNIT_15_3D uses a formula of degree 15.');
writef(1,['%s \n'], ' ');
writef(1,['%s %0.15g \n'], '  Unit sphere volume = ', sphere_unit_volume_nd ( 3 ));
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], '  Rule:      #7             #14           #15');
writef(1,['%s \n'], '  F(X)');
writef(1,['%s \n'], ' ');

for i = 1: nfunc;
[dumvar1, i ]=funcset ( 'SET', i );
[dumvar1, result1 ]=sphere_unit_07_3d( @funcd3, result1 );
[dumvar1, result2 ]=sphere_unit_14_3d( @funcd3, result2 );
[dumvar1, result3 ]=sphere_unit_15_3d( @funcd3, result3 );
writef(1,['%7s',repmat('%14.8f',1,4),'\n'], fname(i), result1, result2, result3);
end; i = fix(nfunc+1);

return;
end
function test09(varargin)
%
%*******************************************************************************
%
%! TEST09 tests SPHERE_UNIT_F1_ND.
%! TEST09 tests SPHERE_UNIT_F3_ND.
%
persistent i max_n n nfunc result1 result2 ; 

if isempty(max_n), max_n = 3; end;
%
if isempty(i), i=0; end;
if isempty(n), n=0; end;
if isempty(nfunc), nfunc=0; end;
if isempty(result1), result1=0; end;
if isempty(result2), result2=0; end;
%
%
[dumvar1, nfunc ]=funcset ( 'COUNT', nfunc );
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'TEST09');
writef(1,['%s \n'], '  For integrals inside the unit sphere in ND:');
writef(1,['%s \n'], '  SPHERE_UNIT_F1_ND approximates the integral;');
writef(1,['%s \n'], '  SPHERE_UNIT_F3_ND approximates the integral.');
writef(1,['%s \n'], ' ');

for n = 2: max_n;

writef(1,['%s \n'], ' ');
writef(1,['%s %0.15g \n'], '  Spatial dimension N = ', n);
writef(1,['%s %0.15g \n'], '  Unit sphere volume = ', sphere_unit_volume_nd ( n ));
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], '  Rule:      F1          F3');
writef(1,['%s \n'], '  F(X)');
writef(1,['%s \n'], ' ');

for i = 1: nfunc;
[dumvar1, i ]=funcset ( 'SET', i );
[dumvar1, n, result1 ]=sphere_unit_f1_nd( @funcdn, n, result1 );
[dumvar1, n, result2 ]=sphere_unit_f3_nd( @funcdn, n, result2 );
writef(1,['%7s',repmat('%14.8f',1,2),'\n'], fname(i), result1, result2);

end; i = fix(nfunc+1);

end; n = fix(max_n+1);

return;
end
function test095(varargin)
%
%*******************************************************************************
%
%! TEST095 tests SPHERE_F1_ND.
%! TEST095 tests SPHERE_F3_ND.
%
persistent i max_n n nfunc r result1 result2 xc ; 

if isempty(max_n), max_n = 3; end;
%
if isempty(i), i=0; end;
if isempty(n), n=0; end;
if isempty(nfunc), nfunc=0; end;
if isempty(result1), result1=0; end;
if isempty(result2), result2=0; end;
if isempty(r), r=0; end;
if isempty(xc), xc=zeros(1,max_n); end;
%
%
[dumvar1, nfunc ]=funcset ( 'COUNT', nfunc );
r = 2.0d+00;
xc([1:max_n]) =[ 1.0d+00, -1.0d+00, 2.0d+00 ];
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'TEST095');
writef(1,['%s \n'], '  For integrals inside a sphere in ND:');
writef(1,['%s \n'], '  SPHERE_F1_ND approximates the integral;');
writef(1,['%s \n'], '  SPHERE_F3_ND approximates the integral.');
writef(1,['%s \n'], ' ');
for n = 2: max_n;
writef(1,['%s \n'], ' ');
writef(1,['%s %0.15g \n'], '  Spatial dimension N = ', n);
writef(1,['%s \n'], '  Sphere center:');
writef(1,[repmat('%10.4f',1,3),'\n'], xc([1:n]));
writef(1,['%s %0.15g \n'], '  Sphere radius = ', r);
writef(1,['%s %0.15g \n'], '  Sphere volume = ', sphere_volume_nd ( n, r ));
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], '  Rule:      F1          F3');
writef(1,['%s \n'], '  F(X)');
writef(1,['%s \n'], ' ');
for i = 1: nfunc;
[dumvar1, i ]=funcset ( 'SET', i );
[dumvar1, n, xc, r, result1 ]=sphere_f1_nd( @funcdn, n, xc, r, result1 );
[dumvar1, n, xc, r, result2 ]=sphere_f3_nd( @funcdn, n, xc, r, result2 );
writef(1,['%7s',repmat('%14.8f',1,2),'\n'], fname(i), result1, result2);
end; i = fix(nfunc+1);
end; n = fix(max_n+1);
return;
end
function test096(varargin)
%
%*******************************************************************************
%
%! TEST096 tests SPHERE_F1_ND.
%! TEST096 tests SPHERE_F3_ND.
%! TEST096 tests SPHERE_SHELL_03_ND.
%! TEST096 tests SPHERE_SHELL_07_ND.
%
persistent i j max_n n nfunc r1 r2 result1 result2 result3 result4 result5 result6 xc ; 

if isempty(max_n), max_n = 3; end;
%
if isempty(i), i=0; end;
if isempty(j), j=0; end;
if isempty(n), n=0; end;
if isempty(nfunc), nfunc=0; end;
if isempty(result1), result1=0; end;
if isempty(result2), result2=0; end;
if isempty(result3), result3=0; end;
if isempty(result4), result4=0; end;
if isempty(result5), result5=0; end;
if isempty(result6), result6=0; end;
if isempty(r1), r1=0; end;
if isempty(r2), r2=0; end;
if isempty(xc), xc=zeros(1,max_n); end;
%
%
[dumvar1, nfunc ]=funcset ( 'COUNT', nfunc );
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'TEST096');
writef(1,['%s \n'], '  For integrals inside a spherical shell in ND:');
writef(1,['%s \n'], '  SPHERE_SHELL_03_ND approximates the integral.');
writef(1,['%s \n'], '  SPHERE_SHELL_07_ND approximates the integral.');
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], '  We compare these results with those computed by');
writef(1,['%s \n'], '  from the difference of two spherical integrals.');
writef(1,['%s \n'], '  SPHERE_F1_ND approximates the integral;');
writef(1,['%s \n'], '  SPHERE_F3_ND approximates the integral.');
writef(1,['%s \n'], ' ');
for j = 1: 2;
if( j == 1 )
r1 = 0.0d+00;
r2 = 1.0d+00;
xc([1:max_n]) = 0.0d+00;
else;
r1 = 2.0d+00;
r2 = 3.0d+00;
xc([1:max_n]) =[ 1.0d+00, -1.0d+00, 2.0d+00 ];
end;
for n = 2: max_n;
writef(1,['%s \n'], ' ');
writef(1,['%s %0.15g \n'], '  Spatial dimension N = ', n);
writef(1,['%s \n'], '  Sphere center:');
writef(1,[repmat('%10.4f',1,3),'\n'], xc([1:n]));
writef(1,['%s %0.15g \n'], '  Inner sphere radius = ', r1);
writef(1,['%s %0.15g \n'], '  Outer sphere radius = ', r2);
writef(1,['%s %0.15g \n'], '  Spherical shell volume = ',sphere_shell_volume_nd ( n, r1, r2 ));
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], ['  Rule:      #3       #7    F1(R2)-F1(R1)  ','F3(R2)-F3(R1)']);
writef(1,['%s \n'], '  F(X)');
writef(1,['%s \n'], ' ');
for i = 1: nfunc;
[dumvar1, i ]=funcset ( 'SET', i );
[dumvar1, n, xc, r1, r2, result1 ]=sphere_shell_03_nd( @funcdn, n, xc, r1, r2, result1 );
[dumvar1, n, xc, r1, r2, result2 ]=sphere_shell_07_nd( @funcdn, n, xc, r1, r2, result2 );
[dumvar1, n, xc, r1, result3 ]=sphere_f1_nd( @funcdn, n, xc, r1, result3 );
[dumvar1, n, xc, r2, result4 ]=sphere_f1_nd( @funcdn, n, xc, r2, result4 );

[dumvar1, n, xc, r1, result5 ]=sphere_f3_nd( @funcdn, n, xc, r1, result5 );
[dumvar1, n, xc, r2, result6 ]=sphere_f3_nd( @funcdn, n, xc, r2, result6 );
writef(1,['%7s',repmat('%14.8f',1,4),'\n'], fname(i), result1, result2, result4-result3,result6-result5);
end; i = fix(nfunc+1);
end; n = fix(max_n+1);
end; j = fix(2+1);
return;
end
function test20(varargin)
%
%*******************************************************************************
%
%! TEST20 tests SPHERE_UNIT_AREA_ND.
%! TEST20 tests SPHERE_UNIT_VOLUME_ND.
%
persistent a n v ; 

if isempty(a), a=0; end;
if isempty(n), n=0; end;
if isempty(v), v=0; end;
%
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'TEST20');
writef(1,['%s \n'], '  For a unit sphere in N dimensions:');
writef(1,['%s \n'], '  SPHERE_UNIT_AREA_ND computes the area;');
writef(1,['%s \n'], '  SPHERE_UNIT_VOLUME_ND computes the volume.');
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], '  N    Area    Volume');
writef(1,['%s \n'], ' ');
for n = 2: 10;
[a , n ]=sphere_unit_area_nd( n );
[v , n ]=sphere_unit_volume_nd( n );
writef(1,['%3i',repmat('%14.8f',1,2),'\n'], n, a, v);
end; n = fix(10+1);
return;
end
function test21(varargin)
%
%*******************************************************************************
%
%! TEST21 tests SQUARE_UNIT_SET.
%! TEST21 tests RECTANGLE_SUB_2D.
%
persistent i j max_order n nfunc norder nsub result rule weight xtab xval ytab yval ; 

if isempty(max_order), max_order = 16; end;
%
if isempty(i), i=0; end;
if isempty(j), j=0; end;
if isempty(n), n=0; end;
if isempty(nfunc), nfunc=0; end;
if isempty(norder), norder=0; end;
if isempty(nsub), nsub=zeros(1,2); end;
if isempty(result), result=0; end;
if isempty(rule), rule=0; end;
if isempty(weight), weight=zeros(1,max_order); end;
if isempty(xtab), xtab=zeros(1,max_order); end;
if isempty(xval), xval=zeros(1,2); end;
if isempty(ytab), ytab=zeros(1,max_order); end;
if isempty(yval), yval=zeros(1,2); end;
%
%
[dumvar1, nfunc ]=funcset ( 'COUNT', nfunc );
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'TEST21');
writef(1,['%s \n'], '  SQUARE_UNIT_SET sets up a quadrature rule on a unit square.');
writef(1,['%s \n'], '  RECTANGLE_SUB_2D applies it to subrectangles of an');
writef(1,['%s \n'], '  arbitrary rectangle.');
writef(1,['%s \n'], ' ');
%
%  Set the location of the square.
%
xval(1) = 1.0d+00;
yval(1) = 2.0d+00;
xval(2) = 3.0d+00;
yval(2) = 3.0d+00;
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], '  The corners of the rectangle are:');
writef(1,['%s \n'], ' ');
writef(1,['%0.15g %0.15g \n'], xval(1), yval(1));
writef(1,['%0.15g %0.15g \n'], xval(2), yval(2));
%
%  Get the quadrature abscissas and weights for a unit square.
%
rule = 2;
[ rule, norder, xtab, ytab, weight ]=square_unit_set( rule, norder, xtab, ytab, weight );
writef(1,['%s \n'], ' ');
writef(1,['%s %0.15g \n'], '  Using unit square integration rule number ', rule);
%
%  Set the function.
%
for i = 1: nfunc;
[dumvar1, i ]=funcset ( 'SET', i );
%
%  Try an increasing number of subdivisions.
%
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], '  Function  Subdivisions  Integral');
writef(1,['%s \n'], ' ');
for j = 1: 5;
nsub(1) = fix(j);
nsub(2) = fix(2 .* j);
[dumvar1, xval, yval, nsub, norder, xtab, ytab,weight, result ]=rectangle_sub_2d( @funcd2, xval, yval, nsub, norder, xtab, ytab,weight, result );
writef(1,['%7s',repmat('%4i',1,2),'%14.8f','\n'], fname(i), nsub(1), nsub(2), result);
end; j = fix(5+1);
end; i = fix(nfunc+1);
return;
end
function test22(varargin)
%
%*******************************************************************************
%
%! TEST22 tests SQUARE_UNIT_SET.
%! TEST22 tests SQUARE_SUM.
%
persistent i ihi ilo max_order max_rule nfunc norder r result rule weight xc xtab yc ytab ; 

if isempty(max_order), max_order = 64; end;
if isempty(max_rule), max_rule = 6; end;
%
if isempty(i), i=0; end;
if isempty(ihi), ihi=0; end;
if isempty(ilo), ilo=0; end;
if isempty(nfunc), nfunc=0; end;
if isempty(norder), norder=0; end;
if isempty(r), r=0; end;
if isempty(result), result=zeros(1,max_rule); end;
if isempty(rule), rule=0; end;
if isempty(weight), weight=zeros(1,max_order); end;
if isempty(xc), xc=0; end;
if isempty(xtab), xtab=zeros(1,max_order); end;
if isempty(yc), yc=0; end;
if isempty(ytab), ytab=zeros(1,max_order); end;
%
%
[dumvar1, nfunc ]=funcset ( 'COUNT', nfunc );
xc = 2.0d+00;
yc = 2.0d+00;
r = 3.0d+00;
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'TEST22');
writef(1,['%s \n'], '  SQUARE_UNIT_SET sets up quadrature on the unit square;');
writef(1,['%s \n'], '  SQUARE_SUM carries it out on an arbitrary square.');
writef(1,['%s \n'], ' ');
writef(1,['%s %0.15g %0.15g \n'], '  Square center is ', xc, yc);
writef(1,['%s %0.15g \n'], '  Square radius is ', r);
for ilo = 1: 5: max_rule;
ihi = fix(min( ilo + 4, max_rule ));
writef(1,['%s \n'], ' ');
for  rule =( ilo):( ihi ), writef(1,['%s %0.15g \n'], 'Rule:    ',  rule); end;
writef(1,['%s \n'], 'Function ');
writef(1,['%s \n'], ' ');
for i = 1: nfunc;
[dumvar1, i ]=funcset ( 'SET', i );
for rule = ilo: ihi;
[ rule, norder, xtab, ytab, weight ]=square_unit_set( rule, norder, xtab, ytab, weight );
[dumvar1, xc, yc, r, norder, xtab, ytab, weight,result(rule) ]=square_sum( @funcd2, xc, yc, r, norder, xtab, ytab, weight,result(rule) );
end; rule = fix(ihi+1);
writef(1,['%7s',repmat('%14.8f',1,5),'\n'], fname(i), result([ilo:ihi]));
end; i = fix(nfunc+1);
end; ilo = fix(max_rule+1);

return;
end
function test23(varargin)
%
%*******************************************************************************
%
%! TEST23 tests SQUARE_UNIT_SET.
%! TEST23 tests SQUARE_UNIT_SUM.
%
persistent i ihi ilo max_order max_rule nfunc norder result rule weight xtab ytab ; 

if isempty(max_order), max_order = 64; end;
if isempty(max_rule), max_rule = 6; end;
%
if isempty(i), i=0; end;
if isempty(ihi), ihi=0; end;
if isempty(ilo), ilo=0; end;
if isempty(nfunc), nfunc=0; end;
if isempty(norder), norder=0; end;
if isempty(result), result=zeros(1,max_rule); end;
if isempty(rule), rule=0; end;
if isempty(weight), weight=zeros(1,max_order); end;
if isempty(xtab), xtab=zeros(1,max_order); end;
if isempty(ytab), ytab=zeros(1,max_order); end;
%
%
[dumvar1, nfunc ]=funcset ( 'COUNT', nfunc );
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'TEST23');
writef(1,['%s \n'], '  SQUARE_UNIT_SET sets up quadrature on the unit square;');
writef(1,['%s \n'], '  SQUARE_UNIT_SUM carries it out on the unit square.');
writef(1,['%s \n'], ' ');

for ilo = 1: 5: max_rule;
ihi = fix(min( ilo + 4, max_rule ));
writef(1,['%s \n'], ' ');
for  rule =( ilo):( ihi ), writef(1,['%s %0.15g \n'], 'Rule:    ',  rule); end;
writef(1,['%s \n'], 'Function ');
writef(1,['%s \n'], ' ');
for i = 1: nfunc;
[dumvar1, i ]=funcset ( 'SET', i );
for rule = ilo: ihi;
[ rule, norder, xtab, ytab, weight ]=square_unit_set( rule, norder, xtab, ytab, weight );
[dumvar1, norder, xtab, ytab, weight,result(rule) ]=square_unit_sum( @funcd2, norder, xtab, ytab, weight,result(rule) );
end; rule = fix(ihi+1);
writef(1,['%7s',repmat('%14.8f',1,5),'\n'], fname(i), result([ilo:ihi]));
end; i = fix(nfunc+1);
end; ilo = fix(max_rule+1);

return;
end
function test24(varargin)
%
%*******************************************************************************
%
%! TEST24 tests TETRA_07.
%! TEST24 tests TETRA_TPRODUCT.
%
persistent i max_order nfunc norder result2 result3 x y z ; 

if isempty(max_order), max_order = 9; end;
%
if isempty(i), i=0; end;
if isempty(nfunc), nfunc=0; end;
if isempty(norder), norder=0; end;
if isempty(result2), result2=0; end;
if isempty(result3), result3=zeros(1,max_order); end;
if isempty(x), x=zeros(1,4); end;
if isempty(y), y=zeros(1,4); end;
if isempty(z), z=zeros(1,4); end;
%
%
[dumvar1, nfunc ]=funcset ( 'COUNT', nfunc );
x([1:4]) =[ 1.0d+00, 4.0d+00, 1.0d+00, 1.0d+00 ];
y([1:4]) =[ 2.0d+00, 2.0d+00, 3.0d+00, 2.0d+00 ];
z([1:4]) =[ 6.0d+00, 6.0d+00, 6.0d+00, 8.0d+00 ];
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'TEST24');
writef(1,['%s \n'], '  For integrals inside an arbitrary tetrahedron:');
writef(1,['%s \n'], '  TETRA_07 uses a formula of degree 7;');
writef(1,['%s \n'], '  TETRA_TPRODUCT uses a triangular product formula ');
writef(1,['%s \n'], '    of varying degree.');
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], '  Tetrahedron vertices:');
writef(1,['%s \n'], ' ');
for i = 1: 4;
writef(1,[repmat('%4.0f',1,3),'\n'], x(i), y(i), z(i));
end; i = fix(4+1);
writef(1,['%s \n'], ' ');
writef(1,['%s %0.15g \n'], '  Tetrahedron unit volume = ', tetra_unit_volume ( ));
writef(1,['%s %0.15g \n'], '  Tetrahedron Volume = ', tetra_volume ( x, y, z ));
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], '  F(X)    TETRA_07');
writef(1,['%s \n'], '          TETRA_TPRODUCT(1:4)');
writef(1,['%s \n'], '          TETRA_TPRODUCT(5:8)');
writef(1,['%s \n'], '          TETRA_TPRODUCT(9)');
writef(1,['%s \n'], ' ');
for i = 1: nfunc;
[dumvar1, i ]=funcset ( 'SET', i );
[dumvar1, x, y, z, result2 ]=tetra_07( @funcd3, x, y, z, result2 );
for norder = 1: max_order;
[dumvar1, norder, x, y, z, result3(norder) ]=tetra_tproduct( @funcd3, norder, x, y, z, result3(norder) );
end; norder = fix(max_order+1);
writef(1,['%s \n'], ' ');
writef(1,['%7s',repmat('%16.10f',1,4),'\n'], fname(i), result2);
writef(1,[repmat(' ',1,7),repmat('%16.10f',1,4),'\n'],           result3([1:4]));
writef(1,[repmat(' ',1,7),repmat('%16.10f',1,4),'\n'],           result3([5:8]));
writef(1,[repmat(' ',1,7),repmat('%16.10f',1,4),'\n'],           result3(9));
end; i = fix(nfunc+1);
return;
end
function test25(varargin)
%
%*******************************************************************************
%
%! TEST25 tests TETRA_UNIT_SET.
%! TEST25 tests TETRA_UNIT_SUM.
%
persistent i ihi ilo max_order max_rule nfunc norder result rule weight xtab ytab ztab ; 

if isempty(max_order), max_order = 64; end;
if isempty(max_rule), max_rule = 7; end;
%
if isempty(i), i=0; end;
if isempty(ihi), ihi=0; end;
if isempty(ilo), ilo=0; end;
if isempty(nfunc), nfunc=0; end;
if isempty(norder), norder=0; end;
if isempty(result), result=zeros(1,max_rule); end;
if isempty(rule), rule=0; end;
if isempty(weight), weight=zeros(1,max_order); end;
if isempty(xtab), xtab=zeros(1,max_order); end;
if isempty(ytab), ytab=zeros(1,max_order); end;
if isempty(ztab), ztab=zeros(1,max_order); end;
%
%
[dumvar1, nfunc ]=funcset ( 'COUNT', nfunc );
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'TEST25');
writef(1,['%s \n'], '  TETRA_UNIT_SET sets quadrature rules for the unit tetrahedron;');
writef(1,['%s \n'], '  TETRA_UNIT_SUM applies them to the unit tetrahedron.');
writef(1,['%s \n'], ' ');
for ilo = 1: 5: max_rule;
ihi = fix(min( ilo +  4, max_rule ));
writef(1,['%s \n'], ' ');
for  rule =( ilo):( ihi ), writef(1,['%s %0.15g \n'], 'Rule:   ',  rule); end;
writef(1,['%s \n'], 'Function');
writef(1,['%s \n'], ' ');
for i = 1: nfunc;
[dumvar1, i ]=funcset ( 'SET', i );
for rule = ilo: ihi;
[ rule, norder, xtab, ytab, ztab, weight ]=tetra_unit_set( rule, norder, xtab, ytab, ztab, weight );

[dumvar1, norder, xtab, ytab, ztab, weight,result(rule) ]=tetra_unit_sum( @funcd3, norder, xtab, ytab, ztab, weight,result(rule) );
end; rule = fix(ihi+1);
writef(1,['%7s',repmat('%14.8f',1,5),'\n'], fname(i), result([ilo:ihi]));
end; i = fix(nfunc+1);
end; ilo = fix(max_rule+1);
return;
end
function test26(varargin)
%
%*******************************************************************************
%
%! TEST26 tests TETRA_UNIT_SET.
%! TEST26 tests TETRA_SUM.
%
persistent i ihi ilo max_order max_rule nfunc norder result rule weight x xtab y ytab z ztab ; 

if isempty(max_order), max_order = 64; end;
if isempty(max_rule), max_rule = 7; end;
%
if isempty(i), i=0; end;
if isempty(ihi), ihi=0; end;
if isempty(ilo), ilo=0; end;
if isempty(nfunc), nfunc=0; end;
if isempty(norder), norder=0; end;
if isempty(result), result=zeros(1,max_rule); end;
if isempty(rule), rule=0; end;
if isempty(weight), weight=zeros(1,max_order); end;
if isempty(x), x=zeros(1,4); end;
if isempty(xtab), xtab=zeros(1,max_order); end;
if isempty(y), y=zeros(1,4); end;
if isempty(ytab), ytab=zeros(1,max_order); end;
if isempty(z), z=zeros(1,4); end;
if isempty(ztab), ztab=zeros(1,max_order); end;
%
%
[dumvar1, nfunc ]=funcset ( 'COUNT', nfunc );
x([1:4]) =[ 1.0d+00, 4.0d+00, 1.0d+00, 1.0d+00 ];
y([1:4]) =[ 2.0d+00, 2.0d+00, 3.0d+00, 2.0d+00 ];
z([1:4]) =[ 6.0d+00, 6.0d+00, 6.0d+00, 8.0d+00 ];
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'TEST26');
writef(1,['%s \n'], '  TETRA_UNIT_SET sets quadrature rules for the unit tetrahedron;');
writef(1,['%s \n'], '  TETRA_SUM applies them to an arbitrary tetrahedron.');
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], '  Tetrahedron vertices:');
writef(1,['%s \n'], ' ');
for i = 1: 4;
writef(1,[repmat('%4.0f',1,3),'\n'], x(i), y(i), z(i));
end; i = fix(4+1);
for ilo = 1: 5: max_rule;
ihi = fix(min( ilo +  4, max_rule ));
writef(1,['%s \n'], ' ');
for  rule =( ilo):( ihi ), writef(1,['%s %0.15g \n'], 'Rule:    ',  rule); end;
writef(1,['%s \n'], 'Function ');
writef(1,['%s \n'], ' ');
for i = 1: nfunc;
[dumvar1, i ]=funcset ( 'SET', i );
for rule = ilo: ihi;
[ rule, norder, xtab, ytab, ztab, weight ]=tetra_unit_set( rule, norder, xtab, ytab, ztab, weight );

[dumvar1, x, y, z, norder, xtab, ytab, ztab, weight,result(rule) ]=tetra_sum( @funcd3, x, y, z, norder, xtab, ytab, ztab, weight,result(rule) );
end; rule = fix(ihi+1);
writef(1,['%7s',repmat('%14.7f',1,5),'\n'], fname(i), result([ilo:ihi]));
end; i = fix(nfunc+1);
end; ilo = fix(max_rule+1);
return;
end
function test27(varargin)
%
%*******************************************************************************
%
%! TEST27 tests TRIANGLE_UNIT_SET.
%! TEST27 tests TRIANGLE_SUB.
%
%  Break up the triangle into NSUB**2 equal subtriangles.  Approximate
%  the integral over the triangle by the sum of the integrals over each
%  subtriangle.
%
persistent i max_order n nfunc norder nsub result rule weight xtab xval ytab yval ; 

if isempty(max_order), max_order = 16; end;
%
if isempty(i), i=0; end;
if isempty(n), n=0; end;
if isempty(nfunc), nfunc=0; end;
if isempty(norder), norder=0; end;
if isempty(nsub), nsub=0; end;
if isempty(result), result=0; end;
if isempty(rule), rule=0; end;
if isempty(weight), weight=zeros(1,max_order); end;
if isempty(xtab), xtab=zeros(1,max_order); end;
if isempty(xval), xval=zeros(1,3); end;
if isempty(ytab), ytab=zeros(1,max_order); end;
if isempty(yval), yval=zeros(1,3); end;
%
%
[dumvar1, nfunc ]=funcset ( 'COUNT', nfunc );
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'TEST27');
writef(1,['%s \n'], '  TRIANGLE_UNIT_SET sets up a quadrature rule on a triangle.');
writef(1,['%s \n'], '  TRIANGLE_SUB applies it to subtriangles of an');
writef(1,['%s \n'], '  arbitrary triangle.');
writef(1,['%s \n'], ' ');
%
%  Set the location of the triangle.
%
xval(1) = 0.0d+00;
yval(1) = 0.0d+00;
xval(2) = 0.0d+00;
yval(2) = 1.0d+00;
xval(3) = 1.0d+00;
yval(3) = 0.0d+00;
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'Triangle vertices:');
writef(1,['%s \n'], ' ');
writef(1,['%0.15g %0.15g \n'], xval(1), yval(1));
writef(1,['%0.15g %0.15g \n'], xval(2), yval(2));
writef(1,['%0.15g %0.15g \n'], xval(3), yval(3));
%
%  Get the quadrature abscissas and weights for a unit triangle.
%
rule = 3;
[ rule, norder, xtab, ytab, weight ]=triangle_unit_set( rule, norder, xtab, ytab, weight );
writef(1,['%s \n'], ' ');
writef(1,['%s %0.15g \n'], '  Using unit triangle quadrature rule ', rule);
%
%  Set the function.
%
for i = 1: nfunc;
[dumvar1, i ]=funcset ( 'SET', i );
%
%  Try an increasing number of subdivisions.
%
for nsub = 1: 5;
[dumvar1, xval, yval, nsub, norder, xtab, ytab,weight, result ]=triangle_sub( @funcd2, xval, yval, nsub, norder, xtab, ytab,weight, result );
writef(1,['%7s','%4i','%14.8f','\n'], fname(i), nsub, result);

end; nsub = fix(5+1);

end; i = fix(nfunc+1);
return;
end
function test28(varargin)
%
%*******************************************************************************
%
%! TEST28 tests TRIANGLE_UNIT_SET.
%! TEST28 tests TRIANGLE_UNIT_SUM.
%
persistent i ihi ilo max_order max_rule nfunc norder result rule weight xtab ytab ; 

if isempty(max_order), max_order = 64; end;
if isempty(max_rule), max_rule = 19; end;
%
if isempty(i), i=0; end;
if isempty(ihi), ihi=0; end;
if isempty(ilo), ilo=0; end;
if isempty(nfunc), nfunc=0; end;
if isempty(norder), norder=0; end;
if isempty(result), result=zeros(1,max_rule); end;
if isempty(rule), rule=0; end;
if isempty(weight), weight=zeros(1,max_order); end;
if isempty(xtab), xtab=zeros(1,max_order); end;
if isempty(ytab), ytab=zeros(1,max_order); end;
%
%
[dumvar1, nfunc ]=funcset ( 'COUNT', nfunc );
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'TEST28');
writef(1,['%s \n'], '  TRIANGLE_UNIT_SET sets up quadrature in the unit triangle,');
writef(1,['%s \n'], '  TRIANGLE_UNIT_SUM applies it.');
writef(1,['%s \n'], ' ');
for ilo = 1: 5: max_rule;
ihi = fix(min( ilo +  4, max_rule ));
writef(1,['%s \n'], ' ');
for  rule =( ilo):( ihi ), writef(1,['%s %0.15g \n'], 'Rule:    ',  rule); end;
writef(1,['%s \n'], 'Function ');
writef(1,['%s \n'], ' ');
for i = 1: nfunc;
[dumvar1, i ]=funcset ( 'SET', i );
for rule = ilo: ihi;
[ rule, norder, xtab, ytab, weight ]=triangle_unit_set( rule, norder, xtab, ytab, weight );

[dumvar1, norder, xtab, ytab, weight,result(rule) ]=triangle_unit_sum( @funcd2, norder, xtab, ytab, weight,result(rule) );
end; rule = fix(ihi+1);
writef(1,['%7s',repmat('%14.8f',1,5),'\n'], fname(i), result([ilo:ihi]));
end; i = fix(nfunc+1);
end; ilo = fix(max_rule+1);
return;
end
function test285(varargin)
%
%*******************************************************************************
%
%! TEST285 tests TRIANGLE_UNIT_PRODUCT_SET.
%! TEST285 tests TRIANGLE_UNIT_SUM.
%
persistent i ihi ilo max_order max_rule nfunc norder result rule weight xtab ytab ; 

if isempty(max_rule), max_rule = 8; end;
if isempty(max_order), max_order = max_rule .* max_rule; end;
%
if isempty(i), i=0; end;
if isempty(ihi), ihi=0; end;
if isempty(ilo), ilo=0; end;
if isempty(nfunc), nfunc=0; end;
if isempty(norder), norder=0; end;
if isempty(result), result=zeros(1,max_rule); end;
if isempty(rule), rule=0; end;
if isempty(weight), weight=zeros(1,max_order); end;
if isempty(xtab), xtab=zeros(1,max_order); end;
if isempty(ytab), ytab=zeros(1,max_order); end;
%
%
[dumvar1, nfunc ]=funcset ( 'COUNT', nfunc );
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'TEST285');
writef(1,['%s \n'], '  TRIANGLE_UNIT_PRODUCT_SET sets up a product quadrature');
writef(1,['%s \n'], '    rule in the unit triangle,');
writef(1,['%s \n'], '  TRIANGLE_UNIT_SUM applies it.');
writef(1,['%s \n'], ' ');
for ilo = 1: 5: max_rule;
ihi = fix(min( ilo +  4, max_rule ));
writef(1,['%s \n'], ' ');
for  rule =( ilo):( ihi ), writef(1,['%s %0.15g \n'], 'Rule Order: ',  rule); end;
writef(1,['%s \n'], 'Function ');
writef(1,['%s \n'], ' ');
for i = 1: nfunc;
[dumvar1, i ]=funcset ( 'SET', i );
for rule = ilo: ihi;
[ rule, norder, xtab, ytab, weight ]=triangle_unit_product_set( rule, norder, xtab, ytab, weight );
[dumvar1, norder, xtab, ytab, weight,result(rule) ]=triangle_unit_sum( @funcd2, norder, xtab, ytab, weight,result(rule) );
end; rule = fix(ihi+1);
writef(1,['%7s',repmat('%14.8f',1,5),'\n'], fname(i), result([ilo:ihi]));
end; i = fix(nfunc+1);
end; ilo = fix(max_rule+1);
return;
end
function test29(varargin)
%
%*******************************************************************************
%
%! TEST29 tests TRIANGLE_UNIT_SET.
%! TEST29 tests TRIANGLE_SUM.
%
persistent i ihi ilo max_order max_rule nfunc norder result rule weight xtab xval ytab yval ; 

if isempty(max_order), max_order = 64; end;
if isempty(max_rule), max_rule = 19; end;
%
if isempty(i), i=0; end;
if isempty(ihi), ihi=0; end;
if isempty(ilo), ilo=0; end;
if isempty(nfunc), nfunc=0; end;
if isempty(norder), norder=0; end;
if isempty(result), result=zeros(1,max_rule); end;
if isempty(rule), rule=0; end;
if isempty(weight), weight=zeros(1,max_order); end;
if isempty(xtab), xtab=zeros(1,max_order); end;
if isempty(xval), xval=zeros(1,3); end;
if isempty(ytab), ytab=zeros(1,max_order); end;
if isempty(yval), yval=zeros(1,3); end;
%
%
[dumvar1, nfunc ]=funcset ( 'COUNT', nfunc );
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'TEST29');
writef(1,['%s \n'], '  TRIANGLE_UNIT_SET sets up quadrature in the unit triangle,');
writef(1,['%s \n'], '  TRIANGLE_SUM applies it to an arbitrary triangle.');
writef(1,['%s \n'], ' ');
xval(1) = 1.0d+00;
yval(1) = 1.0d+00;
xval(2) = 3.0d+00;
yval(2) = 1.0d+00;
xval(3) = 1.0d+00;
yval(3) = 4.0d+00;
for ilo = 1: 5: max_rule;
ihi = fix(min( ilo + 4, max_rule ));
writef(1,['%s \n'], ' ');
for  rule =( ilo):( ihi ), writef(1,['%s %0.15g \n'], 'Rule:    ',  rule); end;
writef(1,['%s \n'], 'Function ');
writef(1,['%s \n'], ' ');
for i = 1: nfunc;
[dumvar1, i ]=funcset ( 'SET', i );
for rule = ilo: ihi;
[ rule, norder, xtab, ytab, weight ]=triangle_unit_set( rule, norder, xtab, ytab, weight );

[dumvar1, xval, yval, norder, xtab, ytab, weight,result(rule) ]=triangle_sum( @funcd2, xval, yval, norder, xtab, ytab, weight,result(rule) );
end; rule = fix(ihi+1);
writef(1,['%7s',repmat('%14.8f',1,5),'\n'], fname(i), result([ilo:ihi]));
end; i = fix(nfunc+1);
end; ilo = fix(max_rule+1);
return;
end
function [lda, n, v]=setsim( lda, n, v );
%
%*******************************************************************************
%
%! SETSIM defines the simplex.
%
%
persistent i ; 

if isempty(i), i=0; end;
v_orig=v;v_shape=[lda,n];v=reshape([v_orig(1:min(prod(v_shape),numel(v_orig))),zeros(1,max(0,prod(v_shape)-numel(v_orig)))],v_shape);
%
v([1:n+1],[1:n]) = 0.0d+00;
for i = 1: n;
v(i,i) = 1.0d+00;
end; i = fix(n+1);

v_orig(1:prod(v_shape))=v;v=v_orig;
return;
end
function test30(varargin)
%
%*******************************************************************************
%
%! TEST30 tests TORUS_1.
%
persistent i j j2 n nfunc r1 r2 result ; 

if isempty(i), i=0; end;
if isempty(j), j=0; end;
if isempty(j2), j2=0; end;
if isempty(n), n=0; end;
if isempty(nfunc), nfunc=0; end;
if isempty(result), result=zeros(1,5); end;
if isempty(r1), r1=0; end;
if isempty(r2), r2=0; end;
%
%
[dumvar1, nfunc ]=funcset ( 'COUNT', nfunc );
r1 = 0.5d+00;
r2 = 1.0d+00;
n = 10;

writef(1,['%s \n'], ' ');
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'TEST30');
writef(1,['%s \n'], '  TORUS_1 approximates integrals on a torus.');
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], '  The degree N will be varied.');
writef(1,['%s \n'], ' ');
writef(1,['%s %0.15g \n'], '  Inner radius = ', r1);
writef(1,['%s %0.15g \n'], '  Outer radius = ', r2);
writef(1,['%s %0.15g \n'], '  Area = ', torus_area_3d ( r1, r2 ));
writef(1,['%s \n'], ' ');
for  j =( 0):( 2 ):( 8), writef(1,['%s %0.15g \n'], '  F(X)  ',  2.^j); end;
writef(1,['%s \n'], ' ');

for i = 1: nfunc;
[dumvar1, i ]=funcset ( 'SET', i );
for j = 1: 5;
j2 = fix(2 .*( j - 1 ));
n = fix(2.^j2);
[dumvar1, r1, r2, n, result(j) ]=torus_1( @funcd3, r1, r2, n, result(j) );
end; j = fix(5+1);
writef(1,['%7s',repmat('%14.8f',1,5),'\n'], fname(i), result([1:5]));
end; i = fix(nfunc+1);

return;
end
function test31(varargin)
%
%*******************************************************************************
%
%! TEST31 tests TORUS_5S2.
%! TEST31 tests TORUS_6S2.
%! TEST31 tests TORUS_14S.
%
persistent i nfunc r1 r2 result1 result2 result3 ; 

if isempty(i), i=0; end;
if isempty(nfunc), nfunc=0; end;
if isempty(result1), result1=0; end;
if isempty(result2), result2=0; end;
if isempty(result3), result3=0; end;
if isempty(r1), r1=0; end;
if isempty(r2), r2=0; end;
%
%
[dumvar1, nfunc ]=funcset ( 'COUNT', nfunc );
r1 = 0.5d+00;
r2 = 1.0d+00;

writef(1,['%s \n'], ' ');
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'TEST31');
writef(1,['%s \n'], '  For the interior of a torus,');
writef(1,['%s \n'], '  TORUS_5S2,');
writef(1,['%s \n'], '  TORUS_6S2, and');
writef(1,['%s \n'], '  TORUS_5S2 approximate integrals.');
writef(1,['%s \n'], ' ');
writef(1,['%s %0.15g \n'], '  Inner radius = ', r1);
writef(1,['%s %0.15g \n'], '  Outer radius = ', r2);
writef(1,['%s %0.15g \n'], '  Volume = ', torus_volume_3d ( r1, r2 ));
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], '  Rule:        #5S2          #6S2          #14S');
writef(1,['%s \n'], '  F(X)');
writef(1,['%s \n'], ' ');

for i = 1: nfunc;
[dumvar1, i ]=funcset ( 'SET', i );
[dumvar1, r1, r2, result1 ]=torus_5s2( @funcd3, r1, r2, result1 );
[dumvar1, r1, r2, result2 ]=torus_6s2( @funcd3, r1, r2, result2 );
[dumvar1, r1, r2, result3 ]=torus_14s( @funcd3, r1, r2, result3 );
writef(1,['%7s',repmat('%14.8f',1,3),'\n'], fname(i), result1, result2, result3);
end; i = fix(nfunc+1);

return;
end
function test32(varargin)
%
%*******************************************************************************
%
%! TEST32 tests TORUS_SQUARE_5C2.
%! TEST32 tests TORUS_SQUARE_14C.
%
persistent i nfunc r1 r2 result1 result2 ; 

if isempty(i), i=0; end;
if isempty(nfunc), nfunc=0; end;
if isempty(result1), result1=0; end;
if isempty(result2), result2=0; end;
if isempty(r1), r1=0; end;
if isempty(r2), r2=0; end;
%
%
[dumvar1, nfunc ]=funcset ( 'COUNT', nfunc );
r1 = 1.0d+00;
r2 = 0.125d+00;

writef(1,['%s \n'], ' ');
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'TEST32');
writef(1,['%s \n'], '  For integrals inside a torus with square cross-section:');
writef(1,['%s \n'], '  TORUS_SQUARE_5C2 approximates the integral;');
writef(1,['%s \n'], '  TORUS_SQUARE_14C approximates the integral.');
writef(1,['%s \n'], ' ');
writef(1,['%s %0.15g \n'], '  Inner radius = ', r1);
writef(1,['%s %0.15g \n'], '  Outer radius = ', r2);
writef(1,['%s %0.15g \n'], '  Volume = ', torus_square_volume_3d ( r1, r2 ));
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], '  F(X)    5C2           14C');
writef(1,['%s \n'], ' ');

for i = 1: nfunc;
[dumvar1, i ]=funcset ( 'SET', i );
[dumvar1, r1, r2, result1 ]=torus_square_5c2( @funcd3, r1, r2, result1 );
[dumvar1, r1, r2, result2 ]=torus_square_14c( @funcd3, r1, r2, result2 );
writef(1,['%7s',repmat('%14.8f',1,2),'\n'], fname(i), result1, result2);
end; i = fix(nfunc+1);

return;
end
function test33(varargin)
%
%*******************************************************************************
%
%! TEST33 tests TVEC_EVEN.
%! TEST33 tests TVEC_EVEN2.
%! TEST33 tests TVEC_EVEN3.
%! TEST33 tests TVEC_EVEN_BRACKET.
%! TEST33 tests TVEC_EVEN_BRACKET2.
%! TEST33 tests TVEC_EVEN_BRACKET3.
%
persistent i maxt nt t theta1 theta2 ; 

if isempty(maxt), maxt = 5; end;
%
if isempty(i), i=0; end;
if isempty(nt), nt=0; end;
if isempty(t), t=zeros(1,maxt); end;
if isempty(theta1), theta1=0; end;
if isempty(theta2), theta2=0; end;
%
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'TEST33');
writef(1,['%s \n'], '  For evenly spaced angles between 0 and 2*PI:');
writef(1,['%s \n'], '  TVEC_EVEN');
writef(1,['%s \n'], '  TVEC_EVEN2');
writef(1,['%s \n'], '  TVEC_EVEN3');
writef(1,['%s \n'], '  For evenly spaced angles between THETA1 and THETA2:');
writef(1,['%s \n'], '  TVEC_EVEN_BRACKET');
writef(1,['%s \n'], '  TVEC_EVEN_BRACKET2.');
writef(1,['%s \n'], '  TVEC_EVEN_BRACKET3.');
writef(1,['%s \n'], ' ');
nt = 4;
[ nt, t ]=tvec_even( nt, t );
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], '  TVEC_EVEN');
writef(1,['%s %0.15g \n'], '    NT = ', nt);
for i = 1: nt;
writef(1,['%0.15g \n'], t(i));
end; i = fix(nt+1);
nt = 4;
[ nt, t ]=tvec_even2( nt, t );
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], '  TVEC_EVEN2');
writef(1,['%s %0.15g \n'], '    NT = ', nt);
for i = 1: nt;
writef(1,['%0.15g \n'], t(i));
end; i = fix(nt+1);
nt = 4;
[ nt, t ]=tvec_even3( nt, t );
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], '  TVEC_EVEN3');
writef(1,['%s %0.15g \n'], '    NT = ', nt);
for i = 1: nt;
writef(1,['%0.15g \n'], t(i));
end; i = fix(nt+1);
nt = 4;
theta1 = 30.0d+00;
theta2 = 90.0d+00;
[ theta1, theta2, nt, t ]=tvec_even_bracket( theta1, theta2, nt, t );
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], '  TVEC_EVEN_BRACKET');
writef(1,['%s %0.15g \n'], '    NT = ', nt);
writef(1,['%s %0.15g \n'], '    THETA1 = ', theta1);
writef(1,['%s %0.15g \n'], '    THETA2 = ', theta2);
for i = 1: nt;
writef(1,['%0.15g \n'], t(i));
end; i = fix(nt+1);
nt = 5;
[ theta1, theta2, nt, t ]=tvec_even_bracket2( theta1, theta2, nt, t );
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], '  TVEC_EVEN_BRACKET2');
writef(1,['%s %0.15g \n'], '    NT = ', nt);
writef(1,['%s %0.15g \n'], '    THETA1 = ', theta1);
writef(1,['%s %0.15g \n'], '    THETA2 = ', theta2);
for i = 1: nt;
writef(1,['%0.15g \n'], t(i));
end; i = fix(nt+1);
nt = 3;
[ theta1, theta2, nt, t ]=tvec_even_bracket3( theta1, theta2, nt, t );
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], '  TVEC_EVEN_BRACKET3');
writef(1,['%s %0.15g \n'], '    NT = ', nt);
writef(1,['%s %0.15g \n'], '    THETA1 = ', theta1);
writef(1,['%s %0.15g \n'], '    THETA2 = ', theta2);
for i = 1: nt;
writef(1,['%0.15g \n'], t(i));
end; i = fix(nt+1);
return;
end
function [funcd1result, x ]=funcd1( x );
%
%*******************************************************************************
%
%! FUNCD1 evaluates a function F(X) of one variable.
%
%
%  Discussion:
%
%    The actual form of the function can be determined by calling FUNCSET.
%
%  Modified:
%
%    21 January 2001
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, doubleprecision X, the value of the variable.
%
%    Output, doubleprecision FUNCD1, the value of the function.
%
funcd1result=[];
persistent funcd1 ifunc ; 

if isempty(funcd1result), funcd1result=0; end;
if isempty(ifunc), ifunc=0; end;
%
[dumvar1, ifunc ]=funcset ( 'GET', ifunc );
if( ifunc == 1 )
funcd1result = 1.0d+00;
elseif( ifunc == 2 ) ;
funcd1result = x;
elseif( ifunc == 3 ) ;
funcd1result = x.^2;
elseif( ifunc == 4 ) ;
funcd1result = x.^3;
elseif( ifunc == 5 ) ;
funcd1result = x.^4;
elseif( ifunc == 6 ) ;
funcd1result = x.^5;
elseif( ifunc == 7 ) ;
funcd1result = x.^6;
elseif( ifunc == 8 ) ;
funcd1result = abs( x );
elseif( ifunc == 9 ) ;
funcd1result = sin( x );
elseif( ifunc == 10 ) ;
funcd1result = exp( x );
elseif( ifunc == 11 ) ;
funcd1result = 1.0d+00 ./( 1.0d+00 + abs( x ) );
elseif( ifunc == 12 ) ;
funcd1result = sqrt( abs( x ) );
else;
funcd1result = 0.0d+00;
end;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',x); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [funcd2result, x, y ]=funcd2( x, y );
%
%*******************************************************************************
%
%! FUNCD2 evaluates a function F(X,Y) of two variables.
%
%
%  Discussion:
%
%    The actual form of the function can be determined by calling FUNCSET.
%
%  Modified:
%
%    21 January 2001
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, doubleprecision X, Y, the value of the variables.
%
%    Output, doubleprecision FUNCD2, the value of the function.
%
funcd2result=[];
persistent funcd2 ifunc ; 

if isempty(funcd2result), funcd2result=0; end;
if isempty(ifunc), ifunc=0; end;
%
[dumvar1, ifunc ]=funcset ( 'GET', ifunc );
if( ifunc == 1 )
funcd2result = 1.0d+00;
elseif( ifunc == 2 ) ;
funcd2result = x;
elseif( ifunc == 3 ) ;
funcd2result = x.^2;
elseif( ifunc == 4 ) ;
funcd2result = x.^3;
elseif( ifunc == 5 ) ;
funcd2result = x.^4;
elseif( ifunc == 6 ) ;
funcd2result = x.^5;
elseif( ifunc == 7 ) ;
funcd2result = x.^6;
elseif( ifunc == 8 ) ;
funcd2result = sqrt( x.^2 + y.^2 );
elseif( ifunc == 9 ) ;
funcd2result = sin( x );
elseif( ifunc == 10 ) ;
funcd2result = exp( x );
elseif( ifunc == 11 ) ;
funcd2result = 1.0d+00 ./( 1.0d+00 + sqrt( 1.0d+00 + x.^2 + y.^2 ) );
elseif( ifunc == 12 ) ;
funcd2result = sqrt( sqrt( x.^2 + y.^2 ) );
else;
funcd2result = 0.0d+00;
end;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(2)), assignin('caller','FUntemp',y); evalin('caller',[inputname(2),'=FUntemp;']); end
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',x); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [funcd3result, x, y, z ]=funcd3( x, y, z );
%
%*******************************************************************************
%
%! FUNCD3 evaluates a function F(X,Y,Z) of 3 variables.
%
%
%  Discussion:
%
%    The actual form of the function can be determined by calling FUNCSET.
%
%  Modified:
%
%    21 January 2001
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, doubleprecision X, Y, Z, the value of the variables.
%
%    Output, doubleprecision FUNCD3, the value of the function.
%
funcd3result=[];
persistent funcd3 ifunc ; 

if isempty(funcd3result), funcd3result=0; end;
if isempty(ifunc), ifunc=0; end;
%
[dumvar1, ifunc ]=funcset ( 'GET', ifunc );
if( ifunc == 1 )
funcd3result = 1.0d+00;
elseif( ifunc == 2 ) ;
funcd3result = x;
elseif( ifunc == 3 ) ;
funcd3result = x.^2;
elseif( ifunc == 4 ) ;
funcd3result = x.^3;
elseif( ifunc == 5 ) ;
funcd3result = x.^4;
elseif( ifunc == 6 ) ;
funcd3result = x.^5;
elseif( ifunc == 7 ) ;
funcd3result = x.^6;
elseif( ifunc == 8 ) ;
funcd3result = sqrt( x.^2 + y.^2 + z.^2 );
elseif( ifunc == 9 ) ;
funcd3result = sin( x );
elseif( ifunc == 10 ) ;
funcd3result = exp( x );
elseif( ifunc == 11 ) ;
funcd3result = 1.0d+00 ./ sqrt( 1.0d+00 + x.^2 + y.^2 + z.^2 );
elseif( ifunc == 12 ) ;
funcd3result = sqrt( sqrt( x.^2 + y.^2 + z.^2 ) );
else;
funcd3result = 0.0d+00;
end;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(3)), assignin('caller','FUntemp',z); evalin('caller',[inputname(3),'=FUntemp;']); end
if csnil&&~isempty(inputname(2)), assignin('caller','FUntemp',y); evalin('caller',[inputname(2),'=FUntemp;']); end
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',x); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [funcdnresult, n, x ]=funcdn( n, x );
%
%*******************************************************************************
%
%! FUNCDN evaluates a function of N variables.
%
%
%  Discussion:
%
%    The actual form of the function can be determined by calling FUNCSET.
%
%  Modified:
%
%    21 January 2001
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, integer N, the number of variables.
%
%    Input, doubleprecision X(N), the value of the variables.
%
%    Output, doubleprecision FUNCDN, the value of the function.
%
%
funcdnresult=[];
persistent funcdn i ifunc temp ; 

if isempty(funcdnresult), funcdnresult=0; end;
if isempty(i), i=0; end;
if isempty(ifunc), ifunc=0; end;
if isempty(temp), temp=0; end;
%
[dumvar1, ifunc ]=funcset ( 'GET', ifunc );
if( ifunc == 1 )
funcdnresult = 1.0d+00;
elseif( ifunc == 2 ) ;
funcdnresult = x(1);
elseif( ifunc == 3 ) ;
funcdnresult = x(1).^2;
elseif( ifunc == 4 ) ;
funcdnresult = x(1).^3;
elseif( ifunc == 5 ) ;
funcdnresult = x(1).^4;
elseif( ifunc == 6 ) ;
funcdnresult = x(1).^5;
elseif( ifunc == 7 ) ;
funcdnresult = x(1).^6;
elseif( ifunc == 8 ) ;
funcdnresult = sqrt( sum(sum( x([1:n]).^2 )) );
elseif( ifunc == 9 ) ;
funcdnresult = sin( x(1) );
elseif( ifunc == 10 ) ;
funcdnresult = exp( x(1) );
elseif( ifunc == 11 ) ;
funcdnresult = 1.0d+00 ./( 1.0d+00 + sqrt( sum(sum( x([1:n]).^2 )) ) );
elseif( ifunc == 12 ) ;
funcdnresult = sqrt( sqrt( sum(sum( x([1:n]).^2 )) ) );
else;
funcdnresult = 0.0d+00;
end;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(2)), assignin('caller','FUntemp',x); evalin('caller',[inputname(2),'=FUntemp;']); end
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',n); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [action, i]=funcset( action, i );
%
%*******************************************************************************
%
%! FUNCSET sets or reports the index of the current function.
%
%
%  Modified:
%
%    21 January 2001
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, character ( len = * ) ACTION:
%    'COUNT', please return the number of functions.
%    'GET', please return the index of the current function.
%    'SET', please set the function according to the input index.
%
%    Input/output, integer I.
%    If ACTION = 'SET', then I is input, and is the index of the desired
%    function.
%    If ACTION = 'COUNT', then I is output, and is the number of functions
%    available.
%    If ACTION = 'GET', then I is output, and is the index of the current
%    function.
%
persistent ival ; 

if isempty(ival), ival = 0; end;
%
if( strcmp(deblank(action),deblank('SET')) )
ival = fix(i);
elseif( strcmp(deblank(action),deblank('GET')) ) ;
i = fix(ival);
elseif( strcmp(deblank(action),deblank('COUNT')) ) ;
i = 12;
end;
return;
end
function [fnameresult, ifunc ]=fname( ifunc );
%
%*******************************************************************************
%
%! FNAME returns the name of the current function.
%
%
%  Modified:
%
%    21 January 2001
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, integer IFUNC, the index of the function.
%
%    Output, character ( len = * ) FNAME, the name of the function.
%
fnameresult=[];
persistent fname ; 

if isempty(fnameresult), fnameresult=''; end;
%
if( ifunc == 1 )
fnameresult = '      1';
elseif( ifunc == 2 ) ;
fnameresult = '      X';
elseif( ifunc == 3 ) ;
fnameresult = '   X**2';
elseif( ifunc == 4 ) ;
fnameresult = '   X**3';
elseif( ifunc == 5 ) ;
fnameresult = '   X**4';
elseif( ifunc == 6 ) ;
fnameresult = '   X**5';
elseif( ifunc == 7 ) ;
fnameresult = '   X**6';
elseif( ifunc == 8 ) ;
fnameresult = '      R';
elseif( ifunc == 9 ) ;
fnameresult = ' SIN(X)';
elseif( ifunc == 10 ) ;
fnameresult = ' EXP(X)';
elseif( ifunc == 11 ) ;
fnameresult = '1/(1+R)';
elseif( ifunc == 12 ) ;
fnameresult = 'SQRT(R)';
else;
fnameresult = '???????';
end;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',ifunc); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [fup7result, x ]=fup7( x );
%
%*******************************************************************************
%
%! FUP7 is the upper limit function for QMULT_2D.
%
fup7result=[];
persistent fup7 ; 

if isempty(fup7result), fup7result=0; end;
%
fup7result = 1.0d+00;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',x); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [flo7result, x ]=flo7( x );
%
%*******************************************************************************
%
%! FLO7 is the lower limit function for QMULT_2D.
%
flo7result=[];
persistent flo7 ; 

if isempty(flo7result), flo7result=0; end;
%
flo7result = -1.0d+00;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',x); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [fu18result, x ]=fu18( x );
%
%*******************************************************************************
%
%! FU18 is the upper limit function for problem 18.
%
fu18result=[];
persistent fu18 ; 

if isempty(fu18result), fu18result=0; end;
%
fu18result = 1.0d+00;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',x); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [fl18result, x ]=fl18( x );
%
%*******************************************************************************
%
%! FL18 is the lower limit function for problem 18.
%
fl18result=[];
persistent fl18 ; 

if isempty(fl18result), fl18result=0; end;
%
fl18result = -1.0d+00;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',x); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [fu28result, x, y ]=fu28( x, y );
%
%*******************************************************************************
%
%! FU28...
%
fu28result=[];
persistent fu28 ; 

if isempty(fu28result), fu28result=0; end;
%
fu28result = 1.0d+00;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(2)), assignin('caller','FUntemp',y); evalin('caller',[inputname(2),'=FUntemp;']); end
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',x); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [fl28result, x, y ]=fl28( x, y );
%
%*******************************************************************************
%
%! FL28...
%
fl28result=[];
persistent fl28 ; 

if isempty(fl28result), fl28result=0; end;
%
fl28result = -1.0d+00;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(2)), assignin('caller','FUntemp',y); evalin('caller',[inputname(2),'=FUntemp;']); end
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',x); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [func, xc, yc, radius1, radius2, nr, result]=circle_annulus( func, xc, yc, radius1, radius2, nr, result );
%
%*******************************************************************************
%
%! CIRCLE_ANNULUS approximates an integral in an annulus.
%
%
%  Discussion:
%
%    An annulus is bounded by two concentric circles.
%
%  Modified:
%
%    17 January 2001
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    William Peirce,
%    Numerical Integration Over the Planar Annulus,
%    Journal of the Society for Industrial and Applied Mathematics,
%    Volume 5, Issue 2, June 1957, pages 66-73.
%
%  Parameters:
%
%    Input, external FUNC, the name of the user supplied function of two
%    variables which is to be integrated, of the form:
%
%      function func ( x, y )
%      doubleprecision func
%      doubleprecision x
%      doubleprecision y
%
%    Input, doubleprecision XC, YC, the center of the circle.
%
%    Input, doubleprecision RADIUS1, RADIUS2, the radii of the circles.
%
%    Input, integer NR, the order of the rule.  This quantity specifies
%    the number of distinct radii to use.  The number of angles used will
%    be 4*NR, for a total of 4*NR**2 points.
%
%    Output, doubleprecision RESULT, the approximation to the integral.
%
%
persistent a area b c d i j nt quad r ra rw t tw x y ; 

if isempty(a), a=0; end;
if isempty(area), area=0; end;
if isempty(b), b=0; end;
if isempty(c), c=0; end;
if isempty(d), d=0; end;
if isempty(i), i=0; end;
if isempty(j), j=0; end;
if isempty(nt), nt=0; end;
if isempty(quad), quad=0; end;
if isempty(r), r=0; end;
if isempty(ra), ra=zeros(1,nr); end;
if isempty(rw), rw=zeros(1,nr); end;
if isempty(t), t=0; end;
if isempty(tw), tw=0; end;
if isempty(x), x=0; end;
if isempty(y), y=0; end;
%
%
%  Choose radial abscissas and weights.
%
[ nr, ra, rw ]=legendre_set( nr, ra, rw );
a = -1.0d+00;
b = +1.0d+00;
c = radius1.^2;
d = radius2.^2;
[ a, b, c, d, nr, ra, rw ]=rule_adjust( a, b, c, d, nr, ra, rw );
ra([1:nr]) = sqrt( ra([1:nr]) );
rw([1:nr]) = rw([1:nr]) ./( radius2.^2 - radius1.^2 );
%!!  print *,'radius1=',radius1
%!!  print *,'radius2=',radius2
%!!  print *,'rw=',rw
%!!  print *,'nr=',nr
%!!  stop
%
%  Set angular abscissas and weights.
%
nt = fix(4 .* nr);
tw = 1.0d+00 ./ ( nt );
%
%  Approximate the integral.
%
quad = 0.0d+00;
for i = 1: nt;
t = fix(((2.0d+00 .* pi()) .* ( i - 1 )) ./ ( nt ));
for j = 1: nr;
x = xc + ra(j) .* cos( t );
y = yc + ra(j) .* sin( t );
quad = quad + tw .* rw(j) .* func( x, y );
end; j = fix(nr+1);
end; i = fix(nt+1);
[area , radius1, radius2 ]=circle_annulus_area_2d( radius1, radius2 );
result = quad .* area;
return;
end
function [circle_annulus_area_2dresult, radius1, radius2 ]=circle_annulus_area_2d( radius1, radius2 );
%
%*******************************************************************************
%
%! CIRCLE_ANNULUS_AREA_2D returns the area of a circular annulus in 2D.
%
%
%  Discussion:
%
%    An annulus comprises the area between two concentric circles.
%
%  Modified:
%
%    14 January 2001
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, doubleprecision RADIUS1, RADIUS2, the radii of the circles.
%
%    Output, doubleprecision CIRCLE_ANNULUS_AREA_2D, the area of the annulus.
%
circle_annulus_area_2dresult=[];
persistent circle_annulus_area_2d ; 

if isempty(circle_annulus_area_2dresult), circle_annulus_area_2dresult=0; end;
%
circle_annulus_area_2dresult = pi() .*( radius1 + radius2 ) .*( radius2 - radius1 );
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(2)), assignin('caller','FUntemp',radius2); evalin('caller',[inputname(2),'=FUntemp;']); end
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',radius1); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [func, xc, yc, radius1, radius2, theta1,theta2, nr, result]=circle_annulus_sector( func, xc, yc, radius1, radius2, theta1,theta2, nr, result );
%
%*******************************************************************************
%
%! CIRCLE_ANNULUS_SECTOR approximates an integral in a circular annulus sector.
%
%
%  Discussion:
%
%    A circular annulus sector comprises the area between two concentric
%    circles and two concentric rays.
%
%  Modified:
%
%    19 January 2001
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    William Peirce,
%    Numerical Integration Over the Planar Annulus,
%    Journal of the Society for Industrial and Applied Mathematics,
%    Volume 5, Issue 2, June 1957, pages 66-73.
%
%  Parameters:
%
%    Input, external FUNC, the name of the user supplied function of two
%    variables which is to be integrated, of the form:
%
%      function func ( x, y )
%      doubleprecision func
%      doubleprecision x
%      doubleprecision y
%
%    Input, doubleprecision XC, YC, the center of the circle.
%
%    Input, doubleprecision RADIUS1, RADIUS2, the radii of the circles.
%
%    Input, doubleprecision THETA1, THETA2, the angles defining the sector.
%    The sector is measured from THETA1 to THETA2.
%
%    Input, integer NR, the order of the rule.  This quantity specifies
%    the number of distinct radii to use.  The number of angles used will
%    be 4*NR, for a total of 4*NR**2 points.
%
%    Output, doubleprecision RESULT, the approximation to the integral.
%
%
persistent a area b c d i j nt pi quad r ra rw ta tw x y ; 

if isempty(a), a=0; end;
if isempty(area), area=0; end;
if isempty(b), b=0; end;
if isempty(c), c=0; end;
if isempty(d), d=0; end;
if isempty(i), i=0; end;
if isempty(j), j=0; end;
if isempty(nt), nt=0; end;
if isempty(pi), pi=0; end;
if isempty(quad), quad=0; end;
if isempty(r), r=0; end;
if isempty(ra), ra=zeros(1,nr); end;
if isempty(rw), rw=zeros(1,nr); end;
if isempty(ta), ta=zeros(1,4.*nr); end;
if isempty(tw), tw=zeros(1,4.*nr); end;
if isempty(x), x=0; end;
if isempty(y), y=0; end;
%
%
%  Set the radial abscissas and weights.
%
[ nr, ra, rw ]=legendre_set( nr, ra, rw );
a = -1.0d+00;
b = +1.0d+00;
c = radius1.^2;
d = radius2.^2;
[ a, b, c, d, nr, ra, rw ]=rule_adjust( a, b, c, d, nr, ra, rw );
ra([1:nr]) = sqrt( ra([1:nr]) );
rw([1:nr]) = rw([1:nr]) ./( radius2.^2 - radius1.^2 );
%
%  Pick angles evenly spaced between THETA1 and THETA2, but do not
%  include the endpoints, and use a half interval for the first and last.
%
nt = fix(4 .* nr);
[ theta1, theta2, nt, ta ]=tvec_even_bracket3( theta1, theta2, nt, ta );
tw([1:nt]) = 1.0d+00 ./ ( nt );
%
%  Approximate the integral.
%
quad = 0.0d+00;
for i = 1: nt;
for j = 1: nr;
x = xc + ra(j) .* cos( ta(i) );
y = yc + ra(j) .* sin( ta(i) );
quad = quad + tw(i) .* rw(j) .* func( x, y );
end; j = fix(nr+1);
end; i = fix(nt+1);
[area , radius1, radius2, theta1, theta2 ]=circle_annulus_sector_area_2d( radius1, radius2, theta1, theta2 );
result = quad .* area;
return;
end
function [circle_annulus_sector_area_2dresult, radius1, radius2, theta1, theta2 ]=circle_annulus_sector_area_2d( radius1, radius2, theta1, theta2 );
%
%*******************************************************************************
%
%! CIRCLE_ANNULUS_SECTOR_AREA_2D returns the area of a circular annulus sector in 2D.
%
%
%  Discussion:
%
%    A circular annulus sector comprises the area between two concentric
%    circles and two concentric rays.
%
%  Modified:
%
%    19 January 2001
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, doubleprecision RADIUS1, RADIUS2, the radii of the circles.
%
%    Input, doubleprecision THETA1, THETA2, the angles of the rays.
%    Ordinarily, (THETA2-THETA1) is between 0 and 2*PI.
%
%    Output, doubleprecision CIRCLE_ANNULUS_SECTOR_AREA_2D, the area of the
%    circulare annulus sector.
%
circle_annulus_sector_area_2dresult=[];
persistent circle_annulus_sector_area_2d ; 

if isempty(circle_annulus_sector_area_2dresult), circle_annulus_sector_area_2dresult=0; end;
%
circle_annulus_sector_area_2dresult = 0.5 .*( radius1 + radius2 ).*( radius2 - radius1 ) .*( theta2 - theta1 );
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(4)), assignin('caller','FUntemp',theta2); evalin('caller',[inputname(4),'=FUntemp;']); end
if csnil&&~isempty(inputname(3)), assignin('caller','FUntemp',theta1); evalin('caller',[inputname(3),'=FUntemp;']); end
if csnil&&~isempty(inputname(2)), assignin('caller','FUntemp',radius2); evalin('caller',[inputname(2),'=FUntemp;']); end
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',radius1); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [circle_area_2dresult, r ]=circle_area_2d( r );
%
%*******************************************************************************
%
%! CIRCLE_AREA_2D returns the area of a circle in 2D.
%
%
%  Modified:
%
%    12 December 2000
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, doubleprecision R, the radius of the circle.
%
%    Output, doubleprecision CIRCLE_AREA_2D, the area of the circle.
%
circle_area_2dresult=[];
persistent circle_area_2d ; 

if isempty(circle_area_2dresult), circle_area_2dresult=0; end;
%
circle_area_2dresult = pi( ) .* r .* r;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',r); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [func, xc, yc, radius, norder, result]=circle_cum( func, xc, yc, radius, norder, result );
%
%*******************************************************************************
%
%! CIRCLE_CUM approximates an integral on the circumference of a circle in 2D.
%
%
%  Discussion:
%
%    An NORDER point, (NORDER-1)-th degree formula is used, Stroud number U2:M-1.
%
%  Integration region:
%
%    Points (X,Y) such that:
%
%      (X-XC)**2 + (Y-YC)**2 = RADIUS**2.
%
%  Reference:
%
%    Arthur H Stroud,
%    Approximate Calculation of Multiple Integrals,
%    Prentice Hall, 1971.
%
%  Modified:
%
%    07 September 1998
%
%  Parameters:
%
%    Input, external FUNC, the name of the user supplied function of two
%    variables which is to be integrated, of the form:
%
%      function func ( x, y )
%      doubleprecision func
%      doubleprecision x
%      doubleprecision y
%
%    Input, doubleprecision XC, YC, the coordinates of the center of the circle.
%
%    Input, doubleprecision RADIUS, the radius of the circle.
%
%    Input, integer NORDER, the number of points to use.
%
%    Output, doubleprecision RESULT, the approximate integral of the function.
%
persistent angle i quad volume x y ; 

if isempty(angle), angle=0; end;
if isempty(i), i=0; end;
if isempty(quad), quad=0; end;
if isempty(volume), volume=0; end;
if isempty(x), x=0; end;
if isempty(y), y=0; end;
%
%
quad = 0.0d+00;
for i = 1: norder;
angle = ( 2 .* i ) .* pi( ) ./ ( norder );
x = xc + radius .* cos( angle );
y = yc + radius .* sin( angle );
quad = quad + func( x, y );
end; i = fix(norder+1);
quad = quad ./ ( norder );
[volume ,dumvar2, radius ]=sphere_area_nd( 2, radius );
result = quad .* volume;
return;
end
function [circle_lune_area_2dresult, r, theta1, theta2 ]=circle_lune_area_2d( r, theta1, theta2 );
%
%*******************************************************************************
%
%! CIRCLE_LUNE_AREA_2D returns the area of a circular lune in 2D.
%
%
%  Discussion:
%
%    A lune is formed by drawing a circular arc, and joining its endpoints.
%
%  Modified:
%
%    13 January 2001
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, doubleprecision R, the radius of the circle.
%
%    Input, doubleprecision THETA1, THETA2, the angles of the rays
%    that begin and end the arc.
%
%    Output, doubleprecision CIRCLE_LUNE_AREA_2D, the area of the lune.
%
circle_lune_area_2dresult=[];
persistent circle_lune_area_2d sector triangle ; 

if isempty(circle_lune_area_2dresult), circle_lune_area_2dresult=0; end;
if isempty(sector), sector=0; end;
if isempty(triangle), triangle=0; end;
%
[sector , r, theta1, theta2 ]=circle_sector_area_2d( r, theta1, theta2 );
[triangle , r, theta1, theta2 ]=circle_triangle_area_2d( r, theta1, theta2 );
circle_lune_area_2dresult = sector - triangle;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(3)), assignin('caller','FUntemp',theta2); evalin('caller',[inputname(3),'=FUntemp;']); end
if csnil&&~isempty(inputname(2)), assignin('caller','FUntemp',theta1); evalin('caller',[inputname(2),'=FUntemp;']); end
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',r); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [circle_lune_h_area_2dresult, r, h ]=circle_lune_h_area_2d( r, h );
%
%*******************************************************************************
%
%! CIRCLE_LUNE_H_AREA_2D returns the area of a circular lune in 2D.
%
%
%  Discussion:
%
%    A lune is formed by drawing a circular arc, and joining its endpoints.
%    This lune is described by the 'height' of the region.  In other words,
%    the lune is the area that would be submerged if a circle of radius
%    R were standing in water of depth H.
%
%  Modified:
%
%    15 February 2001
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, doubleprecision R, the radius of the circle.
%
%    Input, doubleprecision H, the height of the lune region.
%
%    Output, doubleprecision CIRCLE_LUNE_H_AREA_2D, the area of the lune.
%
circle_lune_h_area_2dresult=[];
persistent angle area circle_lune_h_area_2d half_width sector triangle ; 

if isempty(angle), angle=0; end;
if isempty(area), area=0; end;
if isempty(circle_lune_h_area_2dresult), circle_lune_h_area_2dresult=0; end;
if isempty(half_width), half_width=0; end;
if isempty(sector), sector=0; end;
if isempty(triangle), triangle=0; end;
%
if( h <= 0.0d+00 )
area = 0.0d+00;
elseif( h >= 2.0d+00 .* r ) ;
area = pi() .* r.^2;
else;
half_width = sqrt( h .*( 2.0d+00 .* r - h ) );
angle = 2.0d+00 .* atan2( half_width, r - h );
sector = r.^2 .* angle ./ 2.0d+00;
triangle =( r - h ) .* half_width;
area = sector - triangle;
end;
circle_lune_h_area_2dresult = area;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',r); evalin('caller',[inputname(1),'=FUntemp;']); end
if csnil&&~isempty(inputname(2)), assignin('caller','FUntemp',h); evalin('caller',[inputname(2),'=FUntemp;']); end
return;
end
function [circle_lune_w_area_2dresult, r, w ]=circle_lune_w_area_2d( r, w );
%
%*******************************************************************************
%
%! CIRCLE_LUNE_W_AREA_2D returns the area of a circular lune in 2D.
%
%
%  Discussion:
%
%    A lune is formed by drawing a circular arc, and joining its endpoints.
%    This lune is described by the 'width' of the region.  In other words,
%    the lune is the portion of the circle under water if the width
%    of the water surface is W.  There are two possible values for this
%    area, A and (PI*R**2-A).  The routine returns the smaller of the two values.
%
%  Modified:
%
%    15 February 2001
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, doubleprecision R, the radius of the circle.
%
%    Input, doubleprecision W, the width of the lune region.
%
%    Output, doubleprecision CIRCLE_LUNE_W_AREA_2D, the area of the lune.
%
circle_lune_w_area_2dresult=[];
persistent angle area circle_lune_w_area_2d h half_width sector triangle ; 

if isempty(angle), angle=0; end;
if isempty(area), area=0; end;
if isempty(circle_lune_w_area_2dresult), circle_lune_w_area_2dresult=0; end;
if isempty(h), h=0; end;
if isempty(half_width), half_width=0; end;
if isempty(sector), sector=0; end;
if isempty(triangle), triangle=0; end;
%
if( w <= 0.0d+00 )
area = 0.0d+00;
elseif( w >= 2.0d+00 .* r ) ;
area = 0.5d+00 .* pi() .* r.^2;
else;
half_width = 0.5d+00 .* w;
h = r - sqrt( r.^2 - half_width.^2 );
angle = 2.0d+00 .* atan2( half_width, r - h );
sector = r.^2 .* angle ./ 2.0d+00;
triangle =( r - h ) .* half_width;
area = sector - triangle;
end;
circle_lune_w_area_2dresult = area;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(2)), assignin('caller','FUntemp',w); evalin('caller',[inputname(2),'=FUntemp;']); end
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',r); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [func, xc, yc, radius, theta1, theta2, nr, result]=circle_sector( func, xc, yc, radius, theta1, theta2, nr, result );
%
%*******************************************************************************
%
%! CIRCLE_SECTOR approximates an integral in a circular sector.
%
%
%  Discussion:
%
%    A sector is contained within a circular arc and the lines joining each
%    endpoint of the arc to the center of the circle.
%
%  Modified:
%
%    19 January 2001
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, external FUNC, the name of the user supplied function of two
%    variables which is to be integrated, of the form:
%
%      function func ( x, y )
%      doubleprecision func
%      doubleprecision x
%      doubleprecision y
%
%    Input, doubleprecision XC, YC, the center of the circle.
%
%    Input, doubleprecision RADIUS, the radius of the circle.
%
%    Input, doubleprecision THETA1, THETA2, the angles defining the sector.
%    The sector is measured from THETA1 to THETA2.
%
%    Input, integer NR, the number of radial values used in the approximation
%    of the integral.  NR must be at least 1.  Higher values improve the
%    accuracy of the integration, at the cost of more function evaluations.
%
%    Output, doubleprecision RESULT, the approximation to the integral.
%
%
persistent a area b c d i j nt pi quad ra rw t ta tw x y ; 

if isempty(a), a=0; end;
if isempty(area), area=0; end;
if isempty(b), b=0; end;
if isempty(c), c=0; end;
if isempty(d), d=0; end;
if isempty(i), i=0; end;
if isempty(j), j=0; end;
if isempty(nt), nt=0; end;
if isempty(pi), pi=0; end;
if isempty(quad), quad=0; end;
if isempty(ra), ra=zeros(1,nr); end;
if isempty(rw), rw=zeros(1,nr); end;
if isempty(t), t=0; end;
if isempty(ta), ta=zeros(1,4.*nr); end;
if isempty(tw), tw=zeros(1,4.*nr); end;
if isempty(x), x=0; end;
if isempty(y), y=0; end;
%
%
%  Set the radial abscissas and weights.
%
[ nr, ra, rw ]=legendre_set( nr, ra, rw );
a = -1.0d+00;
b = +1.0d+00;
c =  0.0d+00;
d =  radius.^2;
[ a, b, c, d, nr, ra, rw ]=rule_adjust( a, b, c, d, nr, ra, rw );
ra([1:nr]) = sqrt( ra([1:nr]) );
rw([1:nr]) = rw([1:nr]) ./ radius.^2;
%
%  Pick angles evenly spaced between THETA1 and THETA2, but do not
%  include the endpoints, and use a half interval for the first and last.
%
nt = fix(4 .* nr);
[ theta1, theta2, nt, ta ]=tvec_even_bracket3( theta1, theta2, nt, ta );
tw([1:nt]) = 1.0d+00 ./ ( nt );
%
%  Approximate the integral.
%
quad = 0.0d+00;
for i = 1: nr;
for j = 1: nt;
x = xc + ra(i) .* cos( ta(j) );
y = yc + ra(i) .* sin( ta(j) );
quad = quad + rw(i) .* tw(j) .* func( x, y );
end; j = fix(nt+1);
end; i = fix(nr+1);
[area , radius, theta1, theta2 ]=circle_sector_area_2d( radius, theta1, theta2 );
result = quad .* area;
return;
end
function [circle_sector_area_2dresult, r, theta1, theta2 ]=circle_sector_area_2d( r, theta1, theta2 );
%
%*******************************************************************************
%
%! CIRCLE_SECTOR_AREA_2D returns the area of a circular sector in 2D.
%
%
%  Discussion:
%
%    A sector is contained within a circular arc and the lines joining each
%    endpoint of the arc to the center of the circle.
%
%  Modified:
%
%    12 December 2000
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, doubleprecision R, the radius of the circle.
%
%    Input, doubleprecision THETA1, THETA2, the angles of the rays
%    that delimit the sector.
%
%    Output, doubleprecision CIRCLE_SECTOR_AREA_2D, the area of the sector.
%
circle_sector_area_2dresult=[];
persistent circle_sector_area_2d ; 

if isempty(circle_sector_area_2dresult), circle_sector_area_2dresult=0; end;
%
circle_sector_area_2dresult = 0.50d+00 .* r.^2 .*( theta2 - theta1 );
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(3)), assignin('caller','FUntemp',theta2); evalin('caller',[inputname(3),'=FUntemp;']); end
if csnil&&~isempty(inputname(2)), assignin('caller','FUntemp',theta1); evalin('caller',[inputname(2),'=FUntemp;']); end
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',r); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [rule, nr, ra, rw, nt, ta, tw, zw]=circle_set_rt( rule, nr, ra, rw, nt, ta, tw, zw );
%
%*******************************************************************************
%
%! CIRCLE_SET_RT sets an R, THETA product quadrature rule in the unit circle.
%
%
%  Integration region:
%
%    Points (X,Y) such that:
%
%      X**2 + Y**2 <= 1.0.
%
%  Reference:
%
%    Abramowitz and Stegun,
%    Handbook of Mathematical Functions,
%    National Bureau of Standards, 1964.
%
%    Arthur H Stroud,
%    Approximate Calculation of Multiple Integrals,
%    Prentice Hall, 1971.
%
%  Modified:
%
%    16 December 2000
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, integer RULE, the rule desired.
%      1, 1 + 0 * 0 points.
%      2, 0 + 1 * 4 points.
%      3, 1 + 1 * 4 points.
%      4, 1 + 1 * 6 points.
%      5, 1 + 2 * 4 points.
%      6, 0 + 3 * 4 points.
%      8, 0 + 4 * 16 points.
%      9, 0 + 5 * 20 points.
%
%    Output, integer NR, the number of R abscissas.
%
%    Output, doubleprecision RA(NR), RW(NR), the R abscissas and weights.
%
%    Output, integer NT, the number of Theta abscissas.
%
%    Output, doubleprecision TA(NT), TW(NT), the THETA abscissas and weights.
%
%    Output, doubleprecision ZW, the weight to use for the center.
%
persistent a b c d u v w ; 

if isempty(a), a=0; end;
if isempty(b), b=0; end;
if isempty(c), c=0; end;
if isempty(d), d=0; end;
ra_shape=size(ra);ra=reshape(ra,1,[]);
rw_shape=size(rw);rw=reshape(rw,1,[]);
ta_shape=size(ta);ta=reshape(ta,1,[]);
tw_shape=size(tw);tw=reshape(tw,1,[]);
if isempty(u), u=0; end;
if isempty(v), v=0; end;
if isempty(w), w=0; end;
%
if( rule == 1 )
nr = 0;
nt = 0;
zw = 1.0d+00;
elseif( rule == 2 ) ;
nr = 1;
ra(1) = 0.5d+00;
rw(1) = 1.0d+00;
nt = 4;
[ nt, ta ]=tvec_even2( nt, ta );
tw([1:nt]) = 1.0d+00 ./ ( nt );
zw = 0.0d+00;
elseif( rule == 3 ) ;
nr = 1;
ra(1) = 1.0d+00;
rw(1) = 1.0d+00;
nt = 4;
[ nt, ta ]=tvec_even( nt, ta );
tw([1:4]) = 0.125d+00;
zw = 0.5d+00;
elseif( rule == 4 ) ;
nr = 1;
ra(1) = sqrt( 2.0d+00 ./ 3.0d+00 );
rw(1) = 1.0d+00;
nt = 6;
[ nt, ta ]=tvec_even( nt, ta );
tw([1:nt]) = 0.125d+00;
zw = 0.25d+00;
elseif( rule == 5 ) ;
a = 1.0d+00;
b = sqrt( 2.0d+00 ) ./ 2.0d+00;
u = 1.0d+00 ./ 6.0d+00;
v = 4.0d+00 ./ 6.0d+00;
nr = 2;
ra([1:nr]) =[ a, b ];
rw([1:nr]) =[ u, v ];
nt = 4;
[ nt, ta ]=tvec_even( nt, ta );
tw([1:nt]) = 1.0d+00 ./ ( nt );
zw = 4.0d+00 ./ 24.0d+00;
elseif( rule == 6 ) ;
a = sqrt( 3.0d+00 ) ./ 2.0d+00;
b = sqrt(( 27.0d+00 - 3.0d+00 .* sqrt( 29.0d+00 ) ) ./ 52.0d+00 );
c = sqrt(( 27.0d+00 + 3.0d+00 .* sqrt( 29.0d+00 ) ) ./ 52.0d+00 );
u = 8.0d+00 ./ 27.0d+00;
v =( 551.0d+00 + 41.0d+00 .* sqrt( 29.0d+00 ) ) ./ 1566.0d+00;
w =( 551.0d+00 - 41.0d+00 .* sqrt( 29.0d+00 ) ) ./ 1566.0d+00;
nr = 3;
ra([1:nr]) =[ a, b, c ];
rw([1:nr]) =[ u, v, w ];
nt = 4;
[ nt, ta ]=tvec_even( nt, ta );
tw([1:nt]) = 1.0d+00 ./ ( nt );
zw = 0.0d+00;
elseif( rule == 7 ) ;
a = sqrt(( 6.0d+00 - sqrt( 6.0d+00 ) ) ./ 10.0d+00 );
b = sqrt(( 6.0d+00 + sqrt( 6.0d+00 ) ) ./ 10.0d+00 );
u =( 16.0d+00 + sqrt( 6.0d+00 ) ) ./ 36.0d+00;
v =( 16.0d+00 - sqrt( 6.0d+00 ) ) ./ 36.0d+00;
nr = 2;
ra([1:nr]) =[ a, b ];
rw([1:nr]) =[ u, v ];
nt = 10;
[ nt, ta ]=tvec_even( nt, ta );
tw([1:nt]) = 1.0d+00 ./ ( nt );
zw = 1.0d+00 ./ 9.0d+00;
elseif( rule == 8 ) ;
nr = 4;
[ nr, ra, rw ]=legendre_set( nr, ra, rw );
a = -1.0d+00;
b = +1.0d+00;
c =  0.0d+00;
d = +1.0d+00;
[ a, b, c, d, nr, ra, rw ]=rule_adjust( a, b, c, d, nr, ra, rw );
ra([1:nr]) = sqrt( ra([1:nr]) );
nt = 16;
[ nt, ta ]=tvec_even( nt, ta );
tw([1:nt]) = 1.0d+00 ./ ( nt );
zw = 0.0d+00;
elseif( rule == 9 ) ;
nr = 5;
[ nr, ra, rw ]=legendre_set( nr, ra, rw );
a = -1.0d+00;
b = +1.0d+00;
c =  0.0d+00;
d = +1.0d+00;
[ a, b, c, d, nr, ra, rw ]=rule_adjust( a, b, c, d, nr, ra, rw );
ra([1:nr]) = sqrt( ra([1:nr]) );
nt = 20;
[ nt, ta ]=tvec_even( nt, ta );
tw([1:nt]) = 1.0d+00 ./ ( nt );
zw = 0.0d+00;
else;
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'CIRCLE_SET_RT - Fatal error!');
writef(1,['%s %0.15g \n'], '  There is no rule of index ', rule);
error(['stop encountered in original fortran code  ',char(10),';']);
end;
ra_shape=zeros(ra_shape);ra_shape(:)=ra(1:numel(ra_shape));ra=ra_shape;
rw_shape=zeros(rw_shape);rw_shape(:)=rw(1:numel(rw_shape));rw=rw_shape;
ta_shape=zeros(ta_shape);ta_shape(:)=ta(1:numel(ta_shape));ta=ta_shape;
tw_shape=zeros(tw_shape);tw_shape(:)=tw(1:numel(tw_shape));tw=tw_shape;
return;
end
function [rule, norder, xtab, ytab, weight]=circle_set_xy( rule, norder, xtab, ytab, weight );
%
%*******************************************************************************
%
%! CIRCLE_SET_XY sets an XY quadrature rule inside the unit circle in 2D.
%
%
%  Integration region:
%
%    Points (X,Y) such that:
%
%      X**2 + Y**2 <= 1.0.
%
%  Reference:
%
%    Abramowitz and Stegun,
%    Handbook of Mathematical Functions,
%    National Bureau of Standards, 1964.
%
%    Frank Lether,
%    A Generalized Product Rule for the Circle,
%    SIAM Journal on Numerical Analysis,
%    Volume 8, Number 2, June 1971, pages 249-253.
%
%    Arthur H Stroud,
%    Approximate Calculation of Multiple Integrals,
%    Prentice Hall, 1971.
%
%  Modified:
%
%    16 December 2000
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, integer RULE, the rule desired.
%      1, 1 point 1-st degree;
%      2, 4 point 3-rd degree, Stroud S2:3-1;
%      3, 4 point 3-rd degree, Lether #1;
%      4, 4 point 3-rd degree, Stroud S2:3-2;
%      5, 5 point 3-rd degree;
%      6, 7 point 5-th degree;
%      7, 9 point 5-th degree;
%      8, 9 point 5-th degree, Lether #2;
%      9, 12 point 7-th degree;
%     10, 16 point 7-th degree, Lether #3;
%     11, 21 point 9-th degree, Stroud S2:9-3;
%     12, 25 point 9-th degree, Lether #4 (after correcting error);
%     13, 64 point 15-th degree Gauss product rule.
%
%    Output, integer NORDER, the order of the desired rule.
%
%    Output, doubleprecision XTAB(*), YTAB(*), the NORDER abscissas of the rule.
%
%    Output, doubleprecision WEIGHT(*), the NORDER weights of the rule.
%
persistent a b c d e f g h i j k nr r ra rw s t u v w w1 w2 w3 w4 w5 w6 w7 w8 w9 z ; 

if isempty(a), a=0; end;
if isempty(b), b=0; end;
if isempty(c), c=0; end;
if isempty(d), d=0; end;
if isempty(e), e=0; end;
if isempty(f), f=0; end;
if isempty(g), g=0; end;
if isempty(h), h=0; end;
if isempty(i), i=0; end;
if isempty(j), j=0; end;
if isempty(k), k=0; end;
if isempty(nr), nr=0; end;
if isempty(r), r=0; end;
if isempty(ra), ra=zeros(1,4); end;
if isempty(rw), rw=zeros(1,4); end;
if isempty(s), s=0; end;
if isempty(t), t=0; end;
if isempty(u), u=0; end;
if isempty(v), v=0; end;
if isempty(w), w=0; end;
if isempty(w1), w1=0; end;
if isempty(w2), w2=0; end;
if isempty(w3), w3=0; end;
if isempty(w4), w4=0; end;
if isempty(w5), w5=0; end;
if isempty(w6), w6=0; end;
if isempty(w7), w7=0; end;
if isempty(w8), w8=0; end;
if isempty(w9), w9=0; end;
weight_shape=size(weight);weight=reshape(weight,1,[]);
xtab_shape=size(xtab);xtab=reshape(xtab,1,[]);
ytab_shape=size(ytab);ytab=reshape(ytab,1,[]);
if isempty(z), z=0; end;
%
if( rule == 1 )
norder = 1;
xtab(1) = 0.0d+00;
ytab(1) = 0.0d+00;
weight(1) = 1.0d+00;
elseif( rule == 2 ) ;
a = 0.5d+00;
b = 0.25d+00;
z = 0.0d+00;
norder = 4;
xtab([1:4]) =[  a, -a,  z,  z ];
ytab([1:4]) =[  z,  z,  a, -a ];
weight([1:4]) =[  b,  b,  b,  b ];
elseif( rule == 3 ) ;
a = 0.5d+00;
b = 0.25d+00;
norder = 4;
xtab([1:4]) =[  a, -a, -a,  a ];
ytab([1:4]) =[  a,  a, -a, -a ];
weight([1:4]) =[  b,  b,  b,  b ];
elseif( rule == 4 ) ;
a = sqrt( 2.0d+00 ) ./ 2.0d+00;
b = 0.25d+00;
norder = 4;
xtab([1:4]) =[  a, -a, -a,  a ];
ytab([1:4]) =[  a,  a, -a, -a ];
weight([1:4]) =[  b,  b,  b,  b ];
elseif( rule == 5 ) ;
a = 1.0d+00;
b = 0.5d+00;
c = 0.125d+00;
z = 0.0d+00;
norder = 5;
xtab([1:5]) =[ z, a, z, -a,  z ];
ytab([1:5]) =[ z, z, a,  z, -a ];
weight([1:5]) =[ b, c, c,  c,  c ];
elseif( rule == 6 ) ;
a = sqrt( 2.0d+00 ./ 3.0d+00 );
b = sqrt( 1.0d+00 ./ 6.0d+00 );
c = sqrt( 2.0d+00 ) ./ 2.0d+00;
d = 0.125d+00;
e = 0.25d+00;
z = 0.0d+00;
norder = 7;
xtab([1:7]) =[ z, a, -a,  b, -b,  b, -b ];
ytab([1:7]) =[ z, z,  z,  c,  c, -c, -c ];
weight([1:7]) =[ e, d,  d,  d,  d,  d,  d ];
elseif( rule == 7 ) ;
a = 0.5d+00;
b = 1.0d+00;
c = 4.0d+00 ./ 24.0d+00;
d = 1.0d+00 ./ 24.0d+00;
z = 0.0d+00;
norder = 9;
xtab([1:9]) =[ z,  b, -b,  z,  z,  a, -a, -a,  a ];
ytab([1:9]) =[ z,  z,  z,  b, -b,  a,  a, -a, -a ];
weight([1:9]) =[ c,  d,  d,  d,  d,  c,  c,  c,  c ];
elseif( rule == 8 ) ;
a = sqrt( 2.0d+00 ) ./ 2.0d+00;
b = sqrt( 3.0d+00 ./ 5.0d+00 );
c = sqrt( 3.0d+00 ./ 10.0d+00 );
w1 = 16.0d+00 ./ 72.0d+00;
w2 =  8.0d+00 ./ 72.0d+00;
w3 = 10.0d+00 ./ 72.0d+00;
w4 =  5.0d+00 ./ 72.0d+00;
z = 0.0d+00;
norder = 9;
xtab([1:9]) =[  z,   a,  -a,   z,   z,   a,   a,  -a,  -a ];
ytab([1:9]) =[  z,   z,   z,   b,  -b,   c,  -c,   c,  -c ];
weight([1:9]) =[ w1,  w2,  w2,  w3,  w3,  w4,  w4,  w4,  w4 ];
elseif( rule == 9 ) ;
a = sqrt( 3.0d+00 ) ./ 2.0d+00;
b = sqrt(( 27.0d+00 - 3.0d+00 .* sqrt( 29.0d+00 ) ) ./ 104.0d+00 );
c = sqrt(( 27.0d+00 + 3.0d+00 .* sqrt( 29.0d+00 ) ) ./ 104.0d+00 );
u = 2.0d+00 ./ 27.0d+00;
v =( 551.0d+00 + 41.0d+00 .* sqrt( 29.0d+00 ) ) ./ 6264.0d+00;
w =( 551.0d+00 - 41.0d+00 .* sqrt( 29.0d+00 ) ) ./ 6264.0d+00;
z = 0.0d+00;
norder = 12;
xtab([1:12]) =[ a, -a,  z,  z,  b, -b,  b, -b,  c,  c, -c, -c ];
ytab([1:12]) =[ z,  z,  a, -a,  b,  b, -b, -b,  c, -c,  c, -c ];
weight([1:12]) =[ u,  u,  u,  u,  v,  v,  v,  v,  w,  w,  w,  w ];
elseif( rule == 10 ) ;
a = sqrt(( 3.0d+00 - sqrt( 5.0d+00 ) ) ./ 8.0d+00 );
b = sqrt(( 15.0d+00 + 3.0d+00 .* sqrt( 5.0d+00 )- 2.0d+00 .* sqrt( 30.0d+00 ) - 2.0d+00 .* sqrt( 6.0d+00 ) ) ./ 56.0d+00 );
c = sqrt(( 15.0d+00 + 3.0d+00 .* sqrt( 5.0d+00 )+ 2.0d+00 .* sqrt( 30.0d+00 ) + 2.0d+00 .* sqrt( 6.0d+00 ) ) ./ 56.0d+00 );
d = sqrt(( 3.0d+00 + sqrt( 5.0d+00 ) ) ./ 8.0d+00 );
e = sqrt(( 15.0d+00 - 3.0d+00 .* sqrt( 5.0d+00 )- 2.0d+00 .* sqrt( 30.0d+00 ) + 2.0d+00 .* sqrt( 6.0d+00 ) ) ./ 56.0d+00 );
f = sqrt(( 15.0d+00 - 3.0d+00 .* sqrt( 5.0d+00 )+ 2.0d+00 .* sqrt( 30.0d+00 ) - 2.0d+00 .* sqrt( 6.0d+00 ) ) ./ 56.0d+00 );
w1 =( 90.0d+00 + 5.0d+00 .* sqrt( 30.0d+00 ) + 18.0d+00 .* sqrt( 5.0d+00 )+ 5.0d+00 .* sqrt( 6.0d+00 ) ) ./ 1440.0d+00;
w2 =( 90.0d+00 - 5.0d+00 .* sqrt( 30.0d+00 ) + 18.0d+00 .* sqrt( 5.0d+00 )- 5.0d+00 .* sqrt( 6.0d+00 ) ) ./ 1440.0d+00;
w3 =( 90.0d+00 + 5.0d+00 .* sqrt( 30.0d+00 ) - 18.0d+00 .* sqrt( 5.0d+00 )- 5.0d+00 .* sqrt( 6.0d+00 ) ) ./ 1440.0d+00;
w4 =( 90.0d+00 - 5.0d+00 .* sqrt( 30.0d+00 ) - 18.0d+00 .* sqrt( 5.0d+00 )+ 5.0d+00 .* sqrt( 6.0d+00 ) ) ./ 1440.0d+00;
norder = 16;
xtab([1:norder]) =[  a,  a, -a, -a,  a,  a, -a, -a,  d,  d, -d, -d,d,  d, -d, -d ];
ytab([1:norder]) =[  b, -b,  b, -b,  c, -c,  c, -c,  e, -e,  e, -e,f, -f,  f, -f ];
weight([1:norder]) =[ w1, w1, w1, w1, w2, w2, w2, w2, w3, w3, w3, w3,w4, w4, w4, w4 ];
elseif( rule == 11 ) ;
norder = 21;
xtab(1) = 0.0d+00;
ytab(1) = 0.0d+00;
weight(1) = 1.0d+00 ./ 9.0d+00;
weight([2:11]) =( 16.0d+00 + sqrt( 6.0d+00 ) ) ./ 360.0d+00;
weight([12:21]) =( 16.0d+00 - sqrt( 6.0d+00 ) ) ./ 360.0d+00;
r = sqrt(( 6.0d+00 - sqrt( 6.0d+00 ) ) ./ 10.0d+00 );
for i = 1: 10;
a = 2.0d+00 .* pi( ) .* ( i ) ./ ( 10.0d+00 );
xtab(1+i) = r .* cos( a );
ytab(1+i) = r .* sin( a );
end; i = fix(10+1);
r = sqrt(( 6.0d+00 + sqrt( 6.0d+00 ) ) ./ 10.0d+00 );
for i = 1: 10;
a = 2.0d+00 .* pi( ) .* ( i ) ./ ( 10.0d+00 );
xtab(11+i) = r .* cos( a );
ytab(11+i) = r .* sin( a );
end; i = fix(10+1);
%
%  There was apparently a misprint in the Lether paper.  The quantity
%  which here reads '322' was printed there as '332'.
%
elseif( rule == 12 ) ;
a = 0.5d+00;
b = sqrt( 3.0d+00 ) ./ 2.0d+00;
c = sqrt(( 35.0d+00 + 2.0d+00 .* sqrt( 70.0d+00 ) ) ./ 252.0d+00 );
d = sqrt(( 35.0d+00 - 2.0d+00 .* sqrt( 70.0d+00 ) ) ./ 252.0d+00 );
e = sqrt(( 35.0d+00 + 2.0d+00 .* sqrt( 70.0d+00 ) ) ./ 84.0d+00 );
f = sqrt(( 35.0d+00 - 2.0d+00 .* sqrt( 70.0d+00 ) ) ./ 84.0d+00 );
g = sqrt(( 35.0d+00 + 2.0d+00 .* sqrt( 70.0d+00 ) ) ./ 63.0d+00 );
h = sqrt(( 35.0d+00 - 2.0d+00 .* sqrt( 70.0d+00 ) ) ./ 63.0d+00 );
w1 = 64.0d+00 ./ 675.0d+00;
w2 = 16.0d+00 ./ 225.0d+00;
w3 = 16.0d+00 ./ 675.0d+00;
w4 =( 322.0d+00 - 13.0d+00 .* sqrt( 70.0d+00 ) ) ./ 21600.0d+00;
w5 =( 322.0d+00 + 13.0d+00 .* sqrt( 70.0d+00 ) ) ./ 21600.0d+00;
w6 =( 322.0d+00 - 13.0d+00 .* sqrt( 70.0d+00 ) ) ./ 7200.0d+00;
w7 =( 322.0d+00 + 13.0d+00 .* sqrt( 70.0d+00 ) ) ./ 7200.0d+00;
w8 =( 322.0d+00 - 13.0d+00 .* sqrt( 70.0d+00 ) ) ./ 5400.0d+00;
w9 =( 322.0d+00 + 13.0d+00 .* sqrt( 70.0d+00 ) ) ./ 5400.0d+00;
z = 0.0d+00;
norder = 25;
xtab([1:norder]) =[  z,  a, -a,  b, -b,  b,  b, -b, -b,  b,  b, -b, -b,a,  a, -a, -a,  a,  a, -a, -a,  z,  z,  z,  z ];
ytab([1:norder]) =[  z,  z,  z,  z,  z,  c, -c,  c, -c,  d, -d,  d, -d,e, -e,  e, -e,  f, -f,  f, -f,  g, -g,  h, -h ];
weight([1:norder]) =[ w1, w2, w2, w3, w3, w4, w4, w4, w4, w5, w5, w5, w5,w6, w6, w6, w6, w7, w7, w7, w7, w8, w8, w9, w9 ];
elseif( rule == 13 ) ;
nr = 4;
[ nr, ra, rw ]=legendre_set( nr, ra, rw );
a = -1.0d+00;
b = +1.0d+00;
c =  0.0d+00;
d = +1.0d+00;
[ a, b, c, d, nr, ra, rw ]=rule_adjust( a, b, c, d, nr, ra, rw );
ra([1:nr]) = sqrt( ra([1:nr]) );
norder = 64;
i = 0;
for j = 1: 16;
c = cos( pi( ) .* ( j ) ./ 8.0d+00 );
s = sin( pi( ) .* ( j ) ./ 8.0d+00 );
for k = 1: 4;
i = fix(i + 1);
xtab(i) = c .* ra(k);
ytab(i) = s .* ra(k);
weight(i) = rw(k) ./ 16.0d+00;
end; k = fix(4+1);
end; j = fix(16+1);
else;
norder = 0;
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'CIRCLE_SET_XY - Fatal error!');
writef(1,['%s %0.15g \n'], '  There is no rule of index ', rule);
error(['stop encountered in original fortran code  ',char(10),';']);
end;
weight_shape=zeros(weight_shape);weight_shape(:)=weight(1:numel(weight_shape));weight=weight_shape;
xtab_shape=zeros(xtab_shape);xtab_shape(:)=xtab(1:numel(xtab_shape));xtab=xtab_shape;
ytab_shape=zeros(ytab_shape);ytab_shape(:)=ytab(1:numel(ytab_shape));ytab=ytab_shape;
return;
end
function [func, xc, yc, radius, nr, ra, rw, nt, ta, tw, zw,result]=circle_sum_rt( func, xc, yc, radius, nr, ra, rw, nt, ta, tw, zw,result );
%
%*******************************************************************************
%
%! CIRCLE_SUM_RT applies an R, THETA product quadrature rule inside a circle.
%
%
%  Discussion:
%
%    The product rule is assumed to be have the form:
%
%      Integral_Approx = ZW * F(XC,YC) +
%        Sum ( 1 <= IR <= NR ) Sum ( 1 <= IT <= NT )
%        RW(IR) * TW(IT) * F ( XC + R(IR) * RADIUS * Cos ( TA(IT) ),
%                              YC + R(IR) * RADIUS * Sin ( TA(IT) ) )
%
%  Integration region:
%
%    Points (X,Y) such that:
%
%      (X-XC)**2 + (Y-YC)**2 <= RADIUS**2.
%
%  Modified:
%
%    15 December 2000
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, external FUNC, the name of the user supplied
%    function of two variables which is to be integrated,
%    of the form:
%
%      function func ( x, y )
%      doubleprecision func
%      doubleprecision x
%      doubleprecision y
%
%    Input, doubleprecision XC, YC, the coordinates of the center of the circle.
%
%    Input, doubleprecision RADIUS, the radius of the circle.
%
%    Input, integer NR, the number of R abscissas.
%
%    Input, doubleprecision RA(NR), RW(NR), the R abscissas and weights.
%
%    Input, integer NT, the number of Theta abscissas.
%
%    Input, doubleprecision TA(NT), TW(NT), the THETA abscissas and weights.
%
%    Input, doubleprecision ZW, the weight to use for the center.
%
%    Output, doubleprecision RESULT, the approximate integral of the function.
%
%
persistent ir it quad rct rst volume x y ; 

if isempty(ir), ir=0; end;
if isempty(it), it=0; end;
if isempty(quad), quad=0; end;
if isempty(rct), rct=0; end;
if isempty(rst), rst=0; end;
if isempty(volume), volume=0; end;
if isempty(x), x=0; end;
if isempty(y), y=0; end;
%
%
quad = 0.0d+00;
if( zw ~= 0.0d+00 )
x = xc;
y = yc;
quad = quad + zw .* func( x, y );
end;
for it = 1: nt;
rct = radius .* cos( ta(it) );
rst = radius .* sin( ta(it) );
for ir = 1: nr;
x = xc + ra(ir) .* rct;
y = yc + ra(ir) .* rst;
quad = quad + tw(it) .* rw(ir) .* func( x, y );
end; ir = fix(nr+1);
end; it = fix(nt+1);
[volume , radius ]=circle_area_2d( radius );
result = quad .* volume;
return;
end
function [func, xc, yc, radius, norder, xtab, ytab, weight,result]=circle_sum_xy( func, xc, yc, radius, norder, xtab, ytab, weight,result );
%
%*******************************************************************************
%
%! CIRCLE_SUM_XY applies an XY quadrature rule inside a circle in 2D.
%
%
%  Integration region:
%
%    Points (X,Y) such that:
%
%      (X-XC)**2 + (Y-YC)**2 <= RADIUS**2.
%
%  Modified:
%
%    14 December 2000
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, external FUNC, the name of the user supplied
%    function of two variables which is to be integrated,
%    of the form:
%
%      function func ( x, y )
%      doubleprecision func
%      doubleprecision x
%      doubleprecision y
%
%    Input, doubleprecision XC, YC, the coordinates of the center of the circle.
%
%    Input, doubleprecision RADIUS, the radius of the circle.
%
%    Input, integer NORDER, the order of the rule.  The rule is
%    assumed to be defined on the unit circle.
%
%    Input, doubleprecision XTAB(NORDER), YTAB(NORDER), the XY
%    coordinates of the abscissas of the quadrature rule for a unit circle.
%
%    Input, doubleprecision WEIGHT(NORDER), the weights of the rule.
%
%    Output, doubleprecision RESULT, the approximate integral of the function.
%
%
persistent i quad volume x y ; 

if isempty(i), i=0; end;
if isempty(quad), quad=0; end;
if isempty(volume), volume=0; end;
if isempty(x), x=0; end;
if isempty(y), y=0; end;
%
%
quad = 0.0d+00;
for i = 1: norder;
x = xc + radius .* xtab(i);
y = yc + radius .* ytab(i);
quad = quad + weight(i) .* func( x, y );
end; i = fix(norder+1);
[volume , radius ]=circle_area_2d( radius );
result = quad .* volume;
return;
end
function [circle_triangle_area_2dresult, r, theta1, theta2 ]=circle_triangle_area_2d( r, theta1, theta2 );
%
%*******************************************************************************
%
%! CIRCLE_TRIANGLE_AREA_2D returns the area of a circle triangle in 2D.
%
%
%  Discussion:
%
%    A circle triangle is formed by drawing a circular arc, and considering
%    the triangle formed by the endpoints of the arc plus the center of
%    the circle.
%
%    Note that for angles greater than PI, the triangle will actually
%    have NEGATIVE area.
%
%  Modified:
%
%    12 December 2000
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, doubleprecision R, the radius of the circle.
%
%    Input, doubleprecision THETA1, THETA2, the angles of the rays that
%    delimit the arc.
%
%    Output, doubleprecision CIRCLE_TRIANGLE_AREA_2D, the (signed) area
%    of the triangle.
%
circle_triangle_area_2dresult=[];
persistent circle_triangle_area_2d ; 

if isempty(circle_triangle_area_2dresult), circle_triangle_area_2dresult=0; end;
%
circle_triangle_area_2dresult = 0.5d+00 .* r.^2 .* sin( theta2 - theta1 );
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(3)), assignin('caller','FUntemp',theta2); evalin('caller',[inputname(3),'=FUntemp;']); end
if csnil&&~isempty(inputname(2)), assignin('caller','FUntemp',theta1); evalin('caller',[inputname(2),'=FUntemp;']); end
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',r); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [func, result]=cone_unit_3d( func, result );
%
%*******************************************************************************
%
%! CONE_UNIT_3D approximates an integral inside a unit cone in 3D.
%
%
%  Integration Region:
%
%    X**2 + Y**2 <= 1 - Z
%    0 <= Z <= 1.
%
%  Discussion:
%
%    An 48 point degree 7 formula, Stroud CN:S2:7-1, is used.
%
%    (There is a typographical error in the S2:7-1 formula for B3.)
%
%  Reference:
%
%    Arthur H Stroud,
%    Approximate Calculation of Multiple Integrals,
%    Prentice Hall, 1971, page 339.
%
%  Modified:
%
%    18 November 2000
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, external FUNC, the name of the user supplied function which
%    evaluates F(X,Y,Z), of the form
%
%      function func ( x, y, z )
%      doubleprecision func
%      doubleprecision x
%      doubleprecision y
%      doubleprecision z
%
%    Output, doubleprecision RESULT, the approximate integral of the function.
%
persistent a b c h i pi quad r u volume w1 w2 x y z ; 

if isempty(a), a=0; end;
if isempty(b), b=0; end;
if isempty(c), c=0; end;
if isempty(h), h=0; end;
if isempty(i), i=0; end;
if isempty(pi), pi=0; end;
if isempty(quad), quad=0; end;
if isempty(r), r=0; end;
if isempty(u), u([1:4]) =[ 0.04850054945d+00, 0.2386007376d+00, 0.5170472951d+00,  0.7958514179d+00 ]; end;
if isempty(volume), volume=0; end;
if isempty(w1), w1([1:4]) =[ 0.1108884156d+00,  0.1434587878d+00, 0.06863388717d+00, 0.01035224075d+00 ]; end;
if isempty(w2), w2=zeros(1,3); end;
if isempty(x), x=0; end;
if isempty(y), y=0; end;
if isempty(z), z=0; end;
%
%
a = sqrt( 3.0d+00 ) ./ 2.0d+00;
b = sqrt(( 27.0d+00 - 3.0d+00 .* sqrt( 29.0d+00 ) ) ./ 104.0d+00 );
c = sqrt(( 27.0d+00 + 3.0d+00 .* sqrt( 29.0d+00 ) ) ./ 104.0d+00 );
w2([1:3]) = 3.0d+00 .*[2.0d+00 ./ 27.0d+00,( 551.0d+00 + 4.0d+00 .* sqrt( 29.0d+00 ) ) ./ 6264.0d+00,( 551.0d+00 - 4.0d+00 .* sqrt( 29.0d+00 ) ) ./ 6264.0d+00 ];
quad = 0.0d+00;
for i = 1: 4;
x = a .*( 1.0d+00 - u(i) );
y = 0.0d+00;
z = u(i);
quad = quad + w1(i) .* w2(1) .* func( x, y, z );
x = - a .*( 1.0d+00 - u(i) );
y = 0.0d+00;
z = u(i);
quad = quad + w1(i) .* w2(1) .* func( x, y, z );
x = 0.0d+00;
y = a .*( 1.0d+00 - u(i) );
z = u(i);
quad = quad + w1(i) .* w2(1) .* func( x, y, z );
x = 0.0d+00;
y = - a .*( 1.0d+00 - u(i) );
z = u(i);
quad = quad + w1(i) .* w2(1) .* func( x, y, z );
end; i = fix(4+1);
for i = 1: 4;
x =   b .*( 1.0d+00 - u(i) );
y =   b .*( 1.0d+00 - u(i) );
z =   u(i);
quad = quad + w1(i) .* w2(2) .* func( x, y, z );
x = - b .*( 1.0d+00 - u(i) );
y =   b .*( 1.0d+00 - u(i) );
z =   u(i);
quad = quad + w1(i) .* w2(2) .* func( x, y, z );
x = - b .*( 1.0d+00 - u(i) );
y = - b .*( 1.0d+00 - u(i) );
z =   u(i);
quad = quad + w1(i) .* w2(2) .* func( x, y, z );
x =   b .*( 1.0d+00 - u(i) );
y = - b .*( 1.0d+00 - u(i) );
z =   u(i);
quad = quad + w1(i) .* w2(2) .* func( x, y, z );
x =   c .*( 1.0d+00 - u(i) );
y =   c .*( 1.0d+00 - u(i) );
z =   u(i);
quad = quad + w1(i) .* w2(3) .* func( x, y, z );
x = - c .*( 1.0d+00 - u(i) );
y =   c .*( 1.0d+00 - u(i) );
z =   u(i);
quad = quad + w1(i) .* w2(3) .* func( x, y, z );
x = - c .*( 1.0d+00 - u(i) );
y = - c .*( 1.0d+00 - u(i) );
z =   u(i);
quad = quad + w1(i) .* w2(3) .* func( x, y, z );
x =   c .*( 1.0d+00 - u(i) );
y = - c .*( 1.0d+00 - u(i) );
z =   u(i);
quad = quad + w1(i) .* w2(3) .* func( x, y, z );
end; i = fix(4+1);
r = 1.0d+00;
h = 1.0d+00;
[volume , r, h ]=cone_volume_3d( r, h );
result = quad .* volume;
return;
end
function [cone_volume_3dresult, r, h ]=cone_volume_3d( r, h );
%
%*******************************************************************************
%
%! CONE_VOLUME_3D returns the volume of a cone in 3D.
%
%
%  Modified:
%
%    16 November 2000
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, doubleprecision R, the radius of the base of the cone.
%
%    Input, doubleprecision H, the height of the cone.
%
%    Output, doubleprecision CONE_VOLUME_3D, the volume of the cone.
%
cone_volume_3dresult=[];
persistent cone_volume_3d ; 

if isempty(cone_volume_3dresult), cone_volume_3dresult=0; end;
%
cone_volume_3dresult =( pi( ) ./ 3.0d+00 ) .* h .* r.^2;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',r); evalin('caller',[inputname(1),'=FUntemp;']); end
if csnil&&~isempty(inputname(2)), assignin('caller','FUntemp',h); evalin('caller',[inputname(2),'=FUntemp;']); end
return;
end
function [func, n, r1, r2, result]=cube_shell_nd( func, n, r1, r2, result );
%
%*******************************************************************************
%
%! CUBE_SHELL_ND approximates an integral inside a cubic shell in N dimensions.
%
%
%  Discussion:
%
%    An N*2**N point third degree formula is used, Stroud number CNSHELL:3-4.
%
%  Integration region:
%
%    N dimensional points X() such that:
%
%      R1 <= ABS ( X(I) ) <= R2, for I = 1 to N.
%
%  Reference:
%
%    Arthur H Stroud,
%    Approximate Calculation of Multiple Integrals,
%    Prentice Hall, 1971.
%
%  Modified:
%
%    25 August 1998
%
%  Parameters:
%
%    Input, external FUNC, the name of the user supplied
%    function which evaluates F at the N-vector X, of the form
%
%      function func ( n, x )
%      integer n
%      doubleprecision func
%      doubleprecision x(n)
%
%    Input, integer N, the dimension of the space.
%
%    Input, doubleprecision R1, R2, the inner and outer radii of the cubical
%    shell.  The outer cube is of side 2*R2, the inner, missing cube of side
%    2*R1.
%
%    Output, doubleprecision RESULT, the approximate integral of the function.
%
%
persistent i iadd ix j jhi more ncard quad u v volume w x ; 

if isempty(i), i=0; end;
if isempty(iadd), iadd=0; end;
if isempty(ix), ix=zeros(1,n); end;
if isempty(j), j=0; end;
if isempty(jhi), jhi=0; end;
if isempty(more), more=false; end;
if isempty(ncard), ncard=0; end;
if isempty(quad), quad=0; end;
if isempty(u), u=0; end;
if isempty(v), v=0; end;
if isempty(volume), volume=0; end;
if isempty(w), w=0; end;
if isempty(x), x=zeros(1,n); end;
%
%
if( r1 == r2 )
result = 0.0d+00;
return;
end;
u = sqrt( ( n ) .*( r2.^(n+2) - r1.^(n+2) )./( ( n + 2 ) .*( r2.^n - r1.^n ) ) );
v = u ./ sqrt( 3.0d+00 );
w = 1.0d+00 ./ ( n .* 2.^n );
quad = 0.0d+00;
for i = 1: n;
x([1:n]) = - v;
x(i) = - u;
more = false;
jhi = fix(2.^n);
for j = 1: jhi;
[ n, ix, more, ncard, iadd ]=subset_next( n, ix, more, ncard, iadd );
if( iadd ~= 0 )
x(iadd) = - x(iadd);
end;
quad = quad + w .* func( n, x );
end; j = fix(jhi+1);
end; i = fix(n+1);
[volume , n, r1, r2 ]=cube_shell_volume_nd( n, r1, r2 );
result = quad .* volume;
return;
end
function [cube_shell_volume_ndresult, n, r1, r2 ]=cube_shell_volume_nd( n, r1, r2 );
%
%*******************************************************************************
%
%! CUBE_SHELL_VOLUME_ND computes the volume of a cubic shell in ND.
%
%
%  Integration region:
%
%    N dimensional points X() such that:
%
%      R1 <= ABS ( X(I) ) <= R2, for I = 1 to N.
%
%  Modified:
%
%    20 November 2000
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, integer N, the dimension of the space.
%
%    Input, doubleprecision R1, R2, the inner and outer radii of the cubic
%    shell.  The outer cube is of side 2*R2, the inner, missing cube of side
%    2*R1.
%
%    Output, doubleprecision CUBE_SHELL_VOLUME_ND, the volume of the cubic
%    shell.
%
cube_shell_volume_ndresult=[];
persistent cube_shell_volume_nd ; 

if isempty(cube_shell_volume_ndresult), cube_shell_volume_ndresult=0; end;
%
cube_shell_volume_ndresult =( r2.^n - r1.^n ) .* 2.^n;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(3)), assignin('caller','FUntemp',r2); evalin('caller',[inputname(3),'=FUntemp;']); end
if csnil&&~isempty(inputname(2)), assignin('caller','FUntemp',r1); evalin('caller',[inputname(2),'=FUntemp;']); end
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',n); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [func, result]=cube_unit_3d( func, result );
%
%*******************************************************************************
%
%! CUBE_UNIT_3D approximates an integral inside the unit cube in 3D.
%
%
%  Discussion:
%
%    An 8 point third degree formula is used.
%
%  Integration region:
%
%    Points (X,Y,Z) such that:
%
%      -1 <= X <= 1,
%      -1 <= Y <= 1,
%      -1 <= Z <= 1.
%
%  Reference:
%
%    Arthur H Stroud,
%    Approximate Calculation of Multiple Integrals,
%    Prentice Hall, 1971.
%
%  Modified:
%
%    15 August 1998
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, external FUNC, the name of the user supplied
%    function which evaluates F(X,Y,Z), of the form
%
%      function func ( x, y, z )
%      doubleprecision func
%      doubleprecision x
%      doubleprecision y
%      doubleprecision z
%
%    Output, doubleprecision RESULT, the approximate integral of the function.
%
persistent quad s volume w x y z ; 

if isempty(quad), quad=0; end;
if isempty(s), s=0; end;
if isempty(volume), volume=0; end;
if isempty(w), w=0; end;
if isempty(x), x=0; end;
if isempty(y), y=0; end;
if isempty(z), z=0; end;
%
%
s = 1.0d+00 ./ sqrt( 3.0d+00 );
w = 1.0d+00 ./ 8.0d+00;
x = s;
y = s;
z = s;
quad = w .*( func(  x,  y,  z ) + func(  x,  y, -z ) +func(  x, -y,  z ) + func(  x, -y, -z ) +func( -x,  y,  z ) + func( -x,  y, -z ) +func( -x, -y,  z ) + func( -x, -y, -z ) );
[volume ]=cube_unit_volume_nd( 3 );
result = quad .* volume;
return;
end
function [func, qa, qb, n, k]=cube_unit_nd( func, qa, qb, n, k );
%
%*******************************************************************************
%
%! CUBE_UNIT_ND approximates an integral inside the unit cube in ND.
%
%
%  Discussion:
%
%    A K**N point product formula is used.
%
%  Integration region:
%
%    N dimensional points X() such that:
%
%      -1 <= X(I) <= 1 for all I.
%
%  Reference:
%
%    Lyness and McHugh,
%    Integration Over Multidimensional Hypercubes, A Progressive Procedure,
%    Computer J, volume 6, 1963, pages 264-270.
%
%    Arthur H Stroud,
%    Approximate Calculation of Multiple Integrals,
%    Prentice Hall, 1971.
%
%  Modified:
%
%    21 November 2000
%
%  Parameters:
%
%    Input, external FUNC, the name of the user supplied
%    function which evaluates the function, of the form
%
%      function func ( n, x )
%      integer n
%      doubleprecision func
%      doubleprecision x(n)
%
%    Output, doubleprecision QA(K), QB(K), two sets of estimates for
%    the integral.  The QB entries are obtained from the
%    QA entries by Richardson extrapolation, and QB(K) is
%    the best estimate for the integral.
%
%    Input, integer N, the dimension of the cube.
%
%    Input, integer K, the highest order of integration, and the order
%    of Richardson extrapolation.  K can be no greater than 10.
%
persistent g i j kmax ; 

if isempty(kmax), kmax = 10; end;
%
%
if isempty(g), g=zeros(kmax,kmax); end;
if isempty(i), i=0; end;
if isempty(j), j=0; end;
%
%
%
g([1:kmax],[1:kmax]) = 0.0d+00;
g( 1, 1) =   1.0d+00;
g( 2, 1) = - 0.3333333333333d+00;
g( 2, 2) =   0.1333333333333d+01;
g( 3, 1) =   0.4166666666667d-01;
g( 3, 2) = - 0.1066666666667d+01;
g( 3, 3) =   0.2025000000000d+01;
g( 4, 1) = - 0.2777777777778d-02;
g( 4, 2) =   0.3555555555556d+00;
g( 4, 3) = - 0.2603571428571d+01;
g( 4, 4) =   0.3250793650794d+01;
g( 5, 1) =   0.1157407407407d-03;
g( 5, 2) = - 0.6772486772487d-01;
g( 5, 3) =   0.1464508928571d+01;
g( 5, 4) = - 0.5779188712522d+01;
g( 5, 5) =   0.5382288910935d+01;
g( 6, 1) = - 0.3306878306878d-05;
g( 6, 2) =   0.8465608465608d-02;
g( 6, 3) = - 0.4881696428571d+00;
g( 6, 4) =   0.4623350970018d+01;
g( 6, 5) = - 0.1223247479758d+02;
g( 6, 6) =   0.9088831168831d+01;
g( 7, 1) =   0.6889329805996d-07;
g( 7, 2) = - 0.7524985302763d-03;
g( 7, 3) =   0.1098381696429d+00;
g( 7, 4) = - 0.2241624712736d+01;
g( 7, 5) =   0.1274216124748d+02;
g( 7, 6) = - 0.2516907092907d+02;
g( 7, 7) =   0.1555944865432d+02;
g( 8, 1) = - 0.1093544413650d-08;
g( 8, 2) =   0.5016656868509d-04;
g( 8, 3) = - 0.1797351866883d-01;
g( 8, 4) =   0.7472082375786d+00;
g( 8, 5) = - 0.8168052081717d+01;
g( 8, 6) =   0.3236023405166d+02;
g( 8, 7) = - 0.5082753227079d+02;
g( 8, 8) =   0.2690606541646d+02;
g( 9, 1) =   0.1366930517063d-10;
g( 9, 2) = - 0.2606055516108d-05;
g( 9, 3) =   0.2246689833604d-02;
g( 9, 4) = - 0.1839281815578d+00;
g( 9, 5) =   0.3646451822195d+01;
g( 9, 6) = - 0.2588818724133d+02;
g( 9, 7) =   0.7782965878964d+02;
g( 9, 8) = - 0.1012934227443d+03;
g( 9, 9) =   0.4688718347156d+02;
g(10, 1) = - 0.1380737896023d-12;
g(10, 2) =   0.1085856465045d-06;
g(10, 3) = - 0.2222000934334d-03;
g(10, 4) =   0.3503393934435d-01;
g(10, 5) = - 0.1215483940732d+01;
g(10, 6) =   0.1456210532325d+02;
g(10, 7) = - 0.7477751530769d+02;
g(10, 8) =   0.1800771959898d+03;
g(10, 9) = - 0.1998874663788d+03;
g(10,10) =   0.8220635246624d+02;
%
if( k > kmax )
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'CUBE_UNIT_ND - Fatal error!');
writef(1,['%s %0.15g \n'], '  K must be no greater than KMAX = ', kmax);
writef(1,['%s %0.15g \n'], '  but the input K is ', k);
error(['stop encountered in original fortran code  ',char(10),';']);
end;
for i = 1: k;
[ func, n, i, qa(i) ]=qmdpt( func, n, i, qa(i) );
end; i = fix(k+1);
qb(1) = qa(1);
for i = 2: k;
qb(i) = 0.0d+00;
for j = 1: i;
qb(i) = qb(i) + g(i,j) .* qa(j);
end; j = fix(i+1);
end; i = fix(k+1);
return;
end
function [cube_unit_volume_ndresult, n ]=cube_unit_volume_nd( n );
%
%*******************************************************************************
%
%! CUBE_UNIT_VOLUME_ND returns the volume of the unit cube in ND.
%
%
%  Modified:
%
%    07 November 2000
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, integer N, the dimension of the space.
%
%    Output, doubleprecision CUBE_UNIT_VOLUME_ND, the volume of the unit
%    cube in ND.
%
cube_unit_volume_ndresult=[];
persistent cube_unit_volume_nd ; 

if isempty(cube_unit_volume_ndresult), cube_unit_volume_ndresult=0; end;
%
cube_unit_volume_ndresult = 2.0d+00.^n;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',n); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [x, y]=d_swap( x, y );
%
%*******************************************************************************
%
%! D_SWAP switches two doubleprecision values.
%
%
%  Modified:
%
%    01 May 2000
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input/output, doubleprecision X, Y.  On output, the values of X and
%    Y have been interchanged.
%
persistent z ; 

if isempty(z), z=0; end;
%
z = x;
x = y;
y = z;
return;
end
function [x1, x2, x3]=d_swap3( x1, x2, x3 );
%
%*******************************************************************************
%
%! D_SWAP3 swaps three doubleprecision items.
%
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input/output, doubleprecision X1, X2, X3.
%
%    On output, the values have been shifted so that
%
%      X1 := X3;
%      X2 := X1;
%      X3 := X2;
%
persistent x0 ; 

if isempty(x0), x0=0; end;
%
x0 = x1;
x1 = x3;
x3 = x2;
x2 = x0;
return;
end
function [lda, n, ierror]=dge_check( lda, n, ierror );
%
%*******************************************************************************
%
%! DGE_CHECK checks the dimensions of a general matrix.
%
%
%  Modified:
%
%    16 October 1998
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, integer LDA, the leading dimension of the array.
%    LDA must be at least N.
%
%    Input, integer N, the order of the matrix.
%    N must be positive.
%
%    Output, integer IERROR, reports whether any errors were detected.
%    IERROR is set to 0 before the checks are made, and then:
%    IERROR = IERROR + 1 if LDA is illegal;
%    IERROR = IERROR + 2 if N is illegal.
%
%

ierror = 0;
if( lda < n )
ierror = fix(ierror + 1);
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'DGE_CHECK - Illegal LDA  < N.');
writef(1,['%s %0.15g \n'], '  LDA = ', lda);
writef(1,['%s %0.15g \n'], '  N =   ', n);
end;
if( n < 1 )
ierror = fix(ierror + 2);
writef(1,['%s \n'], ' ');
writef(1,['%s %0.15g \n'], 'DGE_CHECK - Illegal N = ', n);
end;
return;
end
function [a, lda, n, ipivot, det]=dge_det( a, lda, n, ipivot, det );
%
%*******************************************************************************
%
%! DGE_DET computes the determinant of a matrix factored by DGE_FA or DGE_TRF.
%
%
%  Modified:
%
%    19 October 1998
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, doubleprecision A(LDA,N), the LU factors computed by DGE_FA
%    or DGE_TRF.
%
%    Input, integer LDA, the leading dimension of the array.
%    LDA must be at least N.
%
%    Input, integer N, the order of the matrix.
%    N must be positive.
%
%    Input, integer IPIVOT(N), as computed by DGE_FA or DGE_TRF.
%
%    Output, doubleprecision DET, the determinant of the matrix.
%
%
persistent i ierror ; 

a_orig=a;a_shape=[lda,n];a=reshape([a_orig(1:min(prod(a_shape),numel(a_orig))),zeros(1,max(0,prod(a_shape)-numel(a_orig)))],a_shape);
if isempty(i), i=0; end;
if isempty(ierror), ierror=0; end;
%
%  Check the dimensions.
%
[ lda, n, ierror ]=dge_check( lda, n, ierror );
if( ierror ~= 0 )
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'DGE_DET - Fatal error!');
writef(1,['%s \n'], '  Illegal dimensions.');
a_orig(1:prod(a_shape))=a;a=a_orig;
return;
end;
det = 1.0d+00;
for i = 1: n;
det = det .* a(i,i);
end; i = fix(n+1);
for i = 1: n;
if( ipivot(i) ~= i )
det = - det;
end;
end; i = fix(n+1);
a_orig(1:prod(a_shape))=a;a=a_orig;
return;
end
function [a, lda, n, ipivot, info]=dge_fa( a, lda, n, ipivot, info );
%
%*******************************************************************************
%
%! DGE_FA factors a general matrix.
%
%
%  Note:
%
%    DGE_FA is a simplified version of the LINPACK routine DGEFA.
%
%  Modified:
%
%    04 March 1999
%
%  Parameters:
%
%    Input/output, doubleprecision A(LDA,N), the matrix to be factored.
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
persistent i ierror j k l t ; 

a_orig=a;a_shape=[lda,n];a=reshape([a_orig(1:min(prod(a_shape),numel(a_orig))),zeros(1,max(0,prod(a_shape)-numel(a_orig)))],a_shape);
if isempty(i), i=0; end;
if isempty(ierror), ierror=0; end;
if isempty(j), j=0; end;
if isempty(k), k=0; end;
if isempty(l), l=0; end;
if isempty(t), t=0; end;
%
%  Check the dimensions.
%
[ lda, n, ierror ]=dge_check( lda, n, ierror );
if( ierror ~= 0 )
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'DGE_FA - Fatal error!');
writef(1,['%s \n'], '  Illegal dimensions.');
a_orig(1:prod(a_shape))=a;a=a_orig;
return;
end;
info = 0;
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
if( a(l,k) == 0.0d+00 )
info = fix(k);
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'DGE_FA - Fatal error!');
writef(1,['%s %0.15g \n'], '  Zero pivot on step ', info);
a_orig(1:prod(a_shape))=a;a=a_orig;
return;
end;
%
%  Interchange rows L and K if necessary.
%
if( l ~= k )
[ a(l,k), a(k,k) ]=d_swap( a(l,k), a(k,k) );
end;
%
%  Normalize the values that lie below the pivot entry A(K,K).
%
a([k+1:n],k) = - a([k+1:n],k) ./ a(k,k);
%
%  Row elimination with column indexing.
%
for j = k+1: n;
if( l ~= k )
[ a(l,j), a(k,j) ]=d_swap( a(l,j), a(k,j) );
end;
a([k+1:n],j) = a([k+1:n],j) + a([k+1:n],k) .* a(k,j);
end; j = fix(n+1);
end; k = fix(n-1+1);
ipivot(n) = fix(n);
if( a(n,n) == 0.0d+00 )
info = fix(n);
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'SGE_FA - Fatal error!');
writef(1,['%s %0.15g \n'], '  Zero pivot on step ', info);
end;
a_orig(1:prod(a_shape))=a;a=a_orig;
return;
end
function [xlo, xhi, n, ival, xval]=dvec_even_select( xlo, xhi, n, ival, xval );
%
%*******************************************************************************
%
%! DVEC_EVEN_SELECT returns the I-th of N evenly spaced values in [ XLO, XHI ].
%
%
%  Formula:
%
%    XVAL = ( (N-IVAL) * XLO + (IVAL-1) * XHI ) / dble ( N - 1 )
%
%  Modified:
%
%    31 October 2000
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, doubleprecision XLO, XHI, the low and high values.
%
%    Input, integer N, the number of values.
%
%    Input, integer IVAL, the index of the desired point.
%    IVAL is normally between 1 and N, but may be any
%    integer value.
%
%    Output, doubleprecision XVAL, the IVAL-th of N evenly spaced values
%    between XLO and XHI.
%
%    Unless N = 1, X(1) = XLO and X(N) = XHI.
%
%    If N = 1, then X(1) = 0.5*(XLO+XHI).
%
%
%

if( n == 1 )
xval = 0.5d+00 .*( xlo + xhi );
else;
xval =( ( n - ival ) .* xlo + ( ival - 1 ) .* xhi ) ./ ( n - 1 );
end;
return;
end
function [ellipse_area_2dresult, r1, r2 ]=ellipse_area_2d( r1, r2 );
%
%*******************************************************************************
%
%! ELLIPSE_AREA_2D returns the area of an ellipse in 2D.
%
%
%  Modified:
%
%    16 November 2000
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, doubleprecision R1, R2, the major and minor semi-axes.
%
%    Output, doubleprecision ELLIPSE_AREA_2D, the area of the ellipse.
%
ellipse_area_2dresult=[];
persistent ellipse_area_2d ; 

if isempty(ellipse_area_2dresult), ellipse_area_2dresult=0; end;
%
ellipse_area_2dresult = pi( ) .* r1 .* r2;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(2)), assignin('caller','FUntemp',r2); evalin('caller',[inputname(2),'=FUntemp;']); end
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',r1); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [hexagon_area_2dresult, radius ]=hexagon_area_2d( radius );
%
%*******************************************************************************
%
%! HEXAGON_AREA_2D returns the area of a regular hexagon in 2D.
%
%
%  Modified:
%
%    16 November 2000
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, doubleprecision RADIUS, the radius of the hexagon.
%
%    Output, doubleprecision HEXAGON_AREA_2D, the area of the hexagon.
%
hexagon_area_2dresult=[];
persistent hexagon_area_2d ; 

if isempty(hexagon_area_2dresult), hexagon_area_2dresult=0; end;
%
hexagon_area_2dresult = radius.^2 .* hexagon_unit_area_2d( );
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',radius); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [func, xc, yc, radius, norder, xtab, ytab, weight,result]=hexagon_sum( func, xc, yc, radius, norder, xtab, ytab, weight,result );
%
%*******************************************************************************
%
%! HEXAGON_SUM applies a quadrature rule inside a hexagon in 2D.
%
%
%  Integration region:
%
%    The definition is given in terms of the angle in degrees of the
%    vector (X,Y)-(XC,YC):
%
%        0 and  60: Y-YC = - SQRT(3) * X-XC + RADIUS * SQRT(3)
%       60 and 120: Y-YC =                    RADIUS * SQRT(3)/2
%      120 and 180: Y-YC =   SQRT(3) * X-XC + RADIUS * SQRT(3)
%      180 and 240: Y-YC = - SQRT(3) * X-XC - RADIUS * SQRT(3)
%      240 and 300: Y-YC =                  - RADIUS * SQRT(3)/2
%      300 and 360: Y-YC =   SQRT(3) * X-XC - RADIUS * SQRT(3)
%
%  Modified:
%
%    06 November 2000
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, external FUNC, the name of the user supplied
%    function of two variables which is to be integrated,
%    of the form:
%
%      function func ( x, y )
%      doubleprecision func
%      doubleprecision x
%      doubleprecision y
%
%    Input, doubleprecision XC, YC, the coordinates of the center of the hexagon.
%
%    Input, doubleprecision RADIUS, the radius of the hexagon.
%
%    Input, integer NORDER, the order of the rule.
%
%    Input, doubleprecision XTAB(NORDER), YTAB(NORDER), the abscissas.
%
%    Input, doubleprecision WEIGHT(NORDER), the weights of the rule.
%
%    Output, doubleprecision RESULT, the approximate integral of the function.
%
%
persistent i pi quad volume x y ; 

if isempty(i), i=0; end;
if isempty(pi), pi=0; end;
if isempty(quad), quad=0; end;
if isempty(volume), volume=0; end;
if isempty(x), x=0; end;
if isempty(y), y=0; end;
%
%
quad = 0.0d+00;
for i = 1: norder;
x = xc + radius .* xtab(i);
y = yc + radius .* ytab(i);
quad = quad + weight(i) .* func( x, y );
end; i = fix(norder+1);
[volume , radius ]=hexagon_area_2d( radius );
result = quad .* volume;
return;
end
function [hexagon_unit_area_2dresult]=hexagon_unit_area_2d( );
%
%*******************************************************************************
%
%! HEXAGON_UNIT_AREA_2D returns the area of a unit regular hexagon in 2D.
%
%
%  Modified:
%
%    07 November 2000
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Output, doubleprecision HEXAGON_UNIT_AREA_2D, the area of the hexagon.
%
hexagon_unit_area_2dresult=[];
persistent hexagon_unit_area_2d rad ; 

if isempty(hexagon_unit_area_2dresult), hexagon_unit_area_2dresult=0; end;
if isempty(rad), rad=0; end;
%
hexagon_unit_area_2dresult = 3.0d+00 .* sqrt( 3.0d+00 ) ./ 2.0d+00;
return;
end
function [rule, norder, xtab, ytab, weight]=hexagon_unit_set( rule, norder, xtab, ytab, weight );
%
%*******************************************************************************
%
%! HEXAGON_UNIT_SET sets a quadrature rule inside the unit hexagon in 2D.
%
%
%  Integration region:
%
%    The definition is given in terms of the angle in degrees of the
%    vector (X,Y):
%
%        0 and  60: Y = - SQRT(3) * X + SQRT(3)
%       60 and 120: Y =                 SQRT(3)/2
%      120 and 180: Y =   SQRT(3) * X + SQRT(3)
%      180 and 240: Y = - SQRT(3) * X - SQRT(3)
%      240 and 300: Y =               - SQRT(3)/2
%      300 and 360: Y =   SQRT(3) * X - SQRT(3)
%
%    or, the convex hull of
%
%      (1,0), (0.5,A), (-0.5,A), (-1,0), (-0.5,-A), (0.5,A)
%
%    where A = SQRT(3)/2.
%
%  Reference:
%
%    Abramowitz and Stegun,
%    Handbook of Mathematical Functions,
%    National Bureau of Standards, 1964.
%
%    Arthur H Stroud,
%    Approximate Calculation of Multiple Integrals,
%    Prentice Hall, 1971.
%
%  Modified:
%
%    09 December 2000
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, integer RULE, the rule desired.
%      1, 1 point,  degree 1;
%      2, 4 points, degree 3;
%      3, 7 points, degree 3;
%      4, 7 points, degree 5;
%
%    Output, integer NORDER, the order of the desired rule.
%    If RULE is not legal, then NORDER is returned as 0.
%
%    Output, doubleprecision XTAB(*), YTAB(*), the abscissas of the rule.
%
%    Output, doubleprecision WEIGHT(*), the NORDER weights of the rule.
%
persistent a b c d e z ; 

if isempty(a), a=0; end;
if isempty(b), b=0; end;
if isempty(c), c=0; end;
if isempty(d), d=0; end;
if isempty(e), e=0; end;
weight_shape=size(weight);weight=reshape(weight,1,[]);
xtab_shape=size(xtab);xtab=reshape(xtab,1,[]);
ytab_shape=size(ytab);ytab=reshape(ytab,1,[]);
if isempty(z), z=0; end;
%
if( rule == 1 )
norder = 1;
xtab(1) = 0.0d+00;
ytab(1) = 0.0d+00;
weight(1) = 1.0d+00;
%
%  Stroud rule H2:3-1.
%
elseif( rule == 2 ) ;
a = sqrt( 5.0d+00 ./ 12.0d+00 );
b = 1.0d+00 ./ 4.0d+00;
z = 0.0d+00;
norder = 4;
xtab([1:4]) =[  a, -a,  z,  z ];
ytab([1:4]) =[  z,  z,  a, -a ];
weight([1:4]) =[  b,  b,  b,  b ];
%
%  Stroud rule H2:3-2.
%
elseif( rule == 3 ) ;
a = sqrt( 3.0d+00 ) ./ 2.0d+00;
b =  0.5d+00;
c =  1.0d+00;
d =  5.0d+00 ./ 72.0d+00;
e = 42.0d+00 ./ 72.0d+00;
z =  0.0d+00;
norder = 7;
xtab([1:7]) =[  z,  c, -c,  b, -b,  b, -b ];
ytab([1:7]) =[  z,  z,  z,  a,  a, -a, -a ];
weight([1:7]) =[  e,  d,  d,  d,  d,  d,  d ];
%
%  Stroud rule H2:5-1.
%
elseif( rule == 4 ) ;
a = sqrt( 14.0d+00 ) ./ 5.0d+00;
b = sqrt( 14.0d+00 ) ./ 10.0d+00;
c = sqrt( 42.0d+00 ) ./ 10.0d+00;
d = 125.0d+00 ./ 1008.0d+00;
e = 258.0d+00 ./ 1008.0d+00;
z = 0.0d+00;
norder = 7;
xtab([1:7]) =[ z,  a, -a,  b, -b,  b, -b ];
ytab([1:7]) =[ z,  z,  z,  c,  c, -c, -c ];
weight([1:7]) =[ e,  d,  d,  d,  d,  d,  d ];
else;
norder = 0;
end;
weight_shape=zeros(weight_shape);weight_shape(:)=weight(1:numel(weight_shape));weight=weight_shape;
xtab_shape=zeros(xtab_shape);xtab_shape(:)=xtab(1:numel(xtab_shape));xtab=xtab_shape;
ytab_shape=zeros(ytab_shape);ytab_shape(:)=ytab(1:numel(ytab_shape));ytab=ytab_shape;
return;
end
function [n, k, iarray, in, iout]=ksub_next2( n, k, iarray, in, iout );
%
%*******************************************************************************
%
%! KSUB_NEXT2 computes the next K subset of an N set.
%
%
%  Discussion:
%
%    This routine uses the revolving door method.  It has no 'memory'.
%    It simply calculates the successor of the input set,
%    and will start from the beginning after the last set.
%
%  Reference:
%
%    A Nijenhuis and H Wilf,
%    Combinatorial Algorithms,
%    Academic Press, 1978, second edition,
%    ISBN 0-12-519260-6.
%
%  Modified:
%
%    15 April 1999
%
%  Parameters:
%
%    Input, integer N, the size of the set from which subsets are drawn.
%
%    Input, integer K, the size of the desired subset.  K must be
%    between 0 and N.
%
%    Input/output, integer IARRAY(K).  On input, the user must
%    supply a subset of size K in IARRAY.  That is, IARRAY must
%    contain K unique numbers, in order, between 1 and N.  On
%    output, IARRAY(I) is the I-th element of the output subset.
%    The output array is also in sorted order.
%
%    Output, integer IN, the element of the output subset which
%    was not in the input set.  Each new subset differs from the
%    last one by adding one element and deleting another.
%
%    Output, integer IOUT, the element of the input subset which
%    is not in the output subset.
%
%
persistent j m ; 

if isempty(j), j=0; end;
if isempty(m), m=0; end;
%
if( k < 0 || k > n )
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'KSUB_NEXT2 - Fatal error!');
writef(1,['%s %0.15g %s %0.15g \n'], '  N = ', n, ' and K = ', k);
writef(1,['%s \n'], '  but 0 <= K <= N is required!');
error(['stop encountered in original fortran code  ',char(10),';']);
end;
j = 0;
if( rem( k, 2 ) ~= 0 )
go to 20;
end;
% 10    continue;
j = fix(j + 1);
if( j > k )
iarray(k) = fix(k);
in = fix(k);
iout = fix(n);
return;
end;
if( iarray(j) ~= j )
iout = fix(iarray(j));
in = fix(iout - 1);
iarray(j) = fix(in);
if( j ~= 1 )
in = fix(j - 1);
iarray(j-1) = fix(in);
end;
return;
end;
% 20    continue;
j = fix(j + 1);
m = fix(n);
if( j < k )
m = fix(iarray(j+1) - 1);
end;
if( m == iarray(j) )
go to 10;
end;
in = fix(iarray(j) + 1);
iarray(j) = fix(in);
iout = fix(in - 1);
if( j ~= 1 )
iarray(j-1) = fix(iout);
iout = fix(j - 1);
end;
return;
end
function [norder, xtab, weight]=legendre_set( norder, xtab, weight );
%
%*******************************************************************************
%
%! LEGENDRE_SET sets abscissas and weights for Gauss-Legendre quadrature.
%
%
%  Integration interval:
%
%    [ -1, 1 ]
%
%  Weight function:
%
%    1.0D+00
%
%  Integral to approximate:
%
%    INTEGRAL ( -1 <= X <= 1 ) F(X) dX
%
%  Approximate integral:
%
%    SUM ( I = 1 to NORDER ) WEIGHT(I) * F ( XTAB(I) )
%
%  Precision:
%
%    The quadrature rule will integrate exactly all polynomials up to
%    X**(2*NORDER-1).
%
%  Note:
%
%    The abscissas of the rule are the zeroes of the Legendre polynomial
%    P(NORDER,X).
%
%    The integral produced by a Gauss-Legendre rule is equal to the
%    integral of the unique polynomial of degree NORDER-1 which
%    agrees with the function at the NORDER abscissas of the rule.
%
%  Reference:
%
%    Abramowitz and Stegun,
%    Handbook of Mathematical Functions,
%    National Bureau of Standards, 1964.
%
%    Vladimir Krylov,
%    Approximate Calculation of Integrals,
%    MacMillan, 1962.
%
%    Arthur Stroud and Don Secrest,
%    Gaussian Quadrature Formulas,
%    Prentice Hall, 1966.
%
%    Daniel Zwillinger, editor,
%    Standard Mathematical Tables and Formulae,
%    30th Edition,
%    CRC Press, 1996.
%
%  Modified:
%
%    18 December 2000
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, integer NORDER, the order of the rule.
%    NORDER must be between 1 and 20, 32 or 64.
%
%    Output, doubleprecision XTAB(NORDER), the abscissas of the rule.
%
%    Output, doubleprecision WEIGHT(NORDER), the weights of the rule.
%    The weights are positive, symmetric and should sum to 2.
%
%
%

if( norder == 1 )
xtab(1) =   0.0d+00;
weight(1) = 2.0d+00;
elseif( norder == 2 ) ;
xtab(1) = - 0.577350269189625764509148780502d+00;
xtab(2) =   0.577350269189625764509148780502d+00;
weight(1) = 1.0d+00;
weight(2) = 1.0d+00;
elseif( norder == 3 ) ;
xtab(1) = - 0.774596669241483377035853079956d+00;
xtab(2) =   0.0d+00;
xtab(3) =   0.774596669241483377035853079956d+00;
weight(1) = 5.0d+00 ./ 9.0d+00;
weight(2) = 8.0d+00 ./ 9.0d+00;
weight(3) = 5.0d+00 ./ 9.0d+00;
elseif( norder == 4 ) ;
xtab(1) = - 0.861136311594052575223946488893d+00;
xtab(2) = - 0.339981043584856264802665759103d+00;
xtab(3) =   0.339981043584856264802665759103d+00;
xtab(4) =   0.861136311594052575223946488893d+00;
weight(1) = 0.347854845137453857373063949222d+00;
weight(2) = 0.652145154862546142626936050778d+00;
weight(3) = 0.652145154862546142626936050778d+00;
weight(4) = 0.347854845137453857373063949222d+00;
elseif( norder == 5 ) ;
xtab(1) = - 0.906179845938663992797626878299d+00;
xtab(2) = - 0.538469310105683091036314420700d+00;
xtab(3) =   0.0d+00;
xtab(4) =   0.538469310105683091036314420700d+00;
xtab(5) =   0.906179845938663992797626878299d+00;
weight(1) = 0.236926885056189087514264040720d+00;
weight(2) = 0.478628670499366468041291514836d+00;
weight(3) = 0.568888888888888888888888888889d+00;
weight(4) = 0.478628670499366468041291514836d+00;
weight(5) = 0.236926885056189087514264040720d+00;
elseif( norder == 6 ) ;
xtab(1) = - 0.932469514203152027812301554494d+00;
xtab(2) = - 0.661209386466264513661399595020d+00;
xtab(3) = - 0.238619186083196908630501721681d+00;
xtab(4) =   0.238619186083196908630501721681d+00;
xtab(5) =   0.661209386466264513661399595020d+00;
xtab(6) =   0.932469514203152027812301554494d+00;
weight(1) = 0.171324492379170345040296142173d+00;
weight(2) = 0.360761573048138607569833513838d+00;
weight(3) = 0.467913934572691047389870343990d+00;
weight(4) = 0.467913934572691047389870343990d+00;
weight(5) = 0.360761573048138607569833513838d+00;
weight(6) = 0.171324492379170345040296142173d+00;
elseif( norder == 7 ) ;
xtab(1) = - 0.949107912342758524526189684048d+00;
xtab(2) = - 0.741531185599394439863864773281d+00;
xtab(3) = - 0.405845151377397166906606412077d+00;
xtab(4) =   0.0d+00;
xtab(5) =   0.405845151377397166906606412077d+00;
xtab(6) =   0.741531185599394439863864773281d+00;
xtab(7) =   0.949107912342758524526189684048d+00;
weight(1) = 0.129484966168869693270611432679d+00;
weight(2) = 0.279705391489276667901467771424d+00;
weight(3) = 0.381830050505118944950369775489d+00;
weight(4) = 0.417959183673469387755102040816d+00;
weight(5) = 0.381830050505118944950369775489d+00;
weight(6) = 0.279705391489276667901467771424d+00;
weight(7) = 0.129484966168869693270611432679d+00;
elseif( norder == 8 ) ;
xtab(1) = - 0.960289856497536231683560868569d+00;
xtab(2) = - 0.796666477413626739591553936476d+00;
xtab(3) = - 0.525532409916328985817739049189d+00;
xtab(4) = - 0.183434642495649804939476142360d+00;
xtab(5) =   0.183434642495649804939476142360d+00;
xtab(6) =   0.525532409916328985817739049189d+00;
xtab(7) =   0.796666477413626739591553936476d+00;
xtab(8) =   0.960289856497536231683560868569d+00;
weight(1) = 0.101228536290376259152531354310d+00;
weight(2) = 0.222381034453374470544355994426d+00;
weight(3) = 0.313706645877887287337962201987d+00;
weight(4) = 0.362683783378361982965150449277d+00;
weight(5) = 0.362683783378361982965150449277d+00;
weight(6) = 0.313706645877887287337962201987d+00;
weight(7) = 0.222381034453374470544355994426d+00;
weight(8) = 0.101228536290376259152531354310d+00;
elseif( norder == 9 ) ;
xtab(1) = - 0.968160239507626089835576202904d+00;
xtab(2) = - 0.836031107326635794299429788070d+00;
xtab(3) = - 0.613371432700590397308702039341d+00;
xtab(4) = - 0.324253423403808929038538014643d+00;
xtab(5) =   0.0d+00;
xtab(6) =   0.324253423403808929038538014643d+00;
xtab(7) =   0.613371432700590397308702039341d+00;
xtab(8) =   0.836031107326635794299429788070d+00;
xtab(9) =   0.968160239507626089835576202904d+00;
weight(1) = 0.812743883615744119718921581105d-01;
weight(2) = 0.180648160694857404058472031243d+00;
weight(3) = 0.260610696402935462318742869419d+00;
weight(4) = 0.312347077040002840068630406584d+00;
weight(5) = 0.330239355001259763164525069287d+00;
weight(6) = 0.312347077040002840068630406584d+00;
weight(7) = 0.260610696402935462318742869419d+00;
weight(8) = 0.180648160694857404058472031243d+00;
weight(9) = 0.812743883615744119718921581105d-01;
elseif( norder == 10 ) ;
xtab(1) =  - 0.973906528517171720077964012084d+00;
xtab(2) =  - 0.865063366688984510732096688423d+00;
xtab(3) =  - 0.679409568299024406234327365115d+00;
xtab(4) =  - 0.433395394129247190799265943166d+00;
xtab(5) =  - 0.148874338981631210884826001130d+00;
xtab(6) =    0.148874338981631210884826001130d+00;
xtab(7) =    0.433395394129247190799265943166d+00;
xtab(8) =    0.679409568299024406234327365115d+00;
xtab(9) =    0.865063366688984510732096688423d+00;
xtab(10) =   0.973906528517171720077964012084d+00;
weight(1) =  0.666713443086881375935688098933d-01;
weight(2) =  0.149451349150580593145776339658d+00;
weight(3) =  0.219086362515982043995534934228d+00;
weight(4) =  0.269266719309996355091226921569d+00;
weight(5) =  0.295524224714752870173892994651d+00;
weight(6) =  0.295524224714752870173892994651d+00;
weight(7) =  0.269266719309996355091226921569d+00;
weight(8) =  0.219086362515982043995534934228d+00;
weight(9) =  0.149451349150580593145776339658d+00;
weight(10) = 0.666713443086881375935688098933d-01;
elseif( norder == 11 ) ;
xtab(1) =  - 0.978228658146056992803938001123d+00;
xtab(2) =  - 0.887062599768095299075157769304d+00;
xtab(3) =  - 0.730152005574049324093416252031d+00;
xtab(4) =  - 0.519096129206811815925725669459d+00;
xtab(5) =  - 0.269543155952344972331531985401d+00;
xtab(6) =    0.0d+00;
xtab(7) =    0.269543155952344972331531985401d+00;
xtab(8) =    0.519096129206811815925725669459d+00;
xtab(9) =    0.730152005574049324093416252031d+00;
xtab(10) =   0.887062599768095299075157769304d+00;
xtab(11) =   0.978228658146056992803938001123d+00;
weight(1) =  0.556685671161736664827537204425d-01;
weight(2) =  0.125580369464904624634694299224d+00;
weight(3) =  0.186290210927734251426097641432d+00;
weight(4) =  0.233193764591990479918523704843d+00;
weight(5) =  0.262804544510246662180688869891d+00;
weight(6) =  0.272925086777900630714483528336d+00;
weight(7) =  0.262804544510246662180688869891d+00;
weight(8) =  0.233193764591990479918523704843d+00;
weight(9) =  0.186290210927734251426097641432d+00;
weight(10) = 0.125580369464904624634694299224d+00;
weight(11) = 0.556685671161736664827537204425d-01;
elseif( norder == 12 ) ;
xtab(1) =  - 0.981560634246719250690549090149d+00;
xtab(2) =  - 0.904117256370474856678465866119d+00;
xtab(3) =  - 0.769902674194304687036893833213d+00;
xtab(4) =  - 0.587317954286617447296702418941d+00;
xtab(5) =  - 0.367831498998180193752691536644d+00;
xtab(6) =  - 0.125233408511468915472441369464d+00;
xtab(7) =    0.125233408511468915472441369464d+00;
xtab(8) =    0.367831498998180193752691536644d+00;
xtab(9) =    0.587317954286617447296702418941d+00;
xtab(10) =   0.769902674194304687036893833213d+00;
xtab(11) =   0.904117256370474856678465866119d+00;
xtab(12) =   0.981560634246719250690549090149d+00;
weight(1) =  0.471753363865118271946159614850d-01;
weight(2) =  0.106939325995318430960254718194d+00;
weight(3) =  0.160078328543346226334652529543d+00;
weight(4) =  0.203167426723065921749064455810d+00;
weight(5) =  0.233492536538354808760849898925d+00;
weight(6) =  0.249147045813402785000562436043d+00;
weight(7) =  0.249147045813402785000562436043d+00;
weight(8) =  0.233492536538354808760849898925d+00;
weight(9) =  0.203167426723065921749064455810d+00;
weight(10) = 0.160078328543346226334652529543d+00;
weight(11) = 0.106939325995318430960254718194d+00;
weight(12) = 0.471753363865118271946159614850d-01;
elseif( norder == 13 ) ;
xtab(1) =  - 0.984183054718588149472829448807d+00;
xtab(2) =  - 0.917598399222977965206547836501d+00;
xtab(3) =  - 0.801578090733309912794206489583d+00;
xtab(4) =  - 0.642349339440340220643984606996d+00;
xtab(5) =  - 0.448492751036446852877912852128d+00;
xtab(6) =  - 0.230458315955134794065528121098d+00;
xtab(7) =    0.0d+00;
xtab(8) =    0.230458315955134794065528121098d+00;
xtab(9) =    0.448492751036446852877912852128d+00;
xtab(10) =   0.642349339440340220643984606996d+00;
xtab(11) =   0.801578090733309912794206489583d+00;
xtab(12) =   0.917598399222977965206547836501d+00;
xtab(13) =   0.984183054718588149472829448807d+00;
weight(1) =  0.404840047653158795200215922010d-01;
weight(2) =  0.921214998377284479144217759538d-01;
weight(3) =  0.138873510219787238463601776869d+00;
weight(4) =  0.178145980761945738280046691996d+00;
weight(5) =  0.207816047536888502312523219306d+00;
weight(6) =  0.226283180262897238412090186040d+00;
weight(7) =  0.232551553230873910194589515269d+00;
weight(8) =  0.226283180262897238412090186040d+00;
weight(9) =  0.207816047536888502312523219306d+00;
weight(10) = 0.178145980761945738280046691996d+00;
weight(11) = 0.138873510219787238463601776869d+00;
weight(12) = 0.921214998377284479144217759538d-01;
weight(13) = 0.404840047653158795200215922010d-01;
elseif( norder == 14 ) ;
xtab(1) =  - 0.986283808696812338841597266704d+00;
xtab(2) =  - 0.928434883663573517336391139378d+00;
xtab(3) =  - 0.827201315069764993189794742650d+00;
xtab(4) =  - 0.687292904811685470148019803019d+00;
xtab(5) =  - 0.515248636358154091965290718551d+00;
xtab(6) =  - 0.319112368927889760435671824168d+00;
xtab(7) =  - 0.108054948707343662066244650220d+00;
xtab(8) =    0.108054948707343662066244650220d+00;
xtab(9) =    0.319112368927889760435671824168d+00;
xtab(10) =   0.515248636358154091965290718551d+00;
xtab(11) =   0.687292904811685470148019803019d+00;
xtab(12) =   0.827201315069764993189794742650d+00;
xtab(13) =   0.928434883663573517336391139378d+00;
xtab(14) =   0.986283808696812338841597266704d+00;
weight(1) =  0.351194603317518630318328761382d-01;
weight(2) =  0.801580871597602098056332770629d-01;
weight(3) =  0.121518570687903184689414809072d+00;
weight(4) =  0.157203167158193534569601938624d+00;
weight(5) =  0.185538397477937813741716590125d+00;
weight(6) =  0.205198463721295603965924065661d+00;
weight(7) =  0.215263853463157790195876443316d+00;
weight(8) =  0.215263853463157790195876443316d+00;
weight(9) =  0.205198463721295603965924065661d+00;
weight(10) = 0.185538397477937813741716590125d+00;
weight(11) = 0.157203167158193534569601938624d+00;
weight(12) = 0.121518570687903184689414809072d+00;
weight(13) = 0.801580871597602098056332770629d-01;
weight(14) = 0.351194603317518630318328761382d-01;
elseif( norder == 15 ) ;
xtab(1) =  - 0.987992518020485428489565718587d+00;
xtab(2) =  - 0.937273392400705904307758947710d+00;
xtab(3) =  - 0.848206583410427216200648320774d+00;
xtab(4) =  - 0.724417731360170047416186054614d+00;
xtab(5) =  - 0.570972172608538847537226737254d+00;
xtab(6) =  - 0.394151347077563369897207370981d+00;
xtab(7) =  - 0.201194093997434522300628303395d+00;
xtab(8) =    0.0d+00;
xtab(9) =    0.201194093997434522300628303395d+00;
xtab(10) =   0.394151347077563369897207370981d+00;
xtab(11) =   0.570972172608538847537226737254d+00;
xtab(12) =   0.724417731360170047416186054614d+00;
xtab(13) =   0.848206583410427216200648320774d+00;
xtab(14) =   0.937273392400705904307758947710d+00;
xtab(15) =   0.987992518020485428489565718587d+00;
weight(1) =  0.307532419961172683546283935772d-01;
weight(2) =  0.703660474881081247092674164507d-01;
weight(3) =  0.107159220467171935011869546686d+00;
weight(4) =  0.139570677926154314447804794511d+00;
weight(5) =  0.166269205816993933553200860481d+00;
weight(6) =  0.186161000015562211026800561866d+00;
weight(7) =  0.198431485327111576456118326444d+00;
weight(8) =  0.202578241925561272880620199968d+00;
weight(9) =  0.198431485327111576456118326444d+00;
weight(10) = 0.186161000015562211026800561866d+00;
weight(11) = 0.166269205816993933553200860481d+00;
weight(12) = 0.139570677926154314447804794511d+00;
weight(13) = 0.107159220467171935011869546686d+00;
weight(14) = 0.703660474881081247092674164507d-01;
weight(15) = 0.307532419961172683546283935772d-01;
elseif( norder == 16 ) ;
xtab(1) =  - 0.989400934991649932596154173450d+00;
xtab(2) =  - 0.944575023073232576077988415535d+00;
xtab(3) =  - 0.865631202387831743880467897712d+00;
xtab(4) =  - 0.755404408355003033895101194847d+00;
xtab(5) =  - 0.617876244402643748446671764049d+00;
xtab(6) =  - 0.458016777657227386342419442984d+00;
xtab(7) =  - 0.281603550779258913230460501460d+00;
xtab(8) =  - 0.950125098376374401853193354250d-01;
xtab(9) =    0.950125098376374401853193354250d-01;
xtab(10) =   0.281603550779258913230460501460d+00;
xtab(11) =   0.458016777657227386342419442984d+00;
xtab(12) =   0.617876244402643748446671764049d+00;
xtab(13) =   0.755404408355003033895101194847d+00;
xtab(14) =   0.865631202387831743880467897712d+00;
xtab(15) =   0.944575023073232576077988415535d+00;
xtab(16) =   0.989400934991649932596154173450d+00;
weight(1) =  0.271524594117540948517805724560d-01;
weight(2) =  0.622535239386478928628438369944d-01;
weight(3) =  0.951585116824927848099251076022d-01;
weight(4) =  0.124628971255533872052476282192d+00;
weight(5) =  0.149595988816576732081501730547d+00;
weight(6) =  0.169156519395002538189312079030d+00;
weight(7) =  0.182603415044923588866763667969d+00;
weight(8) =  0.189450610455068496285396723208d+00;
weight(9) =  0.189450610455068496285396723208d+00;
weight(10) = 0.182603415044923588866763667969d+00;
weight(11) = 0.169156519395002538189312079030d+00;
weight(12) = 0.149595988816576732081501730547d+00;
weight(13) = 0.124628971255533872052476282192d+00;
weight(14) = 0.951585116824927848099251076022d-01;
weight(15) = 0.622535239386478928628438369944d-01;
weight(16) = 0.271524594117540948517805724560d-01;
elseif( norder == 17 ) ;
xtab(1) =  - 0.990575475314417335675434019941d+00;
xtab(2) =  - 0.950675521768767761222716957896d+00;
xtab(3) =  - 0.880239153726985902122955694488d+00;
xtab(4) =  - 0.781514003896801406925230055520d+00;
xtab(5) =  - 0.657671159216690765850302216643d+00;
xtab(6) =  - 0.512690537086476967886246568630d+00;
xtab(7) =  - 0.351231763453876315297185517095d+00;
xtab(8) =  - 0.178484181495847855850677493654d+00;
xtab(9) =    0.0d+00;
xtab(10) =   0.178484181495847855850677493654d+00;
xtab(11) =   0.351231763453876315297185517095d+00;
xtab(12) =   0.512690537086476967886246568630d+00;
xtab(13) =   0.657671159216690765850302216643d+00;
xtab(14) =   0.781514003896801406925230055520d+00;
xtab(15) =   0.880239153726985902122955694488d+00;
xtab(16) =   0.950675521768767761222716957896d+00;
xtab(17) =   0.990575475314417335675434019941d+00;
weight(1) =  0.241483028685479319601100262876d-01;
weight(2) =  0.554595293739872011294401653582d-01;
weight(3) =  0.850361483171791808835353701911d-01;
weight(4) =  0.111883847193403971094788385626d+00;
weight(5) =  0.135136368468525473286319981702d+00;
weight(6) =  0.154045761076810288081431594802d+00;
weight(7) =  0.168004102156450044509970663788d+00;
weight(8) =  0.176562705366992646325270990113d+00;
weight(9) =  0.179446470356206525458265644262d+00;
weight(10) = 0.176562705366992646325270990113d+00;
weight(11) = 0.168004102156450044509970663788d+00;
weight(12) = 0.154045761076810288081431594802d+00;
weight(13) = 0.135136368468525473286319981702d+00;
weight(14) = 0.111883847193403971094788385626d+00;
weight(15) = 0.850361483171791808835353701911d-01;
weight(16) = 0.554595293739872011294401653582d-01;
weight(17) = 0.241483028685479319601100262876d-01;
elseif( norder == 18 ) ;
xtab(1) =  - 0.991565168420930946730016004706d+00;
xtab(2) =  - 0.955823949571397755181195892930d+00;
xtab(3) =  - 0.892602466497555739206060591127d+00;
xtab(4) =  - 0.803704958972523115682417455015d+00;
xtab(5) =  - 0.691687043060353207874891081289d+00;
xtab(6) =  - 0.559770831073947534607871548525d+00;
xtab(7) =  - 0.411751161462842646035931793833d+00;
xtab(8) =  - 0.251886225691505509588972854878d+00;
xtab(9) =  - 0.847750130417353012422618529358d-01;
xtab(10) =   0.847750130417353012422618529358d-01;
xtab(11) =   0.251886225691505509588972854878d+00;
xtab(12) =   0.411751161462842646035931793833d+00;
xtab(13) =   0.559770831073947534607871548525d+00;
xtab(14) =   0.691687043060353207874891081289d+00;
xtab(15) =   0.803704958972523115682417455015d+00;
xtab(16) =   0.892602466497555739206060591127d+00;
xtab(17) =   0.955823949571397755181195892930d+00;
xtab(18) =   0.991565168420930946730016004706d+00;
weight(1) =  0.216160135264833103133427102665d-01;
weight(2) =  0.497145488949697964533349462026d-01;
weight(3) =  0.764257302548890565291296776166d-01;
weight(4) =  0.100942044106287165562813984925d+00;
weight(5) =  0.122555206711478460184519126800d+00;
weight(6) =  0.140642914670650651204731303752d+00;
weight(7) =  0.154684675126265244925418003836d+00;
weight(8) =  0.164276483745832722986053776466d+00;
weight(9) =  0.169142382963143591840656470135d+00;
weight(10) = 0.169142382963143591840656470135d+00;
weight(11) = 0.164276483745832722986053776466d+00;
weight(12) = 0.154684675126265244925418003836d+00;
weight(13) = 0.140642914670650651204731303752d+00;
weight(14) = 0.122555206711478460184519126800d+00;
weight(15) = 0.100942044106287165562813984925d+00;
weight(16) = 0.764257302548890565291296776166d-01;
weight(17) = 0.497145488949697964533349462026d-01;
weight(18) = 0.216160135264833103133427102665d-01;
elseif( norder == 19 ) ;
xtab(1) =  - 0.992406843843584403189017670253d+00;
xtab(2) =  - 0.960208152134830030852778840688d+00;
xtab(3) =  - 0.903155903614817901642660928532d+00;
xtab(4) =  - 0.822714656537142824978922486713d+00;
xtab(5) =  - 0.720966177335229378617095860824d+00;
xtab(6) =  - 0.600545304661681023469638164946d+00;
xtab(7) =  - 0.464570741375960945717267148104d+00;
xtab(8) =  - 0.316564099963629831990117328850d+00;
xtab(9) =  - 0.160358645640225375868096115741d+00;
xtab(10) =   0.0d+00;
xtab(11) =   0.160358645640225375868096115741d+00;
xtab(12) =   0.316564099963629831990117328850d+00;
xtab(13) =   0.464570741375960945717267148104d+00;
xtab(14) =   0.600545304661681023469638164946d+00;
xtab(15) =   0.720966177335229378617095860824d+00;
xtab(16) =   0.822714656537142824978922486713d+00;
xtab(17) =   0.903155903614817901642660928532d+00;
xtab(18) =   0.960208152134830030852778840688d+00;
xtab(19) =   0.992406843843584403189017670253d+00;
weight(1) =  0.194617882297264770363120414644d-01;
weight(2) =  0.448142267656996003328381574020d-01;
weight(3) =  0.690445427376412265807082580060d-01;
weight(4) =  0.914900216224499994644620941238d-01;
weight(5) =  0.111566645547333994716023901682d+00;
weight(6) =  0.128753962539336227675515784857d+00;
weight(7) =  0.142606702173606611775746109442d+00;
weight(8) =  0.152766042065859666778855400898d+00;
weight(9) =  0.158968843393954347649956439465d+00;
weight(10) = 0.161054449848783695979163625321d+00;
weight(11) = 0.158968843393954347649956439465d+00;
weight(12) = 0.152766042065859666778855400898d+00;
weight(13) = 0.142606702173606611775746109442d+00;
weight(14) = 0.128753962539336227675515784857d+00;
weight(15) = 0.111566645547333994716023901682d+00;
weight(16) = 0.914900216224499994644620941238d-01;
weight(17) = 0.690445427376412265807082580060d-01;
weight(18) = 0.448142267656996003328381574020d-01;
weight(19) = 0.194617882297264770363120414644d-01;
elseif( norder == 20 ) ;
xtab(1) =  - 0.993128599185094924786122388471d+00;
xtab(2) =  - 0.963971927277913791267666131197d+00;
xtab(3) =  - 0.912234428251325905867752441203d+00;
xtab(4) =  - 0.839116971822218823394529061702d+00;
xtab(5) =  - 0.746331906460150792614305070356d+00;
xtab(6) =  - 0.636053680726515025452836696226d+00;
xtab(7) =  - 0.510867001950827098004364050955d+00;
xtab(8) =  - 0.373706088715419560672548177025d+00;
xtab(9) =  - 0.227785851141645078080496195369d+00;
xtab(10) = - 0.765265211334973337546404093988d-01;
xtab(11) =   0.765265211334973337546404093988d-01;
xtab(12) =   0.227785851141645078080496195369d+00;
xtab(13) =   0.373706088715419560672548177025d+00;
xtab(14) =   0.510867001950827098004364050955d+00;
xtab(15) =   0.636053680726515025452836696226d+00;
xtab(16) =   0.746331906460150792614305070356d+00;
xtab(17) =   0.839116971822218823394529061702d+00;
xtab(18) =   0.912234428251325905867752441203d+00;
xtab(19) =   0.963971927277913791267666131197d+00;
xtab(20) =   0.993128599185094924786122388471d+00;
weight(1) =  0.176140071391521183118619623519d-01;
weight(2) =  0.406014298003869413310399522749d-01;
weight(3) =  0.626720483341090635695065351870d-01;
weight(4) =  0.832767415767047487247581432220d-01;
weight(5) =  0.101930119817240435036750135480d+00;
weight(6) =  0.118194531961518417312377377711d+00;
weight(7) =  0.131688638449176626898494499748d+00;
weight(8) =  0.142096109318382051329298325067d+00;
weight(9) =  0.149172986472603746787828737002d+00;
weight(10) = 0.152753387130725850698084331955d+00;
weight(11) = 0.152753387130725850698084331955d+00;
weight(12) = 0.149172986472603746787828737002d+00;
weight(13) = 0.142096109318382051329298325067d+00;
weight(14) = 0.131688638449176626898494499748d+00;
weight(15) = 0.118194531961518417312377377711d+00;
weight(16) = 0.101930119817240435036750135480d+00;
weight(17) = 0.832767415767047487247581432220d-01;
weight(18) = 0.626720483341090635695065351870d-01;
weight(19) = 0.406014298003869413310399522749d-01;
weight(20) = 0.176140071391521183118619623519d-01;
elseif( norder == 32 ) ;
xtab(1) =  - 0.997263861849481563544981128665d+00;
xtab(2) =  - 0.985611511545268335400175044631d+00;
xtab(3) =  - 0.964762255587506430773811928118d+00;
xtab(4) =  - 0.934906075937739689170919134835d+00;
xtab(5) =  - 0.896321155766052123965307243719d+00;
xtab(6) =  - 0.849367613732569970133693004968d+00;
xtab(7) =  - 0.794483795967942406963097298970d+00;
xtab(8) =  - 0.732182118740289680387426665091d+00;
xtab(9) =  - 0.663044266930215200975115168663d+00;
xtab(10) = - 0.587715757240762329040745476402d+00;
xtab(11) = - 0.506899908932229390023747474378d+00;
xtab(12) = - 0.421351276130635345364119436172d+00;
xtab(13) = - 0.331868602282127649779916805730d+00;
xtab(14) = - 0.239287362252137074544603209166d+00;
xtab(15) = - 0.144471961582796493485186373599d+00;
xtab(16) = - 0.483076656877383162348125704405d-01;
xtab(17) =   0.483076656877383162348125704405d-01;
xtab(18) =   0.144471961582796493485186373599d+00;
xtab(19) =   0.239287362252137074544603209166d+00;
xtab(20) =   0.331868602282127649779916805730d+00;
xtab(21) =   0.421351276130635345364119436172d+00;
xtab(22) =   0.506899908932229390023747474378d+00;
xtab(23) =   0.587715757240762329040745476402d+00;
xtab(24) =   0.663044266930215200975115168663d+00;
xtab(25) =   0.732182118740289680387426665091d+00;
xtab(26) =   0.794483795967942406963097298970d+00;
xtab(27) =   0.849367613732569970133693004968d+00;
xtab(28) =   0.896321155766052123965307243719d+00;
xtab(29) =   0.934906075937739689170919134835d+00;
xtab(30) =   0.964762255587506430773811928118d+00;
xtab(31) =   0.985611511545268335400175044631d+00;
xtab(32) =   0.997263861849481563544981128665d+00;
weight(1) =  0.701861000947009660040706373885d-02;
weight(2) =  0.162743947309056706051705622064d-01;
weight(3) =  0.253920653092620594557525897892d-01;
weight(4) =  0.342738629130214331026877322524d-01;
weight(5) =  0.428358980222266806568786466061d-01;
weight(6) =  0.509980592623761761961632446895d-01;
weight(7) =  0.586840934785355471452836373002d-01;
weight(8) =  0.658222227763618468376500637069d-01;
weight(9) =  0.723457941088485062253993564785d-01;
weight(10) = 0.781938957870703064717409188283d-01;
weight(11) = 0.833119242269467552221990746043d-01;
weight(12) = 0.876520930044038111427714627518d-01;
weight(13) = 0.911738786957638847128685771116d-01;
weight(14) = 0.938443990808045656391802376681d-01;
weight(15) = 0.956387200792748594190820022041d-01;
weight(16) = 0.965400885147278005667648300636d-01;
weight(17) = 0.965400885147278005667648300636d-01;
weight(18) = 0.956387200792748594190820022041d-01;
weight(19) = 0.938443990808045656391802376681d-01;
weight(20) = 0.911738786957638847128685771116d-01;
weight(21) = 0.876520930044038111427714627518d-01;
weight(22) = 0.833119242269467552221990746043d-01;
weight(23) = 0.781938957870703064717409188283d-01;
weight(24) = 0.723457941088485062253993564785d-01;
weight(25) = 0.658222227763618468376500637069d-01;
weight(26) = 0.586840934785355471452836373002d-01;
weight(27) = 0.509980592623761761961632446895d-01;
weight(28) = 0.428358980222266806568786466061d-01;
weight(29) = 0.342738629130214331026877322524d-01;
weight(30) = 0.253920653092620594557525897892d-01;
weight(31) = 0.162743947309056706051705622064d-01;
weight(32) = 0.701861000947009660040706373885d-02;
elseif( norder == 64 ) ;
xtab(1) =  - 0.999305041735772139456905624346d+00;
xtab(2) =  - 0.996340116771955279346924500676d+00;
xtab(3) =  - 0.991013371476744320739382383443d+00;
xtab(4) =  - 0.983336253884625956931299302157d+00;
xtab(5) =  - 0.973326827789910963741853507352d+00;
xtab(6) =  - 0.961008799652053718918614121897d+00;
xtab(7) =  - 0.946411374858402816062481491347d+00;
xtab(8) =  - 0.929569172131939575821490154559d+00;
xtab(9) =  - 0.910522137078502805756380668008d+00;
xtab(10) = - 0.889315445995114105853404038273d+00;
xtab(11) = - 0.865999398154092819760783385070d+00;
xtab(12) = - 0.840629296252580362751691544696d+00;
xtab(13) = - 0.813265315122797559741923338086d+00;
xtab(14) = - 0.783972358943341407610220525214d+00;
xtab(15) = - 0.752819907260531896611863774886d+00;
xtab(16) = - 0.719881850171610826848940217832d+00;
xtab(17) = - 0.685236313054233242563558371031d+00;
xtab(18) = - 0.648965471254657339857761231993d+00;
xtab(19) = - 0.611155355172393250248852971019d+00;
xtab(20) = - 0.571895646202634034283878116659d+00;
xtab(21) = - 0.531279464019894545658013903544d+00;
xtab(22) = - 0.489403145707052957478526307022d+00;
xtab(23) = - 0.446366017253464087984947714759d+00;
xtab(24) = - 0.402270157963991603695766771260d+00;
xtab(25) = - 0.357220158337668115950442615046d+00;
xtab(26) = - 0.311322871990210956157512698560d+00;
xtab(27) = - 0.264687162208767416373964172510d+00;
xtab(28) = - 0.217423643740007084149648748989d+00;
xtab(29) = - 0.169644420423992818037313629748d+00;
xtab(30) = - 0.121462819296120554470376463492d+00;
xtab(31) = - 0.729931217877990394495429419403d-01;
xtab(32) = - 0.243502926634244325089558428537d-01;
xtab(33) =   0.243502926634244325089558428537d-01;
xtab(34) =   0.729931217877990394495429419403d-01;
xtab(35) =   0.121462819296120554470376463492d+00;
xtab(36) =   0.169644420423992818037313629748d+00;
xtab(37) =   0.217423643740007084149648748989d+00;
xtab(38) =   0.264687162208767416373964172510d+00;
xtab(39) =   0.311322871990210956157512698560d+00;
xtab(40) =   0.357220158337668115950442615046d+00;
xtab(41) =   0.402270157963991603695766771260d+00;
xtab(42) =   0.446366017253464087984947714759d+00;
xtab(43) =   0.489403145707052957478526307022d+00;
xtab(44) =   0.531279464019894545658013903544d+00;
xtab(45) =   0.571895646202634034283878116659d+00;
xtab(46) =   0.611155355172393250248852971019d+00;
xtab(47) =   0.648965471254657339857761231993d+00;
xtab(48) =   0.685236313054233242563558371031d+00;
xtab(49) =   0.719881850171610826848940217832d+00;
xtab(50) =   0.752819907260531896611863774886d+00;
xtab(51) =   0.783972358943341407610220525214d+00;
xtab(52) =   0.813265315122797559741923338086d+00;
xtab(53) =   0.840629296252580362751691544696d+00;
xtab(54) =   0.865999398154092819760783385070d+00;
xtab(55) =   0.889315445995114105853404038273d+00;
xtab(56) =   0.910522137078502805756380668008d+00;
xtab(57) =   0.929569172131939575821490154559d+00;
xtab(58) =   0.946411374858402816062481491347d+00;
xtab(59) =   0.961008799652053718918614121897d+00;
xtab(60) =   0.973326827789910963741853507352d+00;
xtab(61) =   0.983336253884625956931299302157d+00;
xtab(62) =   0.991013371476744320739382383443d+00;
xtab(63) =   0.996340116771955279346924500676d+00;
xtab(64) =   0.999305041735772139456905624346d+00;
weight(1) =  0.178328072169643294729607914497d-02;
weight(2) =  0.414703326056246763528753572855d-02;
weight(3) =  0.650445796897836285611736039998d-02;
weight(4) =  0.884675982636394772303091465973d-02;
weight(5) =  0.111681394601311288185904930192d-01;
weight(6) =  0.134630478967186425980607666860d-01;
weight(7) =  0.157260304760247193219659952975d-01;
weight(8) =  0.179517157756973430850453020011d-01;
weight(9) =  0.201348231535302093723403167285d-01;
weight(10) = 0.222701738083832541592983303842d-01;
weight(11) = 0.243527025687108733381775504091d-01;
weight(12) = 0.263774697150546586716917926252d-01;
weight(13) = 0.283396726142594832275113052002d-01;
weight(14) = 0.302346570724024788679740598195d-01;
weight(15) = 0.320579283548515535854675043479d-01;
weight(16) = 0.338051618371416093915654821107d-01;
weight(17) = 0.354722132568823838106931467152d-01;
weight(18) = 0.370551285402400460404151018096d-01;
weight(19) = 0.385501531786156291289624969468d-01;
weight(20) = 0.399537411327203413866569261283d-01;
weight(21) = 0.412625632426235286101562974736d-01;
weight(22) = 0.424735151236535890073397679088d-01;
weight(23) = 0.435837245293234533768278609737d-01;
weight(24) = 0.445905581637565630601347100309d-01;
weight(25) = 0.454916279274181444797709969713d-01;
weight(26) = 0.462847965813144172959532492323d-01;
weight(27) = 0.469681828162100173253262857546d-01;
weight(28) = 0.475401657148303086622822069442d-01;
weight(29) = 0.479993885964583077281261798713d-01;
weight(30) = 0.483447622348029571697695271580d-01;
weight(31) = 0.485754674415034269347990667840d-01;
weight(32) = 0.486909570091397203833653907347d-01;
weight(33) = 0.486909570091397203833653907347d-01;
weight(34) = 0.485754674415034269347990667840d-01;
weight(35) = 0.483447622348029571697695271580d-01;
weight(36) = 0.479993885964583077281261798713d-01;
weight(37) = 0.475401657148303086622822069442d-01;
weight(38) = 0.469681828162100173253262857546d-01;
weight(39) = 0.462847965813144172959532492323d-01;
weight(40) = 0.454916279274181444797709969713d-01;
weight(41) = 0.445905581637565630601347100309d-01;
weight(42) = 0.435837245293234533768278609737d-01;
weight(43) = 0.424735151236535890073397679088d-01;
weight(44) = 0.412625632426235286101562974736d-01;
weight(45) = 0.399537411327203413866569261283d-01;
weight(46) = 0.385501531786156291289624969468d-01;
weight(47) = 0.370551285402400460404151018096d-01;
weight(48) = 0.354722132568823838106931467152d-01;
weight(49) = 0.338051618371416093915654821107d-01;
weight(50) = 0.320579283548515535854675043479d-01;
weight(51) = 0.302346570724024788679740598195d-01;
weight(52) = 0.283396726142594832275113052002d-01;
weight(53) = 0.263774697150546586716917926252d-01;
weight(54) = 0.243527025687108733381775504091d-01;
weight(55) = 0.222701738083832541592983303842d-01;
weight(56) = 0.201348231535302093723403167285d-01;
weight(57) = 0.179517157756973430850453020011d-01;
weight(58) = 0.157260304760247193219659952975d-01;
weight(59) = 0.134630478967186425980607666860d-01;
weight(60) = 0.111681394601311288185904930192d-01;
weight(61) = 0.884675982636394772303091465973d-02;
weight(62) = 0.650445796897836285611736039998d-02;
weight(63) = 0.414703326056246763528753572855d-02;
weight(64) = 0.178328072169643294729607914497d-02;
else;
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'LEGENDRE_SET - Fatal error!');
writef(1,['%s %0.15g \n'], '  Illegal value of NORDER = ', norder);
writef(1,['%s \n'], '  Legal values are 1 to 20, 32 or 64.');
error(['stop encountered in original fortran code  ',char(10),';']);
end;
return;
end
function [norder, xtab, weight]=legendre_set_x1( norder, xtab, weight );
%
%*******************************************************************************
%
%! LEGENDRE_SET_X1 sets a Gauss-Legendre rule for ( 1 + X ) * F(X) on [-1,1].
%
%
%  Integration interval:
%
%    [ -1, 1 ]
%
%  Weight function:
%
%    1 + X
%
%  Integral to approximate:
%
%    INTEGRAL ( -1 <= X <= 1 ) ( 1 + X ) * F(X) dX
%
%  Approximate integral:
%
%    SUM ( I = 1 to NORDER ) WEIGHT(I) * F ( XTAB(I) )
%
%  Reference:
%
%    Arthur Stroud and Don Secrest,
%    Gaussian Quadrature Formulas,
%    Prentice Hall, 1966, Table #3.
%
%  Modified:
%
%    18 December 2000
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, integer NORDER, the order of the rule.
%    NORDER must be between 1 and 9.
%
%    Output, doubleprecision XTAB(NORDER), the abscissas of the rule.
%
%    Output, doubleprecision WEIGHT(NORDER), the weights of the rule.
%
%
%

if( norder == 1 )
xtab(1) =  0.333333333333333333333333333333d+00;
weight(1) = 2.0d+00;
elseif( norder == 2 ) ;
xtab(1) = -0.289897948556635619639456814941d+00;
xtab(2) =  0.689897948556635619639456814941d+00;
weight(1) =  0.727834473024091322422523991699d+00;
weight(2) =  1.27216552697590867757747600830d+00;
elseif( norder == 3 ) ;
xtab(1) = -0.575318923521694112050483779752d+00;
xtab(2) =  0.181066271118530578270147495862d+00;
xtab(3) =  0.822824080974592105208907712461d+00;
weight(1) =  0.279307919605816490135525088716d+00;
weight(2) =  0.916964425438344986775682378225d+00;
weight(3) =  0.803727654955838523088792533058d+00;
elseif( norder == 4 ) ;
xtab(1) = -0.720480271312438895695825837750d+00;
xtab(2) = -0.167180864737833640113395337326d+00;
xtab(3) =  0.446313972723752344639908004629d+00;
xtab(4) =  0.885791607770964635613757614892d+00;
weight(1) =  0.124723883800032328695500588386d+00;
weight(2) =  0.519390190432929763305824811559d+00;
weight(3) =  0.813858272041085443165617903743d+00;
weight(4) =  0.542027653725952464833056696312d+00;
elseif( norder == 5 ) ;
xtab(1) = -0.802929828402347147753002204224d+00;
xtab(2) = -0.390928546707272189029229647442d+00;
xtab(3) =  0.124050379505227711989974959990d+00;
xtab(4) =  0.603973164252783654928415726409d+00;
xtab(5) =  0.920380285897062515318386619813d+00;
weight(1) =  0.0629916580867691047411692662740d+00;
weight(2) =  0.295635480290466681402532877367d+00;
weight(3) =  0.585547948338679234792151477424d+00;
weight(4) =  0.668698552377478261966702492391d+00;
weight(5) =  0.387126360906606717097443886545d+00;
elseif( norder == 6 ) ;
xtab(1) = -0.853891342639482229703747931639d+00;
xtab(2) = -0.538467724060109001833766720231d+00;
xtab(3) = -0.117343037543100264162786683611d+00;
xtab(4) =  0.326030619437691401805894055838d+00;
xtab(5) =  0.703842800663031416300046295008d+00;
xtab(6) =  0.941367145680430216055899446174d+00;
weight(1) =  0.0349532072544381270240692132496d+00;
weight(2) =  0.175820662202035902032706497222d+00;
weight(3) =  0.394644603562621056482338042193d+00;
weight(4) =  0.563170215152795712476307356284d+00;
weight(5) =  0.542169988926074467362761586552d+00;
weight(6) =  0.289241322902034734621817304499d+00;
elseif( norder == 7 ) ;
xtab(1) = -0.887474878926155707068695617935d+00;
xtab(2) = -0.639518616526215270024840114382d+00;
xtab(3) = -0.294750565773660725252184459658d+00;
xtab(4) =  0.0943072526611107660028971153047d+00;
xtab(5) =  0.468420354430821063046421216613d+00;
xtab(6) =  0.770641893678191536180719525865d+00;
xtab(7) =  0.955041227122575003782349000858d+00;
weight(1) =  0.0208574488112296163587654972151d+00;
weight(2) =  0.109633426887493901777324193433d+00;
weight(3) =  0.265538785861965879934591955055d+00;
weight(4) =  0.428500262783494679963649011999d+00;
weight(5) =  0.509563589198353307674937943100d+00;
weight(6) =  0.442037032763498409684482945478d+00;
weight(7) =  0.223869453693964204606248453720d+00;
elseif( norder == 8 ) ;
xtab(1) = -0.910732089420060298533757956283d+00;
xtab(2) = -0.711267485915708857029562959544d+00;
xtab(3) = -0.426350485711138962102627520502d+00;
xtab(4) = -0.0903733696068532980645444599064d+00;
xtab(5) =  0.256135670833455395138292079035d+00;
xtab(6) =  0.571383041208738483284917464837d+00;
xtab(7) =  0.817352784200412087992517083851d+00;
xtab(8) =  0.964440169705273096373589797925d+00;
weight(1) =  0.0131807657689951954189692640444d+00;
weight(2) =  0.0713716106239448335742111888042d+00;
weight(3) =  0.181757278018795592332221684383d+00;
weight(4) =  0.316798397969276640481632757440d+00;
weight(5) =  0.424189437743720042818124385645d+00;
weight(6) =  0.450023197883549464687088394417d+00;
weight(7) =  0.364476094545494505382889847132d+00;
weight(8) =  0.178203217446223725304862478136d+00;
elseif( norder == 9 ) ;
xtab(1) = -0.927484374233581078117671398464d+00;
xtab(2) = -0.763842042420002599615429776011d+00;
xtab(3) = -0.525646030370079229365386614293d+00;
xtab(4) = -0.236234469390588049278459503207d+00;
xtab(5) =  0.0760591978379781302337137826389d+00;
xtab(6) =  0.380664840144724365880759065541d+00;
xtab(7) =  0.647766687674009436273648507855d+00;
xtab(8) =  0.851225220581607910728163628088d+00;
xtab(9) =  0.971175180702246902734346518378d+00;
weight(1) =  0.00872338834309252349019620448007d+00;
weight(2) =  0.0482400171391415162069086091476d+00;
weight(3) =  0.127219285964216005046760427743d+00;
weight(4) =  0.233604781180660442262926091607d+00;
weight(5) =  0.337433287379681397577000079834d+00;
weight(6) =  0.401235236773473158616600898930d+00;
weight(7) =  0.394134968689382820640692081477d+00;
weight(8) =  0.304297020437232650320317215016d+00;
weight(9) =  0.145112014093119485838598391765d+00;
else;
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'LEGENDRE_SET_X1 - Fatal error!');
writef(1,['%s %0.15g \n'], '  Illegal input value of NORDER = ', norder);
error(['stop encountered in original fortran code  ',char(10),';']);
end;
return;
end
function [norder, xtab, weight]=legendre_set_x2( norder, xtab, weight );
%
%*******************************************************************************
%
%! LEGENDRE_SET_X2 sets a Gauss-Legendre rule for ( 1 + X )**2 * F(X) on [-1,1].
%
%
%  Integration interval:
%
%    [ -1, 1 ]
%
%  Weight function:
%
%    ( 1 + X )**2
%
%  Integral to approximate:
%
%    INTEGRAL ( -1 <= X <= 1 ) ( 1 + X )**2 * F(X) dX
%
%  Approximate integral:
%
%    SUM ( I = 1 to NORDER ) WEIGHT(I) * F ( XTAB(I) )
%
%  Reference:
%
%    Arthur Stroud and Don Secrest,
%    Gaussian Quadrature Formulas,
%    Prentice Hall, 1966, Table #3.
%
%  Modified:
%
%    18 December 2000
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, integer NORDER, the order of the rule.
%    NORDER must be between 1 and 9.
%
%    Output, doubleprecision XTAB(NORDER), the abscissas of the rule.
%
%    Output, doubleprecision WEIGHT(NORDER), the weights of the rule.
%
%
%

if( norder == 1 )
xtab(1) =  0.5d+00;
weight(1) =  2.66666666666666666666666666666d+00;
elseif( norder == 2 ) ;
xtab(1) = -0.0883036880224505775998524725910d+00;
xtab(2) =  0.754970354689117244266519139258d+00;
weight(1) =  0.806287056638603444666851075928d+00;
weight(2) =  1.86037961002806322199981559074d+00;
elseif( norder == 3 ) ;
xtab(1) = -0.410004419776996766244796955168d+00;
xtab(2) =  0.305992467923296230556472913192d+00;
xtab(3) =  0.854011951853700535688324041976d+00;
weight(1) =  0.239605624068645584091811926047d+00;
weight(2) =  1.16997015407892817602809616291d+00;
weight(3) =  1.25709088851909290654675857771d+00;
elseif( norder == 4 ) ;
xtab(1) = -0.591702835793545726606755921586d+00;
xtab(2) = -0.0340945902087350046811467387661d+00;
xtab(3) =  0.522798524896275389882037174551d+00;
xtab(4) =  0.902998901106005341405865485802d+00;
weight(1) =  0.0828179259993445222751812523731d+00;
weight(2) =  0.549071097383384602539010760334d+00;
weight(3) =  1.14767031839371367238662411421d+00;
weight(4) =  0.887107324890223869465850539752d+00;
elseif( norder == 5 ) ;
xtab(1) = -0.702108425894032836232448374820d+00;
xtab(2) = -0.268666945261773544694327777841d+00;
xtab(3) =  0.220227225868961343518209179230d+00;
xtab(4) =  0.653039358456608553790815164028d+00;
xtab(5) =  0.930842120163569816951085142737d+00;
weight(1) =  0.0329106016247920636689299329544d+00;
weight(2) =  0.256444805783695354037991444453d+00;
weight(3) =  0.713601289772720001490035944563d+00;
weight(4) =  1.00959169519929190423066348132d+00;
weight(5) =  0.654118274286167343239045863379d+00;
elseif( norder == 6 ) ;
xtab(1) = -0.773611232355123732602532012021d+00;
xtab(2) = -0.431362254623427837535325249187d+00;
xtab(3) = -0.0180728263295041680220798103354d+00;
xtab(4) =  0.395126163954217534500188844163d+00;
xtab(5) =  0.736872116684029732026178298518d+00;
xtab(6) =  0.948190889812665614490712786006d+00;
weight(1) =  0.0146486064549543818622276447204d+00;
weight(2) =  0.125762377479560410622810097040d+00;
weight(3) =  0.410316569036929681761034600615d+00;
weight(4) =  0.756617493988329628546336413760d+00;
weight(5) =  0.859011997894245060846045458784d+00;
weight(6) =  0.500309621812647503028212451747d+00;
elseif( norder == 7 ) ;
xtab(1) = -0.822366333126005527278634734418d+00;
xtab(2) = -0.547034493182875002223997992852d+00;
xtab(3) = -0.200043026557985860387937545780d+00;
xtab(4) =  0.171995710805880507163425502299d+00;
xtab(5) =  0.518891747903884926692601716998d+00;
xtab(6) =  0.793821941703901970495546427988d+00;
xtab(7) =  0.959734452453198985538996625765d+00;
weight(1) =  0.00714150426951365443207221475404d+00;
weight(2) =  0.0653034050584375560578544725498d+00;
weight(3) =  0.235377690316228918725962815880d+00;
weight(4) =  0.505171029671130381676271523850d+00;
weight(5) =  0.733870426238362032891332767175d+00;
weight(6) =  0.725590596901489156295739839779d+00;
weight(7) =  0.394212014211504966587433032679d+00;
elseif( norder == 8 ) ;
xtab(1) = -0.857017929919813794402037235698d+00;
xtab(2) = -0.631543407166567521509503573952d+00;
xtab(3) = -0.339104543648722903660229021109d+00;
xtab(4) = -0.0111941563689783438801237300122d+00;
xtab(5) =  0.316696017045595559454075475675d+00;
xtab(6) =  0.609049663022520165351466780939d+00;
xtab(7) =  0.834198765028697794599267293239d+00;
xtab(8) =  0.967804480896157932935972899807d+00;
weight(1) =  0.00374814227227757804631954025851d+00;
weight(2) =  0.0357961737041152639660521680263d+00;
weight(3) =  0.137974910241879862433949246199d+00;
weight(4) =  0.326515411108352185491692769217d+00;
weight(5) =  0.547577467373226177976217604887d+00;
weight(6) =  0.682278153375510121675529810121d+00;
weight(7) =  0.614544746137780998436053880546d+00;
weight(8) =  0.318231662453524478640851647411d+00;
elseif( norder == 9 ) ;
xtab(1) = -0.882491728426548422828684254270d+00;
xtab(2) = -0.694873684026474640346360850039d+00;
xtab(3) = -0.446537143480670863635920316400d+00;
xtab(4) = -0.159388112702326252531544826624d+00;
xtab(5) =  0.141092709224374414981503995427d+00;
xtab(6) =  0.428217823321559204544020866175d+00;
xtab(7) =  0.676480966471850715860378175342d+00;
xtab(8) =  0.863830940812464825046988286026d+00;
xtab(9) =  0.973668228805771018909618924364d+00;
weight(1) =  0.00209009877215570354392734918986d+00;
weight(2) =  0.0205951891648697848186537272448d+00;
weight(3) =  0.0832489326348178964194106978875d+00;
weight(4) =  0.210746247220398685903797568021d+00;
weight(5) =  0.388325022916052063676224499399d+00;
weight(6) =  0.554275165518437673725822282791d+00;
weight(7) =  0.621388553284444032628761363828d+00;
weight(8) =  0.523916296267173054255512857631d+00;
weight(9) =  0.262081160888317771694556320674d+00;
else;
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'LEGENDRE_SET_X2 - Fatal error!');
writef(1,['%s %0.15g \n'], '  Illegal input value of NORDER = ', norder);
error(['stop encountered in original fortran code  ',char(10),';']);
end;
return;
end
function [n_factorialresult, n ]=n_factorial( n );
%
%*******************************************************************************
%
%! N_FACTORIAL computes N! (for small values of N).
%
%
%  Modified:
%
%    08 December 2000
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, integer N, the argument.
%
%    Output, integer N_FACTORIAL, the value of N!.
%
n_factorialresult=[];
persistent i n_factorial ; 

if isempty(i), i=0; end;
if isempty(n_factorialresult), n_factorialresult=0; end;
%
n_factorialresult = 1;
for i = 1: n;
n_factorialresult = fix(n_factorialresult .* i);
end; i = fix(n+1);
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',n); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [func, n, result]=octahedron_unit_nd( func, n, result );
%
%*******************************************************************************
%
%! OCTAHEDRON_UNIT_ND approximates integrals in a unit octahedron in ND.
%
%
%  Discussion:
%
%    A 2*N point 3rd degree formula is used, Stroud number GN:3-1.
%
%  Integration region:
%
%    N dimensional points X() such that:
%
%      SUM ( I = 1 to N ) ABS ( X(I) ) <= 1.
%
%  Reference:
%
%    Arthur H Stroud,
%    Approximate Calculation of Multiple Integrals,
%    Prentice Hall, 1971.
%
%  Modified:
%
%    21 November 2000
%
%  Parameters:
%
%    Input, external FUNC, the name of the user supplied
%    function of which is to be integrated, of the form:
%
%      function func ( n, x )
%      integer n
%      doubleprecision func
%      doubleprecision x(n)
%
%    Input, integer N, the dimension of the octahedron.
%
%    Output, doubleprecision RESULT, the approximate integral of the function.
%
%
persistent i j quad r volume w x ; 

if isempty(i), i=0; end;
if isempty(j), j=0; end;
if isempty(quad), quad=0; end;
if isempty(r), r=0; end;
if isempty(volume), volume=0; end;
if isempty(w), w=0; end;
if isempty(x), x=zeros(1,n); end;
%
%
w = 1.0d+00 ./ ( 2 .* n );
r = sqrt( fix(( 2 .* n ) ./ (( n + 1 ) .*( n + 2 ) )) );
x([1:n]) = 0.0d+00;
quad = 0.0d+00;
for i = 1: n;
x(i) = r;
for j = 1: 2;
quad = quad + w .* func( n, x );
x(i) = - x(i);
end; j = fix(2+1);
x(i) = 0.0d+00;
end; i = fix(n+1);
[volume , n ]=octahedron_unit_volume_nd( n );
result = quad .* volume;
return;
end
function [octahedron_unit_volume_ndresult, n ]=octahedron_unit_volume_nd( n );
%
%*******************************************************************************
%
%! OCTAHEDRON_UNIT_VOLUME_ND returns the volume of the unit octahedron in ND.
%
%
%  Modified:
%
%    06 November 2000
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, integer N, the dimension of the space.
%
%    Output, doubleprecision OCTAHEDRON_UNIT_VOLUME_ND, the volume of
%    the unit octahedron.
%
octahedron_unit_volume_ndresult=[];
persistent i octahedron_unit_volume_nd volume ; 

if isempty(i), i=0; end;
if isempty(octahedron_unit_volume_ndresult), octahedron_unit_volume_ndresult=0; end;
if isempty(volume), volume=0; end;
%
volume = 1.0d+00;
for i = 1: n;
volume = volume .* 2.0d+00 ./ ( i );
end; i = fix(n+1);
octahedron_unit_volume_ndresult = volume;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',n); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [parallelipiped_volume_3dresult, x, y, z ]=parallelipiped_volume_3d( x, y, z );
%
%*******************************************************************************
%
%! PARALLELIPIPED_VOLUME_3D returns the volume of a parallelipiped in 3D.
%
%
%  Modified:
%
%    09 December 2000
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, doubleprecision X(4), Y(4), Z(4), the coordinates of one corner
%    of the parallelipiped, and its 3 immediate neighbors.
%
%    Output, doubleprecision PARALLELIPIPED_VOLUME_3D, the volume of
%    the parallelipiped.
%
parallelipiped_volume_3dresult=[];
persistent parallelipiped_volume_3d ; 

if isempty(parallelipiped_volume_3dresult), parallelipiped_volume_3dresult=0; end;
%
parallelipiped_volume_3dresult = abs(( z(2) - z(1) ) .*( y(4) .* x(3) - y(3) .* x(4) ) +( z(3) - z(1) ) .*( x(4) .* y(2) - x(2) .* y(4) ) +( z(4) - z(1) ) .*( x(2) .* y(3) - x(3) .* y(2) ) +( z(3) - z(2) ) .*( y(4) .* x(1) - y(1) .* x(4) ) +( z(4) - z(2) ) .*( x(3) .* y(1) - x(1) .* y(3) ) +( z(4) - z(3) ) .*( x(1) .* y(2) - x(2) .* y(1) ) );
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(3)), assignin('caller','FUntemp',z); evalin('caller',[inputname(3),'=FUntemp;']); end
if csnil&&~isempty(inputname(2)), assignin('caller','FUntemp',y); evalin('caller',[inputname(2),'=FUntemp;']); end
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',x); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [parallelipiped_volume_ndresult, lda, n, v ]=parallelipiped_volume_nd( lda, n, v );
%
%*******************************************************************************
%
%! PARALLELIPIPED_VOLUME_ND returns the volume of a parallelipiped in ND.
%
%
%  Modified:
%
%    09 December 2000
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, integer LDA, the leading dimension of the array V.
%    LDA must be at least N.
%
%    Input, integer N, the dimension of the space.
%
%    Input/output, doubleprecision V(LDA,N+1).  On input, each of the
%    N+1 rows of V contains the N coordinates of one of the
%    'corners' of the parallelipiped in entries 1 through N, with
%    the last column being left free.
%    On output, V has been overwritten.
%
%    Output, doubleprecision PARALLELIPIPED_VOLUME_ND, the volume of
%    the parallelipiped.
%
%
parallelipiped_volume_ndresult=[];
persistent det det1 info parallelipiped_volume_nd pivot ; 

if isempty(det), det=zeros(1,2); end;
if isempty(det1), det1=0; end;
if isempty(info), info=0; end;
if isempty(parallelipiped_volume_ndresult), parallelipiped_volume_ndresult=0; end;
if isempty(pivot), pivot=zeros(1,n+1); end;
v_orig=v;v_shape=[lda,n+1];v=reshape([v_orig(1:min(prod(v_shape),numel(v_orig))),zeros(1,max(0,prod(v_shape)-numel(v_orig)))],v_shape);
%
%  Compute the volume of the N-dimensional parallelipiped.
%
v([1:n+1],n+1) = 1.0d+00;
[ v, lda,dumvar3, pivot, info ]=dge_fa( v, lda, n+1, pivot, info );
if( info ~= 0 )
parallelipiped_volume_ndresult = 0.0d+00;
v_orig(1:prod(v_shape))=v;v=v_orig;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(3)), assignin('caller','FUntemp',v); evalin('caller',[inputname(3),'=FUntemp;']); end
if csnil&&~isempty(inputname(2)), assignin('caller','FUntemp',n); evalin('caller',[inputname(2),'=FUntemp;']); end
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',lda); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end;
[ v, lda,dumvar3, pivot, det1 ]=dge_det( v, lda, n+1, pivot, det1 );
det(1)=det1;
parallelipiped_volume_ndresult = abs( det(1) ) .* 10.0d+00.^det(2);
v_orig(1:prod(v_shape))=v;v=v_orig;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(3)), assignin('caller','FUntemp',v); evalin('caller',[inputname(3),'=FUntemp;']); end
if csnil&&~isempty(inputname(2)), assignin('caller','FUntemp',n); evalin('caller',[inputname(2),'=FUntemp;']); end
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',lda); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [piresult]=pi( );
%
%*******************************************************************************
%
%! PI returns the value of pi.
%
%
%  Modified:
%
%    04 December 1998
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Output, doubleprecision PI, the value of pi.
%
piresult=[];
persistent pi ; 

if isempty(piresult), piresult=0; end;
%
piresult = 3.14159265358979323846264338327950288419716939937510d+00;
return;
end
function [func, result]=pyramid_unit_3d( func, result );
%
%*******************************************************************************
%
%! PYRAMID_UNIT_3D approximates an integral inside a unit pyramid in 3D.
%
%
%  Integration Region:
%
%    Z - 1 <= X <= 1 - Z
%    Z - 1 <= Y <= 1 - Z
%    0 <= Z <= 1.
%
%  Discussion:
%
%    An 48 point degree 7 formula, Stroud CN:C2:7-1, is used.
%
%  Reference:
%
%    Arthur H Stroud,
%    Approximate Calculation of Multiple Integrals,
%    Prentice Hall, 1971, page 339.
%
%  Modified:
%
%    22 November 2000
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, external FUNC, the name of the user supplied function which
%    evaluates F(X,Y,Z), of the form
%
%      function func ( x, y, z )
%      doubleprecision func
%      doubleprecision x
%      doubleprecision y
%      doubleprecision z
%
%    Output, doubleprecision RESULT, the approximate integral of the function.
%
persistent a b c h i pi quad r u volume w1 w2 x y z ; 

if isempty(a), a=0; end;
if isempty(b), b=0; end;
if isempty(c), c=0; end;
if isempty(h), h=0; end;
if isempty(i), i=0; end;
if isempty(pi), pi=0; end;
if isempty(quad), quad=0; end;
if isempty(r), r=0; end;
if isempty(u), u([1:4]) =[ 0.04850054945d+00, 0.2386007376d+00, 0.5170472951d+00,  0.7958514179d+00 ]; end;
if isempty(volume), volume=0; end;
if isempty(w1), w1([1:4]) =[ 0.1108884156d+00,  0.1434587878d+00, 0.06863388717d+00, 0.01035224075d+00 ]; end;
if isempty(w2), w2=zeros(1,3); end;
if isempty(x), x=0; end;
if isempty(y), y=0; end;
if isempty(z), z=0; end;
%
%
a = sqrt( 6.0d+00 ) ./ 7.0d+00;
b = sqrt(( 114.0d+00 - 3.0d+00 .* sqrt( 583.0d+00 ) ) ./ 287.0d+00 );
c = sqrt(( 114.0d+00 + 3.0d+00 .* sqrt( 583.0d+00 ) ) ./ 287.0d+00 );
w2([1:3]) =[49.0d+00 ./ 270.0d+00,( 178981.0d+00 + 2769.0d+00 .* sqrt( 583.0d+00 ) ) ./ 629640.0d+00,( 178981.0d+00 - 2769.0d+00 .* sqrt( 583.0d+00 ) ) ./ 629640.0d+00 ];
quad = 0.0d+00;
for i = 1: 4;
x = a .*( 1.0d+00 - u(i) );
y = 0.0d+00;
z = u(i);
quad = quad + w1(i) .* w2(1) .* func( x, y, z );
x = - a .*( 1.0d+00 - u(i) );
y = 0.0d+00;
z = u(i);
quad = quad + w1(i) .* w2(1) .* func( x, y, z );
x = 0.0d+00;
y = a .*( 1.0d+00 - u(i) );
z = u(i);
quad = quad + w1(i) .* w2(1) .* func( x, y, z );
x = 0.0d+00;
y = - a .*( 1.0d+00 - u(i) );
z = u(i);
quad = quad + w1(i) .* w2(1) .* func( x, y, z );
end; i = fix(4+1);
for i = 1: 4;
x =   b .*( 1.0d+00 - u(i) );
y =   b .*( 1.0d+00 - u(i) );
z =   u(i);
quad = quad + w1(i) .* w2(2) .* func( x, y, z );
x = - b .*( 1.0d+00 - u(i) );
y =   b .*( 1.0d+00 - u(i) );
z =   u(i);
quad = quad + w1(i) .* w2(2) .* func( x, y, z );
x = - b .*( 1.0d+00 - u(i) );
y = - b .*( 1.0d+00 - u(i) );
z =   u(i);
quad = quad + w1(i) .* w2(2) .* func( x, y, z );
x =   b .*( 1.0d+00 - u(i) );
y = - b .*( 1.0d+00 - u(i) );
z =   u(i);
quad = quad + w1(i) .* w2(2) .* func( x, y, z );
x =   c .*( 1.0d+00 - u(i) );
y =   c .*( 1.0d+00 - u(i) );
z =   u(i);
quad = quad + w1(i) .* w2(3) .* func( x, y, z );
x = - c .*( 1.0d+00 - u(i) );
y =   c .*( 1.0d+00 - u(i) );
z =   u(i);
quad = quad + w1(i) .* w2(3) .* func( x, y, z );
x = - c .*( 1.0d+00 - u(i) );
y = - c .*( 1.0d+00 - u(i) );
z =   u(i);
quad = quad + w1(i) .* w2(3) .* func( x, y, z );
x =   c .*( 1.0d+00 - u(i) );
y = - c .*( 1.0d+00 - u(i) );
z =   u(i);
quad = quad + w1(i) .* w2(3) .* func( x, y, z );
end; i = fix(4+1);
r = 1.0d+00;
h = 1.0d+00;
[volume , r, h ]=pyramid_volume_3d( r, h );
result = quad .* volume;
return;
end
function [pyramid_volume_3dresult, r, h ]=pyramid_volume_3d( r, h );
%
%*******************************************************************************
%
%! PYRAMID_VOLUME_3D returns the volume of a pyramid with square base in 3D.
%
%
%  Discussion:
%
%    A pyramid with square base can be regarded as the upper half of a
%    3D octahedron.
%
%  Modified:
%
%    16 November 2000
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, doubleprecision R, the 'radius' of the pyramid, that is, half the
%    length of one of the sides of the square base.
%
%    Input, doubleprecision H, the height of the pyramid.
%
%    Output, doubleprecision PYRAMID_VOLUME_3D, the volume of the pyramid.
%
pyramid_volume_3dresult=[];
persistent pyramid_volume_3d ; 

if isempty(pyramid_volume_3dresult), pyramid_volume_3dresult=0; end;
%
pyramid_volume_3dresult =( 4.0d+00 ./ 3.0d+00 ) .* h .* r.^2;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',r); evalin('caller',[inputname(1),'=FUntemp;']); end
if csnil&&~isempty(inputname(2)), assignin('caller','FUntemp',h); evalin('caller',[inputname(2),'=FUntemp;']); end
return;
end
function [func, n, nsub, result]=qmdpt( func, n, nsub, result );
%
%*******************************************************************************
%
%! QMDPT carries out product midpoint quadrature for the unit cube in ND.
%
%
%  Integration region:
%
%    N dimensional points X() such that:
%
%      -1 <= X(I) <= 1 for all I.
%
%  Reference:
%
%    Arthur H Stroud,
%    Approximate Calculation of Multiple Integrals,
%    Prentice Hall, 1971.
%
%  Modified:
%
%    21 November 2000
%
%  Parameters:
%
%    Input, external FUNC, the name of the user supplied
%    function which evaluates the function, of the form
%
%      function func ( n, x )
%      integer n
%      doubleprecision func
%      doubleprecision x(n)
%
%    Output, doubleprecision QA(K), QB(K), two sets of estimates for
%    the integral.  The QB entries are obtained from the
%    QA entries by Richardson extrapolation, and QB(K) is
%    the best estimate for the integral.
%
%    Input, integer N, the dimension of the cube.
%
%    Input, integer NSUB, the number of subdivisions (in each dimension).
%
%    Output, doubleprecision RESULT, the approximate integral of the function.
%
%
persistent i ihi ix j more quad volume w x ; 

if isempty(i), i=0; end;
if isempty(ihi), ihi=0; end;
if isempty(ix), ix=zeros(1,n); end;
if isempty(j), j=0; end;
if isempty(more), more=false; end;
if isempty(quad), quad=0; end;
if isempty(volume), volume=0; end;
if isempty(w), w=0; end;
if isempty(x), x=zeros(1,n); end;
%
%
w = 1.0d+00 ./ ( nsub.^n );
quad = 0.0d+00;
more = false;
ihi = fix(nsub.^n);
for i = 1: ihi;
[ n, ix, more, nsub ]=vec_next( n, ix, more, nsub );
x([1:n]) = fix(( (2 .* ix([1:n])) + 1 - nsub ) ./ ( nsub ));
quad = quad + w .* func( n, x );
end; i = fix(ihi+1);
volume = 2.0d+00.^n;
result = quad .* volume;
return;
end
function [qmult_1dresult, func, a, b ]=qmult_1d( func, a, b );
%
%*******************************************************************************
%
%! QMULT_1D approximates an integral over an interval in 1D.
%
%
%  Discussion:
%
%    A 16 point 31-st degree Gauss-Legendre formula is used.
%
%  Integration region:
%
%    Points X such that:
%
%      A <= X <= B.
%
%  Reference:
%
%    Arthur H Stroud,
%    Approximate Calculation of Multiple Integrals,
%    Prentice Hall, 1971.
%
%  Modified:
%
%    21 November 2000
%
%  Parameters:
%
%    Input, external FUNC, the name of the user supplied
%    function which evaluates F(X), of the form
%
%      function func ( x )
%
%      doubleprecision func
%      doubleprecision x
%
%    Input, doubleprecision A, B, the lower and upper limits of integration.
%
%    Output, doubleprecision QMULT_1D, the approximate integral of the function.
%
qmult_1dresult=[];
persistent i norder qmult_1d quad volume weight x xtab ; 

if isempty(norder), norder = 16; end;
%
if isempty(i), i=0; end;
if isempty(quad), quad=0; end;
if isempty(qmult_1dresult), qmult_1dresult=0; end;
if isempty(volume), volume=0; end;
if isempty(weight), weight=zeros(1,norder); end;
if isempty(x), x=0; end;
if isempty(xtab), xtab=zeros(1,norder); end;
%
%
[ norder, xtab, weight ]=legendre_set( norder, xtab, weight );
quad = 0.0d+00;
for i = 1: norder;
x = 0.5d+00 .*( b - a ) .* xtab(i) + 0.5d+00 .*( a + b );
quad = quad + 0.5d+00 .* weight(i) .* func( x );
end; i = fix(norder+1);
volume = b - a;
qmult_1dresult = quad .* volume;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',func); evalin('caller',[inputname(1),'=FUntemp;']); end
if csnil&&~isempty(inputname(3)), assignin('caller','FUntemp',b); evalin('caller',[inputname(3),'=FUntemp;']); end
if csnil&&~isempty(inputname(2)), assignin('caller','FUntemp',a); evalin('caller',[inputname(2),'=FUntemp;']); end
return;
end
function [qmult_2dresult, func, a, b, fup, flo ]=qmult_2d( func, a, b, fup, flo );
%
%*******************************************************************************
%
%! QMULT_2D approximates an integral with varying Y dimension in 2D.
%
%
%  Discussion:
%
%    A 256 point product of two 16 point 31-st degree Gauss-Legendre
%    quadrature formulas is used.
%
%  Integration region:
%
%    Points (X,Y) such that:
%
%      A <= X <= B,
%      FLO(X) <= Y <= FHI(X).
%
%  Reference:
%
%    Arthur H Stroud,
%    Approximate Calculation of Multiple Integrals,
%    Prentice Hall, 1971.
%
%  Modified:
%
%    21 November 2000
%
%  Parameters:
%
%    Input, external FUNC, the name of the user supplied
%    function which evaluates F(X,Y), of the form
%
%      function func ( x, y )
%      doubleprecision func
%      doubleprecision x
%      doubleprecision y
%
%    Input, doubleprecision A, B, the lower and upper limits of X integration.
%
%    Input, external FUP, FLO, the names of the user
%    supplied functions which evaluate the upper and lower
%    limits of the Y integration, of the form
%
%      function fup(x)
%      doubleprecision fup
%      doubleprecision x
%
%    and
%
%      function flo(x)
%      doubleprecision flo
%      doubleprecision x
%
%    Output, doubleprecision QMULT_2D, the approximate integral of the function.
%
qmult_2dresult=[];
persistent c d i j norder qmult_2d quad w1 w2 weight x xtab y ; 

if isempty(norder), norder = 16; end;
%
if isempty(c), c=0; end;
if isempty(d), d=0; end;
if isempty(i), i=0; end;
if isempty(j), j=0; end;
if isempty(quad), quad=0; end;
if isempty(qmult_2dresult), qmult_2dresult=0; end;
if isempty(w1), w1=0; end;
if isempty(w2), w2=0; end;
if isempty(weight), weight=zeros(1,norder); end;
if isempty(x), x=0; end;
if isempty(xtab), xtab=zeros(1,norder); end;
if isempty(y), y=0; end;
%
%
[ norder, xtab, weight ]=legendre_set( norder, xtab, weight );
quad = 0.0d+00;
for i = 1: norder;
w1 = 0.5d+00 .*( b - a ) .* weight(i);
x = 0.5d+00 .*( b - a ) .* xtab(i) + 0.5d+00 .*( b + a );
c = flo( x );
d = fup( x );
for j = 1: norder;
w2 = 0.5d+00 .*( d - c ) .* weight(j);
y = 0.5d+00 .*( d - c ) .* xtab(j) + 0.5d+00 .*( d + c );
quad = quad + w1 .* w2 .* func( x, y );
end; j = fix(norder+1);
end; i = fix(norder+1);
qmult_2dresult = quad;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(4)), assignin('caller','FUntemp',fup); evalin('caller',[inputname(4),'=FUntemp;']); end
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',func); evalin('caller',[inputname(1),'=FUntemp;']); end
if csnil&&~isempty(inputname(5)), assignin('caller','FUntemp',flo); evalin('caller',[inputname(5),'=FUntemp;']); end
if csnil&&~isempty(inputname(3)), assignin('caller','FUntemp',b); evalin('caller',[inputname(3),'=FUntemp;']); end
if csnil&&~isempty(inputname(2)), assignin('caller','FUntemp',a); evalin('caller',[inputname(2),'=FUntemp;']); end
return;
end
function [qmult_3dresult, func, a, b, fup1, flo1, fup2, flo2 ]=qmult_3d( func, a, b, fup1, flo1, fup2, flo2 );
%
%*******************************************************************************
%
%! QMULT_3D approximates an integral with varying Y and Z dimension in 3D.
%
%
%  Discussion:
%
%    A 4096 point product of three 16 point 31-st degree Gauss-Legendre
%    quadrature formulas is used.
%
%  Integration region:
%
%    Points (X,Y,Z) such that:
%
%      A         <= X <= B,
%      FLO(X)    <= Y <= FHI(X),
%      FLO2(X,Y) <= Z <= FHI2(X,Y).
%
%  Reference:
%
%    Arthur H Stroud,
%    Approximate Calculation of Multiple Integrals,
%    Prentice Hall, 1971.
%
%  Modified:
%
%    21 November 2000
%
%  Parameters:
%
%    Input, external FUNC, the name of the user supplied
%    function which evaluates F(X,Y,Z), of the form
%
%      function func ( x, y, z )
%      doubleprecision func
%      doubleprecision x
%      doubleprecision y
%      doubleprecision z
%
%    Input, doubleprecision A, B, the lower and upper limits of X integration.
%
%    Input, external FUP1, FLO1, the names of the user
%    supplied functions which evaluate the upper and lower
%    limits of the Y integration, of the form
%
%      function fup1(x)
%      doubleprecision fup1
%      doubleprecision x
%
%    and
%
%      function flo1(x)
%      doubleprecision flo1
%      doubleprecision x
%
%    Input, external FUP2, FLO2, the names of the user
%    supplied functions which evaluate the upper and lower
%    limits of the Z integration, of the form
%
%      function fup2(x,y)
%      doubleprecision fup2
%      doubleprecision x
%      doubleprecision y
%
%    and
%
%      function flo2(x,y)
%      doubleprecision flo2
%      doubleprecision x
%      doubleprecision y
%
%    Output, doubleprecision QMULT_3D, the approximate integral of the function.
%
qmult_3dresult=[];
persistent c d e f i j k norder qmult_3d quad volume w1 w2 w3 weight x xtab y z ; 

if isempty(norder), norder = 16; end;
%
if isempty(c), c=0; end;
if isempty(d), d=0; end;
if isempty(e), e=0; end;
if isempty(f), f=0; end;
if isempty(i), i=0; end;
if isempty(j), j=0; end;
if isempty(k), k=0; end;
if isempty(qmult_3dresult), qmult_3dresult=0; end;
if isempty(quad), quad=0; end;
if isempty(volume), volume=0; end;
if isempty(w1), w1=0; end;
if isempty(w2), w2=0; end;
if isempty(w3), w3=0; end;
if isempty(weight), weight=zeros(1,norder); end;
if isempty(x), x=0; end;
if isempty(xtab), xtab=zeros(1,norder); end;
if isempty(y), y=0; end;
if isempty(z), z=0; end;
%
%
[ norder, xtab, weight ]=legendre_set( norder, xtab, weight );
quad = 0.0d+00;
for i = 1: norder;
x = 0.5d+00 .*( b - a ) .* xtab(i) + 0.5d+00 .*( b + a );
w1 = 0.5d+00 .* weight(i);
c = flo1( x );
d = fup1( x );
for j = 1: norder;
w2 = 0.5d+00 .*( d - c ) .* weight(j);
y = 0.5d+00 .*( d - c ) .* xtab(j) + 0.5d+00 .*( d + c );
e = flo2( x, y );
f = fup2( x, y );
for k = 1: norder;
w3 = 0.5d+00 .*( f - e ) .* weight(k);
z = 0.5d+00 .*( f - e ) .* xtab(k) + 0.5d+00 .*( f + e );
quad = quad + w1 .* w2 .* w3 .* func( x, y, z );
end; k = fix(norder+1);
end; j = fix(norder+1);
end; i = fix(norder+1);
volume = b - a;
qmult_3dresult = quad .* volume;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(6)), assignin('caller','FUntemp',fup2); evalin('caller',[inputname(6),'=FUntemp;']); end
if csnil&&~isempty(inputname(4)), assignin('caller','FUntemp',fup1); evalin('caller',[inputname(4),'=FUntemp;']); end
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',func); evalin('caller',[inputname(1),'=FUntemp;']); end
if csnil&&~isempty(inputname(7)), assignin('caller','FUntemp',flo2); evalin('caller',[inputname(7),'=FUntemp;']); end
if csnil&&~isempty(inputname(5)), assignin('caller','FUntemp',flo1); evalin('caller',[inputname(5),'=FUntemp;']); end
if csnil&&~isempty(inputname(3)), assignin('caller','FUntemp',b); evalin('caller',[inputname(3),'=FUntemp;']); end
if csnil&&~isempty(inputname(2)), assignin('caller','FUntemp',a); evalin('caller',[inputname(2),'=FUntemp;']); end
return;
end
function [func, a, b, result]=rectangle_3d( func, a, b, result );
%
%*******************************************************************************
%
%! RECTANGLE_3D approximates an integral inside a rectangular block in 3D.
%
%
%  Discussion:
%
%    An 8 point third degree formula is used.
%
%  Integration region:
%
%    Points (X,Y,Z) such that:
%
%      A(1) <= X <= B(1),
%      A(2) <= Y <= B(2),
%      A(3) <= Z <= B(3).
%
%  Reference:
%
%    Arthur H Stroud,
%    Approximate Calculation of Multiple Integrals,
%    Prentice Hall, 1971.
%
%  Modified:
%
%    21 November 2000
%
%  Parameters:
%
%    Input, external FUNC, the name of the user supplied function which
%    evaluates F(X,Y,Z), of the form
%
%      function func ( x, y, z )
%      doubleprecision func
%      doubleprecision x
%      doubleprecision y
%      doubleprecision z
%
%    Input, doubleprecision A(3), B(3), the lower and upper limits
%    for X, Y and Z.
%
%    Output, doubleprecision RESULT, the approximate integral of the function.
%
persistent i j k quad sqr3 volume w x y z ; 

if isempty(i), i=0; end;
if isempty(j), j=0; end;
if isempty(k), k=0; end;
if isempty(quad), quad=0; end;
if isempty(sqr3), sqr3=0; end;
if isempty(volume), volume=0; end;
if isempty(w), w=0; end;
if isempty(x), x=0; end;
if isempty(y), y=0; end;
if isempty(z), z=0; end;
%
%
sqr3 = 1.0d+00 ./ sqrt( 3.0d+00 );
w = 1.0d+00 ./ 8.0d+00;
quad = 0.0d+00;
for i = 1: 2;
x = sqr3 .*( -1 ).^i;
x = 0.5d+00 .*(( 1.0d+00 - x ) .* b(1) +( 1.0d+00 + x ) .* a(1) );
for j = 1: 2;
y = sqr3 .*(  -1 ).^j;
y = 0.5d+00 .*(( 1.0d+00 - y ) .* b(2) +( 1.0d+00 + y ) .* a(2) );
for k = 1: 2;
z = sqr3 .*( -1 ).^k;
z = 0.5d+00 .*(( 1.0d+00 - z ) .* b(3) +( 1.0d+00 + z ) .* a(3) );
quad = quad + w .* func( x, y, z );
end; k = fix(2+1);
end; j = fix(2+1);
end; i = fix(2+1);
volume =( b(1) - a(1) ) .*( b(2) - a(2) ) .*( b(3) - a(3) );
result = volume .* quad;
return;
end
function [func, xval, yval, nsub, norder, xtab, ytab,weight, result]=rectangle_sub_2d( func, xval, yval, nsub, norder, xtab, ytab,weight, result );
%
%*******************************************************************************
%
%! RECTANGLE_SUB_2D carries out a composite quadrature over a rectangle in 2D.
%
%
%  Integration interval:
%
%    XVAL(1) <= X <= XVAL(2),
%    YVAL(1) <= Y <= YVAL(2).
%
%  Modified:
%
%    21 November 2000
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, EXTERNAL FUNC, the name of the function to be
%    integrated.  The user must declare the name an EXTERNAL
%    parameter in the calling program, pass the name of the
%    function in FUNC, and write a function of the form
%
%      function func ( x, y )
%
%    which evaluates the function at the point (X,Y).
%
%    Input, doubleprecision XVAL(2), the left and right X coordinates.
%
%    Input, doubleprecision YVAL(2), the lower and upper Y coordinates.
%
%    Input, integer NSUB(2).
%    NSUB(1) is the number of subintervals to use in the X direction,
%    and NSUB(2) is the same thing for Y.
%
%    Input, integer NORDER, the order of the rule.
%
%    Input, doubleprecision XTAB(NORDER), YTAB(NORDER), the abscissas.
%
%    Input, doubleprecision WEIGHT(NORDER), the weights of the rule.
%
%    Output, doubleprecision RESULT, the approximate integral of the function.
%
%
persistent a b i j k quad_sub result_sub volume volume_sub x xhi xlo y yhi ylo ; 

if isempty(a), a=zeros(1,2); end;
if isempty(b), b=zeros(1,2); end;
if isempty(i), i=0; end;
if isempty(j), j=0; end;
if isempty(k), k=0; end;
if isempty(quad_sub), quad_sub=0; end;
if isempty(result_sub), result_sub=0; end;
if isempty(volume), volume=0; end;
if isempty(volume_sub), volume_sub=0; end;
if isempty(x), x=0; end;
if isempty(xhi), xhi=0; end;
if isempty(xlo), xlo=0; end;
if isempty(y), y=0; end;
if isempty(yhi), yhi=0; end;
if isempty(ylo), ylo=0; end;
%
%
a(1) = xval(1);
a(2) = yval(1);
b(1) = xval(2);
b(2) = yval(2);
for i = 1: 2;
if( a(i) == b(i) )
result = 0.0d+00;
return;
end;
end; i = fix(2+1);
for i = 1: 2;
if( nsub(i) < 1 )
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'RECTANGLE_SUB_2D - Fatal error!');
writef(1,['%s %0.15g \n'], '  Nonpositive value of NSUB(I) = ', nsub(i));
writef(1,['%s %0.15g \n'], '  for index I = ', i);
error(['stop encountered in original fortran code  ',char(10),';']);
end;
end; i = fix(2+1);
%
%  Break up the X interval into NSUB(1) subintervals.
%
volume = 0.0d+00;
result = 0.0d+00;
for i = 1: nsub(1);
[ a(1), b(1),dumvar3, i, xlo ]=dvec_even_select( a(1), b(1), nsub(1)+1, i, xlo );
[ a(1), b(1),dumvar3,dumvar4, xhi ]=dvec_even_select( a(1), b(1), nsub(1)+1, i+1, xhi );
%
%  Break up the Y interval into NSUB(2) subintervals.
%
for j = 1: nsub(2);
[ a(2), b(2),dumvar3, j,   ylo ]=dvec_even_select( a(2), b(2), nsub(2)+1, j,   ylo );
[ a(2), b(2),dumvar3,dumvar4, yhi ]=dvec_even_select( a(2), b(2), nsub(2)+1, j+1, yhi );
quad_sub = 0.0d+00;
for k = 1: norder;
x = xlo + 0.5d+00 .*( xtab(k) + 1.0d+00 ) .*( xhi - xlo );
y = ylo + 0.5d+00 .*( ytab(k) + 1.0d+00 ) .*( yhi - ylo );
quad_sub = quad_sub + weight(k) .* func( x, y ) ./ 4.0d+00;
end; k = fix(norder+1);
volume_sub =( xhi - xlo ) .*( yhi - ylo );
result_sub = quad_sub .* volume_sub;
volume = volume + volume_sub;
result = result + result_sub;
end; j = fix(nsub(2)+1);
end; i = fix(nsub(1)+1);
return;
end
function [a, b, c, d, norder, x, w]=rule_adjust( a, b, c, d, norder, x, w );
%
%*******************************************************************************
%
%! RULE_ADJUST maps a quadrature rule from [A,B] to [C,D].
%
%
%  Discussion:
%
%    Most quadrature rules are defined on a special interval, like
%    [-1,1] or [0,1].  To integrate over an interval, the abscissas
%    and weights must be adjusted.  This can be done on the fly,
%    or by calling this routine.
%
%    If the weight function W(X) is not 1, then the W vector will
%    require further adjustment by the user.
%
%  Modified:
%
%    06 December 2000
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, doubleprecision A, B, the endpoints of the definition interval.
%
%    Input, doubleprecision C, D, the endpoints of the integration interval.
%
%    Input, integer NORDER, the number of abscissas and weights.
%
%    Input/output, doubleprecision X(NORDER), W(NORDER), the abscissas
%    and weights.
%
%
%

x([1:norder]) =(( b - x([1:norder]) ) .* c +( x([1:norder]) - a ) .* d ) ./( b - a );
w([1:norder]) =(( d - c ) ./( b - a ) ) .* w([1:norder]);
return;
end
function [func, lda, n, v, result]=simplex_nd( func, lda, n, v, result );
%
%*******************************************************************************
%
%! SIMPLEX_ND approximates an integral inside a simplex in ND.
%
%
%  Discussion:
%
%    An N+1 point second degree formula is used.
%
%  Integration region:
%
%    The simplex bounded by the origin and a convex combination of N points.
%
%  Reference:
%
%    Arthur H Stroud,
%    Approximate Calculation of Multiple Integrals,
%    Prentice Hall, 1971.
%
%  Modified:
%
%    21 November 2000
%
%  Parameters:
%
%    Input, external FUNC, the name of the user supplied
%    function which evaluates F(X) at the N-dimensional point
%    X, of the form
%
%      function func ( n, x )
%      integer n
%      doubleprecision func
%      doubleprecision x(n)
%
%    Input, integer LDA, the leading dimension of the array V.
%    LDA must be at least N.
%
%    Input, integer N, the dimension of the space.
%
%    Input/output, doubleprecision V(LDA,N+1).  On input, each of the
%    N+1 rows of V contains the N coordinates of one of the
%    'corners' of the simplex in entries 1 through N, with
%    the last column being left free.
%
%    On output, V has been overwritten in the process of
%    computing the volume of the simplex.
%
%    Output, doubleprecision RESULT, the approximate integral of the function.
%
%
persistent c i quad volume w x ; 

if isempty(c), c=0; end;
if isempty(i), i=0; end;
if isempty(quad), quad=0; end;
v_orig=v;v_shape=[lda,n+1];v=reshape([v_orig(1:min(prod(v_shape),numel(v_orig))),zeros(1,max(0,prod(v_shape)-numel(v_orig)))],v_shape);
if isempty(volume), volume=0; end;
if isempty(w), w=0; end;
if isempty(x), x=zeros(1,n); end;
%
%
c = 1.0d+00 ./ sqrt( ( n + 2 ) );
w = 1.0d+00 ./ ( n + 1 );
for i = 1: n;
x(i) = w .*( 1.0d+00 - c ) .* sum(sum( v([1:n+1],i) ));
end; i = fix(n+1);
quad = 0.0d+00;
for i = 1: n+1;
x([1:n]) = x([1:n]) + c .* v(i,[1:n]);
quad = quad + w .* func( n, x );
x([1:n]) = x([1:n]) - c .* v(i,[1:n]);
end; i = fix(n+1+1);
[volume , lda, n, v ]=simplex_volume_nd( lda, n, v );
result = quad .* volume;
v_orig(1:prod(v_shape))=v;v=v_orig;
return;
end
function [simplex_unit_volume_ndresult, n ]=simplex_unit_volume_nd( n );
%
%*******************************************************************************
%
%! SIMPLEX_UNIT_VOLUME_ND returns the volume of the unit simplex in ND.
%
%
%  Modified:
%
%    27 October 2000
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, integer N, the dimension of the space.
%
%    Output, doubleprecision SIMPLEX_UNIT_VOLUME_ND, the volume of the
%    unit simplex.
%
simplex_unit_volume_ndresult=[];
persistent simplex_unit_volume_nd ; 

if isempty(simplex_unit_volume_ndresult), simplex_unit_volume_ndresult=0; end;
%
simplex_unit_volume_ndresult = 1.0d+00 ./ ( n_factorial( n ) );
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',n); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [simplex_volume_ndresult, lda, n, v ]=simplex_volume_nd( lda, n, v );
%
%*******************************************************************************
%
%! SIMPLEX_VOLUME_ND returns the volume of a simplex in ND.
%
%
%  Modified:
%
%    09 December 2000
%
%  Author:
%
%    John Burkardt
%
%  Parameters:]
%
%    Input, integer LDA, the leading dimension of the array V.
%    LDA must be at least N.
%
%    Input, integer N, the dimension of the space.
%
%    Input/output, doubleprecision V(LDA,N+1).  On input, each of the
%    N+1 rows of V contains the N coordinates of one of the
%    'corners' of the simplex in entries 1 through N, with
%    the last column being left free.
%
%    On output, V has been overwritten in the process of
%    computing the volume of the simplex.
%
%    Output, doubleprecision SIMPLEX_VOLUME_ND, the volume of the unit simplex.
%
%
simplex_volume_ndresult=[];
persistent simplex_volume_nd volume ; 

if isempty(simplex_volume_ndresult), simplex_volume_ndresult=0; end;
v_orig=v;v_shape=[lda,n+1];v=reshape([v_orig(1:min(prod(v_shape),numel(v_orig))),zeros(1,max(0,prod(v_shape)-numel(v_orig)))],v_shape);
if isempty(volume), volume=0; end;
%
%  Compute the volume of the parallelipiped.
%
[volume , lda, n, v ]=parallelipiped_volume_nd( lda, n, v );
%
%  Multiply by the volume of the unit simplex, which serves as a
%  conversion factor between a parallelipiped and the simplex.
%
simplex_volume_ndresult = volume .* simplex_unit_volume_nd( n );
v_orig(1:prod(v_shape))=v;v=v_orig;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(3)), assignin('caller','FUntemp',v); evalin('caller',[inputname(3),'=FUntemp;']); end
if csnil&&~isempty(inputname(2)), assignin('caller','FUntemp',n); evalin('caller',[inputname(2),'=FUntemp;']); end
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',lda); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [sphere_area_3dresult, r ]=sphere_area_3d( r );
%
%*******************************************************************************
%
%! SPHERE_AREA_3D computes the surface area of a sphere in 3D.
%
%
%  Modified:
%
%    12 December 2000
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, doubleprecision R, the radius of the sphere.
%
%    Output, doubleprecision SPHERE_AREA_3D, the area of the sphere.
%
sphere_area_3dresult=[];
persistent sphere_area_3d ; 

if isempty(sphere_area_3dresult), sphere_area_3dresult=0; end;
%
sphere_area_3dresult = 4.0d+00 .* pi( ) .* r.^2;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',r); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [sphere_area_ndresult, n, r ]=sphere_area_nd( n, r );
%
%*******************************************************************************
%
%! SPHERE_AREA_ND computes the surface area of a sphere in ND.
%
%
%  Discussion:
%
%    N   Area
%
%    2   2       * PI    * R
%    3   4       * PI    * R**2
%    4   2       * PI**2 * R**3
%    5   (8/3)   * PI**2 * R**4
%    6             PI**3 * R**5
%    7   (16/15) * PI**3 * R**6
%
%  Modified:
%
%    26 October 2000
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, integer N, the dimension of the space.
%
%    Input, doubleprecision R, the radius of the sphere.
%
%    Output, doubleprecision SPHERE_AREA_ND, the area of the sphere.
%
sphere_area_ndresult=[];
persistent sphere_area_nd ; 

if isempty(sphere_area_ndresult), sphere_area_ndresult=0; end;
%
sphere_area_ndresult = sphere_unit_area_nd( n ) .* r.^(n-1);
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(2)), assignin('caller','FUntemp',r); evalin('caller',[inputname(2),'=FUntemp;']); end
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',n); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [func, n, xc, r, result]=sphere_f1_nd( func, n, xc, r, result );
%
%*******************************************************************************
%
%! SPHERE_F1_ND approximates an integral inside a sphere in ND.
%
%
%  Discussion:
%
%    An (N+1)*2**N point 5-th degree formula is used, Stroud number SN:5-6.
%
%  Integration region:
%
%    N dimensional points X() such that:
%
%      SUM ( I = 1 to N ) ( X(I) - XC(I) )**2 <= R**2.
%
%  Reference:
%
%    Arthur H Stroud,
%    Approximate Calculation of Multiple Integrals,
%    Prentice Hall, 1971.
%
%  Modified:
%
%    19 December 2000
%
%  Parameters:
%
%    Input, external FUNC, the name of the user supplied
%    function which evaluates F at the N-vector X, of the form
%
%      function func ( n, x )
%      integer n
%      doubleprecision func
%      doubleprecision x(n)
%
%    Input, integer N, the dimension of the sphere.
%
%    Input, doubleprecision XC(N), the center of the sphere.
%
%    Input, doubleprecision R, the radius of the sphere.
%
%    Output, doubleprecision RESULT, the approximate integral of the function.
%
%
persistent i ihi itemp j k khi ktemp pi quad t temp u u2 v volume w x y ; 

if isempty(i), i=0; end;
if isempty(ihi), ihi=0; end;
if isempty(itemp), itemp=0; end;
if isempty(j), j=0; end;
if isempty(k), k=0; end;
if isempty(khi), khi=0; end;
if isempty(ktemp), ktemp=0; end;
if isempty(pi), pi=0; end;
if isempty(quad), quad=0; end;
if isempty(t), t=0; end;
if isempty(temp), temp=0; end;
if isempty(u), u=0; end;
if isempty(u2), u2=0; end;
if isempty(v), v=0; end;
if isempty(volume), volume=0; end;
if isempty(w), w=0; end;
if isempty(x), x=zeros(1,n); end;
if isempty(y), y=0; end;
%
%
if( r == 0.0d+00 )
result = 0.0d+00;
return;
end;
u2 =( 1.0d+00 - 2.0d+00 .* sqrt( 1.0d+00 ./ ( n + 4 ) ) ) ./ ( n + 2 );
u = sqrt( u2 );
x([1:n]) = xc([1:n]) - r .* u;
w = 1.0d+00 ./ (( n + 1 ) .* 2.^n );
quad = 0.0d+00;
ihi = fix(2.^n);
for i = 1: ihi;
itemp = fix(i - 1);
for j = 1: n;
u =( xc(j) - x(j) ) ./ r;
if( rem( itemp, 2 ) == 1 )
x(j) = xc(j) - abs( x(j) - xc(j) );
else;
x(j) = xc(j) + abs( x(j) - xc(j) );
end;
itemp = fix(fix(itemp ./ 2));
end; j = fix(n+1);
quad = quad + w .* func( n, x );
end; i = fix(ihi+1);
temp = sqrt( ( n + 4 ) );
t = (sqrt( (2.0d+00 .* ( n + 1 )) ./ ( n + 2 ) ) ./( ( n ) .* temp ));
y =( 1.0d+00 + 2.0d+00 ./( ( n ) .* temp ) ) ./ ( n + 2 );
v = sqrt( y - t );
u = sqrt( y + ( n - 1 ) .* t );
khi = fix(2.^n);
for i = 1: n;
x([1:n]) = xc([1:n]) - r .* v;
x(i) = xc(i) - r .* u;
for k = 1: khi;
ktemp = fix(k - 1);
for j = 1: n;
if( rem( ktemp, 2 ) == 1 )
x(j) = xc(j) - abs( x(j) - xc(j) );
else;
x(j) = xc(j) + abs( x(j) - xc(j) );
end;
ktemp = fix(fix(ktemp ./ 2));
end; j = fix(n+1);
quad = quad + w .* func( n, x );
end; k = fix(khi+1);
x(i) = xc(i) - r .* v;
end; i = fix(n+1);
[volume , n, r ]=sphere_volume_nd( n, r );
result = quad .* volume;
return;
end
function [func, n, xc, r, result]=sphere_f3_nd( func, n, xc, r, result );
%
%*******************************************************************************
%
%! SPHERE_F3_ND approximates an integral inside a sphere in ND.
%
%
%  Discussion:
%
%    A 2**(N+1)-1 point 5-th degree formula is used, Stroud number SN:5-4.
%
%  Integration region:
%
%    N dimensional points X() such that:
%
%      SUM ( I = 1 to N ) ( X(I) - XC(I) )**2 <= R**2.
%
%  Reference:
%
%    Arthur H Stroud,
%    Approximate Calculation of Multiple Integrals,
%    Prentice Hall, 1971.
%
%  Modified:
%
%    21 November 2000
%
%  Parameters:
%
%    Input, external FUNC, the name of the user supplied
%    function which evaluates F at the N-vector X, of the form
%
%      function func ( n, x )
%      integer n
%      doubleprecision func
%      doubleprecision x(n)
%
%    Input, integer N, the dimension of the sphere.
%
%    Input, doubleprecision XC(N), the center of the sphere.
%
%    Input, doubleprecision R, the radius of the sphere.
%
%    Output, doubleprecision RESULT, the approximate integral of the function.
%
%
persistent i j jtemp k pi quad ri s volume weight x ; 

if isempty(i), i=0; end;
if isempty(j), j=0; end;
if isempty(jtemp), jtemp=0; end;
if isempty(k), k=0; end;
if isempty(pi), pi=0; end;
if isempty(quad), quad=0; end;
if isempty(ri), ri=0; end;
if isempty(s), s=0; end;
if isempty(volume), volume=0; end;
if isempty(weight), weight=0; end;
if isempty(x), x=zeros(1,n); end;
%
%
if( r == 0.0d+00 )
result = 0.0d+00;
return;
end;
quad = 0.0d+00;
%
%  The first point is the center of the sphere.
%
x([1:n]) = xc([1:n]);
weight = 4.0d+00 ./ ( n + 2 ).^2;
quad = quad + weight .* func( n, x );
s = 1.0d+00 ./ sqrt( ( n + 4 ) );
for i = 1: n;
ri = sqrt( fix(( i + 2 ) ./ ( n + 4 )) );
%
%  Set up the first point, with (I-1) zeroes, RI, and then N-I S's.
%
for j = 1: n;
if( j < i )
x(j) = xc(j);
elseif( j == i ) ;
x(j) = xc(j) + r .* ri;
else;
x(j) = xc(j) + r .* s;
end;
end; j = fix(n+1);
weight = 2.0d+00.^fix((( i - n ) .* ( n + 4 ))./ ((( i + 1 ) .*( i + 2 )) .*( n + 2 ) ));
%
%  Now go through all sign permutations of the basic point.
%
for j = 1: 2.^(n+1-i);
jtemp = fix(j - 1);
for k = i: n;
if( rem( jtemp, 2 ) == 1 )
x(k) = xc(k) - abs( x(k) - xc(k) );
else;
x(k) = xc(k) + abs( x(k) - xc(k) );
end;
jtemp = fix(fix(jtemp ./ 2));
end; k = fix(n+1);
quad = quad + weight .* func( n, x );
end; j = fix(2.^(n+1-i)+1);
end; i = fix(n+1);
[volume , n, r ]=sphere_volume_nd( n, r );
result = quad .* volume;
return;
end
function [func, n, xc, r1, r2, result]=sphere_shell_03_nd( func, n, xc, r1, r2, result );
%
%*******************************************************************************
%
%! SPHERE_SHELL_03_ND approximates an integral inside a spherical shell in ND.
%
%
%  Discussion:
%
%    An 2*N point 3-rd degree formula is used, Stroud number SN-Shell:3-1.
%
%  Integration region:
%
%    N dimensional points X() such that:
%
%      R1**2 <= SUM ( I = 1 to N ) ( X(I) - XC(I) )**2 <= R2**2.
%
%  Reference:
%
%    Arthur H Stroud,
%    Approximate Calculation of Multiple Integrals,
%    Prentice Hall, 1971.
%
%  Modified:
%
%    21 December 2000
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, external FUNC, the name of the user supplied
%    function which evaluates F at the N-vector X, of the form
%
%      function func ( n, x )
%      integer n
%      doubleprecision func
%      doubleprecision x(n)
%
%    Input, integer N, the dimension of the space.
%
%    Input, doubleprecision XC(N), the center of the spheres.
%
%    Input, doubleprecision R1, R2, the inner and outer radiuses that
%    define the spherical shell.
%
%    Output, doubleprecision RESULT, the approximate integral of the function.
%
%
persistent i quad r rho volume w x ; 

if isempty(i), i=0; end;
if isempty(quad), quad=0; end;
if isempty(r), r=0; end;
if isempty(rho), rho=0; end;
if isempty(volume), volume=0; end;
if isempty(w), w=0; end;
if isempty(x), x=zeros(1,n); end;
%
%
if( r1 == r2 )
result = 0.0d+00;
return;
end;
rho = r1 ./ r2;
r = ( n ) .*( 1.0d+00 - rho.^(n+2) )./( ( n + 2 ) .*( 1.0d+00 - rho.^n ) );
r = sqrt( r );
w = 1.0d+00 ./ ( 2 .* n );
x([1:n]) = xc([1:n]);
quad = 0.0d+00;
for i = 1: n;
x(i) = xc(i) + r .* r2;
quad = quad + w .* func( n, x );
x(i) = xc(i) - r .* r2;
quad = quad + w .* func( n, x );
x(i) = xc(i);
end; i = fix(n+1);
[volume , n, r1, r2 ]=sphere_shell_volume_nd( n, r1, r2 );
result = quad .* volume;
return;
end
function [func, n, xc, r1, r2, result]=sphere_shell_07_nd( func, n, xc, r1, r2, result );
%
%*******************************************************************************
%
%! SPHERE_SHELL_07_ND approximates an integral inside a spherical shell in ND.
%
%
%  Discussion:
%
%    An 2*M point 7-th degree formula is used, Stroud number SN-Shell:7-1.
%
%    Here M is the number of points in some formula of degree 7 for the
%    surface of the sphere in ND.
%
%    For the formula we have chosen, M is 2**N + 2*N**2
%
%  Integration region:
%
%    N dimensional points X() such that:
%
%      R1**2 <= SUM ( I = 1 to N ) ( X(I) - XC(I) )**2 <= R2**2.
%
%  Reference:
%
%    Arthur H Stroud,
%    Approximate Calculation of Multiple Integrals,
%    Prentice Hall, 1971.
%
%  Modified:
%
%    21 December 2000
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, external FUNC, the name of the user supplied
%    function which evaluates F at the N-vector X, of the form
%
%      function func ( n, x )
%      integer n
%      doubleprecision func
%      doubleprecision x(n)
%
%    Input, integer N, the dimension of the space.
%
%    Input, doubleprecision XC(N), the center of the spheres.
%
%    Input, doubleprecision R1, R2, the inner and outer radiuses that
%    define the spherical shell.
%
%    Output, doubleprecision RESULT, the approximate integral of the function.
%
%
persistent a b c0 c2 c4 c6 i iadd ix j jhi k more ncard quad ra rho rw volume w w1 w2 w3 x x1 x2 x3 ; 

if isempty(a), a=0; end;
if isempty(b), b=0; end;
if isempty(c0), c0=0; end;
if isempty(c2), c2=0; end;
if isempty(c4), c4=0; end;
if isempty(c6), c6=0; end;
if isempty(i), i=0; end;
if isempty(iadd), iadd=0; end;
if isempty(ix), ix=zeros(1,n); end;
if isempty(j), j=0; end;
if isempty(jhi), jhi=0; end;
if isempty(k), k=0; end;
if isempty(more), more=false; end;
if isempty(ncard), ncard=0; end;
if isempty(quad), quad=0; end;
if isempty(ra), ra=zeros(1,2); end;
if isempty(rho), rho=0; end;
if isempty(rw), rw=zeros(1,2); end;
if isempty(volume), volume=0; end;
if isempty(w), w=0; end;
if isempty(w1), w1=0; end;
if isempty(w2), w2=0; end;
if isempty(w3), w3=0; end;
if isempty(x), x=zeros(1,n); end;
if isempty(x1), x1=0; end;
if isempty(x2), x2=0; end;
if isempty(x3), x3=0; end;
%
%
%  Determine the radii:
%
rho = r1 ./ r2;
c0 =( 1.0d+00 - rho.^n     ) ./ ( n     );
c2 =( 1.0d+00 - rho.^(n+2) ) ./ ( n + 2 );
c4 =( 1.0d+00 - rho.^(n+4) ) ./ ( n + 4 );
c6 =( 1.0d+00 - rho.^(n+6) ) ./ ( n + 6 );
a =( c4 .* c2 - c0 .* c6 ) ./( c2 .* c2 - c4 .* c0 );
b =( c4 .* c4 - c2 .* c6 ) ./( c2 .* c2 - c4 .* c0 );
ra(1) = 0.5d+00 .*( a + sqrt( a.^2 - 4.0d+00 .* b ) );
ra(2) = 0.5d+00 .*( a - sqrt( a.^2 - 4.0d+00 .* b ) );
ra(1) =( 1.0d+00 - ra(1) ) .* r1 + ra(1) .* r2;
ra(2) =( 1.0d+00 - ra(2) ) .* r1 + ra(2) .* r2;
%
%  Determine the radial weights
%
rw(1) =( c0 .* r2.^2 - c2 ) ./(( r2 + r1 ) .*( r2 - r1 ) );
rw(2) =( c2 - c0 .* r1.^2 ) ./(( r2 + r1 ) .*( r2 - r1 ) );
rw(1) = rw(1) .* ( n ) .* r2.^n ./( r2.^n - r1.^n );
rw(2) = rw(2) .* ( n ) .* r2.^n ./( r2.^n - r1.^n );
%
%  Set up the integration rule for a unit spherical surface.
%
w1 = fix(( 8 - n ) ./ ( (n .*( n + 2 )) .*( n + 4 ) ));
w2 = fix(( n.^3 ) ./ ( 2.^(((n .* n) .*( n + 2 )) .*( n + 4 )) ));
w3 = 4.0d+00 ./ ( n .*( n + 2 ) .*( n + 4 ) );
x1 = 1.0d+00;
x2 = 1.0d+00 ./ sqrt( ( n ) );
x3 = 1.0d+00 ./ sqrt( 2.0d+00 );
%
%  Carry out the quadrature.
%
x([1:n]) = xc([1:n]);
quad = 0.0d+00;
for k = 1: 2;
%
%  First term.
%
for i = 1: n;
x(i) = xc(i) + ra(k) .* x1;
quad = quad + rw(k) .* w1 .* func( n, x );
x(i) = xc(i) - ra(k) .* x1;
quad = quad + rw(k) .* w1 .* func( n, x );
x(i) = xc(i);
end; i = fix(n+1);
%
%  Second term.
%
x([1:n]) = xc([1:n]) - ra(k) .* x2;
more = false;
jhi = fix(2.^n);
for j = 1: jhi;
[ n, ix, more, ncard, iadd ]=subset_next( n, ix, more, ncard, iadd );
if( iadd ~= 0 )
x(iadd) = xc(iadd) -( x(iadd) - xc(iadd) );
end;
quad = quad + rw(k) .* w2 .* func( n, x );
end; j = fix(jhi+1);
%
%  Third term.
%
x([1:n]) = xc([1:n]);
for i = 1: n-1;
for j = i+1: n;
x(i) = xc(i) + ra(k) .* x3;
x(j) = xc(j) + ra(k) .* x3;
quad = quad + rw(k) .* w3 .* func( n, x );
x(i) = xc(i) - ra(k) .* x3;
x(j) = xc(j) + ra(k) .* x3;
quad = quad + rw(k) .* w3 .* func( n, x );
x(i) = xc(i) + ra(k) .* x3;
x(j) = xc(j) - ra(k) .* x3;
quad = quad + rw(k) .* w3 .* func( n, x );
x(i) = xc(i) - ra(k) .* x3;
x(j) = xc(j) - ra(k) .* x3;
quad = quad + rw(k) .* w3 .* func( n, x );
x(i) = xc(i);
x(j) = xc(j);
end; j = fix(n+1);
end; i = fix(n-1+1);
end; k = fix(2+1);
[volume , n, r1, r2 ]=sphere_shell_volume_nd( n, r1, r2 );
result = quad .* volume;
return;
end
function [sphere_shell_volume_ndresult, n, r1, r2 ]=sphere_shell_volume_nd( n, r1, r2 );
%
%*******************************************************************************
%
%! SPHERE_SHELL_VOLUME_ND computes the volume of a spherical shell in ND.
%
%
%  Discussion:
%
%    The spherical shell is the volume between two concentric spheres of
%    radius R1 and R2.
%
%  Modified:
%
%    19 December 2000
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, integer N, the dimension of the space.
%
%    Input, doubleprecision R1, R2, the radiuses of the inner and outer spheres.
%
%    Output, doubleprecision SPHERE_SHELL_VOLUME_ND, the volume of the
%    spherical shell.
%
sphere_shell_volume_ndresult=[];
persistent sphere_shell_volume_nd ; 

if isempty(sphere_shell_volume_ndresult), sphere_shell_volume_ndresult=0; end;
%
sphere_shell_volume_ndresult = sphere_volume_nd( n, r2 )- sphere_volume_nd( n, r1 );
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(3)), assignin('caller','FUntemp',r2); evalin('caller',[inputname(3),'=FUntemp;']); end
if csnil&&~isempty(inputname(2)), assignin('caller','FUntemp',r1); evalin('caller',[inputname(2),'=FUntemp;']); end
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',n); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [func, n, xc, r, result]=sphere_surface_5_nd( func, n, xc, r, result );
%
%*******************************************************************************
%
%! SPHERE_SURFACE_5_ND approximates surface integrals on a sphere in ND.
%
%
%  Discussion:
%
%    A 2*N+2**N points 5-th degree formula is used, Stroud number UN:5-2.
%
%  Integration region:
%
%    N dimensional points X() such that:
%
%      SUM ( I = 1 to N ) ( X(I) - XC(I) )**2 = R**2.
%
%  Reference:
%
%    Arthur H Stroud,
%    Approximate Calculation of Multiple Integrals,
%    Prentice Hall, 1971.
%
%  Modified:
%
%    20 December 2000
%
%  Parameters:
%
%    Input, external FUNC, the name of the user supplied
%    function which evaluates F(X), at the N dimensional point
%    X, of the form
%
%      function func ( n, x )
%      integer n
%      doubleprecision func
%      doubleprecision x(n)
%
%    Input, integer N, the dimension of the space.
%
%    Input, doubleprecision XC(N), the center of the sphere.
%
%    Input, doubleprecision R, the radius of the sphere.
%
%    Output, doubleprecision RESULT, the approximate integral of the function.
%
%
persistent i iadd ihi ix more ncard quad volume w1 w2 x x1 x2 ; 

if isempty(i), i=0; end;
if isempty(iadd), iadd=0; end;
if isempty(ihi), ihi=0; end;
if isempty(ix), ix=zeros(1,n); end;
if isempty(more), more=false; end;
if isempty(ncard), ncard=0; end;
if isempty(quad), quad=0; end;
if isempty(volume), volume=0; end;
if isempty(w1), w1=0; end;
if isempty(w2), w2=0; end;
if isempty(x), x=zeros(1,n); end;
if isempty(x1), x1=0; end;
if isempty(x2), x2=0; end;
%
%
x1 = 1.0d+00;
x2 = 1.0d+00 ./ sqrt( ( n ) );
w1 = 1.0d+00 ./ ( n .*( n + 2 ) );
w2 = fix(( n ) ./ ((( n + 2 ) .* 2).^n ));
x([1:n]) = xc([1:n]);
quad = 0.0d+00;
for i = 1: n;
x(i) = xc(i) + r .* x1;
quad = quad + w1 .* func( n, x );
x(i) = xc(i) - r .* x1;
quad = quad + w1 .* func( n, x );
x(i) = xc(i);
end; i = fix(n+1);
more = false;
ihi = fix(2.^n);
x([1:n]) = xc([1:n]) - r .* x2;
for i = 1: ihi;
[ n, ix, more, ncard, iadd ]=subset_next( n, ix, more, ncard, iadd );
if( iadd ~= 0 )
x(iadd) = xc(iadd) -( x(iadd) - xc(iadd) );
end;
quad = quad + w2 .* func( n, x );
end; i = fix(ihi+1);
[volume , n, r ]=sphere_area_nd( n, r );
result = quad .* volume;
return;
end
function [func, n, xc, r, result]=sphere_surface_7_1_nd( func, n, xc, r, result );
%
%*******************************************************************************
%
%! SPHERE_SURFACE_7_1_ND approximates surface integrals on a sphere in ND.
%
%
%  Discussion:
%
%    A 2**N + 2*N**2 point 7th degree formula is used, Stroud number UN:7-1.
%
%  Integration region:
%
%    N dimensional points X() such that:
%
%      SUM ( I = 1 to N ) ( X(I) - XC(I) )**2 = R**2.
%
%  Reference:
%
%    Arthur H Stroud,
%    Approximate Calculation of Multiple Integrals,
%    Prentice Hall, 1971.
%
%  Modified:
%
%    20 December 2000
%
%  Parameters:
%
%    Input, external FUNC, the name of the user supplied
%    function which evaluates F(X), at the N dimensional point
%    X, of the form
%
%      function func ( n, x )
%      integer n
%      doubleprecision func
%      doubleprecision x(n)
%
%    Input, integer N, the dimension of the space.
%
%    Input, doubleprecision XC(N), the center of the sphere.
%
%    Input, doubleprecision R, the radius of the sphere.
%
%    Output, doubleprecision RESULT, the approximate integral of the function.
%
%
persistent i iadd ix j jhi more ncard quad volume w1 w2 w3 x x1 x2 x3 ; 

if isempty(i), i=0; end;
if isempty(iadd), iadd=0; end;
if isempty(ix), ix=zeros(1,n); end;
if isempty(j), j=0; end;
if isempty(jhi), jhi=0; end;
if isempty(more), more=false; end;
if isempty(ncard), ncard=0; end;
if isempty(quad), quad=0; end;
if isempty(volume), volume=0; end;
if isempty(w1), w1=0; end;
if isempty(w2), w2=0; end;
if isempty(w3), w3=0; end;
if isempty(x), x=zeros(1,n); end;
if isempty(x1), x1=0; end;
if isempty(x2), x2=0; end;
if isempty(x3), x3=0; end;
%
%
x([1:n]) = xc([1:n]);
w1 = fix(( 8 - n ) ./ ( (n .*( n + 2 )) .*( n + 4 ) ));
w2 = fix(( n.^3 ) ./ ( 2.^(((n .* n) .*( n + 2 )) .*( n + 4 )) ));
w3 = 4.0d+00 ./ ( n .*( n + 2 ) .*( n + 4 ) );
x1 = 1.0d+00;
x2 = 1.0d+00 ./ sqrt( ( n ) );
x3 = 1.0d+00 ./ sqrt( 2.0d+00 );
quad = 0.0d+00;
%
%  First term.
%
for i = 1: n;
x(i) = xc(i) + r .* x1;
quad = quad + w1 .* func( n, x );
x(i) = xc(i) - r .* x1;
quad = quad + w1 .* func( n, x );
x(i) = xc(i);
end; i = fix(n+1);
%
%  Second term.
%
x([1:n]) = xc([1:n]) - r .* x2;
more = false;
jhi = fix(2.^n);
for j = 1: jhi;
[ n, ix, more, ncard, iadd ]=subset_next( n, ix, more, ncard, iadd );
if( iadd ~= 0 )
x(iadd) = xc(iadd) -( x(iadd) - xc(iadd) );
end;
quad = quad + w2 .* func( n, x );
end; j = fix(jhi+1);
%
%  Third term.
%
x([1:n]) = xc([1:n]);
for i = 1: n-1;
for j = i+1: n;
x(i) = xc(i) + r .* x3;
x(j) = xc(j) + r .* x3;
quad = quad + w3 .* func( n, x );
x(i) = xc(i) - r .* x3;
x(j) = xc(j) + r .* x3;
quad = quad + w3 .* func( n, x );
x(i) = xc(i) + r .* x3;
x(j) = xc(j) - r .* x3;
quad = quad + w3 .* func( n, x );
x(i) = xc(i) - r .* x3;
x(j) = xc(j) - r .* x3;
quad = quad + w3 .* func( n, x );
x(i) = xc(i);
x(j) = xc(j);
end; j = fix(n+1);
end; i = fix(n-1+1);
[volume , n, r ]=sphere_area_nd( n, r );
result = quad .* volume;
return;
end
function [func, result]=sphere_unit_07_3d( func, result );
%
%*******************************************************************************
%
%! SPHERE_UNIT_07_3D approximates an integral inside the unit sphere in 3D.
%
%
%  Discussion:
%
%    A 64 point 7-th degree formula is used.
%
%  Integration region:
%
%    Points (X,Y,Z) such that:
%
%      X**2 + Y**2 + Z**2 <= 1.
%
%  Modified:
%
%    21 November 2000
%
%  Reference:
%
%    Arthur H Stroud,
%    Approximate Calculation of Multiple Integrals,
%    Prentice Hall, 1971.
%
%  Parameters:
%
%    Input, external FUNC, the name of the user supplied
%    function which evaluates F(X,Y,Z), of the form
%
%      function func ( x, y, z )
%      doubleprecision func
%      doubleprecision x
%      doubleprecision y
%      doubleprecision z
%
%    Output, doubleprecision RESULT, the approximate integral of the function.
%
persistent angle i j k norder quad volume w weight1 weight2 weight3 x xtab1 xtab2 xtab3 y z ; 

if isempty(norder), norder = 4; end;
%
if isempty(angle), angle=0; end;
if isempty(i), i=0; end;
if isempty(j), j=0; end;
if isempty(k), k=0; end;
if isempty(quad), quad=0; end;
if isempty(volume), volume=0; end;
if isempty(w), w=0; end;
if isempty(weight1), weight1=zeros(1,norder); end;
if isempty(weight2), weight2=zeros(1,norder); end;
if isempty(weight3), weight3=zeros(1,norder); end;
if isempty(x), x=0; end;
if isempty(xtab1), xtab1=zeros(1,norder); end;
if isempty(xtab2), xtab2=zeros(1,norder); end;
if isempty(xtab3), xtab3=zeros(1,norder); end;
if isempty(y), y=0; end;
if isempty(z), z=0; end;
%
%
%  This is the 5 point Gauss-Legendre rule,
%  but with the midpoint deleted, and with different weights.
%
xtab1(1) = -0.906179845938663992797626878299d+00;
xtab1(2) = -0.538469310105683091036314420700d+00;
xtab1(3) =  0.538469310105683091036314420700d+00;
xtab1(4) =  0.906179845938663992797626878299d+00;
weight1(1) = 0.19455533421780251826d+00;
weight1(2) = 0.13877799911553081506d+00;
weight1(3) = 0.13877799911553081506d+00;
weight1(4) = 0.19455533421780251826d+00;
%
%  Set XTAB2 and WEIGHT2.
%
for j = 1: norder;
angle = ((pi( ) .* ( (2 .* j) - 1 )) ./ ( 2 .* norder ));
xtab2(j) = cos( angle );
end; j = fix(norder+1);
weight2([1:norder]) = 1.0d+00;
%
%  Set XTAB3 and WEIGHT3 for the interval [-1,1].
%
[ norder, xtab3, weight3 ]=legendre_set( norder, xtab3, weight3 );
w = 3.0d+00 ./ 16.0d+00;
quad = 0.0d+00;
for i = 1: norder;
for j = 1: norder;
for k = 1: norder;
x = xtab1(i) .* sqrt( 1.0d+00 - xtab2(j).^2 ).* sqrt( 1.0d+00 - xtab3(k).^2 );
y = xtab1(i) .* xtab2(j) .* sqrt( 1.0d+00 - xtab3(k).^2 );
z = xtab1(i) .* xtab3(k);
quad = quad + w .* weight1(i) .* weight2(j) .* weight3(k) .* func( x, y, z );
end; k = fix(norder+1);
end; j = fix(norder+1);
end; i = fix(norder+1);
volume = sphere_unit_volume_3d( );
result = quad .* volume;
return;
end
function [func, result]=sphere_unit_14_3d( func, result );
%
%*******************************************************************************
%
%! SPHERE_UNIT_14_3D approximates an integral inside the unit sphere in 3D.
%
%
%  Discussion:
%
%    A 288 point 14-th degree formula is used, Stroud number S3:14-1.
%
%  Integration region:
%
%    Points (X,Y,Z) such that:
%
%      X**2 + Y**2 + Z**2 <= 1.
%
%  Reference:
%
%    Arthur H Stroud,
%    Approximate Calculation of Multiple Integrals,
%    Prentice Hall, 1971.
%
%  Modified:
%
%    21 November 2000
%
%  Parameters:
%
%    Input, external FUNC, the name of the user supplied
%    function which evaluates F(X,Y,Z), of the form
%
%      function func ( x, y, z )
%      doubleprecision func
%      doubleprecision x
%      doubleprecision y
%      doubleprecision z
%
%    Output, doubleprecision RESULT, the approximate integral of the function.
%
persistent i j k l m n pi quad r temp volume w1 w2 weight x xtab y ytab z ztab ; 

if isempty(i), i=0; end;
if isempty(j), j=0; end;
if isempty(k), k=0; end;
if isempty(l), l=0; end;
if isempty(m), m=0; end;
if isempty(n), n=0; end;
if isempty(pi), pi=0; end;
if isempty(quad), quad=0; end;
if isempty(r), r([1:4]) =[0.968160240d+00, 0.836031107d+00, 0.613371433d+00, 0.324253423d+00 ]; end;
if isempty(temp), temp=0; end;
if isempty(volume), volume=0; end;
if isempty(w1), w1=0; end;
if isempty(w2), w2=0; end;
if isempty(weight), weight([1:4]) =[0.076181268d+00, 0.126263673d+00, 0.098048133d+00, 0.032840260d+00 ]; end;
if isempty(x), x=0; end;
if isempty(xtab), xtab([1:5]) =[-0.151108275d+00, 0.315838353d+00, 0.346307112d+00, -0.101808787d+00, -0.409228403d+00 ]; end;
if isempty(y), y=0; end;
if isempty(ytab), ytab([1:5]) =[0.155240600d+00, 0.257049387d+00, 0.666277790d+00, 0.817386065d+00, 0.501547712d+00 ]; end;
if isempty(z), z=0; end;
if isempty(ztab), ztab([1:5]) =[0.976251323d+00, 0.913330032d+00, 0.660412970d+00, 0.567022920d+00, 0.762221757d+00 ]; end;
%
%
quad = 0.0d+00;
for m = 1: 4;
w1 = 125.0d+00 .* weight(m) ./ 3360.0d+00;
x = 0.525731112d+00 .* r(m);
y = 0.850650808d+00 .* r(m);
z = 0.0d+00;
for j = 1: 2;
x = - x;
for k = 1: 2;
y = - y;
for l = 1: 3;
[ x, y, z ]=d_swap3( x, y, z );
quad = quad + w1 .* func( x, y, z );
end; l = fix(3+1);
end; k = fix(2+1);
end; j = fix(2+1);
w2 = 143.0d+00 .* weight(m) ./ 3360.0d+00;
for n = 1: 5;
x = xtab(n) .* r(m);
y = ytab(n) .* r(m);
z = ztab(n) .* r(m);
for i = 1: 3;
temp = x;
x = z;
z = - y;
y = - temp;
for j = 1: 3;
[ x, y, z ]=d_swap3( x, y, z );
quad = quad + w2 .* func( x, y, z );
end; j = fix(3+1);
y = - y;
z = - z;
quad = quad + w2 .* func( x, y, z );
end; i = fix(3+1);
end; n = fix(5+1);
end; m = fix(4+1);
volume = sphere_unit_volume_3d( );
result = quad .* volume;
return;
end
function [func, result]=sphere_unit_15_3d( func, result );
%
%*******************************************************************************
%
%! SPHERE_UNIT_15_3D approximates an integral inside the unit sphere in 3D.
%
%
%  Discussion:
%
%    A 512 point 15-th degree formula is used.
%
%  Integration region:
%
%    Points (X,Y,Z) such that:
%
%      X**2 + Y**2 + Z**2 <= 1.
%
%  Reference:
%
%    Arthur H Stroud,
%    Approximate Calculation of Multiple Integrals,
%    Prentice Hall, 1971.
%
%  Modified:
%
%    28 October 2000
%
%  Parameters:
%
%    Input, external FUNC, the name of the user supplied
%    function which evaluates F(X,Y,Z), of the form
%
%      function func ( x, y, z )
%      doubleprecision func
%      doubleprecision x
%      doubleprecision y
%      doubleprecision z
%
%    Output, doubleprecision RESULT, the approximate integral of the function.
%
persistent cj ck i j k norder1 norder2 quad sj sk volume w weight1 weight2 x xtab1 xtab2 y z ; 

if isempty(norder1), norder1 = 4; end;
if isempty(norder2), norder2 = 8; end;
%
if isempty(cj), cj=0; end;
if isempty(ck), ck=0; end;
if isempty(i), i=0; end;
if isempty(j), j=0; end;
if isempty(k), k=0; end;
if isempty(quad), quad=0; end;
if isempty(sj), sj=0; end;
if isempty(sk), sk=0; end;
if isempty(volume), volume=0; end;
if isempty(w), w=0; end;
if isempty(weight1), weight1([1:norder1]) =[0.0328402599d+00, 0.0980481327d+00, 0.1262636728d+00, 0.0761812678d+00 ]; end;
if isempty(weight2), weight2=zeros(1,norder2); end;
if isempty(x), x=0; end;
if isempty(xtab1), xtab1([1:norder1]) =[0.3242534234d+00, 0.6133714327d+00, 0.8360311073d+00, 0.9681602395d+00 ]; end;
if isempty(xtab2), xtab2=zeros(1,norder2); end;
if isempty(y), y=0; end;
if isempty(z), z=0; end;
%
%
[ norder2, xtab2, weight2 ]=legendre_set( norder2, xtab2, weight2 );
w = 3.0d+00 ./ 32.0d+00;
quad = 0.0d+00;
for i = 1: norder1;
for j = 1: norder2;
sj = xtab2(j);
cj = sqrt( 1.0d+00 - sj.^2 );
for k = 1: 16;
sk = sin( ( k ) .* pi( ) ./ 8.0d+00 );
ck = cos( ( k ) .* pi( ) ./ 8.0d+00 );
x = xtab1(i) .* cj .* ck;
y = xtab1(i) .* cj .* sk;
z = xtab1(i) .* sj;
quad = quad + w .* weight1(i) .* weight2(j) .* func( x, y, z );
end; k = fix(16+1);
end; j = fix(norder2+1);
end; i = fix(norder1+1);
volume = sphere_unit_volume_3d( );
result = quad .* volume;
return;
end
function [sphere_unit_area_3dresult]=sphere_unit_area_3d( );
%
%*******************************************************************************
%
%! SPHERE_UNIT_AREA_3D computes the surface area of a unit sphere in 3D.
%
%
%  Modified:
%
%    20 December 2000
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Output, doubleprecision SPHERE_UNIT_AREA_3D, the area of the sphere.
%
sphere_unit_area_3dresult=[];
persistent sphere_unit_area_3d ; 

if isempty(sphere_unit_area_3dresult), sphere_unit_area_3dresult=0; end;
%
sphere_unit_area_3dresult = 4.0d+00 .* pi( );
return;
end
function [sphere_unit_area_ndresult, n ]=sphere_unit_area_nd( n );
%
%*******************************************************************************
%
%! SPHERE_UNIT_AREA_ND computes the surface area of a unit sphere in ND.
%
%
%  Discussion:
%
%    N   Area
%
%    2   2       * PI
%    3   4       * PI
%    4   2       * PI**2
%    5   (8/3)   * PI**2
%    6             PI**3
%    7   (16/15) * PI**3
%
%  Modified:
%
%    26 October 2000
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, integer N, the dimension of the space.
%
%    Output, doubleprecision SPHERE_UNIT_AREA_ND, the area of the sphere.
%
sphere_unit_area_ndresult=[];
persistent area i m sphere_unit_area_nd ; 

if isempty(area), area=0; end;
if isempty(i), i=0; end;
if isempty(m), m=0; end;
if isempty(sphere_unit_area_ndresult), sphere_unit_area_ndresult=0; end;
%
if( rem( n, 2 ) == 0 )
m = fix(fix(n ./ 2));
area = 2.0d+00 .*( pi( ) ).^m;
for i = 1: m-1;
area = area ./ ( i );
end; i = fix(m-1+1);
else;
m =fix(fix(( n - 1 ) ./ 2));
area = 2.0d+00.^n .*( pi() ).^m;
for i = m+1: 2.*m;
area = area ./ ( i );
end; i = fix(2.*m+1);
end;
sphere_unit_area_ndresult = area;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',n); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [func, n, result]=sphere_unit_f1_nd( func, n, result );
%
%*******************************************************************************
%
%! SPHERE_UNIT_F1_ND approximates an integral inside a unit sphere in ND.
%
%
%  Discussion:
%
%    An (N+1)*2**N point 5-th degree formula is used, Stroud number SN:5-6.
%
%  Integration region:
%
%    N dimensional points X() such that:
%
%      SUM ( I = 1 to N ) X(I)**2 <= 1.
%
%  Reference:
%
%    Arthur H Stroud,
%    Approximate Calculation of Multiple Integrals,
%    Prentice Hall, 1971.
%
%  Modified:
%
%    21 November 2000
%
%  Parameters:
%
%    Input, external FUNC, the name of the user supplied
%    function which evaluates F at the N-vector X, of the form
%
%      function func ( n, x )
%      integer n
%      doubleprecision func
%      doubleprecision x(n)
%
%    Input, integer N, the dimension of the sphere.
%
%    Output, doubleprecision RESULT, the approximate integral of the function.
%
%
persistent i ihi itemp j k khi ktemp pi quad t temp u u2 v volume w x y ; 

if isempty(i), i=0; end;
if isempty(ihi), ihi=0; end;
if isempty(itemp), itemp=0; end;
if isempty(j), j=0; end;
if isempty(k), k=0; end;
if isempty(khi), khi=0; end;
if isempty(ktemp), ktemp=0; end;
if isempty(pi), pi=0; end;
if isempty(quad), quad=0; end;
if isempty(t), t=0; end;
if isempty(temp), temp=0; end;
if isempty(u), u=0; end;
if isempty(u2), u2=0; end;
if isempty(v), v=0; end;
if isempty(volume), volume=0; end;
if isempty(w), w=0; end;
if isempty(x), x=zeros(1,n); end;
if isempty(y), y=0; end;
%
%
u2 =( 1.0d+00 - 2.0d+00 .* sqrt( 1.0d+00 ./ ( n + 4 ) ) ) ./ ( n + 2 );
u = sqrt( u2 );
x([1:n]) = - u;
w = 1.0d+00 ./ (( n + 1 ) .* 2.^n );
quad = 0.0d+00;
ihi = fix(2.^n);
for i = 1: ihi;
itemp = fix(i - 1);
for j = 1: n;
if( rem( itemp, 2 ) == 1 )
x(j) = - abs( x(j) );
else;
x(j) = abs( x(j) );
end;
itemp = fix(fix(itemp ./ 2));
end; j = fix(n+1);
quad = quad + w .* func( n, x );
end; i = fix(ihi+1);
temp = sqrt( ( n + 4 ) );
t = (sqrt( (2.0d+00 .* ( n + 1 )) ./ ( n + 2 ) ) ./( ( n ) .* temp ));
y =( 1.0d+00 + 2.0d+00 ./( ( n ) .* temp ) ) ./ ( n + 2 );
v = sqrt( y - t );
u = sqrt( y + ( n - 1 ) .* t );
khi = fix(2.^n);
for i = 1: n;
x([1:n]) = - v;
x(i) = - u;
for k = 1: khi;
ktemp = fix(k - 1);
for j = 1: n;
if( rem( ktemp, 2 ) == 1 )
x(j) = - abs( x(j) );
else;
x(j) = abs( x(j) );
end;
ktemp = fix(fix(ktemp ./ 2));
end; j = fix(n+1);
quad = quad + w .* func( n, x );
end; k = fix(khi+1);
x(i) = - v;
end; i = fix(n+1);
[volume , n ]=sphere_unit_volume_nd( n );
result = quad .* volume;
return;
end
function [func, n, result]=sphere_unit_f3_nd( func, n, result );
%
%*******************************************************************************
%
%! SPHERE_UNIT_F3_ND approximates an integral inside a unit sphere in ND.
%
%
%  Discussion:
%
%    A 2**(N+1)-1 point 5-th degree formula is used, Stroud number SN:5-4.
%
%  Integration region:
%
%    N dimensional points X() such that:
%
%      SUM ( I = 1 to N ) X(I)**2 <= 1.
%
%  Reference:
%
%    Arthur H Stroud,
%    Approximate Calculation of Multiple Integrals,
%    Prentice Hall, 1971.
%
%  Modified:
%
%    21 November 2000
%
%  Parameters:
%
%    Input, external FUNC, the name of the user supplied
%    function which evaluates F at the N-vector X, of the form
%
%      function func ( n, x )
%      integer n
%      doubleprecision func
%      doubleprecision x(n)
%
%    Input, integer N, the dimension of the sphere.
%
%    Output, doubleprecision RESULT, the approximate integral of the function.
%
%
persistent i j jtemp k pi quad ri s volume weight x ; 

if isempty(i), i=0; end;
if isempty(j), j=0; end;
if isempty(jtemp), jtemp=0; end;
if isempty(k), k=0; end;
if isempty(pi), pi=0; end;
if isempty(quad), quad=0; end;
if isempty(ri), ri=0; end;
if isempty(s), s=0; end;
if isempty(volume), volume=0; end;
if isempty(weight), weight=0; end;
if isempty(x), x=zeros(1,n); end;
%
%
quad = 0.0d+00;
%
%  The first point is the center of the sphere.
%
x([1:n]) = 0.0d+00;
weight = 4.0d+00 ./ ( n + 2 ).^2;
quad = quad + weight .* func( n, x );
s = 1.0d+00 ./ sqrt( ( n + 4 ) );
for i = 1: n;
ri = sqrt( fix(( i + 2 ) ./ ( n + 4 )) );
%
%  Set up the first point, with (I-1) zeroes, RI, and then N-I S's.
%
for j = 1: n;
if( j < i )
x(j) = 0.0d+00;
elseif( j == i ) ;
x(j) = ri;
else;
x(j) = s;
end;
end; j = fix(n+1);
weight = 2.0d+00.^fix((( i - n ) .* ( n + 4 ))./ ((( i + 1 ) .*( i + 2 )) .*( n + 2 ) ));
%
%  Now go through all sign permutations of the basic point.
%
for j = 1: 2.^(n+1-i);
jtemp = fix(j - 1);
for k = i: n;
if( rem( jtemp, 2 ) == 1 )
x(k) = - abs( x(k) );
else;
x(k) = abs( x(k) );
end;
jtemp = fix(fix(jtemp ./ 2));
end; k = fix(n+1);
quad = quad + weight .* func( n, x );
end; j = fix(2.^(n+1-i)+1);
end; i = fix(n+1);
[volume , n ]=sphere_unit_volume_nd( n );
result = quad .* volume;
return;
end
function [func, result]=sphere_unit_surface_07_3d( func, result );
%
%*******************************************************************************
%
%! SPHERE_UNIT_SURFACE_07_3D approximates surface integrals on the unit sphere in 3D.
%
%
%  Discussion:
%
%    A 32 point 7-th degree formula is used.
%
%  Integration region:
%
%    Points (X,Y,Z) such that:
%
%      X**2 + Y**2 + Z**2 = 1.
%
%  Reference:
%
%    Arthur H Stroud,
%    Approximate Calculation of Multiple Integrals,
%    Prentice Hall, 1971.
%
%  Modified:
%
%    21 November 2000
%
%  Parameters:
%
%    Input, external FUNC, the name of the user supplied
%    function which evaluates F(X,Y,Z), of the form
%
%      function func ( x, y, z )
%      doubleprecision func
%      doubleprecision x
%      doubleprecision y
%      doubleprecision z
%
%    Output, doubleprecision RESULT, the approximate integral of the function.
%
persistent angle i j k norder1 norder2 norder3 quad volume weight1 weight2 weight3 x xtab1 xtab2 xtab3 y z ; 

if isempty(norder1), norder1 = 2; end;
if isempty(norder2), norder2 = 4; end;
if isempty(norder3), norder3 = 4; end;
%
if isempty(angle), angle=0; end;
if isempty(i), i=0; end;
if isempty(j), j=0; end;
if isempty(k), k=0; end;
if isempty(quad), quad=0; end;
if isempty(volume), volume=0; end;
if isempty(weight1), weight1=zeros(1,norder1); end;
if isempty(weight2), weight2=zeros(1,norder2); end;
if isempty(weight3), weight3=zeros(1,norder3); end;
if isempty(x), x=0; end;
if isempty(xtab1), xtab1=zeros(1,norder1); end;
if isempty(xtab2), xtab2=zeros(1,norder2); end;
if isempty(xtab3), xtab3=zeros(1,norder3); end;
if isempty(y), y=0; end;
if isempty(z), z=0; end;
%
%
%  Set XTAB1 and WATE1.
%
xtab1(1) = -1.0d+00;
xtab1(2) =  1.0d+00;
weight1(1) = 1.0d+00;
weight1(2) = 1.0d+00;
%
%  Set XTAB2 and WATE2.
%
for j = 1: norder2;
angle = ((pi( ) .* ( (2 .* j) - 1 )) ./ ( 2 .* norder2 ));
xtab2(j) = cos( angle );
end; j = fix(norder2+1);
weight2([1:norder2]) = 1.0d+00 ./ ( 4 .* norder2 );
%
%  Set XTAB3 and WATE3.
%
[ norder3, xtab3, weight3 ]=legendre_set( norder3, xtab3, weight3 );
quad = 0.0d+00;
for i = 1: norder1;
for j = 1: norder2;
for k = 1: norder3;
x = xtab1(i) .* sqrt( 1.0d+00 - xtab2(j).^2 ).* sqrt( 1.0d+00 - xtab3(k).^2 );
y = xtab1(i) .* xtab2(j) .* sqrt( 1.0d+00 - xtab3(k).^2 );
z = xtab1(i) .* xtab3(k);
quad = quad + weight1(i) .* weight2(j) .* weight3(k) .* func( x, y, z );
end; k = fix(norder3+1);
end; j = fix(norder2+1);
end; i = fix(norder1+1);
volume = sphere_unit_area_3d( );
result = quad .* volume;
return;
end
function [func, result]=sphere_unit_surface_11_3d( func, result );
%
%*******************************************************************************
%
%! SPHERE_UNIT_SURFACE_11_3D approximates surface integrals on the unit sphere in 3D.
%
%
%  Discussion:
%
%    A 50 point 11-th degree formula is used, Stroud number U3:11-1.
%
%  Integration region:
%
%    Points (X,Y,Z) such that:
%
%      X**2 + Y**2 + Z**2 = 1.
%
%  Reference:
%
%    A D McLaren,
%    Math. Comp.
%    Volume 17, pages 361-383, 1963.
%
%    Arthur H Stroud,
%    Approximate Calculation of Multiple Integrals,
%    Prentice Hall, 1971.
%
%  Modified:
%
%    07 November 2000
%
%  Parameters:
%
%    Input, external FUNC, the name of the user supplied
%    function which evaluates F(X,Y,Z), of the form
%
%      function func ( x, y, z )
%      doubleprecision func
%      doubleprecision x
%      doubleprecision y
%      doubleprecision z
%
%    Output, doubleprecision RESULT, the approximate integral of the function.
%
persistent i j k l pi quad volume w1 w2 w3 w4 x y z ; 

if isempty(i), i=0; end;
if isempty(j), j=0; end;
if isempty(k), k=0; end;
if isempty(l), l=0; end;
if isempty(pi), pi=0; end;
if isempty(quad), quad=0; end;
if isempty(volume), volume=0; end;
if isempty(w1), w1=0; end;
if isempty(w2), w2=0; end;
if isempty(w3), w3=0; end;
if isempty(w4), w4=0; end;
if isempty(x), x=0; end;
if isempty(y), y=0; end;
if isempty(z), z=0; end;
%
%
quad = 0.0d+00;
w1 = 9216.0d+00 ./ 725760.0d+00;
x = 1.0d+00;
y = 0.0d+00;
z = 0.0d+00;
for i = 1: 2;
x = - x;
for j = 1: 3;
[ x, y, z ]=d_swap3( x, y, z );
quad = quad + w1 .* func( x, y, z );
end; j = fix(3+1);
end; i = fix(2+1);
w2 = 16384.0d+00 ./ 725760.0d+00;
x = sqrt( 0.5d+00 );
y = sqrt( 0.5d+00 );
z = 0.0d+00;
for i = 1: 2;
x = - x;
for j = 1: 2;
y = - y;
for k = 1: 3;
[ x, y, z ]=d_swap3( x, y, z );
quad = quad + w2 .* func( x, y, z );
end; k = fix(3+1);
end; j = fix(2+1);
end; i = fix(2+1);
w3 = 15309.0d+00 ./ 725760.0d+00;
x = sqrt( 1.0d+00 ./ 3.0d+00 );
y = sqrt( 1.0d+00 ./ 3.0d+00 );
z = sqrt( 1.0d+00 ./ 3.0d+00 );
for i = 1: 2;
x = - x;
for j = 1: 2;
y = - y;
for k = 1: 2;
z = - z;
quad = quad + w3 .* func( x, y, z );
end; k = fix(2+1);
end; j = fix(2+1);
end; i = fix(2+1);
w4 = 14641.0d+00 ./ 725760.0d+00;
x = sqrt( 1.0d+00 ./ 11.0d+00 );
y = sqrt( 1.0d+00 ./ 11.0d+00 );
z = 3.0d+00 .* sqrt( 1.0d+00 ./ 11.0d+00 );
for i = 1: 2;
x = - x;
for j = 1: 2;
y = - y;
for k = 1: 2;
z = - z;
for l = 1: 3;
[ x, y, z ]=d_swap3( x, y, z );
quad = quad + w4 .* func( x, y, z );
end; l = fix(3+1);
end; k = fix(2+1);
end; j = fix(2+1);
end; i = fix(2+1);
volume = sphere_unit_area_3d( );
result = quad .* volume;
return;
end
function [func, result]=sphere_unit_surface_14_3d( func, result );
%
%*******************************************************************************
%
%! SPHERE_UNIT_SURFACE_14_3D approximates surface integrals on the unit sphere in 3D.
%
%
%  Discussion:
%
%    A 72 point 14-th degree formula is used, Stroud number U3:14-1.
%
%  Integration region:
%
%    Points (X,Y,Z) such that:
%
%      X**2 + Y**2 + Z**2 = 1.
%
%  Reference:
%
%    A D McLaren,
%    Math. Comp.
%    Volume 17, pages 361-383, 1963.
%
%    Arthur H Stroud,
%    Approximate Calculation of Multiple Integrals,
%    Prentice Hall, 1971.
%
%  Modified:
%
%    21 November 2000
%
%  Parameters:
%
%    Input, external FUNC, the name of the user supplied
%    function which evaluates F(X,Y,Z), of the form
%
%      function func ( x, y, z )
%      doubleprecision func
%      doubleprecision x
%      doubleprecision y
%      doubleprecision z
%
%    Output, doubleprecision RESULT, the approximate integral of the function.
%
persistent i j k pi quad temp volume w1 w2 x xtab y ytab z ztab ; 

if isempty(i), i=0; end;
if isempty(j), j=0; end;
if isempty(k), k=0; end;
if isempty(pi), pi=0; end;
if isempty(quad), quad=0; end;
if isempty(temp), temp=0; end;
if isempty(volume), volume=0; end;
if isempty(w1), w1=0; end;
if isempty(w2), w2=0; end;
if isempty(x), x=0; end;
if isempty(xtab), xtab([1:5]) =[-0.151108275d+00, 0.315838353d+00, 0.346307112d+00, -0.101808787d+00,-0.409228403d+00 ]; end;
if isempty(y), y=0; end;
if isempty(ytab), ytab([1:5]) =[0.155240600d+00, 0.257049387d+00, 0.666277790d+00,  0.817386065d+00,0.501547712d+00 ]; end;
if isempty(z), z=0; end;
if isempty(ztab), ztab([1:5]) =[0.976251323d+00, 0.913330032d+00, 0.660412970d+00,  0.567022920d+00,0.762221757d+00 ]; end;
%
%
quad = 0.0d+00;
w1 = 125.0d+00 ./ 10080.0d+00;
x = 0.525731112d+00;
y = 0.850650808d+00;
z = 0.0d+00;
for i = 1: 2;
x = - x;
for j = 1: 2;
y = - y;
for k = 1: 3;
[ x, y, z ]=d_swap3( x, y, z );
quad = quad + w1 .* func( x, y, z );
end; k = fix(3+1);
end; j = fix(2+1);
end; i = fix(2+1);
w2 = 143.0d+00 ./ 10080.0d+00;
for i = 1: 5;
x = xtab(i);
y = ytab(i);
z = ztab(i);
for j = 1: 3;
temp = x;
x = z;
z = - y;
y = - temp;
for k = 1: 3;
[ x, y, z ]=d_swap3( x, y, z );
quad = quad + w2 .* func( x, y, z );
end; k = fix(3+1);
y = - y;
z = - z;
quad = quad + w2 .* func( x, y, z );
end; j = fix(3+1);
end; i = fix(5+1);
volume = sphere_unit_area_3d( );
result = quad .* volume;
return;
end
function [func, result]=sphere_unit_surface_15_3d( func, result );
%
%*******************************************************************************
%
%! SPHERE_UNIT_SURFACE_15_3D approximates surface integrals on the unit sphere in 3D.
%
%
%  Discussion:
%
%    A 128 point 15-th degree spherical product Gauss formula is used.
%
%  Integration region:
%
%    Points (X,Y,Z) such that:
%
%      X**2 + Y**2 + Z**2 = 1.
%
%  Reference:
%
%    Arthur H Stroud,
%    Approximate Calculation of Multiple Integrals,
%    Prentice Hall, 1971.
%
%  Modified:
%
%    21 November 2000
%
%  Parameters:
%
%    Input, external FUNC, the name of the user supplied
%    function which evaluates F(X,Y,Z), of the form
%
%      function func ( x, y, z )
%      doubleprecision func
%      doubleprecision x
%      doubleprecision y
%      doubleprecision z
%
%    Output, doubleprecision RESULT, the approximate integral of the function.
%
persistent angle j k norder quad volume weight x xtab y z ; 

if isempty(norder), norder = 8; end;
%
if isempty(angle), angle=0; end;
if isempty(j), j=0; end;
if isempty(k), k=0; end;
if isempty(quad), quad=0; end;
if isempty(volume), volume=0; end;
if isempty(weight), weight=zeros(1,norder); end;
if isempty(x), x=0; end;
if isempty(xtab), xtab=zeros(1,norder); end;
if isempty(y), y=0; end;
if isempty(z), z=0; end;
%
%
[ norder, xtab, weight ]=legendre_set( norder, xtab, weight );
weight([1:norder]) = weight([1:norder]) ./ 32.0d+00;
quad = 0.0d+00;
for j = 1: norder;
for k = 1: 16;
angle = ( k ) .* pi( ) ./ 8.0d+00;
x = sqrt( 1.0d+00 - xtab(j).^2 ) .* cos( angle );
y = sqrt( 1.0d+00 - xtab(j).^2 ) .* sin( angle );
z = xtab(j);
quad = quad + weight(j) .* func( x, y, z );
end; k = fix(16+1);
end; j = fix(norder+1);
volume = sphere_unit_area_3d( );
result = quad .* volume;
return;
end
function [func, n, result]=sphere_unit_surface_3_nd( func, n, result );
%
%*******************************************************************************
%
%! SPHERE_UNIT_SURFACE_3_ND approximates surface integrals on the unit sphere in ND.
%
%
%  Discussion:
%
%    A 2*N point 3rd degree formula is used, Stroud number UN:3-1.
%
%  Integration region:
%
%    N dimensional points X() such that:
%
%      SUM ( I = 1 to N ) X(I)**2 = 1.
%
%  Reference:
%
%    Arthur H Stroud,
%    Approximate Calculation of Multiple Integrals,
%    Prentice Hall, 1971.
%
%  Modified:
%
%    21 November 2000
%
%  Parameters:
%
%    Input, external FUNC, the name of the user supplied
%    function which evaluates F(X), at the N dimensional point
%    X, of the form
%
%      function func ( n, x )
%      integer n
%      doubleprecision func
%      doubleprecision x(n)
%
%    Input, integer N, the dimension of the space.
%
%    Output, doubleprecision RESULT, the approximate integral of the function.
%
%
persistent i pi quad volume w x ; 

if isempty(i), i=0; end;
if isempty(pi), pi=0; end;
if isempty(quad), quad=0; end;
if isempty(volume), volume=0; end;
if isempty(w), w=0; end;
if isempty(x), x=zeros(1,n); end;
%
%
x([1:n]) = 0.0d+00;
w = 1.0d+00 ./ ( 2 .* n );
quad = 0.0d+00;
for i = 1: n;
x(i) = 1.0d+00;
quad = quad + w .* func( n, x );
x(i) = - 1.0d+00;
quad = quad + w .* func( n, x );
x(i) = 0.0d+00;
end; i = fix(n+1);
[volume , n ]=sphere_unit_area_nd( n );
result = quad .* volume;
return;
end
function [func, n, result]=sphere_unit_surface_4_nd( func, n, result );
%
%*******************************************************************************
%
%! SPHERE_UNIT_SURFACE_4_ND approximates surface integrals on the unit sphere in ND.
%
%
%  Discussion:
%
%    A 2*N**2 point 5th degree formula is used, Stroud number UN:5-1.
%
%  Integration region:
%
%    N dimensional points X() such that:
%
%      SUM ( I = 1 to N ) X(I)**2 = 1.
%
%  Reference:
%
%    Arthur H Stroud,
%    Approximate Calculation of Multiple Integrals,
%    Prentice Hall, 1971.
%
%  Modified:
%
%    21 November 2000
%
%  Parameters:
%
%    Input, external FUNC, the name of the user supplied
%    function which evaluates F(X), at the N dimensional point
%    X, of the form
%
%      function func ( n, x )
%      integer n
%      doubleprecision func
%      doubleprecision x(n)
%
%    Input, integer N, the dimension of the space.
%
%    Output, doubleprecision RESULT, the approximate integral of the function.
%
%
persistent i j pi quad s volume w1 w2 x ; 

if isempty(i), i=0; end;
if isempty(j), j=0; end;
if isempty(pi), pi=0; end;
if isempty(quad), quad=0; end;
if isempty(s), s=0; end;
if isempty(volume), volume=0; end;
if isempty(w1), w1=0; end;
if isempty(w2), w2=0; end;
if isempty(x), x=zeros(1,n); end;
%
%
x([1:n]) = 0.0d+00;
w1 = fix(( 4 - n ) ./ ( (2 .* n) .*( n + 2 ) ));
quad = 0.0d+00;
for i = 1: n;
x(i) = 1.0d+00;
quad = quad + w1 .* func( n, x );
x(i) = - 1.0d+00;
quad = quad + w1 .* func( n, x );
x(i) = 0.0d+00;
end; i = fix(n+1);
s = 1.0d+00 ./ sqrt( 2.0d+00 );
w2 = 1.0d+00 ./ ( n .*( n + 2 ) );
for i = 1: n;
x(i) = s;
for j = i+1: n;
x(j) = s;
quad = quad + w2 .* func( n, x );
x(j) = -s;
quad = quad + w2 .* func( n, x );
x(j) = 0.0d+00;
end; j = fix(n+1);
x(i) = - s;
for j = i+1: n;
x(j) = s;
quad = quad + w2 .* func( n, x );
x(j) = - s;
quad = quad + w2 .* func( n, x );
x(j) = 0.0d+00;
end; j = fix(n+1);
x(i) = 0.0d+00;
end; i = fix(n+1);
[volume , n ]=sphere_unit_area_nd( n );
result = quad .* volume;
return;
end
function [func, n, result]=sphere_unit_surface_5_nd( func, n, result );
%
%*******************************************************************************
%
%! SPHERE_UNIT_SURFACE_5_ND approximates surface integrals on the unit sphere in ND.
%
%
%  Discussion:
%
%    A 2*N+2**N points 5-th degree formula is used, Stroud number UN:5-2.
%
%  Integration region:
%
%    N dimensional points X() such that:
%
%      SUM ( I = 1 to N ) X(I)**2 = 1.
%
%  Reference:
%
%    Arthur H Stroud,
%    Approximate Calculation of Multiple Integrals,
%    Prentice Hall, 1971.
%
%  Modified:
%
%    21 November 2000
%
%  Parameters:
%
%    Input, external FUNC, the name of the user supplied
%    function which evaluates F(X), at the N dimensional point
%    X, of the form
%
%      function func ( n, x )
%      integer n
%      doubleprecision func
%      doubleprecision x(n)
%
%    Input, integer N, the dimension of the space.
%
%    Output, doubleprecision RESULT, the approximate integral of the function.
%
%
persistent i iadd ihi ix more ncard quad volume w1 w2 x x1 x2 ; 

if isempty(i), i=0; end;
if isempty(iadd), iadd=0; end;
if isempty(ihi), ihi=0; end;
if isempty(ix), ix=zeros(1,n); end;
if isempty(more), more=false; end;
if isempty(ncard), ncard=0; end;
if isempty(quad), quad=0; end;
if isempty(volume), volume=0; end;
if isempty(w1), w1=0; end;
if isempty(w2), w2=0; end;
if isempty(x), x=zeros(1,n); end;
if isempty(x1), x1=0; end;
if isempty(x2), x2=0; end;
%
%
x1 = 1.0d+00;
x2 = 1.0d+00 ./ sqrt( ( n ) );
w1 = 1.0d+00 ./ ( n .*( n + 2 ) );
w2 = fix(( n ) ./ ((( n + 2 ) .* 2).^n ));
x([1:n]) = 0.0d+00;
quad = 0.0d+00;
for i = 1: n;
x(i) = x1;
quad = quad + w1 .* func( n, x );
x(i) = -x1;
quad = quad + w1 .* func( n, x );
x(i) = 0.0d+00;
end; i = fix(n+1);
more = false;
ihi = fix(2.^n);
x([1:n]) = - x2;
for i = 1: ihi;
[ n, ix, more, ncard, iadd ]=subset_next( n, ix, more, ncard, iadd );
if( iadd ~= 0 )
x(iadd) = - x(iadd);
end;
quad = quad + w2 .* func( n, x );
end; i = fix(ihi+1);
[volume , n ]=sphere_unit_area_nd( n );
result = quad .* volume;
return;
end
function [func, n, result]=sphere_unit_surface_7_1_nd( func, n, result );
%
%*******************************************************************************
%
%! SPHERE_UNIT_SURFACE_7_1_ND approximates surface integrals on the unit sphere in ND.
%
%
%  Discussion:
%
%    A 2**N + 2*N**2 point 7th degree formula is used, Stroud number UN:7-1.
%
%  Integration region:
%
%    N dimensional points X() such that:
%
%      SUM ( I = 1 to N ) X(I)**2 = 1.
%
%  Reference:
%
%    Arthur H Stroud,
%    Approximate Calculation of Multiple Integrals,
%    Prentice Hall, 1971.
%
%  Modified:
%
%    20 December 2000
%
%  Parameters:
%
%    Input, external FUNC, the name of the user supplied
%    function which evaluates F(X), at the N dimensional point
%    X, of the form
%
%      function func ( n, x )
%      integer n
%      doubleprecision func
%      doubleprecision x(n)
%
%    Input, integer N, the dimension of the space.
%
%    Output, doubleprecision RESULT, the approximate integral of the function.
%
%
persistent i iadd ix j jhi more ncard quad volume w1 w2 w3 x x1 x2 x3 ; 

if isempty(i), i=0; end;
if isempty(iadd), iadd=0; end;
if isempty(ix), ix=zeros(1,n); end;
if isempty(j), j=0; end;
if isempty(jhi), jhi=0; end;
if isempty(more), more=false; end;
if isempty(ncard), ncard=0; end;
if isempty(quad), quad=0; end;
if isempty(volume), volume=0; end;
if isempty(w1), w1=0; end;
if isempty(w2), w2=0; end;
if isempty(w3), w3=0; end;
if isempty(x), x=zeros(1,n); end;
if isempty(x1), x1=0; end;
if isempty(x2), x2=0; end;
if isempty(x3), x3=0; end;
%
%
w1 = fix(( 8 - n ) ./ ( (n .*( n + 2 )) .*( n + 4 ) ));
w2 = fix(( n.^3 ) ./ ( 2.^(((n .* n) .*( n + 2 )) .*( n + 4 )) ));
w3 = 4.0d+00 ./ ( n .*( n + 2 ) .*( n + 4 ) );
x1 = 1.0d+00;
x2 = 1.0d+00 ./ sqrt( ( n ) );
x3 = 1.0d+00 ./ sqrt( 2.0d+00 );
x([1:n]) = 0.0d+00;
quad = 0.0d+00;
%
%  First term.
%
for i = 1: n;
x(i) = x1;
quad = quad + w1 .* func( n, x );
x(i) = -x1;
quad = quad + w1 .* func( n, x );
x(i) = 0.0d+00;
end; i = fix(n+1);
%
%  Second term.
%
x([1:n]) = -x2;
more = false;
jhi = fix(2.^n);
for j = 1: jhi;
[ n, ix, more, ncard, iadd ]=subset_next( n, ix, more, ncard, iadd );
if( iadd ~= 0 )
x(iadd) = -x(iadd);
end;
quad = quad + w2 .* func( n, x );
end; j = fix(jhi+1);
%
%  Third term.
%
x([1:n]) = 0.0d+00;
for i = 1: n-1;
for j = i+1: n;
x(i) = x3;
x(j) = x3;
quad = quad + w3 .* func( n, x );
x(i) = -x3;
x(j) = x3;
quad = quad + w3 .* func( n, x );
x(i) = x3;
x(j) = -x3;
quad = quad + w3 .* func( n, x );
x(i) = -x3;
x(j) = -x3;
quad = quad + w3 .* func( n, x );
x(i) = 0.0d+00;
x(j) = 0.0d+00;
end; j = fix(n+1);
end; i = fix(n-1+1);
[volume , n ]=sphere_unit_area_nd( n );
result = quad .* volume;
return;
end
function [func, n, result]=sphere_unit_surface_7_2_nd( func, n, result );
%
%*******************************************************************************
%
%! SPHERE_UNIT_SURFACE_7_2_ND approximates surface integrals on the unit sphere in ND.
%
%
%  Discussion:
%
%    A 2**N * ( N + 1 ) point 7th degree formula is used, Stroud number UN:7-2.
%
%    Some of the weights in this quadrature formula are negative.
%
%  Integration region:
%
%    N dimensional points X() such that:
%
%      SUM ( I = 1 to N ) X(I)**2 = 1.
%
%  Reference:
%
%    Arthur H Stroud,
%    Approximate Calculation of Multiple Integrals,
%    Prentice Hall, 1971.
%
%  Modified:
%
%    20 December 2000
%
%  Parameters:
%
%    Input, external FUNC, the name of the user supplied
%    function which evaluates F(X), at the N dimensional point
%    X, of the form
%
%      function func ( n, x )
%      integer n
%      doubleprecision func
%      doubleprecision x(n)
%
%    Input, integer N, the dimension of the space.
%
%    Output, doubleprecision RESULT, the approximate integral of the function.
%
%
persistent i iadd ix j jhi more ncard quad volume w1 w2 x x1 x2 x3 ; 

if isempty(iadd), iadd=0; end;
if isempty(i), i=0; end;
if isempty(ix), ix=zeros(1,n); end;
if isempty(j), j=0; end;
if isempty(jhi), jhi=0; end;
if isempty(more), more=false; end;
if isempty(ncard), ncard=0; end;
if isempty(quad), quad=0; end;
if isempty(volume), volume=0; end;
if isempty(w1), w1=0; end;
if isempty(w2), w2=0; end;
if isempty(x), x=zeros(1,n); end;
if isempty(x1), x1=0; end;
if isempty(x2), x2=0; end;
if isempty(x3), x3=0; end;
%
%
x([1:n]) = 0.0d+00;
w1 = - fix(( n.^2 ) ./ ( 2.^((n+3) .*( n + 2 )) ));
w2 = fix((( n + 4 ).^2 ) ./ ( 2.^(((n+3) .* n) .*( n + 2 )) ));
x1 = 1.0d+00 ./ sqrt( ( n ) );
x2 = sqrt( 5.0d+00 ./ ( n + 4 ) );
x3 = 1.0d+00 ./ sqrt( ( n + 4 ) );
quad = 0.0d+00;
x([1:n]) = -x1;
more = false;
jhi = fix(2.^n);
for j = 1: jhi;
[ n, ix, more, ncard, iadd ]=subset_next( n, ix, more, ncard, iadd );
if( iadd ~= 0 )
x(iadd) = -x(iadd);
end;
quad = quad + w1 .* func( n, x );
end; j = fix(jhi+1);
for i = 1: n;
x([1:n]) = -x3;
x(i) = -x2;
more = false;
for j = 1: jhi;
[ n, ix, more, ncard, iadd ]=subset_next( n, ix, more, ncard, iadd );
if( iadd ~= 0 )
x(iadd) = - x(iadd);
end;
quad = quad + w2 .* func( n, x );
end; j = fix(jhi+1);
end; i = fix(n+1);
[volume , n ]=sphere_unit_area_nd( n );
result = quad .* volume;
return;
end
function [sphere_unit_volume_3dresult]=sphere_unit_volume_3d( );
%
%*******************************************************************************
%
%! SPHERE_UNIT_VOLUME_3D computes the volume of a unit sphere in 3D.
%
%
%  Modified:
%
%    20 December 2000
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Output, doubleprecision SPHERE_UNIT_VOLUME_3D, the volume of the sphere.
%
sphere_unit_volume_3dresult=[];
persistent sphere_unit_volume_3d ; 

if isempty(sphere_unit_volume_3dresult), sphere_unit_volume_3dresult=0; end;
%
sphere_unit_volume_3dresult =( 4.0d+00 ./ 3.0d+00 ) .* pi( );
return;
end
function [sphere_unit_volume_ndresult, n ]=sphere_unit_volume_nd( n );
%
%*******************************************************************************
%
%! SPHERE_UNIT_VOLUME_ND computes the volume of a unit sphere in ND.
%
%
%  Discussion:
%
%    N  Volume
%
%    2             PI
%    3  (4/3)    * PI
%    4  (1/2)    * PI**2
%    5  (8/15)   * PI**2
%    6  (1/6)    * PI**3
%    7  (16/105) * PI**3
%
%  Modified:
%
%    26 October 2000
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, integer N, the dimension of the space.
%
%    Output, doubleprecision SPHERE_UNIT_VOLUME_ND, the volume of the sphere.
%
sphere_unit_volume_ndresult=[];
persistent i m sphere_unit_volume_nd volume ; 

if isempty(i), i=0; end;
if isempty(m), m=0; end;
if isempty(sphere_unit_volume_ndresult), sphere_unit_volume_ndresult=0; end;
if isempty(volume), volume=0; end;
%
if( rem( n, 2 ) == 0 )
m = fix(fix(n ./ 2));
volume =( pi() ).^m;
for i = 1: m;
volume = volume ./ ( i );
end; i = fix(m+1);
else;
m =fix(fix(( n - 1 ) ./ 2));
volume =( pi() ).^m .* 2.0d+00.^n;
for i = m+1: 2.*m+1;
volume = volume ./ ( i );
end; i = fix(2.*m+1+1);
end;
sphere_unit_volume_ndresult = volume;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',n); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [sphere_volume_3dresult, r ]=sphere_volume_3d( r );
%
%*******************************************************************************
%
%! SPHERE_VOLUME_3D computes the volume of a sphere in 3D.
%
%
%  Modified:
%
%    12 December 2000
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, doubleprecision R, the radius of the sphere.
%
%    Output, doubleprecision SPHERE_VOLUME_3D, the volume of the sphere.
%
sphere_volume_3dresult=[];
persistent sphere_volume_3d ; 

if isempty(sphere_volume_3dresult), sphere_volume_3dresult=0; end;
%
sphere_volume_3dresult =( 4.0d+00 ./ 3.0d+00 ) .* pi( ) .* r.^3;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',r); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [sphere_volume_ndresult, n, r ]=sphere_volume_nd( n, r );
%
%*******************************************************************************
%
%! SPHERE_VOLUME_ND computes the volume of a sphere in ND.
%
%
%  Discussion:
%
%    N  Volume
%
%    2             PI    * R**2
%    3  (4/3)    * PI    * R**3
%    4  (1/2)    * PI**2 * R**4
%    5  (8/15)   * PI**2 * R**5
%    6  (1/6)    * PI**3 * R**6
%    7  (16/105) * PI**3 * R**7
%
%  Modified:
%
%    26 October 2000
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, integer N, the dimension of the space.
%
%    Input, doubleprecision R, the radius of the sphere.
%
%    Output, doubleprecision SPHERE_VOLUME_ND, the volume of the sphere.
%
sphere_volume_ndresult=[];
persistent sphere_volume_nd ; 

if isempty(sphere_volume_ndresult), sphere_volume_ndresult=0; end;
%
sphere_volume_ndresult = sphere_unit_volume_nd( n ) .* r.^n;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(2)), assignin('caller','FUntemp',r); evalin('caller',[inputname(2),'=FUntemp;']); end
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',n); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [func, xc, yc, r, norder, xtab, ytab, weight, result]=square_sum( func, xc, yc, r, norder, xtab, ytab, weight, result );
%
%*******************************************************************************
%
%! SQUARE_SUM carries out a quadrature rule over a square.
%
%
%  Integration interval:
%
%    abs ( X - XC ) <= R
%    abs ( Y - YC ) <= R
%
%  Modified:
%
%    20 November 2000
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, EXTERNAL FUNC, the name of the function to be
%    integrated.  The user must declare the name an EXTERNAL
%    parameter in the calling program, pass the name of the
%    function in FUNC, and write a function of the form
%
%      function FUNC(X,Y)
%
%    which evaluates the function at the point (X,Y).
%
%    Input, integer NORDER, the order of the rule.
%
%    Input, doubleprecision XTAB(NORDER), YTAB(NORDER), the abscissas of the rule.
%
%    Input, doubleprecision WEIGHT(NORDER), the weights of the rule.
%
%    Output, doubleprecision RESULT, the approximate integral of the function.
%
%
persistent i quad volume x y ; 

if isempty(i), i=0; end;
if isempty(quad), quad=0; end;
if isempty(volume), volume=0; end;
if isempty(x), x=0; end;
if isempty(y), y=0; end;
%
%
quad = 0.0d+00;
for i = 1: norder;
x = xc + r .* xtab(i);
y = yc + r .* ytab(i);
quad = quad + 0.25d+00 .* weight(i) .* func( x, y );
end; i = fix(norder+1);
volume = 4.0d+00 .* r.^2;
result = quad .* volume;
return;
end
function [rule, norder, xtab, ytab, weight]=square_unit_set( rule, norder, xtab, ytab, weight );
%
%*******************************************************************************
%
%! SQUARE_UNIT_SET sets weights and abscissas for quadrature within a unit square.
%
%
%  Integration interval:
%
%    -1 <= X <= 1,
%    -1 <= Y <= 1.
%
%  References:
%
%    Strang and Fix,
%    An Analysis of the Finite Element Method,
%    Prentice Hall, 1973.
%
%    Arthur H Stroud,
%    Approximate Calculation of Multiple Integrals,
%    Prentice Hall, 1971.
%
%  Modified:
%
%    07 November 2000
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, integer RULE, the rule number.
%    1, a 1 point 1st degree rule.
%    2, a 4 point 3rd degree rule.
%    3, a 9 point 5th degree rule.
%    4, a 12 point 7-th degree rule, Stroud number C2:7-1.
%    5, a 13 point 7-th degree rule, Stroud number C2:7-3.
%    6, a 64 point 15-th degree product rule.
%
%    Output, integer NORDER, the order of the rule.
%
%    Output, doubleprecision XTAB(*), YTAB(*), the NORDER abscissas of the rule.
%
%    Output, doubleprecision WEIGHT(*), the weights of the rule.
%
persistent a c i j k norder2 r s t w w1 w2 w3 weight2 xtab2 z ; 

if isempty(norder2), norder2 = 8; end;
%
if isempty(a), a=0; end;
if isempty(c), c=0; end;
if isempty(i), i=0; end;
if isempty(j), j=0; end;
if isempty(k), k=0; end;
if isempty(r), r=0; end;
if isempty(s), s=0; end;
if isempty(t), t=0; end;
if isempty(w), w=0; end;
if isempty(w1), w1=0; end;
if isempty(w2), w2=0; end;
if isempty(w3), w3=0; end;
weight_shape=size(weight);weight=reshape(weight,1,[]);
if isempty(weight2), weight2=zeros(1,norder2); end;
xtab_shape=size(xtab);xtab=reshape(xtab,1,[]);
if isempty(xtab2), xtab2=zeros(1,norder2); end;
ytab_shape=size(ytab);ytab=reshape(ytab,1,[]);
if isempty(z), z=0; end;
%
if( rule == 1 )
norder = 1;
weight(1) = 4.0d+00;
xtab(1) = 0.0d+00;
ytab(1) = 0.0d+00;
elseif( rule == 2 ) ;
a = 1.0d+00;
s = 1.0d+00 ./ sqrt( 3.0d+00 );
norder = 4;
xtab([1:4]) =[ -s, +s, -s, +s ];
ytab([1:4]) =[ -s, -s, +s, +s ];
weight([1:4]) =[  a,  a,  a,  a ];
elseif( rule == 3 ) ;
s = sqrt( 0.6d+00 );
z = 0.0d+00;
w1 = 64.0d+00 ./ 81.0d+00;
w2 = 25.0d+00 ./ 81.0d+00;
w3 = 40.0d+00 ./ 81.0d+00;
norder = 9;
xtab([1:9]) =[   z,  -s, +s, -s, +s,   z, -s, +s,  z ];
ytab([1:9]) =[   z,  -s, -s, +s, +s,  -s,  z,  z, +s ];
weight([1:9]) =[  w1,  w2, w2, w2, w2,  w3, w3, w3, w3 ];
elseif( rule == 4 ) ;
r = sqrt( 6.0d+00 ./ 7.0d+00 );
c = 3.0d+00 .* sqrt( 583.0d+00 );
s = sqrt(( 114.0d+00 - c ) ./ 287.0d+00 );
t = sqrt(( 114.0d+00 + c ) ./ 287.0d+00 );
w1 = 4.0d+00 .* 49.0d+00 ./ 810.0d+00;
w2 = 4.0d+00 .*( 178981.0d+00 + 923.0d+00 .* c ) ./ 1888920.0d+00;
w3 = 4.0d+00 .*( 178981.0d+00 - 923.0d+00 .* c ) ./ 1888920.0d+00;
z = 0.0d+00;
norder = 12;
xtab([1:12]) =[   r,  z, -r,  z,   s, -s, -s,  s,  t, -t, -t,  t ];
ytab([1:12]) =[   z,  r,  z,  -r,  s,  s, -s, -s,  t,  t, -t, -t ];
weight([1:12]) =[  w1, w1,  w1, w1, w2, w2, w2, w2, w3, w3, w3, w3 ];
elseif( rule == 5 ) ;
r = sqrt( 12.0d+00 ./ 35.0d+00 );
c = 3.0d+00 .* sqrt( 186.0d+00 );
s = sqrt(( 93.0d+00 + c ) ./ 155.0d+00 );
t = sqrt(( 93.0d+00 - c ) ./ 155.0d+00 );
w1 =  8.0d+00 ./ 162.0d+00;
w2 = 98.0d+00 ./ 162.0d+00;
w3 = 31.0d+00 ./ 162.0d+00;
z = 0.0d+00;
norder = 13;
xtab([1:13]) =[  z,  r, -r,  z,  z,  s,  s, -s, -s,  t,  t, -t, -t ];
ytab([1:13]) =[  z,  z,  z,  r, -r,  t, -t,  t, -t,  s, -s,  s, -s ];
weight([1:13]) =[ w1, w2, w2, w2, w2, w3, w3, w3, w3, w3, w3, w3, w3 ];
elseif( rule == 6 ) ;
norder = 64;
[ norder2, xtab2, weight2 ]=legendre_set( norder2, xtab2, weight2 );
k = 0;
for i = 1: norder2;
for j = 1: norder2;
k = fix(k + 1);
xtab(k) = xtab2(i);
ytab(k) = xtab2(j);
weight(k) = weight2(i) .* weight2(j);
end; j = fix(norder2+1);
end; i = fix(norder2+1);
else;
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'SQUARE_UNIT_SET - Fatal error!');
writef(1,['%s %0.15g \n'], '  Illegal value of NORDER = ', norder);
error(['stop encountered in original fortran code  ',char(10),';']);
end;
weight_shape=zeros(weight_shape);weight_shape(:)=weight(1:numel(weight_shape));weight=weight_shape;
xtab_shape=zeros(xtab_shape);xtab_shape(:)=xtab(1:numel(xtab_shape));xtab=xtab_shape;
ytab_shape=zeros(ytab_shape);ytab_shape(:)=ytab(1:numel(ytab_shape));ytab=ytab_shape;
return;
end
function [func, norder, xtab, ytab, weight, result]=square_unit_sum( func, norder, xtab, ytab, weight, result );
%
%*******************************************************************************
%
%! SQUARE_UNIT_SUM carries out a quadrature rule over the unit square.
%
%
%  Integration interval:
%
%    -1 <= X <= 1,
%    -1 <= Y <= 1.
%
%  Modified:
%
%    25 August 1998
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, EXTERNAL FUNC, the name of the function to be
%    integrated.  The user must declare the name an EXTERNAL
%    parameter in the calling program, pass the name of the
%    function in FUNC, and write a function of the form
%
%      function FUNC(X,Y)
%
%    which evaluates the function at the point (X,Y).
%
%    Input, integer NORDER, the order of the rule.
%
%    Input, doubleprecision XTAB(NORDER), YTAB(NORDER), the abscissas of the rule.
%
%    Input, doubleprecision WEIGHT(NORDER), the weights of the rule.
%
%    Output, doubleprecision RESULT, the approximate integral of the function.
%
%
persistent i quad volume ; 

if isempty(i), i=0; end;
if isempty(quad), quad=0; end;
if isempty(volume), volume=0; end;
%
%
quad = 0.0d+00;
for i = 1: norder;
quad = quad + weight(i) .* func( xtab(i), ytab(i) ) ./ 4.0d+00;
end; i = fix(norder+1);
volume = 1.0d+00;
result = quad .* volume;
return;
end
function [n, a, more, ncard, iadd]=subset_next( n, a, more, ncard, iadd );
%
%*******************************************************************************
%
%! SUBSET_NEXT generates all subsets of a set of order N, one at a time.
%
%
%  Discussion:
%
%    It generates the subsets one at a time, by adding or subtracting
%    exactly one element on each step.
%
%    The user should set MORE = false and the value of N before
%    the first call.  On return, the user may examine A which contains
%    the definition of the new subset, and must check .MORE., because
%    as soon as it is false on return, all the subsets have been
%    generated and the user probably should cease calling.
%
%    The first set returned is the empty set.
%
%  Reference:
%
%    A Nijenhuis and H Wilf,
%    Combinatorial Algorithms,
%    Academic Press, 1978, second edition,
%    ISBN 0-12-519260-6.
%
%  Modified:
%
%    15 April 1999
%
%  Parameters:
%
%    Input, integer N, the order of the total set from which
%    subsets will be drawn.
%
%    Output, integer A(N).  On each return, the Gray code for the newly
%    generated subset.  A(I) = 0 if element I is in the subset, 1 otherwise.
%
%    Input/output, logical MORE.  Set this variable false before
%    the first call.  Normally, MORE will be returned true but once
%    all the subsets have been generated, MORE will be
%    reset false on return and you should stop calling the program.
%
%    Output, integer NCARD, the cardinality of the set returned,
%    which may be any value between 0 (the empty set) and N (the
%    whole set).
%
%    Output, integer IADD, the element which was added or removed to the
%    previous subset to generate the current one.  Exception:
%    the empty set is returned on the first call, and IADD is set to 0.
%
%
persistent i ; 

if isempty(i), i=0; end;
%
%  First set returned is the empty set.
%
if( ~ more )
a([1:n]) = 0;
iadd = 0;
ncard = 0;
more = true;
else;
iadd = 1;
if( rem( ncard, 2 ) ~= 0 )
while (1);
iadd = fix(iadd + 1);
if( a(iadd-1) ~= 0 )
break;
end;
end;
end;
a(iadd) = fix(1 - a(iadd));
ncard = fix(ncard + 2 .* a(iadd) - 1);
%
%  Last set returned is the singleton A(N).
%
if( ncard == a(n) )
more = false;
end;
end;
return;
end
function [func, x, y, z, result]=tetra_07( func, x, y, z, result );
%
%*******************************************************************************
%
%! TETRA_07 approximates an integral inside a tetrahedron in 3D.
%
%
%  Discussion:
%
%    A 64 point 7-th degree conical product Gauss formula is used,
%    Stroud number T3:7-1.
%
%  Integration region:
%
%    Points inside a tetrahedron whose four corners are given.
%
%  Reference:
%
%    Arthur H Stroud,
%    Approximate Calculation of Multiple Integrals,
%    Prentice Hall, 1971.
%
%    Stroud and Secrest,
%    Gaussian Quadrature Formulas,
%    Prentice Hall, 1966, pages 42-43.
%
%  Modified:
%
%    08 December 2000
%
%  Parameters:
%
%    Input, external FUNC, the name of the user supplied
%    function of three variables which is to be integrated,
%    of the form:
%
%      function func ( x, y, z )
%      doubleprecision func
%      doubleprecision x
%      doubleprecision y
%      doubleprecision z
%
%    Input, doubleprecision X(4), Y(4), Z(4), the X, Y and Z coordinates of the vertices.
%
%    Output, doubleprecision RESULT, the approximate integral of the function.
%
persistent a b c d i j k norder quad t u v volume w weight1 weight2 weight3 xtab1 xtab2 xtab3 xval yval zval ; 

if isempty(norder), norder = 4; end;
%
if isempty(a), a=0; end;
if isempty(b), b=0; end;
if isempty(c), c=0; end;
if isempty(d), d=0; end;
if isempty(i), i=0; end;
if isempty(j), j=0; end;
if isempty(k), k=0; end;
if isempty(quad), quad=0; end;
if isempty(t), t=0; end;
if isempty(u), u=0; end;
if isempty(v), v=0; end;
if isempty(volume), volume=0; end;
if isempty(w), w=0; end;
if isempty(weight1), weight1=zeros(1,norder); end;
if isempty(weight2), weight2([1:norder]) =[0.1355069134d+00, 0.2034645680d+00, 0.1298475476d+00, 0.0311809709d+00 ]; end;
if isempty(weight3), weight3([1:norder]) =[0.1108884156d+00, 0.1434587898d+00, 0.0686338872d+00, 0.0103522407d+00 ]; end;
if isempty(xtab1), xtab1=zeros(1,norder); end;
if isempty(xtab2), xtab2([1:norder]) =[0.0571041961d+00, 0.2768430136d+00, 0.5835904324d+00, 0.8602401357d+00 ]; end;
if isempty(xtab3), xtab3([1:norder]) =[0.0485005495d+00, 0.2386007376d+00, 0.5170472951d+00, 0.7958514179d+00 ]; end;
if isempty(xval), xval=0; end;
if isempty(yval), yval=0; end;
if isempty(zval), zval=0; end;
%
%
%  Get the Gauss-Legendre weights and abscissas for [-1,1].
%
[ norder, xtab1, weight1 ]=legendre_set( norder, xtab1, weight1 );
%
%  Adjust the rule for the interval [0,1].
%
a = -1.0d+00;
b = +1.0d+00;
c =  0.0d+00;
d =  1.0d+00;
[ a, b, c, d, norder, xtab1, weight1 ]=rule_adjust( a, b, c, d, norder, xtab1, weight1 );
%
%  Carry out the quadrature.
%
quad = 0.0d+00;
for i = 1: norder;
for j = 1: norder;
for k = 1: norder;
%
%  Compute the barycentric coordinates of the point in the unit triangle.
%
t =                                                 xtab3(k);
u =                        xtab2(j)   .*( 1.0d+00 - xtab3(k) );
v = xtab1(i) .*( 1.0d+00 - xtab2(j) ) .*( 1.0d+00 - xtab3(k) );
w = 1.0d+00 - t - u - v;
%
%  Compute the corresponding point in the triangle.
%
xval = t .* x(1) + u .* x(2) + v .* x(3) + w .* x(4);
yval = t .* y(1) + u .* y(2) + v .* y(3) + w .* y(4);
zval = t .* z(1) + u .* z(2) + v .* z(3) + w .* z(4);
quad = quad + 6.0d+00 .* weight1(i) .* weight2(j) .* weight3(k).* func( xval, yval, zval );
end; k = fix(norder+1);
end; j = fix(norder+1);
end; i = fix(norder+1);
[volume , x, y, z ]=tetra_volume( x, y, z );
result = quad .* volume;
return;
end
function [func, x, y, z, norder, xtab, ytab, ztab, weight, result]=tetra_sum( func, x, y, z, norder, xtab, ytab, ztab, weight, result );
%
%*******************************************************************************
%
%! TETRA_SUM carries out a quadrature rule in a tetrahedron in 3D.
%
%
%  Integration region:
%
%    A tetrahedron whose vertices are specified.
%
%  Modified:
%
%    19 November 2000
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, external FUNC, name of the function, of the form:
%
%      function func ( x, y, z )
%      doubleprecision func
%      doubleprecision x
%      doubleprecision y
%      doubleprecision z
%
%    Input, doubleprecision X(4), Y(4), Z(4), the coordinates of the vertices.
%
%    Input, integer NORDER, the order of the rule.
%
%    Input, doubleprecision XTAB(NORDER), YTAB(NORDER), ZTAB(NORDER), the abscissas.
%
%    Input, doubleprecision WEIGHT(NORDER), the weights.
%
%    Output, doubleprecision RESULT, the approximate integral of the function.
%
%
persistent i quad volume xval yval zval ; 

if isempty(i), i=0; end;
if isempty(quad), quad=0; end;
if isempty(volume), volume=0; end;
if isempty(xval), xval=0; end;
if isempty(yval), yval=0; end;
if isempty(zval), zval=0; end;
%
%
quad = 0.0d+00;
for i = 1: norder;
xval = xtab(i) .* x(1) + ytab(i) .* x(2) + ztab(i) .* x(3)+( 1.0d+00 - xtab(i) - ytab(i) - ztab(i) ) .* x(4);
yval = xtab(i) .* y(1) + ytab(i) .* y(2) + ztab(i) .* y(3)+( 1.0d+00 - xtab(i) - ytab(i) - ztab(i) ) .* y(4);
xval = xtab(i) .* z(1) + ytab(i) .* z(2) + ztab(i) .* z(3)+( 1.0d+00 - xtab(i) - ytab(i) - ztab(i) ) .* z(4);
quad = quad + weight(i) .* func( xval, yval, zval );
end; i = fix(norder+1);
[volume , x, y, z ]=tetra_volume( x, y, z );
result = quad .* volume;
return;
end
function [func, norder, x, y, z, result]=tetra_tproduct( func, norder, x, y, z, result );
%
%*******************************************************************************
%
%! TETRA_TPRODUCT approximates an integral in a tetrahedron in 3D.
%
%
%  Discussion:
%
%    An NORDER**3 point (2*NORDER-1)-th degree triangular product
%    Gauss-Legendre rule is used.
%
%    With NORDER = 8, this routine is equivalent to the routine TETR15
%    in the reference, page 367.
%
%    Thanks to Joerg Behrens, jbehren@gwdg.de, for numerous suggestions
%    and corrections.
%
%  Integration region:
%
%    Points inside a tetrahedron whose four corners are given.
%
%  Reference:
%
%    Arthur H Stroud,
%    Approximate Calculation of Multiple Integrals,
%    Prentice Hall, 1971.
%
%  Modified:
%
%    18 December 2000
%
%  Parameters:
%
%    Input, external FUNC, the name of the user supplied
%    function of three variables which is to be integrated,
%    of the form:
%
%      function func ( x, y, z )
%      doubleprecision func
%      doubleprecision x
%      doubleprecision y
%      doubleprecision z
%
%    Input, integer NORDER, the order of the basic quadrature rules.
%    NORDER should be between 1 and 9.
%
%    Input, doubleprecision X(4), Y(4), Z(4), the coordinates of the vertices
%    of the tetrahedron.
%
%    Output, doubleprecision RESULT, the approximate integral of the function.
%
%
persistent a b c d i j k quad volume weight0 weight1 weight2 xtab0 xtab1 xtab2 xval yval zval ; 

if isempty(a), a=0; end;
if isempty(b), b=0; end;
if isempty(c), c=0; end;
if isempty(d), d=0; end;
if isempty(i), i=0; end;
if isempty(j), j=0; end;
if isempty(k), k=0; end;
if isempty(quad), quad=0; end;
if isempty(volume), volume=0; end;
if isempty(weight0), weight0=zeros(1, norder ); end;
if isempty(weight1), weight1=zeros(1, norder ); end;
if isempty(weight2), weight2=zeros(1, norder ); end;
if isempty(xtab0), xtab0=zeros(1, norder ); end;
if isempty(xtab1), xtab1=zeros(1, norder ); end;
if isempty(xtab2), xtab2=zeros(1, norder ); end;
if isempty(xval), xval=0; end;
if isempty(yval), yval=0; end;
if isempty(zval), zval=0; end;
%
%
if( norder < 1 || norder > 9 )
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'TETRA_TPRODUCT - Fatal error!');
writef(1,['%s \n'], '  The quadrature rule orders must be between 1 and 9.');
writef(1,['%s %0.15g \n'], '  The input value was NORDER = ', norder);
error(['stop encountered in original fortran code  ',char(10),';']);
end;
%
%  Get the Gauss-Legendre NORDER point rules on [-1,1] for integrating
%    F(X),
%    X * F(X),
%    X * X * F(X).
%
[ norder, xtab0, weight0 ]=legendre_set( norder, xtab0, weight0 );
[ norder, xtab1, weight1 ]=legendre_set_x1( norder, xtab1, weight1 );
[ norder, xtab2, weight2 ]=legendre_set_x2( norder, xtab2, weight2 );
%
%  Adjust the rules from [-1,1] to [0,1].
%
a = -1.0d+00;
b = +1.0d+00;
c =  0.0d+00;
d =  1.0d+00;
[ a, b, c, d, norder, xtab0, weight0 ]=rule_adjust( a, b, c, d, norder, xtab0, weight0 );
[ a, b, c, d, norder, xtab1, weight1 ]=rule_adjust( a, b, c, d, norder, xtab1, weight1 );
[ a, b, c, d, norder, xtab2, weight2 ]=rule_adjust( a, b, c, d, norder, xtab2, weight2 );
%
%  For rules with a weight function that is not 1, the weight vectors
%  require further adjustment.
%
weight1([1:norder]) = weight1([1:norder]) ./ 2.0d+00;
weight2([1:norder]) = weight2([1:norder]) ./ 4.0d+00;
%
%  Carry out the quadrature.
%
quad = 0.0d+00;
for k = 1: norder;
for j = 1: norder;
for i = 1: norder;
xval = x(1) +((( x(4) - x(3) )   .* xtab0(i)+( x(3) - x(2) ) ) .* xtab1(j)+( x(2) - x(1) ) ) .* xtab2(k);
yval = y(1) +((( y(4) - y(3) )   .* xtab0(i)+( y(3) - y(2) ) ) .* xtab1(j)+( y(2) - y(1) ) ) .* xtab2(k);
zval = z(1) +((( z(4) - z(3) )   .* xtab0(i)+( z(3) - z(2) ) ) .* xtab1(j)+( z(2) - z(1) ) ) .* xtab2(k);
quad = quad + 6.0d+00 .* weight0(i) .* weight1(j) .* weight2(k).* func( xval, yval, zval );
end; i = fix(norder+1);
end; j = fix(norder+1);
end; k = fix(norder+1);
%
%  Compute the volume of the tetrahedron.
%
[volume , x, y, z ]=tetra_volume( x, y, z );
result = quad .* volume;
return;
end
function [rule, norder, xtab, ytab, ztab, weight]=tetra_unit_set( rule, norder, xtab, ytab, ztab, weight );
%
%*******************************************************************************
%
%! TETRA_UNIT_SET sets weights and abscissas for quadrature on a unit tetrahedron.
%
%
%  Integration region:
%
%    0 <= X
%    0 <= Y
%    0 <= Z
%    X + Y + Z <= 1.
%
%  References:
%
%    H Engels,
%    Numerical Quadrature and Cubature,
%    Academic Press, 1980.
%
%    O C Zienkiewicz,
%    The Finite Element Method,
%    McGraw Hill, Third Edition, 1977, page 202.
%
%  Modified:
%
%    11 December 2000
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, integer RULE, the index of the rule.
%
%     1, NORDER =  1, precision 0, Newton Cotes formula, Zienkiewicz #1.
%     2, NORDER =  4, precision 1, Newton Cotes formula.
%     3, NORDER =  4, Zienkiewicz #2.
%     4, NORDER =  10, precision 2, Newton Cotes formula.
%     5, NORDER =  5, Zienkiewicz #3.
%     6, NORDER =  8, precision 3, Newton Cotes formula.
%     7, NORDER =  35, precision 4, Newton Cotes formula.
%
%    Output, integer NORDER, the order of the rule.
%
%    Output, doubleprecision XTAB(NORDER), YTAB(NORDER), ZTAB(NORDER),
%    the abscissas.
%
%    Output, doubleprecision WEIGHT(NORDER), the weights.
%
persistent a b c d e f g h z ; 

if isempty(a), a=0; end;
if isempty(b), b=0; end;
if isempty(c), c=0; end;
if isempty(d), d=0; end;
if isempty(e), e=0; end;
if isempty(f), f=0; end;
if isempty(g), g=0; end;
if isempty(h), h=0; end;
weight_shape=size(weight);weight=reshape(weight,1,[]);
xtab_shape=size(xtab);xtab=reshape(xtab,1,[]);
ytab_shape=size(ytab);ytab=reshape(ytab,1,[]);
if isempty(z), z=0; end;
ztab_shape=size(ztab);ztab=reshape(ztab,1,[]);
%
%  Newton Cotes.
%
if( rule == 1 )
norder = 1;
xtab(1) = 1.0d+00 ./ 3.0d+00;
ytab(1) = 1.0d+00 ./ 3.0d+00;
ztab(1) = 1.0d+00 ./ 3.0d+00;
weight(1) = 1.0d+00;
%
%  Newton Cotes.
%
elseif( rule == 2 ) ;
a = 1.0d+00;
b = 1.0d+00 ./ 4.0d+00;
z = 0.0d+00;
norder = 4;
xtab([1:4]) =[ z, a, z, z ];
ytab([1:4]) =[ z, z, a, z ];
ztab([1:4]) =[ z, z, z, a ];
weight([1:4]) =[ b, b, b, b ];
%
%  Zienkiewicz #2.
%
elseif( rule == 3 ) ;
a =  0.58541020d+00;
b =  0.13819660d+00;
c =  0.25d+00;
norder = 4;
xtab([1:4]) =[ a, b, b, b ];
ytab([1:4]) =[ b, a, b, b ];
ztab([1:4]) =[ b, b, a, b ];
weight([1:4]) =[ c, c, c, c ];
%
%  Newton Cotes.
%
elseif( rule == 4 ) ;
a =  1.0d+00;
b =  0.5d+00;
c = -1.0d+00 ./ 20.0d+00;
d =  4.0d+00 ./ 20.0d+00;
z =  0.0d+00;
norder = 10;
xtab([1:10]) =[ z, a, z, z, b, z, z, b, b, z ];
ytab([1:10]) =[ z, z, a, z, z, b, z, b, z, b ];
ztab([1:10]) =[ z, z, z, a, z, z, b, z, b, b ];
weight([1:10]) =[ c, c, c, c, d, d, d, d, d, d ];
%
%  Zienkiewicz #3.
%
elseif( rule == 5 ) ;
a =   1.0d+00 ./ 6.0d+00;
b =   0.25d+00;
c =   0.5d+00;
d = - 0.8d+00;
e =   0.45d+00;
norder = 5;
xtab([1:5]) =[ b, c, a, a, a ];
ytab([1:5]) =[ b, a, c, a, a ];
ztab([1:5]) =[ b, a, a, c, a ];
weight([1:5]) =[ d, e, e, e, e ];
%
%  Newton Cotes.
%
elseif( rule == 6 ) ;
a = 1.0d+00;
b = 1.0d+00 ./ 40.0d+00;
c = 1.0d+00 ./  3.0d+00;
d = 9.0d+00 ./ 40.0d+00;
z = 0.0d+00;
norder = 8;
xtab([1:8]) =[ z, a, z, z, c, c, z, c ];
ytab([1:8]) =[ z, z, a, z, c, z, c, c ];
ztab([1:8]) =[ z, z, z, a, z, c, c, c ];
weight([1:8]) =[ b, b, b, b, d, d, d, d ];
%
%  Newton Cotes.
%
elseif( rule == 7 ) ;
a =   0.25d+00;
b =   0.50d+00;
c =   0.75d+00;
d =   1.00d+00;
e =  -5.0d+00 ./ 420.0d+00;
f = -12.0d+00 ./ 420.0d+00;
g =  16.0d+00 ./ 420.0d+00;
h = 128.0d+00 ./ 420.0d+00;
z =   0.0d+00;
norder = 35;
xtab([1:35]) =[ z, d, z, z, a, z, z, c, c, c, z, a, z, z, a, z, b, z, z,b, b, z, a, b, a, a, b, z, b, z, a, a, z, a, a ];
ytab([1:35]) =[ z, z, d, z, z, a, z, z, a, z, c, c, c, z, z, a, z, b, z,b, z, b, a, a, b, z, z, a, a, b, b, z, a, a, a ];
ztab([1:35]) =[ z, z, z, d, z, z, a, z, z, a, z, z, a, c, c, c, z, z, b,z, b, b, z, z, z, a, a, a, a, a, a, b, b, b, a ];
weight([1:35]) =[ e, e, e, e, g, g, g, g, g, g, g, g, g, g, g, g, f, f, f,f, f, f, g, g, g, g, g, g, g, g, g, g, g, g, h ];
end;
weight_shape=zeros(weight_shape);weight_shape(:)=weight(1:numel(weight_shape));weight=weight_shape;
xtab_shape=zeros(xtab_shape);xtab_shape(:)=xtab(1:numel(xtab_shape));xtab=xtab_shape;
ytab_shape=zeros(ytab_shape);ytab_shape(:)=ytab(1:numel(ytab_shape));ytab=ytab_shape;
ztab_shape=zeros(ztab_shape);ztab_shape(:)=ztab(1:numel(ztab_shape));ztab=ztab_shape;
return;
end
function [func, norder, xtab, ytab, ztab, weight, result]=tetra_unit_sum( func, norder, xtab, ytab, ztab, weight, result );
%
%*******************************************************************************
%
%! TETRA_UNIT_SUM carries out a quadrature rule in the unit tetrahedron in 3D.
%
%
%  Integration region:
%
%    0 <= X
%    0 <= Y
%    0 <= Z
%    X + Y + Z <= 1.
%
%  Modified:
%
%    26 October 2000
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, external FUNC, the name of the user supplied
%    function of three variables which is to be integrated,
%    of the form:
%
%      function func ( x, y, z )
%      doubleprecision func
%      doubleprecision x
%      doubleprecision y
%      doubleprecision z
%
%    Input, integer NORDER, the order of the rule.
%
%    Input, doubleprecision XTAB(NORDER), YTAB(NORDER), ZTAB(NORDER), the abscissas.
%
%    Input, doubleprecision WEIGHT(NORDER), the weights.
%
%    Output, doubleprecision RESULT, the approximate integral of the function.
%
%
persistent i quad volume ; 

if isempty(i), i=0; end;
if isempty(quad), quad=0; end;
if isempty(volume), volume=0; end;
%
%
quad = 0.0d+00;
for i = 1: norder;
quad = quad + weight(i) .* func( xtab(i), ytab(i), ztab(i) );
end; i = fix(norder+1);
volume = tetra_unit_volume( );
result = quad .* volume;
return;
end
function [tetra_unit_volumeresult]=tetra_unit_volume( );
%
%*******************************************************************************
%
%! TETRA_UNIT_VOLUME returns the volume of the unit tetrahedron in 3D.
%
%
%  Modified:
%
%    27 October 2000
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Output, doubleprecision TETRA_UNIT_VOLUME, the volume of the unit tetrahedron.
%
tetra_unit_volumeresult=[];
persistent tetra_unit_volume ; 

if isempty(tetra_unit_volumeresult), tetra_unit_volumeresult=0; end;
%
tetra_unit_volumeresult = 1.0d+00 ./ 6.0d+00;
return;
end
function [tetra_volumeresult, x, y, z ]=tetra_volume( x, y, z );
%
%*******************************************************************************
%
%! TETRA_VOLUME computes the volume of a tetrahedron in 3D.
%
%
%  Integration region:
%
%    Points inside a tetrahedron whose four vertices are given.
%
%  Modified:
%
%    19 November 2000
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, doubleprecision X(4), Y(4), Z(4), the vertices.
%
%    Output, doubleprecision TETRA_VOLUME, the volume of the tetrahedron.
%
tetra_volumeresult=[];
persistent tetra_volume volume ; 

if isempty(tetra_volumeresult), tetra_volumeresult=0; end;
if isempty(volume), volume=0; end;
%
[volume , x, y, z ]=parallelipiped_volume_3d( x, y, z );
tetra_volumeresult = volume .* tetra_unit_volume( );
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(3)), assignin('caller','FUntemp',z); evalin('caller',[inputname(3),'=FUntemp;']); end
if csnil&&~isempty(inputname(2)), assignin('caller','FUntemp',y); evalin('caller',[inputname(2),'=FUntemp;']); end
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',x); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [func, r1, r2, n, result]=torus_1( func, r1, r2, n, result );
%
%*******************************************************************************
%
%! TORUS_1 approximates an integral on the surface of a torus in 3D.
%
%
%  Discussion:
%
%    An (N+1)*(N+2) point N-th degree formula is used.
%
%  Integration region:
%
%    Points (X,Y,Z) such that:
%
%      ( SQRT ( X**2 + Y**2 ) - R1 )**2 + Z**2 = R2**2.
%
%  Reference:
%
%    Arthur H Stroud,
%    Approximate Calculation of Multiple Integrals,
%    Prentice Hall, 1971.
%
%  Modified:
%
%    07 November 2000
%
%  Parameters:
%
%    Input, external FUNC, the name of the user supplied
%    function of three variables which is to be integrated,
%    of the form:
%
%      function func ( x, y, z )
%      doubleprecision x
%      doubleprecision y
%      doubleprecision z
%
%    Input, doubleprecision R1, R2, the two radii that define the torus.
%
%    Input, integer N, defines the degree of the formula
%    used to approximate the integral.
%
%    Output, doubleprecision RESULT, the approximate integral of the function.
%
persistent angle ct1 i j quad st1 u volume w x y z ; 

if isempty(angle), angle=0; end;
if isempty(ct1), ct1=0; end;
if isempty(i), i=0; end;
if isempty(j), j=0; end;
if isempty(quad), quad=0; end;
if isempty(st1), st1=0; end;
if isempty(u), u=0; end;
if isempty(volume), volume=0; end;
if isempty(w), w=0; end;
if isempty(x), x=0; end;
if isempty(y), y=0; end;
if isempty(z), z=0; end;
%
%
w = 1.0d+00 ./( r1 .* (( n + 1 ) .*( n + 2 ) ) );
quad = 0.0d+00;
for i = 1: n+1;
angle = fix(((2.0d+00 .* pi( )) .* ( i )) ./ ( n + 1 ));
ct1 = cos( angle );
st1 = sin( angle );
for j = 1: n+2;
angle = fix(((2.0d+00 .* pi( )) .* ( j )) ./ ( n + 2 ));
u = r1 + r2 .* cos( angle );
x = u .* ct1;
y = u .* st1;
z = r2 .* sin( angle );
quad = quad + w .* u .* func( x, y, z );
end; j = fix(n+2+1);
end; i = fix(n+1+1);
[volume , r1, r2 ]=torus_area_3d( r1, r2 );
result = quad .* volume;
return;
end
function [func, r1, r2, result]=torus_14s( func, r1, r2, result );
%
%*******************************************************************************
%
%! TORUS_14S approximates an integral inside a torus in 3D.
%
%
%  Discussion:
%
%    A 960 point 14-th degree formula is used.
%
%  Integration region:
%
%    Points (X,Y,Z) such that:
%
%      ( SQRT ( X**2 + Y**2 ) - R1 )**2 + Z**2 <= R2**2.
%
%  Reference:
%
%    Arthur H Stroud,
%    Approximate Calculation of Multiple Integrals,
%    Prentice Hall, 1971.
%
%  Modified:
%
%    07 November 2000
%
%  Parameters:
%
%    Input, external FUNC, the name of the user supplied
%    function of three variables which is to be integrated,
%    of the form:
%
%      function func ( x, y, z )
%      doubleprecision func
%      doubleprecision x
%      doubleprecision y
%      doubleprecision z
%
%    Input, doubleprecision R1, R2, the two radii that define the torus.
%
%    Output, doubleprecision RESULT, the approximate integral of the function.
%
persistent angle ct cth i j n norder quad r st sth u volume weight x y z ; 

if isempty(norder), norder = 4; end;
%
if isempty(angle), angle=0; end;
if isempty(ct), ct=0; end;
if isempty(cth), cth=0; end;
if isempty(i), i=0; end;
if isempty(j), j=0; end;
if isempty(n), n=0; end;
if isempty(quad), quad=0; end;
if isempty(r), r([1:norder]) =[0.263499230d+00, 0.574464514d+00, 0.818529487d+00, 0.964659606d+00 ]; end;
if isempty(st), st=0; end;
if isempty(sth), sth=0; end;
if isempty(u), u=0; end;
if isempty(volume), volume=0; end;
if isempty(weight), weight([1:norder]) =[0.086963711d+00, 0.163036289d+00, 0.163036289d+00, 0.086963711d+00 ]; end;
if isempty(x), x=0; end;
if isempty(y), y=0; end;
if isempty(z), z=0; end;
%
%
quad = 0.0d+00;
for n = 1: 15;
angle = 2.0d+00 .* pi( ) .* ( n ) ./ 15.0d+00;
cth = cos( angle );
sth = sin( angle );
for i = 1: 16;
angle = pi( ) .* ( i ) ./ 8.0d+00;
ct = cos( angle );
st = sin( angle );
for j = 1: norder;
u = r1 + r(j) .* ct .* r2;
x = u .* cth;
y = u .* sth;
z = r(j) .* st .* r2;
quad = quad + u .* weight(j) .* func( x, y, z ) ./( 120.0d+00 .* r1 );
end; j = fix(norder+1);
end; i = fix(16+1);
end; n = fix(15+1);
[volume , r1, r2 ]=torus_volume_3d( r1, r2 );
result = quad .* volume;
return;
end
function [func, r1, r2, result]=torus_5s2( func, r1, r2, result );
%
%*******************************************************************************
%
%! TORUS_5S2 approximates an integral inside a torus in 3D.
%
%
%  Discussion:
%
%    A 24 point, 5-th degree formula is used, Stroud number TOR3-S2:5-1.
%
%  Integration region:
%
%    Points (X,Y,Z) such that:
%
%      ( SQRT ( X**2 + Y**2 ) - R1 )**2 + Z**2 <= R2**2.
%
%  Reference:
%
%    Arthur H Stroud,
%    Approximate Calculation of Multiple Integrals,
%    Prentice Hall, 1971.
%
%  Modified:
%
%    21 November 2000
%
%  Parameters:
%
%    Input, external FUNC, the name of the user supplied
%    function of three variables which is to be integrated,
%    of the form:
%
%      function func ( x, y, z )
%      doubleprecision func
%      doubleprecision x
%      doubleprecision y
%      doubleprecision z
%
%    Input, doubleprecision R1, R2, the two radii that define the torus.
%
%    Output, doubleprecision RESULT, the approximate integral of the function.
%
persistent angle cs i quad sn u1 u2 u3 volume w x y z ; 

if isempty(angle), angle=0; end;
if isempty(cs), cs=0; end;
if isempty(i), i=0; end;
if isempty(quad), quad=0; end;
if isempty(sn), sn=0; end;
if isempty(u1), u1=0; end;
if isempty(u2), u2=0; end;
if isempty(u3), u3=0; end;
if isempty(volume), volume=0; end;
if isempty(w), w=0; end;
if isempty(x), x=0; end;
if isempty(y), y=0; end;
if isempty(z), z=0; end;
%
%
w = 1.0d+00 ./ 24.0d+00;
quad = 0.0d+00;
u1 = sqrt( r1.^2 + 0.5d+00 .* r2.^2 );
u2 = sqrt( r1.^2 + sqrt( 2.0d+00 ) .* r1 .* r2 + r2.^2 );
u3 = sqrt( r1.^2 - sqrt( 2.0d+00 ) .* r1 .* r2 + r2.^2 );
for i = 1: 6;
angle = pi( ) .* ( i ) ./ 3.0d+00;
cs = cos( angle );
sn = sin( angle );
x = u1 .* cs;
y = u1 .* sn;
z = r2 ./ sqrt( 2.0d+00 );
quad = quad + w .* func( x, y, z );
x = u1 .* cs;
y = u1 .* sn;
z = - r2 ./ sqrt( 2.0d+00 );
quad = quad + w .* func( x, y, z );
x = u2 .* cs;
y = u2 .* sn;
z = 0.0d+00;
quad = quad + w .* func( x, y, z );
x = u3 .* cs;
y = u3 .* sn;
z = 0.0d+00;
quad = quad + w .* func( x, y, z );
end; i = fix(6+1);
[volume , r1, r2 ]=torus_volume_3d( r1, r2 );
result = quad .* volume;
return;
end
function [func, r1, r2, result]=torus_6s2( func, r1, r2, result );
%
%*******************************************************************************
%
%! TORUS_6S2 approximates an integral inside a torus in 3D.
%
%
%  Discussion:
%
%    An 84 point 6-th degree formula is used, Stroud number TOR3-S2:6-1.
%
%  Integration region:
%
%    Points (X,Y,Z) such that:
%
%      ( SQRT ( X**2 + Y**2 ) - R1 )**2 + Z**2 <= R2**2.
%
%  Reference:
%
%    Arthur H Stroud,
%    Approximate Calculation of Multiple Integrals,
%    Prentice Hall, 1971.
%
%  Modified:
%
%    07 November 2000
%
%  Parameters:
%
%    Input, external FUNC, the name of the user supplied
%    function of three variables which is to be integrated,
%    of the form:
%
%      function func ( x, y, z )
%      doubleprecision func
%      doubleprecision x
%      doubleprecision y
%      doubleprecision z
%
%    Input, doubleprecision R1, R2, the two radii that define the torus.
%
%    Output, doubleprecision RESULT, the approximate integral of the function.
%
persistent cth i j k n norder quad s sth u v volume w weight x y z ; 

if isempty(norder), norder = 2; end;
%
if isempty(cth), cth=0; end;
if isempty(i), i=0; end;
if isempty(j), j=0; end;
if isempty(k), k=0; end;
if isempty(n), n=0; end;
if isempty(quad), quad=0; end;
if isempty(s), s([1:norder]) =[ 0.322914992d+00, 0.644171310d+00 ]; end;
if isempty(sth), sth=0; end;
if isempty(u), u=0; end;
if isempty(v), v=0; end;
if isempty(volume), volume=0; end;
if isempty(w), w=0; end;
if isempty(weight), weight([1:norder]) =[ 0.387077796d+00, 0.165609800d+00 ]; end;
if isempty(x), x=0; end;
if isempty(y), y=0; end;
if isempty(z), z=0; end;
%
%
w = 1.0d+00 ./( 7.0d+00 .* r1 .* pi() );
quad = 0.0d+00;
for n = 1: 7;
u = 0.5d+00 .* sqrt( 3.0d+00 ) .* r2;
cth = cos( 2.0d+00 .* pi( ) .* ( n ) ./ 7.0d+00 );
sth = sin( 2.0d+00 .* pi( ) .* ( n ) ./ 7.0d+00 );
for i = 1: 2;
u = - u;
x =( r1 + u ) .* cth;
y =( r1 + u ) .* sth;
z = 0.0d+00;
quad = quad + 0.232710567d+00 .* w .*( r1 + u ) .* func( x, y, z );
x = r1 .* cth;
y = r1 .* sth;
z = u;
quad = quad + 0.232710567d+00 .* w .* r1 .* func( x, y, z );
end; i = fix(2+1);
for k = 1: norder;
u = s(k) .* r2;
v = u;
for i = 1: 2;
u = - u;
for j = 1: 2;
v = - v;
x =( r1 + u ) .* cth;
y =( r1 + u ) .* sth;
z = v;
quad = quad + weight(k) .* w .*( r1 + u ) .* func( x, y, z );
end; j = fix(2+1);
end; i = fix(2+1);
end; k = fix(norder+1);
end; n = fix(7+1);
[volume , r1, r2 ]=torus_volume_3d( r1, r2 );
result = quad .* volume;
return;
end
function [torus_area_3dresult, r1, r2 ]=torus_area_3d( r1, r2 );
%
%*******************************************************************************
%
%! TORUS_AREA_3D returns the area of a torus in 3D.
%
%
%  Integration region:
%
%    Points (X,Y,Z) such that:
%
%      ( SQRT ( X**2 + Y**2 ) - R1 )**2 + Z**2 = R2**2.
%
%  Modified:
%
%    07 November 2000
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, doubleprecision R1, R2, the two radii that define the torus.
%
%    Output, doubleprecision TORUS_AREA_3D, the area of the torus.
%
torus_area_3dresult=[];
persistent torus_area_3d ; 

if isempty(torus_area_3dresult), torus_area_3dresult=0; end;
%
torus_area_3dresult = 4.0d+00 .* pi().^2 .* r1 .* r2;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(2)), assignin('caller','FUntemp',r2); evalin('caller',[inputname(2),'=FUntemp;']); end
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',r1); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [func, r1, r2, result]=torus_square_14c( func, r1, r2, result );
%
%*******************************************************************************
%
%! TORUS_SQUARE_14C approximates an integral in a 'square' torus in 3D.
%
%
%  Discussion:
%
%    A 14-th degree 960 point formula is used.
%
%  Integration region:
%
%    Points (X,Y,Z) such that:
%
%      R1 - R2 <= SQRT ( X**2 + Y**2 ) <= R1 + R2,
%       -R2 <= Z <= R2.
%
%  Reference:
%
%    Arthur H Stroud,
%    Approximate Calculation of Multiple Integrals,
%    Prentice Hall, 1971.
%
%  Modified:
%
%    08 November 2000
%
%  Parameters:
%
%    Input, external FUNC, the name of the user supplied
%    function of three variables which is to be integrated, of the form:
%
%      function func ( x, y, z )
%      doubleprecision func
%      doubleprecision x
%      doubleprecision y
%      doubleprecision z
%
%    Input, doubleprecision R1, R2, the radii that define the torus.
%
%    Output, doubleprecision RESULT, the approximate integral of the function.
%
persistent angle cth i j n norder quad rtab sth u volume w weight x y z ; 

if isempty(norder), norder = 8; end;
%
if isempty(angle), angle=0; end;
if isempty(cth), cth=0; end;
if isempty(i), i=0; end;
if isempty(j), j=0; end;
if isempty(n), n=0; end;
if isempty(quad), quad=0; end;
if isempty(rtab), rtab=zeros(1,norder); end;
if isempty(sth), sth=0; end;
if isempty(u), u=0; end;
if isempty(volume), volume=0; end;
if isempty(w), w=0; end;
if isempty(weight), weight=zeros(1,norder); end;
if isempty(x), x=0; end;
if isempty(y), y=0; end;
if isempty(z), z=0; end;
%
%
[ norder, rtab, weight ]=legendre_set( norder, rtab, weight );
w = 1.0d+00 ./( 60.0d+00 .* r1 );
quad = 0.0d+00;
for n = 1: 15;
angle = 2.0d+00 .* pi( ) .* ( n ) ./ 15.0d+00;
cth = cos( angle );
sth = sin( angle );
for i = 1: norder;
u = r1 + rtab(i) .* r2;
x = u .* cth;
y = u .* sth;
for j = 1: norder;
z = rtab(j) .* r2;
quad = quad + u .* w .* weight(i) .* weight(j) .* func( x, y, z );
end; j = fix(norder+1);
end; i = fix(norder+1);
end; n = fix(15+1);
[volume , r1, r2 ]=torus_square_volume_3d( r1, r2 );
result = quad .* volume;
return;
end
function [func, r1, r2, result]=torus_square_5c2( func, r1, r2, result );
%
%*******************************************************************************
%
%! TORUS_SQUARE_5C2 approximates an integral in a 'square' torus in 3D.
%
%
%  Discussion:
%
%    A 24 point 5-th degree formula is used, Stroud number TOR3-C2:5-1.
%
%  Integration region:
%
%    Points (X,Y,Z) such that:
%
%      R1 - R2 <= SQRT ( X**2 + Y**2 ) <= R1 + R2,
%      -R2 <= Z <= R2.
%
%  Reference:
%
%    Arthur H Stroud,
%    Approximate Calculation of Multiple Integrals,
%    Prentice Hall, 1971.
%
%  Modified:
%
%    08 November 2000
%
%  Parameters:
%
%    Input, external FUNC, the name of the user supplied
%    function of three variables which is to be integrated,
%    of the form:
%
%      function func ( x, y, z )
%      doubleprecision func
%      doubleprecision x
%      doubleprecision y
%      doubleprecision z
%
%    Input, doubleprecision R1, the primary radius of the torus.
%
%    Input, doubleprecision R2, one-half the length of a side of the
%    square cross-section.
%
%    Output, doubleprecision RESULT, the approximate integral of the function.
%
persistent b1 b2 cs i quad sn u1 u2 u3 v volume x y z ; 

if isempty(b1), b1 = 5.0d+00 ./ 108.0d+00; end;
if isempty(b2), b2 = 4.0d+00 ./ 108.0d+00; end;
if isempty(cs), cs=0; end;
if isempty(i), i=0; end;
if isempty(quad), quad=0; end;
if isempty(sn), sn=0; end;
if isempty(u1), u1=0; end;
if isempty(u2), u2=0; end;
if isempty(u3), u3=0; end;
if isempty(v), v=0; end;
if isempty(volume), volume=0; end;
if isempty(x), x=0; end;
if isempty(y), y=0; end;
if isempty(z), z=0; end;
%
%
quad = 0.0d+00;
u1 = sqrt( r1.^2 + r2.^2 );
v = r2 .* sqrt( 0.6d+00 );
u2 = sqrt( r1.^2 - sqrt( 3.0d+00 ) .* r1 .* r2 + r2.^2 );
u3 = sqrt( r1.^2 + sqrt( 3.0d+00 ) .* r1 .* r2 + r2.^2 );
for i = 1: 6;
cs = cos( ( i ) .* pi( ) ./ 3.0d+00 );
sn = sin( ( i ) .* pi( ) ./ 3.0d+00 );
x = u1 .* cs;
y = u1 .* sn;
z = v;
quad = quad + b1 .* func( x, y, z );
z = -v;
quad = quad + b1 .* func( x, y, z );
x = u2 .* cs;
y = u2 .* sn;
z = 0.0d+00;
quad = quad + b2 .* func( x, y, z );
x = u3 .* cs;
y = u3 .* sn;
z = 0.0d+00;
quad = quad + b2 .* func( x, y, z );
end; i = fix(6+1);
[volume , r1, r2 ]=torus_square_volume_3d( r1, r2 );
result = quad .* volume;
return;
end
function [torus_square_area_3dresult, r1, r2 ]=torus_square_area_3d( r1, r2 );
%
%*******************************************************************************
%
%! TORUS_SQUARE_AREA_3D returns the area of a square torus in 3D.
%
%
%  Integration region:
%
%    Points (X,Y,Z) such that:
%
%      R1 - R2 <= SQRT ( X**2 + Y**2 ) <= R1 + R2,
%      -R2 <= Z <= R2.
%
%  Modified:
%
%    07 November 2000
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, doubleprecision R1, R2, the two radii that define the torus.
%
%    Output, doubleprecision TORUS_SQUARE_AREA_3D, the area of the torus.
%
torus_square_area_3dresult=[];
persistent torus_square_area_3d ; 

if isempty(torus_square_area_3dresult), torus_square_area_3dresult=0; end;
%
torus_square_area_3dresult = 16.0d+00 .* pi( ) .* r1 .* r2;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(2)), assignin('caller','FUntemp',r2); evalin('caller',[inputname(2),'=FUntemp;']); end
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',r1); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [torus_square_volume_3dresult, r1, r2 ]=torus_square_volume_3d( r1, r2 );
%
%*******************************************************************************
%
%! TORUS_SQUARE_VOLUME_3D returns the volume of a square torus in 3D.
%
%
%  Integration region:
%
%    Points (X,Y,Z) such that:
%
%      R1 - R2 <= SQRT ( X**2 + Y**2 ) <= R1 + R2,
%      -R2 <= Z <= R2.
%
%  Modified:
%
%    07 November 2000
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, doubleprecision R1, R2, the two radii that define the torus.
%
%    Output, doubleprecision TORUS_SQUARE_VOLUME_3D, the volume of the torus.
%
torus_square_volume_3dresult=[];
persistent torus_square_volume_3d ; 

if isempty(torus_square_volume_3dresult), torus_square_volume_3dresult=0; end;
%
torus_square_volume_3dresult = 8.0d+00 .* pi( ) .* r1 .* r2.^2;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(2)), assignin('caller','FUntemp',r2); evalin('caller',[inputname(2),'=FUntemp;']); end
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',r1); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [torus_volume_3dresult, r1, r2 ]=torus_volume_3d( r1, r2 );
%
%*******************************************************************************
%
%! TORUS_VOLUME_3D returns the volume of a torus in 3D.
%
%
%  Integration region:
%
%    Points (X,Y,Z) such that:
%
%      ( SQRT ( X**2 + Y**2 ) - R1 )**2 + Z**2 = R2**2.
%
%  Modified:
%
%    07 November 2000
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, doubleprecision R1, R2, the two radii that define the torus.
%
%    Output, doubleprecision TORUS_VOLUME_3D, the volume of the torus.
%
torus_volume_3dresult=[];
persistent torus_volume_3d ; 

if isempty(torus_volume_3dresult), torus_volume_3dresult=0; end;
%
torus_volume_3dresult = 2.0d+00 .* pi().^2 .* r1 .* r2.^2;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(2)), assignin('caller','FUntemp',r2); evalin('caller',[inputname(2),'=FUntemp;']); end
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',r1); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [xval, yval, norder, xtab, ytab, weight,xtab2, ytab2, weight2]=triangle_rule_adjust( xval, yval, norder, xtab, ytab, weight,xtab2, ytab2, weight2 );
%
%*******************************************************************************
%
%! TRIANGLE_RULE_ADJUST adjusts a unit quadrature rule to an arbitrary triangle.
%
%
%  Discussion:
%
%    This routine accepts as input abscissas and weights appropriate for
%    quadrature in a unit triangle, and returns abscissas and weights
%    appropriate for quadrature in a given triangle.
%
%    Once this routine has been called, an integral over the given triangle
%    can be approximated as:
%
%      QUAD = Sum ( 1 <= I <= NORDER ) WTAB2(I) * FUNC ( XTAB2(I), YTAB2(I) )
%
%  Integration region:
%
%    Points (X,Y) such that:
%
%      (X,Y) = ALPHA * (X1,Y1) + BETA * (X2,Y2) + ( 1 - ALPHA - BETA ) * (X3,Y3)
%      0 <= ALPHA <= 1 - BETA
%      0 <= BETA <= 1 - ALPHA
%
%  Modified:
%
%    21 January 2001
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, doubleprecision XVAL(3), YVAL(3), the coordinates of the nodes.
%
%    Input, integer NORDER, the order of the rule.
%
%    Input, doubleprecision XTAB(NORDER), YTAB(NORDER), the abscissas for
%    a unit triangle.
%
%    Input, doubleprecision WEIGHT(NORDER), the weights for a unit triangle.
%
%    Output, doubleprecision XTAB2(NORDER), YTAB2(NORDER), the adjusted abscissas.
%
%    Output, doubleprecision WEIGHT2(NORDER), the adjusted weights.
%
%
persistent i volume ; 

if isempty(i), i=0; end;
if isempty(volume), volume=0; end;
%
[volume , xval, yval ]=triangle_volume( xval, yval );
for i = 1: norder;
xtab2(i) = xtab(i) .* xval(1) + ytab(i) .* xval(2)+( 1.0d+00 - xtab(i) - ytab(i) ) .* xval(3);
ytab2(i) = xtab(i) .* yval(1) + ytab(i) .* yval(2)+( 1.0d+00 - xtab(i) - ytab(i) ) .* yval(3);
weight2(i) = weight(i) .* 2.0d+00 .* volume;
end; i = fix(norder+1);
return;
end
function [func, xval, yval, nsub, norder, xtab, ytab, weight,result]=triangle_sub( func, xval, yval, nsub, norder, xtab, ytab, weight,result );
%
%*******************************************************************************
%
%! TRIANGLE_SUB carries out quadrature over subdivisions of a triangular region.
%
%
%  Integration region:
%
%    Points (X,Y) such that:
%
%      (X,Y) =
%          ALPHA                * ( XVAL(1), YVAL(1) )
%        + BETA                 * ( XVAL(2), YVAL(2) )
%        + ( 1 - ALPHA - BETA ) * ( XVAL(3), YVAL(3) )
%      0 <= ALPHA <= 1 - BETA
%      0 <= BETA <= 1 - ALPHA
%
%  Modified:
%
%    12 September 1998
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, external FUNC, the name of the user supplied function of
%    two variables which is to be integrated, of the form:
%
%      function func ( x, y )
%      doubleprecision func
%      doubleprecision x
%      doubleprecision y
%
%    Input, XVAL(3), YVAL(3), the coordinates of the triangle vertices.
%
%    Input, integer NSUB, the number of subdivisions of each side of the
%    input triangle to be made.  NSUB = 1 means no subdivisions are made.
%    NSUB = 3 means that each side of the triangle is subdivided into
%    three portions, and that the original triangle is subdivided into
%    NSUB**2 triangles.  NSUB must be at least 1.
%
%    Input, integer NORDER, the order of the rule.
%
%    Input, doubleprecision XTAB(NORDER), YTAB(NORDER), the abscissas.
%
%    Input, doubleprecision WEIGHT(NORDER), the weights of the rule.
%
%    Output, doubleprecision RESULT, the approximate integral of the function.
%
%
persistent area i j k quad temp1 temp2 volume x x1 x2 x3 y y1 y2 y3 ; 

if isempty(area), area=0; end;
if isempty(i), i=0; end;
if isempty(j), j=0; end;
if isempty(k), k=0; end;
if isempty(quad), quad=0; end;
if isempty(temp1), temp1=0; end;
if isempty(temp2), temp2=0; end;
if isempty(volume), volume=0; end;
if isempty(x), x=0; end;
if isempty(x1), x1=0; end;
if isempty(x2), x2=0; end;
if isempty(x3), x3=0; end;
if isempty(y), y=0; end;
if isempty(y1), y1=0; end;
if isempty(y2), y2=0; end;
if isempty(y3), y3=0; end;
%
%
%  Initialize RESULT, the approximate integral.
%
result = 0.0d+00;
%
%  NSUB must be positive.
%
if( nsub <= 0 )
return;
end;
%
%  Initialize QUAD, the quadrature sum.
%
quad = 0.0d+00;
%
%  The sub-triangles can be grouped into NSUB strips.
%
for i = 1: nsub;
temp1 = 0.0d+00;
temp2 = fix(( i ) ./ ( nsub ));
x2 = xval(2) + temp1 .*( xval(3) - xval(2) )+ temp2 .*( xval(1) - xval(2) );
y2 = yval(2) + temp1 .*( yval(3) - yval(2) )+ temp2 .*( yval(1) - yval(2) );
temp1 = 0.0d+00;
temp2 = fix(( i - 1 ) ./ ( nsub ));
x3 = xval(2) + temp1 .*( xval(3) - xval(2) )+ temp2 .*( xval(1) - xval(2) );
y3 = yval(2) + temp1 .*( yval(3) - yval(2) )+ temp2 .*( yval(1) - yval(2) );
%
%  There are 2*I-1 triangles in strip number I.
%  The next triangle in the strip shares two nodes with the previous one.
%  Compute its corners, (X1,Y1), (X2,Y2), (X3,Y3).
%
for j = 1: 2.*i-1;
x1 = x2;
y1 = y2;
x2 = x3;
y2 = y3;
temp1 = fix((fix(( j + 1 ) ./ 2) ) ./ ( nsub ));
temp2 = fix(( i - 1 -( fix(j ./ 2) ) ) ./ ( nsub ));
x3 = xval(2) + temp1 .*( xval(3) - xval(2) )+ temp2 .*( xval(1) - xval(2) );
y3 = yval(2) + temp1 .*( yval(3) - yval(2) )+ temp2 .*( yval(1) - yval(2) );
%
%  Now integrate over the triangle, mapping the points ( XTAB(K), YTAB(K) )
%  into the triangle.
%
for k = 1: norder;
x = x2 + xtab(k) .*( x3 - x2 ) + ytab(k) .*( x1 - x2 );
y = y2 + xtab(k) .*( y3 - y2 ) + ytab(k) .*( y1 - y2 );
quad = quad + weight(k) .* func( x, y );
end; k = fix(norder+1);
end; j = fix(2.*i-1+1);
end; i = fix(nsub+1);
volume = triangle_volume( xval, yval ) ./ ( nsub.^2 );
result = quad .* volume;
return;
end
function [func, xval, yval, norder, xtab, ytab, weight, result]=triangle_sum( func, xval, yval, norder, xtab, ytab, weight, result );
%
%*******************************************************************************
%
%! TRIANGLE_SUM carries out a unit quadrature rule in an arbitrary triangle.
%
%
%  Integration region:
%
%    Points (X,Y) such that:
%
%      (X,Y) = ALPHA * (X1,Y1) + BETA * (X2,Y2) + ( 1 - ALPHA - BETA ) * (X3,Y3)
%      0 <= ALPHA <= 1 - BETA
%      0 <= BETA <= 1 - ALPHA
%
%  Modified:
%
%    25 August 1998
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, external FUNC, the name of the user supplied function of
%    two variables which is to be integrated, of the form:
%
%      function func ( x, y )
%      doubleprecision func
%      doubleprecision x
%      doubleprecision y
%
%    Input, doubleprecision XVAL(3), YVAL(3), the coordinates of the nodes.
%
%    Input, integer NORDER, the order of the rule.
%
%    Input, doubleprecision XTAB(NORDER), YTAB(NORDER), the abscissas.
%
%    Input, doubleprecision WEIGHT(NORDER), the weights of the rule.
%
%    Output, doubleprecision RESULT, the approximate integral of the function.
%
%
persistent i quad volume x y ; 

if isempty(i), i=0; end;
if isempty(quad), quad=0; end;
if isempty(volume), volume=0; end;
if isempty(x), x=0; end;
if isempty(y), y=0; end;
%
%
quad = 0.0d+00;
for i = 1: norder;
x = xtab(i) .* xval(1) + ytab(i) .* xval(2)+( 1.0d+00 - xtab(i) - ytab(i) ) .* xval(3);
y = xtab(i) .* yval(1) + ytab(i) .* yval(2)+( 1.0d+00 - xtab(i) - ytab(i) ) .* yval(3);
quad = quad + weight(i) .* func( x, y );
end; i = fix(norder+1);
[volume , xval, yval ]=triangle_volume( xval, yval );
result = quad .* volume;
return;
end
function [func, norder, xtab, ytab, weight, result]=triangle_sum_adjusted( func, norder, xtab, ytab, weight, result );
%
%*******************************************************************************
%
%! TRIANGLE_SUM_ADJUSTED carries out an adjusted quadrature rule in a triangle.
%
%
%  Discussion:
%
%    It is assumed that a quadrature rule approprate for a unit triangle
%    was generated, and then adjusted to a particular triangle by calling
%    TRIANGLE_RULE_ADJUST.
%
%  Integration region:
%
%    Points (X,Y) such that:
%
%      (X,Y) = ALPHA * (X1,Y1) + BETA * (X2,Y2) + ( 1 - ALPHA - BETA ) * (X3,Y3)
%      0 <= ALPHA <= 1 - BETA
%      0 <= BETA <= 1 - ALPHA
%
%  Modified:
%
%    21 January 2001
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, external FUNC, the name of the user supplied function of
%    two variables which is to be integrated, of the form:
%
%      function func ( x, y )
%      doubleprecision func
%      doubleprecision x
%      doubleprecision y
%
%    Input, integer NORDER, the order of the rule.
%
%    Input, doubleprecision XTAB(NORDER), YTAB(NORDER), the abscissas.
%
%    Input, doubleprecision WEIGHT(NORDER), the weights of the rule.
%
%    Output, doubleprecision RESULT, the approximate integral of the function.
%
%
persistent i ; 

if isempty(i), i=0; end;
%
%
result = 0.0d+00;
for i = 1: norder;
result = result + weight(i) .* func( xtab(i), ytab(i) );
end; i = fix(norder+1);
return;
end
function [rule, norder, xtab, ytab, weight]=triangle_unit_product_set( rule, norder, xtab, ytab, weight );
%
%*******************************************************************************
%
%! TRIANGLE_UNIT_PRODUCT_SET sets a product quadrature rule on a unit triangle.
%
%
%  Integration region:
%
%    0 <= X <= 1 - Y,
%    0 <= Y <= 1 - X.
%
%  References:
%
%    Arthur H Stroud,
%    Approximate Calculation of Multiple Integrals,
%    Prentice Hall, 1971.
%
%  Modified:
%
%    21 January 2001
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, integer RULE, the rule number, or the order of the 1D rule.
%
%    Output, integer NORDER, the order of the rule.  NORDER = RULE**2.
%
%    Output, doubleprecision XTAB(NORDER), YTAB(NORDER), the abscissas.
%
%    Output, doubleprecision WEIGHT(NORDER), the weights of the rule.
%
%
persistent a b c d i j k norder0 norder1 weight0 weight1 xtab0 xtab1 ytab0 ytab1 ; 

if isempty(a), a=0; end;
if isempty(b), b=0; end;
if isempty(c), c=0; end;
if isempty(d), d=0; end;
if isempty(i), i=0; end;
if isempty(j), j=0; end;
if isempty(k), k=0; end;
if isempty(norder0), norder0=0; end;
if isempty(norder1), norder1=0; end;
if isempty(weight0), weight0=zeros(1,rule); end;
if isempty(weight1), weight1=zeros(1,rule); end;
if isempty(xtab0), xtab0=zeros(1,rule); end;
if isempty(xtab1), xtab1=zeros(1,rule); end;
if isempty(ytab0), ytab0=zeros(1,rule); end;
if isempty(ytab1), ytab1=zeros(1,rule); end;
%
norder = fix(rule .* rule);
a = -1.0d+00;
b = +1.0d+00;
c =  0.0d+00;
d = +1.0d+00;
norder0 = fix(rule);
[ norder0, xtab0, weight0 ]=legendre_set( norder0, xtab0, weight0 );
[ a, b, c, d, norder0, xtab0, weight0 ]=rule_adjust( a, b, c, d, norder0, xtab0, weight0 );
norder1 = fix(rule);
[ norder1, xtab1, weight1 ]=legendre_set_x1( norder1, xtab1, weight1 );
[ a, b, c, d, norder1, xtab1, weight1 ]=rule_adjust( a, b, c, d, norder1, xtab1, weight1 );
k = 0;
for j = 1: norder1;
for i = 1: norder0;
k = fix(k + 1);
xtab(k) = 1.0d+00 - xtab1(j);
ytab(k) = xtab0(i) .* xtab1(j);
weight(k) = weight0(i) .* weight1(j);
end; i = fix(norder0+1);
end; j = fix(norder1+1);
return;
end
function [rule, norder, xtab, ytab, weight]=triangle_unit_set( rule, norder, xtab, ytab, weight );
%
%*******************************************************************************
%
%! TRIANGLE_UNIT_SET sets weights and abscissas for quadrature on a unit triangle.
%
%
%  Integration region:
%
%    0 <= X <= 1 - Y,
%    0 <= Y <= 1 - X.
%
%  References:
%
%    H R Schwarz,
%    Methode der Finiten Elemente,
%    Teubner Studienbuecher, 1980.
%
%    Strang and Fix,
%    An Analysis of the Finite Element Method,
%    Prentice Hall, 1973, page 184.
%
%    Arthur H Stroud,
%    Approximate Calculation of Multiple Integrals,
%    Prentice Hall, 1971.
%
%    O C Zienkiewicz,
%    The Finite Element Method,
%    McGraw Hill, Third Edition, 1977, page 201.
%
%  Modified:
%
%    10 December 2000
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, integer RULE, the index of the rule.
%
%     1, NORDER =  1, precision 1, Zienkiewicz #1.
%     2, NORDER =  3, precision 2, Strang and Fix formula #1.
%     3, NORDER =  3, precision 2, Strang and Fix formula #2, Zienkiewicz #2.
%     4, NORDER =  4, precision 3, Strang and Fix formula #3, Zienkiewicz #3.
%     5, NORDER =  6, precision 3, Strang and Fix formula #4.
%     6, NORDER =  6, precision 3, Stroud formula T2:3-1.
%     7, NORDER =  6, precision 4, Strang and Fix formula #5.
%     8, NORDER =  7, precision 4, Strang and Fix formula #6.
%     9, NORDER =  7, precision 5, Strang and Fix formula #7,
%        Stroud formula T2:5-1, Zienkiewicz #4, Schwarz Table 2.2.
%    10, NORDER =  9, precision 6, Strang and Fix formula #8.
%    11, NORDER = 12, precision 6, Strang and Fix formula #9.
%    12, NORDER = 13, precision 7, Strang and Fix formula #10.
%    13, NORDER =  7, precision ?.
%    14, NORDER = 16, precision 7, conical product Gauss, Stroud formula T2:7-1.
%    15, NORDER = 64, precision 15, triangular product Gauss rule.
%    16, NORDER = 19, precision 8, from CUBTRI, ACM TOMS #584.
%    17, NORDER = 19, precision 9, from TRIEX, Lyness and Jespersen.
%    18, NORDER = 28, precision 11, from TRIEX, Lyness and Jespersen.
%    19, NORDER = 37, precision 13, from ACM TOMS #706.
%
%    Output, integer NORDER, the order of the rule.
%
%    Output, doubleprecision XTAB(NORDER), YTAB(NORDER), the abscissas.
%
%    Output, doubleprecision WEIGHT(NORDER), the weights of the rule.
%
persistent a b c d e f g h i j k norder2 p q r s t u v w w1 w2 w3 w4 w5 w6 w7 w8 w9 weight1 weight2 wx x xtab1 xtab2 y z ; 

if isempty(a), a=0; end;
if isempty(b), b=0; end;
if isempty(c), c=0; end;
if isempty(d), d=0; end;
if isempty(e), e=0; end;
if isempty(f), f=0; end;
if isempty(g), g=0; end;
if isempty(h), h=0; end;
if isempty(i), i=0; end;
if isempty(j), j=0; end;
if isempty(k), k=0; end;
if isempty(norder2), norder2=0; end;
if isempty(p), p=0; end;
if isempty(q), q=0; end;
if isempty(r), r=0; end;
if isempty(s), s=0; end;
if isempty(t), t=0; end;
if isempty(u), u=0; end;
if isempty(v), v=0; end;
if isempty(w), w=0; end;
if isempty(w1), w1=0; end;
if isempty(w2), w2=0; end;
if isempty(w3), w3=0; end;
if isempty(w4), w4=0; end;
if isempty(w5), w5=0; end;
if isempty(w6), w6=0; end;
if isempty(w7), w7=0; end;
if isempty(w8), w8=0; end;
if isempty(w9), w9=0; end;
weight_shape=size(weight);weight=reshape(weight,1,[]);
if isempty(weight1), weight1=zeros(1,8); end;
if isempty(weight2), weight2=zeros(1,8); end;
if isempty(wx), wx=0; end;
if isempty(x), x=0; end;
xtab_shape=size(xtab);xtab=reshape(xtab,1,[]);
if isempty(xtab1), xtab1=zeros(1,8); end;
if isempty(xtab2), xtab2=zeros(1,8); end;
if isempty(y), y=0; end;
ytab_shape=size(ytab);ytab=reshape(ytab,1,[]);
if isempty(z), z=0; end;
%
%  1 point, precision 1.
%
if( rule == 1 )
a = 1.0d+00 ./ 3.0d+00;
w = 1.0d+00;
norder = 1;
xtab(1) = a;
ytab(1) = a;
weight(1) = w;
%
%  3 points, precision 2, Strang and Fix formula #1.
%
elseif( rule == 2 ) ;
a = 1.0d+00;
b = 3.0d+00;
c = 4.0d+00;
d = 6.0d+00;
norder = 3;
xtab([1:3]) =[ c, a, a ] ./ d;
ytab([1:3]) =[ a, c, a ] ./ d;
weight([1:3]) =[ a, a, a ] ./ b;
%
%  3 points, precision 2, Strang and Fix formula #2.
%
elseif( rule == 3 ) ;
a = 0.5d+00;
b = 1.0d+00;
c = 1.0d+00 ./ 3.0d+00;
z = 0.0d+00;
norder = 3;
xtab([1:3]) =[ z, a, a ];
ytab([1:3]) =[ a, z, a ];
weight([1:3]) =[ c, c, c ];
%
%  4 points, precision 3, Strang and Fix formula #3.
%
elseif( rule == 4 ) ;
a =   6.0d+00;
b =  10.0d+00;
c =  18.0d+00;
d =  25.0d+00;
e = -27.0d+00;
f =  30.0d+00;
g =  48.0d+00;
norder = 4;
xtab([1:4]) =[ b, c, a, a ] ./ f;
ytab([1:4]) =[ b, a, c, a ] ./ f;
weight([1:4]) =[ e, d, d, d ] ./ g;
%
%  6 points, precision 3, Strang and Fix formula #4.
%
elseif( rule == 5 ) ;
a = 0.659027622374092d+00;
b = 0.231933368553031d+00;
c = 0.109039009072877d+00;
w = 1.0d+00 ./ 6.0d+00;
norder = 6;
xtab([1:6]) =[ a, a, b, b, c, c ];
ytab([1:6]) =[ b, c, a, c, a, b ];
weight([1:6]) =[ w, w, w, w, w, w ];
%
%  6 points, precision 3, Stroud T2:3-1.
%
elseif( rule == 6 ) ;
a = 0.0d+00;
b = 0.5d+00;
c = 2.0d+00 ./  3.0d+00;
d = 1.0d+00 ./  6.0d+00;
v = 1.0d+00 ./ 30.0d+00;
w = 3.0d+00 ./ 10.0d+00;
norder = 6;
xtab([1:6]) =[ a, b, b, c, d, d ];
ytab([1:6]) =[ b, a, b, d, c, d ];
weight([1:6]) =[ v, v, v, w, w, w ];
%
%  6 points, precision 4, Strang and Fix, formula #5.
%
elseif( rule == 7 ) ;
a = 0.816847572980459d+00;
b = 0.091576213509771d+00;
c = 0.108103018168070d+00;
d = 0.445948490915965d+00;
v = 0.109951743655322d+00;
w = 0.223381589678011d+00;
norder = 6;
xtab([1:6]) =[ a, b, b, c, d, d ];
ytab([1:6]) =[ b, a, b, d, c, d ];
weight([1:6]) =[ v, v, v, w, w, w ];
%
%  7 points, precision 4, Strang and Fix formula #6.
%
elseif( rule == 8 ) ;
a = 1.0d+00 ./ 3.0d+00;
c = 0.736712498968435d+00;
d = 0.237932366472434d+00;
e = 0.025355134551932d+00;
v = 0.375d+00;
w = 0.104166666666667d+00;
norder = 7;
xtab([1:7]) =[ a, c, c, d, d, e, e ];
ytab([1:7]) =[ a, d, e, c, e, c, d ];
weight([1:7]) =[ v, w, w, w, w, w, w ];
%
%  7 points, precision 5, Strang and Fix formula #7, Stroud T2:5-1
%
elseif( rule == 9 ) ;
a = 1.0d+00 ./ 3.0d+00;
b =( 9.0d+00 + 2.0d+00 .* sqrt( 15.0d+00 ) ) ./ 21.0d+00;
c =( 6.0d+00 -           sqrt( 15.0d+00 ) ) ./ 21.0d+00;
d =( 9.0d+00 - 2.0d+00 .* sqrt( 15.0d+00 ) ) ./ 21.0d+00;
e =( 6.0d+00 +           sqrt( 15.0d+00 ) ) ./ 21.0d+00;
u = 0.225d+00;
v =( 155.0d+00 - sqrt( 15.0d+00 ) ) ./ 1200.0d+00;
w =( 155.0d+00 + sqrt( 15.0d+00 ) ) ./ 1200.0d+00;
norder = 7;
xtab([1:7]) =[ a, b, c, c, d, e, e ];
ytab([1:7]) =[ a, c, b, c, e, d, e ];
weight([1:7]) =[ u, v, v, v, w, w, w ];
%
%  9 points, precision 6, Strang and Fix formula #8.
%
elseif( rule == 10 ) ;
a = 0.124949503233232d+00;
b = 0.437525248383384d+00;
c = 0.797112651860071d+00;
d = 0.165409927389841d+00;
e = 0.037477420750088d+00;
u = 0.205950504760887d+00;
v = 0.063691414286223d+00;
norder = 9;
xtab([1:9]) =[ a, b, b, c, c, d, d, e, e ];
ytab([1:9]) =[ b, a, b, d, e, c, e, c, d ];
weight([1:9]) =[ u, u, u, v, v, v, v, v, v ];
%
%  12 points, precision 6, Strang and Fix, formula #9.
%
elseif( rule == 11 ) ;
a = 0.873821971016996d+00;
b = 0.063089014491502d+00;
c = 0.501426509658179d+00;
d = 0.249286745170910d+00;
e = 0.636502499121399d+00;
f = 0.310352451033785d+00;
g = 0.053145049844816d+00;
u = 0.050844906370207d+00;
v = 0.116786275726379d+00;
w = 0.082851075618374d+00;
norder = 12;
xtab([1:12]) =[ a, b, b, d, c, d, e, e, f, f, g, g ];
ytab([1:12]) =[ b, a, b, c, d, d, f, g, e, g, e, f ];
weight([1:12]) =[ u, u, u, v, v, v, w, w, w, w, w, w ];
%
%  13 points, precision 7, Strang and Fix, formula #10.
%
elseif( rule == 12 ) ;
a = 0.479308067841923d+00;
b = 0.260345966079038d+00;
c = 0.869739794195568d+00;
d = 0.065130102902216d+00;
e = 0.638444188569809d+00;
f = 0.312865496004875d+00;
g = 0.048690315425316d+00;
h = 1.0d+00 ./ 3.0d+00;
t = 0.175615257433204d+00;
u = 0.053347235608839d+00;
v = 0.077113760890257d+00;
w = -0.149570044467670d+00;
norder = 13;
xtab([1:13]) =[ a, b, b, c, d, d, e, e, f, f, g, g, h ];
ytab([1:13]) =[ b, a, b, d, c, d, f, g, e, g, e, f, h ];
weight([1:13]) =[ t, t, t, u, u, u, v, v, v, v, v, v, w ];
%
%  7 points, precision ?.
%
elseif( rule == 13 ) ;
a = 1.0d+00 ./ 3.0d+00;
b = 1.0d+00;
c = 0.5d+00;
z = 0.0d+00;
u = 27.0d+00 ./ 60.0d+00;
v =  3.0d+00 ./ 60.0d+00;
w =  8.0d+00 ./ 60.0d+00;
norder = 7;
xtab([1:7]) =[ a, b, z, z, z, c, c ];
ytab([1:7]) =[ a, z, b, z, c, z, c ];
weight([1:7]) =[ u, v, v, v, w, w, w ];
%
%  16 points.
%
elseif( rule == 14 ) ;
norder = 16;
norder2 = 4;
[ norder2, xtab1, weight1 ]=legendre_set( norder2, xtab1, weight1 );
xtab1([1:norder2]) = 0.5d+00 .*( xtab1([1:norder2]) + 1.0d+00 );
weight2(1) = 0.1355069134d+00;
weight2(2) = 0.2034645680d+00;
weight2(3) = 0.1298475476d+00;
weight2(4) = 0.0311809709d+00;
xtab2(1) = 0.0571041961d+00;
xtab2(2) = 0.2768430136d+00;
xtab2(3) = 0.5835904324d+00;
xtab2(4) = 0.8602401357d+00;
k = 0;
for i = 1: norder2;
for j = 1: norder2;
k = fix(k + 1);
xtab(k) = xtab2(j);
ytab(k) = xtab1(i) .*( 1.0d+00 - xtab2(j) );
weight(k) = weight1(i) .* weight2(j);
end; j = fix(norder2+1);
end; i = fix(norder2+1);
%
%  64 points, precision 15.
%
elseif( rule == 15 ) ;
norder = 64;
weight2(1) = 0.00329519144d+00;
weight2(2) = 0.01784290266d+00;
weight2(3) = 0.04543931950d+00;
weight2(4) = 0.07919959949d+00;
weight2(5) = 0.10604735944d+00;
weight2(6) = 0.11250579947d+00;
weight2(7) = 0.09111902364d+00;
weight2(8) = 0.04455080436d+00;
xtab2(1) = 0.04463395529d+00;
xtab2(2) = 0.14436625704d+00;
xtab2(3) = 0.28682475714d+00;
xtab2(4) = 0.45481331520d+00;
xtab2(5) = 0.62806783542d+00;
xtab2(6) = 0.78569152060d+00;
xtab2(7) = 0.90867639210d+00;
xtab2(8) = 0.98222008485d+00;
norder2 = 8;
[ norder2, xtab1, weight1 ]=legendre_set( norder2, xtab1, weight1 );
k = 0;
for j = 1: norder2;
for i = 1: norder2;
k = fix(k + 1);
xtab(k) = 1.0d+00 - xtab2(j);
ytab(k) = 0.5d+00 .*( 1.0d+00 + xtab1(i) ) .* xtab2(j);
weight(k) = weight1(i) .* weight2(j);
end; i = fix(norder2+1);
end; j = fix(norder2+1);
%
%  19 points, precision 8.
%
elseif( rule == 16 ) ;
a = 1.0d+00 ./ 3.0d+00;
b =( 9.0d+00 + 2.0d+00 .* sqrt( 15.0d+00 ) ) ./ 21.0d+00;
c =( 6.0d+00 -       sqrt( 15.0d+00 ) ) ./ 21.0d+00;
d =( 9.0d+00 - 2.0d+00 .* sqrt( 15.0d+00 ) ) ./ 21.0d+00;
e =( 6.0d+00 +       sqrt( 15.0d+00 ) ) ./ 21.0d+00;
f =( 40.0d+00 - 10.0d+00 .* sqrt( 15.0d+00 )+ 10.0d+00 .* sqrt( 7.0d+00 ) + 2.0d+00 .* sqrt( 105.0d+00 ) ) ./ 90.0d+00;
g =( 25.0d+00 +  5.0d+00 .* sqrt( 15.0d+00 )-  5.0d+00 .* sqrt( 7.0d+00 ) - sqrt( 105.0d+00 ) ) ./ 90.0d+00;
p =( 40.0d+00 + 10.0d+00 .* sqrt( 15.0d+00 )+ 10.0d+00 .* sqrt( 7.0d+00 ) - 2.0d+00 .* sqrt( 105.0d+00 ) ) ./ 90.0d+00;
q =( 25.0d+00 -  5.0d+00 .* sqrt( 15.0d+00 )-  5.0d+00 .* sqrt( 7.0d+00 ) + sqrt( 105.0d+00 ) ) ./ 90.0d+00;
r =( 40.0d+00 + 10.0d+00 .* sqrt( 7.0d+00 ) ) ./ 90.0d+00;
s =( 25.0d+00 +  5.0d+00 .* sqrt( 15.0d+00 ) - 5.0d+00 .* sqrt( 7.0d+00 )- sqrt( 105.0d+00 ) ) ./ 90.0d+00;
t =( 25.0d+00 -  5.0d+00 .* sqrt( 15.0d+00 ) - 5.0d+00 .* sqrt( 7.0d+00 )+ sqrt( 105.0d+00 ) ) ./ 90.0d+00;
w1 =( 7137.0d+00 - 1800.0d+00 .* sqrt( 7.0d+00 ) ) ./ 62720.0d+00;
w2 = - 9301697.0d+00 ./ 4695040.0d+00 - 13517313.0d+00 .* sqrt( 15.0d+00 )./ 23475200.0d+00 + 764885.0d+00 .* sqrt( 7.0d+00 ) ./ 939008.0d+00+ 198763.0d+00 .* sqrt( 105.0d+00 ) ./ 939008.0d+00;
w2 = w2 ./ 3.0d+00;
w3 = -9301697.0d+00 ./ 4695040.0d+00 + 13517313.0d+00 .* sqrt( 15.0d+00 )./ 23475200.0d+00+ 764885.0d+00 .* sqrt( 7.0d+00 ) ./ 939008.0d+00- 198763.0d+00 .* sqrt( 105.0d+00 ) ./ 939008.0d+00;
w3 = w3 ./ 3.0d+00;
w4 =( 102791225.0d+00 - 23876225.0d+00 .* sqrt( 15.0d+00 )- 34500875.0d+00 .* sqrt( 7.0d+00 )+ 9914825.0d+00 .* sqrt( 105.0d+00 ) ) ./ 59157504.0d+00;
w4 = w4 ./ 3.0d+00;
w5 =( 102791225.0d+00 + 23876225.0d+00 .* sqrt( 15.0d+00 )- 34500875.0d+00 .* sqrt( 7.0d+00 )- 9914825d+00 .* sqrt( 105.0d+00 ) ) ./ 59157504.0d+00;
w5 = w5 ./ 3.0d+00;
w6 =( 11075.0d+00 - 3500.0d+00 .* sqrt( 7.0d+00 ) ) ./ 8064.0d+00;
w6 = w6 ./ 6.0d+00;
norder = 19;
xtab([1:19]) =[  a,  b,  c,  c,  d,  e,  e,  f,  g,  g,  p,  q,  q,r,  r,  s,  s,  t,  t ];
ytab([1:19]) =[  a,  c,  b,  c,  e,  d,  e,  g,  f,  g,  q,  p,  q,s,  t,  r,  t,  r,  s ];
weight([1:19]) =[ w1, w2, w2, w2, w3, w3, w3, w4, w4, w4, w5, w5, w5,w6, w6, w6, w6, w6, w6 ];
%
%  19 points, precision 9.
%
elseif( rule == 17 ) ;
norder = 19;
a = 1.0d+00 ./ 3.0d+00;
b = 0.02063496160252593d+00;
c = 0.4896825191987370d+00;
d = 0.1258208170141290d+00;
e = 0.4370895914929355d+00;
f = 0.6235929287619356d+00;
g = 0.1882035356190322d+00;
r = 0.9105409732110941d+00;
s = 0.04472951339445297d+00;
t = 0.7411985987844980d+00;
u = 0.03683841205473626d+00;
v = 0.22196288916076574d+00;
w1 = 0.09713579628279610d+00;
w2 = 0.03133470022713983d+00;
w3 = 0.07782754100477543d+00;
w4 = 0.07964773892720910d+00;
w5 = 0.02557767565869810d+00;
w6 = 0.04328353937728940d+00;
xtab([1:19]) =[  a,  b,  c,  c,  d,  e,  e,  f,  g,  g,  r,  s,  s,t, t, u, u, v, v ];
ytab([1:19]) =[  a,  c,  b,  c,  e,  d,  e,  g,  f,  g,  s,  r,  s,u, v, t, v, t, u ];
weight([1:19]) =[ w1, w2, w2, w2, w3, w3, w3, w4, w4, w4, w5, w5, w5,w6, w6, w6, w6, w6, w6 ];
%
%  28 points, precision 11.
%
elseif( rule == 18 ) ;
a = 1.0d+00 ./ 3.0d+00;
b = 0.9480217181434233d+00;
c = 0.02598914092828833d+00;
d = 0.8114249947041546d+00;
e = 0.09428750264792270d+00;
f = 0.01072644996557060d+00;
g = 0.4946367750172147d+00;
p = 0.5853132347709715d+00;
q = 0.2073433826145142d+00;
r = 0.1221843885990187d+00;
s = 0.4389078057004907d+00;
t = 0.6779376548825902d+00;
u = 0.04484167758913055d+00;
v = 0.27722066752827925d+00;
w = 0.8588702812826364d+00;
x = 0.0d+00;
y = 0.1411297187173636d+00;
w1 = 0.08797730116222190d+00;
w2 = 0.008744311553736190d+00;
w3 = 0.03808157199393533d+00;
w4 = 0.01885544805613125d+00;
w5 = 0.07215969754474100d+00;
w6 = 0.06932913870553720d+00;
w7 = 0.04105631542928860d+00;
w8 = 0.007362383783300573d+00;
norder = 28;
xtab([1:28]) =[  a,  b,  c,  c,  d,  e,  e,  f,  g,  g,  p,  q,  q,r,  s,  s,  t,  t,  u,  u,  v,  v,  w,  w,  x,  x,  y,  y ];
ytab([1:28]) =[  a,  c,  b,  c,  e,  d,  e,  g,  f,  g,  q,  p,  q,s,  r,  s,  u,  v,  t,  v,  t,  u,  x,  y,  w,  y,  w,  x ];
weight([1:28]) =[ w1, w2, w2, w2, w3, w3, w3, w4, w4, w4, w5, w5, w5,w6, w6, w6, w7, w7, w7, w7, w7, w7, w8, w8, w8, w8, w8, w8 ];
%
%  37 points, precision 13.
%
elseif( rule == 19 ) ;
a = 1.0d+00 ./ 3.0d+00;
b = 0.950275662924105565450352089520d+00;
c = 0.024862168537947217274823955239d+00;
d = 0.171614914923835347556304795551d+00;
e = 0.414192542538082326221847602214d+00;
f = 0.539412243677190440263092985511d+00;
g = 0.230293878161404779868453507244d+00;
w1 = 0.051739766065744133555179145422d+00;
w2 = 0.008007799555564801597804123460d+00;
w3 = 0.046868898981821644823226732071d+00;
w4 = 0.046590940183976487960361770070d+00;
w5 = 0.031016943313796381407646220131d+00;
w6 = 0.010791612736631273623178240136d+00;
w7 = 0.032195534242431618819414482205d+00;
w8 = 0.015445834210701583817692900053d+00;
w9 = 0.017822989923178661888748319485d+00;
wx = 0.037038683681384627918546472190d+00;
norder = 37;
xtab([1:10]) =[ a, b, c, c, d, e, e, f, g, g ];
ytab([1:10]) =[ a, c, b, c, e, d, e, g, f, g ];
weight([1:37]) =[ w1, w2, w2, w2, w3, w3, w3, w4, w4, w4, w5, w5, w5,w6, w6, w6, w7, w7, w7, w8, w8, w8, w8, w8, w8, w9,w9, w9, w9, w9, w9, wx, wx, wx, wx, wx, wx ];
a = 0.772160036676532561750285570113d+00;
b = 0.113919981661733719124857214943d+00;
xtab(11) = a;
ytab(11) = b;
xtab(12) = b;
ytab(12) = a;
xtab(13) = b;
ytab(13) = b;
a = 0.009085399949835353883572964740d+00;
b = 0.495457300025082323058213517632d+00;
xtab(14) = a;
ytab(14) = b;
xtab(15) = b;
ytab(15) = a;
xtab(16) = b;
ytab(16) = b;
a = 0.062277290305886993497083640527d+00;
b = 0.468861354847056503251458179727d+00;
xtab(17) = a;
ytab(17) = b;
xtab(18) = b;
ytab(18) = a;
xtab(19) = b;
ytab(19) = b;
a = 0.022076289653624405142446876931d+00;
b = 0.851306504174348550389457672223d+00;
c = 1.0d+00 - a - b;
xtab(20) = a;
ytab(20) = b;
xtab(21) = a;
ytab(21) = c;
xtab(22) = b;
ytab(22) = a;
xtab(23) = b;
ytab(23) = c;
xtab(24) = c;
ytab(24) = a;
xtab(25) = c;
ytab(25) = b;
a = 0.018620522802520968955913511549d+00;
b = 0.689441970728591295496647976487d+00;
c = 1.0d+00 - a - b;
xtab(26) = a;
ytab(26) = b;
xtab(27) = a;
ytab(27) = c;
xtab(28) = b;
ytab(28) = a;
xtab(29) = b;
ytab(29) = c;
xtab(30) = c;
ytab(30) = a;
xtab(31) = c;
ytab(31) = b;
a = 0.096506481292159228736516560903d+00;
b = 0.635867859433872768286976979827d+00;
c = 1.0d+00 - a - b;
xtab(32) = a;
ytab(32) = b;
xtab(33) = a;
ytab(33) = c;
xtab(34) = b;
ytab(34) = a;
xtab(35) = b;
ytab(35) = c;
xtab(36) = c;
ytab(36) = a;
xtab(37) = c;
ytab(37) = b;
else;
writef(1,['%s \n'], ' ');
writef(1,['%s \n'], 'TRIANGLE_UNIT_SET - Fatal error!');
writef(1,['%s %0.15g \n'], '  Illegal value of RULE = ', rule);
error(['stop encountered in original fortran code  ',char(10),';']);
end;
weight_shape=zeros(weight_shape);weight_shape(:)=weight(1:numel(weight_shape));weight=weight_shape;
xtab_shape=zeros(xtab_shape);xtab_shape(:)=xtab(1:numel(xtab_shape));xtab=xtab_shape;
ytab_shape=zeros(ytab_shape);ytab_shape(:)=ytab(1:numel(ytab_shape));ytab=ytab_shape;
return;
end
function [func, norder, xtab, ytab, weight, result]=triangle_unit_sum( func, norder, xtab, ytab, weight, result );
%
%*******************************************************************************
%
%! TRIANGLE_UNIT_SUM carries out a quadrature rule in the unit triangle.
%
%
%  Integration region:
%
%    Points (X,Y) such that:
%
%      0 <= X <= 1 - Y,
%      0 <= Y <= 1 - X.
%
%  Modified:
%
%    25 August 1998
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, external FUNC, the name of the user supplied
%    function of two variables which is to be integrated,
%    of the form:
%
%      function func ( x, y )
%      doubleprecision func
%      doubleprecision x
%      doubleprecision y
%
%    Input, integer NORDER, the order of the rule.
%
%    Input, doubleprecision XTAB(NORDER), YTAB(NORDER), the abscissas.
%
%    Input, doubleprecision WEIGHT(NORDER), the weights of the rule.
%
%    Output, doubleprecision RESULT, the approximate integral of the function.
%
%
persistent i quad volume ; 

if isempty(i), i=0; end;
if isempty(quad), quad=0; end;
if isempty(volume), volume=0; end;
%
%
quad = 0.0d+00;
for i = 1: norder;
quad = quad + weight(i) .* func( xtab(i), ytab(i) );
end; i = fix(norder+1);
volume = triangle_unit_volume( );
result = quad .* volume;
return;
end
function [triangle_unit_volumeresult]=triangle_unit_volume( );
%
%*******************************************************************************
%
%! TRIANGLE_UNIT_VOLUME returns the 'volume' of the unit triangle in 2D.
%
%
%  Discussion:
%
%    The 'volume' of a triangle is usually called its area.
%
%  Modified:
%
%    27 October 2000
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Output, doubleprecision TRIANGLE_UNIT_VOLUME, the volume of the unit
%    triangle.
%
triangle_unit_volumeresult=[];
persistent triangle_unit_volume ; 

if isempty(triangle_unit_volumeresult), triangle_unit_volumeresult=0; end;
%
triangle_unit_volumeresult = 1.0d+00 ./ 2.0d+00;
return;
end
function [triangle_volumeresult, x, y ]=triangle_volume( x, y );
%
%*******************************************************************************
%
%! TRIANGLE_VOLUME returns the 'volume' of a triangle in 2D.
%
%
%  Modified:
%
%    19 November 2000
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, doubleprecision X(3), Y(3), the vertices of the triangle.
%
%    Output, doubleprecision TRIANGLE_VOLUME, the volume of the triangle.
%
triangle_volumeresult=[];
persistent triangle_volume ; 

if isempty(triangle_volumeresult), triangle_volumeresult=0; end;
%
triangle_volumeresult = 0.5d+00 .* abs(x(1) .*( y(2) - y(3) ) +x(2) .*( y(3) - y(1) ) +x(3) .*( y(1) - y(2) ) );
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(2)), assignin('caller','FUntemp',y); evalin('caller',[inputname(2),'=FUntemp;']); end
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',x); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [nt, t]=tvec_even( nt, t );
%
%*******************************************************************************
%
%! TVEC_EVEN computes an evenly spaced set of angles between 0 and 2*PI.
%
%
%  Discussion:
%
%    The computation realizes that 0 = 2 * PI, and does not include that value.
%
%  Example:
%
%    NT = 4
%
%    T = ( 0, PI/2, PI, 3*PI/2 )
%
%  Modified:
%
%    14 December 2000
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, integer NT, the number of values to compute.
%
%    Output, doubleprecision TVEC(NT), the evenly spaced angles, in radians.
%
%
persistent i pi ; 

if isempty(i), i=0; end;
if isempty(pi), pi =3.14159265358979323846264338327950288419716939937510d+00; end;
%
for i = 1: nt;
t(i) = ( 2 .*( i - 1 ) ) .* pi ./ ( nt );
end; i = fix(nt+1);
return;
end
function [nt, t]=tvec_even2( nt, t );
%
%*******************************************************************************
%
%! TVEC_EVEN2 computes an evenly spaced set of angles between 0 and 2*PI.
%
%
%  Discussion:
%
%    The computation realizes that 0 = 2 * PI.  The values are equally
%    spaced in the circle, do not include 0, and are symmetric about 0.
%
%  Example:
%
%    NT = 4
%
%    T = ( PI/4, 3*PI/4, 5*PI/4, 7*PI/4 )
%
%  Modified:
%
%    15 December 2000
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, integer NT, the number of values to compute.
%
%    Output, doubleprecision TVEC(NT), the evenly spaced angles, in radians.
%
%
persistent i pi ; 

if isempty(i), i=0; end;
if isempty(pi), pi =3.14159265358979323846264338327950288419716939937510d+00; end;
%
for i = 1: nt;
t(i) = ( 2 .* i - 1 ) .* pi ./ ( nt );
end; i = fix(nt+1);
return;
end
function [nt, t]=tvec_even3( nt, t );
%
%*******************************************************************************
%
%! TVEC_EVEN3 computes an evenly spaced set of angles between 0 and 2*PI.
%
%
%  Discussion:
%
%    The angles begin with 0 and end with 2*PI.
%
%  Example:
%
%    NT = 4
%
%    T = ( 0, 2*PI/3, 4*PI/3 2*PI )
%
%  Modified:
%
%    13 January 2001
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, integer NT, the number of values to compute.
%
%    Output, doubleprecision TVEC(NT), the evenly spaced angles, in radians.
%
%
persistent i pi ; 

if isempty(i), i=0; end;
if isempty(pi), pi =3.14159265358979323846264338327950288419716939937510d+00; end;
%
if( nt == 1 )
t(1) = pi;
else;
for i = 1: nt;
t(i) = ( 2 .*( i - 1 ) ) .* pi ./ ( nt - 1 );
end; i = fix(nt+1);
end;
return;
end
function [theta1, theta2, nt, t]=tvec_even_bracket( theta1, theta2, nt, t );
%
%*******************************************************************************
%
%! TVEC_EVEN_BRACKET computes an evenly spaced set of angles between THETA1 and THETA2.
%
%
%  Example:
%
%    NT = 4
%    THETA1 = 30
%    THETA2 = 90
%
%    T = ( 30, 50, 70, 90 )
%
%  Modified:
%
%    13 January 2001
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, doubleprecision THETA1, THETA2, the limiting angles.
%
%    Input, integer NT, the number of values to compute.
%
%    Output, doubleprecision TVEC(NT), the evenly spaced angles.
%
%
persistent i ; 

if isempty(i), i=0; end;
%
if( nt == 1 )
t(i) =( theta1 + theta2 ) ./ 2.0d+00;
else;
for i = 1: nt;
t(i) = ( ( nt - i ) .* theta1+ ( i - 1 ) .* theta2 ) ./ ( nt - 1 );
end; i = fix(nt+1);
end;
return;
end
function [theta1, theta2, nt, t]=tvec_even_bracket2( theta1, theta2, nt, t );
%
%*******************************************************************************
%
%! TVEC_EVEN_BRACKET2 computes an evenly spaced set of angles between THETA1 and THETA2.
%
%
%  Example:
%
%    NT = 5
%    THETA1 = 30
%    THETA2 = 90
%
%    T = ( 40, 50, 60, 70, 80 )
%
%  Modified:
%
%    13 January 2001
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, doubleprecision THETA1, THETA2, the limiting angles.
%
%    Input, integer NT, the number of values to compute.
%
%    Output, doubleprecision TVEC(NT), the evenly spaced angles.
%
%
persistent i ; 

if isempty(i), i=0; end;
%
for i = 1: nt;
t(i) = ( ( nt + 1 - i ) .* theta1+ ( i ) .* theta2 ) ./ ( nt + 1 );
end; i = fix(nt+1);
return;
end
function [theta1, theta2, nt, t]=tvec_even_bracket3( theta1, theta2, nt, t );
%
%*******************************************************************************
%
%! TVEC_EVEN_BRACKET3 computes an evenly spaced set of angles between THETA1 and THETA2.
%
%
%  Example:
%
%    NT = 3
%    THETA1 = 30
%    THETA2 = 90
%
%    T = ( 40, 60, 80 )
%
%  Modified:
%
%    13 January 2001
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, doubleprecision THETA1, THETA2, the limiting angles.
%
%    Input, integer NT, the number of values to compute.
%
%    Output, doubleprecision TVEC(NT), the evenly spaced angles.
%
%
persistent i ; 

if isempty(i), i=0; end;
%
for i = 1: nt;
t(i) = ( ( 2 .* nt + 1 - 2 .* i ) .* theta1+ ( 2 .* i - 1 ) .* theta2 ) ./ ( 2 .* nt );
end; i = fix(nt+1);
return;
end
function [n, iarray, more, ibase]=vec_next( n, iarray, more, ibase );
%
%*******************************************************************************
%
%! VEC_NEXT generates all N-vectors of integers modulo a given base.
%
%
%  Examples:
%
%    N = 2, IBASE = 3
%
%    0   0
%    0   1
%    0   2
%    1   0
%    1   1
%    1   2
%    2   0
%    2   1
%    2   2
%
%  Comment:
%
%    The vectors are produced in lexical order, starting with
%    (0,0,...,0), (0,0,...,1), ... through (IBASE-1,IBASE-1,...,IBASE-1).
%
%  Modified:
%
%    15 April 1999
%
%  Parameters:
%
%    Input, integer N, the size of the vectors to be used.
%
%    Output, integer IARRAY(N).  On each return, IARRAY
%    will contain entries in the range 0 to IBASE-1.
%
%    Input/output, logical MORE.  Set this variable false before
%    the first call.  Normally, MORE will be returned true but
%    once all the vectors have been generated, MORE will be
%    reset false and you should stop calling the program.
%
%    Input, integer IBASE, the base to be used.  IBASE = 2 will
%    give vectors of 0's and 1's, for instance.
%
%
persistent i kount last nn ; 

if isempty(i), i=0; end;
if isempty(kount), kount = 0; end;
if isempty(last), last = 0; end;
if isempty(nn), nn=0; end;
%
if( ~ more )
kount = 1;
last = fix(ibase.^n);
more = true;
iarray([1:n]) = 0;
else;
kount = fix(kount + 1);
if( kount == last )
more = false;
end;
iarray(n) = fix(iarray(n) + 1);
for i = 1: n;
nn = fix(n - i);
if( iarray(nn+1) < ibase )
return;
end;
iarray(nn+1) = 0;
if( nn ~= 0 )
iarray(nn) = fix(iarray(nn) + 1);
end;
end; i = fix(n+1);
end;
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