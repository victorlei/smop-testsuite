function main(varargin)
%*******************************************************************************
%
%! TOMS757_PRB tests the routines in TOMS757.
%
%  Discussion:
%
%    This program tests the 37 functions in the MISCFUN package.
%    It is a fairly simple code with each function being tested
%    at 20 different arguments.  The code compares the value
%    from the function with a pre-computed value, and produces
%    the absolute and relative errors.
%
%  Author:
%
%    Allan McLeod,
%    Department of Mathematics and Statistics,
%    Paisley University, High Street, Paisley, Scotland, PA12BE
%    macl_ms0@paisley.ac.uk
%
clear global; clear functions;


timestamp( );
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TOMS757_PRB:');
writef(1,['%s','\n'], '  Test the uncommon special function routines in TOMS757.');
test01;
test02;
test03;
test04;
test05;
test06;
test07;
test08;
test09;
test10;
test11;
test12;
test13;
test14;
test15;
test16;
test17;
test18;
test19;
test20;
test21;
test22;
test23;
test24;
test25;
test26;
test27;
test28;
test29;
test30;
test31;
test32;
test33;
test34;
test35;
test36;
test37;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TOMS757_PRB:');
writef(1,['%s','\n'], '  Normal end of execution.');
writef(1,['%s','\n'], ' ');
timestamp( );
%stop
end
function test01(varargin)
%*******************************************************************************
%
%! TEST01 tests ABRAM0.
%
persistent abserr comp fx n_data relerr x ; 

if isempty(abserr), abserr=0; end;
if isempty(comp), comp=0; end;
if isempty(fx), fx=0; end;
if isempty(n_data), n_data=0; end;
if isempty(relerr), relerr=0; end;
if isempty(x), x=0; end;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TEST01');
writef(1,['%s','\n'], '  Testing function ABRAM0');
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], '     Argument             Abs. error             Rel. error');
writef(1,['%s','\n'], ' ');
n_data = 0;
while (1);
[ n_data, x, fx ]=abram0_values( n_data, x, fx );
if( n_data <= 0 )
break;
end;
[comp , x ]=abram0( x );
abserr = abs( fx - comp );
relerr = abserr ./ abs( fx );
writef(1,[repmat(' ',1,2),'%15.10f',repmat(' ',1,2),'%15.5f',repmat(' ',1,8),'%15.5f','\n'], x, abserr, relerr);
end;
return;
end
function test02(varargin)
%*******************************************************************************
%
%! TEST02 tests ABRAM1.
%
%
persistent abserr comp fx n_data relerr x ; 

if isempty(abserr), abserr=0; end;
if isempty(comp), comp=0; end;
if isempty(fx), fx=0; end;
if isempty(n_data), n_data=0; end;
if isempty(relerr), relerr=0; end;
if isempty(x), x=0; end;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TEST02');
writef(1,['%s','\n'], '  Testing function ABRAM1');
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], '     Argument             Abs. error             Rel. error');
writef(1,['%s','\n'], ' ');
n_data = 0;
while (1);
[ n_data, x, fx ]=abram1_values( n_data, x, fx );
if( n_data <= 0 )
break;
end;
[comp , x ]=abram1( x );
abserr = abs( fx - comp );
relerr = abserr ./ abs( fx );
writef(1,[repmat(' ',1,2),'%15.10f',repmat(' ',1,2),'%15.5f',repmat(' ',1,8),'%15.5f','\n'], x, abserr, relerr);
end;
return;
end
function test03(varargin)
%*******************************************************************************
%
%! TEST03 tests ABRAM2.
%
persistent abserr comp fx n_data relerr x ; 

if isempty(abserr), abserr=0; end;
if isempty(comp), comp=0; end;
if isempty(fx), fx=0; end;
if isempty(n_data), n_data=0; end;
if isempty(relerr), relerr=0; end;
if isempty(x), x=0; end;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TEST03');
writef(1,['%s','\n'], '  Testing function ABRAM2');
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], '     Argument             Abs. error             Rel. error');
writef(1,['%s','\n'], ' ');
n_data = 0;
while (1);
[ n_data, x, fx ]=abram2_values( n_data, x, fx );
if( n_data <= 0 )
break;
end;
[comp , x ]=abram2( x );
abserr = abs( fx - comp );
relerr = abserr ./ abs( fx );
writef(1,[repmat(' ',1,2),'%15.10f',repmat(' ',1,2),'%15.5f',repmat(' ',1,8),'%15.5f','\n'], x, abserr, relerr);
end;
return;
end
function test04(varargin)
%*******************************************************************************
%
%! TEST04 tests AIRY_AI_INT.
%
persistent abserr comp fx n_data relerr x ; 

if isempty(abserr), abserr=0; end;
if isempty(comp), comp=0; end;
if isempty(fx), fx=0; end;
if isempty(n_data), n_data=0; end;
if isempty(relerr), relerr=0; end;
if isempty(x), x=0; end;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TEST04');
writef(1,['%s','\n'], '  Testing function AIRY_AI_INT');
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], '     Argument             Abs. error             Rel. error');
writef(1,['%s','\n'], ' ');
n_data = 0;
while (1);
[ n_data, x, fx ]=airy_ai_int_values( n_data, x, fx );
if( n_data <= 0 )
break;
end;
[comp , x ]=airy_ai_int( x );
abserr = abs( fx - comp );
relerr = abserr ./ abs( fx );
writef(1,[repmat(' ',1,2),'%15.10f',repmat(' ',1,2),'%15.5f',repmat(' ',1,8),'%15.5f','\n'], x, abserr, relerr);
end;
return;
end
function test05(varargin)
%*******************************************************************************
%
%! TEST05 tests AIRY_BI_INT.
%
persistent abserr comp fx n_data relerr x ; 

if isempty(abserr), abserr=0; end;
if isempty(comp), comp=0; end;
if isempty(fx), fx=0; end;
if isempty(n_data), n_data=0; end;
if isempty(relerr), relerr=0; end;
if isempty(x), x=0; end;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TEST05');
writef(1,['%s','\n'], '  Testing function AIRY_BI_INT');
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], '     Argument             Abs. error             Rel. error');
writef(1,['%s','\n'], ' ');
n_data = 0;
while (1);
[ n_data, x, fx ]=airy_bi_int_values( n_data, x, fx );
if( n_data <= 0 )
break;
end;
[comp , x ]=airy_bi_int( x );
abserr = abs( fx - comp );
relerr = abserr ./ abs( fx );
writef(1,[repmat(' ',1,2),'%15.10f',repmat(' ',1,2),'%15.5f',repmat(' ',1,8),'%15.5f','\n'], x, abserr, relerr);
end;
return;
end
function test06(varargin)
%*******************************************************************************
%
%! TEST06 tests AIRY_GI.
%
persistent abserr comp fx n_data relerr x ; 

if isempty(abserr), abserr=0; end;
if isempty(comp), comp=0; end;
if isempty(fx), fx=0; end;
if isempty(n_data), n_data=0; end;
if isempty(relerr), relerr=0; end;
if isempty(x), x=0; end;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TEST06');
writef(1,['%s','\n'], '  Testing function AIRY_GI');
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], '     Argument             Abs. error             Rel. error');
writef(1,['%s','\n'], ' ');
n_data = 0;
while (1);
[ n_data, x, fx ]=airy_gi_values( n_data, x, fx );
if( n_data <= 0 )
break;
end;
[comp , x ]=airy_gi( x );
abserr = abs( fx - comp );
relerr = abserr ./ abs( fx );
writef(1,[repmat(' ',1,2),'%15.10f',repmat(' ',1,2),'%15.5f',repmat(' ',1,8),'%15.5f','\n'], x, abserr, relerr);
end;
return;
end
function test07(varargin)
%*******************************************************************************
%
%! TEST07 tests AIRY_HI.
%
persistent abserr comp fx n_data relerr x ; 

if isempty(abserr), abserr=0; end;
if isempty(comp), comp=0; end;
if isempty(fx), fx=0; end;
if isempty(n_data), n_data=0; end;
if isempty(relerr), relerr=0; end;
if isempty(x), x=0; end;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TEST07');
writef(1,['%s','\n'], '  Testing function AIRY_HI');
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], '     Argument             Abs. error             Rel. error');
writef(1,['%s','\n'], ' ');
n_data = 0;
while (1);
[ n_data, x, fx ]=airy_hi_values( n_data, x, fx );
if( n_data <= 0 )
break;
end;
[comp , x ]=airy_hi( x );
abserr = abs( fx - comp );
relerr = abserr ./ abs( fx );
writef(1,[repmat(' ',1,2),'%15.10f',repmat(' ',1,2),'%15.5f',repmat(' ',1,8),'%15.5f','\n'], x, abserr, relerr);
end;
return;
end
function test08(varargin)
%*******************************************************************************
%
%! TEST08 tests ARCTAN_INT.
%
persistent abserr comp fx n_data relerr x ; 

if isempty(abserr), abserr=0; end;
if isempty(comp), comp=0; end;
if isempty(fx), fx=0; end;
if isempty(n_data), n_data=0; end;
if isempty(relerr), relerr=0; end;
if isempty(x), x=0; end;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TEST08');
writef(1,['%s','\n'], '  Testing function ARCTAN_INT');
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], '     Argument             Abs. error             Rel. error');
writef(1,['%s','\n'], ' ');
n_data = 0;
while (1);
[ n_data, x, fx ]=arctan_int_values( n_data, x, fx );
if( n_data <= 0 )
break;
end;
[comp , x ]=arctan_int( x );
abserr = abs( fx - comp );
relerr = abserr ./ abs( fx );
writef(1,[repmat(' ',1,2),'%15.10f',repmat(' ',1,2),'%15.5f',repmat(' ',1,8),'%15.5f','\n'], x, abserr, relerr);
end;
return;
end
function test09(varargin)
%*******************************************************************************
%
%! TEST09 tests BESSEL_I0_INT.
%
persistent abserr comp fx n_data relerr x ; 

if isempty(abserr), abserr=0; end;
if isempty(comp), comp=0; end;
if isempty(fx), fx=0; end;
if isempty(n_data), n_data=0; end;
if isempty(relerr), relerr=0; end;
if isempty(x), x=0; end;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TEST09');
writef(1,['%s','\n'], '  Testing function BESSEL_I0_INT');
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], '     Argument             Abs. error             Rel. error');
writef(1,['%s','\n'], ' ');
n_data = 0;
while (1);
[ n_data, x, fx ]=bessel_i0_int_values( n_data, x, fx );
if( n_data <= 0 )
break;
end;
[comp , x ]=bessel_i0_int( x );
abserr = abs( fx - comp );
relerr = abserr ./ abs( fx );
writef(1,[repmat(' ',1,2),'%15.10f',repmat(' ',1,2),'%15.5f',repmat(' ',1,8),'%15.5f','\n'], x, abserr, relerr);
end;
return;
end
function test10(varargin)
%*******************************************************************************
%
%! TEST10 tests BESSEL_J0_INT.
%
persistent abserr comp fx n_data relerr x ; 

if isempty(abserr), abserr=0; end;
if isempty(comp), comp=0; end;
if isempty(fx), fx=0; end;
if isempty(n_data), n_data=0; end;
if isempty(relerr), relerr=0; end;
if isempty(x), x=0; end;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TEST10');
writef(1,['%s','\n'], '  Testing function BESSEL_J0_INT');
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], '     Argument             Abs. error             Rel. error');
writef(1,['%s','\n'], ' ');
n_data = 0;
while (1);
[ n_data, x, fx ]=bessel_j0_int_values( n_data, x, fx );
if( n_data <= 0 )
break;
end;
[comp , x ]=bessel_j0_int( x );
abserr = abs( fx - comp );
relerr = abserr ./ abs( fx );
writef(1,[repmat(' ',1,2),'%15.10f',repmat(' ',1,2),'%15.5f',repmat(' ',1,8),'%15.5f','\n'], x, abserr, relerr);
end;
return;
end
function test11(varargin)
%*******************************************************************************
%
%! TEST11 tests BESSEL_K0_INT.
%
persistent abserr comp fx n_data relerr x ; 

if isempty(abserr), abserr=0; end;
if isempty(comp), comp=0; end;
if isempty(fx), fx=0; end;
if isempty(n_data), n_data=0; end;
if isempty(relerr), relerr=0; end;
if isempty(x), x=0; end;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TEST11');
writef(1,['%s','\n'], '  Testing function BESSEL_K0_INT');
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], '     Argument             Abs. error             Rel. error');
writef(1,['%s','\n'], ' ');
n_data = 0;
while (1);
[ n_data, x, fx ]=bessel_k0_int_values( n_data, x, fx );
if( n_data <= 0 )
break;
end;
[comp , x ]=bessel_k0_int( x );
abserr = abs( fx - comp );
relerr = abserr ./ abs( fx );
writef(1,[repmat(' ',1,2),'%15.10f',repmat(' ',1,2),'%15.5f',repmat(' ',1,8),'%15.5f','\n'], x, abserr, relerr);
end;
return;
end
function test12(varargin)
%*******************************************************************************
%
%! TEST12 tests BESSEL_Y0_INT.
%
persistent abserr comp fx n_data relerr x ; 

if isempty(abserr), abserr=0; end;
if isempty(comp), comp=0; end;
if isempty(fx), fx=0; end;
if isempty(n_data), n_data=0; end;
if isempty(relerr), relerr=0; end;
if isempty(x), x=0; end;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TEST12');
writef(1,['%s','\n'], '  Testing function BESSEL_Y0_INT');
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], '     Argument             Abs. error             Rel. error');
writef(1,['%s','\n'], ' ');
n_data = 0;
while (1);
[ n_data, x, fx ]=bessel_y0_int_values( n_data, x, fx );
if( n_data <= 0 )
break;
end;
[comp , x ]=bessel_y0_int( x );
abserr = abs( fx - comp );
relerr = abserr ./ abs( fx );
writef(1,[repmat(' ',1,2),'%15.10f',repmat(' ',1,2),'%15.5f',repmat(' ',1,8),'%15.5f','\n'], x, abserr, relerr);
end;
return;
end
function test13(varargin)
%*******************************************************************************
%
%! TEST13 tests CLAUSEN.
%
persistent abserr comp fx n_data relerr x ; 

if isempty(abserr), abserr=0; end;
if isempty(comp), comp=0; end;
if isempty(fx), fx=0; end;
if isempty(n_data), n_data=0; end;
if isempty(relerr), relerr=0; end;
if isempty(x), x=0; end;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TEST13');
writef(1,['%s','\n'], '  Testing function CLAUSEN');
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], '     Argument             Abs. error             Rel. error');
writef(1,['%s','\n'], ' ');
n_data = 0;
while (1);
[ n_data, x, fx ]=clausen_values( n_data, x, fx );
if( n_data <= 0 )
break;
end;
[comp , x ]=clausen( x );
abserr = abs( fx - comp );
relerr = abserr ./ abs( fx );
writef(1,[repmat(' ',1,2),'%15.10f',repmat(' ',1,2),'%15.5f',repmat(' ',1,8),'%15.5f','\n'], x, abserr, relerr);
end;
return;
end
function test14(varargin)
%*******************************************************************************
%
%! TEST14 tests DEBYE1.
%
persistent abserr comp fx n_data relerr x ; 

if isempty(abserr), abserr=0; end;
if isempty(comp), comp=0; end;
if isempty(fx), fx=0; end;
if isempty(n_data), n_data=0; end;
if isempty(relerr), relerr=0; end;
if isempty(x), x=0; end;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TEST14');
writef(1,['%s','\n'], '  Testing function DEBYE1');
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], '     Argument             Abs. error             Rel. error');
writef(1,['%s','\n'], ' ');
n_data = 0;
while (1);
[ n_data, x, fx ]=debye1_values( n_data, x, fx );
if( n_data <= 0 )
break;
end;
[comp , x ]=debye1( x );
abserr = abs( fx - comp );
relerr = abserr ./ abs( fx );
writef(1,[repmat(' ',1,2),'%15.10f',repmat(' ',1,2),'%15.5f',repmat(' ',1,8),'%15.5f','\n'], x, abserr, relerr);
end;
return;
end
function test15(varargin)
%*******************************************************************************
%
%! TEST15 tests DEBYE2.
%
persistent abserr comp fx n_data relerr x ; 

if isempty(abserr), abserr=0; end;
if isempty(comp), comp=0; end;
if isempty(fx), fx=0; end;
if isempty(n_data), n_data=0; end;
if isempty(relerr), relerr=0; end;
if isempty(x), x=0; end;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TEST15');
writef(1,['%s','\n'], '  Testing function DEBYE2');
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], '     Argument             Abs. error             Rel. error');
writef(1,['%s','\n'], ' ');
n_data = 0;
while (1);
[ n_data, x, fx ]=debye2_values( n_data, x, fx );
if( n_data <= 0 )
break;
end;
[comp , x ]=debye2( x );
abserr = abs( fx - comp );
relerr = abserr ./ abs( fx );
writef(1,[repmat(' ',1,2),'%15.10f',repmat(' ',1,2),'%15.5f',repmat(' ',1,8),'%15.5f','\n'], x, abserr, relerr);
end;
return;
end
function test16(varargin)
%*******************************************************************************
%
%! TEST16 tests DEBYE3.
%
persistent abserr comp fx n_data relerr x ; 

if isempty(abserr), abserr=0; end;
if isempty(comp), comp=0; end;
if isempty(fx), fx=0; end;
if isempty(n_data), n_data=0; end;
if isempty(relerr), relerr=0; end;
if isempty(x), x=0; end;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TEST16');
writef(1,['%s','\n'], '  Testing function DEBYE3');
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], '     Argument             Abs. error             Rel. error');
writef(1,['%s','\n'], ' ');
n_data = 0;
while (1);
[ n_data, x, fx ]=debye3_values( n_data, x, fx );
if( n_data <= 0 )
break;
end;
[comp , x ]=debye3( x );
abserr = abs( fx - comp );
relerr = abserr ./ abs( fx );
writef(1,[repmat(' ',1,2),'%15.10f',repmat(' ',1,2),'%15.5f',repmat(' ',1,8),'%15.5f','\n'], x, abserr, relerr);
end;
return;
end
function test17(varargin)
%*******************************************************************************
%
%! TEST17 tests DEBYE4.
%
persistent abserr comp fx n_data relerr x ; 

if isempty(abserr), abserr=0; end;
if isempty(comp), comp=0; end;
if isempty(fx), fx=0; end;
if isempty(n_data), n_data=0; end;
if isempty(relerr), relerr=0; end;
if isempty(x), x=0; end;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TEST17');
writef(1,['%s','\n'], '  Testing function DEBYE4');
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], '     Argument             Abs. error             Rel. error');
writef(1,['%s','\n'], ' ');
n_data = 0;
while (1);
[ n_data, x, fx ]=debye4_values( n_data, x, fx );
if( n_data <= 0 )
break;
end;
[comp , x ]=debye4( x );
abserr = abs( fx - comp );
relerr = abserr ./ abs( fx );
writef(1,[repmat(' ',1,2),'%15.10f',repmat(' ',1,2),'%15.5f',repmat(' ',1,8),'%15.5f','\n'], x, abserr, relerr);
end;
return;
end
function test18(varargin)
%*******************************************************************************
%
%! TEST18 tests EXP3_INT.
%
persistent abserr comp fx n_data relerr x ; 

if isempty(abserr), abserr=0; end;
if isempty(comp), comp=0; end;
if isempty(fx), fx=0; end;
if isempty(n_data), n_data=0; end;
if isempty(relerr), relerr=0; end;
if isempty(x), x=0; end;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TEST18');
writef(1,['%s','\n'], '  Testing function EXP3_INT');
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], '     Argument             Abs. error             Rel. error');
writef(1,['%s','\n'], ' ');
n_data = 0;
while (1);
[ n_data, x, fx ]=exp3_int_values( n_data, x, fx );
if( n_data <= 0 )
break;
end;
[comp , x ]=exp3_int( x );
abserr = abs( fx - comp );
relerr = abserr ./ abs( fx );
writef(1,[repmat(' ',1,2),'%15.10f',repmat(' ',1,2),'%15.5f',repmat(' ',1,8),'%15.5f','\n'], x, abserr, relerr);
end;
return;
end
function test19(varargin)
%*******************************************************************************
%
%! TEST19 tests GOODWIN.
%
persistent abserr comp fx n_data relerr x ; 

if isempty(abserr), abserr=0; end;
if isempty(comp), comp=0; end;
if isempty(fx), fx=0; end;
if isempty(n_data), n_data=0; end;
if isempty(relerr), relerr=0; end;
if isempty(x), x=0; end;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TEST19');
writef(1,['%s','\n'], '  Testing function GOODWIN');
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], '     Argument             Abs. error             Rel. error');
writef(1,['%s','\n'], ' ');
n_data = 0;
while (1);
[ n_data, x, fx ]=goodwin_values( n_data, x, fx );
if( n_data <= 0 )
break;
end;
[comp , x ]=goodwin( x );
abserr = abs( fx - comp );
relerr = abserr ./ abs( fx );
writef(1,[repmat(' ',1,2),'%15.10f',repmat(' ',1,2),'%15.5f',repmat(' ',1,8),'%15.5f','\n'], x, abserr, relerr);
end;
return;
end
function test20(varargin)
%*******************************************************************************
%
%! TEST20 tests I0ML0.
%
persistent abserr comp fx n_data relerr x ; 

if isempty(abserr), abserr=0; end;
if isempty(comp), comp=0; end;
if isempty(fx), fx=0; end;
if isempty(n_data), n_data=0; end;
if isempty(relerr), relerr=0; end;
if isempty(x), x=0; end;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TEST20');
writef(1,['%s','\n'], '  Testing function I0ML0');
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], '     Argument             Abs. error             Rel. error');
writef(1,['%s','\n'], ' ');
n_data = 0;
while (1);
[ n_data, x, fx ]=i0ml0_values( n_data, x, fx );
if( n_data <= 0 )
break;
end;
[comp , x ]=i0ml0( x );
abserr = abs( fx - comp );
relerr = abserr ./ abs( fx );
writef(1,[repmat(' ',1,2),'%15.10f',repmat(' ',1,2),'%15.5f',repmat(' ',1,8),'%15.5f','\n'], x, abserr, relerr);
end;
return;
end
function test21(varargin)
%*******************************************************************************
%
%! TEST21 tests I1ML1.
%
persistent abserr comp fx n_data relerr x ; 

if isempty(abserr), abserr=0; end;
if isempty(comp), comp=0; end;
if isempty(fx), fx=0; end;
if isempty(n_data), n_data=0; end;
if isempty(relerr), relerr=0; end;
if isempty(x), x=0; end;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TEST21');
writef(1,['%s','\n'], '  Testing function I1ML1');
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], '     Argument             Abs. error             Rel. error');
writef(1,['%s','\n'], ' ');
n_data = 0;
while (1);
[ n_data, x, fx ]=i1ml1_values( n_data, x, fx );
if( n_data <= 0 )
break;
end;
[comp , x ]=i1ml1( x );
abserr = abs( fx - comp );
relerr = abserr ./ abs( fx );
writef(1,[repmat(' ',1,2),'%15.10f',repmat(' ',1,2),'%15.5f',repmat(' ',1,8),'%15.5f','\n'], x, abserr, relerr);
end;
return;
end
function test22(varargin)
%*******************************************************************************
%
%! TEST22 tests LOBACHEVSKY.
%
persistent abserr comp fx n_data relerr x ; 

if isempty(abserr), abserr=0; end;
if isempty(comp), comp=0; end;
if isempty(fx), fx=0; end;
if isempty(n_data), n_data=0; end;
if isempty(relerr), relerr=0; end;
if isempty(x), x=0; end;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TEST22');
writef(1,['%s','\n'], '  Testing function LOBACHEVSKY');
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], '     Argument             Abs. error             Rel. error');
writef(1,['%s','\n'], ' ');
n_data = 0;
while (1);
[ n_data, x, fx ]=lobachevsky_values( n_data, x, fx );
if( n_data <= 0 )
break;
end;
[comp , x ]=lobachevsky( x );
abserr = abs( fx - comp );
relerr = abserr ./ abs( fx );
writef(1,[repmat(' ',1,2),'%15.10f',repmat(' ',1,2),'%15.5f',repmat(' ',1,8),'%15.5f','\n'], x, abserr, relerr);
end;
return;
end
function test23(varargin)
%*******************************************************************************
%
%! TEST23 tests STROMGEN.
%
persistent abserr comp fx n_data relerr x ; 

if isempty(abserr), abserr=0; end;
if isempty(comp), comp=0; end;
if isempty(fx), fx=0; end;
if isempty(n_data), n_data=0; end;
if isempty(relerr), relerr=0; end;
if isempty(x), x=0; end;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TEST23');
writef(1,['%s','\n'], '  Testing function STROMGEN');
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], '     Argument             Abs. error             Rel. error');
writef(1,['%s','\n'], ' ');
n_data = 0;
while (1);
[ n_data, x, fx ]=stromgen_values( n_data, x, fx );
if( n_data <= 0 )
break;
end;
[comp , x ]=stromgen( x );
abserr = abs( fx - comp );
relerr = abserr ./ abs( fx );
writef(1,[repmat(' ',1,2),'%15.10f',repmat(' ',1,2),'%15.5f',repmat(' ',1,8),'%15.5f','\n'], x, abserr, relerr);
end;
return;
end
function test24(varargin)
%*******************************************************************************
%
%! TEST24 tests STRUVE_H0.
%
persistent abserr comp fx n_data relerr x ; 

if isempty(abserr), abserr=0; end;
if isempty(comp), comp=0; end;
if isempty(fx), fx=0; end;
if isempty(n_data), n_data=0; end;
if isempty(relerr), relerr=0; end;
if isempty(x), x=0; end;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TEST24');
writef(1,['%s','\n'], '  Testing function STRUVE_H0');
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], '     Argument             Abs. error             Rel. error');
writef(1,['%s','\n'], ' ');
n_data = 0;
while (1);
[ n_data, x, fx ]=struve_h0_values( n_data, x, fx );
if( n_data <= 0 )
break;
end;
[comp , x ]=struve_h0( x );
abserr = abs( fx - comp );
relerr = abserr ./ abs( fx );
writef(1,[repmat(' ',1,2),'%15.10f',repmat(' ',1,2),'%15.5f',repmat(' ',1,8),'%15.5f','\n'], x, abserr, relerr);
end;
return;
end
function test25(varargin)
%*******************************************************************************
%
%! TEST25 tests STRUVE_H1.
%
persistent abserr comp fx n_data relerr x ; 

if isempty(abserr), abserr=0; end;
if isempty(comp), comp=0; end;
if isempty(fx), fx=0; end;
if isempty(n_data), n_data=0; end;
if isempty(relerr), relerr=0; end;
if isempty(x), x=0; end;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TEST25');
writef(1,['%s','\n'], '  Testing function STRUVE_H1');
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], '     Argument             Abs. error             Rel. error');
writef(1,['%s','\n'], ' ');
n_data = 0;
while (1);
[ n_data, x, fx ]=struve_h1_values( n_data, x, fx );
if( n_data <= 0 )
break;
end;
[comp , x ]=struve_h1( x );
abserr = abs( fx - comp );
relerr = abserr ./ abs( fx );
writef(1,[repmat(' ',1,2),'%15.10f',repmat(' ',1,2),'%15.5f',repmat(' ',1,8),'%15.5f','\n'], x, abserr, relerr);
end;
return;
end
function test26(varargin)
%*******************************************************************************
%
%! TEST26 tests STRUVE_L0.
%
persistent abserr comp fx n_data relerr x ; 

if isempty(abserr), abserr=0; end;
if isempty(comp), comp=0; end;
if isempty(fx), fx=0; end;
if isempty(n_data), n_data=0; end;
if isempty(relerr), relerr=0; end;
if isempty(x), x=0; end;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TEST26');
writef(1,['%s','\n'], '  Testing function STRUVE_L0');
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], '     Argument             Abs. error             Rel. error');
writef(1,['%s','\n'], ' ');
n_data = 0;
while (1);
[ n_data, x, fx ]=struve_l0_values( n_data, x, fx );
if( n_data <= 0 )
break;
end;
[comp , x ]=struve_l0( x );
abserr = abs( fx - comp );
relerr = abserr ./ abs( fx );
writef(1,[repmat(' ',1,2),'%15.10f',repmat(' ',1,2),'%15.5f',repmat(' ',1,8),'%15.5f','\n'], x, abserr, relerr);
end;
return;
end
function test27(varargin)
%*******************************************************************************
%
%! TEST27 tests STRUVE_L1.
%
persistent abserr comp fx n_data relerr x ; 

if isempty(abserr), abserr=0; end;
if isempty(comp), comp=0; end;
if isempty(fx), fx=0; end;
if isempty(n_data), n_data=0; end;
if isempty(relerr), relerr=0; end;
if isempty(x), x=0; end;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TEST27');
writef(1,['%s','\n'], '  Testing function STRUVE_L1');
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], '     Argument             Abs. error             Rel. error');
writef(1,['%s','\n'], ' ');
n_data = 0;
while (1);
[ n_data, x, fx ]=struve_l1_values( n_data, x, fx );
if( n_data <= 0 )
break;
end;
[comp , x ]=struve_l1( x );
abserr = abs( fx - comp );
relerr = abserr ./ abs( fx );
writef(1,[repmat(' ',1,2),'%15.10f',repmat(' ',1,2),'%15.5f',repmat(' ',1,8),'%15.5f','\n'], x, abserr, relerr);
end;
return;
end
function test28(varargin)
%*******************************************************************************
%
%! TEST28 tests SYNCH1.
%
persistent abserr comp fx n_data relerr x ; 

if isempty(abserr), abserr=0; end;
if isempty(comp), comp=0; end;
if isempty(fx), fx=0; end;
if isempty(n_data), n_data=0; end;
if isempty(relerr), relerr=0; end;
if isempty(x), x=0; end;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TEST28');
writef(1,['%s','\n'], '  Testing function SYNCH1');
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], '     Argument             Abs. error             Rel. error');
writef(1,['%s','\n'], ' ');
n_data = 0;
while (1);
[ n_data, x, fx ]=synch1_values( n_data, x, fx );
if( n_data <= 0 )
break;
end;
[comp , x ]=synch1( x );
abserr = abs( fx - comp );
relerr = abserr ./ abs( fx );
writef(1,[repmat(' ',1,2),'%15.10f',repmat(' ',1,2),'%15.5f',repmat(' ',1,8),'%15.5f','\n'], x, abserr, relerr);
end;
return;
end
function test29(varargin)
%*******************************************************************************
%
%! TEST29 tests SYNCH2.
%
persistent abserr comp fx n_data relerr x ; 

if isempty(abserr), abserr=0; end;
if isempty(comp), comp=0; end;
if isempty(fx), fx=0; end;
if isempty(n_data), n_data=0; end;
if isempty(relerr), relerr=0; end;
if isempty(x), x=0; end;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TEST29');
writef(1,['%s','\n'], '  Testing function SYNCH2');
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], '     Argument             Abs. error             Rel. error');
writef(1,['%s','\n'], ' ');
n_data = 0;
while (1);
[ n_data, x, fx ]=synch2_values( n_data, x, fx );
if( n_data <= 0 )
break;
end;
[comp , x ]=synch2( x );
abserr = abs( fx - comp );
relerr = abserr ./ abs( fx );
writef(1,[repmat(' ',1,2),'%15.10f',repmat(' ',1,2),'%15.5f',repmat(' ',1,8),'%15.5f','\n'], x, abserr, relerr);
end;
return;
end
function test30(varargin)
%*******************************************************************************
%
%! TEST30 tests TRAN02.
%
persistent abserr comp fx n_data relerr x ; 

if isempty(abserr), abserr=0; end;
if isempty(comp), comp=0; end;
if isempty(fx), fx=0; end;
if isempty(n_data), n_data=0; end;
if isempty(relerr), relerr=0; end;
if isempty(x), x=0; end;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TEST30');
writef(1,['%s','\n'], '  Testing function TRAN02');
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], '     Argument             Abs. error             Rel. error');
writef(1,['%s','\n'], ' ');
n_data = 0;
while (1);
[ n_data, x, fx ]=tran02_values( n_data, x, fx );
if( n_data <= 0 )
break;
end;
[comp , x ]=tran02( x );
abserr = abs( fx - comp );
relerr = abserr ./ abs( fx );
writef(1,[repmat(' ',1,2),'%15.10f',repmat(' ',1,2),'%15.5f',repmat(' ',1,8),'%15.5f','\n'], x, abserr, relerr);
end;
return;
end
function test31(varargin)
%*******************************************************************************
%
%! TEST31 tests TRAN03.
%
persistent abserr comp fx n_data relerr x ; 

if isempty(abserr), abserr=0; end;
if isempty(comp), comp=0; end;
if isempty(fx), fx=0; end;
if isempty(n_data), n_data=0; end;
if isempty(relerr), relerr=0; end;
if isempty(x), x=0; end;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TEST31');
writef(1,['%s','\n'], '  Testing function TRAN03');
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], '     Argument             Abs. error             Rel. error');
writef(1,['%s','\n'], ' ');
n_data = 0;
while (1);
[ n_data, x, fx ]=tran03_values( n_data, x, fx );
if( n_data <= 0 )
break;
end;
[comp , x ]=tran03( x );
abserr = abs( fx - comp );
relerr = abserr ./ abs( fx );
writef(1,[repmat(' ',1,2),'%15.10f',repmat(' ',1,2),'%15.5f',repmat(' ',1,8),'%15.5f','\n'], x, abserr, relerr);
end;
return;
end
function test32(varargin)
%*******************************************************************************
%
%! TEST32 tests TRAN04.
%
persistent abserr comp fx n_data relerr x ; 

if isempty(abserr), abserr=0; end;
if isempty(comp), comp=0; end;
if isempty(fx), fx=0; end;
if isempty(n_data), n_data=0; end;
if isempty(relerr), relerr=0; end;
if isempty(x), x=0; end;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TEST32');
writef(1,['%s','\n'], '  Testing function TRAN04');
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], '     Argument             Abs. error             Rel. error');
writef(1,['%s','\n'], ' ');
n_data = 0;
while (1);
[ n_data, x, fx ]=tran04_values( n_data, x, fx );
if( n_data <= 0 )
break;
end;
[comp , x ]=tran04( x );
abserr = abs( fx - comp );
relerr = abserr ./ abs( fx );
writef(1,[repmat(' ',1,2),'%15.10f',repmat(' ',1,2),'%15.5f',repmat(' ',1,8),'%15.5f','\n'], x, abserr, relerr);
end;
return;
end
function test33(varargin)
%*******************************************************************************
%
%! TEST33 tests TRAN05.
%
persistent abserr comp fx n_data relerr x ; 

if isempty(abserr), abserr=0; end;
if isempty(comp), comp=0; end;
if isempty(fx), fx=0; end;
if isempty(n_data), n_data=0; end;
if isempty(relerr), relerr=0; end;
if isempty(x), x=0; end;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TEST33');
writef(1,['%s','\n'], '  Testing function TRAN05');
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], '     Argument             Abs. error             Rel. error');
writef(1,['%s','\n'], ' ');
n_data = 0;
while (1);
[ n_data, x, fx ]=tran05_values( n_data, x, fx );
if( n_data <= 0 )
break;
end;
[comp , x ]=tran05( x );
abserr = abs( fx - comp );
relerr = abserr ./ abs( fx );
writef(1,[repmat(' ',1,2),'%15.10f',repmat(' ',1,2),'%15.5f',repmat(' ',1,8),'%15.5f','\n'], x, abserr, relerr);
end;
return;
end
function test34(varargin)
%*******************************************************************************
%
%! TEST34 tests TRAN06.
%
persistent abserr comp fx n_data relerr x ; 

if isempty(abserr), abserr=0; end;
if isempty(comp), comp=0; end;
if isempty(fx), fx=0; end;
if isempty(n_data), n_data=0; end;
if isempty(relerr), relerr=0; end;
if isempty(x), x=0; end;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TEST34');
writef(1,['%s','\n'], '  Testing function TRAN06');
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], '     Argument             Abs. error             Rel. error');
writef(1,['%s','\n'], ' ');
n_data = 0;
while (1);
[ n_data, x, fx ]=tran06_values( n_data, x, fx );
if( n_data <= 0 )
break;
end;
[comp , x ]=tran06( x );
abserr = abs( fx - comp );
relerr = abserr ./ abs( fx );
writef(1,[repmat(' ',1,2),'%15.10f',repmat(' ',1,2),'%15.5f',repmat(' ',1,8),'%15.5f','\n'], x, abserr, relerr);
end;
return;
end
function test35(varargin)
%*******************************************************************************
%
%! TEST35 tests TRAN07.
%
persistent abserr comp fx n_data relerr x ; 

if isempty(abserr), abserr=0; end;
if isempty(comp), comp=0; end;
if isempty(fx), fx=0; end;
if isempty(n_data), n_data=0; end;
if isempty(relerr), relerr=0; end;
if isempty(x), x=0; end;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TEST35');
writef(1,['%s','\n'], '  Testing function TRAN07');
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], '     Argument             Abs. error             Rel. error');
writef(1,['%s','\n'], ' ');
n_data = 0;
while (1);
[ n_data, x, fx ]=tran07_values( n_data, x, fx );
if( n_data <= 0 )
break;
end;
[comp , x ]=tran07( x );
abserr = abs( fx - comp );
relerr = abserr ./ abs( fx );
writef(1,[repmat(' ',1,2),'%15.10f',repmat(' ',1,2),'%15.5f',repmat(' ',1,8),'%15.5f','\n'], x, abserr, relerr);
end;
return;
end
function test36(varargin)
%*******************************************************************************
%
%! TEST36 tests TRAN08.
%
persistent abserr comp fx n_data relerr x ; 

if isempty(abserr), abserr=0; end;
if isempty(comp), comp=0; end;
if isempty(fx), fx=0; end;
if isempty(n_data), n_data=0; end;
if isempty(relerr), relerr=0; end;
if isempty(x), x=0; end;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TEST36');
writef(1,['%s','\n'], '  Testing function TRAN08');
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], '     Argument             Abs. error             Rel. error');
writef(1,['%s','\n'], ' ');
n_data = 0;
while (1);
[ n_data, x, fx ]=tran08_values( n_data, x, fx );
if( n_data <= 0 )
break;
end;
[comp , x ]=tran08( x );
abserr = abs( fx - comp );
relerr = abserr ./ abs( fx );
writef(1,[repmat(' ',1,2),'%15.10f',repmat(' ',1,2),'%15.5f',repmat(' ',1,8),'%15.5f','\n'], x, abserr, relerr);
end;
return;
end
function test37(varargin)
%*******************************************************************************
%
%! TEST37 tests TRAN09.
%
persistent abserr comp fx n_data relerr x ; 

if isempty(abserr), abserr=0; end;
if isempty(comp), comp=0; end;
if isempty(fx), fx=0; end;
if isempty(n_data), n_data=0; end;
if isempty(relerr), relerr=0; end;
if isempty(x), x=0; end;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TEST37');
writef(1,['%s','\n'], '  Testing function TRAN09');
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], '     Argument             Abs. error             Rel. error');
writef(1,['%s','\n'], ' ');
n_data = 0;
while (1);
[ n_data, x, fx ]=tran09_values( n_data, x, fx );
if( n_data <= 0 )
break;
end;
[comp , x ]=tran09( x );
abserr = abs( fx - comp );
relerr = abserr ./ abs( fx );
writef(1,[repmat(' ',1,2),'%15.10f',repmat(' ',1,2),'%15.5f',repmat(' ',1,8),'%15.5f','\n'], x, abserr, relerr);
end;
return;
end
function [abram0result, xvalue ]=abram0( xvalue );
%*******************************************************************************
%
%! ABRAM0 evaluates the Abramowitz function of order 0.
%
%  Discussion:
%
%    The function is defined by:
%
%      ABRAM0(x) = Integral ( 0 <= t < infinity ) exp ( -t^2 - x / t ) dt
%
%    The code uses Chebyshev expansions with the coefficients
%    given to an accuracy of 20 decimal places.
%
%    This subroutine is set up to work on IEEE machines.
%
%  Modified:
%
%    07 August 2004
%
%  Author:
%
%    Allan McLeod,
%    Department of Mathematics and Statistics,
%    Paisley University, High Street, Paisley, Scotland, PA12BE
%    macl_ms0@paisley.ac.uk
%
%  Reference:
%
%    Allan Mcleod,
%    Algorithm 757, MISCFUN: A software package to compute uncommon
%      special functions,
%    ACM Transactions on Mathematical Software,
%    Volume 22, Number 3, September 1996, pages 288-301.
%
%  Parameters:
%
%    Input, real ( kind = 8 ) XVALUE, the argument of the function.
%
%    Output, real ( kind = 8 ) ABRAM0, the value of the function.
%
abram0result=[];
persistent ab0as ab0f ab0g ab0h abram0 asln asval fval gval gval0 half hval lnxmin nterma ntermf ntermg ntermh onerpi rt3bpi rtpib2 six t three two v x xlow1 zero ; 

if isempty(ab0f), ab0f([0:8]+1) =[real( -0.68121927093549469816d0),real( -0.78867919816149252495d0),real(  0.5121581776818819543d-1),real( -0.71092352894541296d-3),real(  0.368681808504287d-5),real( -0.917832337237d-8),real(  0.1270202563d-10),real( -0.1076888d-13),real(  0.599d-17) ]; end;
if isempty(ab0g), ab0g([0:8]+1) =[real( -0.60506039430868273190d0),real( -0.41950398163201779803d0),real(  0.1703265125190370333d-1),real( -0.16938917842491397d-3),real(  0.67638089519710d-6),real( -0.135723636255d-8),real(  0.156297065d-11),real( -0.112887d-14),real(  0.55d-18) ]; end;
if isempty(ab0h), ab0h([0:8]+1) =[real(  1.38202655230574989705d0),real( -0.30097929073974904355d0),real(  0.794288809364887241d-2),real( -0.6431910276847563d-4),real(  0.22549830684374d-6),real( -0.41220966195d-9),real(  0.44185282d-12),real( -0.30123d-15),real(  0.14d-18)  ]; end;
if isempty(ab0as), ab0as([0:27]+1) =[real(  1.97755499723693067407d+0),real( -0.1046024792004819485d-1),real(  0.69680790253625366d-3),real( -0.5898298299996599d-4),real(  0.577164455305320d-5),real( -0.61523013365756d-6),real(  0.6785396884767d-7),real( -0.723062537907d-8),real(  0.63306627365d-9),real( -0.989453793d-11),real( -0.1681980530d-10),real(  0.673799551d-11),real( -0.200997939d-11),real(  0.54055903d-12),real( -0.13816679d-12),real(  0.3422205d-13),real( -0.826686d-14),real(  0.194566d-14),real( -0.44268d-15),real(  0.9562d-16),real( -0.1883d-16),real(  0.301d-17),real( -0.19d-18),real( -0.14d-18),real(  0.11d-18),real( -0.4d-19),real(  0.2d-19),real( -0.1d-19) ]; end;
if isempty(abram0result), abram0result=0; end;
if isempty(asln), asln=0; end;
if isempty(asval), asval=0; end;
if isempty(fval), fval=0; end;
if isempty(gval), gval=0; end;
if isempty(gval0), gval0 = 0.13417650264770070909d+00; end;
if isempty(half), half = 0.5d+00; end;
if isempty(hval), hval=0; end;
if isempty(lnxmin), lnxmin = -708.3964d+00; end;
if isempty(nterma), nterma = 22; end;
if isempty(ntermf), ntermf = 8; end;
if isempty(ntermg), ntermg = 8; end;
if isempty(ntermh), ntermh = 8; end;
if isempty(onerpi), onerpi = 0.56418958354775628695d+00; end;
if isempty(rt3bpi), rt3bpi = 0.97720502380583984317d+00; end;
if isempty(rtpib2), rtpib2 = 0.88622692545275801365d+00; end;
if isempty(six), six = 6.0d+00; end;
if isempty(t), t=0; end;
if isempty(three), three = 3.0d+00; end;
if isempty(two), two = 2.0d+00; end;
if isempty(v), v=0; end;
if isempty(x), x=0; end;
if isempty(xlow1), xlow1 = 1.490116d-08; end;
if isempty(zero), zero = 0.0d+00; end;
x = xvalue;
if( x < zero )
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'ABRAM0 - Fatal error!');
writef(1,['%s','\n'], '  Argument X < 0.');
abram0result = zero;
elseif( x == zero ) ;
abram0result = rtpib2;
elseif( x < xlow1 ) ;
abram0result = rtpib2 + x .*( log( x ) - gval0 );
elseif( x <= two ) ;
t =( x .* x ./ two - half ) - half;
[fval , ntermf, ab0f, t ]=cheval( ntermf, ab0f, t );
[gval , ntermg, ab0g, t ]=cheval( ntermg, ab0g, t );
[hval , ntermh, ab0h, t ]=cheval( ntermh, ab0h, t );
abram0result = fval ./ onerpi + x .*( log( x ) .* hval - gval );
else;
v = three .*(( x ./ two ) .^( two ./ three ) );
t =( six ./ v - half ) - half;
[asval , nterma, ab0as, t ]=cheval( nterma, ab0as, t );
asln = log( asval ./ rt3bpi ) - v;
if( asln < lnxmin )
abram0result = zero;
else;
abram0result = exp( asln );
end;
end;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',xvalue); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [n_data, x, fx]=abram0_values( n_data, x, fx );
%*******************************************************************************
%
%! ABRAM0_VALUES returns some values of the Abramowitz0 function.
%
%  Discussion:
%
%    The function is defined by:
%
%      ABRAM0(x) = Integral ( 0 <= t < infinity ) exp ( -t^2 - x / t ) dt
%
%    The data was reported by McLeod.
%
%  Modified:
%
%    21 August 2004
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Milton Abramowitz and Irene Stegun,
%    Handbook of Mathematical Functions,
%    US Department of Commerce, 1964.
%
%    Allan McLeod,
%    Algorithm 757, MISCFUN: A software package to compute uncommon
%      special functions,
%    ACM Transactions on Mathematical Software,
%    Volume 22, Number 3, September 1996, pages 288-301.
%
%  Parameters:
%
%    Input/output, integer N_DATA.  The user sets N_DATA to 0 before the
%    first call.  On each call, the routine increments N_DATA by 1, and
%    returns the corresponding data; when there is no more data, the
%    output value of N_DATA will be 0 again.
%
%    Output, real ( kind = 8 ) X, the argument of the function.
%
%    Output, real ( kind = 8 ) FX, the value of the function.
%
persistent fx_vec n_max x_vec ; 

if isempty(n_max), n_max = 20; end;
if isempty(fx_vec), fx_vec([1:n_max]) =[real( 0.87377726306985360531d+00),real( 0.84721859650456925922d+00),real( 0.77288934483988301615d+00),real( 0.59684345853450151603d+00),real( 0.29871735283675888392d+00),real( 0.15004596450516388138d+00),real( 0.11114662419157955096d+00),real( 0.83909567153151897766d-01),real( 0.56552321717943417515d-01),real( 0.49876496603033790206d-01),real( 0.44100889219762791328d-01),real( 0.19738535180254062496d-01),real( 0.86193088287161479900d-02),real( 0.40224788162540127227d-02),real( 0.19718658458164884826d-02),real( 0.10045868340133538505d-02),real( 0.15726917263304498649d-03),real( 0.10352666912350263437d-04),real( 0.91229759190956745069e-06),real( 0.25628287737952698742e-09) ]; end;
if isempty(x_vec), x_vec([1:n_max]) =[real( 0.0019531250d+00),real( 0.0078125000d+00),real( 0.0312500000d+00),real( 0.1250000000d+00),real( 0.5000000000d+00),real( 1.0000000000d+00),real( 1.2500000000d+00),real( 1.5000000000d+00),real( 1.8750000000d+00),real( 2.0000000000d+00),real( 2.1250000000d+00),real( 3.0000000000d+00),real( 4.0000000000d+00),real( 5.0000000000d+00),real( 6.0000000000d+00),real( 7.0000000000d+00),real( 10.0000000000d+00),real( 15.0000000000d+00),real( 20.0000000000d+00),real( 40.0000000000d+00) ]; end;
if( n_data < 0 )
n_data = 0;
end;
n_data = fix(n_data + 1);
if( n_max < n_data )
n_data = 0;
x = 0.0d+00;
fx = 0.0d+00;
else;
x  = x_vec(n_data);
fx = fx_vec(n_data);
end;
return;
end
function [abram1result, xvalue ]=abram1( xvalue );
%*******************************************************************************
%
%! ABRAM1 evaluates the Abramowitz function of order 1.
%
%  Discussion:
%
%    The function is defined by:
%
%      ABRAM1(x) = Integral ( 0 <= t < infinity ) t * exp ( -t^2 - x / t ) dt
%
%    The code uses Chebyshev expansions with the coefficients
%    given to an accuracy of 20 decimal places.
%
%    This subroutine is set up to work on IEEE machines.
%
%  Modified:
%
%    07 August 2004
%
%  Author:
%
%    Allan McLeod,
%    Department of Mathematics and Statistics,
%    Paisley University, High Street, Paisley, Scotland, PA12BE
%    macl_ms0@paisley.ac.uk
%
%  Reference:
%
%    Allan Mcleod,
%    Algorithm 757, MISCFUN: A software package to compute uncommon
%      special functions,
%    ACM Transactions on Mathematical Software,
%    Volume 22, Number 3, September 1996, pages 288-301.
%
%  Parameters:
%
%    Input, real ( kind = 8 ) XVALUE, the argument of the function.
%
%    Output, real ( kind = 8 ) ABRAM1, the value of the function.
%
abram1result=[];
persistent ab1as ab1f ab1g ab1h abram1 asln asval firstCall fval gval half hval lnxmin nterma ntermf ntermg ntermh one onerpi rt3bpi six t three two v x xlow xlow1 zero ; if isempty(firstCall),firstCall=1;end; 

if isempty(ab1as), ab1as=zeros(1,27+1); end;
if isempty(ab1f), ab1f=zeros(1,9+1); end;
if isempty(ab1g), ab1g=zeros(1,8+1); end;
if isempty(ab1h), ab1h=zeros(1,8+1); end;
if isempty(abram1result), abram1result=0; end;
if isempty(asln), asln=0; end;
if isempty(asval), asval=0; end;
if isempty(fval), fval=0; end;
if isempty(gval), gval=0; end;
if isempty(half), half = 0.5d+00; end;
if isempty(hval), hval=0; end;
if isempty(lnxmin), lnxmin=0; end;
if isempty(nterma), nterma = 23; end;
if isempty(ntermf), ntermf = 9; end;
if isempty(ntermg), ntermg = 8; end;
if isempty(ntermh), ntermh = 8; end;
if isempty(one), one = 1.0d+00; end;
if isempty(onerpi), onerpi=0; end;
if isempty(rt3bpi), rt3bpi = 0.97720502380583984317d+00; end;
if isempty(six), six = 6.0d+00; end;
if isempty(t), t=0; end;
if isempty(three), three = 3.0d+00; end;
if isempty(two), two = 2.0d+00; end;
if isempty(v), v=0; end;
if isempty(x), x=0; end;
if isempty(xlow), xlow=0; end;
if isempty(xlow1), xlow1=0; end;
if isempty(zero), zero = 0.0d+00; end;
if firstCall,   ab1f=[1.47285192577978807369d0,0.10903497570168956257d0,-0.12430675360056569753d0,0.306197946853493315d-2,-0.2218410323076511d-4,0.6989978834451d-7,-0.11597076444d-9,0.11389776d-12,-0.7173d-16,0.3d-19];  end;
if firstCall,   ab1g=[0.39791277949054503528d0,-0.29045285226454720849d0,0.1048784695465363504d-1,-0.10249869522691336d-3,0.41150279399110d-6,-0.83652638940d-9,0.97862595d-12,-0.71868d-15,0.35d-18];  end;
if firstCall,   ab1h=[0.84150292152274947030d0,-0.7790050698774143395d-1,0.133992455878390993d-2,-0.808503907152788d-5,0.2261858281728d-7,-0.3441395838d-10,0.3159858d-13,-0.1884d-16,0.1d-19];  end;
if firstCall,   ab1as(0+1)=[2.13013643429065549448d0];  end;
if firstCall,   ab1as(1+1)=[0.6371526795218539933d-1];  end;
if firstCall,   ab1as(2+1)=[-0.129334917477510647d-2];  end;
if firstCall,   ab1as(3+1)=[0.5678328753228265d-4];  end;
if firstCall,   ab1as(4+1)=[-0.279434939177646d-5];  end;
if firstCall,   ab1as(5+1)=[0.5600214736787d-7];  end;
if firstCall,   ab1as(6+1)=[0.2392009242798d-7];  end;
if firstCall,   ab1as(7+1)=[-0.750984865009d-8];  end;
if firstCall,   ab1as(8+1)=[0.173015330776d-8];  end;
if firstCall,   ab1as(9+1)=[-0.36648877955d-9];  end;
if firstCall,   ab1as(10+1)=[0.7520758307d-10];  end;
if firstCall,   ab1as(11+1)=[-0.1517990208d-10];  end;
if firstCall,   ab1as(12+1)=[0.301713710d-11];  end;
if firstCall,   ab1as(13+1)=[-0.58596718d-12];  end;
if firstCall,   ab1as(14+1)=[0.10914455d-12];  end;
if firstCall,   ab1as(15+1)=[-0.1870536d-13];  end;
if firstCall,   ab1as(16+1)=[0.262542d-14];  end;
if firstCall,   ab1as(17+1)=[-0.14627d-15];  end;
if firstCall,   ab1as(18+1)=[-0.9500d-16];  end;
if firstCall,   ab1as(19+1)=[0.5873d-16];  end;
if firstCall,   ab1as(20+1)=[-0.2420d-16];  end;
if firstCall,   ab1as(21+1)=[0.868d-17];  end;
if firstCall,   ab1as(22+1)=[-0.290d-17];  end;
if firstCall,   ab1as(23+1)=[0.93d-18];  end;
if firstCall,   ab1as(24+1)=[-0.29d-18];  end;
if firstCall,   ab1as(25+1)=[0.9d-19];  end;
if firstCall,   ab1as(26+1)=[-0.3d-19];  end;
if firstCall,   ab1as(27+1)=[0.1d-19];  end;
if firstCall,   onerpi=[0.56418958354775628695d0];  end;
%
%  Machine-dependent constants (suitable for IEEE machines)
%
if firstCall,   xlow=[1.11023d-16];  end;
if firstCall, xlow1=[1.490116d-8];  end;
if firstCall, lnxmin=[-708.3964d0];  end;
firstCall=0;
x = xvalue;
if( x < zero )
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'ABRAM1 - Fatal error!');
writef(1,['%s','\n'], '  Argument X < 0.');
abram1result = zero;
elseif( x == zero ) ;
abram1result = half;
elseif( x < xlow ) ;
abram1result = half;
elseif( x < xlow1 ) ;
abram1result =( one - x ./ onerpi - x .* x .* log( x ) ) .* half;
elseif( x <= two ) ;
t =( x .* x ./ two - half ) - half;
[fval , ntermf, ab1f, t ]=cheval( ntermf, ab1f, t );
[gval , ntermg, ab1g, t ]=cheval( ntermg, ab1g, t );
[hval , ntermh, ab1h, t ]=cheval( ntermh, ab1h, t );
abram1result = fval - x .*( gval ./ onerpi + x .* log( x ) .* hval );
else;
v = three .*(( x ./ two ) .^( two ./ three ) );
t =( six ./ v - half ) - half;
[asval , nterma, ab1as, t ]=cheval( nterma, ab1as, t );
asln = log( asval .* sqrt( v ./ three ) ./ rt3bpi ) - v;
if( asln < lnxmin )
abram1result = zero;
else;
abram1result = exp( asln );
end;
end;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',xvalue); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [n_data, x, fx]=abram1_values( n_data, x, fx );
%*******************************************************************************
%
%! ABRAM1_VALUES returns some values of the Abramowitz1 function.
%
%  Discussion:
%
%    The function is defined by:
%
%      ABRAM1(x) = Integral ( 0 <= t < infinity ) t * exp ( -t^2 - x / t ) dt
%
%    The data was reported by McLeod.
%
%  Modified:
%
%    21 August 2004
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Milton Abramowitz and Irene Stegun,
%    Handbook of Mathematical Functions,
%    US Department of Commerce, 1964.
%
%    Allan McLeod,
%    Algorithm 757, MISCFUN: A software package to compute uncommon
%      special functions,
%    ACM Transactions on Mathematical Software,
%    Volume 22, Number 3, September 1996, pages 288-301.
%
%  Parameters:
%
%    Input/output, integer N_DATA.  The user sets N_DATA to 0 before the
%    first call.  On each call, the routine increments N_DATA by 1, and
%    returns the corresponding data; when there is no more data, the
%    output value of N_DATA will be 0 again.
%
%    Output, real ( kind = 8 ) X, the argument of the function.
%
%    Output, real ( kind = 8 ) FX, the value of the function.
%
persistent fx_vec n_max x_vec ; 

if isempty(n_max), n_max = 20; end;
if isempty(fx_vec), fx_vec([1:n_max]) =[real( 0.49828219848799921792d+00),real( 0.49324391773047288556d+00),real( 0.47431612784691234649d+00),real( 0.41095983258760410149d+00),real( 0.25317617388227035867d+00),real( 0.14656338138597777543d+00),real( 0.11421547056018366587d+00),real( 0.90026307383483764795d-01),real( 0.64088214170742303375d-01),real( 0.57446614314166191085d-01),real( 0.51581624564800730959d-01),real( 0.25263719555776416016d-01),real( 0.11930803330196594536d-01),real( 0.59270542280915272465d-02),real( 0.30609215358017829567d-02),real( 0.16307382136979552833d-02),real( 0.28371851916959455295d-03),real( 0.21122150121323238154d-04),real( 0.20344578892601627337d-05),real( 0.71116517236209642290e-09) ]; end;
if isempty(x_vec), x_vec([1:n_max]) =[real( 0.0019531250d+00),real( 0.0078125000d+00),real( 0.0312500000d+00),real( 0.1250000000d+00),real( 0.5000000000d+00),real( 1.0000000000d+00),real( 1.2500000000d+00),real( 1.5000000000d+00),real( 1.8750000000d+00),real( 2.0000000000d+00),real( 2.1250000000d+00),real( 3.0000000000d+00),real( 4.0000000000d+00),real( 5.0000000000d+00),real( 6.0000000000d+00),real( 7.0000000000d+00),real( 10.0000000000d+00),real( 15.0000000000d+00),real( 20.0000000000d+00),real( 40.0000000000d+00) ]; end;
if( n_data < 0 )
n_data = 0;
end;
n_data = fix(n_data + 1);
if( n_max < n_data )
n_data = 0;
x = 0.0d+00;
fx = 0.0d+00;
else;
x  = x_vec(n_data);
fx = fx_vec(n_data);
end;
return;
end
function [abram2result, xvalue ]=abram2( xvalue );
%*******************************************************************************
%
%! ABRAM2 evaluates the Abramowitz function of order 2.
%
%  Discussion:
%
%    The function is defined by:
%
%      ABRAM2(x) = Integral ( 0 <= t < infinity ) t^2 * exp ( -t^2 - x / t ) dt
%
%    The code uses Chebyshev expansions with the coefficients
%    given to an accuracy of 20 decimal places.
%
%    This subroutine is set up to work on IEEE machines.
%
%  Modified:
%
%    07 August 2004
%
%  Author:
%
%    Allan McLeod,
%    Department of Mathematics and Statistics,
%    Paisley University, High Street, Paisley, Scotland, PA12BE
%    macl_ms0@paisley.ac.uk
%
%  Reference:
%
%    Allan Mcleod,
%    Algorithm 757, MISCFUN: A software package to compute uncommon
%      special functions,
%    ACM Transactions on Mathematical Software,
%    Volume 22, Number 3, September 1996, pages 288-301.
%
%  Parameters:
%
%    Input, real ( kind = 8 ) XVALUE, the argument of the function.
%
%    Output, real ( kind = 8 ) ABRAM2, the value of the function.
%
abram2result=[];
persistent ab2as ab2f ab2g ab2h abram2 asln asval firstCall fval gval half hval lnxmin nterma ntermf ntermg ntermh onerpi rt3bpi rtpib4 six t three two v x xlow xlow1 zero ; if isempty(firstCall),firstCall=1;end; 

if isempty(abram2result), abram2result=0; end;
if isempty(half), half = 0.5d+00; end;
if isempty(nterma), nterma = 23; end;
if isempty(ntermf), ntermf = 9; end;
if isempty(ntermg), ntermg = 8; end;
if isempty(ntermh), ntermh = 7; end;
if isempty(six), six = real( 6.0d+00); end;
if isempty(three), three = 3.0d+00; end;
if isempty(two), two = 2.0d+00; end;
if isempty(x), x=0; end;
if isempty(zero), zero = 0.0d+00; end;
if isempty(ab2f), ab2f=zeros(1,9+1); end;
if isempty(ab2g), ab2g=zeros(1,8+1); end;
if isempty(ab2h), ab2h=zeros(1,7+1); end;
if isempty(ab2as), ab2as=zeros(1,26+1); end;
if isempty(asln), asln=0; end;
if isempty(asval), asval=0; end;
if isempty(fval), fval=0; end;
if isempty(gval), gval=0; end;
if isempty(hval), hval=0; end;
if isempty(lnxmin), lnxmin=0; end;
if isempty(onerpi), onerpi=0; end;
if isempty(rtpib4), rtpib4=0; end;
if isempty(rt3bpi), rt3bpi=0; end;
if isempty(t), t=0; end;
if isempty(v), v=0; end;
if isempty(xlow), xlow=0; end;
if isempty(xlow1), xlow1=0; end;
if firstCall,   ab2f=[1.03612162804243713846d0,0.19371246626794570012d0,-0.7258758839233007378d-1,0.174790590864327399d-2,-0.1281223233756549d-4,0.4115018153651d-7,-0.6971047256d-10,0.6990183d-13,-0.4492d-16,0.2d-19];  end;
if firstCall,   ab2g=[1.46290157198630741150d0,0.20189466883154014317d0,-0.2908292087997129022d-1,0.47061049035270050d-3,-0.257922080359333d-5,0.656133712946d-8,-0.914110203d-11,0.774276d-14,-0.429d-17];  end;
if firstCall,   ab2h=[0.30117225010910488881d0,-0.1588667818317623783d-1,0.19295936935584526d-3,-0.90199587849300d-6,0.206105041837d-8,-0.265111806d-11,0.210864d-14,-0.111d-17];  end;
if firstCall,   ab2as(0+1)=[2.46492325304334856893d0];  end;
if firstCall,   ab2as(1+1)=[0.23142797422248905432d0];  end;
if firstCall,   ab2as(2+1)=[-0.94068173010085773d-3];  end;
if firstCall,   ab2as(3+1)=[0.8290270038089733d-4];  end;
if firstCall,   ab2as(4+1)=[-0.883894704245866d-5];  end;
if firstCall,   ab2as(5+1)=[0.106638543567985d-5];  end;
if firstCall,   ab2as(6+1)=[-0.13991128538529d-6];  end;
if firstCall,   ab2as(7+1)=[0.1939793208445d-7];  end;
if firstCall,   ab2as(8+1)=[-0.277049938375d-8];  end;
if firstCall,   ab2as(9+1)=[0.39590687186d-9];  end;
if firstCall,   ab2as(10+1)=[-0.5408354342d-10];  end;
if firstCall,   ab2as(11+1)=[0.635546076d-11];  end;
if firstCall,   ab2as(12+1)=[-0.38461613d-12];  end;
if firstCall,   ab2as(13+1)=[-0.11696067d-12];  end;
if firstCall,   ab2as(14+1)=[0.6896671d-13];  end;
if firstCall,   ab2as(15+1)=[-0.2503113d-13];  end;
if firstCall,   ab2as(16+1)=[0.785586d-14];  end;
if firstCall,   ab2as(17+1)=[-0.230334d-14];  end;
if firstCall,   ab2as(18+1)=[0.64914d-15];  end;
if firstCall,   ab2as(19+1)=[-0.17797d-15];  end;
if firstCall,   ab2as(20+1)=[0.4766d-16];  end;
if firstCall,   ab2as(21+1)=[-0.1246d-16];  end;
if firstCall,   ab2as(22+1)=[0.316d-17];  end;
if firstCall,   ab2as(23+1)=[-0.77d-18];  end;
if firstCall,   ab2as(24+1)=[0.18d-18];  end;
if firstCall,   ab2as(25+1)=[-0.4d-19];  end;
if firstCall,   ab2as(26+1)=[0.1d-19];  end;
if firstCall,   rt3bpi=[0.97720502380583984317d0];  end;
if firstCall,   rtpib4=[0.44311346272637900682d0];  end;
if firstCall,   onerpi=[0.56418958354775628695d0];  end;
%
%   Machine-dependent constants (suitable for IEEE machines)
%
if firstCall,   xlow=[2.22045d-16];  end;
if firstCall, xlow1=[1.490116d-8];  end;
if firstCall, lnxmin=[-708.3964d0];  end;
firstCall=0;
x = xvalue;
if( x < zero )
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'ABRAM2 - Fatal error!');
writef(1,['%s','\n'], '  Argument X < 0.');
abram2result = zero;
elseif( x == zero ) ;
abram2result = rtpib4;
elseif( x < xlow ) ;
abram2result = rtpib4;
elseif( x < xlow1 ) ;
abram2result = rtpib4 - half .* x + x .* x .* x .* log( x ) ./ six;
elseif( x <= 2.0d+00 ) ;
t =( x .* x ./ two - half ) - half;
[fval , ntermf, ab2f, t ]=cheval( ntermf, ab2f, t );
[gval , ntermg, ab2g, t ]=cheval( ntermg, ab2g, t );
[hval , ntermh, ab2h, t ]=cheval( ntermh, ab2h, t );
abram2result = fval ./ onerpi + x .*( x .* x .* log( x ) .* hval - gval );
else;
v = three .*(( x ./ two ) .^( two ./ three ) );
t =( six ./ v - half ) - half;
[asval , nterma, ab2as, t ]=cheval( nterma, ab2as, t );
asln = log( asval ./ rt3bpi ) + log( v ./ three ) - v;
if( asln < lnxmin )
abram2result = zero;
else;
abram2result = exp( asln );
end;
end;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',xvalue); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [n_data, x, fx]=abram2_values( n_data, x, fx );
%*******************************************************************************
%
%! ABRAM2_VALUES returns some values of the Abramowitz2 function.
%
%  Discussion:
%
%    The function is defined by:
%
%      ABRAM2(x) = Integral ( 0 <= t < infinity ) t^2 * exp ( -t^2 - x / t ) dt
%
%    The data was reported by McLeod.
%
%  Modified:
%
%    22 August 2004
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Milton Abramowitz and Irene Stegun,
%    Handbook of Mathematical Functions,
%    US Department of Commerce, 1964.
%
%    Allan McLeod,
%    Algorithm 757, MISCFUN: A software package to compute uncommon
%      special functions,
%    ACM Transactions on Mathematical Software,
%    Volume 22, Number 3, September 1996, pages 288-301.
%
%  Parameters:
%
%    Input/output, integer N_DATA.  The user sets N_DATA to 0 before the
%    first call.  On each call, the routine increments N_DATA by 1, and
%    returns the corresponding data; when there is no more data, the
%    output value of N_DATA will be 0 again.
%
%    Output, real ( kind = 8 ) X, the argument of the function.
%
%    Output, real ( kind = 8 ) FX, the value of the function.
%
persistent fx_vec n_max x_vec ; 

if isempty(n_max), n_max = 20; end;
if isempty(fx_vec), fx_vec([1:n_max]) =[real( 0.44213858162107913430d+00),real( 0.43923379545684026308d+00),real( 0.42789857297092602234d+00),real( 0.38652825661854504406d+00),real( 0.26538204413231368110d+00),real( 0.16848734838334595000d+00),real( 0.13609200032513227112d+00),real( 0.11070330027727917352d+00),real( 0.82126019995530382267d-01),real( 0.74538781999594581763d-01),real( 0.67732034377612811390d-01),real( 0.35641808698811851022d-01),real( 0.17956589956618269083d-01),real( 0.94058737143575370625d-02),real( 0.50809356204299213556d-02),real( 0.28149565414209719359d-02),real( 0.53808696422559303431d-03),real( 0.44821756380146327259d-04),real( 0.46890678427324100410d-05),real( 0.20161544850996420504d-08) ]; end;
if isempty(x_vec), x_vec([1:n_max]) =[real( 0.0019531250d+00),real( 0.0078125000d+00),real( 0.0312500000d+00),real( 0.1250000000d+00),real( 0.5000000000d+00),real( 1.0000000000d+00),real( 1.2500000000d+00),real( 1.5000000000d+00),real( 1.8750000000d+00),real( 2.0000000000d+00),real( 2.1250000000d+00),real( 3.0000000000d+00),real( 4.0000000000d+00),real( 5.0000000000d+00),real( 6.0000000000d+00),real( 7.0000000000d+00),real( 10.0000000000d+00),real( 15.0000000000d+00),real( 20.0000000000d+00),real( 40.0000000000d+00) ]; end;
if( n_data < 0 )
n_data = 0;
end;
n_data = fix(n_data + 1);
if( n_max < n_data )
n_data = 0;
x = 0.0d+00;
fx = 0.0d+00;
else;
x  = x_vec(n_data);
fx = fx_vec(n_data);
end;
return;
end
function [airy_ai_intresult, xvalue ]=airy_ai_int( xvalue );
%*******************************************************************************
%
%! AIRY_AI_INT calculates the integral of the Airy function Ai.
%
%  Discussion:
%
%    The function is defined by:
%
%      AIRY_AI_INT(x) = Integral ( 0 <= t <= x ) Ai(t) dt
%
%    The program uses Chebyshev expansions, the coefficients of which
%    are given to 20 decimal places.
%
%    This subroutine is set up to work on IEEE machines.
%
%  Modified:
%
%    07 August 2004
%
%  Author:
%
%    Allan McLeod,
%    Department of Mathematics and Statistics,
%    Paisley University, High Street, Paisley, Scotland, PA12BE
%    macl_ms0@paisley.ac.uk
%
%  Reference:
%
%    Allan Mcleod,
%    Algorithm 757, MISCFUN: A software package to compute uncommon
%      special functions,
%    ACM Transactions on Mathematical Software,
%    Volume 22, Number 3, September 1996, pages 288-301.
%
%  Parameters:
%
%    Input, real ( kind = 8 ) XVALUE, the argument of the function.
%
%    Output, real ( kind = 8 ) AIRY_AI_INT, the value of the function.
%
airy_ai_intresult=[];
persistent aaint1 aaint2 aaint3 aaint4 aaint5 airy_ai_int airzer arg eight firstCall forty1 four fr996 gval hval nine ninhun nterm1 nterm2 nterm3 nterm4 nterm5 one piby4 pitim6 rt2b3p t temp three two x xhigh1 xlow1 xneg1 z zero ; if isempty(firstCall),firstCall=1;end; 

if isempty(aaint1), aaint1=zeros(1,25+1); end;
if isempty(aaint2), aaint2=zeros(1,21+1); end;
if isempty(aaint3), aaint3=zeros(1,40+1); end;
if isempty(aaint4), aaint4=zeros(1,17+1); end;
if isempty(aaint5), aaint5=zeros(1,17+1); end;
if isempty(airy_ai_intresult), airy_ai_intresult=0; end;
if isempty(airzer), airzer=0; end;
if isempty(arg), arg=0; end;
if isempty(eight), eight = 8.0d+00; end;
if isempty(forty1), forty1=0; end;
if isempty(four), four = 4.0d+00; end;
if isempty(fr996), fr996=0; end;
if isempty(gval), gval=0; end;
if isempty(hval), hval=0; end;
if isempty(nine), nine=0; end;
if isempty(ninhun), ninhun=0; end;
if isempty(nterm1), nterm1 = 22; end;
if isempty(nterm2), nterm2 = 17; end;
if isempty(nterm3), nterm3 = 37; end;
if isempty(nterm4), nterm4=0; end;
if isempty(nterm5), nterm5=0; end;
if isempty(one), one = 1.0d+00; end;
if isempty(piby4), piby4=0; end;
if isempty(pitim6), pitim6=0; end;
if isempty(rt2b3p), rt2b3p=0; end;
if isempty(t), t=0; end;
if isempty(temp), temp=0; end;
if isempty(three), three = 3.0d+00; end;
if isempty(two), two = 2.0d+00; end;
if isempty(x), x=0; end;
if isempty(xhigh1), xhigh1=0; end;
if isempty(xlow1), xlow1=0; end;
if isempty(xneg1), xneg1=0; end;
if isempty(zero), zero = 0.0d+00; end;
if isempty(z), z=0; end;
if firstCall,   aaint1(0+1)=[0.37713517694683695526d0];  end;
if firstCall,   aaint1(1+1)=[-0.13318868432407947431d0];  end;
if firstCall,   aaint1(2+1)=[0.3152497374782884809d-1];  end;
if firstCall,   aaint1(3+1)=[-0.318543076436574077d-2];  end;
if firstCall,   aaint1(4+1)=[-0.87398764698621915d-3];  end;
if firstCall,   aaint1(5+1)=[0.46699497655396971d-3];  end;
if firstCall,   aaint1(6+1)=[-0.9544936738983692d-4];  end;
if firstCall,   aaint1(7+1)=[0.542705687156716d-5];  end;
if firstCall,   aaint1(8+1)=[0.239496406252188d-5];  end;
if firstCall,   aaint1(9+1)=[-0.75690270205649d-6];  end;
if firstCall,   aaint1(10+1)=[0.9050138584518d-7];  end;
if firstCall,   aaint1(11+1)=[0.320529456043d-8];  end;
if firstCall,   aaint1(12+1)=[-0.303825536444d-8];  end;
if firstCall,   aaint1(13+1)=[0.48900118596d-9];  end;
if firstCall,   aaint1(14+1)=[-0.1839820572d-10];  end;
if firstCall,   aaint1(15+1)=[-0.711247519d-11];  end;
if firstCall,   aaint1(16+1)=[0.151774419d-11];  end;
if firstCall,   aaint1(17+1)=[-0.10801922d-12];  end;
if firstCall,   aaint1(18+1)=[-0.963542d-14];  end;
if firstCall,   aaint1(19+1)=[0.313425d-14];  end;
if firstCall,   aaint1(20+1)=[-0.29446d-15];  end;
if firstCall,   aaint1(21+1)=[-0.477d-17];  end;
if firstCall,   aaint1(22+1)=[0.461d-17];  end;
if firstCall,   aaint1(23+1)=[-0.53d-18];  end;
if firstCall,   aaint1(24+1)=[0.1d-19];  end;
if firstCall,   aaint1(25+1)=[0.1d-19];  end;
if firstCall,   aaint2(0+1)=[1.92002524081984009769d0];  end;
if firstCall,   aaint2(1+1)=[-0.4220049417256287021d-1];  end;
if firstCall,   aaint2(2+1)=[-0.239457722965939223d-2];  end;
if firstCall,   aaint2(3+1)=[-0.19564070483352971d-3];  end;
if firstCall,   aaint2(4+1)=[-0.1547252891056112d-4];  end;
if firstCall,   aaint2(5+1)=[-0.140490186137889d-5];  end;
if firstCall,   aaint2(6+1)=[-0.12128014271367d-6];  end;
if firstCall,   aaint2(7+1)=[-0.1179186050192d-7];  end;
if firstCall,   aaint2(8+1)=[-0.104315578788d-8];  end;
if firstCall,   aaint2(9+1)=[-0.10908209293d-9];  end;
if firstCall,   aaint2(10+1)=[-0.929633045d-11];  end;
if firstCall,   aaint2(11+1)=[-0.110946520d-11];  end;
if firstCall,   aaint2(12+1)=[-0.7816483d-13];  end;
if firstCall,   aaint2(13+1)=[-0.1319661d-13];  end;
if firstCall,   aaint2(14+1)=[-0.36823d-15];  end;
if firstCall,   aaint2(15+1)=[-0.21505d-15];  end;
if firstCall,   aaint2(16+1)=[0.1238d-16];  end;
if firstCall,   aaint2(17+1)=[-0.557d-17];  end;
if firstCall,   aaint2(18+1)=[0.84d-18];  end;
if firstCall,   aaint2(19+1)=[-0.21d-18];  end;
if firstCall,   aaint2(20+1)=[0.4d-19];  end;
if firstCall,   aaint2(21+1)=[-0.1d-19];  end;
if firstCall,   aaint3(0+1)=[0.47985893264791052053d0];  end;
if firstCall,   aaint3(1+1)=[-0.19272375126169608863d0];  end;
if firstCall,   aaint3(2+1)=[0.2051154129525428189d-1];  end;
if firstCall,   aaint3(3+1)=[0.6332000070732488786d-1];  end;
if firstCall,   aaint3(4+1)=[-0.5093322261845754082d-1];  end;
if firstCall,   aaint3(5+1)=[0.1284424078661663016d-1];  end;
if firstCall,   aaint3(6+1)=[0.2760137088989479413d-1];  end;
if firstCall,   aaint3(7+1)=[-0.1547066673866649507d-1];  end;
if firstCall,   aaint3(8+1)=[-0.1496864655389316026d-1];  end;
if firstCall,   aaint3(9+1)=[0.336617614173574541d-2];  end;
if firstCall,   aaint3(10+1)=[0.530851163518892985d-2];  end;
if firstCall,   aaint3(11+1)=[0.41371226458555081d-3];  end;
if firstCall,   aaint3(12+1)=[-0.102490579926726266d-2];  end;
if firstCall,   aaint3(13+1)=[-0.32508221672025853d-3];  end;
if firstCall,   aaint3(14+1)=[0.8608660957169213d-4];  end;
if firstCall,   aaint3(15+1)=[0.6671367298120775d-4];  end;
if firstCall,   aaint3(16+1)=[0.449205999318095d-5];  end;
if firstCall,   aaint3(17+1)=[-0.670427230958249d-5];  end;
if firstCall,   aaint3(18+1)=[-0.196636570085009d-5];  end;
if firstCall,   aaint3(19+1)=[0.22229677407226d-6];  end;
if firstCall,   aaint3(20+1)=[0.22332222949137d-6];  end;
if firstCall,   aaint3(21+1)=[0.2803313766457d-7];  end;
if firstCall,   aaint3(22+1)=[-0.1155651663619d-7];  end;
if firstCall,   aaint3(23+1)=[-0.433069821736d-8];  end;
if firstCall,   aaint3(24+1)=[-0.6227777938d-10];  end;
if firstCall,   aaint3(25+1)=[0.26432664903d-9];  end;
if firstCall,   aaint3(26+1)=[0.5333881114d-10];  end;
if firstCall,   aaint3(27+1)=[-0.522957269d-11];  end;
if firstCall,   aaint3(28+1)=[-0.382229283d-11];  end;
if firstCall,   aaint3(29+1)=[-0.40958233d-12];  end;
if firstCall,   aaint3(30+1)=[0.11515622d-12];  end;
if firstCall,   aaint3(31+1)=[0.3875766d-13];  end;
if firstCall,   aaint3(32+1)=[0.140283d-14];  end;
if firstCall,   aaint3(33+1)=[-0.141526d-14];  end;
if firstCall,   aaint3(34+1)=[-0.28746d-15];  end;
if firstCall,   aaint3(35+1)=[0.923d-17];  end;
if firstCall,   aaint3(36+1)=[0.1224d-16];  end;
if firstCall,   aaint3(37+1)=[0.157d-17];  end;
if firstCall,   aaint3(38+1)=[-0.19d-18];  end;
if firstCall,   aaint3(39+1)=[-0.8d-19];  end;
if firstCall,   aaint3(40+1)=[-0.1d-19];  end;
if firstCall,   aaint4=[1.99653305828522730048d0,-0.187541177605417759d-2,-0.15377536280305750d-3,-0.1283112967682349d-4,-0.108128481964162d-5,-0.9182131174057d-7,-0.784160590960d-8,-0.67292453878d-9,-0.5796325198d-10,-0.501040991d-11,-0.43420222d-12,-0.3774305d-13,-0.328473d-14,-0.28700d-15,-0.2502d-16,-0.220d-17,-0.19d-18,-0.2d-19];  end;
if firstCall,   aaint5=[1.13024602034465716133d0,-0.464718064639872334d-2,-0.35137413382693203d-3,-0.2768117872545185d-4,-0.222057452558107d-5,-0.18089142365974d-6,-0.1487613383373d-7,-0.123515388168d-8,-0.10310104257d-9,-0.867493013d-11,-0.73080054d-12,-0.6223561d-13,-0.525128d-14,-0.45677d-15,-0.3748d-16,-0.356d-17,-0.23d-18,-0.4d-19];  end;
if firstCall,   nine=[9.0d0];  end;
if firstCall, forty1=[41.0d0];  end;
if firstCall,   ninhun=[900.0d0];  end;
if firstCall, fr996=[4996.0d0];  end;
if firstCall,   piby4=[0.78539816339744830962d0];  end;
if firstCall,   pitim6=[18.84955592153875943078d0];  end;
if firstCall,   rt2b3p=[0.46065886596178063902d0];  end;
if firstCall,   airzer=[0.35502805388781723926d0];  end;
%
%   Machine-dependant constants (suitable for IEEE machines)
%
if firstCall,   nterm4=[15];  end;
if firstCall, nterm5=[15];  end;
if firstCall,   xlow1=[2.22045d-16];  end;
if firstCall, xhigh1=[14.480884d0];  end;
if firstCall, xneg1=[-2.727134d10];  end;
firstCall=0;
x = xvalue;
if( x < xneg1 )
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'AIRY_AI_INT - Fatal error!');
writef(1,['%s','\n'], '  X too negative for accurate computation.');
airy_ai_intresult = -two ./ three;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',xvalue); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
elseif( x < -eight ) ;
z = -( x + x ) .* sqrt( -x ) ./ three;
arg = z + piby4;
temp = nine .* z .* z;
t =( fr996 - temp ) ./( ninhun + temp );
[gval , nterm4, aaint4, t ]=cheval( nterm4, aaint4, t );
[hval , nterm5, aaint5, t ]=cheval( nterm5, aaint5, t );
temp = gval .* cos( arg ) + hval .* sin( arg ) ./ z;
airy_ai_intresult = rt2b3p .* temp ./ sqrt( z ) - two ./ three;
elseif( x <= -xlow1 );
t = -x ./ four - one;
airy_ai_intresult = x .* cheval( nterm3, aaint3, t );
elseif( x < xlow1 ) ;
airy_ai_intresult = airzer .* x;
elseif( x <= four ) ;
t = x ./ two - one;
airy_ai_intresult = cheval( nterm1, aaint1, t ) .* x;
elseif( x <= xhigh1 ) ;
z =( x + x ) .* sqrt( x ) ./ three;
temp = three .* z;
t =( forty1 - temp ) ./( nine + temp );
temp = exp( -z ) .* cheval( nterm2, aaint2, t ) ./ sqrt( pitim6 .* z );
airy_ai_intresult = one ./ three - temp;
else;
airy_ai_intresult = one ./ three;
end;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',xvalue); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [n_data, x, fx]=airy_ai_int_values( n_data, x, fx );
%*******************************************************************************
%
%! AIRY_AI_INT_VALUES returns some values of the integral of the Airy function.
%
%  Discussion:
%
%    The function is defined by:
%
%      AIRY_AI_INT(x) = Integral ( 0 <= t <= x ) Ai(t) dt
%
%    The data was reported by McLeod.
%
%  Modified:
%
%    22 August 2004
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Milton Abramowitz and Irene Stegun,
%    Handbook of Mathematical Functions,
%    US Department of Commerce, 1964.
%
%    Allan McLeod,
%    Algorithm 757, MISCFUN: A software package to compute uncommon
%      special functions,
%    ACM Transactions on Mathematical Software,
%    Volume 22, Number 3, September 1996, pages 288-301.
%
%  Parameters:
%
%    Input/output, integer N_DATA.  The user sets N_DATA to 0 before the
%    first call.  On each call, the routine increments N_DATA by 1, and
%    returns the corresponding data; when there is no more data, the
%    output value of N_DATA will be 0 again.
%
%    Output, real ( kind = 8 ) X, the argument of the function.
%
%    Output, real ( kind = 8 ) FX, the value of the function.
%
persistent fx_vec n_max x_vec ; 

if isempty(n_max), n_max = 20; end;
if isempty(fx_vec), fx_vec([1:n_max]) =[real( -0.75228838916610124300d+00),real( -0.57348350185854889466d+00),real( -0.76569840313421291743d+00),real( -0.65181015505382467421d+00),real( -0.55881974894471876922d+00),real( -0.56902352870716815309d+00),real( -0.47800749642926168100d+00),real( -0.46567398346706861416d+00),real( -0.96783140945618013679d-01),real( -0.34683049857035607494d-03),real(  0.34658366917927930790d-03),real(  0.27657581846051227124d-02),real(  0.14595330491185717833d+00),real(  0.23631734191710977960d+00),real(  0.33289264538612212697d+00),real(  0.33318759129779422976d+00),real(  0.33332945170523851439d+00),real(  0.33333331724248357420d+00),real(  0.33333333329916901594d+00),real(  0.33333333333329380187d+00) ]; end;
if isempty(x_vec), x_vec([1:n_max]) =[real( -12.0000000000d+00),real( -11.0000000000d+00),real( -10.0000000000d+00),real(  -9.5000000000d+00),real(  -9.0000000000d+00),real(  -6.5000000000d+00),real(  -4.0000000000d+00),real(  -1.0000000000d+00),real(  -0.2500000000d+00),real(  -0.0009765625d+00),real(   0.0009765625d+00),real(   0.0078125000d+00),real(   0.5000000000d+00),real(   1.0000000000d+00),real(   4.0000000000d+00),real(   4.5000000000d+00),real(   6.0000000000d+00),real(   8.0000000000d+00),real(  10.0000000000d+00),real(  12.0000000000d+00) ]; end;
if( n_data < 0 )
n_data = 0;
end;
n_data = fix(n_data + 1);
if( n_max < n_data )
n_data = 0;
x = 0.0d+00;
fx = 0.0d+00;
else;
x  = x_vec(n_data);
fx = fx_vec(n_data);
end;
return;
end
function [airy_bi_intresult, xvalue ]=airy_bi_int( xvalue );
%*******************************************************************************
%
%! AIRY_BI_INT calculates the integral of the Airy function Bi.
%
%  Discussion:
%
%    The function is defined by:
%
%      AIRY_BI_INT(x) = Integral ( 0 <= t <= x ) Bi(t) dt
%
%    The program uses Chebyshev expansions, the coefficients of which
%    are given to 20 decimal places.
%
%    This subroutine is set up to work on IEEE machines.
%
%  Modified:
%
%    07 August 2004
%
%  Author:
%
%    Allan McLeod,
%    Department of Mathematics and Statistics,
%    Paisley University, High Street, Paisley, Scotland, PA12BE
%    macl_ms0@paisley.ac.uk
%
%  Reference:
%
%    Allan Mcleod,
%    Algorithm 757, MISCFUN: A software package to compute uncommon
%      special functions,
%    ACM Transactions on Mathematical Software,
%    Volume 22, Number 3, September 1996, pages 288-301.
%
%  Parameters:
%
%    Input, real ( kind = 8 ) XVALUE, the argument of the function.
%
%    Output, real ( kind = 8 ) AIRY_BI_INT, the value of the function.
%
airy_bi_intresult=[];
persistent abint1 abint2 abint3 abint4 abint5 airy_bi_int arg birzer eight f1 f2 firstCall four nine ninhun nterm1 nterm2 nterm3 nterm4 nterm5 one onept5 piby4 rt2b3p seven sixten t temp thr644 three x xhigh1 xlow1 xmax xneg1 z zero ; if isempty(firstCall),firstCall=1;end; 

if isempty(abint1), abint1=zeros(1,36+1); end;
if isempty(abint2), abint2=zeros(1,37+1); end;
if isempty(abint3), abint3=zeros(1,37+1); end;
if isempty(abint4), abint4=zeros(1,20+1); end;
if isempty(abint5), abint5=zeros(1,20+1); end;
if isempty(airy_bi_intresult), airy_bi_intresult=0; end;
if isempty(eight), eight = real( 8.0); end;
if isempty(four), four = real( 4.0d+00); end;
if isempty(nterm1), nterm1 = 33; end;
if isempty(nterm2), nterm2 = 30; end;
if isempty(nterm3), nterm3 = 34; end;
if isempty(nterm4), nterm4=0; end;
if isempty(nterm5), nterm5=0; end;
if isempty(one), one = 1.0d+00; end;
if isempty(three), three = 3.0d+00; end;
if isempty(x), x=0; end;
if isempty(zero), zero = 0.0d+00; end;
if isempty(arg), arg=0; end;
if isempty(birzer), birzer=0; end;
if isempty(f1), f1=0; end;
if isempty(f2), f2=0; end;
if isempty(nine), nine=0; end;
if isempty(ninhun), ninhun=0; end;
if isempty(onept5), onept5=0; end;
if isempty(piby4), piby4=0; end;
if isempty(rt2b3p), rt2b3p=0; end;
if isempty(sixten), sixten=0; end;
if isempty(seven), seven=0; end;
if isempty(t), t=0; end;
if isempty(temp), temp=0; end;
if isempty(thr644), thr644=0; end;
if isempty(xlow1), xlow1=0; end;
if isempty(xhigh1), xhigh1=0; end;
if isempty(xmax), xmax=0; end;
if isempty(xneg1), xneg1=0; end;
if isempty(z), z=0; end;
if firstCall,   abint1(0+1)=[0.38683352445038543350d0];  end;
if firstCall,   abint1(1+1)=[-0.8823213550888908821d-1];  end;
if firstCall,   abint1(2+1)=[0.21463937440355429239d0];  end;
if firstCall,   abint1(3+1)=[-0.4205347375891315126d-1];  end;
if firstCall,   abint1(4+1)=[0.5932422547496086771d-1];  end;
if firstCall,   abint1(5+1)=[-0.840787081124270210d-2];  end;
if firstCall,   abint1(6+1)=[0.871824772778487955d-2];  end;
if firstCall,   abint1(7+1)=[-0.12191600199613455d-3];  end;
if firstCall,   abint1(8+1)=[0.44024821786023234d-3];  end;
if firstCall,   abint1(9+1)=[0.27894686666386678d-3];  end;
if firstCall,   abint1(10+1)=[-0.7052804689785537d-4];  end;
if firstCall,   abint1(11+1)=[0.5901080066770100d-4];  end;
if firstCall,   abint1(12+1)=[-0.1370862587982142d-4];  end;
if firstCall,   abint1(13+1)=[0.505962573749073d-5];  end;
if firstCall,   abint1(14+1)=[-0.51598837766735d-6];  end;
if firstCall,   abint1(15+1)=[0.397511312349d-8];  end;
if firstCall,   abint1(16+1)=[0.9524985978055d-7];  end;
if firstCall,   abint1(17+1)=[-0.3681435887321d-7];  end;
if firstCall,   abint1(18+1)=[0.1248391688136d-7];  end;
if firstCall,   abint1(19+1)=[-0.249097619137d-8];  end;
if firstCall,   abint1(20+1)=[0.31775245551d-9];  end;
if firstCall,   abint1(21+1)=[0.5434365270d-10];  end;
if firstCall,   abint1(22+1)=[-0.4024566915d-10];  end;
if firstCall,   abint1(23+1)=[0.1393855527d-10];  end;
if firstCall,   abint1(24+1)=[-0.303817509d-11];  end;
if firstCall,   abint1(25+1)=[0.40809511d-12];  end;
if firstCall,   abint1(26+1)=[0.1634116d-13];  end;
if firstCall,   abint1(27+1)=[-0.2683809d-13];  end;
if firstCall,   abint1(28+1)=[0.896641d-14];  end;
if firstCall,   abint1(29+1)=[-0.183089d-14];  end;
if firstCall,   abint1(30+1)=[0.21333d-15];  end;
if firstCall,   abint1(31+1)=[0.1108d-16];  end;
if firstCall,   abint1(32+1)=[-0.1276d-16];  end;
if firstCall,   abint1(33+1)=[0.363d-17];  end;
if firstCall,   abint1(34+1)=[-0.62d-18];  end;
if firstCall,   abint1(35+1)=[0.5d-19];  end;
if firstCall,   abint1(36+1)=[0.1d-19];  end;
if firstCall,   abint2(0+1)=[2.04122078602516135181d0];  end;
if firstCall,   abint2(1+1)=[0.2124133918621221230d-1];  end;
if firstCall,   abint2(2+1)=[0.66617599766706276d-3];  end;
if firstCall,   abint2(3+1)=[0.3842047982808254d-4];  end;
if firstCall,   abint2(4+1)=[0.362310366020439d-5];  end;
if firstCall,   abint2(5+1)=[0.50351990115074d-6];  end;
if firstCall,   abint2(6+1)=[0.7961648702253d-7];  end;
if firstCall,   abint2(7+1)=[0.717808442336d-8];  end;
if firstCall,   abint2(8+1)=[-0.267770159104d-8];  end;
if firstCall,   abint2(9+1)=[-0.168489514699d-8];  end;
if firstCall,   abint2(10+1)=[-0.36811757255d-9];  end;
if firstCall,   abint2(11+1)=[0.4757128727d-10];  end;
if firstCall,   abint2(12+1)=[0.5263621945d-10];  end;
if firstCall,   abint2(13+1)=[0.778973500d-11];  end;
if firstCall,   abint2(14+1)=[-0.460546143d-11];  end;
if firstCall,   abint2(15+1)=[-0.183433736d-11];  end;
if firstCall,   abint2(16+1)=[0.32191249d-12];  end;
if firstCall,   abint2(17+1)=[0.29352060d-12];  end;
if firstCall,   abint2(18+1)=[-0.1657935d-13];  end;
if firstCall,   abint2(19+1)=[-0.4483808d-13];  end;
if firstCall,   abint2(20+1)=[0.27907d-15];  end;
if firstCall,   abint2(21+1)=[0.711921d-14];  end;
if firstCall,   abint2(22+1)=[-0.1042d-16];  end;
if firstCall,   abint2(23+1)=[-0.119591d-14];  end;
if firstCall,   abint2(24+1)=[0.4606d-16];  end;
if firstCall,   abint2(25+1)=[0.20884d-15];  end;
if firstCall,   abint2(26+1)=[-0.2416d-16];  end;
if firstCall,   abint2(27+1)=[-0.3638d-16];  end;
if firstCall,   abint2(28+1)=[0.863d-17];  end;
if firstCall,   abint2(29+1)=[0.591d-17];  end;
if firstCall,   abint2(30+1)=[-0.256d-17];  end;
if firstCall,   abint2(31+1)=[-0.77d-18];  end;
if firstCall,   abint2(32+1)=[0.66d-18];  end;
if firstCall,   abint2(33+1)=[0.3d-19];  end;
if firstCall,   abint2(34+1)=[-0.15d-18];  end;
if firstCall,   abint2(35+1)=[0.2d-19];  end;
if firstCall,   abint2(36+1)=[0.3d-19];  end;
if firstCall,   abint2(37+1)=[-0.1d-19];  end;
if firstCall,   abint3(0+1)=[0.31076961598640349251d0];  end;
if firstCall,   abint3(1+1)=[-0.27528845887452542718d0];  end;
if firstCall,   abint3(2+1)=[0.17355965706136543928d0];  end;
if firstCall,   abint3(3+1)=[-0.5544017909492843130d-1];  end;
if firstCall,   abint3(4+1)=[-0.2251265478295950941d-1];  end;
if firstCall,   abint3(5+1)=[0.4107347447812521894d-1];  end;
if firstCall,   abint3(6+1)=[0.984761275464262480d-2];  end;
if firstCall,   abint3(7+1)=[-0.1555618141666041932d-1];  end;
if firstCall,   abint3(8+1)=[-0.560871870730279234d-2];  end;
if firstCall,   abint3(9+1)=[0.246017783322230475d-2];  end;
if firstCall,   abint3(10+1)=[0.165740392292336978d-2];  end;
if firstCall,   abint3(11+1)=[-0.3277587501435402d-4];  end;
if firstCall,   abint3(12+1)=[-0.24434680860514925d-3];  end;
if firstCall,   abint3(13+1)=[-0.5035305196152321d-4];  end;
if firstCall,   abint3(14+1)=[0.1630264722247854d-4];  end;
if firstCall,   abint3(15+1)=[0.851914057780934d-5];  end;
if firstCall,   abint3(16+1)=[0.29790363004664d-6];  end;
if firstCall,   abint3(17+1)=[-0.64389707896401d-6];  end;
if firstCall,   abint3(18+1)=[-0.15046988145803d-6];  end;
if firstCall,   abint3(19+1)=[0.1587013535823d-7];  end;
if firstCall,   abint3(20+1)=[0.1276766299622d-7];  end;
if firstCall,   abint3(21+1)=[0.140578534199d-8];  end;
if firstCall,   abint3(22+1)=[-0.46564739741d-9];  end;
if firstCall,   abint3(23+1)=[-0.15682748791d-9];  end;
if firstCall,   abint3(24+1)=[-0.403893560d-11];  end;
if firstCall,   abint3(25+1)=[0.666708192d-11];  end;
if firstCall,   abint3(26+1)=[0.128869380d-11];  end;
if firstCall,   abint3(27+1)=[-0.6968663d-13];  end;
if firstCall,   abint3(28+1)=[-0.6254319d-13];  end;
if firstCall,   abint3(29+1)=[-0.718392d-14];  end;
if firstCall,   abint3(30+1)=[0.115296d-14];  end;
if firstCall,   abint3(31+1)=[0.42276d-15];  end;
if firstCall,   abint3(32+1)=[0.2493d-16];  end;
if firstCall,   abint3(33+1)=[-0.971d-17];  end;
if firstCall,   abint3(34+1)=[-0.216d-17];  end;
if firstCall,   abint3(35+1)=[-0.2d-19];  end;
if firstCall,   abint3(36+1)=[0.6d-19];  end;
if firstCall,   abint3(37+1)=[0.1d-19];  end;
if firstCall,   abint4(0+1)=[1.99507959313352047614d0];  end;
if firstCall,   abint4(1+1)=[-0.273736375970692738d-2];  end;
if firstCall,   abint4(2+1)=[-0.30897113081285850d-3];  end;
if firstCall,   abint4(3+1)=[-0.3550101982798577d-4];  end;
if firstCall,   abint4(4+1)=[-0.412179271520133d-5];  end;
if firstCall,   abint4(5+1)=[-0.48235892316833d-6];  end;
if firstCall,   abint4(6+1)=[-0.5678730727927d-7];  end;
if firstCall,   abint4(7+1)=[-0.671874810365d-8];  end;
if firstCall,   abint4(8+1)=[-0.79811649857d-9];  end;
if firstCall,   abint4(9+1)=[-0.9514271478d-10];  end;
if firstCall,   abint4(10+1)=[-0.1137468966d-10];  end;
if firstCall,   abint4(11+1)=[-0.136359969d-11];  end;
if firstCall,   abint4(12+1)=[-0.16381418d-12];  end;
if firstCall,   abint4(13+1)=[-0.1972575d-13];  end;
if firstCall,   abint4(14+1)=[-0.237844d-14];  end;
if firstCall,   abint4(15+1)=[-0.28752d-15];  end;
if firstCall,   abint4(16+1)=[-0.3475d-16];  end;
if firstCall,   abint4(17+1)=[-0.422d-17];  end;
if firstCall,   abint4(18+1)=[-0.51d-18];  end;
if firstCall,   abint4(19+1)=[-0.6d-19];  end;
if firstCall,   abint4(20+1)=[-0.1d-19];  end;
if firstCall,   abint5(0+1)=[1.12672081961782566017d0];  end;
if firstCall,   abint5(1+1)=[-0.671405567525561198d-2];  end;
if firstCall,   abint5(2+1)=[-0.69812918017832969d-3];  end;
if firstCall,   abint5(3+1)=[-0.7561689886425276d-4];  end;
if firstCall,   abint5(4+1)=[-0.834985574510207d-5];  end;
if firstCall,   abint5(5+1)=[-0.93630298232480d-6];  end;
if firstCall,   abint5(6+1)=[-0.10608556296250d-6];  end;
if firstCall,   abint5(7+1)=[-0.1213128916741d-7];  end;
if firstCall,   abint5(8+1)=[-0.139631129765d-8];  end;
if firstCall,   abint5(9+1)=[-0.16178918054d-9];  end;
if firstCall,   abint5(10+1)=[-0.1882307907d-10];  end;
if firstCall,   abint5(11+1)=[-0.220272985d-11];  end;
if firstCall,   abint5(12+1)=[-0.25816189d-12];  end;
if firstCall,   abint5(13+1)=[-0.3047964d-13];  end;
if firstCall,   abint5(14+1)=[-0.358370d-14];  end;
if firstCall,   abint5(15+1)=[-0.42831d-15];  end;
if firstCall,   abint5(16+1)=[-0.4993d-16];  end;
if firstCall,   abint5(17+1)=[-0.617d-17];  end;
if firstCall,   abint5(18+1)=[-0.68d-18];  end;
if firstCall,   abint5(19+1)=[-0.10d-18];  end;
if firstCall,   abint5(20+1)=[-0.1d-19];  end;
if firstCall,   onept5=[1.5d0];  end;
if firstCall,   seven=[7.0d0];  end;
if firstCall,   nine=[9.0d0];  end;
if firstCall, sixten=[16.0d0];  end;
if firstCall,   ninhun=[900.0d0];  end;
if firstCall, thr644=[3644.0d0];  end;
if firstCall,   piby4=[0.78539816339744830962d0];  end;
if firstCall,   rt2b3p=[0.46065886596178063902d0];  end;
if firstCall,   birzer=[0.61492662744600073515d0];  end;
%
%   Machine-dependent parameters (suitable for IEEE machines)
%
if firstCall,   nterm4=[17];  end;
if firstCall, nterm5=[17];  end;
if firstCall,   xlow1=[2.22044604d-16];  end;
if firstCall, xhigh1=[104.587632d0];  end;
if firstCall,   xneg1=[-2.727134d10];  end;
if firstCall, xmax=[1.79d308];  end;
firstCall=0;
x = xvalue;
if( x < xneg1 )
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'AIRY_BI_INT - Warning!');
writef(1,['%s','\n'], '  Argument is too negative for accurate computation.');
airy_bi_intresult = zero;
elseif( x < -seven ) ;
z = -( x + x ) .* sqrt( -x ) ./ three;
arg = z + piby4;
temp = nine .* z .* z;
t =( thr644 - temp ) ./( ninhun + temp );
f1 = cheval( nterm4, abint4, t ) .* sin( arg );
f2 = cheval( nterm5, abint5, t ) .* cos( arg ) ./ z;
airy_bi_intresult =( f2 - f1 ) .* rt2b3p ./ sqrt( z );
elseif( x <= -xlow1 ) ;
t = -( x + x ) ./ seven - one;
airy_bi_intresult = x .* cheval( nterm3, abint3, t );
elseif( x < xlow1 ) ;
airy_bi_intresult = birzer .* x;
elseif( x <= eight ) ;
t = x ./ four - one;
airy_bi_intresult = x .* exp( onept5 .* x ) .* cheval( nterm1, abint1, t );
elseif( x <= xhigh1 ) ;
t = sixten .* sqrt( eight ./ x ) ./ x - one;
z =( x + x ) .* sqrt( x ) ./ three;
temp = rt2b3p .* cheval( nterm2, abint2, t ) ./ sqrt( z );
temp = z + log( temp );
airy_bi_intresult = exp( temp );
else;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'AIRY_BI_INT - Warning!');
writef(1,['%s','\n'], '  Argument is too large for accurate computation.');
airy_bi_intresult = xmax;
end;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',xvalue); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [n_data, x, fx]=airy_bi_int_values( n_data, x, fx );
%*******************************************************************************
%
%! AIRY_BI_INT_VALUES returns some values of the integral of the Airy function.
%
%  Discussion:
%
%    The function is defined by:
%
%      AIRY_BI_INT(x) = Integral ( 0 <= t <= x ) Bi(t) dt
%
%    The data was reported by McLeod.
%
%  Modified:
%
%    23 August 2004
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Milton Abramowitz and Irene Stegun,
%    Handbook of Mathematical Functions,
%    US Department of Commerce, 1964.
%
%    Allan McLeod,
%    Algorithm 757, MISCFUN: A software package to compute uncommon
%      special functions,
%    ACM Transactions on Mathematical Software,
%    Volume 22, Number 3, September 1996, pages 288-301.
%
%  Parameters:
%
%    Input/output, integer N_DATA.  The user sets N_DATA to 0 before the
%    first call.  On each call, the routine increments N_DATA by 1, and
%    returns the corresponding data; when there is no more data, the
%    output value of N_DATA will be 0 again.
%
%    Output, real ( kind = 8 ) X, the argument of the function.
%
%    Output, real ( kind = 8 ) FX, the value of the function.
%
persistent fx_vec n_max x_vec ; 

if isempty(n_max), n_max = 20; end;
if isempty(fx_vec), fx_vec([1:n_max]) =[real(  0.17660819031554631869d-01),real( -0.15040424806140020451d-01),real(  0.14756446293227661920d-01),real( -0.11847304264848446271d+00),real( -0.64916741266165856037d-01),real(  0.97260832464381044540d-01),real(  0.50760058495287539119d-01),real( -0.37300500963429492179d+00),real( -0.13962988442666578531d+00),real( -0.12001735266723296160d-02),real(  0.12018836117890354598d-02),real(  0.36533846550952011043d+00),real(  0.87276911673800812196d+00),real(  0.48219475263803429675d+02),real(  0.44006525804904178439d+06),real(  0.17608153976228301458d+07),real(  0.73779211705220007228d+07),real(  0.14780980310740671617d+09),real(  0.97037614223613433849d+11),real(  0.11632737638809878460d+15) ]; end;
if isempty(x_vec), x_vec([1:n_max]) =[real( -12.0000000000d+00),real( -10.0000000000d+00),real(  -8.0000000000d+00),real(  -7.5000000000d+00),real(  -7.0000000000d+00),real(  -6.5000000000d+00),real(  -4.0000000000d+00),real(  -1.0000000000d+00),real(  -0.2500000000d+00),real(  -0.0019531250d+00),real(   0.0019531250d+00),real(   0.5000000000d+00),real(   1.0000000000d+00),real(   4.0000000000d+00),real(   8.0000000000d+00),real(   8.5000000000d+00),real(   9.0000000000d+00),real(  10.0000000000d+00),real(  12.0000000000d+00),real(  14.0000000000d+00) ]; end;
if( n_data < 0 )
n_data = 0;
end;
n_data = fix(n_data + 1);
if( n_max < n_data )
n_data = 0;
x = 0.0d+00;
fx = 0.0d+00;
else;
x  = x_vec(n_data);
fx = fx_vec(n_data);
end;
return;
end
function [airy_giresult, xvalue ]=airy_gi( xvalue );
%*******************************************************************************
%
%! AIRY_GI computes the modified Airy function Gi(x).
%
%  Discussion:
%
%    The function is defined by:
%
%      AIRY_GI(x) = Integral ( 0 <= t < infinity ) sin ( x*t+t^3/3) dt / pi
%
%    The approximation uses Chebyshev expansions with the coefficients
%    given to 20 decimal places.
%
%    This subroutine is set up to work on IEEE machines.
%
%  Modified:
%
%    07 August 2004
%
%  Author:
%
%    Allan McLeod,
%    Department of Mathematics and Statistics,
%    Paisley University, High Street, Paisley, Scotland, PA12BE
%    macl_ms0@paisley.ac.uk
%
%  Reference:
%
%    Allan Mcleod,
%    Algorithm 757, MISCFUN: A software package to compute uncommon
%      special functions,
%    ACM Transactions on Mathematical Software,
%    Volume 22, Number 3, September 1996, pages 288-301.
%
%  Parameters:
%
%    Input, real ( kind = 8 ) XVALUE, the argument of the function.
%
%    Output, real ( kind = 8 ) AIRY_GI, the value of the function.
%
airy_giresult=[];
persistent airy_gi arbin1 arbin2 argin1 argip1 argip2 arhin1 bi cheb1 cheb2 cosz firstCall five five14 four gizero minate nine nterm1 nterm2 nterm3 nterm4 nterm5 nterm6 one one024 one76 onebpi piby4 rtpiin seven seven2 sinz t temp three twelhu twent8 x xcube xhigh1 xhigh2 xhigh3 xlow1 xminus zero zeta ; if isempty(firstCall),firstCall=1;end; 

if isempty(airy_giresult), airy_giresult=0; end;
if isempty(four), four = real( 4.0d+00); end;
if isempty(nterm1), nterm1 = 28; end;
if isempty(nterm2), nterm2 = 23; end;
if isempty(nterm3), nterm3 = 39; end;
if isempty(nterm4), nterm4=0; end;
if isempty(nterm5), nterm5=0; end;
if isempty(nterm6), nterm6=0; end;
if isempty(one), one = real( 1.0); end;
if isempty(three), three = 3.0d+00; end;
if isempty(x), x=0; end;
if isempty(zero), zero = 0.0d+00; end;
if isempty(argip1), argip1=zeros(1,30+1); end;
if isempty(argip2), argip2=zeros(1,29+1); end;
if isempty(argin1), argin1=zeros(1,42+1); end;
if isempty(arbin1), arbin1=zeros(1,10+1); end;
if isempty(arbin2), arbin2=zeros(1,11+1); end;
if isempty(arhin1), arhin1=zeros(1,15+1); end;
if isempty(bi), bi=0; end;
if isempty(cheb1), cheb1=0; end;
if isempty(cheb2), cheb2=0; end;
if isempty(cosz), cosz=0; end;
if isempty(five), five=0; end;
if isempty(five14), five14=0; end;
if isempty(gizero), gizero=0; end;
if isempty(minate), minate=0; end;
if isempty(nine), nine=0; end;
if isempty(onebpi), onebpi=0; end;
if isempty(one76), one76=0; end;
if isempty(one024), one024=0; end;
if isempty(piby4), piby4=0; end;
if isempty(rtpiin), rtpiin=0; end;
if isempty(seven), seven=0; end;
if isempty(seven2), seven2=0; end;
if isempty(sinz), sinz=0; end;
if isempty(t), t=0; end;
if isempty(temp), temp=0; end;
if isempty(twelhu), twelhu=0; end;
if isempty(twent8), twent8=0; end;
if isempty(xcube), xcube=0; end;
if isempty(xhigh1), xhigh1=0; end;
if isempty(xhigh2), xhigh2=0; end;
if isempty(xhigh3), xhigh3=0; end;
if isempty(xlow1), xlow1=0; end;
if isempty(xminus), xminus=0; end;
if isempty(zeta), zeta=0; end;
if firstCall,   argip1(0+1)=[0.26585770795022745082d0];  end;
if firstCall,   argip1(1+1)=[-0.10500333097501922907d0];  end;
if firstCall,   argip1(2+1)=[0.841347475328454492d-2];  end;
if firstCall,   argip1(3+1)=[0.2021067387813439541d-1];  end;
if firstCall,   argip1(4+1)=[-0.1559576113863552234d-1];  end;
if firstCall,   argip1(5+1)=[0.564342939043256481d-2];  end;
if firstCall,   argip1(6+1)=[-0.59776844826655809d-3];  end;
if firstCall,   argip1(7+1)=[-0.42833850264867728d-3];  end;
if firstCall,   argip1(8+1)=[0.22605662380909027d-3];  end;
if firstCall,   argip1(9+1)=[-0.3608332945592260d-4];  end;
if firstCall,   argip1(10+1)=[-0.785518988788901d-5];  end;
if firstCall,   argip1(11+1)=[0.473252480746370d-5];  end;
if firstCall,   argip1(12+1)=[-0.59743513977694d-6];  end;
if firstCall,   argip1(13+1)=[-0.15917609165602d-6];  end;
if firstCall,   argip1(14+1)=[0.6336129065570d-7];  end;
if firstCall,   argip1(15+1)=[-0.276090232648d-8];  end;
if firstCall,   argip1(16+1)=[-0.256064154085d-8];  end;
if firstCall,   argip1(17+1)=[0.47798676856d-9];  end;
if firstCall,   argip1(18+1)=[0.4488131863d-10];  end;
if firstCall,   argip1(19+1)=[-0.2346508882d-10];  end;
if firstCall,   argip1(20+1)=[0.76839085d-12];  end;
if firstCall,   argip1(21+1)=[0.73227985d-12];  end;
if firstCall,   argip1(22+1)=[-0.8513687d-13];  end;
if firstCall,   argip1(23+1)=[-0.1630201d-13];  end;
if firstCall,   argip1(24+1)=[0.356769d-14];  end;
if firstCall,   argip1(25+1)=[0.25001d-15];  end;
if firstCall,   argip1(26+1)=[-0.10859d-15];  end;
if firstCall,   argip1(27+1)=[-0.158d-17];  end;
if firstCall,   argip1(28+1)=[0.275d-17];  end;
if firstCall,   argip1(29+1)=[-0.5d-19];  end;
if firstCall,   argip1(30+1)=[-0.6d-19];  end;
if firstCall,   argip2(0+1)=[2.00473712275801486391d0];  end;
if firstCall,   argip2(1+1)=[0.294184139364406724d-2];  end;
if firstCall,   argip2(2+1)=[0.71369249006340167d-3];  end;
if firstCall,   argip2(3+1)=[0.17526563430502267d-3];  end;
if firstCall,   argip2(4+1)=[0.4359182094029882d-4];  end;
if firstCall,   argip2(5+1)=[0.1092626947604307d-4];  end;
if firstCall,   argip2(6+1)=[0.272382418399029d-5];  end;
if firstCall,   argip2(7+1)=[0.66230900947687d-6];  end;
if firstCall,   argip2(8+1)=[0.15425323370315d-6];  end;
if firstCall,   argip2(9+1)=[0.3418465242306d-7];  end;
if firstCall,   argip2(10+1)=[0.728157724894d-8];  end;
if firstCall,   argip2(11+1)=[0.151588525452d-8];  end;
if firstCall,   argip2(12+1)=[0.30940048039d-9];  end;
if firstCall,   argip2(13+1)=[0.6149672614d-10];  end;
if firstCall,   argip2(14+1)=[0.1202877045d-10];  end;
if firstCall,   argip2(15+1)=[0.233690586d-11];  end;
if firstCall,   argip2(16+1)=[0.43778068d-12];  end;
if firstCall,   argip2(17+1)=[0.7996447d-13];  end;
if firstCall,   argip2(18+1)=[0.1494075d-13];  end;
if firstCall,   argip2(19+1)=[0.246790d-14];  end;
if firstCall,   argip2(20+1)=[0.37672d-15];  end;
if firstCall,   argip2(21+1)=[0.7701d-16];  end;
if firstCall,   argip2(22+1)=[0.354d-17];  end;
if firstCall,   argip2(23+1)=[-0.49d-18];  end;
if firstCall,   argip2(24+1)=[0.62d-18];  end;
if firstCall,   argip2(25+1)=[-0.40d-18];  end;
if firstCall,   argip2(26+1)=[-0.1d-19];  end;
if firstCall,   argip2(27+1)=[0.2d-19];  end;
if firstCall,   argip2(28+1)=[-0.3d-19];  end;
if firstCall,   argip2(29+1)=[0.1d-19];  end;
if firstCall,   argin1(0+1)=[-0.20118965056732089130d0];  end;
if firstCall,   argin1(1+1)=[-0.7244175303324530499d-1];  end;
if firstCall,   argin1(2+1)=[0.4505018923894780120d-1];  end;
if firstCall,   argin1(3+1)=[-0.24221371122078791099d0];  end;
if firstCall,   argin1(4+1)=[0.2717884964361678294d-1];  end;
if firstCall,   argin1(5+1)=[-0.5729321004818179697d-1];  end;
if firstCall,   argin1(6+1)=[-0.18382107860337763587d0];  end;
if firstCall,   argin1(7+1)=[0.7751546082149475511d-1];  end;
if firstCall,   argin1(8+1)=[0.18386564733927560387d0];  end;
if firstCall,   argin1(9+1)=[0.2921504250185567173d-1];  end;
if firstCall,   argin1(10+1)=[-0.6142294846788018811d-1];  end;
if firstCall,   argin1(11+1)=[-0.2999312505794616238d-1];  end;
if firstCall,   argin1(12+1)=[0.585937118327706636d-2];  end;
if firstCall,   argin1(13+1)=[0.822221658497402529d-2];  end;
if firstCall,   argin1(14+1)=[0.132579817166846893d-2];  end;
if firstCall,   argin1(15+1)=[-0.96248310766565126d-3];  end;
if firstCall,   argin1(16+1)=[-0.45065515998211807d-3];  end;
if firstCall,   argin1(17+1)=[0.772423474325474d-5];  end;
if firstCall,   argin1(18+1)=[0.5481874134758052d-4];  end;
if firstCall,   argin1(19+1)=[0.1245898039742876d-4];  end;
if firstCall,   argin1(20+1)=[-0.246196891092083d-5];  end;
if firstCall,   argin1(21+1)=[-0.169154183545285d-5];  end;
if firstCall,   argin1(22+1)=[-0.16769153169442d-6];  end;
if firstCall,   argin1(23+1)=[0.9636509337672d-7];  end;
if firstCall,   argin1(24+1)=[0.3253314928030d-7];  end;
if firstCall,   argin1(25+1)=[0.5091804231d-10];  end;
if firstCall,   argin1(26+1)=[-0.209180453553d-8];  end;
if firstCall,   argin1(27+1)=[-0.41237387870d-9];  end;
if firstCall,   argin1(28+1)=[0.4163338253d-10];  end;
if firstCall,   argin1(29+1)=[0.3032532117d-10];  end;
if firstCall,   argin1(30+1)=[0.340580529d-11];  end;
if firstCall,   argin1(31+1)=[-0.88444592d-12];  end;
if firstCall,   argin1(32+1)=[-0.31639612d-12];  end;
if firstCall,   argin1(33+1)=[-0.1505076d-13];  end;
if firstCall,   argin1(34+1)=[0.1104148d-13];  end;
if firstCall,   argin1(35+1)=[0.246508d-14];  end;
if firstCall,   argin1(36+1)=[-0.3107d-16];  end;
if firstCall,   argin1(37+1)=[-0.9851d-16];  end;
if firstCall,   argin1(38+1)=[-0.1453d-16];  end;
if firstCall,   argin1(39+1)=[0.118d-17];  end;
if firstCall,   argin1(40+1)=[0.67d-18];  end;
if firstCall,   argin1(41+1)=[0.6d-19];  end;
if firstCall,   argin1(42+1)=[-0.1d-19];  end;
if firstCall,   arbin1=[1.99983763583586155980d0,-0.8104660923669418d-4,0.13475665984689d-6,-0.70855847143d-9,0.748184187d-11,-0.12902774d-12,0.322504d-14,-0.10809d-15,0.460d-17,-0.24d-18,0.1d-19];  end;
if firstCall,   arbin2=[0.13872356453879120276d0,-0.8239286225558228d-4,0.26720919509866d-6,-0.207423685368d-8,0.2873392593d-10,-0.60873521d-12,0.1792489d-13,-0.68760d-15,0.3280d-16,-0.188d-17,0.13d-18,-0.1d-19];  end;
if firstCall,   arhin1=[1.99647720399779650525d0,-0.187563779407173213d-2,-0.12186470897787339d-3,-0.814021609659287d-5,-0.55050925953537d-6,-0.3763008043303d-7,-0.258858362365d-8,-0.17931829265d-9,-0.1245916873d-10,-0.87171247d-12,-0.6084943d-13,-0.431178d-14,-0.29787d-15,-0.2210d-16,-0.136d-17,-0.14d-18];  end;
if firstCall,   five=[5.0d0];  end;
if firstCall, seven=[7.0d0];  end;
if firstCall, minate=[-8.0d0];  end;
if firstCall,   nine=[9.0d0];  end;
if firstCall, twent8=[28.0d0];  end;
if firstCall, seven2=[72.0d0];  end;
if firstCall,   one76=[176.0d0];  end;
if firstCall, five14=[514.0d0];  end;
if firstCall,   one024=[1024.0d0];  end;
if firstCall, twelhu=[1200.0d0];  end;
if firstCall,   gizero=[0.20497554248200024505d0];  end;
if firstCall,   onebpi=[0.31830988618379067154d0];  end;
if firstCall,   piby4=[0.78539816339744830962d0];  end;
if firstCall,   rtpiin=[0.56418958354775628695d0];  end;
%
%   Machine-dependent constants (suitable for IEEE machines)
%
if firstCall,   nterm4=[9];  end;
if firstCall, nterm5=[10];  end;
if firstCall, nterm6=[14];  end;
if firstCall,   xlow1=[2.22045d-16];  end;
if firstCall, xhigh1=[208063.8307d0];  end;
if firstCall,   xhigh2=[0.14274d308];  end;
if firstCall, xhigh3=[-2097152.0d0];  end;
firstCall=0;
x = xvalue;
if( x < -xhigh1 .* xhigh1 )
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'AIRY_GI - Fatal error!');
writef(1,['%s','\n'], '  Argument too negative for accurate computation.');
airy_giresult = zero;
elseif( x <= xhigh3 ) ;
xminus = -x;
t = xminus .* sqrt( xminus );
zeta =( t + t ) ./ three;
temp = rtpiin ./ sqrt( sqrt( xminus ) );
cosz = cos( zeta + piby4 );
sinz = sin( zeta + piby4 ) ./ zeta;
xcube = x .* x .* x;
bi =( cosz + sinz .* five ./ seven2 ) .* temp;
t =( xcube + twelhu ) ./( one76 - xcube );
airy_giresult = bi + cheval( nterm6, arhin1, t ) .* onebpi ./ x;
elseif( x < minate ) ;
xminus = -x;
t = xminus .* sqrt( xminus );
zeta =( t + t ) ./ three;
temp = rtpiin ./ sqrt( sqrt( xminus ) );
cosz = cos( zeta + piby4 );
sinz = sin( zeta + piby4 ) ./ zeta;
xcube = x .* x .* x;
t = -( one024 ./( xcube ) + one );
[cheb1 , nterm4, arbin1, t ]=cheval( nterm4, arbin1, t );
[cheb2 , nterm5, arbin2, t ]=cheval( nterm5, arbin2, t );
bi =( cosz .* cheb1 + sinz .* cheb2 ) .* temp;
t =( xcube + twelhu ) ./( one76 - xcube );
airy_giresult = bi + cheval( nterm6, arhin1, t ) .* onebpi ./ x;
elseif( x <= -xlow1 ) ;
t = -( x + four ) ./ four;
[airy_giresult , nterm3, argin1, t ]=cheval( nterm3, argin1, t );
elseif( x < xlow1 ) ;
airy_giresult = gizero;
elseif( x <= seven ) ;
t =( nine .* x - twent8 ) ./( x + twent8 );
[airy_giresult , nterm1, argip1, t ]=cheval( nterm1, argip1, t );
elseif( x <= xhigh1 ) ;
xcube = x .* x .* x;
t =( twelhu - xcube ) ./( five14 + xcube );
airy_giresult = onebpi .* cheval( nterm2, argip2, t ) ./ x;
elseif( x <= xhigh2 ) ;
airy_giresult = onebpi ./ x;
else;
airy_giresult = zero;
end;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',xvalue); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [n_data, x, fx]=airy_gi_values( n_data, x, fx );
%*******************************************************************************
%
%! AIRY_GI_VALUES returns some values of the Airy Gi function.
%
%  Discussion:
%
%    The function is defined by:
%
%      AIRY_GI(x) = Integral ( 0 <= t < infinity ) sin ( x*t+t^3/3) dt / pi
%
%    The data was reported by McLeod.
%
%  Modified:
%
%    24 August 2004
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Milton Abramowitz and Irene Stegun,
%    Handbook of Mathematical Functions,
%    US Department of Commerce, 1964.
%
%    Allan McLeod,
%    Algorithm 757, MISCFUN: A software package to compute uncommon
%      special functions,
%    ACM Transactions on Mathematical Software,
%    Volume 22, Number 3, September 1996, pages 288-301.
%
%  Parameters:
%
%    Input/output, integer N_DATA.  The user sets N_DATA to 0 before the
%    first call.  On each call, the routine increments N_DATA by 1, and
%    returns the corresponding data; when there is no more data, the
%    output value of N_DATA will be 0 again.
%
%    Output, real ( kind = 8 ) X, the argument of the function.
%
%    Output, real ( kind = 8 ) FX, the value of the function.
%
persistent fx_vec n_max x_vec ; 

if isempty(n_max), n_max = 20; end;
if isempty(fx_vec), fx_vec([1:n_max]) =[real(  0.20468308070040542435d+00),real(  0.18374662832557904078d+00),real( -0.11667221729601528265d+00),real(  0.31466934902729557596d+00),real( -0.37089040722426257729d+00),real( -0.25293059772424019694d+00),real(  0.28967410658692701936d+00),real( -0.34644836492634090590d+00),real(  0.28076035913873049496d+00),real(  0.21814994508094865815d+00),real(  0.20526679000810503329d+00),real(  0.22123695363784773258d+00),real(  0.23521843981043793760d+00),real(  0.82834303363768729338d-01),real(  0.45757385490989281893d-01),real(  0.44150012014605159922d-01),real(  0.39951133719508907541d-01),real(  0.35467706833949671483d-01),real(  0.31896005100679587981d-01),real(  0.26556892713512410405d-01) ]; end;
if isempty(x_vec), x_vec([1:n_max]) =[real(  -0.0019531250d+00),real(  -0.1250000000d+00),real(  -1.0000000000d+00),real(  -4.0000000000d+00),real(  -8.0000000000d+00),real(  -8.2500000000d+00),real(  -9.0000000000d+00),real( -10.0000000000d+00),real( -11.0000000000d+00),real( -13.0000000000d+00),real(   0.0019531250d+00),real(   0.1250000000d+00),real(   1.0000000000d+00),real(   4.0000000000d+00),real(   7.0000000000d+00),real(   7.2500000000d+00),real(   8.0000000000d+00),real(   9.0000000000d+00),real(  10.0000000000d+00),real(  12.0000000000d+00) ]; end;
if( n_data < 0 )
n_data = 0;
end;
n_data = fix(n_data + 1);
if( n_max < n_data )
n_data = 0;
x = 0.0d+00;
fx = 0.0d+00;
else;
x  = x_vec(n_data);
fx = fx_vec(n_data);
end;
return;
end
function [airy_hiresult, xvalue ]=airy_hi( xvalue );
%*******************************************************************************
%
%! AIRY_HI computes the modified Airy function Hi(x).
%
%  Discussion:
%
%    The function is defined by:
%
%      AIRY_HI(x) = Integral ( 0 <= t < infinity ) exp(x*t-t^3/3) dt / pi
%
%    The approximation uses Chebyshev expansions with the coefficients
%    given to 20 decimal places.
%
%    This subroutine is set up to work on IEEE machines.
%
%  Modified:
%
%    07 August 2004
%
%  Author:
%
%    Allan McLeod,
%    Department of Mathematics and Statistics,
%    Paisley University, High Street, Paisley, Scotland, PA12BE
%    macl_ms0@paisley.ac.uk
%
%  Reference:
%
%    Allan Mcleod,
%    Algorithm 757, MISCFUN: A software package to compute uncommon
%      special functions,
%    ACM Transactions on Mathematical Software,
%    Volume 22, Number 3, September 1996, pages 288-301.
%
%  Parameters:
%
%    Input, real ( kind = 8 ) XVALUE, the argument of the function.
%
%    Output, real ( kind = 8 ) AIRY_HI, the value of the function.
%
airy_hiresult=[];
persistent airy_hi arbip argip1 arhin1 arhin2 arhip bi firstCall five14 four gi hizero lnrtpi minate nterm1 nterm2 nterm3 nterm4 nterm5 one one76 onebpi seven t temp thre43 three twelhu twelve two x xcube xhigh1 xlow1 xmax xneg1 xneg2 zero zeta ; if isempty(firstCall),firstCall=1;end; 

if isempty(airy_hiresult), airy_hiresult=0; end;
if isempty(four), four = real( 4.0d+00); end;
if isempty(nterm1), nterm1 = 29; end;
if isempty(nterm2), nterm2 = 17; end;
if isempty(nterm3), nterm3 = 22; end;
if isempty(nterm4), nterm4=0; end;
if isempty(nterm5), nterm5=0; end;
if isempty(one), one = real( 1.0); end;
if isempty(three), three = 3.0d+00; end;
if isempty(two), two = 2.0d+00; end;
if isempty(x), x=0; end;
if isempty(zero), zero = 0.0d+00; end;
if isempty(arhip), arhip=zeros(1,31+1); end;
if isempty(arbip), arbip=zeros(1,23+1); end;
if isempty(argip1), argip1=zeros(1,29+1); end;
if isempty(arhin1), arhin1=zeros(1,21+1); end;
if isempty(arhin2), arhin2=zeros(1,15+1); end;
if isempty(bi), bi=0; end;
if isempty(five14), five14=0; end;
if isempty(gi), gi=0; end;
if isempty(hizero), hizero=0; end;
if isempty(lnrtpi), lnrtpi=0; end;
if isempty(minate), minate=0; end;
if isempty(onebpi), onebpi=0; end;
if isempty(one76), one76=0; end;
if isempty(seven), seven=0; end;
if isempty(t), t=0; end;
if isempty(temp), temp=0; end;
if isempty(thre43), thre43=0; end;
if isempty(twelhu), twelhu=0; end;
if isempty(twelve), twelve=0; end;
if isempty(xcube), xcube=0; end;
if isempty(xhigh1), xhigh1=0; end;
if isempty(xlow1), xlow1=0; end;
if isempty(xmax), xmax=0; end;
if isempty(xneg1), xneg1=0; end;
if isempty(xneg2), xneg2=0; end;
if isempty(zeta), zeta=0; end;
if firstCall,   arhip(0+1)=[1.24013562561762831114d0];  end;
if firstCall,   arhip(1+1)=[0.64856341973926535804d0];  end;
if firstCall,   arhip(2+1)=[0.55236252592114903246d0];  end;
if firstCall,   arhip(3+1)=[0.20975122073857566794d0];  end;
if firstCall,   arhip(4+1)=[0.12025669118052373568d0];  end;
if firstCall,   arhip(5+1)=[0.3768224931095393785d-1];  end;
if firstCall,   arhip(6+1)=[0.1651088671548071651d-1];  end;
if firstCall,   arhip(7+1)=[0.455922755211570993d-2];  end;
if firstCall,   arhip(8+1)=[0.161828480477635013d-2];  end;
if firstCall,   arhip(9+1)=[0.40841282508126663d-3];  end;
if firstCall,   arhip(10+1)=[0.12196479721394051d-3];  end;
if firstCall,   arhip(11+1)=[0.2865064098657610d-4];  end;
if firstCall,   arhip(12+1)=[0.742221556424344d-5];  end;
if firstCall,   arhip(13+1)=[0.163536231932831d-5];  end;
if firstCall,   arhip(14+1)=[0.37713908188749d-6];  end;
if firstCall,   arhip(15+1)=[0.7815800336008d-7];  end;
if firstCall,   arhip(16+1)=[0.1638447121370d-7];  end;
if firstCall,   arhip(17+1)=[0.319857665992d-8];  end;
if firstCall,   arhip(18+1)=[0.61933905307d-9];  end;
if firstCall,   arhip(19+1)=[0.11411161191d-9];  end;
if firstCall,   arhip(20+1)=[0.2064923454d-10];  end;
if firstCall,   arhip(21+1)=[0.360018664d-11];  end;
if firstCall,   arhip(22+1)=[0.61401849d-12];  end;
if firstCall,   arhip(23+1)=[0.10162125d-12];  end;
if firstCall,   arhip(24+1)=[0.1643701d-13];  end;
if firstCall,   arhip(25+1)=[0.259084d-14];  end;
if firstCall,   arhip(26+1)=[0.39931d-15];  end;
if firstCall,   arhip(27+1)=[0.6014d-16];  end;
if firstCall,   arhip(28+1)=[0.886d-17];  end;
if firstCall,   arhip(29+1)=[0.128d-17];  end;
if firstCall,   arhip(30+1)=[0.18d-18];  end;
if firstCall,   arhip(31+1)=[0.3d-19];  end;
if firstCall,   arbip(0+1)=[2.00582138209759064905d0];  end;
if firstCall,   arbip(1+1)=[0.294478449170441549d-2];  end;
if firstCall,   arbip(2+1)=[0.3489754514775355d-4];  end;
if firstCall,   arbip(3+1)=[0.83389733374343d-6];  end;
if firstCall,   arbip(4+1)=[0.3136215471813d-7];  end;
if firstCall,   arbip(5+1)=[0.167865306015d-8];  end;
if firstCall,   arbip(6+1)=[0.12217934059d-9];  end;
if firstCall,   arbip(7+1)=[0.1191584139d-10];  end;
if firstCall,   arbip(8+1)=[0.154142553d-11];  end;
if firstCall,   arbip(9+1)=[0.24844455d-12];  end;
if firstCall,   arbip(10+1)=[0.4213012d-13];  end;
if firstCall,   arbip(11+1)=[0.505293d-14];  end;
if firstCall,   arbip(12+1)=[-0.60032d-15];  end;
if firstCall,   arbip(13+1)=[-0.65474d-15];  end;
if firstCall,   arbip(14+1)=[-0.22364d-15];  end;
if firstCall,   arbip(15+1)=[-0.3015d-16];  end;
if firstCall,   arbip(16+1)=[0.959d-17];  end;
if firstCall,   arbip(17+1)=[0.616d-17];  end;
if firstCall,   arbip(18+1)=[0.97d-18];  end;
if firstCall,   arbip(19+1)=[-0.37d-18];  end;
if firstCall,   arbip(20+1)=[-0.21d-18];  end;
if firstCall,   arbip(21+1)=[-0.1d-19];  end;
if firstCall,   arbip(22+1)=[0.2d-19];  end;
if firstCall,   arbip(23+1)=[0.1d-19];  end;
if firstCall,   argip1(0+1)=[2.00473712275801486391d0];  end;
if firstCall,   argip1(1+1)=[0.294184139364406724d-2];  end;
if firstCall,   argip1(2+1)=[0.71369249006340167d-3];  end;
if firstCall,   argip1(3+1)=[0.17526563430502267d-3];  end;
if firstCall,   argip1(4+1)=[0.4359182094029882d-4];  end;
if firstCall,   argip1(5+1)=[0.1092626947604307d-4];  end;
if firstCall,   argip1(6+1)=[0.272382418399029d-5];  end;
if firstCall,   argip1(7+1)=[0.66230900947687d-6];  end;
if firstCall,   argip1(8+1)=[0.15425323370315d-6];  end;
if firstCall,   argip1(9+1)=[0.3418465242306d-7];  end;
if firstCall,   argip1(10+1)=[0.728157724894d-8];  end;
if firstCall,   argip1(11+1)=[0.151588525452d-8];  end;
if firstCall,   argip1(12+1)=[0.30940048039d-9];  end;
if firstCall,   argip1(13+1)=[0.6149672614d-10];  end;
if firstCall,   argip1(14+1)=[0.1202877045d-10];  end;
if firstCall,   argip1(15+1)=[0.233690586d-11];  end;
if firstCall,   argip1(16+1)=[0.43778068d-12];  end;
if firstCall,   argip1(17+1)=[0.7996447d-13];  end;
if firstCall,   argip1(18+1)=[0.1494075d-13];  end;
if firstCall,   argip1(19+1)=[0.246790d-14];  end;
if firstCall,   argip1(20+1)=[0.37672d-15];  end;
if firstCall,   argip1(21+1)=[0.7701d-16];  end;
if firstCall,   argip1(22+1)=[0.354d-17];  end;
if firstCall,   argip1(23+1)=[-0.49d-18];  end;
if firstCall,   argip1(24+1)=[0.62d-18];  end;
if firstCall,   argip1(25+1)=[-0.40d-18];  end;
if firstCall,   argip1(26+1)=[-0.1d-19];  end;
if firstCall,   argip1(27+1)=[0.2d-19];  end;
if firstCall,   argip1(28+1)=[-0.3d-19];  end;
if firstCall,   argip1(29+1)=[0.1d-19];  end;
if firstCall,   arhin1(0+1)=[0.31481017206423404116d0];  end;
if firstCall,   arhin1(1+1)=[-0.16414499216588964341d0];  end;
if firstCall,   arhin1(2+1)=[0.6176651597730913071d-1];  end;
if firstCall,   arhin1(3+1)=[-0.1971881185935933028d-1];  end;
if firstCall,   arhin1(4+1)=[0.536902830023331343d-2];  end;
if firstCall,   arhin1(5+1)=[-0.124977068439663038d-2];  end;
if firstCall,   arhin1(6+1)=[0.24835515596994933d-3];  end;
if firstCall,   arhin1(7+1)=[-0.4187024096746630d-4];  end;
if firstCall,   arhin1(8+1)=[0.590945437979124d-5];  end;
if firstCall,   arhin1(9+1)=[-0.68063541184345d-6];  end;
if firstCall,   arhin1(10+1)=[0.6072897629164d-7];  end;
if firstCall,   arhin1(11+1)=[-0.367130349242d-8];  end;
if firstCall,   arhin1(12+1)=[0.7078017552d-10];  end;
if firstCall,   arhin1(13+1)=[0.1187894334d-10];  end;
if firstCall,   arhin1(14+1)=[-0.120898723d-11];  end;
if firstCall,   arhin1(15+1)=[0.1189656d-13];  end;
if firstCall,   arhin1(16+1)=[0.594128d-14];  end;
if firstCall,   arhin1(17+1)=[-0.32257d-15];  end;
if firstCall,   arhin1(18+1)=[-0.2290d-16];  end;
if firstCall,   arhin1(19+1)=[0.253d-17];  end;
if firstCall,   arhin1(20+1)=[0.9d-19];  end;
if firstCall,   arhin1(21+1)=[-0.2d-19];  end;
if firstCall,   arhin2=[1.99647720399779650525d0,-0.187563779407173213d-2,-0.12186470897787339d-3,-0.814021609659287d-5,-0.55050925953537d-6,-0.3763008043303d-7,-0.258858362365d-8,-0.17931829265d-9,-0.1245916873d-10,-0.87171247d-12,-0.6084943d-13,-0.431178d-14,-0.29787d-15,-0.2210d-16,-0.136d-17,-0.14d-18];  end;
if firstCall,   seven=[7.0d0];  end;
if firstCall,   minate=[-8.0d0];  end;
if firstCall, twelve=[12.0d0];  end;
if firstCall, one76=[176.0d0];  end;
if firstCall,   thre43=[343.0d0];  end;
if firstCall, five14=[514.0d0];  end;
if firstCall, twelhu=[1200.0d0];  end;
if firstCall,   hizero=[0.40995108496400049010d0];  end;
if firstCall,   lnrtpi=[0.57236494292470008707d0];  end;
if firstCall,   onebpi=[0.31830988618379067154d0];  end;
%
%   Machine-dependent constants (suitable for IEEE machines)
%
if firstCall,   nterm4=[19];  end;
if firstCall, nterm5=[14];  end;
if firstCall,   xlow1=[2.220446d-16];  end;
if firstCall, xhigh1=[104.4175d0];  end;
if firstCall,   xneg1=[-0.14274d308];  end;
if firstCall, xneg2=[-208063.831d0];  end;
if firstCall, xmax=[1.79d308];  end;
firstCall=0;
x = xvalue;
if( xhigh1 < x )
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'AIRY_HI - Fatal error!');
writef(1,['%s','\n'], '  Argument too large.');
airy_hiresult = xmax;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',xvalue); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end;
%
%  Code for x < 0.0
%
if( x < zero )
if( x < minate )
if( x < xneg1 )
airy_hiresult = zero;
else;
if( x < xneg2 )
temp = one;
airy_hiresult = - temp .* onebpi ./ x;
else;
xcube = x .* x .* x;
t =( xcube + twelhu ) ./( one76 - xcube );
[temp , nterm5, arhin2, t ]=cheval( nterm5, arhin2, t );
airy_hiresult = - temp .* onebpi ./ x;
end;
end;
else;
if( -xlow1 < x )
airy_hiresult = hizero;
else;
t =( four .* x + twelve ) ./( x - twelve );
[airy_hiresult , nterm4, arhin1, t ]=cheval( nterm4, arhin1, t );
end;
end;
%
%   Code for x >= 0.0
%
else;
if( x <= seven )
if( x < xlow1 )
airy_hiresult = hizero;
else;
t =( x + x ) ./ seven - one;
temp =( x + x + x ) ./ two;
airy_hiresult = exp( temp ) .* cheval( nterm1, arhip, t );
end;
else;
xcube = x .* x .* x;
temp = sqrt( xcube );
zeta =( temp + temp ) ./ three;
t = two .*( sqrt( thre43 ./ xcube ) ) - one;
[temp , nterm2, arbip, t ]=cheval( nterm2, arbip, t );
temp = zeta + log( temp ) - log( x ) ./ four - lnrtpi;
bi = exp( temp );
t =( twelhu - xcube ) ./( xcube + five14 );
gi = cheval( nterm3, argip1, t ) .* onebpi ./ x;
airy_hiresult = bi - gi;
end;
end;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',xvalue); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [n_data, x, fx]=airy_hi_values( n_data, x, fx );
%*******************************************************************************
%
%! AIRY_HI_VALUES returns some values of the Airy Hi function.
%
%  Discussion:
%
%    The function is defined by:
%
%      AIRY_HI(x) = Integral ( 0 <= t < infinity ) exp ( x * t - t^3 / 3 ) dt / pi
%
%    The data was reported by McLeod.
%
%  Modified:
%
%    24 August 2004
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Milton Abramowitz and Irene Stegun,
%    Handbook of Mathematical Functions,
%    US Department of Commerce, 1964.
%
%    Allan McLeod,
%    Algorithm 757, MISCFUN: A software package to compute uncommon
%      special functions,
%    ACM Transactions on Mathematical Software,
%    Volume 22, Number 3, September 1996, pages 288-301.
%
%  Parameters:
%
%    Input/output, integer N_DATA.  The user sets N_DATA to 0 before the
%    first call.  On each call, the routine increments N_DATA by 1, and
%    returns the corresponding data; when there is no more data, the
%    output value of N_DATA will be 0 again.
%
%    Output, real ( kind = 8 ) X, the argument of the function.
%
%    Output, real ( kind = 8 ) FX, the value of the function.
%
persistent fx_vec n_max x_vec ; 

if isempty(n_max), n_max = 20; end;
if isempty(fx_vec), fx_vec([1:n_max]) =[real( 0.40936798278458884024d+00),real( 0.37495291608048868619d+00),real( 0.22066960679295989454d+00),real( 0.77565356679703713590d-01),real( 0.39638826473124717315d-01),real( 0.38450072575004151871d-01),real( 0.35273216868317898556d-01),real( 0.31768535282502272742d-01),real( 0.28894408288051391369d-01),real( 0.24463284011678541180d-01),real( 0.41053540139998941517d+00),real( 0.44993502381204990817d+00),real( 0.97220515514243332184d+00),real( 0.83764237105104371193d+02),real( 0.80327744952044756016d+05),real( 0.15514138847749108298d+06),real( 0.11995859641733262114d+07),real( 0.21472868855967642259d+08),real( 0.45564115351632913590d+09),real( 0.32980722582904761929d+12) ]; end;
if isempty(x_vec), x_vec([1:n_max]) =[real(  -0.0019531250d+00),real(  -0.1250000000d+00),real(  -1.0000000000d+00),real(  -4.0000000000d+00),real(  -8.0000000000d+00),real(  -8.2500000000d+00),real(  -9.0000000000d+00),real( -10.0000000000d+00),real( -11.0000000000d+00),real( -13.0000000000d+00),real(   0.0019531250d+00),real(   0.1250000000d+00),real(   1.0000000000d+00),real(   4.0000000000d+00),real(   7.0000000000d+00),real(   7.2500000000d+00),real(   8.0000000000d+00),real(   9.0000000000d+00),real(  10.0000000000d+00),real(  12.0000000000d+00) ]; end;
if( n_data < 0 )
n_data = 0;
end;
n_data = fix(n_data + 1);
if( n_max < n_data )
n_data = 0;
x = 0.0d+00;
fx = 0.0d+00;
else;
x  = x_vec(n_data);
fx = fx_vec(n_data);
end;
return;
end
function [arctan_intresult, xvalue ]=arctan_int( xvalue );
%*******************************************************************************
%
%! ARCTAN_INT calculates the inverse tangent integral.
%
%  Discussion:
%
%    The function is defined by:
%
%      ARCTAN_INT(x) = Integral ( 0 <= t <= x ) arctan ( t ) / t dt
%
%    The approximation uses Chebyshev series with the coefficients
%    given to an accuracy of 20D.
%
%    This subroutine is set up to work on IEEE machines.
%
%  Modified:
%
%    24 August 2004
%
%  Author:
%
%    Allan McLeod,
%    Department of Mathematics and Statistics,
%    Paisley University, High Street, Paisley, Scotland, PA12BE
%    macl_ms0@paisley.ac.uk
%
%  Reference:
%
%    Allan Mcleod,
%    Algorithm 757, MISCFUN: A software package to compute uncommon
%      special functions,
%    ACM Transactions on Mathematical Software,
%    Volume 22, Number 3, September 1996, pages 288-301.
%
%  Parameters:
%
%    Input, real ( kind = 8 ) XVALUE, the argument of the function.
%
%    Output, real ( kind = 8 ) ARCTAN_INT, the value of the function.
%
arctan_intresult=[];
persistent arctan_int atnina half ind nterms one t twobpi x xlow xupper zero ; 

if isempty(atnina), atnina([0:22]+1) =[1.91040361296235937512d0,-0.4176351437656746940d-1,0.275392550786367434d-2,-0.25051809526248881d-3,0.2666981285121171d-4,-0.311890514107001d-5,0.38833853132249d-6,-0.5057274584964d-7,0.681225282949d-8,-0.94212561654d-9,0.13307878816d-9,-0.1912678075d-10,0.278912620d-11,-0.41174820d-12,0.6142987d-13,-0.924929d-14,0.140387d-14,-0.21460d-15,0.3301d-16,-0.511d-17,0.79d-18,-0.12d-18,0.2d-19 ]; end;
if isempty(arctan_intresult), arctan_intresult=0; end;
if isempty(half), half = 0.5d+00; end;
if isempty(ind), ind=0; end;
if isempty(nterms), nterms = 19; end;
if isempty(one), one = real( 1.0); end;
if isempty(t), t=0; end;
if isempty(twobpi), twobpi = real( 0.63661977236758134308d0); end;
if isempty(x), x=0; end;
if isempty(xlow), xlow = real( 7.4505806d-9); end;
if isempty(xupper), xupper = real( 4.5036d15); end;
if isempty(zero), zero = 0.0d+00; end;
ind = 1;
x = xvalue;
if( x < zero )
x = -x;
ind = -1;
end;
if( x < xlow )
arctan_intresult = x;
elseif( x <= one ) ;
t = x .* x;
t =( t - half ) +( t - half );
arctan_intresult = x .* cheval( nterms, atnina, t );
elseif( x <= xupper ) ;
t = one ./( x .* x );
t =( t - half ) +( t - half );
arctan_intresult = log( x ) ./ twobpi + cheval( nterms, atnina, t ) ./ x;
else;
arctan_intresult = log( x ) ./ twobpi;
end;
if( ind < 0 )
arctan_intresult = -arctan_intresult;
end;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',xvalue); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [n_data, x, fx]=arctan_int_values( n_data, x, fx );
%*******************************************************************************
%
%! ARCTAN_INT_VALUES returns some values of the inverse tangent integral.
%
%  Discussion:
%
%    The function is defined by:
%
%      ARCTAN_INT(x) = Integral ( 0 <= t <= x ) arctan ( t ) / t dt
%
%    The data was reported by McLeod.
%
%  Modified:
%
%    25 August 2004
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Allan McLeod,
%    Algorithm 757, MISCFUN: A software package to compute uncommon
%      special functions,
%    ACM Transactions on Mathematical Software,
%    Volume 22, Number 3, September 1996, pages 288-301.
%
%  Parameters:
%
%    Input/output, integer N_DATA.  The user sets N_DATA to 0 before the
%    first call.  On each call, the routine increments N_DATA by 1, and
%    returns the corresponding data; when there is no more data, the
%    output value of N_DATA will be 0 again.
%
%    Output, real ( kind = 8 ) X, the argument of the function.
%
%    Output, real ( kind = 8 ) FX, the value of the function.
%
persistent fx_vec n_max x_vec ; 

if isempty(n_max), n_max = 20; end;
if isempty(fx_vec), fx_vec([1:n_max]) =[real(  0.19531241721588483191d-02),real( -0.39062433772980711281d-02),real(  0.78124470192576499535d-02),real(  0.15624576181996527280d-01),real( -0.31246610349485401551d-01),real(  0.62472911335014397321d-01),real(  0.12478419717389654039d+00),real( -0.24830175098230686908d+00),real(  0.48722235829452235711d+00),real(  0.91596559417721901505d+00),real(  0.12749694484943800618d+01),real( -0.15760154034463234224d+01),real(  0.24258878412859089996d+01),real(  0.33911633326292997361d+01),real(  0.44176450919422186583d+01),real( -0.47556713749547247774d+01),real(  0.50961912150934111303d+01),real(  0.53759175735714876256d+01),real( -0.61649904785027487422d+01),real(  0.72437843013083534973d+01) ]; end;
if isempty(x_vec), x_vec([1:n_max]) =[real(   0.0019531250d+00),real(  -0.0039062500d+00),real(   0.0078125000d+00),real(   0.0156250000d+00),real(  -0.0312500000d+00),real(   0.0625000000d+00),real(   0.1250000000d+00),real(  -0.2500000000d+00),real(   0.5000000000d+00),real(   1.0000000000d+00),real(   1.5000000000d+00),real(  -2.0000000000d+00),real(   4.0000000000d+00),real(   8.0000000000d+00),real(  16.0000000000d+00),real( -20.0000000000d+00),real(  25.0000000000d+00),real(  30.0000000000d+00),real( -50.0000000000d+00),real( 100.0000000000d+00) ]; end;
if( n_data < 0 )
n_data = 0;
end;
n_data = fix(n_data + 1);
if( n_max < n_data )
n_data = 0;
x = 0.0d+00;
fx = 0.0d+00;
else;
x  = x_vec(n_data);
fx = fx_vec(n_data);
end;
return;
end
function [bessel_i0_intresult, xvalue ]=bessel_i0_int( xvalue );
%*******************************************************************************
%
%! BESSEL_I0_INT computes the integral of the modified Bessel function I0(X).
%
%  Discussion:
%
%    The function is defined by:
%
%      I0_INT(x) = Integral ( 0 <= t <= x ) I0(t) dt
%
%    The program uses Chebyshev expansions, the coefficients of
%    which are given to 20 decimal places.
%
%    This subroutine is set up to work on IEEE machines.
%
%  Modified:
%
%    29 August 2004
%
%  Author:
%
%    Allan McLeod,
%    Department of Mathematics and Statistics,
%    Paisley University, High Street, Paisley, Scotland, PA12BE
%    macl_ms0@paisley.ac.uk
%
%  Reference:
%
%    Allan Mcleod,
%    Algorithm 757, MISCFUN: A software package to compute uncommon
%      special functions,
%    ACM Transactions on Mathematical Software,
%    Volume 22, Number 3, September 1996, pages 288-301.
%
%  Parameters:
%
%    Input, real ( kind = 8 ) XVALUE, the argument of the function.
%
%    Output, real ( kind = 8 ) BESSEL_I0_INT, the value of the function.
%
bessel_i0_intresult=[];
persistent ari01 ari0a ateen bessel_i0_int firstCall half ind lnr2pi nterm1 nterm2 t temp thirt6 three x xhigh xlow zero ; if isempty(firstCall),firstCall=1;end; 

if isempty(ari01), ari01=zeros(1,28+1); end;
if isempty(ari0a), ari0a=zeros(1,33+1); end;
if isempty(ateen), ateen = real( 18.0d+00); end;
if isempty(bessel_i0_intresult), bessel_i0_intresult=0; end;
if isempty(half), half = 0.5d+00; end;
if isempty(ind), ind=0; end;
if isempty(lnr2pi), lnr2pi =real( 0.91893853320467274178d0); end;
if isempty(nterm1), nterm1 = 25; end;
if isempty(nterm2), nterm2 = 27; end;
if isempty(t), t=0; end;
if isempty(temp), temp=0; end;
if isempty(thirt6), thirt6 = real( 36.0d+00); end;
if isempty(three), three = 3.0d+00; end;
if isempty(x), x=0; end;
if isempty(xhigh), xhigh = real( 713.758339d0); end;
if isempty(xlow), xlow = real( 0.5161914d-7); end;
if isempty(zero), zero = 0.0d+00; end;
if firstCall,   ari01(0+1)=[0.41227906926781516801d0];  end;
if firstCall,   ari01(1+1)=[-0.34336345150081519562d0];  end;
if firstCall,   ari01(2+1)=[0.22667588715751242585d0];  end;
if firstCall,   ari01(3+1)=[-0.12608164718742260032d0];  end;
if firstCall,   ari01(4+1)=[0.6012484628777990271d-1];  end;
if firstCall,   ari01(5+1)=[-0.2480120462913358248d-1];  end;
if firstCall,   ari01(6+1)=[0.892773389565563897d-2];  end;
if firstCall,   ari01(7+1)=[-0.283253729936696605d-2];  end;
if firstCall,   ari01(8+1)=[0.79891339041712994d-3];  end;
if firstCall,   ari01(9+1)=[-0.20053933660964890d-3];  end;
if firstCall,   ari01(10+1)=[0.4416816783014313d-4];  end;
if firstCall,   ari01(11+1)=[-0.822377042246068d-5];  end;
if firstCall,   ari01(12+1)=[0.120059794219015d-5];  end;
if firstCall,   ari01(13+1)=[-0.11350865004889d-6];  end;
if firstCall,   ari01(14+1)=[0.69606014466d-9];  end;
if firstCall,   ari01(15+1)=[0.180622772836d-8];  end;
if firstCall,   ari01(16+1)=[-0.26039481370d-9];  end;
if firstCall,   ari01(17+1)=[-0.166188103d-11];  end;
if firstCall,   ari01(18+1)=[0.510500232d-11];  end;
if firstCall,   ari01(19+1)=[-0.41515879d-12];  end;
if firstCall,   ari01(20+1)=[-0.7368138d-13];  end;
if firstCall,   ari01(21+1)=[0.1279323d-13];  end;
if firstCall,   ari01(22+1)=[0.103247d-14];  end;
if firstCall,   ari01(23+1)=[-0.30379d-15];  end;
if firstCall,   ari01(24+1)=[-0.1789d-16];  end;
if firstCall,   ari01(25+1)=[0.673d-17];  end;
if firstCall,   ari01(26+1)=[0.44d-18];  end;
if firstCall,   ari01(27+1)=[-0.14d-18];  end;
if firstCall,   ari01(28+1)=[-0.1d-19];  end;
if firstCall,   ari0a(0+1)=[2.03739654571143287070d0];  end;
if firstCall,   ari0a(1+1)=[0.1917631647503310248d-1];  end;
if firstCall,   ari0a(2+1)=[0.49923334519288147d-3];  end;
if firstCall,   ari0a(3+1)=[0.2263187103659815d-4];  end;
if firstCall,   ari0a(4+1)=[0.158682108285561d-5];  end;
if firstCall,   ari0a(5+1)=[0.16507855636318d-6];  end;
if firstCall,   ari0a(6+1)=[0.2385058373640d-7];  end;
if firstCall,   ari0a(7+1)=[0.392985182304d-8];  end;
if firstCall,   ari0a(8+1)=[0.46042714199d-9];  end;
if firstCall,   ari0a(9+1)=[-0.7072558172d-10];  end;
if firstCall,   ari0a(10+1)=[-0.6747183961d-10];  end;
if firstCall,   ari0a(11+1)=[-0.2026962001d-10];  end;
if firstCall,   ari0a(12+1)=[-0.87320338d-12];  end;
if firstCall,   ari0a(13+1)=[0.175520014d-11];  end;
if firstCall,   ari0a(14+1)=[0.60383944d-12];  end;
if firstCall,   ari0a(15+1)=[-0.3977983d-13];  end;
if firstCall,   ari0a(16+1)=[-0.8049048d-13];  end;
if firstCall,   ari0a(17+1)=[-0.1158955d-13];  end;
if firstCall,   ari0a(18+1)=[0.827318d-14];  end;
if firstCall,   ari0a(19+1)=[0.282290d-14];  end;
if firstCall,   ari0a(20+1)=[-0.77667d-15];  end;
if firstCall,   ari0a(21+1)=[-0.48731d-15];  end;
if firstCall,   ari0a(22+1)=[0.7279d-16];  end;
if firstCall,   ari0a(23+1)=[0.7873d-16];  end;
if firstCall,   ari0a(24+1)=[-0.785d-17];  end;
if firstCall,   ari0a(25+1)=[-0.1281d-16];  end;
if firstCall,   ari0a(26+1)=[0.121d-17];  end;
if firstCall,   ari0a(27+1)=[0.214d-17];  end;
if firstCall,   ari0a(28+1)=[-0.27d-18];  end;
if firstCall,   ari0a(29+1)=[-0.36d-18];  end;
if firstCall,   ari0a(30+1)=[0.7d-19];  end;
if firstCall,   ari0a(31+1)=[0.6d-19];  end;
if firstCall,   ari0a(32+1)=[-0.2d-19];  end;
if firstCall,   ari0a(33+1)=[-0.1d-19];  end;
firstCall=0;
ind = 1;
x = xvalue;
if( xvalue < zero )
ind = -1;
x = -x;
end;
if( x < xlow )
bessel_i0_intresult = x;
elseif( x <= ateen ) ;
t =( three .* x - ateen ) ./( x + ateen );
bessel_i0_intresult = x .* exp( x ) .* cheval( nterm1, ari01, t );
elseif( x <= xhigh ) ;
t =( thirt6 ./ x - half ) - half;
temp = x - half .* log( x ) - lnr2pi + log( cheval( nterm2, ari0a, t ));
bessel_i0_intresult = exp( temp );
else;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'BESSEL_I0_INT - Fatal error!');
writef(1,['%s','\n'], '  Argument magnitude too large.');
bessel_i0_intresult = exp( xhigh - lnr2pi - half .* log( xhigh ) );
end;
if( ind == -1 )
bessel_i0_intresult = -bessel_i0_intresult;
end;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',xvalue); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [n_data, x, fx]=bessel_i0_int_values( n_data, x, fx );
%*******************************************************************************
%
%! BESSEL_I0_INT_VALUES returns some values of the Bessel I0 integral.
%
%  Discussion:
%
%    The function is defined by:
%
%      I0_INT(x) = Integral ( 0 <= t <= x ) I0(t) dt
%
%    The data was reported by McLeod.
%
%  Modified:
%
%    29 August 2004
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Allan McLeod,
%    Algorithm 757, MISCFUN: A software package to compute uncommon
%      special functions,
%    ACM Transactions on Mathematical Software,
%    Volume 22, Number 3, September 1996, pages 288-301.
%
%  Parameters:
%
%    Input/output, integer N_DATA.  The user sets N_DATA to 0 before the
%    first call.  On each call, the routine increments N_DATA by 1, and
%    returns the corresponding data; when there is no more data, the
%    output value of N_DATA will be 0 again.
%
%    Output, real ( kind = 8 ) X, the argument of the function.
%
%    Output, real ( kind = 8 ) FX, the value of the function.
%
persistent fx_vec n_max x_vec ; 

if isempty(n_max), n_max = 20; end;
if isempty(fx_vec), fx_vec([1:n_max]) =[real(  0.19531256208818052282d-02),real( -0.39062549670565734544d-02),real(  0.62520348032546565850d-01),real(  0.12516285581366971819d+00),real( -0.51051480879740303760d+00),real(  0.10865210970235898158d+01),real(  0.27750019054282535299d+01),real( -0.13775208868039716639d+02),real(  0.46424372058106108576d+03),real(  0.64111867658021584522d+07),real( -0.10414860803175857953d+08),real(  0.44758598913855743089d+08),real( -0.11852985311558287888d+09),real(  0.31430078220715992752d+09),real( -0.83440212900794309620d+09),real(  0.22175367579074298261d+10),real(  0.58991731842803636487d+10),real( -0.41857073244691522147d+11),real(  0.79553885818472357663d+12),real(  0.15089715082719201025d+17) ]; end;
if isempty(x_vec), x_vec([1:n_max]) =[real(   0.0019531250d+00),real(  -0.0039062500d+00),real(   0.0625000000d+00),real(   0.1250000000d+00),real(  -0.5000000000d+00),real(   1.0000000000d+00),real(   2.0000000000d+00),real(  -4.0000000000d+00),real(   8.0000000000d+00),real(  18.0000000000d+00),real( -18.5000000000d+00),real(  20.0000000000d+00),real( -21.0000000000d+00),real(  22.0000000000d+00),real( -23.0000000000d+00),real(  24.0000000000d+00),real(  25.0000000000d+00),real( -27.0000000000d+00),real(  30.0000000000d+00),real(  40.0000000000d+00) ]; end;
if( n_data < 0 )
n_data = 0;
end;
n_data = fix(n_data + 1);
if( n_max < n_data )
n_data = 0;
x = 0.0d+00;
fx = 0.0d+00;
else;
x  = x_vec(n_data);
fx = fx_vec(n_data);
end;
return;
end
function [bessel_j0_intresult, xvalue ]=bessel_j0_int( xvalue );
%*******************************************************************************
%
%! BESSEL_J0_INT calculates the integral of the Bessel function J0.
%
%  Discussion:
%
%    The function is defined by:
%
%      J0_INT(x) = Integral ( 0 <= t <= x ) J0(t) dt
%
%    The code uses Chebyshev expansions whose coefficients are
%    given to 20 decimal places.
%
%    This subroutine is set up to work on IEEE machines.
%
%  Modified:
%
%    07 August 2004
%
%  Author:
%
%    Allan McLeod,
%    Department of Mathematics and Statistics,
%    Paisley University, High Street, Paisley, Scotland, PA12BE
%    macl_ms0@paisley.ac.uk
%
%  Reference:
%
%    Allan Mcleod,
%    Algorithm 757, MISCFUN: A software package to compute uncommon
%      special functions,
%    ACM Transactions on Mathematical Software,
%    Volume 22, Number 3, September 1996, pages 288-301.
%
%  Parameters:
%
%    Input, real ( kind = 8 ) XVALUE, the argument of the function.
%
%    Output, real ( kind = 8 ) BESSEL_J0_INT, the value of the function.
%
bessel_j0_intresult=[];
persistent arj01 arj0a1 arj0a2 bessel_j0_int firstCall five12 ind nterm1 nterm2 nterm3 one one28 pib41 pib411 pib412 pib42 rt2bpi sixten t temp x xhigh xlow xmpi4 zero ; if isempty(firstCall),firstCall=1;end; 

if isempty(bessel_j0_intresult), bessel_j0_intresult=0; end;
if isempty(ind), ind=0; end;
if isempty(nterm1), nterm1 = 22; end;
if isempty(nterm2), nterm2 = 18; end;
if isempty(nterm3), nterm3 = 16; end;
if isempty(one), one = 1.0d+00; end;
if isempty(x), x=0; end;
if isempty(zero), zero = 0.0d+00; end;
if isempty(arj01), arj01=zeros(1,23+1); end;
if isempty(arj0a1), arj0a1=zeros(1,21+1); end;
if isempty(arj0a2), arj0a2=zeros(1,18+1); end;
if isempty(five12), five12=0; end;
if isempty(one28), one28=0; end;
if isempty(pib41), pib41=0; end;
if isempty(pib411), pib411=0; end;
if isempty(pib412), pib412=0; end;
if isempty(pib42), pib42=0; end;
if isempty(rt2bpi), rt2bpi=0; end;
if isempty(sixten), sixten=0; end;
if isempty(t), t=0; end;
if isempty(temp), temp=0; end;
if isempty(xhigh), xhigh=0; end;
if isempty(xlow), xlow=0; end;
if isempty(xmpi4), xmpi4=0; end;
if firstCall,   sixten=[16.0d0];  end;
if firstCall,   one28=[128.0d0];  end;
if firstCall, five12=[512d0];  end;
if firstCall,   rt2bpi=[0.79788456080286535588d0];  end;
if firstCall,   pib411=[201.0d0];  end;
if firstCall, pib412=[256.0d0];  end;
if firstCall,   pib42=[0.24191339744830961566d-3];  end;
if firstCall,   arj01(0+1)=[0.38179279321690173518d0];  end;
if firstCall,   arj01(1+1)=[-0.21275636350505321870d0];  end;
if firstCall,   arj01(2+1)=[0.16754213407215794187d0];  end;
if firstCall,   arj01(3+1)=[-0.12853209772196398954d0];  end;
if firstCall,   arj01(4+1)=[0.10114405455778847013d0];  end;
if firstCall,   arj01(5+1)=[-0.9100795343201568859d-1];  end;
if firstCall,   arj01(6+1)=[0.6401345264656873103d-1];  end;
if firstCall,   arj01(7+1)=[-0.3066963029926754312d-1];  end;
if firstCall,   arj01(8+1)=[0.1030836525325064201d-1];  end;
if firstCall,   arj01(9+1)=[-0.255670650399956918d-2];  end;
if firstCall,   arj01(10+1)=[0.48832755805798304d-3];  end;
if firstCall,   arj01(11+1)=[-0.7424935126036077d-4];  end;
if firstCall,   arj01(12+1)=[0.922260563730861d-5];  end;
if firstCall,   arj01(13+1)=[-0.95522828307083d-6];  end;
if firstCall,   arj01(14+1)=[0.8388355845986d-7];  end;
if firstCall,   arj01(15+1)=[-0.633184488858d-8];  end;
if firstCall,   arj01(16+1)=[0.41560504221d-9];  end;
if firstCall,   arj01(17+1)=[-0.2395529307d-10];  end;
if firstCall,   arj01(18+1)=[0.122286885d-11];  end;
if firstCall,   arj01(19+1)=[-0.5569711d-13];  end;
if firstCall,   arj01(20+1)=[0.227820d-14];  end;
if firstCall,   arj01(21+1)=[-0.8417d-16];  end;
if firstCall,   arj01(22+1)=[0.282d-17];  end;
if firstCall,   arj01(23+1)=[-0.9d-19];  end;
if firstCall,   arj0a1(0+1)=[1.24030133037518970827d0];  end;
if firstCall,   arj0a1(1+1)=[-0.478125353632280693d-2];  end;
if firstCall,   arj0a1(2+1)=[0.6613148891706678d-4];  end;
if firstCall,   arj0a1(3+1)=[-0.186042740486349d-5];  end;
if firstCall,   arj0a1(4+1)=[0.8362735565080d-7];  end;
if firstCall,   arj0a1(5+1)=[-0.525857036731d-8];  end;
if firstCall,   arj0a1(6+1)=[0.42606363251d-9];  end;
if firstCall,   arj0a1(7+1)=[-0.4211761024d-10];  end;
if firstCall,   arj0a1(8+1)=[0.488946426d-11];  end;
if firstCall,   arj0a1(9+1)=[-0.64834929d-12];  end;
if firstCall,   arj0a1(10+1)=[0.9617234d-13];  end;
if firstCall,   arj0a1(11+1)=[-0.1570367d-13];  end;
if firstCall,   arj0a1(12+1)=[0.278712d-14];  end;
if firstCall,   arj0a1(13+1)=[-0.53222d-15];  end;
if firstCall,   arj0a1(14+1)=[0.10844d-15];  end;
if firstCall,   arj0a1(15+1)=[-0.2342d-16];  end;
if firstCall,   arj0a1(16+1)=[0.533d-17];  end;
if firstCall,   arj0a1(17+1)=[-0.127d-17];  end;
if firstCall,   arj0a1(18+1)=[0.32d-18];  end;
if firstCall,   arj0a1(19+1)=[-0.8d-19];  end;
if firstCall,   arj0a1(20+1)=[0.2d-19];  end;
if firstCall,   arj0a1(21+1)=[-0.1d-19];  end;
if firstCall,   arj0a2(0+1)=[1.99616096301341675339d0];  end;
if firstCall,   arj0a2(1+1)=[-0.190379819246668161d-2];  end;
if firstCall,   arj0a2(2+1)=[0.1539710927044226d-4];  end;
if firstCall,   arj0a2(3+1)=[-0.31145088328103d-6];  end;
if firstCall,   arj0a2(4+1)=[0.1110850971321d-7];  end;
if firstCall,   arj0a2(5+1)=[-0.58666787123d-9];  end;
if firstCall,   arj0a2(6+1)=[0.4139926949d-10];  end;
if firstCall,   arj0a2(7+1)=[-0.365398763d-11];  end;
if firstCall,   arj0a2(8+1)=[0.38557568d-12];  end;
if firstCall,   arj0a2(9+1)=[-0.4709800d-13];  end;
if firstCall,   arj0a2(10+1)=[0.650220d-14];  end;
if firstCall,   arj0a2(11+1)=[-0.99624d-15];  end;
if firstCall,   arj0a2(12+1)=[0.16700d-15];  end;
if firstCall,   arj0a2(13+1)=[-0.3028d-16];  end;
if firstCall,   arj0a2(14+1)=[0.589d-17];  end;
if firstCall,   arj0a2(15+1)=[-0.122d-17];  end;
if firstCall,   arj0a2(16+1)=[0.27d-18];  end;
if firstCall,   arj0a2(17+1)=[-0.6d-19];  end;
if firstCall,   arj0a2(18+1)=[0.1d-19];  end;
%
%   Machine-dependent constants (suitable for IEEE machines)
%
if firstCall,   xlow=[3.650024d-8];  end;
if firstCall, xhigh=[9.0072d15];  end;
firstCall=0;
x = xvalue;
ind = 1;
if( x < zero )
x = -x;
ind = -1;
end;
if( x < xlow )
bessel_j0_intresult = x;
elseif( x <= sixten ) ;
t = x .* x ./ one28 - one;
bessel_j0_intresult = x .* cheval( nterm1, arj01, t );
elseif( x <= xhigh ) ;
t = five12 ./( x .* x ) - one;
pib41 = pib411 ./ pib412;
xmpi4 =( x - pib41 ) - pib42;
temp = cos( xmpi4 ) .* cheval( nterm2, arj0a1, t ) ./ x;
temp = temp - sin( xmpi4) .* cheval( nterm3, arj0a2, t );
bessel_j0_intresult = one - rt2bpi .* temp ./ sqrt( x );
else;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'BESSEL_J0_INT - Fatal error!');
writef(1,['%s','\n'], '  Argument magnitude too large.');
bessel_j0_intresult = one;
end;
if( ind == -1 )
bessel_j0_intresult = -bessel_j0_intresult;
end;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',xvalue); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [n_data, x, fx]=bessel_j0_int_values( n_data, x, fx );
%*******************************************************************************
%
%! BESSEL_J0_INT_VALUES returns some values of the Bessel J0 integral.
%
%  Discussion:
%
%    The function is defined by:
%
%      J0_INT(x) = Integral ( 0 <= t <= x ) J0(t) dt
%
%    The data was reported by McLeod.
%
%  Modified:
%
%    29 August 2004
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Allan McLeod,
%    Algorithm 757, MISCFUN: A software package to compute uncommon
%      special functions,
%    ACM Transactions on Mathematical Software,
%    Volume 22, Number 3, September 1996, pages 288-301.
%
%  Parameters:
%
%    Input/output, integer N_DATA.  The user sets N_DATA to 0 before the
%    first call.  On each call, the routine increments N_DATA by 1, and
%    returns the corresponding data; when there is no more data, the
%    output value of N_DATA will be 0 again.
%
%    Output, real ( kind = 8 ) X, the argument of the function.
%
%    Output, real ( kind = 8 ) FX, the value of the function.
%
persistent fx_vec n_max x_vec ; 

if isempty(n_max), n_max = 20; end;
if isempty(fx_vec), fx_vec([1:n_max]) =[real(  0.97656242238978822427d-03),real(  0.39062450329491108875d-02),real( -0.62479657927917933620d-01),real(  0.12483733492120479139d+00),real( -0.48968050664604505505d+00),real(  0.91973041008976023931d+00),real( -0.14257702931970265690d+01),real(  0.10247341594606064818d+01),real( -0.12107468348304501655d+01),real(  0.11008652032736190799d+01),real( -0.10060334829904124192d+01),real(  0.81330572662485953519d+00),real( -0.10583788214211277585d+01),real(  0.87101492116545875169d+00),real( -0.88424908882547488420d+00),real(  0.11257761503599914603d+01),real( -0.90141212258183461184d+00),real(  0.91441344369647797803d+00),real( -0.94482281938334394886d+00),real(  0.92266255696016607257d+00) ]; end;
if isempty(x_vec), x_vec([1:n_max]) =[real(   0.0009765625d+00),real(   0.0039062500d+00),real(  -0.0625000000d+00),real(   0.1250000000d+00),real(  -0.5000000000d+00),real(   1.0000000000d+00),real(  -2.0000000000d+00),real(   4.0000000000d+00),real(  -8.0000000000d+00),real(  16.0000000000d+00),real( -16.5000000000d+00),real(  18.0000000000d+00),real( -20.0000000000d+00),real(  25.0000000000d+00),real( -30.0000000000d+00),real(  40.0000000000d+00),real( -50.0000000000d+00),real(  75.0000000000d+00),real( -80.0000000000d+00),real( 100.0000000000d+00) ]; end;
if( n_data < 0 )
n_data = 0;
end;
n_data = fix(n_data + 1);
if( n_max < n_data )
n_data = 0;
x = 0.0d+00;
fx = 0.0d+00;
else;
x  = x_vec(n_data);
fx = fx_vec(n_data);
end;
return;
end
function [bessel_k0_intresult, xvalue ]=bessel_k0_int( xvalue );
%*******************************************************************************
%
%! BESSEL_K0_INT calculates the integral of the modified Bessel function K0(X).
%
%  Discussion:
%
%    The function is defined by:
%
%      K0_INT(x) = Integral ( 0 <= t <= x ) K0(t) dt
%
%    The code uses Chebyshev expansions, whose coefficients are
%    given to 20 decimal places.
%
%    This subroutine is set up to work on IEEE machines.
%
%  Modified:
%
%    29 August 2004
%
%  Author:
%
%    Allan McLeod,
%    Department of Mathematics and Statistics,
%    Paisley University, High Street, Paisley, Scotland, PA12BE
%    macl_ms0@paisley.ac.uk
%
%  Reference:
%
%    Allan Mcleod,
%    Algorithm 757, MISCFUN: A software package to compute uncommon
%      special functions,
%    ACM Transactions on Mathematical Software,
%    Volume 22, Number 3, September 1996, pages 288-301.
%
%  Parameters:
%
%    Input, real ( kind = 8 ) XVALUE, the argument of the function.
%
%    Output, real ( kind = 8 ) BESSEL_K0_INT, the value of the function.
%
bessel_k0_intresult=[];
persistent ak0in1 ak0in2 ak0ina bessel_k0_int const1 const2 eightn firstCall fval half nterm1 nterm2 nterm3 piby2 rt2bpi six t temp twelve x xhigh xlow zero ; if isempty(firstCall),firstCall=1;end; 

if isempty(bessel_k0_intresult), bessel_k0_intresult=0; end;
if isempty(half), half = 0.5d+00; end;
if isempty(nterm1), nterm1 = 14; end;
if isempty(nterm2), nterm2 = 14; end;
if isempty(nterm3), nterm3 = 23; end;
if isempty(six), six = real( 6.0d+00); end;
if isempty(x), x=0; end;
if isempty(zero), zero = 0.0d+00; end;
if isempty(ak0in1), ak0in1=zeros(1,15+1); end;
if isempty(ak0in2), ak0in2=zeros(1,15+1); end;
if isempty(ak0ina), ak0ina=zeros(1,27+1); end;
if isempty(const1), const1=0; end;
if isempty(const2), const2=0; end;
if isempty(eightn), eightn=0; end;
if isempty(fval), fval=0; end;
if isempty(piby2), piby2=0; end;
if isempty(rt2bpi), rt2bpi=0; end;
if isempty(t), t=0; end;
if isempty(temp), temp=0; end;
if isempty(twelve), twelve=0; end;
if isempty(xhigh), xhigh=0; end;
if isempty(xlow), xlow=0; end;
if firstCall,   twelve=[12.0d0];  end;
if firstCall, eightn =[18.0d0];  end;
if firstCall,   const1=[1.11593151565841244881d0];  end;
if firstCall,   const2=[-0.11593151565841244881d0];  end;
if firstCall,   piby2=[1.57079632679489661923d0];  end;
if firstCall,   rt2bpi=[0.79788456080286535588d0];  end;
if firstCall,   ak0in1=[16.79702714464710959477d0,9.79134687676889407070d0,2.80501316044337939300d0,0.45615620531888502068d0,0.4716224457074760784d-1,0.335265148269698289d-2,0.17335181193874727d-3,0.679951889364702d-5,0.20900268359924d-6,0.516603846976d-8,0.10485708331d-9,0.177829320d-11,0.2556844d-13,0.31557d-15,0.338d-17,0.3d-19];  end;
if firstCall,   ak0in2=[10.76266558227809174077d0,5.62333479849997511550d0,1.43543664879290867158d0,0.21250410143743896043d0,0.2036537393100009554d-1,0.136023584095623632d-2,0.6675388699209093d-4,0.250430035707337d-5,0.7406423741728d-7,0.176974704314d-8,0.3485775254d-10,0.57544785d-12,0.807481d-14,0.9747d-16,0.102d-17,0.1d-19];  end;
if firstCall,   ak0ina(0+1)=[1.91172065445060453895d0];  end;
if firstCall,   ak0ina(1+1)=[-0.4183064565769581085d-1];  end;
if firstCall,   ak0ina(2+1)=[0.213352508068147486d-2];  end;
if firstCall,   ak0ina(3+1)=[-0.15859497284504181d-3];  end;
if firstCall,   ak0ina(4+1)=[0.1497624699858351d-4];  end;
if firstCall,   ak0ina(5+1)=[-0.167955955322241d-5];  end;
if firstCall,   ak0ina(6+1)=[0.21495472478804d-6];  end;
if firstCall,   ak0ina(7+1)=[-0.3058356654790d-7];  end;
if firstCall,   ak0ina(8+1)=[0.474946413343d-8];  end;
if firstCall,   ak0ina(9+1)=[-0.79424660432d-9];  end;
if firstCall,   ak0ina(10+1)=[0.14156555325d-9];  end;
if firstCall,   ak0ina(11+1)=[-0.2667825359d-10];  end;
if firstCall,   ak0ina(12+1)=[0.528149717d-11];  end;
if firstCall,   ak0ina(13+1)=[-0.109263199d-11];  end;
if firstCall,   ak0ina(14+1)=[0.23518838d-12];  end;
if firstCall,   ak0ina(15+1)=[-0.5247991d-13];  end;
if firstCall,   ak0ina(16+1)=[0.1210191d-13];  end;
if firstCall,   ak0ina(17+1)=[-0.287632d-14];  end;
if firstCall,   ak0ina(18+1)=[0.70297d-15];  end;
if firstCall,   ak0ina(19+1)=[-0.17631d-15];  end;
if firstCall,   ak0ina(20+1)=[0.4530d-16];  end;
if firstCall,   ak0ina(21+1)=[-0.1190d-16];  end;
if firstCall,   ak0ina(22+1)=[0.319d-17];  end;
if firstCall,   ak0ina(23+1)=[-0.87d-18];  end;
if firstCall,   ak0ina(24+1)=[0.24d-18];  end;
if firstCall,   ak0ina(25+1)=[-0.7d-19];  end;
if firstCall,   ak0ina(26+1)=[0.2d-19];  end;
if firstCall,   ak0ina(27+1)=[-0.1d-19];  end;
%
%   Machine-dependent values (suitable for IEEE machines)
%
if firstCall,   xlow=[4.47034836d-8];  end;
if firstCall, xhigh=[36.0436534d0];  end;
firstCall=0;
x = xvalue;
if( x < zero )
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'BESSEL_K0_INT - Fatal error!');
writef(1,['%s','\n'], '  Argument X < 0.');
bessel_k0_intresult = zero;
elseif( x == zero ) ;
bessel_k0_intresult = zero;
elseif( x < xlow ) ;
bessel_k0_intresult = x .*( const1 - log( x ) );
elseif( x <= six ) ;
t =(( x .* x ) ./ eightn - half ) - half;
fval =( const2 + log( x ) ) .* cheval( nterm2, ak0in2, t );
bessel_k0_intresult = x .*( cheval( nterm1, ak0in1, t ) - fval );
elseif( x < xhigh ) ;
fval = piby2;
t =( twelve ./ x - half ) - half;
temp = exp( -x ) .* cheval( nterm3, ak0ina, t );
fval = fval - temp ./( sqrt( x ) .* rt2bpi );
bessel_k0_intresult = fval;
else;
bessel_k0_intresult = piby2;
end;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',xvalue); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [n_data, x, fx]=bessel_k0_int_values( n_data, x, fx );
%*******************************************************************************
%
%! BESSEL_K0_INT_VALUES returns some values of the Bessel K0 integral.
%
%  Discussion:
%
%    The function is defined by:
%
%      K0_INT(x) = Integral ( 0 <= t <= x ) K0(t) dt
%
%    The data was reported by McLeod.
%
%  Modified:
%
%    29 August 2004
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Allan McLeod,
%    Algorithm 757, MISCFUN: A software package to compute uncommon
%      special functions,
%    ACM Transactions on Mathematical Software,
%    Volume 22, Number 3, September 1996, pages 288-301.
%
%  Parameters:
%
%    Input/output, integer N_DATA.  The user sets N_DATA to 0 before the
%    first call.  On each call, the routine increments N_DATA by 1, and
%    returns the corresponding data; when there is no more data, the
%    output value of N_DATA will be 0 again.
%
%    Output, real ( kind = 8 ) X, the argument of the function.
%
%    Output, real ( kind = 8 ) FX, the value of the function.
%
persistent fx_vec n_max x_vec ; 

if isempty(n_max), n_max = 20; end;
if isempty(fx_vec), fx_vec([1:n_max]) =[real( 0.78587929563466784589d-02),real( 0.26019991617330578111d-01),real( 0.24311842237541167904d+00),real( 0.39999633750480508861d+00),real( 0.92710252093114907345d+00),real( 0.12425098486237782662d+01),real( 0.14736757343168286825d+01),real( 0.15606495706051741364d+01),real( 0.15673873907283660493d+01),real( 0.15696345532693743714d+01),real( 0.15701153443250786355d+01),real( 0.15706574852894436220d+01),real( 0.15707793116159788598d+01),real( 0.15707942066465767196d+01),real( 0.15707962315469192247d+01),real( 0.15707963262340149876d+01),real( 0.15707963267948756308d+01),real( 0.15707963267948966192d+01),real( 0.15707963267948966192d+01),real( 0.15707963267948966192d+01)  ]; end;
if isempty(x_vec), x_vec([1:n_max]) =[real(   0.0009765625d+00),real(   0.0039062500d+00),real(   0.0625000000d+00),real(   0.1250000000d+00),real(   0.5000000000d+00),real(   1.0000000000d+00),real(   2.0000000000d+00),real(   4.0000000000d+00),real(   5.0000000000d+00),real(   6.0000000000d+00),real(   6.5000000000d+00),real(   8.0000000000d+00),real(  10.0000000000d+00),real(  12.0000000000d+00),real(  15.0000000000d+00),real(  20.0000000000d+00),real(  30.0000000000d+00),real(  50.0000000000d+00),real(  80.0000000000d+00),real( 100.0000000000d+00) ]; end;
if( n_data < 0 )
n_data = 0;
end;
n_data = fix(n_data + 1);
if( n_max < n_data )
n_data = 0;
x = 0.0d+00;
fx = 0.0d+00;
else;
x  = x_vec(n_data);
fx = fx_vec(n_data);
end;
return;
end
function [bessel_y0_intresult, xvalue ]=bessel_y0_int( xvalue );
%*******************************************************************************
%
%! BESSEL_Y0_INT calculates the integral of the Bessel function Y0.
%
%  Discussion:
%
%    The function is defined by:
%
%      Y0_INT(x) = Integral ( 0 <= t <= x ) Y0(t) dt
%
%    The code uses Chebyshev expansions whose coefficients are
%    given to 20 decimal places.
%
%    This subroutine is set up to work on IEEE machines.
%
%  Modified:
%
%    23 August 2004
%
%  Author:
%
%    Allan McLeod,
%    Department of Mathematics and Statistics,
%    Paisley University, High Street, Paisley, Scotland, PA12BE
%    macl_ms0@paisley.ac.uk
%
%  Reference:
%
%    Allan Mcleod,
%    Algorithm 757, MISCFUN: A software package to compute uncommon
%      special functions,
%    ACM Transactions on Mathematical Software,
%    Volume 22, Number 3, September 1996, pages 288-301.
%
%  Parameters:
%
%    Input, real ( kind = 8 ) XVALUE, the argument of the function.
%
%    Output, real ( kind = 8 ) BESSEL_Y0_INT, the value of the function.
%
%
bessel_y0_intresult=[];
persistent arj01 ary01 ary0a1 ary0a2 bessel_y0_int firstCall five12 gal2m1 gamln2 nine nterm1 nterm2 nterm3 nterm4 one one28 pib41 pib411 pib412 pib42 rt2bpi sixten t temp twobpi x xhigh xlow xmpi4 zero ; if isempty(firstCall),firstCall=1;end; 

if isempty(bessel_y0_intresult), bessel_y0_intresult=0; end;
if isempty(nine), nine = real( 9.0); end;
if isempty(nterm1), nterm1 = 22; end;
if isempty(nterm2), nterm2 = 22; end;
if isempty(nterm3), nterm3 = 17; end;
if isempty(nterm4), nterm4 = 15; end;
if isempty(one), one = real( 1.0); end;
if isempty(sixten), sixten = real( 16.0); end;
if isempty(x), x=0; end;
if isempty(zero), zero = 0.0d+00; end;
if isempty(arj01), arj01=zeros(1,23+1); end;
if isempty(ary01), ary01=zeros(1,24+1); end;
if isempty(ary0a1), ary0a1=zeros(1,21+1); end;
if isempty(ary0a2), ary0a2=zeros(1,18+1); end;
if isempty(five12), five12=0; end;
if isempty(gal2m1), gal2m1=0; end;
if isempty(gamln2), gamln2=0; end;
if isempty(one28), one28=0; end;
if isempty(pib41), pib41=0; end;
if isempty(pib411), pib411=0; end;
if isempty(pib412), pib412=0; end;
if isempty(pib42), pib42=0; end;
if isempty(rt2bpi), rt2bpi=0; end;
if isempty(t), t=0; end;
if isempty(temp), temp=0; end;
if isempty(twobpi), twobpi=0; end;
if isempty(xhigh), xhigh=0; end;
if isempty(xlow), xlow=0; end;
if isempty(xmpi4), xmpi4=0; end;
if firstCall,   one28=[128.0d0];  end;
if firstCall, five12=[512.0d0];  end;
if firstCall,   rt2bpi=[0.79788456080286535588d0];  end;
if firstCall,   pib411=[201.0d0];  end;
if firstCall, pib412=[256.0d0];  end;
if firstCall,   pib42=[0.24191339744830961566d-3];  end;
if firstCall,   twobpi=[0.63661977236758134308d0];  end;
if firstCall,   gal2m1=[-1.11593151565841244881d0];  end;
if firstCall,   gamln2=[-0.11593151565841244881d0];  end;
if firstCall,   arj01(0+1)=[0.38179279321690173518d0];  end;
if firstCall,   arj01(1+1)=[-0.21275636350505321870d0];  end;
if firstCall,   arj01(2+1)=[0.16754213407215794187d0];  end;
if firstCall,   arj01(3+1)=[-0.12853209772196398954d0];  end;
if firstCall,   arj01(4+1)=[0.10114405455778847013d0];  end;
if firstCall,   arj01(5+1)=[-0.9100795343201568859d-1];  end;
if firstCall,   arj01(6+1)=[0.6401345264656873103d-1];  end;
if firstCall,   arj01(7+1)=[-0.3066963029926754312d-1];  end;
if firstCall,   arj01(8+1)=[0.1030836525325064201d-1];  end;
if firstCall,   arj01(9+1)=[-0.255670650399956918d-2];  end;
if firstCall,   arj01(10+1)=[0.48832755805798304d-3];  end;
if firstCall,   arj01(11+1)=[-0.7424935126036077d-4];  end;
if firstCall,   arj01(12+1)=[0.922260563730861d-5];  end;
if firstCall,   arj01(13+1)=[-0.95522828307083d-6];  end;
if firstCall,   arj01(14+1)=[0.8388355845986d-7];  end;
if firstCall,   arj01(15+1)=[-0.633184488858d-8];  end;
if firstCall,   arj01(16+1)=[0.41560504221d-9];  end;
if firstCall,   arj01(17+1)=[-0.2395529307d-10];  end;
if firstCall,   arj01(18+1)=[0.122286885d-11];  end;
if firstCall,   arj01(19+1)=[-0.5569711d-13];  end;
if firstCall,   arj01(20+1)=[0.227820d-14];  end;
if firstCall,   arj01(21+1)=[-0.8417d-16];  end;
if firstCall,   arj01(22+1)=[0.282d-17];  end;
if firstCall,   arj01(23+1)=[-0.9d-19];  end;
if firstCall,   ary01(0+1)=[0.54492696302724365490d0];  end;
if firstCall,   ary01(1+1)=[-0.14957323588684782157d0];  end;
if firstCall,   ary01(2+1)=[0.11085634486254842337d0];  end;
if firstCall,   ary01(3+1)=[-0.9495330018683777109d-1];  end;
if firstCall,   ary01(4+1)=[0.6820817786991456963d-1];  end;
if firstCall,   ary01(5+1)=[-0.10324653383368200408d0];  end;
if firstCall,   ary01(6+1)=[0.10625703287534425491d0];  end;
if firstCall,   ary01(7+1)=[-0.6258367679961681990d-1];  end;
if firstCall,   ary01(8+1)=[0.2385645760338293285d-1];  end;
if firstCall,   ary01(9+1)=[-0.644864913015404481d-2];  end;
if firstCall,   ary01(10+1)=[0.131287082891002331d-2];  end;
if firstCall,   ary01(11+1)=[-0.20988088174989640d-3];  end;
if firstCall,   ary01(12+1)=[0.2716042484138347d-4];  end;
if firstCall,   ary01(13+1)=[-0.291199114014694d-5];  end;
if firstCall,   ary01(14+1)=[0.26344333093795d-6];  end;
if firstCall,   ary01(15+1)=[-0.2041172069780d-7];  end;
if firstCall,   ary01(16+1)=[0.137124781317d-8];  end;
if firstCall,   ary01(17+1)=[-0.8070680792d-10];  end;
if firstCall,   ary01(18+1)=[0.419883057d-11];  end;
if firstCall,   ary01(19+1)=[-0.19459104d-12];  end;
if firstCall,   ary01(20+1)=[0.808782d-14];  end;
if firstCall,   ary01(21+1)=[-0.30329d-15];  end;
if firstCall,   ary01(22+1)=[0.1032d-16];  end;
if firstCall,   ary01(23+1)=[-0.32d-18];  end;
if firstCall,   ary01(24+1)=[0.1d-19];  end;
if firstCall,   ary0a1(0+1)=[1.24030133037518970827d0];  end;
if firstCall,   ary0a1(1+1)=[-0.478125353632280693d-2];  end;
if firstCall,   ary0a1(2+1)=[0.6613148891706678d-4];  end;
if firstCall,   ary0a1(3+1)=[-0.186042740486349d-5];  end;
if firstCall,   ary0a1(4+1)=[0.8362735565080d-7];  end;
if firstCall,   ary0a1(5+1)=[-0.525857036731d-8];  end;
if firstCall,   ary0a1(6+1)=[0.42606363251d-9];  end;
if firstCall,   ary0a1(7+1)=[-0.4211761024d-10];  end;
if firstCall,   ary0a1(8+1)=[0.488946426d-11];  end;
if firstCall,   ary0a1(9+1)=[-0.64834929d-12];  end;
if firstCall,   ary0a1(10+1)=[0.9617234d-13];  end;
if firstCall,   ary0a1(11+1)=[-0.1570367d-13];  end;
if firstCall,   ary0a1(12+1)=[0.278712d-14];  end;
if firstCall,   ary0a1(13+1)=[-0.53222d-15];  end;
if firstCall,   ary0a1(14+1)=[0.10844d-15];  end;
if firstCall,   ary0a1(15+1)=[-0.2342d-16];  end;
if firstCall,   ary0a1(16+1)=[0.533d-17];  end;
if firstCall,   ary0a1(17+1)=[-0.127d-17];  end;
if firstCall,   ary0a1(18+1)=[0.32d-18];  end;
if firstCall,   ary0a1(19+1)=[-0.8d-19];  end;
if firstCall,   ary0a1(20+1)=[0.2d-19];  end;
if firstCall,   ary0a1(21+1)=[-0.1d-19];  end;
if firstCall,   ary0a2(0+1)=[1.99616096301341675339d0];  end;
if firstCall,   ary0a2(1+1)=[-0.190379819246668161d-2];  end;
if firstCall,   ary0a2(2+1)=[0.1539710927044226d-4];  end;
if firstCall,   ary0a2(3+1)=[-0.31145088328103d-6];  end;
if firstCall,   ary0a2(4+1)=[0.1110850971321d-7];  end;
if firstCall,   ary0a2(5+1)=[-0.58666787123d-9];  end;
if firstCall,   ary0a2(6+1)=[0.4139926949d-10];  end;
if firstCall,   ary0a2(7+1)=[-0.365398763d-11];  end;
if firstCall,   ary0a2(8+1)=[0.38557568d-12];  end;
if firstCall,   ary0a2(9+1)=[-0.4709800d-13];  end;
if firstCall,   ary0a2(10+1)=[0.650220d-14];  end;
if firstCall,   ary0a2(11+1)=[-0.99624d-15];  end;
if firstCall,   ary0a2(12+1)=[0.16700d-15];  end;
if firstCall,   ary0a2(13+1)=[-0.3028d-16];  end;
if firstCall,   ary0a2(14+1)=[0.589d-17];  end;
if firstCall,   ary0a2(15+1)=[-0.122d-17];  end;
if firstCall,   ary0a2(16+1)=[0.27d-18];  end;
if firstCall,   ary0a2(17+1)=[-0.6d-19];  end;
if firstCall,   ary0a2(18+1)=[0.1d-19];  end;
%
%   Machine-dependent constants (suitable for IEEE machines)
%
if firstCall,   xlow=[3.16101364d-8];  end;
if firstCall, xhigh=[9.007199256d15];  end;
firstCall=0;
%
x = xvalue;
if( x < zero )
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'BESSEL_Y0_INT - Fatal error!');
writef(1,['%s','\n'], '  Argument X < 0.');
bessel_y0_intresult = zero;
elseif( x == zero ) ;
bessel_y0_intresult = zero;
elseif( x < xlow ) ;
bessel_y0_intresult =( log( x ) + gal2m1 ) .* twobpi .* x;
elseif( x <= sixten ) ;
t = x .* x ./ one28 - one;
temp =( log( x ) + gamln2 ) .* cheval( nterm1, arj01, t );
temp = temp - cheval( nterm2, ary01, t );
bessel_y0_intresult = twobpi .* x .* temp;
elseif( x <= xhigh ) ;
t = five12 ./( x .* x ) - one;
pib41 = pib411 ./ pib412;
xmpi4 =( x - pib41 ) - pib42;
temp = sin( xmpi4 ) .* cheval( nterm3, ary0a1, t ) ./ x;
temp = temp + cos( xmpi4 ) .* cheval( nterm4, ary0a2, t );
bessel_y0_intresult = - rt2bpi .* temp ./ sqrt( x );
else;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'BESSEL_Y0_INT - Fatal error!');
writef(1,['%s','\n'], '  Argument too large.');
bessel_y0_intresult = zero;
end;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',xvalue); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [n_data, x, fx]=bessel_y0_int_values( n_data, x, fx );
%*******************************************************************************
%
%! BESSEL_Y0_INT_VALUES returns some values of the Bessel Y0 integral.
%
%  Discussion:
%
%    The function is defined by:
%
%      Y0_INT(x) = Integral ( 0 <= t <= x ) Y0(t) dt
%
%    The data was reported by McLeod.
%
%  Modified:
%
%    30 August 2004
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Allan McLeod,
%    Algorithm 757, MISCFUN: A software package to compute uncommon
%      special functions,
%    ACM Transactions on Mathematical Software,
%    Volume 22, Number 3, September 1996, pages 288-301.
%
%  Parameters:
%
%    Input/output, integer N_DATA.  The user sets N_DATA to 0 before the
%    first call.  On each call, the routine increments N_DATA by 1, and
%    returns the corresponding data; when there is no more data, the
%    output value of N_DATA will be 0 again.
%
%    Output, real ( kind = 8 ) X, the argument of the function.
%
%    Output, real ( kind = 8 ) FX, the value of the function.
%
%
persistent fx_vec n_max x_vec ; 

if isempty(n_max), n_max = 20; end;
%
if isempty(fx_vec), fx_vec([1:n_max]) =[real( -0.91442642860172110926d-02),real( -0.29682047390397591290d-01),real( -0.25391431276585388961d+00),real( -0.56179545591464028187d+00),real( -0.63706937660742309754d+00),real( -0.28219285008510084123d+00),real(  0.38366964785312561103d+00),real( -0.12595061285798929390d+00),real(  0.24129031832266684828d+00),real(  0.17138069757627037938d+00),real(  0.18958142627134083732d+00),real(  0.17203846136449706946d+00),real( -0.16821597677215029611d+00),real( -0.93607927351428988679d-01),real(  0.88229711948036648408d-01),real( -0.89324662736274161841d-02),real( -0.54814071000063488284d-01),real( -0.94958246003466381588d-01),real( -0.19598064853404969850d-01),real( -0.83084772357154773468d-02) ]; end;
if isempty(x_vec), x_vec([1:n_max]) =[real(   0.0019531250d+00),real(   0.0078125000d+00),real(   0.1250000000d+00),real(   0.5000000000d+00),real(   1.0000000000d+00),real(   2.0000000000d+00),real(   4.0000000000d+00),real(   6.0000000000d+00),real(  10.0000000000d+00),real(  16.0000000000d+00),real(  16.2500000000d+00),real(  17.0000000000d+00),real(  20.0000000000d+00),real(  25.0000000000d+00),real(  30.0000000000d+00),real(  40.0000000000d+00),real(  50.0000000000d+00),real(  70.0000000000d+00),real( 100.0000000000d+00),real( 125.0000000000d+00) ]; end;
%
if( n_data < 0 )
n_data = 0;
end;
n_data = fix(n_data + 1);
if( n_max < n_data )
n_data = 0;
x = 0.0d+00;
fx = 0.0d+00;
else;
x  = x_vec(n_data);
fx = fx_vec(n_data);
end;
return;
end
function [chevalresult, n, a, t ]=cheval( n, a, t );
%*******************************************************************************
%
%! CHEVAL evaluates a Chebyshev series.
%
%  Discussion:
%
%    This function evaluates a Chebyshev series, using the
%    Clenshaw method with Reinsch modification, as analysed
%    in the paper by Oliver.
%
%  Modified:
%
%    07 August 2004
%
%  Author:
%
%    Allan McLeod,
%    Department of Mathematics and Statistics,
%    Paisley University, High Street, Paisley, Scotland, PA12BE
%    macl_ms0@paisley.ac.uk
%
%  Reference:
%
%    Allan Mcleod,
%    Algorithm 757, MISCFUN: A software package to compute uncommon
%      special functions,
%    ACM Transactions on Mathematical Software,
%    Volume 22, Number 3, September 1996, pages 288-301.
%
%    J Oliver,
%    'An error analysis of the modified Clenshaw method for
%    evaluating Chebyshev and Fourier series',
%    J.I.M.A., Volume 20, 1977, pages 379-391.
%
%  Parameters:
%
%    Input, integer N, the number of terms in the sequence.
%
%    Input, real ( kind = 8 ) A(0:N), the coefficients of the Chebyshev series.
%
%    Input, real ( kind = 8 ) T, the value at which the series is
%    to be evaluated.
%
%    Output, real ( kind = 8 ) CHEVAL, the value of the Chebyshev series at T.
%
%
%
chevalresult=[];
persistent cheval d1 d2 half i test tt two u0 u1 u2 zero ; 

if isempty(chevalresult), chevalresult=0; end;
if isempty(d1), d1=0; end;
if isempty(d2), d2=0; end;
if isempty(half), half = 0.5d+00; end;
if isempty(i), i=0; end;
if isempty(test), test = real( 0.6); end;
if isempty(tt), tt=0; end;
if isempty(two), two = 2.0d+00; end;
if isempty(u0), u0=0; end;
if isempty(u1), u1=0; end;
if isempty(u2), u2=0; end;
if isempty(zero), zero = 0.0d+00; end;
u1 = zero;
%
%  T <= -0.6, Reinsch modification.
%
if( t <= -test )
d1 = zero;
tt =( t + half ) + half;
tt = tt + tt;
for i = n: -1: 0;
d2 = d1;
u2 = u1;
d1 = tt .* u2 + a(i+1) - d2;
u1 = d1 - u2;
end; i = fix(0-1);
chevalresult =( d1 - d2 ) ./ two;
%
%  -0.6 < T < 0.6, Standard Clenshaw method.
%
elseif( t < test ) ;
u0 = zero;
tt = t + t;
for i = n: -1: 0;
u2 = u1;
u1 = u0;
u0 = tt .* u1 + a(i+1) - u2;
end; i = fix(0-1);
chevalresult =( u0 - u2 ) ./ two;
%
%  0.6 <= T, Reinsch modification.
%
else;
d1 = zero;
tt =( t - half ) - half;
tt = tt + tt;
for i = n: -1: 0;
d2 = d1;
u2 = u1;
d1 = tt .* u2 + a(i+1) + d2;
u1 = d1 + u2;
end; i = fix(0-1);
chevalresult =( d1 + d2 ) ./ two;
end;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(3)), assignin('caller','FUntemp',t); evalin('caller',[inputname(3),'=FUntemp;']); end
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',n); evalin('caller',[inputname(1),'=FUntemp;']); end
if csnil&&~isempty(inputname(2)), assignin('caller','FUntemp',a); evalin('caller',[inputname(2),'=FUntemp;']); end
return;
end
function [clausenresult, xvalue ]=clausen( xvalue );
%*******************************************************************************
%
%! CLAUSEN calculates Clausen's integral.
%
%  Discussion:
%
%    The function is defined by:
%
%      CLAUSEN(x) = Integral ( 0 <= t <= x ) -ln ( 2 * sin ( t / 2 ) ) dt
%
%    The code uses Chebyshev expansions with the coefficients
%    given to 20 decimal places.
%
%    This subroutine is set up to work on IEEE machines.
%
%  Modified:
%
%    07 August 2004
%
%  Author:
%
%    Allan McLeod,
%    Department of Mathematics and Statistics,
%    Paisley University, High Street, Paisley, Scotland, PA12BE
%    macl_ms0@paisley.ac.uk
%
%  Reference:
%
%    Allan Mcleod,
%    Algorithm 757, MISCFUN: A software package to compute uncommon
%      special functions,
%    ACM Transactions on Mathematical Software,
%    Volume 22, Number 3, September 1996, pages 288-301.
%
%  Parameters:
%
%    Input, real ( kind = 8 ) XVALUE, the argument of the function.
%
%    Output, real ( kind = 8 ) CLAUSEN, the value of the function.
%
%
clausenresult=[];
persistent aclaus clausen firstCall half indx nterms one pi pisq t twopi twopia twopib x xhigh xsmall zero ; if isempty(firstCall),firstCall=1;end; 

if isempty(aclaus), aclaus=zeros(1,15+1); end;
if isempty(clausenresult), clausenresult=0; end;
if isempty(half), half = 0.5d+00; end;
if isempty(indx), indx=0; end;
if isempty(nterms), nterms = 13; end;
if isempty(one), one = real( 1.0); end;
if isempty(pi), pi = real( 3.1415926535897932385d0); end;
if isempty(pisq), pisq =real( 9.8696044010893586188d0); end;
if isempty(t), t=0; end;
if isempty(x), x=0; end;
if isempty(zero), zero = 0.0d+00; end;
if isempty(twopi), twopi=0; end;
if isempty(twopia), twopia=0; end;
if isempty(twopib), twopib=0; end;
if isempty(xhigh), xhigh=0; end;
if isempty(xsmall), xsmall=0; end;
if firstCall,   twopi=[6.2831853071795864769d0];  end;
if firstCall,   twopia=[6.28125d0];  end;
if firstCall, twopib=[0.19353071795864769253d-2];  end;
if firstCall,   aclaus=[2.14269436376668844709d0,0.7233242812212579245d-1,0.101642475021151164d-2,0.3245250328531645d-4,0.133315187571472d-5,0.6213240591653d-7,0.313004135337d-8,0.16635723056d-9,0.919659293d-11,0.52400462d-12,0.3058040d-13,0.181969d-14,0.11004d-15,0.675d-17,0.42d-18,0.3d-19];  end;
%
%  Set machine-dependent constants (suitable for IEEE machines)
%
if firstCall,   xsmall=[2.3406689d-8];  end;
if firstCall, xhigh=[4.5036d15];  end;
firstCall=0;
%
x = xvalue;
if( xhigh < abs( x ) )
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'CLAUSEN - Warning!');
writef(1,['%s','\n'], '  Argument magnitude too large for accurate computation.');
clausenresult = zero;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',xvalue); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end;
indx = 1;
if( x < zero )
x = -x;
indx = -1;
end;
%
%  Argument reduced using simulated extra precision
%
if( twopi < x )
t = fix( x ./ twopi );
x =( x - t .* twopia ) - t .* twopib;
end;
if( pi < x )
x =( twopia - x ) + twopib;
indx = fix(-indx);
end;
if( x == zero )
clausenresult = zero;
elseif( x < xsmall ) ;
clausenresult = x .*( one - log( x ) );
else;
t =( x .* x ) ./ pisq - half;
t = t + t;
if( one < t )
t = one;
end;
clausenresult = x .* cheval( nterms, aclaus, t ) - x .* log( x );
end;
if( indx < 0 )
clausenresult = -clausenresult;
end;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',xvalue); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [n_data, x, fx]=clausen_values( n_data, x, fx );
%*******************************************************************************
%
%! CLAUSEN_VALUES returns some values of the Clausen's integral.
%
%  Discussion:
%
%    The function is defined by:
%
%      CLAUSEN(x) = Integral ( 0 <= t <= x ) -ln ( 2 * sin ( t / 2 ) ) dt
%
%    The data was reported by McLeod.
%
%  Modified:
%
%    25 August 2004
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Allan McLeod,
%    Algorithm 757, MISCFUN: A software package to compute uncommon
%      special functions,
%    ACM Transactions on Mathematical Software,
%    Volume 22, Number 3, September 1996, pages 288-301.
%
%  Parameters:
%
%    Input/output, integer N_DATA.  The user sets N_DATA to 0 before the
%    first call.  On each call, the routine increments N_DATA by 1, and
%    returns the corresponding data; when there is no more data, the
%    output value of N_DATA will be 0 again.
%
%    Output, real ( kind = 8 ) X, the argument of the function.
%
%    Output, real ( kind = 8 ) FX, the value of the function.
%
%
persistent fx_vec n_max x_vec ; 

if isempty(n_max), n_max = 20; end;
%
if isempty(fx_vec), fx_vec([1:n_max]) =[real(  0.14137352886760576684d-01),real(  0.13955467081981281934d+00),real( -0.38495732156574238507d+00),real(  0.84831187770367927099d+00),real(  0.10139591323607685043d+01),real( -0.93921859275409211003d+00),real(  0.72714605086327924743d+00),real(  0.43359820323553277936d+00),real( -0.98026209391301421161d-01),real( -0.56814394442986978080d+00),real( -0.70969701784448921625d+00),real(  0.99282013254695671871d+00),real( -0.98127747477447367875d+00),real( -0.64078266570172320959d+00),real(  0.86027963733231192456d+00),real(  0.39071647608680211043d+00),real(  0.47574793926539191502d+00),real(  0.10105014481412878253d+01),real(  0.96332089044363075154d+00),real( -0.61782699481929311757d+00) ]; end;
if isempty(x_vec), x_vec([1:n_max]) =[real(   0.0019531250d+00),real(   0.0312500000d+00),real(  -0.1250000000d+00),real(   0.5000000000d+00),real(   1.0000000000d+00),real(  -1.5000000000d+00),real(   2.0000000000d+00),real(   2.5000000000d+00),real(  -3.0000000000d+00),real(   4.0000000000d+00),real(   4.2500000000d+00),real(  -5.0000000000d+00),real(   5.5000000000d+00),real(   6.0000000000d+00),real(   8.0000000000d+00),real( -10.0000000000d+00),real(  15.0000000000d+00),real(  20.0000000000d+00),real( -30.0000000000d+00),real(  50.0000000000d+00) ]; end;
if( n_data < 0 )
n_data = 0;
end;
n_data = fix(n_data + 1);
if( n_max < n_data )
n_data = 0;
x = 0.0d+00;
fx = 0.0d+00;
else;
x  = x_vec(n_data);
fx = fx_vec(n_data);
end;
return;
end
function [debye1result, xvalue ]=debye1( xvalue );
%*******************************************************************************
%
%! DEBYE1 calculates the Debye function of order 1.
%
%  Discussion:
%
%    The function is defined by:
%
%      DEBYE1(x) = 1 / x * Integral ( 0 <= t <= x ) t / ( exp ( t ) - 1 ) dt
%
%    The code uses Chebyshev series whose coefficients
%    are given to 20 decimal places.
%
%    This subroutine is set up to work on IEEE machines.
%
%  Modified:
%
%    07 August 2004
%
%  Author:
%
%    Allan McLeod,
%    Department of Mathematics and Statistics,
%    Paisley University, High Street, Paisley, Scotland, PA12BE
%    macl_ms0@paisley.ac.uk
%
%  Reference:
%
%    Allan Mcleod,
%    Algorithm 757, MISCFUN: A software package to compute uncommon
%      special functions,
%    ACM Transactions on Mathematical Software,
%    Volume 22, Number 3, September 1996, pages 288-301.
%
%  Parameters:
%
%    Input, real ( kind = 8 ) XVALUE, the argument of the function.
%
%    Output, real ( kind = 8 ) DEBYE1, the value of the function.
%
%
debye1result=[];
persistent adeb1 debinf debye1 eight expmx firstCall four half i nexp nine nterms one quart rk sum1 t thirt6 x xk xlim xlow xupper zero ; if isempty(firstCall),firstCall=1;end; 

if isempty(adeb1), adeb1=zeros(1,18+1); end;
if isempty(debye1result), debye1result=0; end;
if isempty(eight), eight = 8.0d+00; end;
if isempty(four), four = 4.0d+00; end;
if isempty(half), half = 0.5d+00; end;
if isempty(i), i=0; end;
if isempty(nexp), nexp=0; end;
if isempty(nterms), nterms = 15; end;
if isempty(one), one = 1.0d+00; end;
if isempty(quart), quart = 0.25d+00; end;
if isempty(x), x=0; end;
if isempty(zero), zero = 0.0d+00; end;
if isempty(debinf), debinf=0; end;
if isempty(expmx), expmx=0; end;
if isempty(nine), nine=0; end;
if isempty(rk), rk=0; end;
if isempty(sum1), sum1=0; end;
if isempty(t), t=0; end;
if isempty(thirt6), thirt6=0; end;
if isempty(xk), xk=0; end;
if isempty(xlim), xlim=0; end;
if isempty(xlow), xlow=0; end;
if isempty(xupper), xupper=0; end;
if firstCall,   nine=[9.0d0];  end;
if firstCall, thirt6 =[36.0d0];  end;
if firstCall,   debinf=[0.60792710185402662866d0];  end;
if firstCall,   adeb1=[2.40065971903814101941d0,0.19372130421893600885d0,-0.623291245548957703d-2,0.35111747702064800d-3,-0.2282224667012310d-4,0.158054678750300d-5,-0.11353781970719d-6,0.835833611875d-8,-0.62644247872d-9,0.4760334890d-10,-0.365741540d-11,0.28354310d-12,-0.2214729d-13,0.174092d-14,-0.13759d-15,0.1093d-16,-0.87d-18,0.7d-19,-0.1d-19];  end;
%
%   Machine-dependent constants (suitable for IEEE machines)
%
if firstCall,   xlow=[0.298023d-7];  end;
if firstCall, xupper=[35.35051d0];  end;
if firstCall, xlim=[708.39642d0];  end;
firstCall=0;
x = xvalue;
if( x < zero )
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'DEBYE1 - Fatal error!');
writef(1,['%s','\n'], '  Argument X < 0.');
debye1result = zero;
elseif( x < xlow ) ;
debye1result =(( x - nine ) .* x + thirt6 ) ./ thirt6;
elseif( x <= four ) ;
t =(( x .* x ./ eight ) - half ) - half;
debye1result = cheval( nterms, adeb1, t ) - quart .* x;
else;
debye1result = one ./( x .* debinf );
if( x < xlim )
expmx = exp( -x );
if( xupper < x )
debye1result = debye1result - expmx .*( one + one ./ x );
else;
sum1 = zero;
rk = fix( xlim ./ x );
nexp = fix(fix( rk ));
xk = rk .* x;
for i = nexp: -1: 1;
t =( one + one ./ xk ) ./ rk;
sum1 = sum1 .* expmx + t;
rk = rk - one;
xk = xk - x;
end; i = fix(1-1);
debye1result = debye1result - sum1 .* expmx;
end;
end;
end;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',xvalue); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [n_data, x, fx]=debye1_values( n_data, x, fx );
%*******************************************************************************
%
%! DEBYE1_VALUES returns some values of Debye's function of order 1.
%
%  Discussion:
%
%    The function is defined by:
%
%      DEBYE1(x) = 1 / x * Integral ( 0 <= t <= x ) t / ( exp ( t ) - 1 ) dt
%
%    The data was reported by McLeod.
%
%  Modified:
%
%    27 August 2004
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Allan McLeod,
%    Algorithm 757, MISCFUN: A software package to compute uncommon
%      special functions,
%    ACM Transactions on Mathematical Software,
%    Volume 22, Number 3, September 1996, pages 288-301.
%
%  Parameters:
%
%    Input/output, integer N_DATA.  The user sets N_DATA to 0 before the
%    first call.  On each call, the routine increments N_DATA by 1, and
%    returns the corresponding data; when there is no more data, the
%    output value of N_DATA will be 0 again.
%
%    Output, real ( kind = 8 ) X, the argument of the function.
%
%    Output, real ( kind = 8 ) FX, the value of the function.
%
%
persistent fx_vec n_max x_vec ; 

if isempty(n_max), n_max = 20; end;
%
if isempty(fx_vec), fx_vec([1:n_max]) =[real(  0.99951182471380889183d+00),real(  0.99221462647120597836d+00),real(  0.96918395997895308324d+00),real(  0.88192715679060552968d+00),real(  0.77750463411224827642d+00),real(  0.68614531078940204342d+00),real(  0.60694728460981007205d+00),real(  0.53878956907785587703d+00),real(  0.48043521957304283829d+00),real(  0.38814802129793784501d+00),real(  0.36930802829242526815d+00),real(  0.32087619770014612104d+00),real(  0.29423996623154246701d+00),real(  0.27126046678502189985d+00),real(  0.20523930310221503723d+00),real(  0.16444346567994602563d+00),real(  0.10966194482735821276d+00),real(  0.82246701178200016086d-01),real(  0.54831135561510852445d-01),real(  0.32898681336964528729d-01) ]; end;
if isempty(x_vec), x_vec([1:n_max]) =[real(   0.0019531250d+00),real(   0.0312500000d+00),real(   0.1250000000d+00),real(   0.5000000000d+00),real(   1.0000000000d+00),real(   1.5000000000d+00),real(   2.0000000000d+00),real(   2.5000000000d+00),real(   3.0000000000d+00),real(   4.0000000000d+00),real(   4.2500000000d+00),real(   5.0000000000d+00),real(   5.5000000000d+00),real(   6.0000000000d+00),real(   8.0000000000d+00),real(  10.0000000000d+00),real(  15.0000000000d+00),real(  20.0000000000d+00),real(  30.0000000000d+00),real(  50.0000000000d+00) ]; end;
%
if( n_data < 0 )
n_data = 0;
end;
n_data = fix(n_data + 1);
if( n_max < n_data )
n_data = 0;
x = 0.0d+00;
fx = 0.0d+00;
else;
x  = x_vec(n_data);
fx = fx_vec(n_data);
end;
return;
end
function [debye2result, xvalue ]=debye2( xvalue );
%*******************************************************************************
%
%! DEBYE2 calculates the Debye function of order 2.
%
%  Discussion:
%
%    The function is defined by:
%
%      DEBYE2(x) = 2 / x^2 * Integral ( 0 <= t <= x ) t^2 / ( exp ( t ) - 1 ) dt
%
%    The code uses Chebyshev series whose coefficients
%    are given to 20 decimal places.
%
%    This subroutine is set up to work on IEEE machines.
%
%  Modified:
%
%    24 August 2004
%
%  Author:
%
%    Allan McLeod,
%    Department of Mathematics and Statistics,
%    Paisley University, High Street, Paisley, Scotland, PA12BE
%    macl_ms0@paisley.ac.uk
%
%  Reference:
%
%    Allan Mcleod,
%    Algorithm 757, MISCFUN: A software package to compute uncommon
%      special functions,
%    ACM Transactions on Mathematical Software,
%    Volume 22, Number 3, September 1996, pages 288-301.
%
%  Parameters:
%
%    Input, real ( kind = 8 ) XVALUE, the argument of the function.
%
%    Output, real ( kind = 8 ) DEBYE2, the value of the function.
%
%
debye2result=[];
persistent adeb2 debinf debye2 eight expmx firstCall four half i nexp nterms one rk sum1 t three twent4 two x xk xlim1 xlim2 xlow xupper zero ; if isempty(firstCall),firstCall=1;end; 

if isempty(debye2result), debye2result=0; end;
if isempty(eight), eight = real( 8.0); end;
if isempty(four), four = real( 4.0d+00); end;
if isempty(half), half = 0.5d+00; end;
if isempty(i), i=0; end;
if isempty(nexp), nexp=0; end;
if isempty(nterms), nterms = 17; end;
if isempty(one), one = real( 1.0); end;
if isempty(three), three = 3.0d+00; end;
if isempty(two), two = 2.0d+00; end;
if isempty(x), x=0; end;
if isempty(zero), zero = 0.0d+00; end;
if isempty(adeb2), adeb2=zeros(1,18+1); end;
if isempty(debinf), debinf=0; end;
if isempty(expmx), expmx=0; end;
if isempty(rk), rk=0; end;
if isempty(sum1), sum1=0; end;
if isempty(t), t=0; end;
if isempty(twent4), twent4=0; end;
if isempty(xk), xk=0; end;
if isempty(xlim1), xlim1=0; end;
if isempty(xlim2), xlim2=0; end;
if isempty(xlow), xlow=0; end;
if isempty(xupper), xupper=0; end;
if firstCall,   twent4=[24.0d0];  end;
if firstCall,   debinf=[4.80822761263837714160d0];  end;
if firstCall,   adeb2=[2.59438102325707702826d0,0.28633572045307198337d0,-0.1020626561580467129d-1,0.60491097753468435d-3,-0.4052576589502104d-4,0.286338263288107d-5,-0.20863943030651d-6,0.1552378758264d-7,-0.117312800866d-8,0.8973585888d-10,-0.693176137d-11,0.53980568d-12,-0.4232405d-13,0.333778d-14,-0.26455d-15,0.2106d-16,-0.168d-17,0.13d-18,-0.1d-19];  end;
%
%   Machine-dependent constants
%
if firstCall,   xlow=[0.298023d-7];  end;
if firstCall, xupper=[35.35051d0];  end;
if firstCall,   xlim1=[708.39642d0];  end;
if firstCall, xlim2=[2.1572317d154];  end;
firstCall=0;
%
x = xvalue;
if( x < zero )
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'DEBYE2 - Fatal error!');
writef(1,['%s','\n'], '  Argument X < 0.');
debye2result = zero;
elseif( x < xlow ) ;
debye2result =(( x - eight ) .* x + twent4 ) ./ twent4;
elseif( x <= four ) ;
t =(( x .* x ./ eight ) - half ) - half;
debye2result = cheval( nterms, adeb2, t ) - x ./ three;
elseif( x <= xupper ) ;
expmx = exp( -x );
sum1 = zero;
rk = fix( xlim1 ./ x );
nexp = fix(fix( rk ));
xk = rk .* x;
for i = nexp: -1: 1;
t =( one + two ./ xk + two ./( xk .* xk ) ) ./ rk;
sum1 = sum1 .* expmx + t;
rk = rk - one;
xk = xk - x;
end; i = fix(1-1);
debye2result = debinf ./( x .* x ) - two .* sum1 .* expmx;
elseif( x < xlim1 ) ;
expmx = exp( -x );
sum1 =(( x + two ) .* x + two ) ./( x .* x );
debye2result = debinf ./( x .* x ) - two .* sum1 .* expmx;
elseif( x <= xlim2 ) ;
debye2result = debinf ./( x .* x );
else;
debye2result = zero;
end;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',xvalue); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [n_data, x, fx]=debye2_values( n_data, x, fx );
%*******************************************************************************
%
%! DEBYE2_VALUES returns some values of Debye's function of order 2.
%
%  Discussion:
%
%    The function is defined by:
%
%      DEBYE2(x) = 2 / x^2 * Integral ( 0 <= t <= x ) t^2 / ( exp ( t ) - 1 ) dt
%
%    The data was reported by McLeod.
%
%  Modified:
%
%    27 August 2004
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Allan McLeod,
%    Algorithm 757, MISCFUN: A software package to compute uncommon
%      special functions,
%    ACM Transactions on Mathematical Software,
%    Volume 22, Number 3, September 1996, pages 288-301.
%
%  Parameters:
%
%    Input/output, integer N_DATA.  The user sets N_DATA to 0 before the
%    first call.  On each call, the routine increments N_DATA by 1, and
%    returns the corresponding data; when there is no more data, the
%    output value of N_DATA will be 0 again.
%
%    Output, real ( kind = 8 ) X, the argument of the function.
%
%    Output, real ( kind = 8 ) FX, the value of the function.
%
%
persistent fx_vec n_max x_vec ; 

if isempty(n_max), n_max = 20; end;
%
if isempty(fx_vec), fx_vec([1:n_max]) =[real(  0.99934911727904599738d+00),real(  0.98962402299599181205d+00),real(  0.95898426200345986743d+00),real(  0.84372119334725358934d+00),real(  0.70787847562782928288d+00),real(  0.59149637225671282917d+00),real(  0.49308264399053185014d+00),real(  0.41079413579749669069d+00),real(  0.34261396060786351671d+00),real(  0.24055368752127897660d+00),real(  0.22082770061202308232d+00),real(  0.17232915939014138975d+00),real(  0.14724346738730182894d+00),real(  0.12666919046715789982d+00),real(  0.74268805954862854626d-01),real(  0.47971498020121871622d-01),real(  0.21369201683658373846d-01),real(  0.12020564476446432799d-01),real(  0.53424751249537071952d-02),real(  0.19232910450553508562d-02) ]; end;
if isempty(x_vec), x_vec([1:n_max]) =[real(   0.0019531250d+00),real(   0.0312500000d+00),real(   0.1250000000d+00),real(   0.5000000000d+00),real(   1.0000000000d+00),real(   1.5000000000d+00),real(   2.0000000000d+00),real(   2.5000000000d+00),real(   3.0000000000d+00),real(   4.0000000000d+00),real(   4.2500000000d+00),real(   5.0000000000d+00),real(   5.5000000000d+00),real(   6.0000000000d+00),real(   8.0000000000d+00),real(  10.0000000000d+00),real(  15.0000000000d+00),real(  20.0000000000d+00),real(  30.0000000000d+00),real(  50.0000000000d+00) ]; end;
%
if( n_data < 0 )
n_data = 0;
end;
n_data = fix(n_data + 1);
if( n_max < n_data )
n_data = 0;
x = 0.0d+00;
fx = 0.0d+00;
else;
x  = x_vec(n_data);
fx = fx_vec(n_data);
end;
return;
end
function [debye3result, xvalue ]=debye3( xvalue );
%*******************************************************************************
%
%! DEBYE3 calculates the Debye function of order 3.
%
%  Discussion:
%
%    The function is defined by:
%
%      DEBYE3(x) = 3 / x^3 * Integral ( 0 <= t <= x ) t^3 / ( exp ( t ) - 1 ) dt
%
%    The code uses Chebyshev series whose coefficients
%    are given to 20 decimal places.
%
%    This subroutine is set up to work on IEEE machines.
%
%  Modified:
%
%    07 August 2004
%
%  Author:
%
%    Allan McLeod,
%    Department of Mathematics and Statistics,
%    Paisley University, High Street, Paisley, Scotland, PA12BE
%    macl_ms0@paisley.ac.uk
%
%  Reference:
%
%    Allan Mcleod,
%    Algorithm 757, MISCFUN: A software package to compute uncommon
%      special functions,
%    ACM Transactions on Mathematical Software,
%    Volume 22, Number 3, September 1996, pages 288-301.
%
%  Parameters:
%
%    Input, real ( kind = 8 ) XVALUE, the argument of the function.
%
%    Output, real ( kind = 8 ) DEBYE3, the value of the function.
%
%
debye3result=[];
persistent adeb3 debinf debye3 eight expmx firstCall four half i nexp nterms one pt375 rk sevp5 six sum1 t three twenty x xk xki xlim1 xlim2 xlow xupper zero ; if isempty(firstCall),firstCall=1;end; 

if isempty(debye3result), debye3result=0; end;
if isempty(eight), eight = real( 8.0); end;
if isempty(four), four = real( 4.0d+00); end;
if isempty(half), half = 0.5d+00; end;
if isempty(i), i=0; end;
if isempty(nexp), nexp=0; end;
if isempty(nterms), nterms = 16; end;
if isempty(one), one = real( 1.0); end;
if isempty(six), six = real( 6.0d+00); end;
if isempty(three), three = 3.0d+00; end;
if isempty(x), x=0; end;
if isempty(zero), zero = 0.0d+00; end;
if isempty(adeb3), adeb3=zeros(1,18+1); end;
if isempty(debinf), debinf=0; end;
if isempty(expmx), expmx=0; end;
if isempty(pt375), pt375=0; end;
if isempty(rk), rk=0; end;
if isempty(sevp5), sevp5=0; end;
if isempty(sum1), sum1=0; end;
if isempty(t), t=0; end;
if isempty(twenty), twenty=0; end;
if isempty(xk), xk=0; end;
if isempty(xki), xki=0; end;
if isempty(xlim1), xlim1=0; end;
if isempty(xlim2), xlim2=0; end;
if isempty(xlow), xlow=0; end;
if isempty(xupper), xupper=0; end;
if firstCall,   pt375=[0.375d0];  end;
if firstCall,   sevp5=[7.5d0];  end;
if firstCall, twenty=[20.0d0];  end;
if firstCall,   debinf=[0.51329911273421675946d-1];  end;
if firstCall,   adeb3=[2.70773706832744094526d0,0.34006813521109175100d0,-0.1294515018444086863d-1,0.79637553801738164d-3,-0.5463600095908238d-4,0.392430195988049d-5,-0.28940328235386d-6,0.2173176139625d-7,-0.165420999498d-8,0.12727961892d-9,-0.987963459d-11,0.77250740d-12,-0.6077972d-13,0.480759d-14,-0.38204d-15,0.3048d-16,-0.244d-17,0.20d-18,-0.2d-19];  end;
%
%   Machine-dependent constants
%
if firstCall,   xlow=[0.298023d-7];  end;
if firstCall, xupper=[35.35051d0];  end;
if firstCall,   xlim1=[708.39642d0];  end;
if firstCall, xlim2=[0.9487163d103];  end;
firstCall=0;
%
x = xvalue;
if( x < zero )
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'DEBYE3 - Fatal error!');
writef(1,['%s','\n'], '  Argument X < 0.');
debye3result = zero;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',xvalue); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end;
if( x < xlow )
debye3result =(( x - sevp5 ) .* x + twenty ) ./ twenty;
elseif( x <= 4 ) ;
t =(( x .* x ./ eight ) - half ) - half;
debye3result = cheval( nterms, adeb3, t ) - pt375 .* x;
else;
%
%   Code for x > 4.0
%
if( xlim2 < x )
debye3result = zero;
else;
debye3result = one ./( debinf .* x .* x .* x );
if( x < xlim1 )
expmx = exp( -x );
if( xupper < x )
sum1 =((( x + three ) .* x + six ) .* x + six ) ./( x .* x .* x );
else;
sum1 = zero;
rk = fix( xlim1 ./ x );
nexp = fix(fix( rk ));
xk = rk .* x;
for i = nexp: -1: 1;
xki = one ./ xk;
t =((( six .* xki + six ) .* xki + three ) .* xki + one ) ./ rk;
sum1 = sum1 .* expmx + t;
rk = rk - one;
xk = xk - x;
end; i = fix(1-1);
end;
debye3result = debye3result - three .* sum1 .* expmx;
end;
end;
end;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',xvalue); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [n_data, x, fx]=debye3_values( n_data, x, fx );
%*******************************************************************************
%
%! DEBYE3_VALUES returns some values of Debye's function of order 3.
%
%  Discussion:
%
%    The function is defined by:
%
%      DEBYE3(x) = 3 / x^3 * Integral ( 0 <= t <= x ) t^3 / ( exp ( t ) - 1 ) dt
%
%    The data was reported by McLeod.
%
%  Modified:
%
%    28 August 2004
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Allan McLeod,
%    Algorithm 757, MISCFUN: A software package to compute uncommon
%      special functions,
%    ACM Transactions on Mathematical Software,
%    Volume 22, Number 3, September 1996, pages 288-301.
%
%  Parameters:
%
%    Input/output, integer N_DATA.  The user sets N_DATA to 0 before the
%    first call.  On each call, the routine increments N_DATA by 1, and
%    returns the corresponding data; when there is no more data, the
%    output value of N_DATA will be 0 again.
%
%    Output, real ( kind = 8 ) X, the argument of the function.
%
%    Output, real ( kind = 8 ) FX, the value of the function.
%
%
persistent fx_vec n_max x_vec ; 

if isempty(n_max), n_max = 20; end;
%
if isempty(fx_vec), fx_vec([1:n_max]) =[real(  0.99926776885985461940d+00),real(  0.98833007755734698212d+00),real(  0.95390610472023510237d+00),real(  0.82496296897623372315d+00),real(  0.67441556407781468010d+00),real(  0.54710665141286285468d+00),real(  0.44112847372762418113d+00),real(  0.35413603481042394211d+00),real(  0.28357982814342246206d+00),real(  0.18173691382177474795d+00),real(  0.16277924385112436877d+00),real(  0.11759741179993396450d+00),real(  0.95240802723158889887d-01),real(  0.77581324733763020269d-01),real(  0.36560295673194845002d-01),real(  0.19295765690345489563d-01),real(  0.57712632276188798621d-02),real(  0.24352200674805479827d-02),real(  0.72154882216335666096d-03),real(  0.15585454565440389896d-03) ]; end;
if isempty(x_vec), x_vec([1:n_max]) =[real(   0.0019531250d+00),real(   0.0312500000d+00),real(   0.1250000000d+00),real(   0.5000000000d+00),real(   1.0000000000d+00),real(   1.5000000000d+00),real(   2.0000000000d+00),real(   2.5000000000d+00),real(   3.0000000000d+00),real(   4.0000000000d+00),real(   4.2500000000d+00),real(   5.0000000000d+00),real(   5.5000000000d+00),real(   6.0000000000d+00),real(   8.0000000000d+00),real(  10.0000000000d+00),real(  15.0000000000d+00),real(  20.0000000000d+00),real(  30.0000000000d+00),real(  50.0000000000d+00) ]; end;
%
if( n_data < 0 )
n_data = 0;
end;
n_data = fix(n_data + 1);
if( n_max < n_data )
n_data = 0;
x = 0.0d+00;
fx = 0.0d+00;
else;
x  = x_vec(n_data);
fx = fx_vec(n_data);
end;
return;
end
function [debye4result, xvalue ]=debye4( xvalue );
%*******************************************************************************
%
%! DEBYE4 calculates the Debye function of order 4.
%
%  Discussion:
%
%    The function is defined by:
%
%      DEBYE4(x) = 4 / x^4 * Integral ( 0 <= t <= x ) t^4 / ( exp ( t ) - 1 ) dt
%
%    The code uses Chebyshev series whose coefficients
%    are given to 20 decimal places.
%
%    This subroutine is set up to work on IEEE machines.
%
%  Modified:
%
%    07 August 2004
%
%  Author:
%
%    Allan McLeod,
%    Department of Mathematics and Statistics,
%    Paisley University, High Street, Paisley, Scotland, PA12BE
%    macl_ms0@paisley.ac.uk
%
%  Reference:
%
%    Allan Mcleod,
%    Algorithm 757, MISCFUN: A software package to compute uncommon
%      special functions,
%    ACM Transactions on Mathematical Software,
%    Volume 22, Number 3, September 1996, pages 288-301.
%
%  Parameters:
%
%    Input, real ( kind = 8 ) XVALUE, the argument of the function.
%
%    Output, real ( kind = 8 ) DEBYE4, the value of the function.
%
%
debye4result=[];
persistent adeb4 debinf debye4 eight eightn expmx firstCall five forty5 four half i nexp nterms one rk sum1 t twelve twent4 twopt5 x xk xki xlim1 xlim2 xlow xupper zero ; if isempty(firstCall),firstCall=1;end; 

if isempty(debye4result), debye4result=0; end;
if isempty(eight), eight = real( 8.0); end;
if isempty(four), four = real( 4.0d+00); end;
if isempty(half), half = 0.5d+00; end;
if isempty(i), i=0; end;
if isempty(nexp), nexp=0; end;
if isempty(nterms), nterms = 16; end;
if isempty(one), one = real( 1.0); end;
if isempty(x), x=0; end;
if isempty(zero), zero = 0.0d+00; end;
if isempty(adeb4), adeb4=zeros(1,18+1); end;
if isempty(debinf), debinf=0; end;
if isempty(eightn), eightn=0; end;
if isempty(expmx), expmx=0; end;
if isempty(five), five=0; end;
if isempty(forty5), forty5=0; end;
if isempty(rk), rk=0; end;
if isempty(sum1), sum1=0; end;
if isempty(t), t=0; end;
if isempty(twelve), twelve=0; end;
if isempty(twent4), twent4=0; end;
if isempty(twopt5), twopt5=0; end;
if isempty(xk), xk=0; end;
if isempty(xki), xki=0; end;
if isempty(xlim1), xlim1=0; end;
if isempty(xlim2), xlim2=0; end;
if isempty(xlow), xlow=0; end;
if isempty(xupper), xupper=0; end;
if firstCall,   twopt5=[2.5d0];  end;
if firstCall, five=[5.0d0];  end;
if firstCall,   twelve=[12.0d0];  end;
if firstCall, eightn=[18.0d0];  end;
if firstCall,   twent4=[24.0d0];  end;
if firstCall, forty5 =[45.0d0];  end;
if firstCall,   debinf=[99.54506449376351292781d0];  end;
if firstCall,   adeb4=[2.78186941502052346008d0,0.37497678352689286364d0,-0.1494090739903158326d-1,0.94567981143704274d-3,-0.6613291613893255d-4,0.481563298214449d-5,-0.35880839587593d-6,0.2716011874160d-7,-0.208070991223d-8,0.16093838692d-9,-0.1254709791d-10,0.98472647d-12,-0.7772369d-13,0.616483d-14,-0.49107d-15,0.3927d-16,-0.315d-17,0.25d-18,-0.2d-19];  end;
%
%   Machine-dependent constants
%
if firstCall,   xlow=[0.298023d-7];  end;
if firstCall, xupper=[35.35051d0];  end;
if firstCall,   xlim1=[708.39642d0];  end;
if firstCall, xlim2=[2.5826924d77];  end;
firstCall=0;
%
x = xvalue;
%
%   Check XVALUE >= 0.0
%
if( x < zero )
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'DEBYE4 - Fatal error!');
writef(1,['%s','\n'], '  Argument X < 0.');
debye4result = zero;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',xvalue); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end;
if( x < xlow )
debye4result =(( twopt5 .* x - eightn ) .* x + forty5 ) ./ forty5;
elseif( x <= four ) ;
t =(( x .* x ./ eight ) - half ) - half;
debye4result = cheval( nterms, adeb4, t ) -( x + x ) ./ five;
else;
%
%   Code for x > 4.0
%
if( xlim2 < x )
debye4result = zero;
else;
t = x .* x;
debye4result =( debinf ./ t ) ./ t;
if( x < xlim1 )
expmx = exp( -x );
if( xupper < x )
sum1 =(((( x + four ) .* x + twelve ) .* x +twent4 ) .* x + twent4 ) ./( x .* x .* x .* x );
else;
sum1 = zero;
rk = fix( xlim1 ./ x );
nexp = fix(fix( rk ));
xk = rk .* x;
for i = nexp: -1: 1;
xki = one ./ xk;
t =(((( twent4 .* xki + twent4 ) .* xki +twelve ) .* xki + four ) .* xki + one ) ./ rk;
sum1 = sum1 .* expmx + t;
rk = rk - one;
xk = xk - x;
end; i = fix(1-1);
end;
debye4result = debye4result - four .* sum1 .* expmx;
end;
end;
end;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',xvalue); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [n_data, x, fx]=debye4_values( n_data, x, fx );
%*******************************************************************************
%
%! DEBYE4_VALUES returns some values of Debye's function of order 4.
%
%  Discussion:
%
%    The function is defined by:
%
%      DEBYE4(x) = 4 / x^4 * Integral ( 0 <= t <= x ) t^4 / ( exp ( t ) - 1 ) dt
%
%    The data was reported by McLeod.
%
%  Modified:
%
%    28 August 2004
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Allan McLeod,
%    Algorithm 757, MISCFUN: A software package to compute uncommon
%      special functions,
%    ACM Transactions on Mathematical Software,
%    Volume 22, Number 3, September 1996, pages 288-301.
%
%  Parameters:
%
%    Input/output, integer N_DATA.  The user sets N_DATA to 0 before the
%    first call.  On each call, the routine increments N_DATA by 1, and
%    returns the corresponding data; when there is no more data, the
%    output value of N_DATA will be 0 again.
%
%    Output, real ( kind = 8 ) X, the argument of the function.
%
%    Output, real ( kind = 8 ) FX, the value of the function.
%
%
persistent fx_vec n_max x_vec ; 

if isempty(n_max), n_max = 20; end;
%
if isempty(fx_vec), fx_vec([1:n_max]) =[real(  0.99921896192761576256d+00),real(  0.98755425280996071022d+00),real(  0.95086788606389739976d+00),real(  0.81384569172034042516d+00),real(  0.65487406888673697092d+00),real(  0.52162830964878715188d+00),real(  0.41189273671788528876d+00),real(  0.32295434858707304628d+00),real(  0.25187863642883314410d+00),real(  0.15185461258672022043d+00),real(  0.13372661145921413299d+00),real(  0.91471377664481164749d-01),real(  0.71227828197462523663d-01),real(  0.55676547822738862783d-01),real(  0.21967566525574960096d-01),real(  0.96736755602711590082d-02),real(  0.19646978158351837850d-02),real(  0.62214648623965450200d-03),real(  0.12289514092077854510d-03),real(  0.15927210319002161231d-04) ]; end;
if isempty(x_vec), x_vec([1:n_max]) =[real(   0.0019531250d+00),real(   0.0312500000d+00),real(   0.1250000000d+00),real(   0.5000000000d+00),real(   1.0000000000d+00),real(   1.5000000000d+00),real(   2.0000000000d+00),real(   2.5000000000d+00),real(   3.0000000000d+00),real(   4.0000000000d+00),real(   4.2500000000d+00),real(   5.0000000000d+00),real(   5.5000000000d+00),real(   6.0000000000d+00),real(   8.0000000000d+00),real(  10.0000000000d+00),real(  15.0000000000d+00),real(  20.0000000000d+00),real(  30.0000000000d+00),real(  50.0000000000d+00) ]; end;
%
if( n_data < 0 )
n_data = 0;
end;
n_data = fix(n_data + 1);
if( n_max < n_data )
n_data = 0;
x = 0.0d+00;
fx = 0.0d+00;
else;
x  = x_vec(n_data);
fx = fx_vec(n_data);
end;
return;
end
function [exp3_intresult, xvalue ]=exp3_int( xvalue );
%*******************************************************************************
%
%! EXP3_INT calculates the integral of exp(-t^3).
%
%  Discussion:
%
%    The function is defined by:
%
%      EXP3_INT(x) = Integral ( 0 <= t <= x ) exp ( -t^3 ) dt
%
%    The code uses Chebyshev expansions, whose coefficients are
%    given to 20 decimal places.
%
%    This subroutine is set up to work on IEEE machines.
%
%  Modified:
%
%    07 August 2004
%
%  Author:
%
%    Allan McLeod,
%    Department of Mathematics and Statistics,
%    Paisley University, High Street, Paisley, Scotland, PA12BE
%    macl_ms0@paisley.ac.uk
%
%  Reference:
%
%    Allan Mcleod,
%    Algorithm 757, MISCFUN: A software package to compute uncommon
%      special functions,
%    ACM Transactions on Mathematical Software,
%    Volume 22, Number 3, September 1996, pages 288-301.
%
%  Parameters:
%
%    Input, real ( kind = 8 ) XVALUE, the argument of the function.
%
%    Output, real ( kind = 8 ) EXP3_INT, the value of the function.
%
%
exp3_intresult=[];
persistent aexp3 aexp3a exp3_int firstCall four funinf half nterm1 nterm2 one sixten t three two x xlow xupper zero ; if isempty(firstCall),firstCall=1;end; 

if isempty(exp3_intresult), exp3_intresult=0; end;
if isempty(four), four = real( 4.0d+00); end;
if isempty(half), half = 0.5d+00; end;
if isempty(nterm1), nterm1 = 22; end;
if isempty(nterm2), nterm2 = 20; end;
if isempty(one), one = real( 1.0); end;
if isempty(three), three = 3.0d+00; end;
if isempty(two), two = 2.0d+00; end;
if isempty(x), x=0; end;
if isempty(zero), zero = 0.0d+00; end;
if isempty(aexp3), aexp3=zeros(1,24+1); end;
if isempty(aexp3a), aexp3a=zeros(1,24+1); end;
if isempty(funinf), funinf=0; end;
if isempty(sixten), sixten=0; end;
if isempty(t), t=0; end;
if isempty(xlow), xlow=0; end;
if isempty(xupper), xupper=0; end;
if firstCall,   sixten =[16.0d0];  end;
if firstCall,   funinf=[0.89297951156924921122d0];  end;
if firstCall,   aexp3(0+1)=[1.26919841422112601434d0];  end;
if firstCall,   aexp3(1+1)=[-0.24884644638414098226d0];  end;
if firstCall,   aexp3(2+1)=[0.8052622071723104125d-1];  end;
if firstCall,   aexp3(3+1)=[-0.2577273325196832934d-1];  end;
if firstCall,   aexp3(4+1)=[0.759987887307377429d-2];  end;
if firstCall,   aexp3(5+1)=[-0.203069558194040510d-2];  end;
if firstCall,   aexp3(6+1)=[0.49083458669932917d-3];  end;
if firstCall,   aexp3(7+1)=[-0.10768223914202077d-3];  end;
if firstCall,   aexp3(8+1)=[0.2155172626428984d-4];  end;
if firstCall,   aexp3(9+1)=[-0.395670513738429d-5];  end;
if firstCall,   aexp3(10+1)=[0.66992409338956d-6];  end;
if firstCall,   aexp3(11+1)=[-0.10513218080703d-6];  end;
if firstCall,   aexp3(12+1)=[0.1536258019825d-7];  end;
if firstCall,   aexp3(13+1)=[-0.209909603636d-8];  end;
if firstCall,   aexp3(14+1)=[0.26921095381d-9];  end;
if firstCall,   aexp3(15+1)=[-0.3251952422d-10];  end;
if firstCall,   aexp3(16+1)=[0.371148157d-11];  end;
if firstCall,   aexp3(17+1)=[-0.40136518d-12];  end;
if firstCall,   aexp3(18+1)=[0.4123346d-13];  end;
if firstCall,   aexp3(19+1)=[-0.403375d-14];  end;
if firstCall,   aexp3(20+1)=[0.37658d-15];  end;
if firstCall,   aexp3(21+1)=[-0.3362d-16];  end;
if firstCall,   aexp3(22+1)=[0.288d-17];  end;
if firstCall,   aexp3(23+1)=[-0.24d-18];  end;
if firstCall,   aexp3(24+1)=[0.2d-19];  end;
if firstCall,   aexp3a(0+1)=[1.92704649550682737293d0];  end;
if firstCall,   aexp3a(1+1)=[-0.3492935652048138054d-1];  end;
if firstCall,   aexp3a(2+1)=[0.145033837189830093d-2];  end;
if firstCall,   aexp3a(3+1)=[-0.8925336718327903d-4];  end;
if firstCall,   aexp3a(4+1)=[0.705423921911838d-5];  end;
if firstCall,   aexp3a(5+1)=[-0.66717274547611d-6];  end;
if firstCall,   aexp3a(6+1)=[0.7242675899824d-7];  end;
if firstCall,   aexp3a(7+1)=[-0.878258256056d-8];  end;
if firstCall,   aexp3a(8+1)=[0.116722344278d-8];  end;
if firstCall,   aexp3a(9+1)=[-0.16766312812d-9];  end;
if firstCall,   aexp3a(10+1)=[0.2575501577d-10];  end;
if firstCall,   aexp3a(11+1)=[-0.419578881d-11];  end;
if firstCall,   aexp3a(12+1)=[0.72010412d-12];  end;
if firstCall,   aexp3a(13+1)=[-0.12949055d-12];  end;
if firstCall,   aexp3a(14+1)=[0.2428703d-13];  end;
if firstCall,   aexp3a(15+1)=[-0.473311d-14];  end;
if firstCall,   aexp3a(16+1)=[0.95531d-15];  end;
if firstCall,   aexp3a(17+1)=[-0.19914d-15];  end;
if firstCall,   aexp3a(18+1)=[0.4277d-16];  end;
if firstCall,   aexp3a(19+1)=[-0.944d-17];  end;
if firstCall,   aexp3a(20+1)=[0.214d-17];  end;
if firstCall,   aexp3a(21+1)=[-0.50d-18];  end;
if firstCall,   aexp3a(22+1)=[0.12d-18];  end;
if firstCall,   aexp3a(23+1)=[-0.3d-19];  end;
if firstCall,   aexp3a(24+1)=[0.1d-19];  end;
%
%   Machine-dependent constants (suitable for IEEE machines)
%
if firstCall,   xlow=[0.762939d-5];  end;
if firstCall, xupper=[3.3243018d0];  end;
firstCall=0;
%
x = xvalue;
if( x < zero )
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'EXP3_INT - Fatal error!');
writef(1,['%s','\n'], '  Argument X < 0.');
exp3_intresult = zero;
elseif( x < xlow ) ;
exp3_intresult = x;
elseif( x <= two ) ;
t =(( x .* x .* x ./ four ) - half ) - half;
exp3_intresult = x .* cheval( nterm1, aexp3, t );
elseif( x <= xupper ) ;
t =(( sixten ./( x .* x .* x ) ) - half ) - half;
t = cheval( nterm2, aexp3a, t );
t = t .* exp( -x .* x .* x ) ./( three .* x .* x );
exp3_intresult = funinf - t;
else;
exp3_intresult = funinf;
end;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',xvalue); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [n_data, x, fx]=exp3_int_values( n_data, x, fx );
%*******************************************************************************
%
%! EXP3_INT_VALUES returns some values of the EXP3 integral function.
%
%  Discussion:
%
%    The function is defined by:
%
%      EXP3_INT(x) = Integral ( 0 <= t <= x ) exp ( -t^3 ) dt
%
%    The data was reported by McLeod.
%
%  Modified:
%
%    28 August 2004
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Allan McLeod,
%    Algorithm 757, MISCFUN: A software package to compute uncommon
%      special functions,
%    ACM Transactions on Mathematical Software,
%    Volume 22, Number 3, September 1996, pages 288-301.
%
%  Parameters:
%
%    Input/output, integer N_DATA.  The user sets N_DATA to 0 before the
%    first call.  On each call, the routine increments N_DATA by 1, and
%    returns the corresponding data; when there is no more data, the
%    output value of N_DATA will be 0 again.
%
%    Output, real ( kind = 8 ) X, the argument of the function.
%
%    Output, real ( kind = 8 ) FX, the value of the function.
%
%
persistent fx_vec n_max x_vec ; 

if isempty(n_max), n_max = 20; end;
%
if isempty(fx_vec), fx_vec([1:n_max]) =[real(  0.19531249963620212007d-02),real(  0.78124990686775522671d-02),real(  0.31249761583499728667d-01),real(  0.12493899888803079984d+00),real(  0.48491714311363971332d+00),real(  0.80751118213967145286d+00),real(  0.86889265412623270696d+00),real(  0.88861722235357162648d+00),real(  0.89286018500218176869d+00),real(  0.89295351429387631138d+00),real(  0.89297479112737843939d+00),real(  0.89297880579798112220d+00),real(  0.89297950317496621294d+00),real(  0.89297951152951902903d+00),real(  0.89297951156918122102d+00),real(  0.89297951156924734716d+00),real(  0.89297951156924917298d+00),real(  0.89297951156924921121d+00),real(  0.89297951156924921122d+00),real(  0.89297951156924921122d+00) ]; end;
if isempty(x_vec), x_vec([1:n_max]) =[real(  0.0019531250d+00),real(  0.0078125000d+00),real(  0.0312500000d+00),real(  0.1250000000d+00),real(  0.5000000000d+00),real(  1.0000000000d+00),real(  1.2500000000d+00),real(  1.5000000000d+00),real(  1.8750000000d+00),real(  2.0000000000d+00),real(  2.1250000000d+00),real(  2.2500000000d+00),real(  2.5000000000d+00),real(  2.7500000000d+00),real(  3.0000000000d+00),real(  3.1250000000d+00),real(  3.2500000000d+00),real(  3.5000000000d+00),real(  3.7500000000d+00),real(  4.0000000000d+00) ]; end;
%
if( n_data < 0 )
n_data = 0;
end;
n_data = fix(n_data + 1);
if( n_max < n_data )
n_data = 0;
x = 0.0d+00;
fx = 0.0d+00;
else;
x  = x_vec(n_data);
fx = fx_vec(n_data);
end;
return;
end
function [goodwinresult, xvalue ]=goodwin( xvalue );
%*******************************************************************************
%
%! GOODWIN calculates the integral of exp(-t^2/(t+x)).
%
%  Discussion:
%
%    The function is defined by:
%
%      GOODWIN(x) = Integral ( 0 <= t < infinity ) exp ( -t^2 ) / ( t + x ) dt
%
%    The code uses Chebyshev expansions whose coefficients are
%    given to 20 decimal places.
%
%    This subroutine is set up to work on IEEE machines.
%
%  Modified:
%
%    29 August 2004
%
%  Author:
%
%    Allan McLeod,
%    Department of Mathematics and Statistics,
%    Paisley University, High Street, Paisley, Scotland, PA12BE
%    macl_ms0@paisley.ac.uk
%
%  Reference:
%
%    Allan Mcleod,
%    Algorithm 757, MISCFUN: A software package to compute uncommon
%      special functions,
%    ACM Transactions on Mathematical Software,
%    Volume 22, Number 3, September 1996, pages 288-301.
%
%  Parameters:
%
%    Input, real ( kind = 8 ) XVALUE, the argument of the function.
%
%    Output, real ( kind = 8 ) GOODWIN, the value of the function.
%
%
goodwinresult=[];
persistent agost agosta firstCall fval gamby2 goodwin half nterm1 nterm2 one rtpib2 six t two x xhigh xlow zero ; if isempty(firstCall),firstCall=1;end; 

if isempty(goodwinresult), goodwinresult=0; end;
if isempty(half), half = 0.5d+00; end;
if isempty(nterm1), nterm1 = 26; end;
if isempty(nterm2), nterm2 = 20; end;
if isempty(one), one = real( 1.0); end;
if isempty(six), six = real( 6.0); end;
if isempty(two), two = 2.0d+00; end;
if isempty(x), x=0; end;
if isempty(zero), zero = 0.0d+00; end;
if isempty(agost), agost=zeros(1,28+1); end;
if isempty(agosta), agosta=zeros(1,23+1); end;
if isempty(fval), fval=0; end;
if isempty(gamby2), gamby2=0; end;
if isempty(rtpib2), rtpib2=0; end;
if isempty(t), t=0; end;
if isempty(xhigh), xhigh=0; end;
if isempty(xlow), xlow=0; end;
if firstCall,   gamby2=[0.28860783245076643030d0];  end;
if firstCall,   rtpib2=[0.88622692545275801365d0];  end;
if firstCall,   agost(0+1)=[0.63106560560398446247d0];  end;
if firstCall,   agost(1+1)=[0.25051737793216708827d0];  end;
if firstCall,   agost(2+1)=[-0.28466205979018940757d0];  end;
if firstCall,   agost(3+1)=[0.8761587523948623552d-1];  end;
if firstCall,   agost(4+1)=[0.682602267221252724d-2];  end;
if firstCall,   agost(5+1)=[-0.1081129544192254677d-1];  end;
if firstCall,   agost(6+1)=[0.169101244117152176d-2];  end;
if firstCall,   agost(7+1)=[0.50272984622615186d-3];  end;
if firstCall,   agost(8+1)=[-0.18576687204100084d-3];  end;
if firstCall,   agost(9+1)=[-0.428703674168474d-5];  end;
if firstCall,   agost(10+1)=[0.1009598903202905d-4];  end;
if firstCall,   agost(11+1)=[-0.86529913517382d-6];  end;
if firstCall,   agost(12+1)=[-0.34983874320734d-6];  end;
if firstCall,   agost(13+1)=[0.6483278683494d-7];  end;
if firstCall,   agost(14+1)=[0.757592498583d-8];  end;
if firstCall,   agost(15+1)=[-0.277935424362d-8];  end;
if firstCall,   agost(16+1)=[-0.4830235135d-10];  end;
if firstCall,   agost(17+1)=[0.8663221283d-10];  end;
if firstCall,   agost(18+1)=[-0.394339687d-11];  end;
if firstCall,   agost(19+1)=[-0.209529625d-11];  end;
if firstCall,   agost(20+1)=[0.21501759d-12];  end;
if firstCall,   agost(21+1)=[0.3959015d-13];  end;
if firstCall,   agost(22+1)=[-0.692279d-14];  end;
if firstCall,   agost(23+1)=[-0.54829d-15];  end;
if firstCall,   agost(24+1)=[0.17108d-15];  end;
if firstCall,   agost(25+1)=[0.376d-17];  end;
if firstCall,   agost(26+1)=[-0.349d-17];  end;
if firstCall,   agost(27+1)=[0.7d-19];  end;
if firstCall,   agost(28+1)=[0.6d-19];  end;
if firstCall,   agosta(0+1)=[1.81775467984718758767d0];  end;
if firstCall,   agosta(1+1)=[-0.9921146570744097467d-1];  end;
if firstCall,   agosta(2+1)=[-0.894058645254819243d-2];  end;
if firstCall,   agosta(3+1)=[-0.94955331277726785d-3];  end;
if firstCall,   agosta(4+1)=[-0.10971379966759665d-3];  end;
if firstCall,   agosta(5+1)=[-0.1346694539578590d-4];  end;
if firstCall,   agosta(6+1)=[-0.172749274308265d-5];  end;
if firstCall,   agosta(7+1)=[-0.22931380199498d-6];  end;
if firstCall,   agosta(8+1)=[-0.3127844178918d-7];  end;
if firstCall,   agosta(9+1)=[-0.436197973671d-8];  end;
if firstCall,   agosta(10+1)=[-0.61958464743d-9];  end;
if firstCall,   agosta(11+1)=[-0.8937991276d-10];  end;
if firstCall,   agosta(12+1)=[-0.1306511094d-10];  end;
if firstCall,   agosta(13+1)=[-0.193166876d-11];  end;
if firstCall,   agosta(14+1)=[-0.28844270d-12];  end;
if firstCall,   agosta(15+1)=[-0.4344796d-13];  end;
if firstCall,   agosta(16+1)=[-0.659518d-14];  end;
if firstCall,   agosta(17+1)=[-0.100801d-14];  end;
if firstCall,   agosta(18+1)=[-0.15502d-15];  end;
if firstCall,   agosta(19+1)=[-0.2397d-16];  end;
if firstCall,   agosta(20+1)=[-0.373d-17];  end;
if firstCall,   agosta(21+1)=[-0.58d-18];  end;
if firstCall,   agosta(22+1)=[-0.9d-19];  end;
if firstCall,   agosta(23+1)=[-0.1d-19];  end;
%
%   Machine-dependent constants (suitable for IEEE machines)
%
if firstCall,   xlow=[1.11022303d-16];  end;
if firstCall, xhigh=[1.80144d16];  end;
firstCall=0;
%
x = xvalue;
if( x <= zero )
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'GOODWIN - Fatal error!');
writef(1,['%s','\n'], '  Argument X <= 0.');
goodwinresult = zero;
elseif( x < xlow ) ;
goodwinresult = - gamby2 - log( x );
elseif( x <= two ) ;
t =( x - half ) - half;
goodwinresult = cheval( nterm1, agost, t ) - exp( -x .* x ) .* log( x );
elseif( x <= xhigh ) ;
fval = rtpib2 ./ x;
t =( six - x ) ./( two + x );
goodwinresult = fval .* cheval( nterm2, agosta, t );
else;
goodwinresult = rtpib2 ./ x;
end;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',xvalue); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [n_data, x, fx]=goodwin_values( n_data, x, fx );
%*******************************************************************************
%
%! GOODWIN_VALUES returns some values of the Goodwin and Staton function.
%
%  Discussion:
%
%    The function is defined by:
%
%      GOODWIN(x) = Integral ( 0 <= t < infinity ) exp ( -t^2 ) / ( t + x ) dt
%
%    The data was reported by McLeod.
%
%  Modified:
%
%    29 August 2004
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Allan McLeod,
%    Algorithm 757, MISCFUN: A software package to compute uncommon
%      special functions,
%    ACM Transactions on Mathematical Software,
%    Volume 22, Number 3, September 1996, pages 288-301.
%
%  Parameters:
%
%    Input/output, integer N_DATA.  The user sets N_DATA to 0 before the
%    first call.  On each call, the routine increments N_DATA by 1, and
%    returns the corresponding data; when there is no more data, the
%    output value of N_DATA will be 0 again.
%
%    Output, real ( kind = 8 ) X, the argument of the function.
%
%    Output, real ( kind = 8 ) FX, the value of the function.
%
%
persistent fx_vec n_max x_vec ; 

if isempty(n_max), n_max = 20; end;
%
if isempty(fx_vec), fx_vec([1:n_max]) =[real(  0.59531540040441651584d+01),real(  0.45769601268624494109d+01),real(  0.32288921331902217638d+01),real(  0.19746110873568719362d+01),real(  0.96356046208697728563d+00),real(  0.60513365250334458174d+00),real(  0.51305506459532198016d+00),real(  0.44598602820946133091d+00),real(  0.37344458206879749357d+00),real(  0.35433592884953063055d+00),real(  0.33712156518881920994d+00),real(  0.29436170729362979176d+00),real(  0.25193499644897222840d+00),real(  0.22028778222123939276d+00),real(  0.19575258237698917033d+00),real(  0.17616303166670699424d+00),real(  0.16015469479664778673d+00),real(  0.14096116876193391066d+00),real(  0.13554987191049066274d+00),real(  0.11751605060085098084d+00) ]; end;
if isempty(x_vec), x_vec([1:n_max]) =[real(  0.0019531250d+00),real(  0.0078125000d+00),real(  0.0312500000d+00),real(  0.1250000000d+00),real(  0.5000000000d+00),real(  1.0000000000d+00),real(  1.2500000000d+00),real(  1.5000000000d+00),real(  1.8750000000d+00),real(  2.0000000000d+00),real(  2.1250000000d+00),real(  2.5000000000d+00),real(  3.0000000000d+00),real(  3.5000000000d+00),real(  4.0000000000d+00),real(  4.5000000000d+00),real(  5.0000000000d+00),real(  5.7500000000d+00),real(  6.0000000000d+00),real(  7.0000000000d+00) ]; end;
%
if( n_data < 0 )
n_data = 0;
end;
n_data = fix(n_data + 1);
if( n_max < n_data )
n_data = 0;
x = 0.0d+00;
fx = 0.0d+00;
else;
x  = x_vec(n_data);
fx = fx_vec(n_data);
end;
return;
end
function [i0ml0result, xvalue ]=i0ml0( xvalue );
%*******************************************************************************
%
%! I0ML0 calculates the difference between the Bessel I0 and Struve L0 functions.
%
%  Discussion:
%
%    The function is defined by:
%
%      I0ML0(x) = I0(x) - L0(x)
%
%    I0(x) is the modified Bessel function of the first kind of order 0,
%    L0(x) is the modified Struve function of order 0.
%
%    The code uses Chebyshev expansions with the coefficients
%    given to an accuracy of 20D.
%
%    This subroutine is set up to work on IEEE machines.
%
%  Modified:
%
%    29 August 2004
%
%  Author:
%
%    Allan McLeod,
%    Department of Mathematics and Statistics,
%    Paisley University, High Street, Paisley, Scotland, PA12BE
%    macl_ms0@paisley.ac.uk
%
%  Reference:
%
%    Allan Mcleod,
%    Algorithm 757, MISCFUN: A software package to compute uncommon
%      special functions,
%    ACM Transactions on Mathematical Software,
%    Volume 22, Number 3, September 1996, pages 288-301.
%
%  Parameters:
%
%    Input, real ( kind = 8 ) XVALUE, the argument of the function.
%
%    Output, real ( kind = 8 ) I0ML0, the value of the function.
%
%
i0ml0result=[];
persistent ai0l0 ai0l0a atehun firstCall forty i0ml0 nterm1 nterm2 one six sixten t two88 twobpi x xhigh xlow xsq zero ; if isempty(firstCall),firstCall=1;end; 

if isempty(ai0l0), ai0l0=zeros(1,23+1); end;
if isempty(ai0l0a), ai0l0a=zeros(1,23+1); end;
if isempty(i0ml0result), i0ml0result=0; end;
if isempty(nterm1), nterm1 = 21; end;
if isempty(nterm2), nterm2 = 21; end;
if isempty(one), one = real( 1.0); end;
if isempty(six), six = real( 6.0d+00); end;
if isempty(x), x=0; end;
if isempty(zero), zero = 0.0d+00; end;
if isempty(atehun), atehun=0; end;
if isempty(forty), forty=0; end;
if isempty(sixten), sixten=0; end;
if isempty(t), t=0; end;
if isempty(twobpi), twobpi=0; end;
if isempty(two88), two88=0; end;
if isempty(xhigh), xhigh=0; end;
if isempty(xlow), xlow=0; end;
if isempty(xsq), xsq=0; end;
if firstCall,   sixten=[16.0d0];  end;
if firstCall,   forty =[40.0d0];  end;
if firstCall,   two88=[288.0d0];  end;
if firstCall, atehun=[800.0d0];  end;
if firstCall,   twobpi=[0.63661977236758134308d0];  end;
if firstCall,   ai0l0(0+1)=[0.52468736791485599138d0];  end;
if firstCall,   ai0l0(1+1)=[-0.35612460699650586196d0];  end;
if firstCall,   ai0l0(2+1)=[0.20487202864009927687d0];  end;
if firstCall,   ai0l0(3+1)=[-0.10418640520402693629d0];  end;
if firstCall,   ai0l0(4+1)=[0.4634211095548429228d-1];  end;
if firstCall,   ai0l0(5+1)=[-0.1790587192403498630d-1];  end;
if firstCall,   ai0l0(6+1)=[0.597968695481143177d-2];  end;
if firstCall,   ai0l0(7+1)=[-0.171777547693565429d-2];  end;
if firstCall,   ai0l0(8+1)=[0.42204654469171422d-3];  end;
if firstCall,   ai0l0(9+1)=[-0.8796178522094125d-4];  end;
if firstCall,   ai0l0(10+1)=[0.1535434234869223d-4];  end;
if firstCall,   ai0l0(11+1)=[-0.219780769584743d-5];  end;
if firstCall,   ai0l0(12+1)=[0.24820683936666d-6];  end;
if firstCall,   ai0l0(13+1)=[-0.2032706035607d-7];  end;
if firstCall,   ai0l0(14+1)=[0.90984198421d-9];  end;
if firstCall,   ai0l0(15+1)=[0.2561793929d-10];  end;
if firstCall,   ai0l0(16+1)=[-0.710609790d-11];  end;
if firstCall,   ai0l0(17+1)=[0.32716960d-12];  end;
if firstCall,   ai0l0(18+1)=[0.2300215d-13];  end;
if firstCall,   ai0l0(19+1)=[-0.292109d-14];  end;
if firstCall,   ai0l0(20+1)=[-0.3566d-16];  end;
if firstCall,   ai0l0(21+1)=[0.1832d-16];  end;
if firstCall,   ai0l0(22+1)=[-0.10d-18];  end;
if firstCall,   ai0l0(23+1)=[-0.11d-18];  end;
if firstCall,   ai0l0a(0+1)=[2.00326510241160643125d0];  end;
if firstCall,   ai0l0a(1+1)=[0.195206851576492081d-2];  end;
if firstCall,   ai0l0a(2+1)=[0.38239523569908328d-3];  end;
if firstCall,   ai0l0a(3+1)=[0.7534280817054436d-4];  end;
if firstCall,   ai0l0a(4+1)=[0.1495957655897078d-4];  end;
if firstCall,   ai0l0a(5+1)=[0.299940531210557d-5];  end;
if firstCall,   ai0l0a(6+1)=[0.60769604822459d-6];  end;
if firstCall,   ai0l0a(7+1)=[0.12399495544506d-6];  end;
if firstCall,   ai0l0a(8+1)=[0.2523262552649d-7];  end;
if firstCall,   ai0l0a(9+1)=[0.504634857332d-8];  end;
if firstCall,   ai0l0a(10+1)=[0.97913236230d-9];  end;
if firstCall,   ai0l0a(11+1)=[0.18389115241d-9];  end;
if firstCall,   ai0l0a(12+1)=[0.3376309278d-10];  end;
if firstCall,   ai0l0a(13+1)=[0.611179703d-11];  end;
if firstCall,   ai0l0a(14+1)=[0.108472972d-11];  end;
if firstCall,   ai0l0a(15+1)=[0.18861271d-12];  end;
if firstCall,   ai0l0a(16+1)=[0.3280345d-13];  end;
if firstCall,   ai0l0a(17+1)=[0.565647d-14];  end;
if firstCall,   ai0l0a(18+1)=[0.93300d-15];  end;
if firstCall,   ai0l0a(19+1)=[0.15881d-15];  end;
if firstCall,   ai0l0a(20+1)=[0.2791d-16];  end;
if firstCall,   ai0l0a(21+1)=[0.389d-17];  end;
if firstCall,   ai0l0a(22+1)=[0.70d-18];  end;
if firstCall,   ai0l0a(23+1)=[0.16d-18];  end;
%
%   MACHINE-DEPENDENT CONSTANTS (suitable for IEEE-arithmetic machines)
%
if firstCall,   xlow=[1.11022303d-16];  end;
if firstCall, xhigh=[1.8981253d9];  end;
firstCall=0;
%
x = xvalue;
if( x < zero )
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'I0ML0 - Fatal error!');
writef(1,['%s','\n'], '  Argument X < 0.');
i0ml0result = zero;
elseif( x < xlow ) ;
i0ml0result = one;
elseif( x <= sixten ) ;
t =( six .* x - forty ) ./( x + forty );
[i0ml0result , nterm1, ai0l0, t ]=cheval( nterm1, ai0l0, t );
elseif( x <= xhigh ) ;
xsq = x .* x;
t =( atehun - xsq ) ./( two88 + xsq );
i0ml0result = cheval( nterm2, ai0l0a, t ) .* twobpi ./ x;
else;
i0ml0result = twobpi ./ x;
end;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',xvalue); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [n_data, x, fx]=i0ml0_values( n_data, x, fx );
%*******************************************************************************
%
%! I0ML0_VALUES returns some values of the I0ML0 function.
%
%  Discussion:
%
%    The function is defined by:
%
%      I0ML0(x) = I0(x) - L0(x)
%
%    I0(x) is the modified Bessel function of the first kind of order 0,
%    L0(x) is the modified Struve function of order 0.
%
%    The data was reported by McLeod.
%
%  Modified:
%
%    30 August 2004
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Allan McLeod,
%    Algorithm 757, MISCFUN: A software package to compute uncommon
%      special functions,
%    ACM Transactions on Mathematical Software,
%    Volume 22, Number 3, September 1996, pages 288-301.
%
%  Parameters:
%
%    Input/output, integer N_DATA.  The user sets N_DATA to 0 before the
%    first call.  On each call, the routine increments N_DATA by 1, and
%    returns the corresponding data; when there is no more data, the
%    output value of N_DATA will be 0 again.
%
%    Output, real ( kind = 8 ) X, the argument of the function.
%
%    Output, real ( kind = 8 ) FX, the value of the function.
%
%
persistent fx_vec n_max x_vec ; 

if isempty(n_max), n_max = 20; end;
%
if isempty(fx_vec), fx_vec([1:n_max]) =[real( 0.99875755515461749793d+00),real( 0.99011358230706643807d+00),real( 0.92419435310023947018d+00),real( 0.73624267134714273902d+00),real( 0.55582269181411744686d+00),real( 0.34215154434462160628d+00),real( 0.17087174888774706539d+00),real( 0.81081008709219208918d-01),real( 0.53449421441089580702d-01),real( 0.39950321008923244846d-01),real( 0.39330637437584921392d-01),real( 0.37582274342808670750d-01),real( 0.31912486554480390343d-01),real( 0.25506146883504738403d-01),real( 0.21244480317825292412d-01),real( 0.15925498348551684335d-01),real( 0.12737506927242585015d-01),real( 0.84897750814784916847d-02),real( 0.63668349178454469153d-02),real( 0.50932843163122551114d-02) ]; end;
if isempty(x_vec), x_vec([1:n_max]) =[real(   0.0019531250d+00),real(   0.0156250000d+00),real(   0.1250000000d+00),real(   0.5000000000d+00),real(   1.0000000000d+00),real(   2.0000000000d+00),real(   4.0000000000d+00),real(   8.0000000000d+00),real(  12.0000000000d+00),real(  16.0000000000d+00),real(  16.2500000000d+00),real(  17.0000000000d+00),real(  20.0000000000d+00),real(  25.0000000000d+00),real(  30.0000000000d+00),real(  40.0000000000d+00),real(  50.0000000000d+00),real(  75.0000000000d+00),real( 100.0000000000d+00),real( 125.0000000000d+00) ]; end;
%
if( n_data < 0 )
n_data = 0;
end;
n_data = fix(n_data + 1);
if( n_max < n_data )
n_data = 0;
x = 0.0d+00;
fx = 0.0d+00;
else;
x  = x_vec(n_data);
fx = fx_vec(n_data);
end;
return;
end
function [i1ml1result, xvalue ]=i1ml1( xvalue );
%*******************************************************************************
%
%! I1ML1 calculates the difference between the Bessel I1 and Struve L1 functions.
%
%  Discussion:
%
%    The function is defined by:
%
%      I1ML1(x) = I1(x) - L1(x)
%
%    I1(x) is the modified Bessel function of the first kind of order 1,
%    L1(x) is the modified Struve function of order 1.
%
%    The code uses Chebyshev expansions with the coefficients
%    given to an accuracy of 20D.
%
%    This subroutine is set up to work on IEEE machines.
%
%  Modified:
%
%    29 August 2004
%
%  Author:
%
%    Allan McLeod,
%    Department of Mathematics and Statistics,
%    Paisley University, High Street, Paisley, Scotland, PA12BE
%    macl_ms0@paisley.ac.uk
%
%  Reference:
%
%    Allan Mcleod,
%    Algorithm 757, MISCFUN: A software package to compute uncommon
%      special functions,
%    ACM Transactions on Mathematical Software,
%    Volume 22, Number 3, September 1996, pages 288-301.
%
%  Parameters:
%
%    Input, real ( kind = 8 ) XVALUE, the argument of the function.
%    0 <= XVALUE is required.
%
%    Output, real ( kind = 8 ) I1ML1, the value of the function.
%
%
i1ml1result=[];
persistent ai1l1 ai1l1a atehun firstCall forty i1ml1 nterm1 nterm2 one six sixten t two two88 twobpi x xhigh xlow xsq zero ; if isempty(firstCall),firstCall=1;end; 

if isempty(i1ml1result), i1ml1result=0; end;
if isempty(nterm1), nterm1 = 20; end;
if isempty(nterm2), nterm2 = 22; end;
if isempty(one), one = real( 1.0); end;
if isempty(six), six = real( 6.0d+00); end;
if isempty(two), two = 2.0d+00; end;
if isempty(x), x=0; end;
if isempty(zero), zero = 0.0d+00; end;
if isempty(ai1l1), ai1l1=zeros(1,23+1); end;
if isempty(ai1l1a), ai1l1a=zeros(1,25+1); end;
if isempty(atehun), atehun=0; end;
if isempty(forty), forty=0; end;
if isempty(sixten), sixten=0; end;
if isempty(t), t=0; end;
if isempty(twobpi), twobpi=0; end;
if isempty(two88), two88=0; end;
if isempty(xhigh), xhigh=0; end;
if isempty(xlow), xlow=0; end;
if isempty(xsq), xsq=0; end;
if firstCall,   sixten=[16.0d0];  end;
if firstCall, forty=[40.0d0];  end;
if firstCall,   two88=[288.0d0];  end;
if firstCall, atehun=[800.0d0];  end;
if firstCall,   twobpi=[0.63661977236758134308d0];  end;
if firstCall,   ai1l1(0+1)=[0.67536369062350576137d0];  end;
if firstCall,   ai1l1(1+1)=[-0.38134971097266559040d0];  end;
if firstCall,   ai1l1(2+1)=[0.17452170775133943559d0];  end;
if firstCall,   ai1l1(3+1)=[-0.7062105887235025061d-1];  end;
if firstCall,   ai1l1(4+1)=[0.2517341413558803702d-1];  end;
if firstCall,   ai1l1(5+1)=[-0.787098561606423321d-2];  end;
if firstCall,   ai1l1(6+1)=[0.214814368651922006d-2];  end;
if firstCall,   ai1l1(7+1)=[-0.50862199717906236d-3];  end;
if firstCall,   ai1l1(8+1)=[0.10362608280442330d-3];  end;
if firstCall,   ai1l1(9+1)=[-0.1795447212057247d-4];  end;
if firstCall,   ai1l1(10+1)=[0.259788274515414d-5];  end;
if firstCall,   ai1l1(11+1)=[-0.30442406324667d-6];  end;
if firstCall,   ai1l1(12+1)=[0.2720239894766d-7];  end;
if firstCall,   ai1l1(13+1)=[-0.158126144190d-8];  end;
if firstCall,   ai1l1(14+1)=[0.1816209172d-10];  end;
if firstCall,   ai1l1(15+1)=[0.647967659d-11];  end;
if firstCall,   ai1l1(16+1)=[-0.54113290d-12];  end;
if firstCall,   ai1l1(17+1)=[-0.308311d-14];  end;
if firstCall,   ai1l1(18+1)=[0.305638d-14];  end;
if firstCall,   ai1l1(19+1)=[-0.9717d-16];  end;
if firstCall,   ai1l1(20+1)=[-0.1422d-16];  end;
if firstCall,   ai1l1(21+1)=[0.84d-18];  end;
if firstCall,   ai1l1(22+1)=[0.7d-19];  end;
if firstCall,   ai1l1(23+1)=[-0.1d-19];  end;
if firstCall,   ai1l1a(0+1)=[1.99679361896789136501d0];  end;
if firstCall,   ai1l1a(1+1)=[-0.190663261409686132d-2];  end;
if firstCall,   ai1l1a(2+1)=[-0.36094622410174481d-3];  end;
if firstCall,   ai1l1a(3+1)=[-0.6841847304599820d-4];  end;
if firstCall,   ai1l1a(4+1)=[-0.1299008228509426d-4];  end;
if firstCall,   ai1l1a(5+1)=[-0.247152188705765d-5];  end;
if firstCall,   ai1l1a(6+1)=[-0.47147839691972d-6];  end;
if firstCall,   ai1l1a(7+1)=[-0.9020819982592d-7];  end;
if firstCall,   ai1l1a(8+1)=[-0.1730458637504d-7];  end;
if firstCall,   ai1l1a(9+1)=[-0.332323670159d-8];  end;
if firstCall,   ai1l1a(10+1)=[-0.63736421735d-9];  end;
if firstCall,   ai1l1a(11+1)=[-0.12180239756d-9];  end;
if firstCall,   ai1l1a(12+1)=[-0.2317346832d-10];  end;
if firstCall,   ai1l1a(13+1)=[-0.439068833d-11];  end;
if firstCall,   ai1l1a(14+1)=[-0.82847110d-12];  end;
if firstCall,   ai1l1a(15+1)=[-0.15562249d-12];  end;
if firstCall,   ai1l1a(16+1)=[-0.2913112d-13];  end;
if firstCall,   ai1l1a(17+1)=[-0.543965d-14];  end;
if firstCall,   ai1l1a(18+1)=[-0.101177d-14];  end;
if firstCall,   ai1l1a(19+1)=[-0.18767d-15];  end;
if firstCall,   ai1l1a(20+1)=[-0.3484d-16];  end;
if firstCall,   ai1l1a(21+1)=[-0.643d-17];  end;
if firstCall,   ai1l1a(22+1)=[-0.118d-17];  end;
if firstCall,   ai1l1a(23+1)=[-0.22d-18];  end;
if firstCall,   ai1l1a(24+1)=[-0.4d-19];  end;
if firstCall,   ai1l1a(25+1)=[-0.1d-19];  end;
%
%   MACHINE-DEPENDENT CONSTANTS (suitable for IEEE machines)
%
if firstCall,   xlow=[2.22044605d-16];  end;
if firstCall, xhigh=[1.8981253d9];  end;
firstCall=0;
%
x = xvalue;
if( x < zero )
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'I1ML1 - Fatal error!');
writef(1,['%s','\n'], '  Argument X < 0.');
i1ml1result = zero;
elseif( x < xlow ) ;
i1ml1result = x ./ two;
elseif( x <= sixten ) ;
t =( six .* x - forty ) ./( x + forty );
i1ml1result = cheval( nterm1, ai1l1, t ) .* x ./ two;
elseif( x <= xhigh ) ;
xsq = x .* x;
t =( atehun - xsq ) ./( two88 + xsq );
i1ml1result = cheval( nterm2, ai1l1a, t ) .* twobpi;
else;
i1ml1result = twobpi;
end;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',xvalue); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [n_data, x, fx]=i1ml1_values( n_data, x, fx );
%*******************************************************************************
%
%! I1ML1_VALUES returns some values of the I1ML1 function.
%
%  Discussion:
%
%    The function is defined by:
%
%      I1ML1(x) = I1(x) - L1(x)
%
%    I1(x) is the modified Bessel function of the first kind of order 1,
%    L1(x) is the modified Struve function of order 1.
%
%    The data was reported by McLeod.
%
%  Modified:
%
%    30 August 2004
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Allan McLeod,
%    Algorithm 757, MISCFUN: A software package to compute uncommon
%      special functions,
%    ACM Transactions on Mathematical Software,
%    Volume 22, Number 3, September 1996, pages 288-301.
%
%  Parameters:
%
%    Input/output, integer N_DATA.  The user sets N_DATA to 0 before the
%    first call.  On each call, the routine increments N_DATA by 1, and
%    returns the corresponding data; when there is no more data, the
%    output value of N_DATA will be 0 again.
%
%    Output, real ( kind = 8 ) X, the argument of the function.
%
%    Output, real ( kind = 8 ) FX, the value of the function.
%
%
persistent fx_vec n_max x_vec ; 

if isempty(n_max), n_max = 20; end;
%
if isempty(fx_vec), fx_vec([1:n_max]) =[real( 0.97575346155386267134d-03),real( 0.77609293280609272733d-02),real( 0.59302966404545373770d-01),real( 0.20395212276737365307d+00),real( 0.33839472293667639038d+00),real( 0.48787706726961324579d+00),real( 0.59018734196576517506d+00),real( 0.62604539530312149476d+00),real( 0.63209315274909764698d+00),real( 0.63410179313235359215d+00),real( 0.63417966797578128188d+00),real( 0.63439268632392089434d+00),real( 0.63501579073257770690d+00),real( 0.63559616677359459337d+00),real( 0.63591001826697110312d+00),real( 0.63622113181751073643d+00),real( 0.63636481702133606597d+00),real( 0.63650653499619902120d+00),real( 0.63655609126300261851d+00),real( 0.63657902087183929223d+00) ]; end;
if isempty(x_vec), x_vec([1:n_max]) =[real(   0.0019531250d+00),real(   0.0156250000d+00),real(   0.1250000000d+00),real(   0.5000000000d+00),real(   1.0000000000d+00),real(   2.0000000000d+00),real(   4.0000000000d+00),real(   8.0000000000d+00),real(  12.0000000000d+00),real(  16.0000000000d+00),real(  16.2500000000d+00),real(  17.0000000000d+00),real(  20.0000000000d+00),real(  25.0000000000d+00),real(  30.0000000000d+00),real(  40.0000000000d+00),real(  50.0000000000d+00),real(  75.0000000000d+00),real( 100.0000000000d+00),real( 125.0000000000d+00) ]; end;
%
if( n_data < 0 )
n_data = 0;
end;
n_data = fix(n_data + 1);
if( n_max < n_data )
n_data = 0;
x = 0.0d+00;
fx = 0.0d+00;
else;
x  = x_vec(n_data);
fx = fx_vec(n_data);
end;
return;
end
function [lobachevskyresult, xvalue ]=lobachevsky( xvalue );
%*******************************************************************************
%
%! LOBACHEVSKY calculates the Lobachevsky function.
%
%  Discussion:
%
%    The function is defined by:
%
%      LOBACHEVSKY(x) = Integral ( 0 <= t <= x ) -ln ( abs ( cos ( t ) ) dt
%
%    The code uses Chebyshev expansions whose coefficients are given
%    to 20 decimal places.
%
%    This subroutine is set up to work on IEEE machines.
%
%  Modified:
%
%    07 August 2004
%
%  Author:
%
%    Allan McLeod,
%    Department of Mathematics and Statistics,
%    Paisley University, High Street, Paisley, Scotland, PA12BE
%    macl_ms0@paisley.ac.uk
%
%  Reference:
%
%    Allan Mcleod,
%    Algorithm 757, MISCFUN: A software package to compute uncommon
%      special functions,
%    ACM Transactions on Mathematical Software,
%    Volume 22, Number 3, September 1996, pages 288-301.
%
%  Parameters:
%
%    Input, real ( kind = 8 ) XVALUE, the argument of the function.
%
%    Output, real ( kind = 8 ) LOBACHEVSKY, the value of the function.
%
%
lobachevskyresult=[];
persistent arlob1 arlob2 firstCall fval fval1 half indpi2 indsgn lbpb21 lbpb22 lobachevsky lobpi1 lobpi2 lobpia lobpib npi nterm1 nterm2 one pi pi1 pi11 pi12 pi2 piby2 piby21 piby22 piby4 six t tcon two x xcub xhigh xlow1 xlow2 xlow3 xr zero ; if isempty(firstCall),firstCall=1;end; 

if isempty(half), half = 0.5d+00; end;
if isempty(indpi2), indpi2=0; end;
if isempty(indsgn), indsgn=0; end;
if isempty(npi), npi=0; end;
if isempty(nterm1), nterm1 = 13; end;
if isempty(nterm2), nterm2 = 9; end;
if isempty(lobachevskyresult), lobachevskyresult=0; end;
if isempty(one), one = real( 1.0); end;
if isempty(six), six = real( 6.0d+00); end;
if isempty(two), two = 2.0d+00; end;
if isempty(x), x=0; end;
if isempty(zero), zero = 0.0d+00; end;
if isempty(arlob1), arlob1=zeros(1,15+1); end;
if isempty(arlob2), arlob2=zeros(1,10+1); end;
if isempty(fval), fval=0; end;
if isempty(fval1), fval1=0; end;
if isempty(lbpb21), lbpb21=0; end;
if isempty(lbpb22), lbpb22=0; end;
if isempty(lobpia), lobpia=0; end;
if isempty(lobpib), lobpib=0; end;
if isempty(lobpi1), lobpi1=0; end;
if isempty(lobpi2), lobpi2=0; end;
if isempty(pi), pi=0; end;
if isempty(piby2), piby2=0; end;
if isempty(piby21), piby21=0; end;
if isempty(piby22), piby22=0; end;
if isempty(piby4), piby4=0; end;
if isempty(pi1), pi1=0; end;
if isempty(pi11), pi11=0; end;
if isempty(pi12), pi12=0; end;
if isempty(pi2), pi2=0; end;
if isempty(t), t=0; end;
if isempty(tcon), tcon=0; end;
if isempty(xcub), xcub=0; end;
if isempty(xhigh), xhigh=0; end;
if isempty(xlow1), xlow1=0; end;
if isempty(xlow2), xlow2=0; end;
if isempty(xlow3), xlow3=0; end;
if isempty(xr), xr=0; end;
if firstCall,   lobpia=[1115.0d0];  end;
if firstCall, lobpib=[512.0d0];  end;
if firstCall,   lobpi2=[-1.48284696397869499311d-4];  end;
if firstCall,   lbpb22=[-7.41423481989347496556d-5];  end;
if firstCall,   pi11=[201.0d0];  end;
if firstCall, pi12=[64.0d0];  end;
if firstCall,   pi2=[9.67653589793238462643d-4];  end;
if firstCall,   piby22=[4.83826794896619231322d-4];  end;
if firstCall,   tcon=[3.24227787655480868620d0];  end;
if firstCall,   arlob1=[0.34464884953481300507d0,0.584198357190277669d-2,0.19175029694600330d-3,0.787251606456769d-5,0.36507477415804d-6,0.1830287272680d-7,0.96890333005d-9,0.5339055444d-10,0.303408025d-11,0.17667875d-12,0.1049393d-13,0.63359d-15,0.3878d-16,0.240d-17,0.15d-18,0.1d-19];  end;
if firstCall,   arlob2=[2.03459418036132851087d0,0.1735185882027407681d-1,0.5516280426090521d-4,0.39781646276598d-6,0.369018028918d-8,0.3880409214d-10,0.44069698d-12,0.527674d-14,0.6568d-16,0.84d-18,0.1d-19];  end;
%
%   Machine-dependent constants (suitable for IEEE machines)
%
if firstCall,   xlow1=[5.11091385d-103];  end;
if firstCall, xlow2=[4.71216091d-8];  end;
if firstCall,   xlow3=[6.32202727d-8];  end;
if firstCall, xhigh=[4.5035996d15];  end;
firstCall=0;
%
x = abs( xvalue );
indsgn = 1;
if( xvalue < zero )
indsgn = -1;
end;
if( xhigh < x )
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'LOBACHEVSKY - Fatal error!');
writef(1,['%s','\n'], '  Argument magnitude too large.');
lobachevskyresult = zero;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',xvalue); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end;
%
%  Reduce argument to [0,pi]
%
pi1 = pi11 ./ pi12;
pi = pi1 + pi2;
piby2 = pi ./ two;
piby21 = pi1 ./ two;
piby4 = piby2 ./ two;
npi = fix(fix( x ./ pi ));
xr =( x - npi .* pi1 ) - npi .* pi2;
%
%  Reduce argument to [0,pi/2]
%
indpi2 = 0;
if( piby2 < xr )
indpi2 = 1;
xr =( pi1 - xr ) + pi2;
end;
%
%  Code for argument in [0,pi/4]
%
if( xr <= piby4 )
if( xr < xlow1 )
fval = zero;
else;
xcub = xr .* xr .* xr;
if( xr < xlow2 )
fval = xcub ./ six;
else;
t =( tcon .* xr .* xr - half ) - half;
fval = xcub .* cheval( nterm1, arlob1, t );
end;
end;
else;
%
%  Code for argument in [pi/4,pi/2]
%
xr =( piby21 - xr ) + piby22;
if( xr == zero )
fval1 = zero;
else;
if( xr < xlow3 )
fval1 = xr .*( one - log( xr ) );
else;
t =( tcon .* xr .* xr - half ) - half;
fval1 = xr .*( cheval( nterm2, arlob2, t ) - log( xr ) );
end;
end;
lbpb21 = lobpia ./( lobpib + lobpib );
fval =( lbpb21 - fval1 ) + lbpb22;
end;
lobpi1 = lobpia ./ lobpib;
%
%  Compute value for argument in [pi/2,pi]
%
if( indpi2 == 1 )
fval =( lobpi1 - fval ) + lobpi2;
end;
if( npi <= 0 )
lobachevskyresult = fval;
else;
lobachevskyresult =( fval + npi .* lobpi2 ) + npi .* lobpi1;
end;
if( indsgn == -1 )
lobachevskyresult = -lobachevskyresult;
end;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',xvalue); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [n_data, x, fx]=lobachevsky_values( n_data, x, fx );
%*******************************************************************************
%
%! LOBACHEVSKY_VALUES returns some values of the Lobachevsky function.
%
%  Discussion:
%
%    The function is defined by:
%
%      LOBACHEVSKY(x) = Integral ( 0 <= t <= x ) -ln ( abs ( cos ( t ) ) dt
%
%    The data was reported by McLeod.
%
%  Modified:
%
%    31 August 2004
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Allan McLeod,
%    Algorithm 757, MISCFUN: A software package to compute uncommon
%      special functions,
%    ACM Transactions on Mathematical Software,
%    Volume 22, Number 3, September 1996, pages 288-301.
%
%  Parameters:
%
%    Input/output, integer N_DATA.  The user sets N_DATA to 0 before the
%    first call.  On each call, the routine increments N_DATA by 1, and
%    returns the corresponding data; when there is no more data, the
%    output value of N_DATA will be 0 again.
%
%    Output, real ( kind = 8 ) X, the argument of the function.
%
%    Output, real ( kind = 8 ) FX, the value of the function.
%
%
persistent fx_vec n_max x_vec ; 

if isempty(n_max), n_max = 20; end;
%
if isempty(fx_vec), fx_vec([1:n_max]) =[real( 0.12417639065161393857d-08),real( 0.79473344770001088225d-07),real( 0.50867598186208834198d-05),real( 0.32603097901207200319d-03),real( 0.21380536815408214419d-01),real( 0.18753816902083824050d+00),real( 0.83051199971883645115d+00),real( 0.18854362426679034904d+01),real( 0.21315988986516411053d+01),real( 0.21771120185613427221d+01),real( 0.22921027921896650849d+01),real( 0.39137195028784495586d+01),real( 0.43513563983836427904d+01),real( 0.44200644968478185898d+01),real( 0.65656013133623829156d+01),real( 0.10825504661504599479d+02),real( 0.13365512855474227325d+02),real( 0.21131002685639959927d+02),real( 0.34838236589449117389d+02),real( 0.69657062437837394278d+02) ]; end;
if isempty(x_vec), x_vec([1:n_max]) =[real(   0.0019531250d+00),real(   0.0078125000d+00),real(   0.0312500000d+00),real(   0.1250000000d+00),real(   0.5000000000d+00),real(   1.0000000000d+00),real(   1.5000000000d+00),real(   2.0000000000d+00),real(   2.5000000000d+00),real(   3.0000000000d+00),real(   4.0000000000d+00),real(   5.0000000000d+00),real(   6.0000000000d+00),real(   7.0000000000d+00),real(  10.0000000000d+00),real(  15.0000000000d+00),real(  20.0000000000d+00),real(  30.0000000000d+00),real(  50.0000000000d+00),real( 100.0000000000d+00) ]; end;
%
if( n_data < 0 )
n_data = 0;
end;
n_data = fix(n_data + 1);
if( n_max < n_data )
n_data = 0;
x = 0.0d+00;
fx = 0.0d+00;
else;
x  = x_vec(n_data);
fx = fx_vec(n_data);
end;
return;
end
function [stromgenresult, xvalue ]=stromgen( xvalue );
%*******************************************************************************
%
%! STROMGEN calculates Stromgen's integral.
%
%  Discussion:
%
%    The function is defined by:
%
%      STROMGEN(X) = Integral ( 0 <= t <= X ) t^7 * exp(2*t) / (exp(t)-1)^3 dt
%
%    The code uses a Chebyshev series, the coefficients of which are
%    given to an accuracy of 20 decimal places.
%
%    This subroutine is set up to work on IEEE machines.
%
%  Modified:
%
%    07 August 2004
%
%  Author:
%
%    Allan McLeod,
%    Department of Mathematics and Statistics,
%    Paisley University, High Street, Paisley, Scotland, PA12BE
%    macl_ms0@paisley.ac.uk
%
%  Reference:
%
%    Allan Mcleod,
%    Algorithm 757, MISCFUN: A software package to compute uncommon
%      special functions,
%    ACM Transactions on Mathematical Software,
%    Volume 22, Number 3, September 1996, pages 288-301.
%
%  Parameters:
%
%    Input, real ( kind = 8 ) XVALUE, the argument of the function.
%
%    Output, real ( kind = 8 ) STROMGEN, the value of the function.
%
%
stromgenresult=[];
persistent astrom epngln epsln f15bp4 firstCall four half k1 k2 nterms numexp one one5ln pi4b3 rk seven stromgen sum2 sumexp t two valinf x xhigh xk xk1 xlow0 xlow1 zero ; if isempty(firstCall),firstCall=1;end; 

if isempty(astrom), astrom=zeros(1,26+1); end;
if isempty(epngln), epngln=0; end;
if isempty(epsln), epsln=0; end;
if isempty(f15bp4), f15bp4=0; end;
if isempty(four), four = 4.0d+00; end;
if isempty(half), half = 0.5d+00; end;
if isempty(k1), k1=0; end;
if isempty(k2), k2=0; end;
if isempty(nterms), nterms = 23; end;
if isempty(numexp), numexp=0; end;
if isempty(one), one = 1.0d+00; end;
if isempty(one5ln), one5ln=0; end;
if isempty(pi4b3), pi4b3=0; end;
if isempty(rk), rk=0; end;
if isempty(stromgenresult), stromgenresult=0; end;
if isempty(two), two = 2.0d+00; end;
if isempty(x), x=0; end;
if isempty(zero), zero = 0.0d+00; end;
if isempty(seven), seven=0; end;
if isempty(sumexp), sumexp=0; end;
if isempty(sum2), sum2=0; end;
if isempty(t), t=0; end;
if isempty(valinf), valinf=0; end;
if isempty(xhigh), xhigh=0; end;
if isempty(xk), xk=0; end;
if isempty(xk1), xk1=0; end;
if isempty(xlow0), xlow0=0; end;
if isempty(xlow1), xlow1=0; end;
if firstCall,   seven=[7.0d0];  end;
if firstCall,   one5ln=[0.4055d0];  end;
if firstCall,   f15bp4=[0.38497433455066256959d-1];  end;
if firstCall,   pi4b3=[1.29878788045336582982d2];  end;
if firstCall,   valinf=[196.51956920868988261257d0];  end;
if firstCall,   astrom(0+1)=[0.56556120872539155290d0];  end;
if firstCall,   astrom(1+1)=[0.4555731969101785525d-1];  end;
if firstCall,   astrom(2+1)=[-0.4039535875936869170d-1];  end;
if firstCall,   astrom(3+1)=[-0.133390572021486815d-2];  end;
if firstCall,   astrom(4+1)=[0.185862506250538030d-2];  end;
if firstCall,   astrom(5+1)=[-0.4685555868053659d-4];  end;
if firstCall,   astrom(6+1)=[-0.6343475643422949d-4];  end;
if firstCall,   astrom(7+1)=[0.572548708143200d-5];  end;
if firstCall,   astrom(8+1)=[0.159352812216822d-5];  end;
if firstCall,   astrom(9+1)=[-0.28884328431036d-6];  end;
if firstCall,   astrom(10+1)=[-0.2446633604801d-7];  end;
if firstCall,   astrom(11+1)=[0.1007250382374d-7];  end;
if firstCall,   astrom(12+1)=[-0.12482986104d-9];  end;
if firstCall,   astrom(13+1)=[-0.26300625283d-9];  end;
if firstCall,   astrom(14+1)=[0.2490407578d-10];  end;
if firstCall,   astrom(15+1)=[0.485454902d-11];  end;
if firstCall,   astrom(16+1)=[-0.105378913d-11];  end;
if firstCall,   astrom(17+1)=[-0.3604417d-13];  end;
if firstCall,   astrom(18+1)=[0.2992078d-13];  end;
if firstCall,   astrom(19+1)=[-0.163971d-14];  end;
if firstCall,   astrom(20+1)=[-0.61061d-15];  end;
if firstCall,   astrom(21+1)=[0.9335d-16];  end;
if firstCall,   astrom(22+1)=[0.709d-17];  end;
if firstCall,   astrom(23+1)=[-0.291d-17];  end;
if firstCall,   astrom(24+1)=[0.8d-19];  end;
if firstCall,   astrom(25+1)=[0.6d-19];  end;
if firstCall,   astrom(26+1)=[-0.1d-19];  end;
%
%  Machine-dependent constants
%
if firstCall,   xlow0=[7.80293d-62];  end;
if firstCall, xlow1=[2.22045d-16];  end;
if firstCall,   epsln=[-36.0436534d0];  end;
if firstCall, epngln=[-36.7368006d0];  end;
if firstCall,   xhigh=[3.1525197d16];  end;
firstCall=0;
%
x = xvalue;
if( x < zero )
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'STROMGEN - Fatal error!');
writef(1,['%s','\n'], '  Argument X < 0.');
stromgenresult = zero;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',xvalue); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end;
if( x < xlow0 )
stromgenresult = zero;
elseif( x < xlow1 ) ;
stromgenresult = x.^5 ./ pi4b3;
elseif( x <= four ) ;
t =(( x ./ two ) - half ) - half;
stromgenresult = x.^5 .* cheval( nterms, astrom, t ) .* f15bp4;
else;
%
%  Code for x > 4.0
%
if( xhigh < x )
sumexp = one;
else;
numexp = fix(fix( epsln ./( one5ln - x ) ) + 1);
if( 1 < numexp )
t = exp( -x );
else;
t = one;
end;
rk = zero;
for k1 = 1: numexp;
rk = rk + one;
end; k1 = fix(numexp+1);
sumexp = zero;
for k1 = 1: numexp;
sum2 = one;
xk = one ./( rk .* x );
xk1 = one;
for k2 = 1: 7;
sum2 = sum2 .* xk1 .* xk + one;
xk1 = xk1 + one;
end; k2 = fix(7+1);
sum2 = sum2 .*( rk + one ) ./ two;
sumexp = sumexp .* t + sum2;
rk = rk - one;
end; k1 = fix(numexp+1);
end;
t = seven .* log( x ) - x + log( sumexp );
if( t < epngln )
stromgenresult = valinf;
else;
stromgenresult = valinf - exp( t ) .* f15bp4;
end;
end;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',xvalue); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [n_data, x, fx]=stromgen_values( n_data, x, fx );
%*******************************************************************************
%
%! STROMGEN_VALUES returns some values of the Stromgen function.
%
%  Discussion:
%
%    The function is defined by:
%
%      STROMGEN(X) = Integral ( 0 <= t <= X ) t^7 * exp(2*t) / (exp(t)-1)^3 dt
%
%    The data was reported by McLeod.
%
%  Modified:
%
%    31 August 2004
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Allan McLeod,
%    Algorithm 757, MISCFUN: A software package to compute uncommon
%      special functions,
%    ACM Transactions on Mathematical Software,
%    Volume 22, Number 3, September 1996, pages 288-301.
%
%  Parameters:
%
%    Input/output, integer N_DATA.  The user sets N_DATA to 0 before the
%    first call.  On each call, the routine increments N_DATA by 1, and
%    returns the corresponding data; when there is no more data, the
%    output value of N_DATA will be 0 again.
%
%    Output, real ( kind = 8 ) X, the argument of the function.
%
%    Output, real ( kind = 8 ) FX, the value of the function.
%
%
persistent fx_vec n_max x_vec ; 

if isempty(n_max), n_max = 20; end;
%
if isempty(fx_vec), fx_vec([1:n_max]) =[real( 0.21901065985698662316d-15),real( 0.22481399438625244761d-12),real( 0.23245019579558857124d-09),real( 0.24719561475975007037d-06),real( 0.28992610989833245669d-03),real( 0.10698146390809715091d-01),real( 0.89707650964424730705d-01),real( 0.40049605719592888440d+00),real( 0.30504104398079096598d+01),real( 0.11367704858439426431d+02),real( 0.12960679405324786954d+02),real( 0.18548713944748505675d+02),real( 0.27866273821903121400d+02),real( 0.51963334071699323351d+02),real( 0.10861016747891228129d+03),real( 0.15378903316556621624d+03),real( 0.19302665532558721516d+03),real( 0.19636850166006541482d+03),real( 0.19651946766008214217d+03),real( 0.19651956920868316152d+03) ]; end;
if isempty(x_vec), x_vec([1:n_max]) =[real(   0.0019531250d+00),real(   0.0078125000d+00),real(   0.0312500000d+00),real(   0.1250000000d+00),real(   0.5000000000d+00),real(   1.0000000000d+00),real(   1.5000000000d+00),real(   2.0000000000d+00),real(   3.0000000000d+00),real(   4.0000000000d+00),real(   4.1250000000d+00),real(   4.5000000000d+00),real(   5.0000000000d+00),real(   6.0000000000d+00),real(   8.0000000000d+00),real(  10.0000000000d+00),real(  15.0000000000d+00),real(  20.0000000000d+00),real(  30.0000000000d+00),real(  50.0000000000d+00) ]; end;
%
if( n_data < 0 )
n_data = 0;
end;
n_data = fix(n_data + 1);
if( n_max < n_data )
n_data = 0;
x = 0.0d+00;
fx = 0.0d+00;
else;
x  = x_vec(n_data);
fx = fx_vec(n_data);
end;
return;
end
function [struve_h0result, xvalue ]=struve_h0( xvalue );
%*******************************************************************************
%
%! STRUVE_H0 calculates the Struve function of order 0.
%
%  Discussion:
%
%    The function is defined by:
%
%      HO(x) = (2/pi) Integral ( 0 <= t <= pi/2 ) sin ( x * cos ( t ) ) dt
%
%    H0 also satisfies the second-order equation
%
%      x*D(Df) + Df + x * f = 2 * x / pi
%
%    The code uses Chebyshev expansions whose coefficients are
%    given to 20D.
%
%    This subroutine is set up to work on IEEE machines.
%
%  Modified:
%
%    07 August 2004
%
%  Author:
%
%    Allan McLeod,
%    Department of Mathematics and Statistics,
%    Paisley University, High Street, Paisley, Scotland, PA12BE
%    macl_ms0@paisley.ac.uk
%
%  Reference:
%
%    Allan Mcleod,
%    Algorithm 757, MISCFUN: A software package to compute uncommon
%      special functions,
%    ACM Transactions on Mathematical Software,
%    Volume 22, Number 3, September 1996, pages 288-301.
%
%  Parameters:
%
%    Input, real ( kind = 8 ) XVALUE, the argument of the function.
%
%    Output, real ( kind = 8 ) STRUVE_H0, the value of the function.
%
%
struve_h0result=[];
persistent arrh0 arrh0a ay0asp ay0asq eight eleven firstCall h0as half indsgn nterm1 nterm2 nterm3 nterm4 one piby4 rt2bpi sixtp5 struve_h0 t thr2p5 twenty two62 twobpi x xhigh xlow xmp4 xsq y0p y0q y0val zero ; if isempty(firstCall),firstCall=1;end; 

if isempty(arrh0), arrh0=zeros(1,19+1); end;
if isempty(arrh0a), arrh0a=zeros(1,20+1); end;
if isempty(ay0asp), ay0asp=zeros(1,12+1); end;
if isempty(ay0asq), ay0asq=zeros(1,13+1); end;
if isempty(eight), eight = real( 8.0); end;
if isempty(half), half = 0.5d+00; end;
if isempty(indsgn), indsgn=0; end;
if isempty(nterm1), nterm1 = 18; end;
if isempty(nterm2), nterm2 = 18; end;
if isempty(nterm3), nterm3 = 11; end;
if isempty(nterm4), nterm4 = 11; end;
if isempty(one), one = real( 1.0); end;
if isempty(struve_h0result), struve_h0result=0; end;
if isempty(x), x=0; end;
if isempty(zero), zero = 0.0d+00; end;
if isempty(eleven), eleven=0; end;
if isempty(h0as), h0as=0; end;
if isempty(piby4), piby4=0; end;
if isempty(rt2bpi), rt2bpi=0; end;
if isempty(sixtp5), sixtp5=0; end;
if isempty(t), t=0; end;
if isempty(thr2p5), thr2p5=0; end;
if isempty(twenty), twenty=0; end;
if isempty(twobpi), twobpi=0; end;
if isempty(two62), two62=0; end;
if isempty(xhigh), xhigh=0; end;
if isempty(xlow), xlow=0; end;
if isempty(xmp4), xmp4=0; end;
if isempty(xsq), xsq=0; end;
if isempty(y0p), y0p=0; end;
if isempty(y0q), y0q=0; end;
if isempty(y0val), y0val=0; end;
if firstCall,   eleven=[11.0d0];  end;
if firstCall,   twenty =[20.0d0];  end;
if firstCall,   sixtp5=[60.5d0];  end;
if firstCall, two62=[262.0d0];  end;
if firstCall, thr2p5=[302.5d0];  end;
if firstCall,   piby4=[0.78539816339744830962d0];  end;
if firstCall,   rt2bpi=[0.79788456080286535588d0];  end;
if firstCall,   twobpi=[0.63661977236758134308d0];  end;
if firstCall,   arrh0(0+1)=[0.28696487399013225740d0];  end;
if firstCall,   arrh0(1+1)=[-0.25405332681618352305d0];  end;
if firstCall,   arrh0(2+1)=[0.20774026739323894439d0];  end;
if firstCall,   arrh0(3+1)=[-0.20364029560386585140d0];  end;
if firstCall,   arrh0(4+1)=[0.12888469086866186016d0];  end;
if firstCall,   arrh0(5+1)=[-0.4825632815622261202d-1];  end;
if firstCall,   arrh0(6+1)=[0.1168629347569001242d-1];  end;
if firstCall,   arrh0(7+1)=[-0.198118135642418416d-2];  end;
if firstCall,   arrh0(8+1)=[0.24899138512421286d-3];  end;
if firstCall,   arrh0(9+1)=[-0.2418827913785950d-4];  end;
if firstCall,   arrh0(10+1)=[0.187437547993431d-5];  end;
if firstCall,   arrh0(11+1)=[-0.11873346074362d-6];  end;
if firstCall,   arrh0(12+1)=[0.626984943346d-8];  end;
if firstCall,   arrh0(13+1)=[-0.28045546793d-9];  end;
if firstCall,   arrh0(14+1)=[0.1076941205d-10];  end;
if firstCall,   arrh0(15+1)=[-0.35904793d-12];  end;
if firstCall,   arrh0(16+1)=[0.1049447d-13];  end;
if firstCall,   arrh0(17+1)=[-0.27119d-15];  end;
if firstCall,   arrh0(18+1)=[0.624d-17];  end;
if firstCall,   arrh0(19+1)=[-0.13d-18];  end;
if firstCall,   arrh0a(0+1)=[1.99291885751992305515d0];  end;
if firstCall,   arrh0a(1+1)=[-0.384232668701456887d-2];  end;
if firstCall,   arrh0a(2+1)=[-0.32871993712353050d-3];  end;
if firstCall,   arrh0a(3+1)=[-0.2941181203703409d-4];  end;
if firstCall,   arrh0a(4+1)=[-0.267315351987066d-5];  end;
if firstCall,   arrh0a(5+1)=[-0.24681031075013d-6];  end;
if firstCall,   arrh0a(6+1)=[-0.2295014861143d-7];  end;
if firstCall,   arrh0a(7+1)=[-0.215682231833d-8];  end;
if firstCall,   arrh0a(8+1)=[-0.20303506483d-9];  end;
if firstCall,   arrh0a(9+1)=[-0.1934575509d-10];  end;
if firstCall,   arrh0a(10+1)=[-0.182773144d-11];  end;
if firstCall,   arrh0a(11+1)=[-0.17768424d-12];  end;
if firstCall,   arrh0a(12+1)=[-0.1643296d-13];  end;
if firstCall,   arrh0a(13+1)=[-0.171569d-14];  end;
if firstCall,   arrh0a(14+1)=[-0.13368d-15];  end;
if firstCall,   arrh0a(15+1)=[-0.2077d-16];  end;
if firstCall,   arrh0a(16+1)=[0.2d-19];  end;
if firstCall,   arrh0a(17+1)=[-0.55d-18];  end;
if firstCall,   arrh0a(18+1)=[0.10d-18];  end;
if firstCall,   arrh0a(19+1)=[-0.4d-19];  end;
if firstCall,   arrh0a(20+1)=[0.1d-19];  end;
if firstCall,   ay0asp=[1.99944639402398271568d0,-0.28650778647031958d-3,-0.1005072797437620d-4,-0.35835941002463d-6,-0.1287965120531d-7,-0.46609486636d-9,-0.1693769454d-10,-0.61852269d-12,-0.2261841d-13,-0.83268d-15,-0.3042d-16,-0.115d-17,-0.4d-19];  end;
if firstCall,   ay0asq=[1.99542681386828604092d0,-0.236013192867514472d-2,-0.7601538908502966d-4,-0.256108871456343d-5,-0.8750292185106d-7,-0.304304212159d-8,-0.10621428314d-9,-0.377371479d-11,-0.13213687d-12,-0.488621d-14,-0.15809d-15,-0.762d-17,-0.3d-19,-0.3d-19];  end;
%
%   MACHINE-DEPENDENT CONSTANTS (Suitable for IEEE-arithmetic machines)
%
if firstCall,   xlow=[3.1610136d-8];  end;
if firstCall, xhigh=[4.50359963d15];  end;
firstCall=0;
%
x = xvalue;
indsgn = 1;
if( x < zero )
x = -x;
indsgn = -1;
end;
if( x < xlow )
struve_h0result = twobpi .* x;
elseif( x <= eleven ) ;
t =(( x .* x ) ./ sixtp5 - half ) - half;
struve_h0result = twobpi .* x .* cheval( nterm1, arrh0, t );
elseif( x <= xhigh ) ;
xsq = x .* x;
t =( two62 - xsq ) ./( twenty + xsq );
[y0p , nterm3, ay0asp, t ]=cheval( nterm3, ay0asp, t );
y0q = cheval( nterm4, ay0asq, t ) ./( eight .* x );
xmp4 = x - piby4;
y0val = y0p .* sin( xmp4 ) - y0q .* cos( xmp4 );
y0val = y0val .* rt2bpi ./ sqrt( x );
t =( thr2p5 - xsq ) ./( sixtp5 + xsq );
h0as = twobpi .* cheval( nterm2, arrh0a, t ) ./ x;
struve_h0result = y0val + h0as;
else;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'STRUVE_H0 - Fatal error!');
writef(1,['%s','\n'], '  Argument magnitude too large.');
struve_h0result = zero;
end;
if( indsgn == -1 )
struve_h0result = -struve_h0result;
end;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',xvalue); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [n_data, x, fx]=struve_h0_values( n_data, x, fx );
%*******************************************************************************
%
%! STRUVE_H0_VALUES returns some values of the Struve H0 function.
%
%  Discussion:
%
%    The function is defined by:
%
%      HO(x) = (2/pi) * Integral ( 0 <= t <= pi/2 ) sin ( x * cos ( t ) ) dt
%
%    In Mathematica, the function can be evaluated by:
%
%      StruveH[0,x]
%
%    The data was reported by McLeod.
%
%  Modified:
%
%    01 September 2004
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Milton Abramowitz and Irene Stegun,
%    Handbook of Mathematical Functions,
%    US Department of Commerce, 1964.
%
%    Allan McLeod,
%    Algorithm 757, MISCFUN: A software package to compute uncommon
%      special functions,
%    ACM Transactions on Mathematical Software,
%    Volume 22, Number 3, September 1996, pages 288-301.
%
%    Stephen Wolfram,
%    The Mathematica Book,
%    Fourth Edition,
%    Wolfram Media / Cambridge University Press, 1999.
%
%  Parameters:
%
%    Input/output, integer N_DATA.  The user sets N_DATA to 0 before the
%    first call.  On each call, the routine increments N_DATA by 1, and
%    returns the corresponding data; when there is no more data, the
%    output value of N_DATA will be 0 again.
%
%    Output, real ( kind = 8 ) X, the argument of the function.
%
%    Output, real ( kind = 8 ) FX, the value of the function.
%
%
persistent fx_vec n_max x_vec ; 

if isempty(n_max), n_max = 20; end;
%
if isempty(fx_vec), fx_vec([1:n_max]) =[real(  0.12433974658847434366d-02),real( -0.49735582423748415045d-02),real(  0.39771469054536941564d-01),real( -0.15805246001653314198d+00),real(  0.56865662704828795099d+00),real(  0.66598399314899916605d+00),real(  0.79085884950809589255d+00),real( -0.13501457342248639716d+00),real(  0.20086479668164503137d+00),real( -0.11142097800261991552d+00),real( -0.17026804865989885869d+00),real( -0.13544931808186467594d+00),real(  0.94393698081323450897d-01),real( -0.10182482016001510271d+00),real(  0.96098421554162110012d-01),real( -0.85337674826118998952d-01),real( -0.76882290637052720045d-01),real(  0.47663833591418256339d-01),real( -0.70878751689647343204d-01),real(  0.65752908073352785368d-01) ]; end;
if isempty(x_vec), x_vec([1:n_max]) =[real(    0.0019531250d+00),real(   -0.0078125000d+00),real(    0.0625000000d+00),real(   -0.2500000000d+00),real(    1.0000000000d+00),real(    1.2500000000d+00),real(    2.0000000000d+00),real(   -4.0000000000d+00),real(    7.5000000000d+00),real(   11.0000000000d+00),real(   11.5000000000d+00),real(  -16.0000000000d+00),real(   20.0000000000d+00),real(   25.0000000000d+00),real(  -30.0000000000d+00),real(   50.0000000000d+00),real(   75.0000000000d+00),real(  -80.0000000000d+00),real(  100.0000000000d+00),real( -125.0000000000d+00) ]; end;
%
if( n_data < 0 )
n_data = 0;
end;
n_data = fix(n_data + 1);
if( n_max < n_data )
n_data = 0;
x = 0.0d+00;
fx = 0.0d+00;
else;
x  = x_vec(n_data);
fx = fx_vec(n_data);
end;
return;
end
function [struve_h1result, xvalue ]=struve_h1( xvalue );
%*******************************************************************************
%
%! STRUVE_H1 calculates the Struve function of order 1.
%
%  Discussion:
%
%    The function is defined by:
%
%      H1(x) = 2*x/pi * Integral ( 0 <= t <= pi/2 )
%        sin ( x * cos ( t ) )^2 * sin ( t ) dt
%
%    H1 also satisfies the second-order differential equation
%
%      x^2 * D^2 f  +  x * Df  +  (x^2 - 1)f  =  2x^2 / pi
%
%    The code uses Chebyshev expansions with the coefficients
%    given to 20D.
%
%    This subroutine is set up to work on IEEE machines.
%
%  Modified:
%
%    07 August 2004
%
%  Author:
%
%    Allan McLeod,
%    Department of Mathematics and Statistics,
%    Paisley University, High Street, Paisley, Scotland, PA12BE
%    macl_ms0@paisley.ac.uk
%
%  Reference:
%
%    Allan Mcleod,
%    Algorithm 757, MISCFUN: A software package to compute uncommon
%      special functions,
%    ACM Transactions on Mathematical Software,
%    Volume 22, Number 3, September 1996, pages 288-301.
%
%  Parameters:
%
%    Input, real ( kind = 8 ) XVALUE, the argument of the function.
%
%    Output, real ( kind = 8 ) STRUVE_H1, the value of the function.
%
struve_h1result=[];
persistent arrh1 arrh1a ay1asp ay1asq eight firstCall fortp5 h1as half nine nterm1 nterm2 nterm3 nterm4 one82 rt2bpi struve_h1 t thpby4 tw02p5 twenty twobpi x xhigh xlow1 xlow2 xm3p4 xsq y1p y1q y1val zero ; if isempty(firstCall),firstCall=1;end; 

if isempty(arrh1), arrh1=zeros(1,17+1); end;
if isempty(arrh1a), arrh1a=zeros(1,21+1); end;
if isempty(ay1asp), ay1asp=zeros(1,14+1); end;
if isempty(ay1asq), ay1asq=zeros(1,15+1); end;
if isempty(eight), eight = real( 8.0); end;
if isempty(half), half = 0.5d+00; end;
if isempty(nterm1), nterm1 = 15; end;
if isempty(nterm2), nterm2 = 17; end;
if isempty(nterm3), nterm3 = 12; end;
if isempty(nterm4), nterm4 = 13; end;
if isempty(struve_h1result), struve_h1result=0; end;
if isempty(x), x=0; end;
if isempty(zero), zero = 0.0d+00; end;
if isempty(fortp5), fortp5=0; end;
if isempty(h1as), h1as=0; end;
if isempty(nine), nine=0; end;
if isempty(one82), one82=0; end;
if isempty(rt2bpi), rt2bpi=0; end;
if isempty(t), t=0; end;
if isempty(thpby4), thpby4=0; end;
if isempty(twenty), twenty=0; end;
if isempty(twobpi), twobpi=0; end;
if isempty(tw02p5), tw02p5=0; end;
if isempty(xhigh), xhigh=0; end;
if isempty(xlow1), xlow1=0; end;
if isempty(xlow2), xlow2=0; end;
if isempty(xm3p4), xm3p4=0; end;
if isempty(xsq), xsq=0; end;
if isempty(y1p), y1p=0; end;
if isempty(y1q), y1q=0; end;
if isempty(y1val), y1val=0; end;
if firstCall,   nine =[9.0d0];  end;
if firstCall,   twenty =[20.0d0];  end;
if firstCall,   fortp5=[40.5d0];  end;
if firstCall, one82=[182.0d0];  end;
if firstCall, tw02p5=[202.5d0];  end;
if firstCall,   rt2bpi=[0.79788456080286535588d0];  end;
if firstCall,   thpby4=[2.35619449019234492885d0];  end;
if firstCall,   twobpi=[0.63661977236758134308d0];  end;
if firstCall,   arrh1=[0.17319061083675439319d0,-0.12606917591352672005d0,0.7908576160495357500d-1,-0.3196493222321870820d-1,0.808040581404918834d-2,-0.136000820693074148d-2,0.16227148619889471d-3,-0.1442352451485929d-4,0.99219525734072d-6,-0.5441628049180d-7,0.243631662563d-8,-0.9077071338d-10,0.285926585d-11,-0.7716975d-13,0.180489d-14,-0.3694d-16,0.67d-18,-0.1d-19];  end;
if firstCall,   arrh1a(0+1)=[2.01083504951473379407d0];  end;
if firstCall,   arrh1a(1+1)=[0.592218610036099903d-2];  end;
if firstCall,   arrh1a(2+1)=[0.55274322698414130d-3];  end;
if firstCall,   arrh1a(3+1)=[0.5269873856311036d-4];  end;
if firstCall,   arrh1a(4+1)=[0.506374522140969d-5];  end;
if firstCall,   arrh1a(5+1)=[0.49028736420678d-6];  end;
if firstCall,   arrh1a(6+1)=[0.4763540023525d-7];  end;
if firstCall,   arrh1a(7+1)=[0.465258652283d-8];  end;
if firstCall,   arrh1a(8+1)=[0.45465166081d-9];  end;
if firstCall,   arrh1a(9+1)=[0.4472462193d-10];  end;
if firstCall,   arrh1a(10+1)=[0.437308292d-11];  end;
if firstCall,   arrh1a(11+1)=[0.43568368d-12];  end;
if firstCall,   arrh1a(12+1)=[0.4182190d-13];  end;
if firstCall,   arrh1a(13+1)=[0.441044d-14];  end;
if firstCall,   arrh1a(14+1)=[0.36391d-15];  end;
if firstCall,   arrh1a(15+1)=[0.5558d-16];  end;
if firstCall,   arrh1a(16+1)=[-0.4d-19];  end;
if firstCall,   arrh1a(17+1)=[0.163d-17];  end;
if firstCall,   arrh1a(18+1)=[-0.34d-18];  end;
if firstCall,   arrh1a(19+1)=[0.13d-18];  end;
if firstCall,   arrh1a(20+1)=[-0.4d-19];  end;
if firstCall,   arrh1a(21+1)=[0.1d-19];  end;
if firstCall,   ay1asp=[2.00135240045889396402d0,0.71104241596461938d-3,0.3665977028232449d-4,0.191301568657728d-5,0.10046911389777d-6,0.530401742538d-8,0.28100886176d-9,0.1493886051d-10,0.79578420d-12,0.4252363d-13,0.227195d-14,0.12216d-15,0.650d-17,0.36d-18,0.2d-19];  end;
if firstCall,   ay1asq=[5.99065109477888189116d0,-0.489593262336579635d-2,-0.23238321307070626d-3,-0.1144734723857679d-4,-0.57169926189106d-6,-0.2895516716917d-7,-0.147513345636d-8,-0.7596537378d-10,-0.390658184d-11,-0.20464654d-12,-0.1042636d-13,-0.57702d-15,-0.2550d-16,-0.210d-17,0.2d-19,-0.2d-19];  end;
%
%   MACHINE-DEPENDENT CONSTANTS (Suitable for IEEE-arithmetic machines)
%
if firstCall,   xlow1=[2.23750222d-154];  end;
if firstCall, xlow2=[4.08085106d-8];  end;
if firstCall,   xhigh=[4.50359963d15];  end;
firstCall=0;
%
x = abs( xvalue );
if( x < xlow1 )
struve_h1result = zero;
elseif( x < xlow2 ) ;
xsq = x .* x;
struve_h1result = twobpi .* xsq;
elseif( x <= nine ) ;
xsq = x .* x;
t =( xsq ./ fortp5 - half ) - half;
struve_h1result = twobpi .* xsq .* cheval( nterm1, arrh1, t );
elseif( x <= xhigh ) ;
xsq = x .* x;
t =( one82 - xsq ) ./( twenty + xsq );
[y1p , nterm3, ay1asp, t ]=cheval( nterm3, ay1asp, t );
y1q = cheval( nterm4, ay1asq, t ) ./( eight .* x);
xm3p4 = x - thpby4;
y1val = y1p .* sin( xm3p4 ) + y1q .* cos( xm3p4 );
y1val = y1val .* rt2bpi ./ sqrt( x );
t =( tw02p5 - xsq ) ./( fortp5 + xsq );
h1as = twobpi .* cheval( nterm2, arrh1a, t );
struve_h1result = y1val + h1as;
else;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'STRUVE_H1 - Fatal error!');
writef(1,['%s','\n'], '  Argument magnitude too large.');
struve_h1result = zero;
end;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',xvalue); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [n_data, x, fx]=struve_h1_values( n_data, x, fx );
%*******************************************************************************
%
%! STRUVE_H1_VALUES returns some values of the Struve H1 function.
%
%  Discussion:
%
%    The function is defined by:
%
%      H1(x) = 2*x/pi * Integral ( 0 <= t <= pi/2 )
%        sin ( x * cos ( t ) )^2 * sin ( t ) dt
%
%    In Mathematica, the function can be evaluated by:
%
%      StruveH[1,x]
%
%    The data was reported by McLeod.
%
%  Modified:
%
%    02 September 2004
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Milton Abramowitz and Irene Stegun,
%    Handbook of Mathematical Functions,
%    US Department of Commerce, 1964.
%
%    Allan McLeod,
%    Algorithm 757, MISCFUN: A software package to compute uncommon
%      special functions,
%    ACM Transactions on Mathematical Software,
%    Volume 22, Number 3, September 1996, pages 288-301.
%
%    Stephen Wolfram,
%    The Mathematica Book,
%    Fourth Edition,
%    Wolfram Media / Cambridge University Press, 1999.
%
%  Parameters:
%
%    Input/output, integer N_DATA.  The user sets N_DATA to 0 before the
%    first call.  On each call, the routine increments N_DATA by 1, and
%    returns the corresponding data; when there is no more data, the
%    output value of N_DATA will be 0 again.
%
%    Output, real ( kind = 8 ) X, the argument of the function.
%
%    Output, real ( kind = 8 ) FX, the value of the function.
%
%
persistent fx_vec n_max x_vec ; 

if isempty(n_max), n_max = 20; end;
%
if isempty(fx_vec), fx_vec([1:n_max]) =[real( 0.80950369576367526071d-06),real( 0.12952009724113229165d-04),real( 0.82871615165407083021d-03),real( 0.13207748375849572564d-01),real( 0.19845733620194439894d+00),real( 0.29853823231804706294d+00),real( 0.64676372828356211712d+00),real( 0.10697266613089193593d+01),real( 0.38831308000420560970d+00),real( 0.74854243745107710333d+00),real( 0.84664854642567359993d+00),real( 0.58385732464244384564d+00),real( 0.80600584524215772824d+00),real( 0.53880362132692947616d+00),real( 0.72175037834698998506d+00),real( 0.58007844794544189900d+00),real( 0.60151910385440804463d+00),real( 0.70611511147286827018d+00),real( 0.61631110327201338454d+00),real( 0.62778480765443656489d+00) ]; end;
if isempty(x_vec), x_vec([1:n_max]) =[real(    0.0019531250d+00),real(   -0.0078125000d+00),real(    0.0625000000d+00),real(   -0.2500000000d+00),real(    1.0000000000d+00),real(    1.2500000000d+00),real(    2.0000000000d+00),real(   -4.0000000000d+00),real(    7.5000000000d+00),real(   11.0000000000d+00),real(   11.5000000000d+00),real(  -16.0000000000d+00),real(   20.0000000000d+00),real(   25.0000000000d+00),real(  -30.0000000000d+00),real(   50.0000000000d+00),real(   75.0000000000d+00),real(  -80.0000000000d+00),real(  100.0000000000d+00),real( -125.0000000000d+00) ]; end;
%
if( n_data < 0 )
n_data = 0;
end;
n_data = fix(n_data + 1);
if( n_max < n_data )
n_data = 0;
x = 0.0d+00;
fx = 0.0d+00;
else;
x  = x_vec(n_data);
fx = fx_vec(n_data);
end;
return;
end
function [struve_l0result, xvalue ]=struve_l0( xvalue );
%*******************************************************************************
%
%! STRUVE_L0 calculates the modified Struve function of order 0.
%
%  Discussion:
%
%    This function calculates the modified Struve function of
%    order 0, denoted L0(x), defined as the solution of the
%    second-order equation
%
%      x*D(Df) + Df - x*f  =  2x/pi
%
%    This subroutine is set up to work on IEEE machines.
%
%  Modified:
%
%    07 August 2004
%
%  Author:
%
%    Allan McLeod,
%    Department of Mathematics and Statistics,
%    Paisley University, High Street, Paisley, Scotland, PA12BE
%    macl_ms0@paisley.ac.uk
%
%  Reference:
%
%    Allan Mcleod,
%    Algorithm 757, MISCFUN: A software package to compute uncommon
%      special functions,
%    ACM Transactions on Mathematical Software,
%    Volume 22, Number 3, September 1996, pages 288-301.
%
%  Parameters:
%
%    Input, real ( kind = 8 ) XVALUE, the argument of the function.
%
%    Output, real ( kind = 8 ) STRUVE_L0, the value of the function.
%
%
struve_l0result=[];
persistent ai0ml0 arl0 arl0as atehun ch1 ch2 firstCall four indsgn lnr2pi nterm1 nterm2 nterm3 one sixten struve_l0 t test twent4 twent8 two two88 twobpi x xhigh1 xhigh2 xlow xmax xsq zero ; if isempty(firstCall),firstCall=1;end; 

if isempty(four), four = real( 4.0d+00); end;
if isempty(indsgn), indsgn=0; end;
if isempty(nterm1), nterm1 = 25; end;
if isempty(nterm2), nterm2 = 14; end;
if isempty(nterm3), nterm3 = 21; end;
if isempty(one), one = real( 1.0); end;
if isempty(struve_l0result), struve_l0result=0; end;
if isempty(two), two = 2.0d+00; end;
if isempty(x), x=0; end;
if isempty(zero), zero = 0.0d+00; end;
if isempty(arl0), arl0=zeros(1,27+1); end;
if isempty(arl0as), arl0as=zeros(1,15+1); end;
if isempty(ai0ml0), ai0ml0=zeros(1,23+1); end;
if isempty(atehun), atehun=0; end;
if isempty(ch1), ch1=0; end;
if isempty(ch2), ch2=0; end;
if isempty(lnr2pi), lnr2pi=0; end;
if isempty(sixten), sixten=0; end;
if isempty(t), t=0; end;
if isempty(test), test=0; end;
if isempty(twent4), twent4=0; end;
if isempty(twent8), twent8=0; end;
if isempty(twobpi), twobpi=0; end;
if isempty(two88), two88=0; end;
if isempty(xhigh1), xhigh1=0; end;
if isempty(xhigh2), xhigh2=0; end;
if isempty(xlow), xlow=0; end;
if isempty(xmax), xmax=0; end;
if isempty(xsq), xsq=0; end;
if firstCall,   sixten=[16.0d0];  end;
if firstCall,   twent4=[24.0d0];  end;
if firstCall, twent8=[28.0d0];  end;
if firstCall,   two88=[288.0d0];  end;
if firstCall, atehun=[800.0d0];  end;
if firstCall,   lnr2pi=[0.91893853320467274178d0];  end;
if firstCall,   twobpi=[0.63661977236758134308d0];  end;
if firstCall,   arl0(0+1)=[0.42127458349979924863d0];  end;
if firstCall,   arl0(1+1)=[-0.33859536391220612188d0];  end;
if firstCall,   arl0(2+1)=[0.21898994812710716064d0];  end;
if firstCall,   arl0(3+1)=[-0.12349482820713185712d0];  end;
if firstCall,   arl0(4+1)=[0.6214209793866958440d-1];  end;
if firstCall,   arl0(5+1)=[-0.2817806028109547545d-1];  end;
if firstCall,   arl0(6+1)=[0.1157419676638091209d-1];  end;
if firstCall,   arl0(7+1)=[-0.431658574306921179d-2];  end;
if firstCall,   arl0(8+1)=[0.146142349907298329d-2];  end;
if firstCall,   arl0(9+1)=[-0.44794211805461478d-3];  end;
if firstCall,   arl0(10+1)=[0.12364746105943761d-3];  end;
if firstCall,   arl0(11+1)=[-0.3049028334797044d-4];  end;
if firstCall,   arl0(12+1)=[0.663941401521146d-5];  end;
if firstCall,   arl0(13+1)=[-0.125538357703889d-5];  end;
if firstCall,   arl0(14+1)=[0.20073446451228d-6];  end;
if firstCall,   arl0(15+1)=[-0.2588260170637d-7];  end;
if firstCall,   arl0(16+1)=[0.241143742758d-8];  end;
if firstCall,   arl0(17+1)=[-0.10159674352d-9];  end;
if firstCall,   arl0(18+1)=[-0.1202430736d-10];  end;
if firstCall,   arl0(19+1)=[0.262906137d-11];  end;
if firstCall,   arl0(20+1)=[-0.15313190d-12];  end;
if firstCall,   arl0(21+1)=[-0.1574760d-13];  end;
if firstCall,   arl0(22+1)=[0.315635d-14];  end;
if firstCall,   arl0(23+1)=[-0.4096d-16];  end;
if firstCall,   arl0(24+1)=[-0.3620d-16];  end;
if firstCall,   arl0(25+1)=[0.239d-17];  end;
if firstCall,   arl0(26+1)=[0.36d-18];  end;
if firstCall,   arl0(27+1)=[-0.4d-19];  end;
if firstCall,   arl0as(0+1)=[2.00861308235605888600d0];  end;
if firstCall,   arl0as(1+1)=[0.403737966500438470d-2];  end;
if firstCall,   arl0as(2+1)=[-0.25199480286580267d-3];  end;
if firstCall,   arl0as(3+1)=[0.1605736682811176d-4];  end;
if firstCall,   arl0as(4+1)=[-0.103692182473444d-5];  end;
if firstCall,   arl0as(5+1)=[0.6765578876305d-7];  end;
if firstCall,   arl0as(6+1)=[-0.444999906756d-8];  end;
if firstCall,   arl0as(7+1)=[0.29468889228d-9];  end;
if firstCall,   arl0as(8+1)=[-0.1962180522d-10];  end;
if firstCall,   arl0as(9+1)=[0.131330306d-11];  end;
if firstCall,   arl0as(10+1)=[-0.8819190d-13];  end;
if firstCall,   arl0as(11+1)=[0.595376d-14];  end;
if firstCall,   arl0as(12+1)=[-0.40389d-15];  end;
if firstCall,   arl0as(13+1)=[0.2651d-16];  end;
if firstCall,   arl0as(14+1)=[-0.208d-17];  end;
if firstCall,   arl0as(15+1)=[0.11d-18];  end;
if firstCall,   ai0ml0(0+1)=[2.00326510241160643125d0];  end;
if firstCall,   ai0ml0(1+1)=[0.195206851576492081d-2];  end;
if firstCall,   ai0ml0(2+1)=[0.38239523569908328d-3];  end;
if firstCall,   ai0ml0(3+1)=[0.7534280817054436d-4];  end;
if firstCall,   ai0ml0(4+1)=[0.1495957655897078d-4];  end;
if firstCall,   ai0ml0(5+1)=[0.299940531210557d-5];  end;
if firstCall,   ai0ml0(6+1)=[0.60769604822459d-6];  end;
if firstCall,   ai0ml0(7+1)=[0.12399495544506d-6];  end;
if firstCall,   ai0ml0(8+1)=[0.2523262552649d-7];  end;
if firstCall,   ai0ml0(9+1)=[0.504634857332d-8];  end;
if firstCall,   ai0ml0(10+1)=[0.97913236230d-9];  end;
if firstCall,   ai0ml0(11+1)=[0.18389115241d-9];  end;
if firstCall,   ai0ml0(12+1)=[0.3376309278d-10];  end;
if firstCall,   ai0ml0(13+1)=[0.611179703d-11];  end;
if firstCall,   ai0ml0(14+1)=[0.108472972d-11];  end;
if firstCall,   ai0ml0(15+1)=[0.18861271d-12];  end;
if firstCall,   ai0ml0(16+1)=[0.3280345d-13];  end;
if firstCall,   ai0ml0(17+1)=[0.565647d-14];  end;
if firstCall,   ai0ml0(18+1)=[0.93300d-15];  end;
if firstCall,   ai0ml0(19+1)=[0.15881d-15];  end;
if firstCall,   ai0ml0(20+1)=[0.2791d-16];  end;
if firstCall,   ai0ml0(21+1)=[0.389d-17];  end;
if firstCall,   ai0ml0(22+1)=[0.70d-18];  end;
if firstCall,   ai0ml0(23+1)=[0.16d-18];  end;
%
%   MACHINE-DEPENDENT VALUES (Suitable for IEEE-arithmetic machines)
%
if firstCall,   xlow=[4.4703484d-8];  end;
if firstCall, xmax=[1.797693d308];  end;
if firstCall,   xhigh1=[5.1982303d8];  end;
if firstCall, xhigh2=[2.5220158d17];  end;
firstCall=0;
%
x = xvalue;
indsgn = 1;
if( x < zero )
x = -x;
indsgn = -1;
end;
if( x < xlow )
struve_l0result = twobpi .* x;
elseif( x <= sixten ) ;
t =( four .* x - twent4 ) ./( x + twent4 );
struve_l0result = twobpi .* x .* cheval( nterm1, arl0, t ) .* exp( x );
else;
%
%   Code for |xvalue| > 16
%
if( xhigh2 < x )
ch1 = one;
else;
t =( x - twent8 ) ./( four - x );
[ch1 , nterm2, arl0as, t ]=cheval( nterm2, arl0as, t );
end;
if( xhigh1 < x )
ch2 = one;
else;
xsq = x .* x;
t =( atehun - xsq ) ./( two88 + xsq );
[ch2 , nterm3, ai0ml0, t ]=cheval( nterm3, ai0ml0, t );
end;
test = log( ch1 ) - lnr2pi - log( x ) ./ two + x;
if( log( xmax ) < test )
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'STRUVE_L0 - Fatal error!');
writef(1,['%s','\n'], '  Argument would cause overflow.');
struve_l0result = xmax;
else;
struve_l0result = exp( test ) - twobpi .* ch2 ./ x;
end;
end;
if( indsgn == -1 )
struve_l0result = -struve_l0result;
end;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',xvalue); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [n_data, x, fx]=struve_l0_values( n_data, x, fx );
%*******************************************************************************
%
%! STRUVE_L0_VALUES returns some values of the Struve L0 function.
%
%  Discussion:
%
%    In Mathematica, the function can be evaluated by:
%
%      StruveL[0,x]
%
%    The data was reported by McLeod.
%
%  Modified:
%
%    03 September 2004
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Milton Abramowitz and Irene Stegun,
%    Handbook of Mathematical Functions,
%    US Department of Commerce, 1964.
%
%    Allan McLeod,
%    Algorithm 757, MISCFUN: A software package to compute uncommon
%      special functions,
%    ACM Transactions on Mathematical Software,
%    Volume 22, Number 3, September 1996, pages 288-301.
%
%    Stephen Wolfram,
%    The Mathematica Book,
%    Fourth Edition,
%    Wolfram Media / Cambridge University Press, 1999.
%
%  Parameters:
%
%    Input/output, integer N_DATA.  The user sets N_DATA to 0 before the
%    first call.  On each call, the routine increments N_DATA by 1, and
%    returns the corresponding data; when there is no more data, the
%    output value of N_DATA will be 0 again.
%
%    Output, real ( kind = 8 ) X, the argument of the function.
%
%    Output, real ( kind = 8 ) FX, the value of the function.
%
%
persistent fx_vec n_max x_vec ; 

if isempty(n_max), n_max = 20; end;
%
if isempty(fx_vec), fx_vec([1:n_max]) =[real(  0.12433985199262820188d-02),real( -0.19896526647882937004d-01),real(  0.79715713253115014945d-01),real( -0.32724069939418078025d+00),real(  0.71024318593789088874d+00),real(  0.19374337579914456612d+01),real( -0.11131050203248583431d+02),real(  0.16850062034703267148d+03),real( -0.28156522493745948555d+04),real(  0.89344618796978400815d+06),real(  0.11382025002851451057d+07),real( -0.23549701855860190304d+07),real(  0.43558282527641046718d+08),real(  0.49993516476037957165d+09),real( -0.57745606064408041689d+10),real(  0.78167229782395624524d+12),real( -0.14894774793419899908d+17),real(  0.29325537838493363267d+21),real(  0.58940770556098011683d+25),real( -0.12015889579125463605d+30) ]; end;
if isempty(x_vec), x_vec([1:n_max]) =[real(   0.0019531250d+00),real(  -0.0312500000d+00),real(   0.1250000000d+00),real(  -0.5000000000d+00),real(   1.0000000000d+00),real(   2.0000000000d+00),real(  -4.0000000000d+00),real(   7.0000000000d+00),real( -10.0000000000d+00),real(  16.0000000000d+00),real(  16.2500000000d+00),real( -17.0000000000d+00),real(  20.0000000000d+00),real(  22.5000000000d+00),real( -25.0000000000d+00),real(  30.0000000000d+00),real( -40.0000000000d+00),real(  50.0000000000d+00),real(  60.0000000000d+00),real( -70.0000000000d+00) ]; end;
%
if( n_data < 0 )
n_data = 0;
end;
n_data = fix(n_data + 1);
if( n_max < n_data )
n_data = 0;
x = 0.0d+00;
fx = 0.0d+00;
else;
x  = x_vec(n_data);
fx = fx_vec(n_data);
end;
return;
end
function [struve_l1result, xvalue ]=struve_l1( xvalue );
%*******************************************************************************
%
%! STRUVE_L1 calculates the modified Struve function of order 1.
%
%  Discussion:
%
%    This function calculates the modified Struve function of
%    order 1, denoted L1(x), defined as the solution of
%
%      x*x*D(Df) + x*Df - (x*x+1)f = 2 * x * x / pi
%
%    This subroutine is set up to work on IEEE machines.
%
%  Modified:
%
%    07 August 2004
%
%  Author:
%
%    Allan McLeod,
%    Department of Mathematics and Statistics,
%    Paisley University, High Street, Paisley, Scotland, PA12BE
%    macl_ms0@paisley.ac.uk
%
%  Reference:
%
%    Allan Mcleod,
%    Algorithm 757, MISCFUN: A software package to compute uncommon
%      special functions,
%    ACM Transactions on Mathematical Software,
%    Volume 22, Number 3, September 1996, pages 288-301.
%
%  Parameters:
%
%    Input, real ( kind = 8 ) XVALUE, the argument of the function.
%
%    Output, real ( kind = 8 ) STRUVE_L1, the value of the function.
%
%
struve_l1result=[];
persistent ai1ml1 arl1 arl1as atehun ch1 ch2 firstCall four lnr2pi nterm1 nterm2 nterm3 one pi3by2 sixten struve_l1 t test thirty twent4 two two88 twobpi x xhigh1 xhigh2 xlow1 xlow2 xmax xsq zero ; if isempty(firstCall),firstCall=1;end; 

if isempty(four), four = real( 4.0); end;
if isempty(nterm1), nterm1 = 24; end;
if isempty(nterm2), nterm2 = 13; end;
if isempty(nterm3), nterm3 = 22; end;
if isempty(one), one = real( 1.0); end;
if isempty(sixten), sixten = real( 16.0); end;
if isempty(struve_l1result), struve_l1result=0; end;
if isempty(two), two = 2.0d+00; end;
if isempty(x), x=0; end;
if isempty(zero), zero = 0.0d+00; end;
if isempty(arl1), arl1=zeros(1,26+1); end;
if isempty(arl1as), arl1as=zeros(1,16+1); end;
if isempty(ai1ml1), ai1ml1=zeros(1,25+1); end;
if isempty(atehun), atehun=0; end;
if isempty(ch1), ch1=0; end;
if isempty(ch2), ch2=0; end;
if isempty(lnr2pi), lnr2pi=0; end;
if isempty(pi3by2), pi3by2=0; end;
if isempty(t), t=0; end;
if isempty(test), test=0; end;
if isempty(thirty), thirty=0; end;
if isempty(twent4), twent4=0; end;
if isempty(twobpi), twobpi=0; end;
if isempty(two88), two88=0; end;
if isempty(xhigh1), xhigh1=0; end;
if isempty(xhigh2), xhigh2=0; end;
if isempty(xlow1), xlow1=0; end;
if isempty(xlow2), xlow2=0; end;
if isempty(xmax), xmax=0; end;
if isempty(xsq), xsq=0; end;
if firstCall,   twent4=[24.0d0];  end;
if firstCall, thirty=[30.0d0];  end;
if firstCall,   two88=[288.0d0];  end;
if firstCall, atehun=[800.0d0];  end;
if firstCall,   lnr2pi=[0.91893853320467274178d0];  end;
if firstCall,   pi3by2=[4.71238898038468985769d0];  end;
if firstCall,   twobpi=[0.63661977236758134308d0];  end;
if firstCall,   arl1(0+1)=[0.38996027351229538208d0];  end;
if firstCall,   arl1(1+1)=[-0.33658096101975749366d0];  end;
if firstCall,   arl1(2+1)=[0.23012467912501645616d0];  end;
if firstCall,   arl1(3+1)=[-0.13121594007960832327d0];  end;
if firstCall,   arl1(4+1)=[0.6425922289912846518d-1];  end;
if firstCall,   arl1(5+1)=[-0.2750032950616635833d-1];  end;
if firstCall,   arl1(6+1)=[0.1040234148637208871d-1];  end;
if firstCall,   arl1(7+1)=[-0.350532294936388080d-2];  end;
if firstCall,   arl1(8+1)=[0.105748498421439717d-2];  end;
if firstCall,   arl1(9+1)=[-0.28609426403666558d-3];  end;
if firstCall,   arl1(10+1)=[0.6925708785942208d-4];  end;
if firstCall,   arl1(11+1)=[-0.1489693951122717d-4];  end;
if firstCall,   arl1(12+1)=[0.281035582597128d-5];  end;
if firstCall,   arl1(13+1)=[-0.45503879297776d-6];  end;
if firstCall,   arl1(14+1)=[0.6090171561770d-7];  end;
if firstCall,   arl1(15+1)=[-0.623543724808d-8];  end;
if firstCall,   arl1(16+1)=[0.38430012067d-9];  end;
if firstCall,   arl1(17+1)=[0.790543916d-11];  end;
if firstCall,   arl1(18+1)=[-0.489824083d-11];  end;
if firstCall,   arl1(19+1)=[0.46356884d-12];  end;
if firstCall,   arl1(20+1)=[0.684205d-14];  end;
if firstCall,   arl1(21+1)=[-0.569748d-14];  end;
if firstCall,   arl1(22+1)=[0.35324d-15];  end;
if firstCall,   arl1(23+1)=[0.4244d-16];  end;
if firstCall,   arl1(24+1)=[-0.644d-17];  end;
if firstCall,   arl1(25+1)=[-0.21d-18];  end;
if firstCall,   arl1(26+1)=[0.9d-19];  end;
if firstCall,   arl1as(0+1)=[1.97540378441652356868d0];  end;
if firstCall,   arl1as(1+1)=[-0.1195130555088294181d-1];  end;
if firstCall,   arl1as(2+1)=[0.33639485269196046d-3];  end;
if firstCall,   arl1as(3+1)=[-0.1009115655481549d-4];  end;
if firstCall,   arl1as(4+1)=[0.30638951321998d-6];  end;
if firstCall,   arl1as(5+1)=[-0.953704370396d-8];  end;
if firstCall,   arl1as(6+1)=[0.29524735558d-9];  end;
if firstCall,   arl1as(7+1)=[-0.951078318d-11];  end;
if firstCall,   arl1as(8+1)=[0.28203667d-12];  end;
if firstCall,   arl1as(9+1)=[-0.1134175d-13];  end;
if firstCall,   arl1as(10+1)=[0.147d-17];  end;
if firstCall,   arl1as(11+1)=[-0.6232d-16];  end;
if firstCall,   arl1as(12+1)=[-0.751d-17];  end;
if firstCall,   arl1as(13+1)=[-0.17d-18];  end;
if firstCall,   arl1as(14+1)=[0.51d-18];  end;
if firstCall,   arl1as(15+1)=[0.23d-18];  end;
if firstCall,   arl1as(16+1)=[0.5d-19];  end;
if firstCall,   ai1ml1(0+1)=[1.99679361896789136501d0];  end;
if firstCall,   ai1ml1(1+1)=[-0.190663261409686132d-2];  end;
if firstCall,   ai1ml1(2+1)=[-0.36094622410174481d-3];  end;
if firstCall,   ai1ml1(3+1)=[-0.6841847304599820d-4];  end;
if firstCall,   ai1ml1(4+1)=[-0.1299008228509426d-4];  end;
if firstCall,   ai1ml1(5+1)=[-0.247152188705765d-5];  end;
if firstCall,   ai1ml1(6+1)=[-0.47147839691972d-6];  end;
if firstCall,   ai1ml1(7+1)=[-0.9020819982592d-7];  end;
if firstCall,   ai1ml1(8+1)=[-0.1730458637504d-7];  end;
if firstCall,   ai1ml1(9+1)=[-0.332323670159d-8];  end;
if firstCall,   ai1ml1(10+1)=[-0.63736421735d-9];  end;
if firstCall,   ai1ml1(11+1)=[-0.12180239756d-9];  end;
if firstCall,   ai1ml1(12+1)=[-0.2317346832d-10];  end;
if firstCall,   ai1ml1(13+1)=[-0.439068833d-11];  end;
if firstCall,   ai1ml1(14+1)=[-0.82847110d-12];  end;
if firstCall,   ai1ml1(15+1)=[-0.15562249d-12];  end;
if firstCall,   ai1ml1(16+1)=[-0.2913112d-13];  end;
if firstCall,   ai1ml1(17+1)=[-0.543965d-14];  end;
if firstCall,   ai1ml1(18+1)=[-0.101177d-14];  end;
if firstCall,   ai1ml1(19+1)=[-0.18767d-15];  end;
if firstCall,   ai1ml1(20+1)=[-0.3484d-16];  end;
if firstCall,   ai1ml1(21+1)=[-0.643d-17];  end;
if firstCall,   ai1ml1(22+1)=[-0.118d-17];  end;
if firstCall,   ai1ml1(23+1)=[-0.22d-18];  end;
if firstCall,   ai1ml1(24+1)=[-0.4d-19];  end;
if firstCall,   ai1ml1(25+1)=[-0.1d-19];  end;
%
%   MACHINE-DEPENDENT VALUES (Suitable for IEEE-arithmetic machines)
%
if firstCall,   xlow1=[5.7711949d-8];  end;
if firstCall, xlow2=[3.3354714d-154];  end;
if firstCall, xmax=[1.797693d308];  end;
if firstCall,   xhigh1=[5.19823025d8];  end;
if firstCall, xhigh2=[2.7021597d17];  end;
firstCall=0;
%
x = abs( xvalue );
if( x <= xlow2 )
struve_l1result = zero;
elseif( x < xlow1 ) ;
xsq = x .* x;
struve_l1result = xsq ./ pi3by2;
elseif( x <= sixten ) ;
xsq = x .* x;
t =( four .* x - twent4 ) ./( x + twent4 );
struve_l1result = xsq .* cheval( nterm1, arl1, t ) .* exp( x ) ./ pi3by2;
else;
if( xhigh2 < x )
ch1 = one;
else;
t =( x - thirty ) ./( two - x );
[ch1 , nterm2, arl1as, t ]=cheval( nterm2, arl1as, t );
end;
if( xhigh1 < x )
ch2 = one;
else;
xsq = x .* x;
t =( atehun - xsq ) ./( two88 + xsq );
[ch2 , nterm3, ai1ml1, t ]=cheval( nterm3, ai1ml1, t );
end;
test = log( ch1 ) - lnr2pi - log( x ) ./ two + x;
if( log( xmax ) < test )
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'STRUVE_L1 - Fatal error!');
writef(1,['%s','\n'], '  Argument would cause overflow.');
struve_l1result = xmax;
else;
struve_l1result = exp( test ) - twobpi .* ch2;
end;
end;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',xvalue); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [n_data, x, fx]=struve_l1_values( n_data, x, fx );
%*******************************************************************************
%
%! STRUVE_L1_VALUES returns some values of the Struve L1 function.
%
%  Discussion:
%
%    In Mathematica, the function can be evaluated by:
%
%      StruveL[1,x]
%
%    The data was reported by McLeod.
%
%  Modified:
%
%    06 September 2004
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Milton Abramowitz and Irene Stegun,
%    Handbook of Mathematical Functions,
%    US Department of Commerce, 1964.
%
%    Allan McLeod,
%    Algorithm 757, MISCFUN: A software package to compute uncommon
%      special functions,
%    ACM Transactions on Mathematical Software,
%    Volume 22, Number 3, September 1996, pages 288-301.
%
%    Stephen Wolfram,
%    The Mathematica Book,
%    Fourth Edition,
%    Wolfram Media / Cambridge University Press, 1999.
%
%  Parameters:
%
%    Input/output, integer N_DATA.  The user sets N_DATA to 0 before the
%    first call.  On each call, the routine increments N_DATA by 1, and
%    returns the corresponding data; when there is no more data, the
%    output value of N_DATA will be 0 again.
%
%    Output, real ( kind = 8 ) X, the argument of the function.
%
%    Output, real ( kind = 8 ) FX, the value of the function.
%
%
persistent fx_vec n_max x_vec ; 

if isempty(n_max), n_max = 20; end;
%
if isempty(fx_vec), fx_vec([1:n_max]) =[real( 0.80950410749865126939d-06),real( 0.20724649092571514607d-03),real( 0.33191834066894516744d-02),real( 0.53942182623522663292d-01),real( 0.22676438105580863683d+00),real( 0.11027597873677158176d+01),real( 0.91692778117386847344d+01),real( 0.15541656652426660966d+03),real( 0.26703582852084829694d+04),real( 0.86505880175304633906d+06),real( 0.11026046613094942620d+07),real( 0.22846209494153934787d+07),real( 0.42454972750111979449d+08),real( 0.48869614587997695539d+09),real( 0.56578651292431051863d+10),real( 0.76853203893832108948d+12),real( 0.14707396163259352103d+17),real( 0.29030785901035567967d+21),real( 0.58447515883904682813d+25),real( 0.11929750788892311875d+30) ]; end;
if isempty(x_vec), x_vec([1:n_max]) =[real(   0.0019531250d+00),real(  -0.0312500000d+00),real(   0.1250000000d+00),real(  -0.5000000000d+00),real(   1.0000000000d+00),real(   2.0000000000d+00),real(  -4.0000000000d+00),real(   7.0000000000d+00),real( -10.0000000000d+00),real(  16.0000000000d+00),real(  16.2500000000d+00),real( -17.0000000000d+00),real(  20.0000000000d+00),real(  22.5000000000d+00),real( -25.0000000000d+00),real(  30.0000000000d+00),real( -40.0000000000d+00),real(  50.0000000000d+00),real(  60.0000000000d+00),real( -70.0000000000d+00) ]; end;
%
if( n_data < 0 )
n_data = 0;
end;
n_data = fix(n_data + 1);
if( n_max < n_data )
n_data = 0;
x = 0.0d+00;
fx = 0.0d+00;
else;
x  = x_vec(n_data);
fx = fx_vec(n_data);
end;
return;
end
function [synch1result, xvalue ]=synch1( xvalue );
%*******************************************************************************
%
%! SYNCH1 calculates the synchrotron radiation function.
%
%  Discussion:
%
%    The function is defined by:
%
%      SYNCH1(x) = x * Integral ( x <= t < infinity ) K(5/3,t) dt
%
%    where K(5/3) is a modified Bessel function of order 5/3.
%
%    The code uses Chebyshev expansions, the coefficients of which
%    are given to 20 decimal places.
%
%    This subroutine is set up to work on IEEE machines.
%
%  Modified:
%
%    07 September 2004
%
%  Author:
%
%    Allan McLeod,
%    Department of Mathematics and Statistics,
%    Paisley University, High Street, Paisley, Scotland, PA12BE
%    macl_ms0@paisley.ac.uk
%
%  Reference:
%
%    Allan Mcleod,
%    Algorithm 757, MISCFUN: A software package to compute uncommon
%      special functions,
%    ACM Transactions on Mathematical Software,
%    Volume 22, Number 3, September 1996, pages 288-301.
%
%  Parameters:
%
%    Input, real ( kind = 8 ) XVALUE, the argument of the function.
%
%    Output, real ( kind = 8 ) SYNCH1, the value of the function.
%
%
synch1result=[];
persistent async1 async2 asynca cheb1 cheb2 conlow eight firstCall four half lnrtp2 nterm1 nterm2 nterm3 one pibrt3 synch1 t three twelve x xhigh1 xhigh2 xlow xpowth zero ; if isempty(firstCall),firstCall=1;end; 

if isempty(eight), eight = real( 8.0d+00); end;
if isempty(four), four = real( 4.0d+00); end;
if isempty(half), half = real( 0.5d+00); end;
if isempty(nterm1), nterm1 = 12; end;
if isempty(nterm2), nterm2 = 10; end;
if isempty(nterm3), nterm3 = 21; end;
if isempty(one), one = real( 1.0d+00); end;
if isempty(synch1result), synch1result=0; end;
if isempty(three), three = real( 3.0d+00); end;
if isempty(x), x=0; end;
if isempty(zero), zero = real( 0.0d+00); end;
if isempty(async1), async1=zeros(1,13+1); end;
if isempty(async2), async2=zeros(1,11+1); end;
if isempty(asynca), asynca=zeros(1,24+1); end;
if isempty(cheb1), cheb1=0; end;
if isempty(cheb2), cheb2=0; end;
if isempty(conlow), conlow=0; end;
if isempty(lnrtp2), lnrtp2=0; end;
if isempty(pibrt3), pibrt3=0; end;
if isempty(t), t=0; end;
if isempty(twelve), twelve=0; end;
if isempty(xhigh1), xhigh1=0; end;
if isempty(xhigh2), xhigh2=0; end;
if isempty(xlow), xlow=0; end;
if isempty(xpowth), xpowth=0; end;
if firstCall,   twelve=[12.0d0];  end;
if firstCall,   conlow=[2.14952824153447863671d0];  end;
if firstCall,   pibrt3=[1.81379936423421785059d0];  end;
if firstCall,   lnrtp2=[0.22579135264472743236d0];  end;
if firstCall,   async1=[30.36468298250107627340d0,17.07939527740839457449d0,4.56013213354507288887d0,0.54928124673041997963d0,0.3729760750693011724d-1,0.161362430201041242d-2,0.4819167721203707d-4,0.105124252889384d-5,0.1746385046697d-7,0.22815486544d-9,0.240443082d-11,0.2086588d-13,0.15167d-15,0.94d-18];  end;
if firstCall,   async2=[0.44907216235326608443d0,0.8983536779941872179d-1,0.810445737721512894d-2,0.42617169910891619d-3,0.1476096312707460d-4,0.36286336153998d-6,0.666348074984d-8,0.9490771655d-10,0.107912491d-11,0.1002201d-13,0.7745d-16,0.51d-18];  end;
if firstCall,   asynca(0+1)=[2.13293051613550009848d0];  end;
if firstCall,   asynca(1+1)=[0.7413528649542002401d-1];  end;
if firstCall,   asynca(2+1)=[0.869680999099641978d-2];  end;
if firstCall,   asynca(3+1)=[0.117038262487756921d-2];  end;
if firstCall,   asynca(4+1)=[0.16451057986191915d-3];  end;
if firstCall,   asynca(5+1)=[0.2402010214206403d-4];  end;
if firstCall,   asynca(6+1)=[0.358277563893885d-5];  end;
if firstCall,   asynca(7+1)=[0.54477476269837d-6];  end;
if firstCall,   asynca(8+1)=[0.8388028561957d-7];  end;
if firstCall,   asynca(9+1)=[0.1306988268416d-7];  end;
if firstCall,   asynca(10+1)=[0.205309907144d-8];  end;
if firstCall,   asynca(11+1)=[0.32518753688d-9];  end;
if firstCall,   asynca(12+1)=[0.5179140412d-10];  end;
if firstCall,   asynca(13+1)=[0.830029881d-11];  end;
if firstCall,   asynca(14+1)=[0.133527277d-11];  end;
if firstCall,   asynca(15+1)=[0.21591498d-12];  end;
if firstCall,   asynca(16+1)=[0.3499673d-13];  end;
if firstCall,   asynca(17+1)=[0.569942d-14];  end;
if firstCall,   asynca(18+1)=[0.92906d-15];  end;
if firstCall,   asynca(19+1)=[0.15222d-15];  end;
if firstCall,   asynca(20+1)=[0.2491d-16];  end;
if firstCall,   asynca(21+1)=[0.411d-17];  end;
if firstCall,   asynca(22+1)=[0.67d-18];  end;
if firstCall,   asynca(23+1)=[0.11d-18];  end;
if firstCall,   asynca(24+1)=[0.2d-19];  end;
%
%   Machine-dependent constants (suitable for IEEE machines)
%
if firstCall,   xlow=[2.98023224d-8];  end;
if firstCall,   xhigh1=[809.595907d0];  end;
if firstCall, xhigh2=[-708.396418d0];  end;
firstCall=0;
%
x = xvalue;
if( x < zero )
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'SYNCH1 - Fatal error!');
writef(1,['%s','\n'], '  Argument X < 0.');
synch1result = zero;
elseif( x < xlow ) ;
xpowth = x .^( one ./ three );
synch1result = conlow .* xpowth;
elseif( x <= four ) ;
xpowth = x .^( one ./ three );
t =( x .* x ./ eight - half ) - half;
[cheb1 , nterm1, async1, t ]=cheval( nterm1, async1, t );
[cheb2 , nterm2, async2, t ]=cheval( nterm2, async2, t );
t = xpowth .* cheb1 - xpowth.^11 .* cheb2;
synch1result = t - pibrt3 .* x;
elseif( x <= xhigh1 ) ;
t =( twelve - x ) ./( x + four );
[cheb1 , nterm3, asynca, t ]=cheval( nterm3, asynca, t );
t = lnrtp2 - x + log( sqrt( x ) .* cheb1 );
if( t < xhigh2 )
synch1result = zero;
else;
synch1result = exp( t );
end;
else;
synch1result = zero;
end;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',xvalue); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [n_data, x, fx]=synch1_values( n_data, x, fx );
%*******************************************************************************
%
%! SYNCH1_VALUES returns some values of the synchrotron radiation function.
%
%  Discussion:
%
%    The function is defined by:
%
%      SYNCH1(x) = x * Integral ( x <= t < infinity ) K(5/3,t) dt
%
%    where K(5/3) is a modified Bessel function of order 5/3.
%
%  Modified:
%
%    05 September 2004
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Milton Abramowitz and Irene Stegun,
%    Handbook of Mathematical Functions,
%    US Department of Commerce, 1964.
%
%    Allan McLeod,
%    Algorithm 757, MISCFUN: A software package to compute uncommon
%      special functions,
%    ACM Transactions on Mathematical Software,
%    Volume 22, Number 3, September 1996, pages 288-301.
%
%    Stephen Wolfram,
%    The Mathematica Book,
%    Fourth Edition,
%    Wolfram Media / Cambridge University Press, 1999.
%
%  Parameters:
%
%    Input/output, integer N_DATA.  The user sets N_DATA to 0 before the
%    first call.  On each call, the routine increments N_DATA by 1, and
%    returns the corresponding data; when there is no more data, the
%    output value of N_DATA will be 0 again.
%
%    Output, real ( kind = 8 ) X, the argument of the function.
%
%    Output, real ( kind = 8 ) FX, the value of the function.
%
%
persistent fx_vec n_max x_vec ; 

if isempty(n_max), n_max = 20; end;
%
if isempty(fx_vec), fx_vec([1:n_max]) =[real(   0.26514864547487397044d+00),real(   0.62050129979079045645d+00),real(   0.85112572132368011206d+00),real(   0.87081914687546885094d+00),real(   0.65142281535536396975d+00),real(   0.45064040920322354579d+00),real(   0.30163590285073940285d+00),real(   0.19814490804441305867d+00),real(   0.12856571000906381300d+00),real(   0.52827396697866818297d-01),real(   0.42139298471720305542d-01),real(   0.21248129774981984268d-01),real(   0.13400258907505536491d-01),real(   0.84260797314108699935d-02),real(   0.12884516186754671469d-02),real(   0.19223826430086897418d-03),real(   0.28221070834007689394d-04),real(   0.15548757973038189372d-05),real(   0.11968634456097453636d-07),real(   0.89564246772237127742d-10) ]; end;
if isempty(x_vec), x_vec([1:n_max]) =[real(   0.0019531250d+00),real(   0.0312500000d+00),real(   0.1250000000d+00),real(   0.5000000000d+00),real(   1.0000000000d+00),real(   1.5000000000d+00),real(   2.0000000000d+00),real(   2.5000000000d+00),real(   3.0000000000d+00),real(   4.0000000000d+00),real(   4.2500000000d+00),real(   5.0000000000d+00),real(   5.5000000000d+00),real(   6.0000000000d+00),real(   8.0000000000d+00),real(  10.0000000000d+00),real(  12.0000000000d+00),real(  15.0000000000d+00),real(  20.0000000000d+00),real(  25.0000000000d+00) ]; end;
%
if( n_data < 0 )
n_data = 0;
end;
n_data = fix(n_data + 1);
if( n_max < n_data )
n_data = 0;
x = 0.0d+00;
fx = 0.0d+00;
else;
x  = x_vec(n_data);
fx = fx_vec(n_data);
end;
return;
end
function [synch2result, xvalue ]=synch2( xvalue );
%*******************************************************************************
%
%! SYNCH2 calculates the synchrotron radiation function.
%
%  Discussion:
%
%    The function is defined by:
%
%      SYNCH2(x) = x * K(2/3,x)
%
%    where K(2/3) is a modified Bessel function of order 2/3.
%
%    The code uses Chebyshev expansions, the coefficients of which
%    are given to 20 decimal places.
%
%    This subroutine is set up to work on IEEE machines.
%
%  Modified:
%
%    07 August 2004
%
%  Author:
%
%    Allan McLeod,
%    Department of Mathematics and Statistics,
%    Paisley University, High Street, Paisley, Scotland, PA12BE
%    macl_ms0@paisley.ac.uk
%
%  Reference:
%
%    Allan Mcleod,
%    Algorithm 757, MISCFUN: A software package to compute uncommon
%      special functions,
%    ACM Transactions on Mathematical Software,
%    Volume 22, Number 3, September 1996, pages 288-301.
%
%  Parameters:
%
%    Input, real ( kind = 8 ) XVALUE, the argument of the function.
%
%    Output, real ( kind = 8 ) SYNCH2, the value of the function.
%
%
synch2result=[];
persistent asyn21 asyn22 asyn2a cheb1 cheb2 conlow eight firstCall four half lnrtp2 nterm1 nterm2 nterm3 one synch2 t ten three two x xhigh1 xhigh2 xlow xpowth zero ; if isempty(firstCall),firstCall=1;end; 

if isempty(eight), eight = real( 8.0); end;
if isempty(four), four = real( 4.0d+00); end;
if isempty(half), half = 0.5d+00; end;
if isempty(nterm1), nterm1 = 13; end;
if isempty(nterm2), nterm2 = 12; end;
if isempty(nterm3), nterm3 = 16; end;
if isempty(one), one = real( 1.0); end;
if isempty(synch2result), synch2result=0; end;
if isempty(three), three = real( 3.0); end;
if isempty(two), two = 2.0d+00; end;
if isempty(x), x=0; end;
if isempty(zero), zero = 0.0d+00; end;
if isempty(asyn21), asyn21=zeros(1,14+1); end;
if isempty(asyn22), asyn22=zeros(1,13+1); end;
if isempty(asyn2a), asyn2a=zeros(1,18+1); end;
if isempty(cheb1), cheb1=0; end;
if isempty(cheb2), cheb2=0; end;
if isempty(conlow), conlow=0; end;
if isempty(lnrtp2), lnrtp2=0; end;
if isempty(t), t=0; end;
if isempty(ten), ten=0; end;
if isempty(xhigh1), xhigh1=0; end;
if isempty(xhigh2), xhigh2=0; end;
if isempty(xlow), xlow=0; end;
if isempty(xpowth), xpowth=0; end;
if firstCall,   ten=[10.0d0];  end;
if firstCall,   conlow=[1.07476412076723931836d0];  end;
if firstCall,   lnrtp2=[0.22579135264472743236d0];  end;
if firstCall,   asyn21=[38.61783992384308548014d0,23.03771559496373459697d0,5.38024998683357059676d0,0.61567938069957107760d0,0.4066880046688955843d-1,0.172962745526484141d-2,0.5106125883657699d-4,0.110459595022012d-5,0.1823553020649d-7,0.23707698034d-9,0.248872963d-11,0.2152868d-13,0.15607d-15,0.96d-18,0.1d-19];  end;
if firstCall,   asyn22=[7.90631482706608042875d0,3.13534636128534256841d0,0.48548794774537145380d0,0.3948166758272372337d-1,0.196616223348088022d-2,0.6590789322930420d-4,0.158575613498559d-5,0.2868653011233d-7,0.40412023595d-9,0.455684443d-11,0.4204590d-13,0.32326d-15,0.210d-17,0.1d-19];  end;
if firstCall,   asyn2a=[2.02033709417071360032d0,0.1095623712180740443d-1,0.85423847301146755d-3,0.7234302421328222d-4,0.631244279626992d-5,0.56481931411744d-6,0.5128324801375d-7,0.471965329145d-8,0.43807442143d-9,0.4102681493d-10,0.386230721d-11,0.36613228d-12,0.3480232d-13,0.333010d-14,0.31856d-15,0.3074d-16,0.295d-17,0.29d-18,0.3d-19];  end;
%
%   Machine-dependent constants (suitable for IEEE machines)
%
if firstCall,   xlow=[2.98023224d-8];  end;
if firstCall,   xhigh1=[809.595907d0];  end;
if firstCall, xhigh2=[-708.396418d0];  end;
firstCall=0;
%
x = xvalue;
if( x < zero )
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'SYNCH2 - Fatal error!');
writef(1,['%s','\n'], '  Argument X < 0.');
synch2result = zero;
elseif( x < xlow ) ;
xpowth = x .^( one ./ three );
synch2result = conlow .* xpowth;
elseif( x <= four ) ;
xpowth = x .^( one ./ three );
t =( x .* x ./ eight - half ) - half;
[cheb1 , nterm1, asyn21, t ]=cheval( nterm1, asyn21, t );
[cheb2 , nterm2, asyn22, t ]=cheval( nterm2, asyn22, t );
synch2result = xpowth .* cheb1 - xpowth.^5 .* cheb2;
elseif( x <= xhigh1 ) ;
t =( ten - x ) ./( x + two );
[cheb1 , nterm3, asyn2a, t ]=cheval( nterm3, asyn2a, t );
t = lnrtp2 - x + log( sqrt( x ) .* cheb1 );
if( t < xhigh2 )
synch2result = zero;
else;
synch2result = exp( t );
end;
else;
synch2result = zero;
end;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',xvalue); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [n_data, x, fx]=synch2_values( n_data, x, fx );
%*******************************************************************************
%
%! SYNCH2_VALUES returns some values of the synchrotron radiation function.
%
%  Discussion:
%
%    The function is defined by:
%
%      SYNCH2(x) = x * K(2/3,x)
%
%    where K(2/3) is a modified Bessel function of order 2/3.
%
%  Modified:
%
%    05 September 2004
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Milton Abramowitz and Irene Stegun,
%    Handbook of Mathematical Functions,
%    US Department of Commerce, 1964.
%
%    Allan McLeod,
%    Algorithm 757, MISCFUN: A software package to compute uncommon
%      special functions,
%    ACM Transactions on Mathematical Software,
%    Volume 22, Number 3, September 1996, pages 288-301.
%
%    Stephen Wolfram,
%    The Mathematica Book,
%    Fourth Edition,
%    Wolfram Media / Cambridge University Press, 1999.
%
%  Parameters:
%
%    Input/output, integer N_DATA.  The user sets N_DATA to 0 before the
%    first call.  On each call, the routine increments N_DATA by 1, and
%    returns the corresponding data; when there is no more data, the
%    output value of N_DATA will be 0 again.
%
%    Output, real ( kind = 8 ) X, the argument of the function.
%
%    Output, real ( kind = 8 ) FX, the value of the function.
%
%
persistent fx_vec n_max x_vec ; 

if isempty(n_max), n_max = 20; end;
%
if isempty(fx_vec), fx_vec([1:n_max]) =[real( 0.13430727275667378338d+00),real( 0.33485265272424176976d+00),real( 0.50404224110911078651d+00),real( 0.60296523236016785113d+00),real( 0.49447506210420826699d+00),real( 0.36036067860473360389d+00),real( 0.24967785497625662113d+00),real( 0.16813830542905833533d+00),real( 0.11117122348556549832d+00),real( 0.46923205826101330711d-01),real( 0.37624545861980001482d-01),real( 0.19222123172484106436d-01),real( 0.12209535343654701398d-01),real( 0.77249644268525771866d-02),real( 0.12029044213679269639d-02),real( 0.18161187569530204281d-03),real( 0.26884338006629353506d-04),real( 0.14942212731345828759d-05),real( 0.11607696854385161390d-07),real( 0.87362343746221526073d-10) ]; end;
if isempty(x_vec), x_vec([1:n_max]) =[real(   0.0019531250d+00),real(   0.0312500000d+00),real(   0.1250000000d+00),real(   0.5000000000d+00),real(   1.0000000000d+00),real(   1.5000000000d+00),real(   2.0000000000d+00),real(   2.5000000000d+00),real(   3.0000000000d+00),real(   4.0000000000d+00),real(   4.2500000000d+00),real(   5.0000000000d+00),real(   5.5000000000d+00),real(   6.0000000000d+00),real(   8.0000000000d+00),real(  10.0000000000d+00),real(  12.0000000000d+00),real(  15.0000000000d+00),real(  20.0000000000d+00),real(  25.0000000000d+00) ]; end;
%
if( n_data < 0 )
n_data = 0;
end;
n_data = fix(n_data + 1);
if( n_max < n_data )
n_data = 0;
x = 0.0d+00;
fx = 0.0d+00;
else;
x  = x_vec(n_data);
fx = fx_vec(n_data);
end;
return;
end
function timestamp(varargin)
%*******************************************************************************
%
%! TIMESTAMP prints the current YMDHMS date as a time stamp.
%
%  Example:
%
%    May 31 2001   9:45:54.872 AM
%
%  Modified:
%
%    15 March 2003
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    None
%
%
persistent string ; 

if isempty(string), string=repmat(' ',1,40); end;
%
[ string ]=timestring( string );
writef(1,['%s','\n'], deblank( string ));
return;
end
function [string]=timestring( string );
%*******************************************************************************
%
%! TIMESTRING writes the current YMDHMS date into a string.
%
%  Example:
%
%    STRING = 'May 31 2001   9:45:54.872 AM'
%
%  Modified:
%
%    15 March 2003
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Output, character ( len = * ) STRING, contains the date information.
%    A character length of 40 should always be sufficient.
%
%
persistent ampm d datemlv h m mm month n s timemlv values y zone ; if isempty(month),month={};end; 

if isempty(ampm), ampm=repmat(' ',1,8); end;
if isempty(d), d=0; end;
if isempty(datemlv), datemlv=repmat(' ',1,8); end;
if isempty(h), h=0; end;
if isempty(m), m=0; end;
if isempty(mm), mm=0; end;
if isempty(month), month = {'January  ', 'February ', 'March    ', 'April    ','May      ', 'June     ', 'July     ', 'August   ','September', 'October  ', 'November ', 'December ' }; end;
if isempty(n), n=0; end;
if isempty(s), s=0; end;
if isempty(timemlv), timemlv=repmat(' ',1,10); end;
if isempty(values), values=zeros(1,8); end;
if isempty(y), y=0; end;
if isempty(zone), zone=repmat(' ',1,5); end;
%
datemlv=datestr(now,'yyyymmdd');
timemlv=[datestr(now,'HHMMSS'),'.',num2str(round((sum(clock)-fix(sum(clock))).*1000),3)];
values (1)=fix(str2num(datestr(now,'yyyy')));
values (2)=fix(str2num(datestr(now,'mm')));
values (3)=fix(str2num(datestr(now,'dd')));
values (4)=0;
values (5)=fix(str2num(datestr(now,'HH')));
values (6)=fix(str2num(datestr(now,'MM')));
values (7)=fix(str2num(datestr(now,'SS')));
values (8)=fix(round((sum(clock)-fix(sum(clock))).*1000));
y = fix(values(1));
m = fix(values(2));
d = fix(values(3));
h = fix(values(5));
n = fix(values(6));
s = fix(values(7));
mm = fix(values(8));
if( h < 12 )
ampm = 'AM';
elseif( h == 12 ) ;
if( n == 0 && s == 0 )
ampm = 'Noon';
else;
ampm = 'PM';
end;
else;
h = fix(h - 12);
if( h < 12 )
ampm = 'PM';
elseif( h == 12 ) ;
if( n == 0 && s == 0 )
ampm = 'Midnight';
else;
ampm = 'AM';
end;
end;
end;
string=sprintf(['%s',repmat(' ',1,1),'%2i',repmat(' ',1,1),'%4i',repmat(' ',1,2),'%2i','%1s','%2.2i','%1s','%2.2i','%1s','%3.3i',repmat(' ',1,1),'%s'],deblank ( month{m} ), d, y, h, ':', n, ':', s, '.', mm, deblank ( ampm ));
return;
end
function [tran02result, xvalue ]=tran02( xvalue );
%*******************************************************************************
%
%! TRAN02 calculates the transport integral of order 2.
%
%  Discussion:
%
%    The function is defined by:
%
%      TRAN02(x) = Integral ( 0 <= t <= x ) t^2 exp(t) / ( exp(t) - 1 )^2 dt
%
%    The program uses a Chebyshev series, the coefficients of which are
%    given to an accuracy of 20 decimal places.
%
%    This subroutine is set up to work on IEEE machines.
%
%  Modified:
%
%    07 August 2004
%
%  Author:
%
%    Allan McLeod,
%    Department of Mathematics and Statistics,
%    Paisley University, High Street, Paisley, Scotland, PA12BE
%    macl_ms0@paisley.ac.uk
%
%  Reference:
%
%    Allan Mcleod,
%    Algorithm 757, MISCFUN: A software package to compute uncommon
%      special functions,
%    ACM Transactions on Mathematical Software,
%    Volume 22, Number 3, September 1996, pages 288-301.
%
%  Parameters:
%
%    Input, real ( kind = 8 ) XVALUE, the argument of the function.
%
%    Output, real ( kind = 8 ) TRAN02, the value of the function.
%
%
tran02result=[];
persistent atran eight firstCall four half k1 k2 nterms numexp numjn one rk rnumjn sum2 sumexp t tran02 valinf x xhigh1 xhigh2 xhigh3 xk xk1 xlow1 zero ; if isempty(firstCall),firstCall=1;end; 

if isempty(eight), eight = real( 8.0); end;
if isempty(four), four = real( 4.0d+00); end;
if isempty(half), half = 0.5d+00; end;
if isempty(k1), k1=0; end;
if isempty(k2), k2=0; end;
if isempty(nterms), nterms = 17; end;
if isempty(numexp), numexp=0; end;
if isempty(numjn), numjn = 2; end;
if isempty(one), one = real( 1.0); end;
if isempty(tran02result), tran02result=0; end;
if isempty(valinf), valinf= real( 0.32898681336964528729d+01); end;
if isempty(x), x=0; end;
if isempty(zero), zero = 0.0d+00; end;
if isempty(atran), atran=zeros(1,19+1); end;
if isempty(rk), rk=0; end;
if isempty(rnumjn), rnumjn=0; end;
if isempty(sumexp), sumexp=0; end;
if isempty(sum2), sum2=0; end;
if isempty(t), t=0; end;
if isempty(xhigh1), xhigh1=0; end;
if isempty(xhigh2), xhigh2=0; end;
if isempty(xhigh3), xhigh3=0; end;
if isempty(xk), xk=0; end;
if isempty(xk1), xk1=0; end;
if isempty(xlow1), xlow1=0; end;
if firstCall,   rnumjn=[2.0d0];  end;
if firstCall,   atran=[1.67176044643453850301d0,-0.14773535994679448986d0,0.1482138199469363384d-1,-0.141953303263056126d-2,0.13065413244157083d-3,-0.1171557958675790d-4,0.103334984457557d-5,-0.9019113042227d-7,0.781771698331d-8,-0.67445656840d-9,0.5799463945d-10,-0.497476185d-11,0.42596097d-12,-0.3642189d-13,0.311086d-14,-0.26547d-15,0.2264d-16,-0.193d-17,0.16d-18,-0.1d-19];  end;
%
%  Machine-dependent constants
%
if firstCall,   xlow1=[2.98023224d-8];  end;
if firstCall,   xhigh1=[36.04365668d0];  end;
if firstCall, xhigh3=[-36.73680056d0];  end;
if firstCall,   xhigh2=[9.00719925d15];  end;
firstCall=0;
%
x = xvalue;
if( x < zero )
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TRAN02 - Fatal error!');
writef(1,['%s','\n'], '  Argument X < 0.');
tran02result = zero;
elseif( x < xlow1 ) ;
tran02result =( x .^( numjn - 1 ) ) ./( rnumjn - one );
elseif( x <= four ) ;
t =((( x .* x ) ./ eight ) - half ) - half;
tran02result =( x .^( numjn - 1 ) ) .* cheval( nterms, atran, t );
else;
if( xhigh2 < x )
sumexp = one;
else;
if( x <= xhigh1 )
numexp = fix(fix( xhigh1 ./ x ) + 1);
t = exp( -x );
else;
numexp = 1;
t = one;
end;
rk = zero;
for k1 = 1: numexp;
rk = rk + one;
end; k1 = fix(numexp+1);
sumexp = zero;
for k1 = 1: numexp;
sum2 = one;
xk = one ./( rk .* x );
xk1 = one;
for k2 = 1: numjn;
sum2 = sum2 .* xk1 .* xk + one;
xk1 = xk1 + one;
end; k2 = fix(numjn+1);
sumexp = sumexp .* t + sum2;
rk = rk - one;
end; k1 = fix(numexp+1);
end;
t = rnumjn .* log( x ) - x + log( sumexp );
if( t < xhigh3 )
tran02result = valinf;
else;
tran02result = valinf - exp( t );
end;
end;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',xvalue); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [n_data, x, fx]=tran02_values( n_data, x, fx );
%*******************************************************************************
%
%! TRAN02_VALUES returns some values of the order 2 transportation function.
%
%  Discussion:
%
%    The function is defined by:
%
%      TRAN02(x) = Integral ( 0 <= t <= x ) t^2 exp(t) / ( exp(t) - 1 )^2 dt
%
%  Modified:
%
%    06 September 2004
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Milton Abramowitz and Irene Stegun,
%    Handbook of Mathematical Functions,
%    US Department of Commerce, 1964.
%
%    Allan McLeod,
%    Algorithm 757, MISCFUN: A software package to compute uncommon
%      special functions,
%    ACM Transactions on Mathematical Software,
%    Volume 22, Number 3, September 1996, pages 288-301.
%
%    Stephen Wolfram,
%    The Mathematica Book,
%    Fourth Edition,
%    Wolfram Media / Cambridge University Press, 1999.
%
%  Parameters:
%
%    Input/output, integer N_DATA.  The user sets N_DATA to 0 before the
%    first call.  On each call, the routine increments N_DATA by 1, and
%    returns the corresponding data; when there is no more data, the
%    output value of N_DATA will be 0 again.
%
%    Output, real ( kind = 8 ) X, the argument of the function.
%
%    Output, real ( kind = 8 ) FX, the value of the function.
%
%
persistent fx_vec n_max x_vec ; 

if isempty(n_max), n_max = 20; end;
%
if isempty(fx_vec), fx_vec([1:n_max]) =[real( 0.19531247930394515480d-02),real( 0.31249152314331109004d-01),real( 0.12494577194783451032d+00),real( 0.49655363615640595865d+00),real( 0.97303256135517012845d+00),real( 0.14121978695932525805d+01),real( 0.18017185674405776809d+01),real( 0.21350385339277043015d+01),real( 0.24110500490169534620d+01),real( 0.28066664045631179931d+01),real( 0.28777421863296234131d+01),real( 0.30391706043438554330d+01),real( 0.31125074928667355940d+01),real( 0.31656687817738577185d+01),real( 0.32623520367816009184d+01),real( 0.32843291144979517358d+01),real( 0.32897895167775788137d+01),real( 0.32898672226665499687d+01),real( 0.32898681336064325400d+01),real( 0.32898681336964528724d+01) ]; end;
if isempty(x_vec), x_vec([1:n_max]) =[real(   0.0019531250d+00),real(   0.0312500000d+00),real(   0.1250000000d+00),real(   0.5000000000d+00),real(   1.0000000000d+00),real(   1.5000000000d+00),real(   2.0000000000d+00),real(   2.5000000000d+00),real(   3.0000000000d+00),real(   4.0000000000d+00),real(   4.2500000000d+00),real(   5.0000000000d+00),real(   5.5000000000d+00),real(   6.0000000000d+00),real(   8.0000000000d+00),real(  10.0000000000d+00),real(  15.0000000000d+00),real(  20.0000000000d+00),real(  30.0000000000d+00),real(  50.0000000000d+00) ]; end;
%
if( n_data < 0 )
n_data = 0;
end;
n_data = fix(n_data + 1);
if( n_max < n_data )
n_data = 0;
x = 0.0d+00;
fx = 0.0d+00;
else;
x  = x_vec(n_data);
fx = fx_vec(n_data);
end;
return;
end
function [tran03result, xvalue ]=tran03( xvalue );
%*******************************************************************************
%
%! TRAN03 calculates the transport integral of order 3.
%
%  Discussion:
%
%    The function is defined by:
%
%      TRAN03(x) = Integral ( 0 <= t <= x ) t^3 * exp(t) / ( exp(t) - 1 )^2 dt
%
%    The program uses a Chebyshev series, the coefficients of which are
%    given to an accuracy of 20 decimal places.
%
%    This subroutine is set up to work on IEEE machines.
%
%  Modified:
%
%    07 August 2004
%
%  Author:
%
%    Allan McLeod,
%    Department of Mathematics and Statistics,
%    Paisley University, High Street, Paisley, Scotland, PA12BE
%    macl_ms0@paisley.ac.uk
%
%  Reference:
%
%    Allan Mcleod,
%    Algorithm 757, MISCFUN: A software package to compute uncommon
%      special functions,
%    ACM Transactions on Mathematical Software,
%    Volume 22, Number 3, September 1996, pages 288-301.
%
%  Parameters:
%
%    Input, real ( kind = 8 ) XVALUE, the argument of the function.
%
%    Output, real ( kind = 8 ) TRAN03, the value of the function.
%
%
tran03result=[];
persistent atran eight firstCall four half k1 k2 nterms numexp numjn one rk rnumjn sum2 sumexp t tran03 valinf x xhigh1 xhigh2 xhigh3 xk xk1 xlow1 xlow2 zero ; if isempty(firstCall),firstCall=1;end; 

if isempty(atran), atran=zeros(1,19+1); end;
if isempty(eight), eight = real( 8.0); end;
if isempty(four), four = real( 4.0d+00); end;
if isempty(half), half = 0.5d+00; end;
if isempty(k1), k1=0; end;
if isempty(k2), k2=0; end;
if isempty(nterms), nterms = 17; end;
if isempty(numexp), numexp=0; end;
if isempty(numjn), numjn=0; end;
if isempty(one), one = real( 1.0); end;
if isempty(tran03result), tran03result=0; end;
if isempty(x), x=0; end;
if isempty(zero), zero = 0.0d+00; end;
if isempty(rk), rk=0; end;
if isempty(rnumjn), rnumjn=0; end;
if isempty(sumexp), sumexp=0; end;
if isempty(sum2), sum2=0; end;
if isempty(t), t=0; end;
if isempty(valinf), valinf=0; end;
if isempty(xhigh1), xhigh1=0; end;
if isempty(xhigh2), xhigh2=0; end;
if isempty(xhigh3), xhigh3=0; end;
if isempty(xk), xk=0; end;
if isempty(xk1), xk1=0; end;
if isempty(xlow1), xlow1=0; end;
if isempty(xlow2), xlow2=0; end;
if firstCall,   numjn=[3];  end;
if firstCall, rnumjn=[3.0d0];  end;
if firstCall,   valinf=[0.72123414189575657124d1];  end;
if firstCall,   atran=[0.76201254324387200657d0,-0.10567438770505853250d0,0.1197780848196578097d-1,-0.121440152036983073d-2,0.11550997693928547d-3,-0.1058159921244229d-4,0.94746633853018d-6,-0.8362212128581d-7,0.731090992775d-8,-0.63505947788d-9,0.5491182819d-10,-0.473213954d-11,0.40676948d-12,-0.3489706d-13,0.298923d-14,-0.25574d-15,0.2186d-16,-0.187d-17,0.16d-18,-0.1d-19];  end;
%
%  Machine-dependent constants
%
if firstCall,   xlow1=[2.98023224d-8];  end;
if firstCall, xlow2=[2.10953733d-154];  end;
if firstCall,   xhigh1=[36.04365668d0];  end;
if firstCall, xhigh3=[-36.73680056d0];  end;
if firstCall,   xhigh2=[1.35107988d16];  end;
firstCall=0;
%
x = xvalue;
if( x < zero )
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TRAN03 - Fatal error!');
writef(1,['%s','\n'], '  Argument X < 0.');
tran03result = zero;
elseif( x < xlow2 ) ;
tran03result = zero;
elseif( x < xlow1 ) ;
tran03result =( x.^( numjn - 1 ) ) ./( rnumjn - one );
elseif( x <= four ) ;
t =((( x.*x ) ./ eight ) - half ) - half;
tran03result =( x.^( numjn - 1 ) ) .* cheval( nterms, atran, t );
else;
if( xhigh2 < x )
sumexp = one;
else;
if( x <= xhigh1 )
numexp = fix(fix( xhigh1 ./ x ) + 1);
t = exp( -x );
else;
numexp = 1;
t = one;
end;
rk = zero;
for k1 = 1: numexp;
rk = rk + one;
end; k1 = fix(numexp+1);
sumexp = zero;
for k1 = 1: numexp;
sum2 = one;
xk = one ./( rk .* x );
xk1 = one;
for k2 = 1: numjn;
sum2 = sum2 .* xk1 .* xk + one;
xk1 = xk1 + one;
end; k2 = fix(numjn+1);
sumexp = sumexp .* t + sum2;
rk = rk - one;
end; k1 = fix(numexp+1);
end;
t = rnumjn .* log( x ) - x + log( sumexp );
if( t < xhigh3 )
tran03result = valinf;
else;
tran03result = valinf - exp( t );
end;
end;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',xvalue); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [n_data, x, fx]=tran03_values( n_data, x, fx );
%*******************************************************************************
%
%! TRAN03_VALUES returns some values of the order 3 transportation function.
%
%  Discussion:
%
%    The function is defined by:
%
%      TRAN03(x) = Integral ( 0 <= t <= x ) t^3 * exp(t) / ( exp(t) - 1 )^2 dt
%
%  Modified:
%
%    06 September 2004
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Milton Abramowitz and Irene Stegun,
%    Handbook of Mathematical Functions,
%    US Department of Commerce, 1964.
%
%    Allan McLeod,
%    Algorithm 757, MISCFUN: A software package to compute uncommon
%      special functions,
%    ACM Transactions on Mathematical Software,
%    Volume 22, Number 3, September 1996, pages 288-301.
%
%    Stephen Wolfram,
%    The Mathematica Book,
%    Fourth Edition,
%    Wolfram Media / Cambridge University Press, 1999.
%
%  Parameters:
%
%    Input/output, integer N_DATA.  The user sets N_DATA to 0 before the
%    first call.  On each call, the routine increments N_DATA by 1, and
%    returns the corresponding data; when there is no more data, the
%    output value of N_DATA will be 0 again.
%
%    Output, real ( kind = 8 ) X, the argument of the function.
%
%    Output, real ( kind = 8 ) FX, the value of the function.
%
%
persistent fx_vec n_max x_vec ; 

if isempty(n_max), n_max = 20; end;
%
if isempty(fx_vec), fx_vec([1:n_max]) =[real( 0.19073483296476379584d-05),real( 0.48826138243180786081d-03),real( 0.78074163848431205820d-02),real( 0.12370868718812031049d+00),real( 0.47984100657241749994d+00),real( 0.10269431622039754738d+01),real( 0.17063547219458658863d+01),real( 0.24539217444475937661d+01),real( 0.32106046629422467723d+01),real( 0.45792174372291563703d+01),real( 0.48722022832940370805d+01),real( 0.56143866138422732286d+01),real( 0.59984455864575470009d+01),real( 0.63033953673480961120d+01),real( 0.69579908688361166266d+01),real( 0.71503227120085929750d+01),real( 0.72110731475871876393d+01),real( 0.72123221966388461839d+01),real( 0.72123414161609465119d+01),real( 0.72123414189575656868d+01) ]; end;
if isempty(x_vec), x_vec([1:n_max]) =[real(   0.0019531250d+00),real(   0.0312500000d+00),real(   0.1250000000d+00),real(   0.5000000000d+00),real(   1.0000000000d+00),real(   1.5000000000d+00),real(   2.0000000000d+00),real(   2.5000000000d+00),real(   3.0000000000d+00),real(   4.0000000000d+00),real(   4.2500000000d+00),real(   5.0000000000d+00),real(   5.5000000000d+00),real(   6.0000000000d+00),real(   8.0000000000d+00),real(  10.0000000000d+00),real(  15.0000000000d+00),real(  20.0000000000d+00),real(  30.0000000000d+00),real(  50.0000000000d+00) ]; end;
%
if( n_data < 0 )
n_data = 0;
end;
n_data = fix(n_data + 1);
if( n_max < n_data )
n_data = 0;
x = 0.0d+00;
fx = 0.0d+00;
else;
x  = x_vec(n_data);
fx = fx_vec(n_data);
end;
return;
end
function [tran04result, xvalue ]=tran04( xvalue );
%*******************************************************************************
%
%! TRAN04 calculates the transport integral of order 4.
%
%  Discussion:
%
%    The function is defined by:
%
%      TRAN04(x) = Integral ( 0 <= t <= x ) t^4 * exp(t) / ( exp(t) - 1 )^2 dt
%
%    The program uses a Chebyshev series, the coefficients of which are
%    given to an accuracy of 20 decimal places.
%
%    This subroutine is set up to work on IEEE machines.
%
%  Modified:
%
%    07 August 2004
%
%  Author:
%
%    Allan McLeod,
%    Department of Mathematics and Statistics,
%    Paisley University, High Street, Paisley, Scotland, PA12BE
%    macl_ms0@paisley.ac.uk
%
%  Reference:
%
%    Allan Mcleod,
%    Algorithm 757, MISCFUN: A software package to compute uncommon
%      special functions,
%    ACM Transactions on Mathematical Software,
%    Volume 22, Number 3, September 1996, pages 288-301.
%
%  Parameters:
%
%    Input, real ( kind = 8 ) XVALUE, the argument of the function.
%
%    Output, real ( kind = 8 ) TRAN04, the value of the function.
%
%
tran04result=[];
persistent atran eight firstCall four half k1 k2 nterms numexp numjn one rk rnumjn sum2 sumexp t tran04 valinf x xhigh1 xhigh2 xhigh3 xk xk1 xlow1 xlow2 zero ; if isempty(firstCall),firstCall=1;end; 

if isempty(eight), eight = real( 8.0); end;
if isempty(four), four = real( 4.0d+00); end;
if isempty(half), half = 0.5d+00; end;
if isempty(k1), k1=0; end;
if isempty(k2), k2=0; end;
if isempty(nterms), nterms = 17; end;
if isempty(numexp), numexp=0; end;
if isempty(numjn), numjn=0; end;
if isempty(one), one = real( 1.0); end;
if isempty(tran04result), tran04result=0; end;
if isempty(x), x=0; end;
if isempty(zero), zero = 0.0d+00; end;
if isempty(atran), atran=zeros(1,19+1); end;
if isempty(rk), rk=0; end;
if isempty(rnumjn), rnumjn=0; end;
if isempty(sumexp), sumexp=0; end;
if isempty(sum2), sum2=0; end;
if isempty(t), t=0; end;
if isempty(valinf), valinf=0; end;
if isempty(xhigh1), xhigh1=0; end;
if isempty(xhigh2), xhigh2=0; end;
if isempty(xhigh3), xhigh3=0; end;
if isempty(xk), xk=0; end;
if isempty(xk1), xk1=0; end;
if isempty(xlow1), xlow1=0; end;
if isempty(xlow2), xlow2=0; end;
if firstCall,   numjn=[4];  end;
if firstCall, rnumjn=[4.0d0];  end;
if firstCall,   valinf=[0.25975757609067316596d2];  end;
if firstCall,   atran=[0.48075709946151105786d0,-0.8175378810321083956d-1,0.1002700665975162973d-1,-0.105993393598201507d-2,0.10345062450304053d-3,-0.964427054858991d-5,0.87455444085147d-6,-0.7793212079811d-7,0.686498861410d-8,-0.59995710764d-9,0.5213662413d-10,-0.451183819d-11,0.38921592d-12,-0.3349360d-13,0.287667d-14,-0.24668d-15,0.2113d-16,-0.181d-17,0.15d-18,-0.1d-19];  end;
%
%  Machine-dependent constants
%
if firstCall,   xlow1=[2.98023224d-8];  end;
if firstCall, xlow2=[4.05653502d-103];  end;
if firstCall,   xhigh1=[36.04365668d0];  end;
if firstCall, xhigh3=[-36.73680056d0];  end;
if firstCall,   xhigh2=[1.80143985d16];  end;
firstCall=0;
%
x = xvalue;
if( x < zero )
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TRAN04 - Fatal error!');
writef(1,['%s','\n'], '  Argument X < 0.');
tran04result = zero;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',xvalue); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end;
%
%   Code for x < =  4.0
%
if( x <= four )
if( x < xlow2 )
tran04result = zero;
else;
if( x < xlow1 )
tran04result =( x .^( numjn-1 ) ) ./( rnumjn - one );
else;
t =((( x .* x ) ./ eight ) - half ) - half;
tran04result =( x .^( numjn-1 ) ) .* cheval( nterms, atran, t );
end;
end;
else;
%
%  Code for x > 4.0
%
if( xhigh2 < x )
sumexp = one;
else;
if( x <= xhigh1 )
numexp = fix(fix( xhigh1 ./ x ) + 1);
t = exp( -x );
else;
numexp = 1;
t = one;
end;
rk = zero;
for k1 = 1: numexp;
rk = rk + one;
end; k1 = fix(numexp+1);
sumexp = zero;
for k1 = 1: numexp;
sum2 = one;
xk = one ./( rk .* x );
xk1 = one;
for k2 = 1: numjn;
sum2 = sum2 .* xk1 .* xk + one;
xk1 = xk1 + one;
end; k2 = fix(numjn+1);
sumexp = sumexp .* t + sum2;
rk = rk - one;
end; k1 = fix(numexp+1);
end;
t = rnumjn .* log( x ) - x + log( sumexp );
if( t < xhigh3 )
tran04result = valinf;
else;
tran04result = valinf - exp( t );
end;
end;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',xvalue); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [n_data, x, fx]=tran04_values( n_data, x, fx );
%*******************************************************************************
%
%! TRAN04_VALUES returns some values of the order 4 transportation function.
%
%  Discussion:
%
%    The function is defined by:
%
%      TRAN04(x) = Integral ( 0 <= t <= x ) t^4 * exp(t) / ( exp(t) - 1 )^2 dt
%
%  Modified:
%
%    06 September 2004
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Milton Abramowitz and Irene Stegun,
%    Handbook of Mathematical Functions,
%    US Department of Commerce, 1964.
%
%    Allan McLeod,
%    Algorithm 757, MISCFUN: A software package to compute uncommon
%      special functions,
%    ACM Transactions on Mathematical Software,
%    Volume 22, Number 3, September 1996, pages 288-301.
%
%    Stephen Wolfram,
%    The Mathematica Book,
%    Fourth Edition,
%    Wolfram Media / Cambridge University Press, 1999.
%
%  Parameters:
%
%    Input/output, integer N_DATA.  The user sets N_DATA to 0 before the
%    first call.  On each call, the routine increments N_DATA by 1, and
%    returns the corresponding data; when there is no more data, the
%    output value of N_DATA will be 0 again.
%
%    Output, real ( kind = 8 ) X, the argument of the function.
%
%    Output, real ( kind = 8 ) FX, the value of the function.
%
%
persistent fx_vec n_max x_vec ; 

if isempty(n_max), n_max = 20; end;
%
if isempty(fx_vec), fx_vec([1:n_max]) =[real( 0.24835263919461834041d-08),real( 0.10172029353616724881d-04),real( 0.65053332405940765479d-03),real( 0.41150448004155727767d-01),real( 0.31724404523442648241d+00),real( 0.10079442901142373591d+01),real( 0.22010881024333408363d+01),real( 0.38846508619156545210d+01),real( 0.59648223973714765245d+01),real( 0.10731932392998622219d+02),real( 0.11940028876819364777d+02),real( 0.15359784316882182982d+02),real( 0.17372587633093742893d+02),real( 0.19122976016053166969d+02),real( 0.23583979156921941515d+02),real( 0.25273667677030441733d+02),real( 0.25955198214572256372d+02),real( 0.25975350935212241910d+02),real( 0.25975757522084093747d+02),real( 0.25975757609067315288d+02) ]; end;
if isempty(x_vec), x_vec([1:n_max]) =[real(   0.0019531250d+00),real(   0.0312500000d+00),real(   0.1250000000d+00),real(   0.5000000000d+00),real(   1.0000000000d+00),real(   1.5000000000d+00),real(   2.0000000000d+00),real(   2.5000000000d+00),real(   3.0000000000d+00),real(   4.0000000000d+00),real(   4.2500000000d+00),real(   5.0000000000d+00),real(   5.5000000000d+00),real(   6.0000000000d+00),real(   8.0000000000d+00),real(  10.0000000000d+00),real(  15.0000000000d+00),real(  20.0000000000d+00),real(  30.0000000000d+00),real(  50.0000000000d+00) ]; end;
%
if( n_data < 0 )
n_data = 0;
end;
n_data = fix(n_data + 1);
if( n_max < n_data )
n_data = 0;
x = 0.0d+00;
fx = 0.0d+00;
else;
x  = x_vec(n_data);
fx = fx_vec(n_data);
end;
return;
end
function [tran05result, xvalue ]=tran05( xvalue );
%*******************************************************************************
%
%! TRAN05 calculates the transport integral of order 5.
%
%  Discussion:
%
%    The function is defined by:
%
%      TRAN05(x) = Integral ( 0 <= t <= x ) t^5 * exp(t) / ( exp(t) - 1 )^2 dt
%
%    The program uses a Chebyshev series, the coefficients of which are
%    given to an accuracy of 20 decimal places.
%
%    This subroutine is set up to work on IEEE machines.
%
%  Modified:
%
%    07 August 2004
%
%  Author:
%
%    Allan McLeod,
%    Department of Mathematics and Statistics,
%    Paisley University, High Street, Paisley, Scotland, PA12BE
%    macl_ms0@paisley.ac.uk
%
%  Reference:
%
%    Allan Mcleod,
%    Algorithm 757, MISCFUN: A software package to compute uncommon
%      special functions,
%    ACM Transactions on Mathematical Software,
%    Volume 22, Number 3, September 1996, pages 288-301.
%
%  Parameters:
%
%    Input, real ( kind = 8 ) XVALUE, the argument of the function.
%
%    Output, real ( kind = 8 ) TRAN05, the value of the function.
%
%
tran05result=[];
persistent atran eight firstCall four half k1 k2 nterms numexp numjn one rk rnumjn sum2 sumexp t tran05 valinf x xhigh1 xhigh2 xhigh3 xk xk1 xlow1 xlow2 zero ; if isempty(firstCall),firstCall=1;end; 

if isempty(eight), eight = real( 8.0); end;
if isempty(four), four = real( 4.0d+00); end;
if isempty(half), half = 0.5d+00; end;
if isempty(k1), k1=0; end;
if isempty(k2), k2=0; end;
if isempty(nterms), nterms = 17; end;
if isempty(numexp), numexp=0; end;
if isempty(numjn), numjn=0; end;
if isempty(one), one = real( 1.0); end;
if isempty(tran05result), tran05result=0; end;
if isempty(x), x=0; end;
if isempty(zero), zero = 0.0d+00; end;
if isempty(atran), atran=zeros(1,19+1); end;
if isempty(rk), rk=0; end;
if isempty(rnumjn), rnumjn=0; end;
if isempty(sumexp), sumexp=0; end;
if isempty(sum2), sum2=0; end;
if isempty(t), t=0; end;
if isempty(valinf), valinf=0; end;
if isempty(xhigh1), xhigh1=0; end;
if isempty(xhigh2), xhigh2=0; end;
if isempty(xhigh3), xhigh3=0; end;
if isempty(xk), xk=0; end;
if isempty(xk1), xk1=0; end;
if isempty(xlow1), xlow1=0; end;
if isempty(xlow2), xlow2=0; end;
if firstCall,   numjn=[5];  end;
if firstCall, rnumjn=[5.0d0];  end;
if firstCall,   valinf=[0.12443133061720439116d3];  end;
if firstCall,   atran=[0.34777777713391078928d0,-0.6645698897605042801d-1,0.861107265688330882d-2,-0.93966822237555384d-3,0.9363248060815134d-4,-0.885713193408328d-5,0.81191498914503d-6,-0.7295765423277d-7,0.646971455045d-8,-0.56849028255d-9,0.4962559787d-10,-0.431093996d-11,0.37310094d-12,-0.3219769d-13,0.277220d-14,-0.23824d-15,0.2044d-16,-0.175d-17,0.15d-18,-0.1d-19];  end;
%
%  Machine-dependent constants
%
if firstCall,   xlow1=[2.98023224d-8];  end;
if firstCall, xlow2=[1.72723372d-77];  end;
if firstCall,   xhigh1=[36.04365668d0];  end;
if firstCall, xhigh3=[-36.73680056d0];  end;
if firstCall,   xhigh2=[2.25179981d16];  end;
firstCall=0;
%
x = xvalue;
if( x < zero )
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TRAN05 - Fatal error!');
writef(1,['%s','\n'], '  Argument X < 0.');
tran05result = zero;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',xvalue); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end;
%
%   Code for x < =  4.0
%
if( x <= four )
if( x < xlow2 )
tran05result = zero;
else;
if( x < xlow1 )
tran05result =( x .^( numjn - 1 ) ) ./( rnumjn - one );
else;
t =((( x .* x ) ./ eight ) - half ) - half;
tran05result =( x .^( numjn-1 ) ) .* cheval( nterms, atran, t );
end;
end;
else;
%
%  Code for x > 4.0
%
if( xhigh2 < x )
sumexp = one;
else;
if( x <= xhigh1 )
numexp = fix(fix( xhigh1 ./ x )  + 1);
t = exp( -x );
else;
numexp = 1;
t = one;
end;
rk = zero;
for k1 = 1: numexp;
rk = rk + one;
end; k1 = fix(numexp+1);
sumexp = zero;
for k1 = 1: numexp;
sum2 = one;
xk = one ./( rk .* x );
xk1 = one;
for k2 = 1: numjn;
sum2 = sum2 .* xk1 .* xk + one;
xk1 = xk1 + one;
end; k2 = fix(numjn+1);
sumexp = sumexp .* t + sum2;
rk = rk - one;
end; k1 = fix(numexp+1);
end;
t = rnumjn .* log( x ) - x + log( sumexp );
if( t < xhigh3 )
tran05result = valinf;
else;
tran05result = valinf - exp( t );
end;
end;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',xvalue); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [n_data, x, fx]=tran05_values( n_data, x, fx );
%*******************************************************************************
%
%! TRAN05_VALUES returns some values of the order 5 transportation function.
%
%  Discussion:
%
%    The function is defined by:
%
%      TRAN05(x) = Integral ( 0 <= t <= x ) t^5 * exp(t) / ( exp(t) - 1 )^2 dt
%
%  Modified:
%
%    06 September 2004
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Milton Abramowitz and Irene Stegun,
%    Handbook of Mathematical Functions,
%    US Department of Commerce, 1964.
%
%    Allan McLeod,
%    Algorithm 757, MISCFUN: A software package to compute uncommon
%      special functions,
%    ACM Transactions on Mathematical Software,
%    Volume 22, Number 3, September 1996, pages 288-301.
%
%    Stephen Wolfram,
%    The Mathematica Book,
%    Fourth Edition,
%    Wolfram Media / Cambridge University Press, 1999.
%
%  Parameters:
%
%    Input/output, integer N_DATA.  The user sets N_DATA to 0 before the
%    first call.  On each call, the routine increments N_DATA by 1, and
%    returns the corresponding data; when there is no more data, the
%    output value of N_DATA will be 0 again.
%
%    Output, real ( kind = 8 ) X, the argument of the function.
%
%    Output, real ( kind = 8 ) FX, the value of the function.
%
%
persistent fx_vec n_max x_vec ; 

if isempty(n_max), n_max = 20; end;
%
if isempty(fx_vec), fx_vec([1:n_max]) =[real( 0.36379780361036116971d-11),real( 0.23840564453948442379d-06),real( 0.60982205372226969189d-04),real( 0.15410004586376649337d-01),real( 0.23661587923909478926d+00),real( 0.11198756851307629651d+01),real( 0.32292901663684049171d+01),real( 0.70362973105160654056d+01),real( 0.12770557691044159511d+02),real( 0.29488339015245845447d+02),real( 0.34471340540362254586d+02),real( 0.50263092218175187785d+02),real( 0.60819909101127165207d+02),real( 0.70873334429213460498d+02),real( 0.10147781242977788097d+03),real( 0.11638074540242071077d+03),real( 0.12409623901262967878d+03),real( 0.12442270155632550228d+03),real( 0.12443132790838589548d+03),real( 0.12443133061720432435d+03) ]; end;
if isempty(x_vec), x_vec([1:n_max]) =[real(   0.0019531250d+00),real(   0.0312500000d+00),real(   0.1250000000d+00),real(   0.5000000000d+00),real(   1.0000000000d+00),real(   1.5000000000d+00),real(   2.0000000000d+00),real(   2.5000000000d+00),real(   3.0000000000d+00),real(   4.0000000000d+00),real(   4.2500000000d+00),real(   5.0000000000d+00),real(   5.5000000000d+00),real(   6.0000000000d+00),real(   8.0000000000d+00),real(  10.0000000000d+00),real(  15.0000000000d+00),real(  20.0000000000d+00),real(  30.0000000000d+00),real(  50.0000000000d+00) ]; end;
%
if( n_data < 0 )
n_data = 0;
end;
n_data = fix(n_data + 1);
if( n_max < n_data )
n_data = 0;
x = 0.0d+00;
fx = 0.0d+00;
else;
x  = x_vec(n_data);
fx = fx_vec(n_data);
end;
return;
end
function [tran06result, xvalue ]=tran06( xvalue );
%*******************************************************************************
%
%! TRAN06 calculates the transport integral of order 6.
%
%  Discussion:
%
%    The function is defined by:
%
%      TRAN06(x) = Integral ( 0 <= t <= x ) t^6 * exp(t) / ( exp(t) - 1 )^2 dt
%
%    The program uses a Chebyshev series, the coefficients of which are
%    given to an accuracy of 20 decimal places.
%
%    This subroutine is set up to work on IEEE machines.
%
%  Modified:
%
%    07 August 2004
%
%  Author:
%
%    Allan McLeod,
%    Department of Mathematics and Statistics,
%    Paisley University, High Street, Paisley, Scotland, PA12BE
%    macl_ms0@paisley.ac.uk
%
%  Reference:
%
%    Allan Mcleod,
%    Algorithm 757, MISCFUN: A software package to compute uncommon
%      special functions,
%    ACM Transactions on Mathematical Software,
%    Volume 22, Number 3, September 1996, pages 288-301.
%
%  Parameters:
%
%    Input, real ( kind = 8 ) XVALUE, the argument of the function.
%
%    Output, real ( kind = 8 ) TRAN06, the value of the function.
%
%
tran06result=[];
persistent atran eight firstCall four half k1 k2 nterms numexp numjn one rk rnumjn sum2 sumexp t tran06 valinf x xhigh1 xhigh2 xhigh3 xk xk1 xlow1 xlow2 zero ; if isempty(firstCall),firstCall=1;end; 

if isempty(eight), eight = real( 8.0); end;
if isempty(four), four = real( 4.0d+00); end;
if isempty(half), half = 0.5d+00; end;
if isempty(k1), k1=0; end;
if isempty(k2), k2=0; end;
if isempty(nterms), nterms = 17; end;
if isempty(numexp), numexp=0; end;
if isempty(numjn), numjn=0; end;
if isempty(one), one = real( 1.0); end;
if isempty(tran06result), tran06result=0; end;
if isempty(x), x=0; end;
if isempty(zero), zero = 0.0d+00; end;
if isempty(atran), atran=zeros(1,19+1); end;
if isempty(rk), rk=0; end;
if isempty(rnumjn), rnumjn=0; end;
if isempty(sumexp), sumexp=0; end;
if isempty(sum2), sum2=0; end;
if isempty(t), t=0; end;
if isempty(valinf), valinf=0; end;
if isempty(xhigh1), xhigh1=0; end;
if isempty(xhigh2), xhigh2=0; end;
if isempty(xhigh3), xhigh3=0; end;
if isempty(xk), xk=0; end;
if isempty(xk1), xk1=0; end;
if isempty(xlow1), xlow1=0; end;
if isempty(xlow2), xlow2=0; end;
if firstCall,   numjn=[6];  end;
if firstCall, rnumjn=[6.0d0];  end;
if firstCall,   valinf=[0.73248700462880338059d3];  end;
if firstCall,   atran=[0.27127335397840008227d0,-0.5588610553191453393d-1,0.753919513290083056d-2,-0.84351138579211219d-3,0.8549098079676702d-4,-0.818715493293098d-5,0.75754240427986d-6,-0.6857306541831d-7,0.611700376031d-8,-0.54012707024d-9,0.4734306435d-10,-0.412701055d-11,0.35825603d-12,-0.3099752d-13,0.267501d-14,-0.23036d-15,0.1980d-16,-0.170d-17,0.15d-18,-0.1d-19];  end;
%
%  Machine-dependent constants
%
if firstCall,   xlow1=[2.98023224d-8];  end;
if firstCall, xlow2=[4.06689432d-62];  end;
if firstCall,   xhigh1=[36.04365668d0];  end;
if firstCall, xhigh3=[-36.73680056d0];  end;
if firstCall,   xhigh2=[2.70215977d16];  end;
firstCall=0;
%
x = xvalue;
if( x < zero )
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TRAN06 - Fatal error!');
writef(1,['%s','\n'], '  Argument X < 0.');
tran06result = zero;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',xvalue); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end;
%
%   Code for x < =  4 .0
%
if( x <= four )
if( x < xlow2 )
tran06result = zero;
else;
if( x < xlow1 )
tran06result =( x .^( numjn-1 ) ) ./( rnumjn - one );
else;
t =((( x .* x ) ./ eight ) - half ) - half;
tran06result =( x .^( numjn-1 )  ) .* cheval( nterms, atran, t );
end;
end;
else;
%
%  Code for x > 4 .0
%
if( xhigh2 < x )
sumexp = one;
else;
if( x <= xhigh1 )
numexp = fix(fix( xhigh1 ./ x ) + 1);
t = exp( - x );
else;
numexp = 1;
t = one;
end;
rk = zero;
for k1 = 1: numexp;
rk = rk + one;
end; k1 = fix(numexp+1);
sumexp = zero;
for k1 = 1: numexp;
sum2 = one;
xk = one ./( rk .* x );
xk1 = one;
for k2 = 1: numjn;
sum2 = sum2 .* xk1 .* xk + one;
xk1 = xk1 + one;
end; k2 = fix(numjn+1);
sumexp = sumexp .* t + sum2;
rk = rk - one;
end; k1 = fix(numexp+1);
end;
t = rnumjn .* log( x ) - x + log( sumexp );
if( t < xhigh3 )
tran06result = valinf;
else;
tran06result = valinf - exp( t );
end;
end;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',xvalue); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [n_data, x, fx]=tran06_values( n_data, x, fx );
%*******************************************************************************
%
%! TRAN06_VALUES returns some values of the order 6 transportation function.
%
%  Discussion:
%
%    The function is defined by:
%
%      TRAN06(x) = Integral ( 0 <= t <= x ) t^6 * exp(t) / ( exp(t) - 1 )^2 dt
%
%  Modified:
%
%    06 September 2004
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Milton Abramowitz and Irene Stegun,
%    Handbook of Mathematical Functions,
%    US Department of Commerce, 1964.
%
%    Allan McLeod,
%    Algorithm 757, MISCFUN: A software package to compute uncommon
%      special functions,
%    ACM Transactions on Mathematical Software,
%    Volume 22, Number 3, September 1996, pages 288-301.
%
%    Stephen Wolfram,
%    The Mathematica Book,
%    Fourth Edition,
%    Wolfram Media / Cambridge University Press, 1999.
%
%  Parameters:
%
%    Input/output, integer N_DATA.  The user sets N_DATA to 0 before the
%    first call.  On each call, the routine increments N_DATA by 1, and
%    returns the corresponding data; when there is no more data, the
%    output value of N_DATA will be 0 again.
%
%    Output, real ( kind = 8 ) X, the argument of the function.
%
%    Output, real ( kind = 8 ) FX, the value of the function.
%
%
persistent fx_vec n_max x_vec ; 

if isempty(n_max), n_max = 20; end;
%
if isempty(fx_vec), fx_vec([1:n_max]) =[real( 0.56843405953641209574d-14),real( 0.59601180165247401484d-08),real( 0.60978424397580572815d-05),real( 0.61578909866319494394d-02),real( 0.18854360275680840514d+00),real( 0.13319251347921659134d+01),real( 0.50857202271697616755d+01),real( 0.13729222365466557122d+02),real( 0.29579592481641441292d+02),real( 0.88600835706899853768d+02),real( 0.10916037113373004909d+03),real( 0.18224323749575359518d+03),real( 0.23765383125586756031d+03),real( 0.29543246745959381136d+03),real( 0.50681244381280455592d+03),real( 0.63878231134946125623d+03),real( 0.72699203556994876111d+03),real( 0.73230331643146851717d+03),real( 0.73248692015882096369d+03),real( 0.73248700462879996604d+03) ]; end;
if isempty(x_vec), x_vec([1:n_max]) =[real(   0.0019531250d+00),real(   0.0312500000d+00),real(   0.1250000000d+00),real(   0.5000000000d+00),real(   1.0000000000d+00),real(   1.5000000000d+00),real(   2.0000000000d+00),real(   2.5000000000d+00),real(   3.0000000000d+00),real(   4.0000000000d+00),real(   4.2500000000d+00),real(   5.0000000000d+00),real(   5.5000000000d+00),real(   6.0000000000d+00),real(   8.0000000000d+00),real(  10.0000000000d+00),real(  15.0000000000d+00),real(  20.0000000000d+00),real(  30.0000000000d+00),real(  50.0000000000d+00) ]; end;
%
if( n_data < 0 )
n_data = 0;
end;
n_data = fix(n_data + 1);
if( n_max < n_data )
n_data = 0;
x = 0.0d+00;
fx = 0.0d+00;
else;
x  = x_vec(n_data);
fx = fx_vec(n_data);
end;
return;
end
function [tran07result, xvalue ]=tran07( xvalue );
%*******************************************************************************
%
%! TRAN07 calculates the transport integral of order 7.
%
%  Discussion:
%
%    The function is defined by:
%
%      TRAN07(x) = Integral ( 0 <= t <= x ) t^7 * exp(t) / ( exp(t) - 1 )^2 dt
%
%    The program uses a Chebyshev series, the coefficients of which are
%    given to an accuracy of 20 decimal places.
%
%    This subroutine is set up to work on IEEE machines.
%
%  Modified:
%
%    07 August 2004
%
%  Author:
%
%    Allan McLeod,
%    Department of Mathematics and Statistics,
%    Paisley University, High Street, Paisley, Scotland, PA12BE
%    macl_ms0@paisley.ac.uk
%
%  Reference:
%
%    Allan Mcleod,
%    Algorithm 757, MISCFUN: A software package to compute uncommon
%      special functions,
%    ACM Transactions on Mathematical Software,
%    Volume 22, Number 3, September 1996, pages 288-301.
%
%  Parameters:
%
%    Input, real ( kind = 8 ) XVALUE, the argument of the function.
%
%    Output, real ( kind = 8 ) TRAN07, the value of the function.
%
%
tran07result=[];
persistent atran eight firstCall four half k1 k2 nterms numexp numjn one rk rnumjn sum2 sumexp t tran07 valinf x xhigh1 xhigh2 xhigh3 xk xk1 xlow1 xlow2 zero ; if isempty(firstCall),firstCall=1;end; 

if isempty(eight), eight = real( 8.0); end;
if isempty(four), four = real( 4.0d+00); end;
if isempty(half), half = 0.5d+00; end;
if isempty(k1), k1=0; end;
if isempty(k2), k2=0; end;
if isempty(nterms), nterms = 17; end;
if isempty(numexp), numexp=0; end;
if isempty(numjn), numjn=0; end;
if isempty(one), one = real( 1.0); end;
if isempty(tran07result), tran07result=0; end;
if isempty(x), x=0; end;
if isempty(zero), zero = 0.0d+00; end;
if isempty(atran), atran=zeros(1,19+1); end;
if isempty(rk), rk=0; end;
if isempty(rnumjn), rnumjn=0; end;
if isempty(sumexp), sumexp=0; end;
if isempty(sum2), sum2=0; end;
if isempty(t), t=0; end;
if isempty(valinf), valinf=0; end;
if isempty(xhigh1), xhigh1=0; end;
if isempty(xhigh2), xhigh2=0; end;
if isempty(xhigh3), xhigh3=0; end;
if isempty(xk), xk=0; end;
if isempty(xk1), xk1=0; end;
if isempty(xlow1), xlow1=0; end;
if isempty(xlow2), xlow2=0; end;
if firstCall,   numjn=[7];  end;
if firstCall, rnumjn=[7.0d0];  end;
if firstCall,   valinf=[0.50820803580048910473d4];  end;
if firstCall,   atran=[0.22189250734010404423d0,-0.4816751061177993694d-1,0.670092448103153629d-2,-0.76495183443082557d-3,0.7863485592348690d-4,-0.761025180887504d-5,0.70991696299917d-6,-0.6468025624903d-7,0.580039233960d-8,-0.51443370149d-9,0.4525944183d-10,-0.395800363d-11,0.34453785d-12,-0.2988292d-13,0.258434d-14,-0.22297d-15,0.1920d-16,-0.165d-17,0.14d-18,-0.1d-19];  end;
%
%  Machine-dependent constants
%
if firstCall,   xlow1=[2.98023224d-8];  end;
if firstCall, xlow2=[7.14906557d-52];  end;
if firstCall,   xhigh1=[36.04365668d0];  end;
if firstCall, xhigh3=[-36.73680056d0];  end;
if firstCall,   xhigh2=[3.15251973d16];  end;
firstCall=0;
%
x = xvalue;
if( x < zero )
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TRAN07 - Fatal error!');
writef(1,['%s','\n'], '  Argument X < 0.');
tran07result = zero;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',xvalue); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end;
%
%   Code for x <= 4.0
%
if( x <= four )
if( x < xlow2 )
tran07result = zero;
else;
if( x < xlow1 )
tran07result =( x.^(numjn-1) ) ./( rnumjn - one );
else;
t =((( x .* x ) ./ eight ) - half ) - half;
tran07result =( x.^(numjn-1) ) .* cheval( nterms, atran, t );
end;
end;
else;
%
%  Code for x > 4.0
%
if( xhigh2 < x )
sumexp = one;
else;
if( x <= xhigh1 )
numexp = fix(fix( xhigh1 ./ x ) + 1);
t = exp( -x );
else;
numexp = 1;
t = one;
end;
rk = zero;
for k1 = 1: numexp;
rk = rk + one;
end; k1 = fix(numexp+1);
sumexp = zero;
for k1 = 1: numexp;
sum2 = one;
xk = one ./( rk .* x );
xk1 = one;
for k2 = 1: numjn;
sum2 = sum2 .* xk1 .* xk + one;
xk1 = xk1 + one;
end; k2 = fix(numjn+1);
sumexp = sumexp .* t + sum2;
rk = rk - one;
end; k1 = fix(numexp+1);
end;
t = rnumjn .* log( x ) - x + log( sumexp );
if( t < xhigh3 )
tran07result = valinf;
else;
tran07result = valinf - exp( t );
end;
end;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',xvalue); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [n_data, x, fx]=tran07_values( n_data, x, fx );
%*******************************************************************************
%
%! TRAN07_VALUES returns some values of the order 7 transportation function.
%
%  Discussion:
%
%    The function is defined by:
%
%      TRAN07(x) = Integral ( 0 <= t <= x ) t^7 * exp(t) / ( exp(t) - 1 )^2 dt
%
%  Modified:
%
%    06 September 2004
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Milton Abramowitz and Irene Stegun,
%    Handbook of Mathematical Functions,
%    US Department of Commerce, 1964.
%
%    Allan McLeod,
%    Algorithm 757, MISCFUN: A software package to compute uncommon
%      special functions,
%    ACM Transactions on Mathematical Software,
%    Volume 22, Number 3, September 1996, pages 288-301.
%
%    Stephen Wolfram,
%    The Mathematica Book,
%    Fourth Edition,
%    Wolfram Media / Cambridge University Press, 1999.
%
%  Parameters:
%
%    Input/output, integer N_DATA.  The user sets N_DATA to 0 before the
%    first call.  On each call, the routine increments N_DATA by 1, and
%    returns the corresponding data; when there is no more data, the
%    output value of N_DATA will be 0 again.
%
%    Output, real ( kind = 8 ) X, the argument of the function.
%
%    Output, real ( kind = 8 ) FX, the value of the function.
%
%
persistent fx_vec n_max x_vec ; 

if isempty(n_max), n_max = 20; end;
%
if isempty(fx_vec), fx_vec([1:n_max]) =[real( 0.92518563327283409427d-17),real( 0.15521095556949867541d-09),real( 0.63516238373841716290d-06),real( 0.25638801246626135714d-02),real( 0.15665328993811649746d+00),real( 0.16538225039181097423d+01),real( 0.83763085709508211054d+01),real( 0.28078570717830763747d+02),real( 0.72009676046751991365d+02),real( 0.28174905701691911450d+03),real( 0.36660227975327792529d+03),real( 0.70556067982603601123d+03),real( 0.99661927562755629434d+03),real( 0.13288914430417403901d+04),real( 0.27987640273169129925d+04),real( 0.39721376409416504325d+04),real( 0.49913492839319899726d+04),real( 0.50781562639825019000d+04),real( 0.50820777202028708434d+04),real( 0.50820803580047164618d+04) ]; end;
if isempty(x_vec), x_vec([1:n_max]) =[real(   0.0019531250d+00),real(   0.0312500000d+00),real(   0.1250000000d+00),real(   0.5000000000d+00),real(   1.0000000000d+00),real(   1.5000000000d+00),real(   2.0000000000d+00),real(   2.5000000000d+00),real(   3.0000000000d+00),real(   4.0000000000d+00),real(   4.2500000000d+00),real(   5.0000000000d+00),real(   5.5000000000d+00),real(   6.0000000000d+00),real(   8.0000000000d+00),real(  10.0000000000d+00),real(  15.0000000000d+00),real(  20.0000000000d+00),real(  30.0000000000d+00),real(  50.0000000000d+00) ]; end;
%
if( n_data < 0 )
n_data = 0;
end;
n_data = fix(n_data + 1);
if( n_max < n_data )
n_data = 0;
x = 0.0d+00;
fx = 0.0d+00;
else;
x  = x_vec(n_data);
fx = fx_vec(n_data);
end;
return;
end
function [tran08result, xvalue ]=tran08( xvalue );
%*******************************************************************************
%
%! TRAN08 calculates the transport integral of order 8.
%
%  Discussion:
%
%    The function is defined by:
%
%      TRAN08(x) = Integral ( 0 <= t <= x ) t^8 * exp(t) / ( exp(t) - 1 )^2 dt
%
%    The program uses a Chebyshev series, the coefficients of which are
%    given to an accuracy of 20 decimal places.
%
%    This subroutine is set up to work on IEEE machines.
%
%  Modified:
%
%    07 August 2004
%
%  Author:
%
%    Allan McLeod,
%    Department of Mathematics and Statistics,
%    Paisley University, High Street, Paisley, Scotland, PA12BE
%    macl_ms0@paisley.ac.uk
%
%  Reference:
%
%    Allan Mcleod,
%    Algorithm 757, MISCFUN: A software package to compute uncommon
%      special functions,
%    ACM Transactions on Mathematical Software,
%    Volume 22, Number 3, September 1996, pages 288-301.
%
%  Parameters:
%
%    Input, real ( kind = 8 ) XVALUE, the argument of the function.
%
%    Output, real ( kind = 8 ) TRAN08, the value of the function.
%
%
tran08result=[];
persistent atran eight firstCall four half k1 k2 nterms numexp numjn one rk rnumjn sum2 sumexp t tran08 valinf x xhigh1 xhigh2 xhigh3 xk xk1 xlow1 xlow2 zero ; if isempty(firstCall),firstCall=1;end; 

if isempty(eight), eight = real( 8.0); end;
if isempty(four), four = real( 4.0d+00); end;
if isempty(half), half = 0.5d+00; end;
if isempty(k1), k1=0; end;
if isempty(k2), k2=0; end;
if isempty(nterms), nterms = 17; end;
if isempty(numexp), numexp=0; end;
if isempty(numjn), numjn=0; end;
if isempty(one), one = real( 1.0); end;
if isempty(tran08result), tran08result=0; end;
if isempty(x), x=0; end;
if isempty(zero), zero = 0.0d+00; end;
if isempty(atran), atran=zeros(1,19+1); end;
if isempty(rk), rk=0; end;
if isempty(rnumjn), rnumjn=0; end;
if isempty(sumexp), sumexp=0; end;
if isempty(sum2), sum2=0; end;
if isempty(t), t=0; end;
if isempty(valinf), valinf=0; end;
if isempty(xhigh1), xhigh1=0; end;
if isempty(xhigh2), xhigh2=0; end;
if isempty(xhigh3), xhigh3=0; end;
if isempty(xk), xk=0; end;
if isempty(xk1), xk1=0; end;
if isempty(xlow1), xlow1=0; end;
if isempty(xlow2), xlow2=0; end;
if firstCall,   numjn=[8];  end;
if firstCall, rnumjn=[8.0d0];  end;
if firstCall,   valinf=[0.40484399001901115764d5];  end;
if firstCall,   atran=[0.18750695774043719233d0,-0.4229527646093673337d-1,0.602814856929065592d-2,-0.69961054811814776d-3,0.7278482421298789d-4,-0.710846250050067d-5,0.66786706890115d-6,-0.6120157501844d-7,0.551465264474d-8,-0.49105307052d-9,0.4335000869d-10,-0.380218700d-11,0.33182369d-12,-0.2884512d-13,0.249958d-14,-0.21605d-15,0.1863d-16,-0.160d-17,0.14d-18,-0.1d-19];  end;
%
%  Machine-dependent constants
%
if firstCall,   xlow1=[2.98023224d-8];  end;
if firstCall, xlow2=[1.48029723d-44];  end;
if firstCall,   xhigh1=[36.04365668d0];  end;
if firstCall, xhigh3=[-36.73680056d0];  end;
if firstCall,   xhigh2=[3.6028797d16];  end;
firstCall=0;
%
x = xvalue;
if( x < zero )
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TRAN08 - Fatal error!');
writef(1,['%s','\n'], '  Argument X < 0.');
tran08result = zero;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',xvalue); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end;
%
%   Code for x < =  4.0
%
if( x <= four )
if( x < xlow2 )
tran08result = zero;
else;
if( x < xlow1 )
tran08result =( x .^( numjn - 1 ) ) ./( rnumjn - one );
else;
t =((( x .* x ) ./ eight ) - half ) - half;
tran08result =( x .^( numjn - 1 ) ) .* cheval( nterms, atran, t );
end;
end;
else;
%
%  Code for x > 4.0
%
if( xhigh2 < x )
sumexp = one;
else;
if( x <= xhigh1 )
numexp = fix(fix( xhigh1 ./ x ) + 1);
t = exp( - x );
else;
numexp = 1;
t = one;
end;
rk = zero;
for k1 = 1: numexp;
rk = rk + one;
end; k1 = fix(numexp+1);
sumexp = zero;
for k1 = 1: numexp;
sum2 = one;
xk = one ./( rk .* x );
xk1 = one;
for k2 = 1: numjn;
sum2 = sum2 .* xk1 .* xk + one;
xk1 = xk1 + one;
end; k2 = fix(numjn+1);
sumexp = sumexp .* t + sum2;
rk = rk - one;
end; k1 = fix(numexp+1);
end;
t = rnumjn .* log( x ) - x + log( sumexp );
if( t < xhigh3 )
tran08result = valinf;
else;
tran08result = valinf - exp( t );
end;
end;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',xvalue); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [n_data, x, fx]=tran08_values( n_data, x, fx );
%*******************************************************************************
%
%! TRAN08_VALUES returns some values of the order 8 transportation function.
%
%  Discussion:
%
%    The function is defined by:
%
%      TRAN08(x) = Integral ( 0 <= t <= x ) t^8 * exp(t) / ( exp(t) - 1 )^2 dt
%
%  Modified:
%
%    06 September 2004
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Milton Abramowitz and Irene Stegun,
%    Handbook of Mathematical Functions,
%    US Department of Commerce, 1964.
%
%    Allan McLeod,
%    Algorithm 757, MISCFUN: A software package to compute uncommon
%      special functions,
%    ACM Transactions on Mathematical Software,
%    Volume 22, Number 3, September 1996, pages 288-301.
%
%    Stephen Wolfram,
%    The Mathematica Book,
%    Fourth Edition,
%    Wolfram Media / Cambridge University Press, 1999.
%
%  Parameters:
%
%    Input/output, integer N_DATA.  The user sets N_DATA to 0 before the
%    first call.  On each call, the routine increments N_DATA by 1, and
%    returns the corresponding data; when there is no more data, the
%    output value of N_DATA will be 0 again.
%
%    Output, real ( kind = 8 ) X, the argument of the function.
%
%    Output, real ( kind = 8 ) FX, the value of the function.
%
%
persistent fx_vec n_max x_vec ; 

if isempty(n_max), n_max = 20; end;
%
if isempty(fx_vec), fx_vec([1:n_max]) =[real( 0.15488598634539359463d-19),real( 0.41574269117845953797d-11),real( 0.68050651245227411689d-07),real( 0.10981703519563009836d-02),real( 0.13396432776187883834d+00),real( 0.21153387806998617182d+01),real( 0.14227877028750735641d+02),real( 0.59312061431647843226d+02),real( 0.18139614577043147745d+03),real( 0.93148001928992220863d+03),real( 0.12817928112604611804d+04),real( 0.28572838386329242218d+04),real( 0.43872971687877730010d+04),real( 0.62993229139406657611d+04),real( 0.16589426277154888511d+05),real( 0.27064780798797398935d+05),real( 0.38974556062543661284d+05),real( 0.40400240716905025786d+05),real( 0.40484316504120655568d+05),real( 0.40484399001892184901d+05) ]; end;
if isempty(x_vec), x_vec([1:n_max]) =[real(   0.0019531250d+00),real(   0.0312500000d+00),real(   0.1250000000d+00),real(   0.5000000000d+00),real(   1.0000000000d+00),real(   1.5000000000d+00),real(   2.0000000000d+00),real(   2.5000000000d+00),real(   3.0000000000d+00),real(   4.0000000000d+00),real(   4.2500000000d+00),real(   5.0000000000d+00),real(   5.5000000000d+00),real(   6.0000000000d+00),real(   8.0000000000d+00),real(  10.0000000000d+00),real(  15.0000000000d+00),real(  20.0000000000d+00),real(  30.0000000000d+00),real(  50.0000000000d+00) ]; end;
%
if( n_data < 0 )
n_data = 0;
end;
n_data = fix(n_data + 1);
if( n_max < n_data )
n_data = 0;
x = 0.0d+00;
fx = 0.0d+00;
else;
x  = x_vec(n_data);
fx = fx_vec(n_data);
end;
return;
end
function [tran09result, xvalue ]=tran09( xvalue );
%*******************************************************************************
%
%! TRAN09 calculates the transport integral of order 9.
%
%  Discussion:
%
%    The function is defined by:
%
%      TRAN09(x) = Integral ( 0 <= t <= x ) t^9 * exp(t) / ( exp(t) - 1 )^2 dt
%
%    The program uses a Chebyshev series, the coefficients of which are
%    given to an accuracy of 20 decimal places.
%
%    This subroutine is set up to work on IEEE machines.
%
%  Modified:
%
%    07 August 2004
%
%  Author:
%
%    Allan McLeod,
%    Department of Mathematics and Statistics,
%    Paisley University, High Street, Paisley, Scotland, PA12BE
%    macl_ms0@paisley.ac.uk
%
%  Reference:
%
%    Allan Mcleod,
%    Algorithm 757, MISCFUN: A software package to compute uncommon
%      special functions,
%    ACM Transactions on Mathematical Software,
%    Volume 22, Number 3, September 1996, pages 288-301.
%
%  Parameters:
%
%    Input, real ( kind = 8 ) XVALUE, the argument of the function.
%
%    Output, real ( kind = 8 ) TRAN09, the value of the function.
%
%
tran09result=[];
persistent atran eight firstCall four half k1 k2 nterms numexp numjn one rk rnumjn sum2 sumexp t tran09 valinf x xhigh1 xhigh2 xhigh3 xk xk1 xlow1 xlow2 zero ; if isempty(firstCall),firstCall=1;end; 

if isempty(atran), atran=zeros(1,19+1); end;
if isempty(eight), eight = real( 8.0); end;
if isempty(four), four = real( 4.0d+00); end;
if isempty(half), half = 0.5d+00; end;
if isempty(k1), k1=0; end;
if isempty(k2), k2=0; end;
if isempty(nterms), nterms = 17; end;
if isempty(numexp), numexp=0; end;
if isempty(numjn), numjn = 9; end;
if isempty(one), one = real( 1.0); end;
if isempty(rk), rk=0; end;
if isempty(rnumjn), rnumjn = 9.0d0; end;
if isempty(sumexp), sumexp=0; end;
if isempty(sum2), sum2=0; end;
if isempty(t), t=0; end;
if isempty(tran09result), tran09result=0; end;
if isempty(valinf), valinf = 0.36360880558872871397d6; end;
if isempty(x), x=0; end;
if isempty(xhigh1), xhigh1=0; end;
if isempty(xhigh2), xhigh2=0; end;
if isempty(xhigh3), xhigh3=0; end;
if isempty(xk), xk=0; end;
if isempty(xk1), xk1=0; end;
if isempty(xlow1), xlow1=0; end;
if isempty(xlow2), xlow2=0; end;
if isempty(zero), zero = 0.0d+00; end;
if firstCall,   atran=[0.16224049991949846835d0,-0.3768351452195937773d-1,0.547669715917719770d-2,-0.64443945009449521d-3,0.6773645285280983d-4,-0.666813497582042d-5,0.63047560019047d-6,-0.5807478663611d-7,0.525551305123d-8,-0.46968861761d-9,0.4159395065d-10,-0.365808491d-11,0.32000794d-12,-0.2787651d-13,0.242017d-14,-0.20953d-15,0.1810d-16,-0.156d-17,0.13d-18,-0.1d-19];  end;
%
%  Machine-dependent constants (for IEEE machines)
%
if firstCall,   xlow1=[2.98023224d-8];  end;
if firstCall, xlow2=[4.5321503d-39];  end;
if firstCall,   xhigh1=[36.04365668d0];  end;
if firstCall, xhigh3=[-36.73680056d0];  end;
if firstCall,   xhigh2=[4.05323966d16];  end;
firstCall=0;
%
x = xvalue;
if( x < zero )
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TRAN09 - Fatal error!');
writef(1,['%s','\n'], '  Argument X < 0.');
tran09result = zero;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',xvalue); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end;
%
%   Code for x < =  4.0
%
if( x <= four )
if( x < xlow2 )
tran09result = zero;
else;
if( x < xlow1 )
tran09result =( x .^( numjn - 1 ) ) ./( rnumjn - one );
else;
t =((( x .* x ) ./ eight ) - half ) - half;
tran09result =( x .^( numjn - 1 ) ) .* cheval( nterms, atran, t );
end;
end;
else;
%
%  Code for x > 4.0
%
if( xhigh2 < x )
sumexp = one;
else;
if( x <= xhigh1 )
numexp = fix(fix( xhigh1 ./ x ) + 1);
t = exp( -x );
else;
numexp = 1;
t = one;
end;
rk = zero;
for k1 = 1: numexp;
rk = rk + one;
end; k1 = fix(numexp+1);
sumexp = zero;
for k1 = 1: numexp;
sum2 = one;
xk = one ./( rk .* x );
xk1 = one;
for k2 = 1: numjn;
sum2 = sum2 .* xk1 .* xk + one;
xk1 = xk1 + one;
end; k2 = fix(numjn+1);
sumexp = sumexp .* t + sum2;
rk = rk - one;
end; k1 = fix(numexp+1);
end;
t = rnumjn .* log( x ) - x + log( sumexp );
if( t < xhigh3 )
tran09result = valinf;
else;
tran09result = valinf - exp( t );
end;
end;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',xvalue); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [n_data, x, fx]=tran09_values( n_data, x, fx );
%*******************************************************************************
%
%! TRAN09_VALUES returns some values of the order 9 transportation function.
%
%  Discussion:
%
%    The function is defined by:
%
%      TRAN09(x) = Integral ( 0 <= t <= x ) t^9 * exp(t) / ( exp(t) - 1 )^2 dt
%
%  Modified:
%
%    06 September 2004
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Milton Abramowitz and Irene Stegun,
%    Handbook of Mathematical Functions,
%    US Department of Commerce, 1964.
%
%    Allan McLeod,
%    Algorithm 757, MISCFUN: A software package to compute uncommon
%      special functions,
%    ACM Transactions on Mathematical Software,
%    Volume 22, Number 3, September 1996, pages 288-301.
%
%    Stephen Wolfram,
%    The Mathematica Book,
%    Fourth Edition,
%    Wolfram Media / Cambridge University Press, 1999.
%
%  Parameters:
%
%    Input/output, integer N_DATA.  The user sets N_DATA to 0 before the
%    first call.  On each call, the routine increments N_DATA by 1, and
%    returns the corresponding data; when there is no more data, the
%    output value of N_DATA will be 0 again.
%
%    Output, real ( kind = 8 ) X, the argument of the function.
%
%    Output, real ( kind = 8 ) FX, the value of the function.
%
%
persistent fx_vec n_max x_vec ; 

if isempty(n_max), n_max = 20; end;
%
if isempty(fx_vec), fx_vec([1:n_max]) =[real( 0.26469772870084897671d-22),real( 0.11367943653594246210d-12),real( 0.74428246255329800255d-08),real( 0.48022728485415366194d-03),real( 0.11700243014358676725d+00),real( 0.27648973910899914391d+01),real( 0.24716631405829192997d+02),real( 0.12827119828849828583d+03),real( 0.46842894800662208986d+03),real( 0.31673967371627895718d+04),real( 0.46140886546630195390d+04),real( 0.11952718545392302185d+05),real( 0.20001612666477027728d+05),real( 0.31011073271851366554d+05),real( 0.10352949905541130133d+06),real( 0.19743173017140591390d+06),real( 0.33826030414658460679d+06),real( 0.36179607036750755227d+06),real( 0.36360622124777561525d+06),real( 0.36360880558827162725d+06) ]; end;
if isempty(x_vec), x_vec([1:n_max]) =[real(   0.0019531250d+00),real(   0.0312500000d+00),real(   0.1250000000d+00),real(   0.5000000000d+00),real(   1.0000000000d+00),real(   1.5000000000d+00),real(   2.0000000000d+00),real(   2.5000000000d+00),real(   3.0000000000d+00),real(   4.0000000000d+00),real(   4.2500000000d+00),real(   5.0000000000d+00),real(   5.5000000000d+00),real(   6.0000000000d+00),real(   8.0000000000d+00),real(  10.0000000000d+00),real(  15.0000000000d+00),real(  20.0000000000d+00),real(  30.0000000000d+00),real(  50.0000000000d+00) ]; end;
%
if( n_data < 0 )
n_data = 0;
end;
n_data = fix(n_data + 1);
if( n_max < n_data )
n_data = 0;
x = 0.0d+00;
fx = 0.0d+00;
else;
x  = x_vec(n_data);
fx = fx_vec(n_data);
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