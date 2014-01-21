function main(varargin)
%*******************************************************************************
%
%! QUADRULE_PRB calls a set of tests for the QUADRULE library.
%
%
%  call timestamp ( )
clear global; clear functions;


writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'QUADRULE_PRB');
writef(1,['%s','\n'], '  Test problems for the QUADRULE library.');
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
test235;
test236;
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
test38;
test39;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'QUADRULE_PRB');
writef(1,['%s','\n'], '  Normal end of execution.');
writef(1,['%s','\n'], ' ');
%  call timestamp ( )

%  stop
end
function test01(varargin)
%
%*******************************************************************************
%
%! TEST01 tests BASHFORTH_SET.
%! TEST01 tests SUMMER.
%
%
persistent i ihi ilo maxfunc maxorder nfunc norder result weight xtab ; 

if isempty(maxfunc), maxfunc = 20; end;
if isempty(maxorder), maxorder = 8; end;
%
if isempty(i), i=0; end;
if isempty(ihi), ihi=0; end;
if isempty(ilo), ilo=0; end;
if isempty(nfunc), nfunc=0; end;
if isempty(norder), norder=0; end;
if isempty(result), result=zeros(1,maxfunc); end;
if isempty(weight), weight=zeros(1,maxorder); end;
if isempty(xtab), xtab=zeros(1,maxorder); end;
%
[dumvar1, nfunc ]=func_set ( 'COUNT', nfunc );
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TEST01');
writef(1,['%s','\n'], '  BASHFORTH_SET sets up Adams-Bashforth quadrature;');
writef(1,['%s','\n'], '  SUMMER carries it out.');
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], '  Integration interval is [0,1].');
writef(1,['%s','\n'], '  Quadrature order will vary.');
writef(1,['%s','\n'], '  Integrand will vary.');
writef(1,['%s','\n'], ' ');
for ilo = 1: 5: nfunc;
ihi = fix(min( ilo + 4, nfunc ));
writef(1,['%s %0.15g \n'], 'ilo=',ilo);
writef(1,['%s %0.15g \n'], 'ihi=',ihi);
writef(1,['%s %0.15g \n'], 'nfunc=',nfunc);
writef(1,['%s','\n'], ' ');
for  i =( ilo):( ihi ), writef(1,[repmat(' ',1,4),repmat('%14c',1,5),'\n'], fname(i)); end;
writef(1,['%s','\n'], ' ');
for norder = 1: maxorder;

for i = ilo: ihi;
[dumvar1, i ]=func_set ( 'SET', i );
[ norder, xtab, weight ]=bashforth_set( norder, xtab, weight );

[dumvar1, norder, xtab, weight, result(i) ]=summer( @func, norder, xtab, weight, result(i) );

end; i = fix(ihi+1);
writef(1,['%2i',repmat(' ',1,2),repmat('%14.8f',1,5),'\n'], norder, result([ilo:ihi]));
end; norder = fix(maxorder+1);
end; ilo = fix(nfunc+1);
return;
end
function test02(varargin)
%
%*******************************************************************************
%
%! TEST02 tests BDFC_SET.
%! TEST02 tests BDF_SUM.
%
%
persistent diftab i ihi ilo maxfunc maxorder nfunc norder result weight xtab ; 

if isempty(maxfunc), maxfunc = 20; end;
if isempty(maxorder), maxorder = 10; end;
%
if isempty(diftab), diftab=zeros(1,maxorder); end;
if isempty(i), i=0; end;
if isempty(ihi), ihi=0; end;
if isempty(ilo), ilo=0; end;
if isempty(nfunc), nfunc=0; end;
if isempty(norder), norder=0; end;
if isempty(result), result=zeros(1,maxfunc); end;
if isempty(weight), weight=zeros(1,maxorder); end;
if isempty(xtab), xtab=zeros(1,maxorder); end;
%
[dumvar1, nfunc ]=func_set ( 'COUNT', nfunc );
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TEST02');
writef(1,['%s','\n'],'  BDFC_SET sets up Backward Difference Corrector quadrature;');
writef(1,['%s','\n'], '  BDF_SUM carries it out.');
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], '  Integration interval is [0,1].');
writef(1,['%s','\n'], '  Quadrature order will vary.');
writef(1,['%s','\n'], '  Integrand will vary.');
writef(1,['%s','\n'], ' ');
for ilo = 1: 5: nfunc;
ihi = fix(min( ilo + 4, nfunc ));
writef(1,['%s','\n'], ' ');
for  i =( ilo):( ihi ), writef(1,[repmat(' ',1,4),repmat('%14c',1,5),'\n'], fname(i)); end;
writef(1,['%s','\n'], ' ');
for norder = 1: maxorder;
for i = ilo: ihi;
[dumvar1, i ]=func_set ( 'SET', i );
[ norder, weight, xtab ]=bdfc_set( norder, weight, xtab );

[dumvar1, norder, weight, xtab, diftab, result(i) ]=bdf_sum( @func, norder, weight, xtab, diftab, result(i) );
end; i = fix(ihi+1);
writef(1,['%2i',repmat(' ',1,2),repmat('%14.8f',1,5),'\n'], norder, result([ilo:ihi]));
end; norder = fix(maxorder+1);
end; ilo = fix(nfunc+1);
return;
end
function test03(varargin)
%
%*******************************************************************************
%
%! TEST03 tests BDFP_SET.
%! TEST03 tests BDF_SUM.
%
%
persistent diftab i ihi ilo maxfunc maxorder nfunc norder result weight xtab ; 

if isempty(maxfunc), maxfunc = 20; end;
if isempty(maxorder), maxorder = 10; end;
%
if isempty(diftab), diftab=zeros(1,maxorder); end;
if isempty(i), i=0; end;
if isempty(ihi), ihi=0; end;
if isempty(ilo), ilo=0; end;
if isempty(nfunc), nfunc=0; end;
if isempty(norder), norder=0; end;
if isempty(result), result=zeros(1,maxfunc); end;
if isempty(weight), weight=zeros(1,maxorder); end;
if isempty(xtab), xtab=zeros(1,maxorder); end;
%
[dumvar1, nfunc ]=func_set ( 'COUNT', nfunc );
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TEST04');
writef(1,['%s','\n'],'  BDFP_SET sets up Backward Difference Predictor quadrature;');
writef(1,['%s','\n'], '  BDF_SUM carries it out.');
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], '  Integration interval is [0,1].');
writef(1,['%s','\n'], '  Quadrature order will vary.');
writef(1,['%s','\n'], '  Integrand will vary.');
writef(1,['%s','\n'], ' ');
for ilo = 1: 5: nfunc;
ihi = fix(min( ilo + 4, nfunc ));
writef(1,['%s','\n'], ' ');
for  i =( ilo):( ihi ), writef(1,[repmat(' ',1,4),repmat('%14c',1,5),'\n'], fname(i)); end;
writef(1,['%s','\n'], ' ');
for norder = 1: maxorder;
for i = ilo: ihi;
[dumvar1, i ]=func_set ( 'SET', i );
[ norder, weight, xtab ]=bdfp_set( norder, weight, xtab );

[dumvar1, norder, weight, xtab, diftab, result(i) ]=bdf_sum( @func, norder, weight, xtab, diftab, result(i) );
end; i = fix(ihi+1);
writef(1,['%2i',repmat(' ',1,2),repmat('%14.8f',1,5),'\n'], norder, result([ilo:ihi]));
end; norder = fix(maxorder+1);
end; ilo = fix(nfunc+1);

return;
end
function test04(varargin)
%
%*******************************************************************************
%
%! TEST04 tests CHEB_SET.
%! TEST04 tests SUM_SUB.
%
%
persistent a b i ihi ilo maxfunc maxorder nfunc norder nsub result weight xhi xlo xtab ; 

if isempty(maxfunc), maxfunc = 20; end;
if isempty(maxorder), maxorder = 9; end;
%
if isempty(a), a=0; end;
if isempty(b), b=0; end;
if isempty(i), i=0; end;
if isempty(ihi), ihi=0; end;
if isempty(ilo), ilo=0; end;
if isempty(nfunc), nfunc=0; end;
if isempty(norder), norder=0; end;
if isempty(nsub), nsub=0; end;
if isempty(result), result=zeros(1,maxfunc); end;
if isempty(weight), weight=zeros(1,maxorder); end;
if isempty(xhi), xhi=0; end;
if isempty(xlo), xlo=0; end;
if isempty(xtab), xtab=zeros(1,maxorder); end;
%
[dumvar1, nfunc ]=func_set ( 'COUNT', nfunc );
a =  0.0d+00;
b =  1.0d+00;
nsub = 1;
xlo = -1.0d+00;
xhi =  1.0d+00;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TEST04');
writef(1,['%s','\n'], '  CHEB_SET sets up Chebyshev quadrature;');
writef(1,['%s','\n'], '  SUM_SUB carries it out.');
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], '  The integration interval is ', a, b);
writef(1,['%s','%6i','\n'], '  Number of subintervals is ', nsub);
writef(1,['%s','\n'], '  Quadrature order will vary.');
writef(1,['%s','\n'], '  Integrand will vary.');
writef(1,['%s','\n'], ' ');
for ilo = 1: 5: nfunc;
ihi = fix(min( ilo + 4, nfunc ));
writef(1,['%s','\n'], ' ');
for  i =( ilo):( ihi ), writef(1,[repmat(' ',1,4),repmat('%14c',1,5),'\n'], fname(i)); end;
writef(1,['%s','\n'], ' ');
for norder = 1: maxorder;
if( norder ~= 8 )
for i = ilo: ihi;
[dumvar1, i ]=func_set ( 'SET', i );
[ norder, xtab, weight ]=cheb_set( norder, xtab, weight );
[dumvar1, a, b, nsub, norder, xlo, xhi, xtab, weight,result(i) ]=sum_sub( @func, a, b, nsub, norder, xlo, xhi, xtab, weight,result(i) );

end; i = fix(ihi+1);
writef(1,['%2i',repmat(' ',1,2),repmat('%14.8f',1,5),'\n'], norder, result([ilo:ihi]));
end;
end; norder = fix(maxorder+1);

end; ilo = fix(nfunc+1);
return;
end
function test05(varargin)
%
%*******************************************************************************
%
%! TEST05 tests CHEB_TC_SET.
%! TEST05 tests SUMMER.
%
%
persistent a b i ihi ilo maxfunc maxorder nfunc norder nsub result weight xtab ; 

if isempty(maxfunc), maxfunc = 20; end;
if isempty(maxorder), maxorder = 6; end;
%
if isempty(a), a=0; end;
if isempty(b), b=0; end;
if isempty(i), i=0; end;
if isempty(ihi), ihi=0; end;
if isempty(ilo), ilo=0; end;
if isempty(nfunc), nfunc=0; end;
if isempty(norder), norder=0; end;
if isempty(nsub), nsub=0; end;
if isempty(result), result=zeros(1,maxfunc); end;
if isempty(weight), weight=zeros(1,maxorder); end;
if isempty(xtab), xtab=zeros(1,maxorder); end;
%
[dumvar1, nfunc ]=func_set ( 'COUNT', nfunc );
a = - 1.0d+00;
b =   1.0d+00;
nsub = 1;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TEST05');
writef(1,['%s','\n'], '  CHEB_TC_SET sets up Gauss-Chebyshev quadrature,');
writef(1,['%s','\n'], '  ( closed, first kind );');
writef(1,['%s','\n'], '  SUMMER carries it out.');
writef(1,['%s','\n'], ' ');
writef(1,['%s',repmat('%14.6f',1,2),'\n'], '  Integration interval is ', a, b);
writef(1,['%s','%6i','\n'], '  Number of subintervals is ', nsub);
writef(1,['%s','\n'], '  Quadrature order will vary.');
writef(1,['%s','\n'], '  Integrand will vary.');
writef(1,['%s','\n'], '  The weight function is 1 / sqrt ( 1 - X**2 )');
writef(1,['%s','\n'], ' ');
for ilo = 1: 5: nfunc;
ihi = fix(min( ilo + 4, nfunc ));
writef(1,['%s','\n'], ' ');
for  i =( ilo):( ihi ), writef(1,[repmat(' ',1,4),repmat('%14c',1,5),'\n'], fname(i)); end;
writef(1,['%s','\n'], ' ');
for norder = 2: maxorder;

for i = ilo: ihi;
[dumvar1, i ]=func_set ( 'SET', i );
[ norder, xtab, weight ]=cheb_tc_set( norder, xtab, weight );
[dumvar1, norder, xtab, weight, result(i) ]=summer( @func, norder, xtab, weight, result(i) );
end; i = fix(ihi+1);
writef(1,['%2i',repmat(' ',1,2),repmat('%14.8f',1,5),'\n'], norder, result([ilo:ihi]));
end; norder = fix(maxorder+1);
end; ilo = fix(nfunc+1);
return;
end
function test06(varargin)
%
%*******************************************************************************
%
%! TEST06 tests CHEB_TO_SET.
%! TEST06 tests SUMMER.
%
%
persistent a b i ihi ilo maxfunc maxorder nfunc norder nsub result weight xtab ; 

if isempty(maxfunc), maxfunc = 20; end;
if isempty(maxorder), maxorder = 6; end;
%
if isempty(a), a=0; end;
if isempty(b), b=0; end;
if isempty(i), i=0; end;
if isempty(ihi), ihi=0; end;
if isempty(ilo), ilo=0; end;
if isempty(nfunc), nfunc=0; end;
if isempty(norder), norder=0; end;
if isempty(nsub), nsub=0; end;
if isempty(result), result=zeros(1,maxfunc); end;
if isempty(weight), weight=zeros(1,maxorder); end;
if isempty(xtab), xtab=zeros(1,maxorder); end;
%
[dumvar1, nfunc ]=func_set ( 'COUNT', nfunc );
a = -1.0d+00;
b =  1.0d+00;
nsub = 1;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TEST06');
writef(1,['%s','\n'], '  CHEB_TO_SET sets up Gauss-Chebyshev quadrature,');
writef(1,['%s','\n'], '  ( open, first kind );');
writef(1,['%s','\n'], '  SUMMER carries it out.');
writef(1,['%s','\n'], ' ');
writef(1,['%s',repmat('%14.6f',1,2),'\n'], '  Integration interval is ', a, b);
writef(1,['%s','%6i','\n'], '  Number of subintervals is ', nsub);
writef(1,['%s','\n'], '  Quadrature order will vary.');
writef(1,['%s','\n'], '  Integrand will vary.');
writef(1,['%s','\n'], '  The weight function is 1 / sqrt ( 1 - X**2 )');
writef(1,['%s','\n'], ' ');
for ilo = 1: 5: nfunc;
ihi = fix(min( ilo + 4, nfunc ));
writef(1,['%s','\n'], ' ');
for  i =( ilo):( ihi ), writef(1,[repmat(' ',1,4),repmat('%14c',1,5),'\n'], fname(i)); end;
writef(1,['%s','\n'], ' ');

for norder = 1: maxorder;

for i = ilo: ihi;

[dumvar1, i ]=func_set ( 'SET', i );
[ norder, xtab, weight ]=cheb_to_set( norder, xtab, weight );
[dumvar1, norder, xtab, weight, result(i) ]=summer( @func, norder, xtab, weight, result(i) );
end; i = fix(ihi+1);
writef(1,['%2i',repmat(' ',1,2),repmat('%14.8f',1,5),'\n'], norder, result([ilo:ihi]));
end; norder = fix(maxorder+1);
end; ilo = fix(nfunc+1);
return;
end
function test07(varargin)
%
%*******************************************************************************
%
%! TEST07 tests CHEB_U_SET.
%! TEST07 tests SUMMER.
%
%
persistent a b i ihi ilo maxfunc maxorder nfunc norder nsub result weight xtab ; 

if isempty(maxfunc), maxfunc = 20; end;
if isempty(maxorder), maxorder = 4; end;
%
if isempty(a), a=0; end;
if isempty(b), b=0; end;
if isempty(i), i=0; end;
if isempty(ihi), ihi=0; end;
if isempty(ilo), ilo=0; end;
if isempty(nfunc), nfunc=0; end;
if isempty(norder), norder=0; end;
if isempty(nsub), nsub=0; end;
if isempty(result), result=zeros(1,maxfunc); end;
if isempty(weight), weight=zeros(1,maxorder); end;
if isempty(xtab), xtab=zeros(1,maxorder); end;
%
[dumvar1, nfunc ]=func_set ( 'COUNT', nfunc );
a = - 1.0d+00;
b =   1.0d+00;
nsub = 1;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TEST07');
writef(1,['%s','\n'], '  CHEB_U_SET sets up Gauss-Chebyshev quadrature;');
writef(1,['%s','\n'], '  SUMMER carries it out.');
writef(1,['%s','\n'], ' ');
writef(1,['%s',repmat('%14.6f',1,2),'\n'], '  Integration interval is ', a, b);
writef(1,['%s','%6i','\n'], '  Number of subintervals is ', nsub);
writef(1,['%s','\n'], '  Quadrature order will vary.');
writef(1,['%s','\n'], '  Integrand will vary.');
writef(1,['%s','\n'], '  The weight function is sqrt ( 1 - X**2 )');
writef(1,['%s','\n'], ' ');
for ilo = 1: 5: nfunc;
ihi = fix(min( ilo + 4, nfunc ));
writef(1,['%s','\n'], ' ');
for  i =( ilo):( ihi ), writef(1,[repmat(' ',1,4),repmat('%14c',1,5),'\n'], fname(i)); end;
writef(1,['%s','\n'], ' ');

for norder = 1: maxorder;

for i = ilo: ihi;
[dumvar1, i ]=func_set ( 'SET', i );
[ norder, xtab, weight ]=cheb_u_set( norder, xtab, weight );
[dumvar1, norder, xtab, weight, result(i) ]=summer( @func, norder, xtab, weight, result(i) );
end; i = fix(ihi+1);
writef(1,['%2i',repmat(' ',1,2),repmat('%14.8f',1,5),'\n'], norder, result([ilo:ihi]));
end; norder = fix(maxorder+1);

end; ilo = fix(nfunc+1);
return;
end
function test08(varargin)
%
%*******************************************************************************
%
%! TEST08 tests HERMITE_COM.
%! TEST08 tests SUMMER.
%
%
persistent i ihi ilo maxfunc maxorder nfunc norder result weight xtab ; 

if isempty(maxfunc), maxfunc = 20; end;
if isempty(maxorder), maxorder = 20; end;
%
if isempty(i), i=0; end;
if isempty(ihi), ihi=0; end;
if isempty(ilo), ilo=0; end;
if isempty(nfunc), nfunc=0; end;
if isempty(norder), norder=0; end;
if isempty(result), result=zeros(1,maxfunc); end;
if isempty(weight), weight=zeros(1,maxorder); end;
if isempty(xtab), xtab=zeros(1,maxorder); end;
%
[dumvar1, nfunc ]=func_set ( 'COUNT', nfunc );
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TEST08');
writef(1,['%s','\n'], '  HERMITE_COM computes a Gauss-Hermite rule;');
writef(1,['%s','\n'], '  SUMMER carries it out.');
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], '  The integration interval is ( -Infinity, +Infinity ).');
for ilo = 1: 5: nfunc;
ihi = fix(min( ilo + 4, nfunc ));
writef(1,['%s','\n'], ' ');
for  i =( ilo):( ihi ), writef(1,[repmat(' ',1,4),repmat('%14c',1,5),'\n'], fname(i)); end;
writef(1,['%s','\n'], ' ');

for norder = 1: maxorder;
for i = ilo: ihi;
[dumvar1, i ]=func_set ( 'SET', i );
[ norder, xtab, weight ]=hermite_com( norder, xtab, weight );

[dumvar1, norder, xtab, weight, result(i) ]=summer( @func, norder, xtab, weight, result(i) );

end; i = fix(ihi+1);

writef(1,['%2i',repmat(' ',1,2),repmat('%14.8f',1,5),'\n'], norder, result([ilo:ihi]));
end; norder = fix(maxorder+1);
end; ilo = fix(nfunc+1);

return;
end
function test09(varargin)
%
%*******************************************************************************
%
%! TEST09 tests HERMITE_SET.
%! TEST09 tests SUMMER.
%
%
persistent i ihi ilo maxfunc maxorder nfunc norder result weight xtab ; 

if isempty(maxfunc), maxfunc = 20; end;
if isempty(maxorder), maxorder = 20; end;
%
if isempty(i), i=0; end;
if isempty(ihi), ihi=0; end;
if isempty(ilo), ilo=0; end;
if isempty(nfunc), nfunc=0; end;
if isempty(norder), norder=0; end;
if isempty(result), result=zeros(1,maxfunc); end;
if isempty(weight), weight=zeros(1,maxorder); end;
if isempty(xtab), xtab=zeros(1,maxorder); end;
%
[dumvar1, nfunc ]=func_set ( 'COUNT', nfunc );
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TEST09');
writef(1,['%s','\n'], '  HERMITE_SET sets up Gauss-Hermite quadrature;');
writef(1,['%s','\n'], '  SUMMER carries it out.');
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], '  The integration interval is ( -Infinity, +Infinity ).');
writef(1,['%s','\n'], ' ');
for ilo = 1: 5: nfunc;
ihi = fix(min( ilo + 4, nfunc ));
writef(1,['%s','\n'], ' ');
for  i =( ilo):( ihi ), writef(1,[repmat(' ',1,4),repmat('%14c',1,5),'\n'], fname(i)); end;
writef(1,['%s','\n'], ' ');

for norder = 1: maxorder;

for i = ilo: ihi;
[dumvar1, i ]=func_set ( 'SET', i );
[ norder, xtab, weight ]=hermite_set( norder, xtab, weight );

[dumvar1, norder, xtab, weight, result(i) ]=summer( @func, norder, xtab, weight, result(i) );

end; i = fix(ihi+1);
writef(1,['%2i',repmat(' ',1,2),repmat('%14.8f',1,5),'\n'],  norder, result([ilo:ihi]));

end; norder = fix(maxorder+1);

end; ilo = fix(nfunc+1);
return;
end
function test10(varargin)
%
%*******************************************************************************
%
%! TEST10 tests JACOBI_COM.
%! TEST10 tests SUM_SUB.
%
%
persistent a alpha b beta i ihi ilo k maxfunc maxorder nfunc norder nsub result weight xhi xlo xtab ; 

if isempty(maxfunc), maxfunc = 20; end;
if isempty(maxorder), maxorder = 10; end;
%
if isempty(a), a=0; end;
if isempty(alpha), alpha=0; end;
if isempty(b), b=0; end;
if isempty(beta), beta=0; end;
if isempty(i), i=0; end;
if isempty(ihi), ihi=0; end;
if isempty(ilo), ilo=0; end;
if isempty(k), k=0; end;
if isempty(nfunc), nfunc=0; end;
if isempty(norder), norder=0; end;
if isempty(nsub), nsub=0; end;
if isempty(result), result=zeros(1,maxfunc); end;
if isempty(weight), weight=zeros(1,maxorder); end;
if isempty(xhi), xhi=0; end;
if isempty(xlo), xlo=0; end;
if isempty(xtab), xtab=zeros(1,maxorder); end;
%
[dumvar1, nfunc ]=func_set ( 'COUNT', nfunc );
a = -1.0d+00;
b =  1.0d+00;
nsub = 1;
xlo = -1.0d+00;
xhi = +1.0d+00;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TEST10');
writef(1,['%s','\n'], '  JACOBI_COM sets up Gauss-Jacobi quadrature;');
writef(1,['%s','\n'], '  SUM_SUB carries it out.');
writef(1,['%s','\n'], ' ');
writef(1,['%s',repmat('%14.6f',1,2),'\n'], '  The integration interval is ', a, b);
writef(1,['%s','%6i','\n'], '  Number of subintervals is ', nsub);
writef(1,['%s','\n'], '  Quadrature order will vary.');
writef(1,['%s','\n'], '  Integrand will vary.');
writef(1,['%s','\n'], ' ');
for k = 1: 2;
if( k == 1 )
alpha = 0.0d+00;
beta = 0.0d+00;
elseif( k == 2 ) ;
alpha = 1.0d+00;
beta = 0.0d+00;
end;
writef(1,['%s','\n'], ' ');
writef(1,['%s','%14.6f','\n'], '  ALPHA = ', alpha);
writef(1,['%s','%14.6f','\n'], '  BETA =  ', beta);
for ilo = 1: 5: nfunc;
ihi = fix(min( ilo + 4, nfunc ));
writef(1,['%s','\n'], ' ');
for  i =( ilo):( ihi ), writef(1,[repmat(' ',1,4),repmat('%14c',1,5),'\n'], fname(i)); end;
writef(1,['%s','\n'], ' ');
for norder = 1: maxorder;
for i = ilo: ihi;
[dumvar1, i ]=func_set ( 'SET', i );
[ norder, xtab, weight, alpha, beta ]=jacobi_com( norder, xtab, weight, alpha, beta );
[dumvar1, a, b, nsub, norder, xlo, xhi, xtab, weight,result(i) ]=sum_sub( @func, a, b, nsub, norder, xlo, xhi, xtab, weight,result(i) );
end; i = fix(ihi+1);
writef(1,['%2i',repmat(' ',1,2),repmat('%14.8f',1,5),'\n'], norder, result([ilo:ihi]));
end; norder = fix(maxorder+1);
end; ilo = fix(nfunc+1);
end; k = fix(2+1);
return;
end
function test11(varargin)
%
%*******************************************************************************
%
%! TEST11 tests KRONROD_SET.
%! TEST11 tests LEGENDRE_SET.
%! TEST11 tests SUMMER_GK.
%
%
persistent norderg norderk resultg resultk weightg weightk xtabg xtabk ; 

if isempty(norderg), norderg = 10; end;
if isempty(norderk), norderk = 2 .* norderg + 1; end;
%
if isempty(resultg), resultg=0; end;
if isempty(resultk), resultk=0; end;
if isempty(weightg), weightg=zeros(1,norderg); end;
if isempty(weightk), weightk=zeros(1,norderk); end;
if isempty(xtabg), xtabg=zeros(1,norderg); end;
if isempty(xtabk), xtabk=zeros(1,norderk); end;
%
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TEST11');
writef(1,['%s','\n'], '  KRONROD_SET sets up Kronrod quadrature;');
writef(1,['%s','\n'], '  LEGENDRE_SET sets up Gauss-Legendre quadrature;');
writef(1,['%s','\n'], '  SUMMER_GK carries it out.');
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], '  Integration interval is [-1, 1].');
writef(1,['%s','\n'], '  Integrand is X**2 / SQRT ( 1.1 - X**2 ).');
writef(1,['%s','\n'], ' ');
[ norderg, xtabg, weightg ]=legendre_set( norderg, xtabg, weightg );
[ norderk, xtabk, weightk ]=kronrod_set( norderk, xtabk, weightk );
[dumvar1, norderg, weightg, resultg, norderk, xtabk,weightk, resultk ]=summer_gk( @fx2sd1, norderg, weightg, resultg, norderk, xtabk,weightk, resultk );
writef(1,['%2i',repmat(' ',1,2),'%16.10f','\n'], norderg, resultg);
writef(1,['%2i',repmat(' ',1,2),'%16.10f','\n'], norderk, resultk);
writef(1,[repmat(' ',1,2),repmat(' ',1,2),'%16.10f','\n'],          resultg-resultk);
return;
end
function test12(varargin)
%
%*******************************************************************************
%
%! TEST12 tests KRONROD_SET.
%! TEST12 tests LEGENDRE_SET.
%! TEST12 tests SUM_SUB_GK.
%
%
persistent a b error norderg norderk nsub resultg resultk weightg weightk xtabg xtabk ; 

if isempty(norderg), norderg = 7; end;
if isempty(norderk), norderk = 2 .* norderg + 1; end;
%
if isempty(a), a=0; end;
if isempty(b), b=0; end;
if isempty(error), error=0; end;
if isempty(nsub), nsub=0; end;
if isempty(resultg), resultg=0; end;
if isempty(resultk), resultk=0; end;
if isempty(weightg), weightg=zeros(1,norderg); end;
if isempty(weightk), weightk=zeros(1,norderk); end;
if isempty(xtabg), xtabg=zeros(1,norderg); end;
if isempty(xtabk), xtabk=zeros(1,norderk); end;
%
a = - 1.0d+00;
b =   1.0d+00;
nsub = 5;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TEST12');
writef(1,['%s','\n'], '  KRONROD_SET sets up Kronrod quadrature;');
writef(1,['%s','\n'], '  LEGENDRE_SET sets up Gauss-Legendre quadrature;');
writef(1,['%s','\n'], '  SUM_SUB_GK carries it out.');
writef(1,['%s','\n'], ' ');
writef(1,['%s',repmat('%14.6f',1,2),'\n'], '  Integration interval is ', a, b);
writef(1,['%s','%6i','\n'], '  Number of subintervals is ', nsub);
writef(1,['%s','\n'], '  Integrand is X**2 / SQRT ( 1.1 - X**2 ).');
writef(1,['%s','\n'], ' ');
[ norderg, xtabg, weightg ]=legendre_set( norderg, xtabg, weightg );
[ norderk, xtabk, weightk ]=kronrod_set( norderk, xtabk, weightk );
[dumvar1, a, b, nsub, norderg, weightg, resultg,norderk, xtabk, weightk, resultk, error ]=sum_sub_gk( @fx2sd1, a, b, nsub, norderg, weightg, resultg,norderk, xtabk, weightk, resultk, error );
writef(1,['%2i',repmat(' ',1,2),'%16.10f','\n'], norderg, resultg);
writef(1,['%2i',repmat(' ',1,2),'%16.10f','\n'], norderk, resultk);
writef(1,[repmat(' ',1,2),repmat(' ',1,2),'%16.10f','\n'],          error);
return;
end
function test13(varargin)
%
%*******************************************************************************
%
%! TEST13 tests LAGUERRE_COM.
%! TEST13 tests LAGUERRE_SUM.
%
%
persistent a alpha i ihi ii ilo maxfunc maxorder nfunc norder result weight xtab ; 

if isempty(maxfunc), maxfunc = 20; end;
if isempty(maxorder), maxorder = 20; end;
%
if isempty(a), a=0; end;
if isempty(alpha), alpha=0; end;
if isempty(i), i=0; end;
if isempty(ihi), ihi=0; end;
if isempty(ii), ii=0; end;
if isempty(ilo), ilo=0; end;
if isempty(nfunc), nfunc=0; end;
if isempty(norder), norder=0; end;
if isempty(result), result=zeros(1,maxfunc); end;
if isempty(weight), weight=zeros(1,maxorder); end;
if isempty(xtab), xtab=zeros(1,maxorder); end;
%
[dumvar1, nfunc ]=func_set ( 'COUNT', nfunc );
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TEST13');
writef(1,['%s','\n'], '  LAGUERRE_COM computes a Gauss-Laguerre rule;');
writef(1,['%s','\n'], '  LAGUERRE_SUM carries it out.');
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], '  Quadrature order will vary.');
writef(1,['%s','\n'], '  Integrand will vary.');
writef(1,['%s','\n'], '  The weight function is EXP ( - X ).');
writef(1,['%s','\n'], ' ');
%
a = 1.0d+00;
alpha = 0.0d+00;
writef(1,['%s','%14.6f','%s','\n'], '  The integration interval is [ ',a, ', +Infinity ).');
writef(1,['%s','%14.6f','\n'], '  ALPHA = ', alpha);
writef(1,['%s','\n'], ' ');
for ilo = 1: 5: nfunc;
ihi = fix(min( ilo + 4, nfunc ));
writef(1,['%s','\n'], ' ');
for  i =( ilo):( ihi ), writef(1,[repmat(' ',1,4),repmat('%14c',1,5),'\n'], fname(i)); end;
writef(1,['%s','\n'], ' ');
for norder = 1: maxorder;
for i = ilo: ihi;
[dumvar1, i ]=func_set ( 'SET', i );
[ norder, xtab, weight, alpha ]=laguerre_com( norder, xtab, weight, alpha );

[dumvar1, a, norder, xtab, weight, result(i) ]=laguerre_sum( @func, a, norder, xtab, weight, result(i) );

end; i = fix(ihi+1);
writef(1,['%2i',repmat(' ',1,2),repmat('%14.8f',1,5),'\n'], norder, result([ilo:ihi]));

end; norder = fix(maxorder+1);
end; ilo = fix(nfunc+1);
return;
end
function test14(varargin)
%
%*******************************************************************************
%
%! TEST14 tests LAGUERRE_COM.
%! TEST14 tests LAGUERRE_SUM.
%
%
persistent a alpha i ihi ii ilo maxfunc maxorder nfunc norder result weight xtab ; 

if isempty(maxfunc), maxfunc = 20; end;
if isempty(maxorder), maxorder = 20; end;
%
if isempty(a), a=0; end;
if isempty(alpha), alpha=0; end;
if isempty(i), i=0; end;
if isempty(ihi), ihi=0; end;
if isempty(ii), ii=0; end;
if isempty(ilo), ilo=0; end;
if isempty(nfunc), nfunc=0; end;
if isempty(norder), norder=0; end;
if isempty(result), result=zeros(1,maxfunc); end;
if isempty(weight), weight=zeros(1,maxorder); end;
if isempty(xtab), xtab=zeros(1,maxorder); end;
%
[dumvar1, nfunc ]=func_set ( 'COUNT', nfunc );
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TEST14');
writef(1,['%s','\n'], '  LAGUERRE_COM computes a Gauss-Laguerre rule;');
writef(1,['%s','\n'], '  LAGUERRE_SUM carries it out.');
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], '  Quadrature order will vary.');
writef(1,['%s','\n'], '  Integrand will vary.');
writef(1,['%s','\n'], '  The weight function is EXP ( - X ).');
writef(1,['%s','\n'], ' ');
%
a = 0.0d+00;
alpha = 0.0d+00;
writef(1,['%s','\n'], ' ');
writef(1,['%s','%14.6f','%s','\n'], '  The integration interval is [ ',a, ', +Infinity ).');
writef(1,['%s','%14.6f','\n'], '  ALPHA = ', alpha);
writef(1,['%s','\n'], ' ');
for ilo = 1: 5: nfunc;
ihi = fix(min( ilo + 4, nfunc ));
writef(1,['%s','\n'], ' ');
for  i =( ilo):( ihi ), writef(1,[repmat(' ',1,4),repmat('%14c',1,5),'\n'], fname(i)); end;
writef(1,['%s','\n'], ' ');
for norder = 1: 10;
for i = ilo: ihi;
[dumvar1, i ]=func_set ( 'SET', i );
[ norder, xtab, weight, alpha ]=laguerre_com( norder, xtab, weight, alpha );

[dumvar1, a, norder, xtab, weight, result(i) ]=laguerre_sum( @func, a, norder, xtab, weight, result(i) );

end; i = fix(ihi+1);
writef(1,['%2i',repmat(' ',1,2),repmat('%14.8f',1,5),'\n'], norder, result([ilo:ihi]));

end; norder = fix(10+1);
end; ilo = fix(nfunc+1);
return;
end
function test15(varargin)
%
%*******************************************************************************
%
%! TEST15 tests LAGUERRE_COM.
%! TEST15 tests LAGUERRE_SUM.
%
%
persistent a alpha i ihi ii ilo maxfunc maxorder nfunc norder result weight xtab ; 

if isempty(maxfunc), maxfunc = 20; end;
if isempty(maxorder), maxorder = 20; end;
%
if isempty(a), a=0; end;
if isempty(alpha), alpha=0; end;
if isempty(i), i=0; end;
if isempty(ihi), ihi=0; end;
if isempty(ii), ii=0; end;
if isempty(ilo), ilo=0; end;
if isempty(nfunc), nfunc=0; end;
if isempty(norder), norder=0; end;
if isempty(result), result=zeros(1,maxfunc); end;
if isempty(weight), weight=zeros(1,maxorder); end;
if isempty(xtab), xtab=zeros(1,maxorder); end;
%
[dumvar1, nfunc ]=func_set ( 'COUNT', nfunc );
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TEST15');
writef(1,['%s','\n'], '  LAGUERRE_COM computes a Gauss-Laguerre rule;');
writef(1,['%s','\n'], '  LAGUERRE_SUM carries it out.');
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], '  Quadrature order will vary.');
writef(1,['%s','\n'], '  Integrand will vary.');
writef(1,['%s','\n'], '  The weight function is EXP ( - X ).');
writef(1,['%s','\n'], ' ');
%
a = 0.0d+00;
alpha = 1.0d+00;
writef(1,['%s','\n'], ' ');
writef(1,['%s','%14.6f','%s','\n'], '  The integration interval is [ ',a, ', +Infinity ).');
writef(1,['%s','%14.6f','\n'], '  ALPHA = ', alpha);
writef(1,['%s','\n'], ' ');
for ilo = 1: 5: nfunc;

ihi = fix(min( ilo + 4, nfunc ));
writef(1,['%s','\n'], ' ');
for  i =( ilo):( ihi ), writef(1,[repmat(' ',1,4),repmat('%14c',1,5),'\n'], fname(i)); end;
writef(1,['%s','\n'], ' ');
for norder = 1: 10;
for i = ilo: ihi;
[dumvar1, i ]=func_set ( 'SET', i );
[ norder, xtab, weight, alpha ]=laguerre_com( norder, xtab, weight, alpha );

[dumvar1, a, norder, xtab, weight, result(i) ]=laguerre_sum( @func, a, norder, xtab, weight, result(i) );

end; i = fix(ihi+1);
writef(1,['%2i',repmat(' ',1,2),repmat('%14.6f',1,5),'\n'], norder, result([ilo:ihi]));

end; norder = fix(10+1);
end; ilo = fix(nfunc+1);
return;
end
function test16(varargin)
%
%*******************************************************************************
%
%! TEST16 tests LAGUERRE_COM.
%! TEST16 tests LAGUERRE_SUM.
%
%
persistent a alpha i ihi ii ilo maxfunc maxorder nfunc norder result weight xtab ; 

if isempty(maxfunc), maxfunc = 20; end;
if isempty(maxorder), maxorder = 20; end;
%
if isempty(a), a=0; end;
if isempty(alpha), alpha=0; end;
if isempty(i), i=0; end;
if isempty(ihi), ihi=0; end;
if isempty(ii), ii=0; end;
if isempty(ilo), ilo=0; end;
if isempty(nfunc), nfunc=0; end;
if isempty(norder), norder=0; end;
if isempty(result), result=zeros(1,maxfunc); end;
if isempty(weight), weight=zeros(1,maxorder); end;
if isempty(xtab), xtab=zeros(1,maxorder); end;
%
[dumvar1, nfunc ]=func_set ( 'COUNT', nfunc );
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TEST16');
writef(1,['%s','\n'], '  LAGUERRE_COM computes a Gauss-Laguerre rule;');
writef(1,['%s','\n'], '  LAGUERRE_SUM carries it out.');
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], '  Quadrature order will vary.');
writef(1,['%s','\n'], '  Integrand will vary.');
writef(1,['%s','\n'], '  The weight function is EXP ( - X ).');
writef(1,['%s','\n'], ' ');
%
a = 0.0d+00;
alpha = 2.0d+00;
writef(1,['%s','\n'], ' ');
writef(1,['%s','%14.6f','%s','\n'], '  The integration interval is [ ',a, ', +Infinity ).');
writef(1,['%s','%14.6f','\n'], '  ALPHA = ', alpha);
writef(1,['%s','\n'], ' ');
for ilo = 1: 5: nfunc;
ihi = fix(min( ilo + 4, nfunc ));
writef(1,['%s','\n'], ' ');
for  i =( ilo):( ihi ), writef(1,[repmat(' ',1,4),repmat('%14c',1,5),'\n'], fname(i)); end;
writef(1,['%s','\n'], ' ');
for norder = 1: 10;
for i = ilo: ihi;
[dumvar1, i ]=func_set ( 'SET', i );
[ norder, xtab, weight, alpha ]=laguerre_com( norder, xtab, weight, alpha );

[dumvar1, a, norder, xtab, weight, result(i) ]=laguerre_sum( @func, a, norder, xtab, weight, result(i) );

end; i = fix(ihi+1);
writef(1,['%2i',repmat(' ',1,2),repmat('%14.6f',1,5),'\n'], norder, result([ilo:ihi]));

end; norder = fix(10+1);
end; ilo = fix(nfunc+1);
return;
end
function test17(varargin)
%
%*******************************************************************************
%
%! TEST17 tests LAGUERRE_SET.
%! TEST17 tests LAGUERRE_SUM.
%
%
persistent a i ihi ilo maxfunc maxorder nfunc norder result weight xtab ; 

if isempty(maxfunc), maxfunc = 20; end;
if isempty(maxorder), maxorder = 20; end;
%
if isempty(a), a=0; end;
if isempty(i), i=0; end;
if isempty(ihi), ihi=0; end;
if isempty(ilo), ilo=0; end;
if isempty(nfunc), nfunc=0; end;
if isempty(norder), norder=0; end;
if isempty(result), result=zeros(1,maxfunc); end;
if isempty(weight), weight=zeros(1,maxorder); end;
if isempty(xtab), xtab=zeros(1,maxorder); end;
%
[dumvar1, nfunc ]=func_set ( 'COUNT', nfunc );
a = 1.0d+00;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TEST17');
writef(1,['%s','\n'], '  LAGUERRE_SET sets up Gauss-Laguerre quadrature;');
writef(1,['%s','\n'], '  LAGUERRE_SUM carries it out.');
writef(1,['%s','\n'], ' ');
writef(1,['%s','%14.6f','%s','\n'], '  The integration interval is [ ',a, ', +Infinity ).');
writef(1,['%s','\n'], '  Quadrature order will vary.');
writef(1,['%s','\n'], '  Integrand will vary.');
writef(1,['%s','\n'], '  The weight function is EXP ( - X ).');
writef(1,['%s','\n'], ' ');
for ilo = 1: 5: nfunc;
ihi = fix(min( ilo + 4, nfunc ));
writef(1,['%s','\n'], ' ');
for  i =( ilo):( ihi ), writef(1,[repmat(' ',1,4),repmat('%14c',1,5),'\n'], fname(i)); end;
writef(1,['%s','\n'], ' ');
for norder = 1: maxorder;
for i = ilo: ihi;
[dumvar1, i ]=func_set ( 'SET', i );
[ norder, xtab, weight ]=laguerre_set( norder, xtab, weight );

[dumvar1, a, norder, xtab, weight, result(i) ]=laguerre_sum( @func, a, norder, xtab, weight, result(i) );

end; i = fix(ihi+1);
writef(1,['%2i',repmat(' ',1,2),repmat('%14.8f',1,5),'\n'], norder, result([ilo:ihi]));

end; norder = fix(maxorder+1);
end; ilo = fix(nfunc+1);

return;
end
function test18(varargin)
%
%*******************************************************************************
%
%! TEST18 compares LEGENDRE_COM.
%! TEST18 compares LEGENDRE_SET.
%
%
persistent i iwdifmax ixdifmax norder wdifmax weight1 weight2 xdifmax xtab1 xtab2 ; 

if isempty(norder), norder = 19; end;
%
if isempty(i), i=0; end;
if isempty(iwdifmax), iwdifmax=0; end;
if isempty(ixdifmax), ixdifmax=0; end;
if isempty(wdifmax), wdifmax=0; end;
if isempty(weight1), weight1=zeros(1,norder); end;
if isempty(weight2), weight2=zeros(1,norder); end;
if isempty(xdifmax), xdifmax=0; end;
if isempty(xtab1), xtab1=zeros(1,norder); end;
if isempty(xtab2), xtab2=zeros(1,norder); end;
%
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TEST16');
writef(1,['%s','\n'], '  LEGENDRE_COM computes Gauss-Legendre data;');
writef(1,['%s','\n'], '  LEGENDRE_SET looks up the same data.');
writef(1,['%s','\n'], ' ');
writef(1,['%s','%6i','\n'], '  Compare the data for NORDER = ', norder);
[ norder, xtab1, weight1 ]=legendre_com( norder, xtab1, weight1 );
[ norder, xtab2, weight2 ]=legendre_set( norder, xtab2, weight2 );

xdifmax = 0.0d+00;
ixdifmax = 0;
wdifmax = 0.0d+00;
iwdifmax = 0;
for i = 1: norder;
if( abs( xtab1(i) - xtab2(i) ) > xdifmax )
xdifmax = abs( xtab1(i) - xtab2(i) );
ixdifmax = fix(i);
end;
if( abs( weight1(i) - weight2(i) ) > wdifmax )
wdifmax = abs( weight1(i) - weight2(i) );
iwdifmax = fix(i);
end;
end; i = fix(norder+1);
if( ixdifmax ~= 0 )
writef(1,['%s','\n'], ' ');
writef(1,['%s','%25.18f','\n'], '  Maximum abscissa difference is ', xdifmax);
writef(1,['%s','%6i','\n'], '  for index I = ', ixdifmax);
writef(1,['%s','%25.18f','\n'], 'Computed:', xtab1(ixdifmax));
writef(1,['%s','%25.18f','\n'], 'Stored:  ', xtab2(ixdifmax));
else;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], '  The computed and stored abscissas are identical.');
end;
if( iwdifmax ~= 0 )
writef(1,['%s','\n'], ' ');
writef(1,['%s','%25.18f','\n'], '  Maximum weight difference is   ', wdifmax);
writef(1,['%s','%6i','\n'], '  for index I = ', iwdifmax);
writef(1,['%s','%25.18f','\n'], 'Computed:', weight1(iwdifmax));
writef(1,['%s','%25.18f','\n'], 'Stored:  ', weight2(iwdifmax));
else;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], '  The computed and stored weights are identical.');
end;

return;
end
function test19(varargin)
%
%*******************************************************************************
%
%! TEST19 tests LEGENDRE_COM.
%! TEST19 tests SUM_SUB.
%
%
persistent a b iexp norder nsub result weight xhi xlo xtab ; 

if isempty(norder), norder = 2; end;
%
if isempty(a), a=0; end;
if isempty(b), b=0; end;
if isempty(iexp), iexp=0; end;
if isempty(nsub), nsub=0; end;
if isempty(result), result=0; end;
if isempty(weight), weight=zeros(1,norder); end;
if isempty(xhi), xhi=0; end;
if isempty(xlo), xlo=0; end;
if isempty(xtab), xtab=zeros(1,norder); end;
%
a = 0.0d+00;
b = 1.0d+00;
xlo = -1.0d+00;
xhi = +1.0d+00;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TEST19');
writef(1,['%s','\n'], '  LEGENDRE_COM computes a Gauss-Legendre rule;');
writef(1,['%s','\n'], '  SUM_SUB carries it out over subintervals.');
writef(1,['%s','\n'], ' ');
writef(1,['%s',repmat('%14.6f',1,2),'\n'], '  The integration interval is ', a, b);
writef(1,['%s','%6i','\n'], '  Here, we use a fixed order NORDER = ', norder);
writef(1,['%s','\n'], '  and use more and more subintervals.');
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], '  NSUB     Integral');
writef(1,['%s','\n'], ' ');

[ norder, xtab, weight ]=legendre_com( norder, xtab, weight );

for iexp = 0: 9;
nsub = fix(2.^iexp);
[dumvar1, a, b, nsub, norder, xlo, xhi, xtab, weight, result ]=sum_sub( @fx2sd1, a, b, nsub, norder, xlo, xhi, xtab, weight, result );
writef(1,['%4i','%16.8f','\n'], nsub, result);
end; iexp = fix(9+1);

return;
end
function test20(varargin)
%
%*******************************************************************************
%
%! TEST20 tests LEGENDRE_COM.
%! TEST20 tests SUM_SUB.
%
%
persistent a b i ihi ilo maxfunc maxorder nfunc norder nsub result weight xhi xlo xtab ; 

if isempty(maxfunc), maxfunc = 20; end;
if isempty(maxorder), maxorder = 10; end;
%
if isempty(a), a=0; end;
if isempty(b), b=0; end;
if isempty(i), i=0; end;
if isempty(ihi), ihi=0; end;
if isempty(ilo), ilo=0; end;
if isempty(nfunc), nfunc=0; end;
if isempty(norder), norder=0; end;
if isempty(nsub), nsub=0; end;
if isempty(result), result=zeros(1,maxfunc); end;
if isempty(weight), weight=zeros(1,maxorder); end;
if isempty(xhi), xhi=0; end;
if isempty(xlo), xlo=0; end;
if isempty(xtab), xtab=zeros(1,maxorder); end;
%
[dumvar1, nfunc ]=func_set ( 'COUNT', nfunc );
a = 0.0d+00;
b = 1.0d+00;
nsub = 1;
xlo = -1.0d+00;
xhi = +1.0d+00;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TEST20');
writef(1,['%s','\n'], '  LEGENDRE_COM sets up Gauss-Legendre quadrature;');
writef(1,['%s','\n'], '  SUM_SUB carries it out.');
writef(1,['%s','\n'], ' ');
writef(1,['%s',repmat('%14.6f',1,2),'\n'], '  The integration interval is ', a, b);
writef(1,['%s','%6i','\n'], '  Number of subintervals is ', nsub);
writef(1,['%s','\n'], '  Quadrature order will vary.');
writef(1,['%s','\n'], '  Integrand will vary.');
writef(1,['%s','\n'], ' ');
for ilo = 1: 5: nfunc;
ihi = fix(min( ilo + 4, nfunc ));
writef(1,['%s','\n'], ' ');
for  i =( ilo):( ihi ), writef(1,[repmat(' ',1,4),repmat('%14c',1,5),'\n'], fname(i)); end;
writef(1,['%s','\n'], ' ');
for norder = 1: maxorder;
for i = ilo: ihi;
[dumvar1, i ]=func_set ( 'SET', i );
[ norder, xtab, weight ]=legendre_com( norder, xtab, weight );
[dumvar1, a, b, nsub, norder, xlo, xhi, xtab, weight,result(i) ]=sum_sub( @func, a, b, nsub, norder, xlo, xhi, xtab, weight,result(i) );
end; i = fix(ihi+1);
writef(1,['%2i',repmat(' ',1,2),repmat('%14.8f',1,5),'\n'], norder, result([ilo:ihi]));
end; norder = fix(maxorder+1);
end; ilo = fix(nfunc+1);

return;
end
function test21(varargin)
%
%*******************************************************************************
%
%! TEST21 tests LEGENDRE_SET.
%! TEST21 tests SUM_SUB.
%
%
persistent a b i ihi ilo maxfunc maxorder nfunc norder nsub result weight xhi xlo xtab ; 

if isempty(maxfunc), maxfunc = 20; end;
if isempty(maxorder), maxorder = 20; end;
%
if isempty(a), a=0; end;
if isempty(b), b=0; end;
if isempty(i), i=0; end;
if isempty(ihi), ihi=0; end;
if isempty(ilo), ilo=0; end;
if isempty(nfunc), nfunc=0; end;
if isempty(norder), norder=0; end;
if isempty(nsub), nsub=0; end;
if isempty(result), result=zeros(1,maxfunc); end;
if isempty(weight), weight=zeros(1,maxorder); end;
if isempty(xhi), xhi=0; end;
if isempty(xlo), xlo=0; end;
if isempty(xtab), xtab=zeros(1,maxorder); end;
%
[dumvar1, nfunc ]=func_set ( 'COUNT', nfunc );
a = 0.0d+00;
b = 1.0d+00;
nsub = 1;
xlo = -1.0d+00;
xhi = +1.0d+00;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TEST21');
writef(1,['%s','\n'], '  LEGENDRE_SET sets up Gauss-Legendre quadrature;');
writef(1,['%s','\n'], '  SUM_SUB carries it out.');
writef(1,['%s','\n'], ' ');
writef(1,['%s',repmat('%14.6f',1,2),'\n'], '  The integration interval is ', a, b);
writef(1,['%s','%6i','\n'], '  Number of subintervals is ', nsub);
writef(1,['%s','\n'], '  Quadrature order will vary.');
writef(1,['%s','\n'], '  Integrand will vary.');
for ilo = 1: 5: nfunc;
ihi = fix(min( ilo + 4, nfunc ));
writef(1,['%s','\n'], ' ');
for  i =( ilo):( ihi ), writef(1,[repmat(' ',1,4),repmat('%14c',1,5),'\n'], fname(i)); end;
writef(1,['%s','\n'], ' ');
for norder = 1: maxorder;
for i = ilo: ihi;
[dumvar1, i ]=func_set ( 'SET', i );
[ norder, xtab, weight ]=legendre_set( norder, xtab, weight );
[dumvar1, a, b, nsub, norder, xlo, xhi, xtab,weight, result(i) ]=sum_sub( @func, a, b, nsub, norder, xlo, xhi, xtab,weight, result(i) );
end; i = fix(ihi+1);
writef(1,['%2i',repmat(' ',1,2),repmat('%14.8f',1,5),'\n'], norder, result([ilo:ihi]));
end; norder = fix(maxorder+1);

end; ilo = fix(nfunc+1);
return;
end
function test22(varargin)
%
%*******************************************************************************
%
%! TEST22 tests LEGENDRE_SET.
%! TEST22 tests LEGENDRE_SET_X0_01
%! TEST22 tests RULE_ADJUST.
%
%
persistent a b c d i norder weight1 weight2 weight3 xtab1 xtab2 xtab3 ; 

if isempty(norder), norder = 5; end;
%
if isempty(a), a=0; end;
if isempty(b), b=0; end;
if isempty(c), c=0; end;
if isempty(d), d=0; end;
if isempty(i), i=0; end;
if isempty(weight1), weight1=zeros(1,norder); end;
if isempty(weight2), weight2=zeros(1,norder); end;
if isempty(weight3), weight3=zeros(1,norder); end;
if isempty(xtab1), xtab1=zeros(1,norder); end;
if isempty(xtab2), xtab2=zeros(1,norder); end;
if isempty(xtab3), xtab3=zeros(1,norder); end;
%
a = -1.0d+00;
b = +1.0d+00;
c =  0.0d+00;
d =  1.0d+00;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TEST22');
writef(1,['%s','\n'], '  LEGENDRE_SET sets up Gauss-Legendre quadrature');
writef(1,['%s','\n'], '    for integrating F(X) over [-1,1];');
writef(1,['%s','\n'], '  RULE_ADJUST adjusts a rule for a new interval.');
writef(1,['%s','\n'], '  LEGENDRE_SET_X0_01 sets up Gauss-Legendre quadrature');
writef(1,['%s','\n'], '    for integrating F(X) over [0,1];');
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], '  We will use LEGENDRE_SET to get a rule for [-1,1],');
writef(1,['%s','\n'], '  adjust it to [0,1] using RULE_ADJUST,');
writef(1,['%s','\n'], '  and compare the results of LEGENDRE_SET_X0_01.');
writef(1,['%s','\n'], ' ');
[ norder, xtab1, weight1 ]=legendre_set( norder, xtab1, weight1 );
xtab2([1:norder]) = xtab1([1:norder]);
weight2([1:norder]) = weight1([1:norder]);
[ a, b, c, d, norder, xtab2, weight2 ]=rule_adjust( a, b, c, d, norder, xtab2, weight2 );
[ norder, xtab3, weight3 ]=legendre_set_x0_01( norder, xtab3, weight3 );
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], '  Abscissas:');
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], '  Original  Adjusted Stored');
writef(1,['%s','\n'], ' ');
for i = 1: norder;
writef(1,['%2i',repmat('%16.12f',1,3),'\n'], i, xtab1(i), xtab2(i), xtab3(i));
end; i = fix(norder+1);
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], '  Weights:');
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], '  Original  Adjusted Stored');
writef(1,['%s','\n'], ' ');
for i = 1: norder;
writef(1,['%2i',repmat('%16.12f',1,3),'\n'], i, weight1(i), weight2(i), weight3(i));
end; i = fix(norder+1);
return;
end
function test23(varargin)
%
%*******************************************************************************
%
%! TEST23 tests LEGENDRE_SET_COS.
%! TEST23 tests SUM_SUB.
%
%
persistent a b i iexp ihi ilo maxfunc maxorder nfunc norder nsub result weight xhi xlo xtab ; 

if isempty(maxfunc), maxfunc = 20; end;
if isempty(maxorder), maxorder = 20; end;
%
if isempty(a), a=0; end;
if isempty(b), b=0; end;
if isempty(i), i=0; end;
if isempty(iexp), iexp=0; end;
if isempty(ihi), ihi=0; end;
if isempty(ilo), ilo=0; end;
if isempty(nfunc), nfunc=0; end;
if isempty(norder), norder=0; end;
if isempty(nsub), nsub=0; end;
if isempty(result), result=zeros(1,maxfunc); end;
if isempty(weight), weight=zeros(1,maxorder); end;
if isempty(xhi), xhi=0; end;
if isempty(xlo), xlo=0; end;
if isempty(xtab), xtab=zeros(1,maxorder); end;
%
a = -0.5d+00 .* d_pi( );
b = +0.5d+00 .* d_pi( );
nsub = 1;
xlo = -0.5d+00 .* d_pi( );
xhi = +0.5d+00 .* d_pi( );
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TEST23');
writef(1,['%s','\n'], '  LEGENDRE_SET_COS sets up Gauss-Legendre quadrature');
writef(1,['%s','\n'], '    over [-PI/2,PI/2] with weight function COS(X);');
writef(1,['%s','\n'], '  SUM_SUB carries it out.');
writef(1,['%s','\n'], ' ');
writef(1,['%s',repmat('%14.6f',1,2),'\n'], '  The integration interval is ', a, b);
writef(1,['%s','%6i','\n'], '  Number of subintervals is ', nsub);
writef(1,['%s','\n'], '  Quadrature order will vary.');
writef(1,['%s','\n'], '  Integrand will vary.');
[dumvar1, nfunc ]=func_set ( 'COUNT', nfunc );
for ilo = 1: 5: nfunc;
ihi = fix(min( ilo + 4, nfunc ));
writef(1,['%s','\n'], ' ');
for  i =( ilo):( ihi ), writef(1,[repmat(' ',1,4),repmat('%14c',1,5),'\n'], fname(i)); end;
writef(1,['%s','\n'], ' ');
for iexp = 0: 4;
norder = fix(2.^iexp);
for i = ilo: ihi;
[dumvar1, i ]=func_set ( 'SET', i );
[ norder, xtab, weight ]=legendre_set_cos( norder, xtab, weight );
[dumvar1, a, b, nsub, norder, xlo, xhi, xtab,weight, result(i) ]=sum_sub( @func, a, b, nsub, norder, xlo, xhi, xtab,weight, result(i) );
end; i = fix(ihi+1);
writef(1,['%2i',repmat(' ',1,2),repmat('%14.8f',1,5),'\n'], norder, result([ilo:ihi]));
end; iexp = fix(4+1);
end; ilo = fix(nfunc+1);
return;
end
function test235(varargin)
%
%*******************************************************************************
%
%! TEST235 tests LEGENDRE_SET_SQRTX_01.
%! TEST235 tests SUM_SUB.
%
%
persistent a b i iexp ihi ilo maxfunc maxorder nfunc norder nsub result weight xhi xlo xtab ; 

if isempty(maxfunc), maxfunc = 20; end;
if isempty(maxorder), maxorder = 20; end;
%
if isempty(a), a=0; end;
if isempty(b), b=0; end;
if isempty(i), i=0; end;
if isempty(iexp), iexp=0; end;
if isempty(ihi), ihi=0; end;
if isempty(ilo), ilo=0; end;
if isempty(nfunc), nfunc=0; end;
if isempty(norder), norder=0; end;
if isempty(nsub), nsub=0; end;
if isempty(result), result=zeros(1,maxfunc); end;
if isempty(weight), weight=zeros(1,maxorder); end;
if isempty(xhi), xhi=0; end;
if isempty(xlo), xlo=0; end;
if isempty(xtab), xtab=zeros(1,maxorder); end;
%
a = 0.0d+00;
b = 1.0d+00;
nsub = 1;
xlo = 0.0d+00;
xhi = 1.0d+00;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TEST235');
writef(1,['%s','\n'], '  LEGENDRE_SET_SQRTX_01 sets up Gauss-Legendre quadrature');
writef(1,['%s','\n'], '    over [0,1] with weight function SQRT(X);');
writef(1,['%s','\n'], '  SUM_SUB carries it out.');
writef(1,['%s','\n'], ' ');
writef(1,['%s',repmat('%14.6f',1,2),'\n'], '  The integration interval is ', a, b);
writef(1,['%s','%6i','\n'], '  Number of subintervals is ', nsub);
writef(1,['%s','\n'], '  Quadrature order will vary.');
writef(1,['%s','\n'], '  Integrand will vary.');
[dumvar1, nfunc ]=func_set ( 'COUNT', nfunc );
for ilo = 1: 5: nfunc;
ihi = fix(min( ilo + 4, nfunc ));
writef(1,['%s','\n'], ' ');
for  i =( ilo):( ihi ), writef(1,[repmat(' ',1,4),repmat('%14c',1,5),'\n'], fname(i)); end;
writef(1,['%s','\n'], ' ');
for iexp = 0: 3;
norder = fix(2.^iexp);
for i = ilo: ihi;
[dumvar1, i ]=func_set ( 'SET', i );
[ norder, xtab, weight ]=legendre_set_sqrtx_01( norder, xtab, weight );
[dumvar1, norder, xtab, weight, result(i) ]=summer( @func, norder, xtab, weight, result(i) );
end; i = fix(ihi+1);
writef(1,['%2i',repmat(' ',1,2),repmat('%14.8f',1,5),'\n'], norder, result([ilo:ihi]));
end; iexp = fix(3+1);
end; ilo = fix(nfunc+1);
return;
end
function test236(varargin)
%
%*******************************************************************************
%
%! TEST236 tests LEGENDRE_SET_SQRTX2_01.
%! TEST236 tests SUM_SUB.
%
%
persistent a b i iexp ihi ilo maxfunc maxorder nfunc norder nsub result weight xhi xlo xtab ; 

if isempty(maxfunc), maxfunc = 20; end;
if isempty(maxorder), maxorder = 20; end;
%
if isempty(a), a=0; end;
if isempty(b), b=0; end;
if isempty(i), i=0; end;
if isempty(iexp), iexp=0; end;
if isempty(ihi), ihi=0; end;
if isempty(ilo), ilo=0; end;
if isempty(nfunc), nfunc=0; end;
if isempty(norder), norder=0; end;
if isempty(nsub), nsub=0; end;
if isempty(result), result=zeros(1,maxfunc); end;
if isempty(weight), weight=zeros(1,maxorder); end;
if isempty(xhi), xhi=0; end;
if isempty(xlo), xlo=0; end;
if isempty(xtab), xtab=zeros(1,maxorder); end;
%
a = 0.0d+00;
b = 1.0d+00;
nsub = 1;
xlo = 0.0d+00;
xhi = 1.0d+00;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TEST236');
writef(1,['%s','\n'], '  LEGENDRE_SET_SQRTX2_01 sets up Gauss-Legendre');
writef(1,['%s','\n'], '    quadrature over [0,1] with weight function 1/SQRT(X);');
writef(1,['%s','\n'], '  SUM_SUB carries it out.');
writef(1,['%s','\n'], ' ');
writef(1,['%s',repmat('%14.6f',1,2),'\n'], '  The integration interval is ', a, b);
writef(1,['%s','%6i','\n'], '  Number of subintervals is ', nsub);
writef(1,['%s','\n'], '  Quadrature order will vary.');
writef(1,['%s','\n'], '  Integrand will vary.');
[dumvar1, nfunc ]=func_set ( 'COUNT', nfunc );
for ilo = 1: 5: nfunc;
ihi = fix(min( ilo + 4, nfunc ));
writef(1,['%s','\n'], ' ');
for  i =( ilo):( ihi ), writef(1,[repmat(' ',1,4),repmat('%14c',1,5),'\n'], fname(i)); end;
writef(1,['%s','\n'], ' ');
for iexp = 0: 3;
norder = fix(2.^iexp);
for i = ilo: ihi;
[dumvar1, i ]=func_set ( 'SET', i );
[ norder, xtab, weight ]=legendre_set_sqrtx2_01( norder, xtab, weight );
[dumvar1, norder, xtab, weight, result(i) ]=summer( @func, norder, xtab, weight, result(i) );
end; i = fix(ihi+1);
writef(1,['%2i',repmat(' ',1,2),repmat('%14.8f',1,5),'\n'], norder, result([ilo:ihi]));
end; iexp = fix(3+1);
end; ilo = fix(nfunc+1);
return;
end
function test24(varargin)
%
%*******************************************************************************
%
%! TEST24 tests LEGENDRE_SET_COS2.
%! TEST24 tests SUM_SUB.
%
%
persistent a b i iexp ihi ilo maxfunc maxorder nfunc norder nsub result weight xhi xlo xtab ; 

if isempty(maxfunc), maxfunc = 20; end;
if isempty(maxorder), maxorder = 20; end;
%
if isempty(a), a=0; end;
if isempty(b), b=0; end;
if isempty(i), i=0; end;
if isempty(iexp), iexp=0; end;
if isempty(ihi), ihi=0; end;
if isempty(ilo), ilo=0; end;
if isempty(nfunc), nfunc=0; end;
if isempty(norder), norder=0; end;
if isempty(nsub), nsub=0; end;
if isempty(result), result=zeros(1,maxfunc); end;
if isempty(weight), weight=zeros(1,maxorder); end;
if isempty(xhi), xhi=0; end;
if isempty(xlo), xlo=0; end;
if isempty(xtab), xtab=zeros(1,maxorder); end;
%
a = -0.5d+00 .* d_pi( );
b = +0.5d+00 .* d_pi( );
nsub = 1;
xlo = -0.5d+00 .* d_pi( );
xhi = +0.5d+00 .* d_pi( );
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TEST24');
writef(1,['%s','\n'], '  LEGENDRE_SET_COS2 sets up Gauss-Legendre quadrature');
writef(1,['%s','\n'], '    over [0,PI/2] with weight function COS(X);');
writef(1,['%s','\n'], '  SUM_SUB carries it out.');
writef(1,['%s','\n'], ' ');
writef(1,['%s',repmat('%14.6f',1,2),'\n'], '  The integration interval is ', a, b);
writef(1,['%s','%6i','\n'], '  Number of subintervals is ', nsub);
writef(1,['%s','\n'], '  Quadrature order will vary.');
writef(1,['%s','\n'], '  Integrand will vary.');
[dumvar1, nfunc ]=func_set ( 'COUNT', nfunc );
for ilo = 1: 5: nfunc;
ihi = fix(min( ilo + 4, nfunc ));
writef(1,['%s','\n'], ' ');
for  i =( ilo):( ihi ), writef(1,[repmat(' ',1,4),repmat('%14c',1,5),'\n'], fname(i)); end;
writef(1,['%s','\n'], ' ');
for iexp = 1: 4;
norder = fix(2.^iexp);
for i = ilo: ihi;
[dumvar1, i ]=func_set ( 'SET', i );
[ norder, xtab, weight ]=legendre_set_cos2( norder, xtab, weight );
[dumvar1, a, b, nsub, norder, xlo, xhi, xtab,weight, result(i) ]=sum_sub( @func, a, b, nsub, norder, xlo, xhi, xtab,weight, result(i) );
end; i = fix(ihi+1);
writef(1,['%2i',repmat(' ',1,2),repmat('%14.8f',1,5),'\n'], norder, result([ilo:ihi]));
end; iexp = fix(4+1);
end; ilo = fix(nfunc+1);
return;
end
function test25(varargin)
%
%*******************************************************************************
%
%! TEST25 tests LEGENDRE_SET_LOG.
%! TEST25 tests SUM_SUB.
%
%
persistent a b i iexp ihi ilo itest maxfunc maxorder nfunc norder norder_test nsub ntest result weight xhi xlo xtab ; 

if isempty(maxfunc), maxfunc = 20; end;
if isempty(maxorder), maxorder = 20; end;
if isempty(ntest), ntest = 9; end;
%
if isempty(a), a=0; end;
if isempty(b), b=0; end;
if isempty(i), i=0; end;
if isempty(iexp), iexp=0; end;
if isempty(ihi), ihi=0; end;
if isempty(ilo), ilo=0; end;
if isempty(itest), itest=0; end;
if isempty(nfunc), nfunc=0; end;
if isempty(norder), norder=0; end;
if isempty(norder_test), norder_test([1:ntest]) =[ 1, 2, 3, 4, 5, 6, 7, 8, 16 ]; end;
if isempty(nsub), nsub=0; end;
if isempty(result), result=zeros(1,maxfunc); end;
if isempty(weight), weight=zeros(1,maxorder); end;
if isempty(xhi), xhi=0; end;
if isempty(xlo), xlo=0; end;
if isempty(xtab), xtab=zeros(1,maxorder); end;
%
a = 0.0d+00;
b = 1.0d+00;
nsub = 1;
xlo = 0.0d+00;
xhi = 1.0d+00;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TEST25');
writef(1,['%s','\n'], '  LEGENDRE_SET_LOG sets up Gauss-Legendre quadrature');
writef(1,['%s','\n'], '    over [0,1] with weight function -LOG(X);');
writef(1,['%s','\n'], '  SUM_SUB carries it out.');
writef(1,['%s','\n'], ' ');
writef(1,['%s',repmat('%14.6f',1,2),'\n'], '  The integration interval is ', a, b);
writef(1,['%s','%6i','\n'], '  Number of subintervals is ', nsub);
writef(1,['%s','\n'], '  Quadrature order will vary.');
writef(1,['%s','\n'], '  Integrand will vary.');
[dumvar1, nfunc ]=func_set ( 'COUNT', nfunc );
for ilo = 1: 5: nfunc;
ihi = fix(min( ilo + 4, nfunc ));
writef(1,['%s','\n'], ' ');
for  i =( ilo):( ihi ), writef(1,[repmat(' ',1,4),repmat('%14c',1,5),'\n'], fname(i)); end;
writef(1,['%s','\n'], ' ');
for itest = 1: ntest;
norder = fix(norder_test(itest));

for i = ilo: ihi;
[dumvar1, i ]=func_set ( 'SET', i );
[ norder, xtab, weight ]=legendre_set_log( norder, xtab, weight );
[dumvar1, a, b, nsub, norder,  xlo, xhi, xtab, weight,result(i) ]=sum_sub( @func, a, b, nsub, norder,  xlo, xhi, xtab, weight,result(i) );
end; i = fix(ihi+1);
writef(1,['%2i',repmat(' ',1,2),repmat('%14.8f',1,5),'\n'], norder, result([ilo:ihi]));
end; itest = fix(ntest+1);

end; ilo = fix(nfunc+1);
return;
end
function test26(varargin)
%
%*******************************************************************************
%
%! TEST26 tests LEGENDRE_SET_X0_01.
%! TEST26 tests SUM_SUB.
%
%
persistent a b i ihi ilo maxfunc maxorder nfunc norder nsub result weight xhi xlo xtab ; 

if isempty(maxfunc), maxfunc = 20; end;
if isempty(maxorder), maxorder = 8; end;
%
if isempty(a), a=0; end;
if isempty(b), b=0; end;
if isempty(i), i=0; end;
if isempty(ihi), ihi=0; end;
if isempty(ilo), ilo=0; end;
if isempty(nfunc), nfunc=0; end;
if isempty(norder), norder=0; end;
if isempty(nsub), nsub=0; end;
if isempty(result), result=zeros(1,maxfunc); end;
if isempty(weight), weight=zeros(1,maxorder); end;
if isempty(xhi), xhi=0; end;
if isempty(xlo), xlo=0; end;
if isempty(xtab), xtab=zeros(1,maxorder); end;
%
[dumvar1, nfunc ]=func_set ( 'COUNT', nfunc );
a = 0.0d+00;
b = 1.0d+00;
nsub = 1;
xlo =  0.0d+00;
xhi =  1.0d+00;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TEST26');
writef(1,['%s','\n'], '  LEGENDRE_SET_X0_01 sets up Gauss-Legendre quadrature');
writef(1,['%s','\n'], '    for integrating F(X) over [0,1];');
writef(1,['%s','\n'], '  SUM_SUB carries it out.');
writef(1,['%s','\n'], ' ');
writef(1,['%s',repmat('%14.6f',1,2),'\n'], '  The integration interval is ', a, b);
writef(1,['%s','%6i','\n'], '  Number of subintervals is ', nsub);
writef(1,['%s','\n'], '  Quadrature order will vary.');
writef(1,['%s','\n'], '  Integrand will vary.');
writef(1,['%s','\n'], ' ');
for ilo = 1: 5: nfunc;
ihi = fix(min( ilo + 4, nfunc ));
writef(1,['%s','\n'], ' ');
for  i =( ilo):( ihi ), writef(1,[repmat(' ',1,4),repmat('%14c',1,5),'\n'], fname(i)); end;
writef(1,['%s','\n'], ' ');
for norder = 1: maxorder;
for i = ilo: ihi;
[dumvar1, i ]=func_set ( 'SET', i );
[ norder, xtab, weight ]=legendre_set_x0_01( norder, xtab, weight );
[dumvar1, a, b, nsub, norder, xlo, xhi, xtab, weight,result(i) ]=sum_sub( @func, a, b, nsub, norder, xlo, xhi, xtab, weight,result(i) );
end; i = fix(ihi+1);
writef(1,['%2i',repmat(' ',1,2),repmat('%14.8f',1,5),'\n'], norder, result([ilo:ihi]));
end; norder = fix(maxorder+1);
end; ilo = fix(nfunc+1);
return;
end
function test27(varargin)
%
%*******************************************************************************
%
%! TEST27 tests LEGENDRE_SET_X1.
%! TEST27 tests SUM_SUB.
%
%
persistent a b i ihi ilo maxfunc maxorder nfunc norder nsub result weight xhi xlo xtab ; 

if isempty(maxfunc), maxfunc = 20; end;
if isempty(maxorder), maxorder = 9; end;
%
if isempty(a), a=0; end;
if isempty(b), b=0; end;
if isempty(i), i=0; end;
if isempty(ihi), ihi=0; end;
if isempty(ilo), ilo=0; end;
if isempty(nfunc), nfunc=0; end;
if isempty(norder), norder=0; end;
if isempty(nsub), nsub=0; end;
if isempty(result), result=zeros(1,maxfunc); end;
if isempty(weight), weight=zeros(1,maxorder); end;
if isempty(xhi), xhi=0; end;
if isempty(xlo), xlo=0; end;
if isempty(xtab), xtab=zeros(1,maxorder); end;
%
[dumvar1, nfunc ]=func_set ( 'COUNT', nfunc );
a =  0.0d+00;
b =  1.0d+00;
nsub = 1;
xlo = -1.0d+00;
xhi = +1.0d+00;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TEST27');
writef(1,['%s','\n'], '  LEGENDRE_SET_X1 sets up Gauss-Legendre quadrature');
writef(1,['%s','\n'], '    for integrating ( 1 + X ) * F(X) over [-1,1];');
writef(1,['%s','\n'], '  SUM_SUB carries it out.');
writef(1,['%s','\n'], ' ');
writef(1,['%s',repmat('%14.6f',1,2),'\n'], '  The integration interval is ', a, b);
writef(1,['%s','%6i','\n'], '  Number of subintervals is ', nsub);
writef(1,['%s','\n'], '  Quadrature order will vary.');
writef(1,['%s','\n'], '  Integrand will vary.');
writef(1,['%s','\n'], ' ');
for ilo = 1: 5: nfunc;
ihi = fix(min( ilo + 4, nfunc ));
writef(1,['%s','\n'], ' ');
for  i =( ilo):( ihi ), writef(1,[repmat(' ',1,4),repmat('%14c',1,5),'\n'], fname(i)); end;
writef(1,['%s','\n'], ' ');
for norder = 1: maxorder;
for i = ilo: ihi;
[dumvar1, i ]=func_set ( 'SET', i );
[ norder, xtab, weight ]=legendre_set_x1( norder, xtab, weight );
[dumvar1, a, b, nsub, norder, xlo, xhi, xtab, weight,result(i) ]=sum_sub( @func, a, b, nsub, norder, xlo, xhi, xtab, weight,result(i) );
end; i = fix(ihi+1);
writef(1,['%2i',repmat(' ',1,2),repmat('%14.8f',1,5),'\n'], norder, result([ilo:ihi]));
end; norder = fix(maxorder+1);
end; ilo = fix(nfunc+1);
return;
end
function test28(varargin)
%
%*******************************************************************************
%
%! TEST28 tests LEGENDRE_SET_X1.
%! TEST28 tests LEGENDRE_SET_X1_01
%! TEST28 tests RULE_ADJUST.
%
%
persistent a b c d i norder weight1 weight2 weight3 xtab1 xtab2 xtab3 ; 

if isempty(norder), norder = 5; end;
%
if isempty(a), a=0; end;
if isempty(b), b=0; end;
if isempty(c), c=0; end;
if isempty(d), d=0; end;
if isempty(i), i=0; end;
if isempty(weight1), weight1=zeros(1,norder); end;
if isempty(weight2), weight2=zeros(1,norder); end;
if isempty(weight3), weight3=zeros(1,norder); end;
if isempty(xtab1), xtab1=zeros(1,norder); end;
if isempty(xtab2), xtab2=zeros(1,norder); end;
if isempty(xtab3), xtab3=zeros(1,norder); end;
%
a = -1.0d+00;
b = +1.0d+00;
c =  0.0d+00;
d =  1.0d+00;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TEST28');
writef(1,['%s','\n'], '  LEGENDRE_SET_X1 sets up Gauss-Legendre quadrature');
writef(1,['%s','\n'], '    for integrating ( 1 + X ) * F(X) over [-1,1];');
writef(1,['%s','\n'], '  RULE_ADJUST adjusts a rule for a new interval.');
writef(1,['%s','\n'], '  LEGENDRE_SET_X1_01 sets up Gauss-Legendre quadrature');
writef(1,['%s','\n'], '    for integrating X * F(X) over [0,1];');
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], '  We will use LEGENDRE_SET_X1 to get a rule for [-1,1],');
writef(1,['%s','\n'], '  adjust it to [0,1] using RULE_ADJUST,');
writef(1,['%s','\n'], '  make further adjustments because the weight function');
writef(1,['%s','\n'], '  is not 1, ');
writef(1,['%s','\n'], '  and compare the results of LEGENDRE_SET_X1_01.');
writef(1,['%s','\n'], ' ');
[ norder, xtab1, weight1 ]=legendre_set_x1( norder, xtab1, weight1 );
xtab2([1:norder]) = xtab1([1:norder]);
weight2([1:norder]) = weight1([1:norder]);
[ a, b, c, d, norder, xtab2, weight2 ]=rule_adjust( a, b, c, d, norder, xtab2, weight2 );
%
%  Because the weight function W(X) is not 1, we need to do more
%  adjustments to the weight vector.
%
weight2([1:norder]) = weight2([1:norder]) ./ 2.0d+00;
[ norder, xtab3, weight3 ]=legendre_set_x1_01( norder, xtab3, weight3 );
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], '  Abscissas:');
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], '  Original        Adjusted         Stored');
writef(1,['%s','\n'], ' ');
for i = 1: norder;
writef(1,['%2i',repmat('%16.12f',1,3),'\n'], i, xtab1(i), xtab2(i), xtab3(i));
end; i = fix(norder+1);
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], '  Weights:');
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], '  Original  Adjusted Stored');
writef(1,['%s','\n'], ' ');
for i = 1: norder;
writef(1,['%2i',repmat('%16.12f',1,3),'\n'], i, weight1(i), weight2(i), weight3(i));
end; i = fix(norder+1);
return;
end
function test29(varargin)
%
%*******************************************************************************
%
%! TEST29 tests LEGENDRE_SET_X1_01.
%! TEST29 tests SUM_SUB.
%
%
persistent a b i ihi ilo maxfunc maxorder nfunc norder nsub result weight xhi xlo xtab ; 

if isempty(maxfunc), maxfunc = 20; end;
if isempty(maxorder), maxorder = 8; end;
%
if isempty(a), a=0; end;
if isempty(b), b=0; end;
if isempty(i), i=0; end;
if isempty(ihi), ihi=0; end;
if isempty(ilo), ilo=0; end;
if isempty(nfunc), nfunc=0; end;
if isempty(norder), norder=0; end;
if isempty(nsub), nsub=0; end;
if isempty(result), result=zeros(1,maxfunc); end;
if isempty(weight), weight=zeros(1,maxorder); end;
if isempty(xhi), xhi=0; end;
if isempty(xlo), xlo=0; end;
if isempty(xtab), xtab=zeros(1,maxorder); end;
%
[dumvar1, nfunc ]=func_set ( 'COUNT', nfunc );
a = 0.0d+00;
b = 1.0d+00;
nsub = 1;
xlo = 0.0d+00;
xhi = 1.0d+00;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TEST29');
writef(1,['%s','\n'], '  LEGENDRE_SET_X1_01 sets up Gauss-Legendre quadrature');
writef(1,['%s','\n'], '    for integrating X * F(X) over [0,1];');
writef(1,['%s','\n'], '  SUM_SUB carries it out.');
writef(1,['%s','\n'], ' ');
writef(1,['%s',repmat('%14.6f',1,2),'\n'], '  The integration interval is ', a, b);
writef(1,['%s','%6i','\n'], '  Number of subintervals is ', nsub);
writef(1,['%s','\n'], '  Quadrature order will vary.');
writef(1,['%s','\n'], '  Integrand will vary.');
writef(1,['%s','\n'], ' ');
for ilo = 1: 5: nfunc;
ihi = fix(min( ilo + 4, nfunc ));
writef(1,['%s','\n'], ' ');
for  i =( ilo):( ihi ), writef(1,[repmat(' ',1,4),repmat('%14c',1,5),'\n'], fname(i)); end;
writef(1,['%s','\n'], ' ');
for norder = 1: maxorder;
for i = ilo: ihi;
[dumvar1, i ]=func_set ( 'SET', i );
[ norder, xtab, weight ]=legendre_set_x1_01( norder, xtab, weight );
[dumvar1, a, b, nsub, norder, xlo, xhi, xtab, weight,result(i) ]=sum_sub( @func, a, b, nsub, norder, xlo, xhi, xtab, weight,result(i) );
end; i = fix(ihi+1);
writef(1,['%2i',repmat(' ',1,2),repmat('%14.8f',1,5),'\n'], norder, result([ilo:ihi]));
end; norder = fix(maxorder+1);
end; ilo = fix(nfunc+1);
return;
end
function test30(varargin)
%
%*******************************************************************************
%
%! TEST30 tests LEGENDRE_SET_X2.
%! TEST30 tests SUM_SUB
%
%
persistent a b i ihi ilo maxfunc maxorder nfunc norder nsub result weight xhi xlo xtab ; 

if isempty(maxfunc), maxfunc = 20; end;
if isempty(maxorder), maxorder = 9; end;
%
if isempty(a), a=0; end;
if isempty(b), b=0; end;
if isempty(i), i=0; end;
if isempty(ihi), ihi=0; end;
if isempty(ilo), ilo=0; end;
if isempty(nfunc), nfunc=0; end;
if isempty(norder), norder=0; end;
if isempty(nsub), nsub=0; end;
if isempty(result), result=zeros(1,maxfunc); end;
if isempty(weight), weight=zeros(1,maxorder); end;
if isempty(xhi), xhi=0; end;
if isempty(xlo), xlo=0; end;
if isempty(xtab), xtab=zeros(1,maxorder); end;
%
[dumvar1, nfunc ]=func_set ( 'COUNT', nfunc );
a = 0.0d+00;
b = 1.0d+00;
nsub = 1;
xlo = -1.0d+00;
xhi = +1.0d+00;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TEST30');
writef(1,['%s','\n'], '  LEGENDRE_SET_X2 sets up Gauss-Legendre quadrature');
writef(1,['%s','\n'], '    for integrating (1+X)**2 * F(X) over [-1,1];');
writef(1,['%s','\n'], '  SUM_SUB carries it out.');
writef(1,['%s','\n'], ' ');
writef(1,['%s',repmat('%14.6f',1,2),'\n'], '  The integration interval is ', a, b);
writef(1,['%s','%6i','\n'], '  Number of subintervals is ', nsub);
writef(1,['%s','\n'], '  Quadrature order will vary.');
writef(1,['%s','\n'], '  Integrand will vary.');
writef(1,['%s','\n'], ' ');
for ilo = 1: 5: nfunc;
ihi = fix(min( ilo + 4, nfunc ));
writef(1,['%s','\n'], ' ');
for  i =( ilo):( ihi ), writef(1,[repmat(' ',1,4),repmat('%14c',1,5),'\n'], fname(i)); end;
writef(1,['%s','\n'], ' ');
for norder = 1: maxorder;
for i = ilo: ihi;
[dumvar1, i ]=func_set ( 'SET', i );
[ norder, xtab, weight ]=legendre_set_x2( norder, xtab, weight );
[dumvar1, a, b, nsub, norder, xlo, xhi, xtab, weight,result(i) ]=sum_sub( @func, a, b, nsub, norder, xlo, xhi, xtab, weight,result(i) );
end; i = fix(ihi+1);
writef(1,['%2i',repmat(' ',1,2),repmat('%14.8f',1,5),'\n'], norder, result([ilo:ihi]));
end; norder = fix(maxorder+1);
end; ilo = fix(nfunc+1);
return;
end
function test31(varargin)
%
%*******************************************************************************
%
%! TEST31 tests LEGENDRE_SET_X2.
%! TEST31 tests LEGENDRE_SET_X2_01
%! TEST31 tests RULE_ADJUST.
%
%
persistent a b c d i norder weight1 weight2 weight3 xtab1 xtab2 xtab3 ; 

if isempty(norder), norder = 5; end;
%
if isempty(a), a=0; end;
if isempty(b), b=0; end;
if isempty(c), c=0; end;
if isempty(d), d=0; end;
if isempty(i), i=0; end;
if isempty(weight1), weight1=zeros(1,norder); end;
if isempty(weight2), weight2=zeros(1,norder); end;
if isempty(weight3), weight3=zeros(1,norder); end;
if isempty(xtab1), xtab1=zeros(1,norder); end;
if isempty(xtab2), xtab2=zeros(1,norder); end;
if isempty(xtab3), xtab3=zeros(1,norder); end;
%
a = -1.0d+00;
b = +1.0d+00;
c =  0.0d+00;
d =  1.0d+00;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TEST31');
writef(1,['%s','\n'], '  LEGENDRE_SET_X2 sets up Gauss-Legendre quadrature');
writef(1,['%s','\n'], '    for integrating ( 1 + X )^2 * F(X) over [-1,1];');
writef(1,['%s','\n'], '  RULE_ADJUST adjusts a rule for a new interval.');
writef(1,['%s','\n'], '  LEGENDRE_SET_X2_01 sets up Gauss-Legendre quadrature');
writef(1,['%s','\n'], '    for integrating X^2 * F(X) over [0,1];');
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], '  We will use LEGENDRE_SET_X2 to get a rule for [-1,1],');
writef(1,['%s','\n'], '  adjust it to [0,1] using RULE_ADJUST,');
writef(1,['%s','\n'], '  make further adjustments because the weight function');
writef(1,['%s','\n'], '  is not 1, ');
writef(1,['%s','\n'], '  and compare the results of LEGENDRE_SET_X2_01.');
writef(1,['%s','\n'], ' ');
[ norder, xtab1, weight1 ]=legendre_set_x2( norder, xtab1, weight1 );
xtab2([1:norder]) = xtab1([1:norder]);
weight2([1:norder]) = weight1([1:norder]);
[ a, b, c, d, norder, xtab2, weight2 ]=rule_adjust( a, b, c, d, norder, xtab2, weight2 );
%
%  Because the weight function W(X) is not 1, we need to do more
%  adjustments to the weight vector.
%
weight2([1:norder]) = weight2([1:norder]) ./ 4.0d+00;
[ norder, xtab3, weight3 ]=legendre_set_x2_01( norder, xtab3, weight3 );
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], '  Abscissas:');
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], '  Original  Adjusted Stored');
writef(1,['%s','\n'], ' ');
for i = 1: norder;
writef(1,['%2i',repmat('%16.12f',1,3),'\n'], i, xtab1(i), xtab2(i), xtab3(i));
end; i = fix(norder+1);
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], '  Weights:');
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], '  Original  Adjusted Stored');
writef(1,['%s','\n'], ' ');
for i = 1: norder;
writef(1,['%2i',repmat('%16.12f',1,3),'\n'], i, weight1(i), weight2(i), weight3(i));
end; i = fix(norder+1);
return;
end
function test32(varargin)
%
%*******************************************************************************
%
%! TEST32 tests LEGENDRE_SET_X2_01.
%! TEST32 tests SUM_SUB
%
%
persistent a b i ihi ilo maxfunc maxorder nfunc norder nsub result weight xhi xlo xtab ; 

if isempty(maxfunc), maxfunc = 20; end;
if isempty(maxorder), maxorder = 8; end;
%
if isempty(a), a=0; end;
if isempty(b), b=0; end;
if isempty(i), i=0; end;
if isempty(ihi), ihi=0; end;
if isempty(ilo), ilo=0; end;
if isempty(nfunc), nfunc=0; end;
if isempty(norder), norder=0; end;
if isempty(nsub), nsub=0; end;
if isempty(result), result=zeros(1,maxfunc); end;
if isempty(weight), weight=zeros(1,maxorder); end;
if isempty(xhi), xhi=0; end;
if isempty(xlo), xlo=0; end;
if isempty(xtab), xtab=zeros(1,maxorder); end;
%
[dumvar1, nfunc ]=func_set ( 'COUNT', nfunc );
a = 0.0d+00;
b = 1.0d+00;
nsub = 1;
xlo = 0.0d+00;
xhi = 1.0d+00;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TEST32');
writef(1,['%s','\n'], '  LEGENDRE_SET_X2_01 sets up Gauss-Legendre quadrature');
writef(1,['%s','\n'], '    for integrating X*X * F(X) over [0,1];');
writef(1,['%s','\n'], '  SUM_SUB carries it out.');
writef(1,['%s','\n'], ' ');
writef(1,['%s',repmat('%14.6f',1,2),'\n'], '  The integration interval is ', a, b);
writef(1,['%s','%6i','\n'], '  Number of subintervals is ', nsub);
writef(1,['%s','\n'], '  Quadrature order will vary.');
writef(1,['%s','\n'], '  Integrand will vary.');
writef(1,['%s','\n'], ' ');
for ilo = 1: 5: nfunc;
ihi = fix(min( ilo + 4, nfunc ));
writef(1,['%s','\n'], ' ');
for  i =( ilo):( ihi ), writef(1,[repmat(' ',1,4),repmat('%14c',1,5),'\n'], fname(i)); end;
writef(1,['%s','\n'], ' ');

for norder = 1: maxorder;
for i = ilo: ihi;
[dumvar1, i ]=func_set ( 'SET', i );
[ norder, xtab, weight ]=legendre_set_x2_01( norder, xtab, weight );
[dumvar1, a, b, nsub, norder, xlo, xhi, xtab, weight,result(i) ]=sum_sub( @func, a, b, nsub, norder, xlo, xhi, xtab, weight,result(i) );
end; i = fix(ihi+1);
writef(1,['%2i',repmat(' ',1,2),repmat('%14.8f',1,5),'\n'], norder, result([ilo:ihi]));
end; norder = fix(maxorder+1);
end; ilo = fix(nfunc+1);
return;
end
function test33(varargin)
%
%*******************************************************************************
%
%! TEST33 tests LOBATTO_SET.
%! TEST33 tests SUM_SUB.
%
%
persistent a b i ihi ilo maxfunc maxorder nfunc norder nsub result weight xhi xlo xtab ; 

if isempty(maxfunc), maxfunc = 20; end;
if isempty(maxorder), maxorder = 20; end;
%
if isempty(a), a=0; end;
if isempty(b), b=0; end;
if isempty(i), i=0; end;
if isempty(ihi), ihi=0; end;
if isempty(ilo), ilo=0; end;
if isempty(nfunc), nfunc=0; end;
if isempty(norder), norder=0; end;
if isempty(nsub), nsub=0; end;
if isempty(result), result=zeros(1,maxfunc); end;
if isempty(weight), weight=zeros(1,maxorder); end;
if isempty(xhi), xhi=0; end;
if isempty(xlo), xlo=0; end;
if isempty(xtab), xtab=zeros(1,maxorder); end;
%
[dumvar1, nfunc ]=func_set ( 'COUNT', nfunc );
a = -1.0d+00;
b =  1.0d+00;
nsub = 1;
xlo = -1.0d+00;
xhi = +1.0d+00;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TEST33');
writef(1,['%s','\n'], '  LOBATTO_SET sets up Lobatto quadrature;');
writef(1,['%s','\n'], '  SUM_SUB carries it out.');
writef(1,['%s','\n'], ' ');
writef(1,['%s',repmat('%14.6f',1,2),'\n'], '  Integration interval is ', a, b);
writef(1,['%s','%6i','\n'], '  Number of subintervals is ', nsub);
writef(1,['%s','\n'], '  Quadrature order will vary.');
writef(1,['%s','\n'], '  Integrand will vary.');
writef(1,['%s','\n'], ' ');
for ilo = 1: 5: nfunc;
ihi = fix(min( ilo + 4, nfunc ));
writef(1,['%s','\n'], ' ');
for  i =( ilo):( ihi ), writef(1,[repmat(' ',1,4),repmat('%14c',1,5),'\n'], fname(i)); end;
writef(1,['%s','\n'], ' ');

for norder = 2: maxorder;

for i = ilo: ihi;
[dumvar1, i ]=func_set ( 'SET', i );
[ norder, xtab, weight ]=lobatto_set( norder, xtab, weight );
[dumvar1, a, b, nsub, norder, xlo, xhi, xtab, weight,result(i) ]=sum_sub( @func, a, b, nsub, norder, xlo, xhi, xtab, weight,result(i) );

end; i = fix(ihi+1);
writef(1,['%2i',repmat(' ',1,2),repmat('%14.8f',1,5),'\n'], norder, result([ilo:ihi]));
end; norder = fix(maxorder+1);
end; ilo = fix(nfunc+1);

return;
end
function test34(varargin)
%
%*******************************************************************************
%
%! TEST34 tests MOULTON_SET.
%! TEST34 tests SUMMER.
%
%
persistent i ihi ilo maxfunc maxorder nfunc norder result weight xtab ; 

if isempty(maxfunc), maxfunc = 20; end;
if isempty(maxorder), maxorder = 10; end;
%
if isempty(i), i=0; end;
if isempty(ihi), ihi=0; end;
if isempty(ilo), ilo=0; end;
if isempty(nfunc), nfunc=0; end;
if isempty(norder), norder=0; end;
if isempty(result), result=zeros(1,maxfunc); end;
if isempty(weight), weight=zeros(1,maxorder); end;
if isempty(xtab), xtab=zeros(1,maxorder); end;
%
[dumvar1, nfunc ]=func_set ( 'COUNT', nfunc );
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TEST34');
writef(1,['%s','\n'], '  MOULTON_SET sets up Adams-Moulton quadrature;');
writef(1,['%s','\n'], '  SUMMER carries it out.');
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], '  Integration interval is [0,1].');
writef(1,['%s','\n'], '  Quadrature order will vary.');
writef(1,['%s','\n'], '  Integrand will vary.');
writef(1,['%s','\n'], ' ');
for ilo = 1: 5: nfunc;
ihi = fix(min( ilo + 4, nfunc ));
writef(1,['%s','\n'], ' ');
for  i =( ilo):( ihi ), writef(1,[repmat(' ',1,4),repmat('%14c',1,5),'\n'], fname(i)); end;
writef(1,['%s','\n'], ' ');
for norder = 1: maxorder;

for i = ilo: ihi;
[dumvar1, i ]=func_set ( 'SET', i );
[ norder, xtab, weight ]=moulton_set( norder, xtab, weight );

[dumvar1, norder, xtab, weight, result(i) ]=summer( @func, norder, xtab, weight, result(i) );

end; i = fix(ihi+1);
writef(1,['%2i',repmat(' ',1,2),repmat('%14.8f',1,5),'\n'], norder, result([ilo:ihi]));
end; norder = fix(maxorder+1);
end; ilo = fix(nfunc+1);
return;
end
function test35(varargin)
%
%*******************************************************************************
%
%! TEST35 tests NCC_SET.
%! TEST35 tests SUM_SUB.
%
%
persistent a b i ihi ilo maxfunc maxorder nfunc norder nsub result weight xhi xlo xtab ; 

if isempty(maxfunc), maxfunc = 20; end;
if isempty(maxorder), maxorder = 20; end;
%
if isempty(a), a=0; end;
if isempty(b), b=0; end;
if isempty(i), i=0; end;
if isempty(ihi), ihi=0; end;
if isempty(ilo), ilo=0; end;
if isempty(nfunc), nfunc=0; end;
if isempty(norder), norder=0; end;
if isempty(nsub), nsub=0; end;
if isempty(result), result=zeros(1,maxfunc); end;
if isempty(weight), weight=zeros(1,maxorder); end;
if isempty(xhi), xhi=0; end;
if isempty(xlo), xlo=0; end;
if isempty(xtab), xtab=zeros(1,maxorder); end;
%
[dumvar1, nfunc ]=func_set ( 'COUNT', nfunc );
a = 0.0d+00;
b = 1.0d+00;
nsub = 1;
xlo = -1.0d+00;
xhi = +1.0d+00;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TEST35');
writef(1,['%s','\n'], '  NCC_SET sets up a closed Newton Cotes rule;');
writef(1,['%s','\n'], '  SUM_SUB carries it out.');
writef(1,['%s','\n'], ' ');
writef(1,['%s',repmat('%14.6f',1,2),'\n'], '  Integration interval is ', a, b);
writef(1,['%s','%6i','\n'], '  Number of subintervals is ', nsub);
writef(1,['%s','\n'], '  Quadrature order will vary.');
writef(1,['%s','\n'], '  Integrand will vary.');
writef(1,['%s','\n'], ' ');
for ilo = 1: 5: nfunc;
ihi = fix(min( ilo + 4, nfunc ));
writef(1,['%s','\n'], ' ');
for  i =( ilo):(ihi ), writef(1,[repmat(' ',1,4),repmat('%14c',1,5),'\n'], fname(i)); end;
writef(1,['%s','\n'], ' ');
for norder = 2: maxorder;

for i = ilo: ihi;
[dumvar1, i ]=func_set ( 'SET', i );
[ norder, xtab, weight ]=ncc_set( norder, xtab, weight );

[dumvar1, a, b, nsub, norder, xlo, xhi, xtab, weight,result(i) ]=sum_sub( @func, a, b, nsub, norder, xlo, xhi, xtab, weight,result(i) );

end; i = fix(ihi+1);
writef(1,['%2i',repmat(' ',1,2),repmat('%14.8f',1,5),'\n'], norder, result([ilo:ihi]));
end; norder = fix(maxorder+1);

end; ilo = fix(nfunc+1);
return;
end
function test36(varargin)
%
%*******************************************************************************
%
%! TEST36 tests NCC_COM.
%! TEST36 tests SUM_SUB.
%
%
persistent a b i ihi ilo maxfunc maxorder nfunc norder nsub result weight xhi xlo xtab ; 

if isempty(maxfunc), maxfunc = 20; end;
if isempty(maxorder), maxorder = 20; end;
%
if isempty(a), a=0; end;
if isempty(b), b=0; end;
if isempty(i), i=0; end;
if isempty(ihi), ihi=0; end;
if isempty(ilo), ilo=0; end;
if isempty(nfunc), nfunc=0; end;
if isempty(norder), norder=0; end;
if isempty(nsub), nsub=0; end;
if isempty(result), result=zeros(1,maxfunc); end;
if isempty(weight), weight=zeros(1,maxorder); end;
if isempty(xhi), xhi=0; end;
if isempty(xlo), xlo=0; end;
if isempty(xtab), xtab=zeros(1,maxorder); end;
%
[dumvar1, nfunc ]=func_set ( 'COUNT', nfunc );
a = 0.0d+00;
b = 1.0d+00;
nsub = 1;
xlo = -1.0d+00;
xhi = +1.0d+00;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TEST36');
writef(1,['%s','\n'], '  NCC_COM computes a closed Newton Cotes rule;');
writef(1,['%s','\n'], '  SUM_SUB carries it out.');
writef(1,['%s','\n'], ' ');
writef(1,['%s',repmat('%14.6f',1,2),'\n'], '  Integration interval is ', a, b);
writef(1,['%s','%6i','\n'], '  Number of subintervals is ', nsub);
writef(1,['%s','\n'], '  Quadrature order will vary.');
writef(1,['%s','\n'], '  Integrand will vary.');
writef(1,['%s','\n'], ' ');
for ilo = 1: 5: nfunc;
ihi = fix(min( ilo + 4, nfunc ));
writef(1,['%s','\n'], ' ');
for  i =( ilo):( ihi ), writef(1,[repmat(' ',1,4),repmat('%14c',1,5),'\n'], fname(i)); end;
writef(1,['%s','\n'], ' ');

for norder = 2: maxorder;

for i = ilo: ihi;
[dumvar1, i ]=func_set ( 'SET', i );
[ norder, xtab, weight ]=ncc_com( norder, xtab, weight );

[dumvar1, a, b, nsub, norder, xlo, xhi, xtab, weight,result(i) ]=sum_sub( @func, a, b, nsub, norder, xlo, xhi, xtab, weight,result(i) );

end; i = fix(ihi+1);
writef(1,['%2i',repmat(' ',1,2),repmat('%14.8f',1,5),'\n'], norder, result([ilo:ihi]));
end; norder = fix(maxorder+1);
end; ilo = fix(nfunc+1);

return;
end
function test37(varargin)
%
%*******************************************************************************
%
%! TEST37 tests NCO_SET.
%! TEST37 tests SUM_SUB.
%
%
persistent a b i ihi ilo maxfunc maxorder nfunc norder nsub result weight xhi xlo xtab ; 

if isempty(maxfunc), maxfunc = 20; end;
if isempty(maxorder), maxorder = 9; end;
%
if isempty(a), a=0; end;
if isempty(b), b=0; end;
if isempty(i), i=0; end;
if isempty(ihi), ihi=0; end;
if isempty(ilo), ilo=0; end;
if isempty(nfunc), nfunc=0; end;
if isempty(norder), norder=0; end;
if isempty(nsub), nsub=0; end;
if isempty(result), result=zeros(1,maxfunc); end;
if isempty(weight), weight=zeros(1,maxorder); end;
if isempty(xhi), xhi=0; end;
if isempty(xlo), xlo=0; end;
if isempty(xtab), xtab=zeros(1,maxorder); end;
%
[dumvar1, nfunc ]=func_set ( 'COUNT', nfunc );
a = 0.0d+00;
b = 1.0d+00;
nsub = 1;
xlo = -1.0d+00;
xhi = +1.0d+00;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TEST37');
writef(1,['%s','\n'], '  NCO_SET sets up an open Newton-Cotes rule;');
writef(1,['%s','\n'], '  SUM_SUB carries it out.');
writef(1,['%s','\n'], ' ');
writef(1,['%s',repmat('%14.6f',1,2),'\n'], '  Integration interval is ', a, b);
writef(1,['%s','%6i','\n'], '  Number of subintervals is ', nsub);
writef(1,['%s','\n'], '  Quadrature order will vary.');
writef(1,['%s','\n'], '  Integrand will vary.');
writef(1,['%s','\n'], ' ');
for ilo = 1: 5: nfunc;
ihi = fix(min( ilo + 4, nfunc ));
writef(1,['%s','\n'], ' ');
for  i =( ilo):( ihi ), writef(1,[repmat(' ',1,4),repmat('%14c',1,5),'\n'], fname(i)); end;
writef(1,['%s','\n'], ' ');
for norder = 1: maxorder;

if( norder <= 7 || norder == 9 )
for i = ilo: ihi;
[dumvar1, i ]=func_set ( 'SET', i );
[ norder, xtab, weight ]=nco_set( norder, xtab, weight );

[dumvar1, a, b, nsub, norder, xlo, xhi, xtab, weight,result(i) ]=sum_sub( @func, a, b, nsub, norder, xlo, xhi, xtab, weight,result(i) );

end; i = fix(ihi+1);
writef(1,['%2i',repmat(' ',1,2),repmat('%14.8f',1,5),'\n'], norder, result([ilo:ihi]));
end;
end; norder = fix(maxorder+1);
end; ilo = fix(nfunc+1);

return;
end
function test38(varargin)
%
%*******************************************************************************
%
%! TEST38 tests NCO_COM.
%! TEST38 tests SUM_SUB.
%
%
persistent a b i ihi ilo maxfunc maxorder nfunc norder nsub result weight xhi xlo xtab ; 

if isempty(maxfunc), maxfunc = 20; end;
if isempty(maxorder), maxorder = 9; end;
%
if isempty(a), a=0; end;
if isempty(b), b=0; end;
if isempty(i), i=0; end;
if isempty(ihi), ihi=0; end;
if isempty(ilo), ilo=0; end;
if isempty(nfunc), nfunc=0; end;
if isempty(norder), norder=0; end;
if isempty(nsub), nsub=0; end;
if isempty(result), result=zeros(1,maxfunc); end;
if isempty(weight), weight=zeros(1,maxorder); end;
if isempty(xhi), xhi=0; end;
if isempty(xlo), xlo=0; end;
if isempty(xtab), xtab=zeros(1,maxorder); end;
%
[dumvar1, nfunc ]=func_set ( 'COUNT', nfunc );
a = 0.0d+00;
b = 1.0d+00;
nsub = 1;
xlo = -1.0d+00;
xhi = +1.0d+00;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TEST38');
writef(1,['%s','\n'], '  NCO_COM sets up an open Newton-Cotes rule;');
writef(1,['%s','\n'], '  SUM_SUB carries it out.');
writef(1,['%s','\n'], ' ');
writef(1,['%s',repmat('%14.6f',1,2),'\n'], '  Integration interval is ', a, b);
writef(1,['%s','%6i','\n'], '  Number of subintervals is ', nsub);
writef(1,['%s','\n'], '  Quadrature order will vary.');
writef(1,['%s','\n'], '  Integrand will vary.');
writef(1,['%s','\n'], ' ');
for ilo = 1: 5: nfunc;
ihi = fix(min( ilo + 4, nfunc ));
writef(1,['%s','\n'], ' ');
for  i =( ilo):( ihi ), writef(1,[repmat(' ',1,4),repmat('%14c',1,5),'\n'], fname(i)); end;
writef(1,['%s','\n'], ' ');
for norder = 1: maxorder;

if( norder <= 7 || norder == 9 )
for i = ilo: ihi;
[dumvar1, i ]=func_set ( 'SET', i );
[ norder, xtab, weight ]=nco_com( norder, xtab, weight );

[dumvar1, a, b, nsub, norder, xlo, xhi, xtab, weight,result(i) ]=sum_sub( @func, a, b, nsub, norder, xlo, xhi, xtab, weight,result(i) );

end; i = fix(ihi+1);
writef(1,['%2i',repmat(' ',1,2),repmat('%14.8f',1,5),'\n'], norder, result([ilo:ihi]));
end;
end; norder = fix(maxorder+1);
end; ilo = fix(nfunc+1);

return;
end
function test39(varargin)
%
%*******************************************************************************
%
%! TEST39 tests RADAU_SET.
%! TEST39 tests SUM_SUB.
%
%
persistent a b i ihi ilo maxfunc maxorder nfunc norder nsub result weight xhi xlo xtab ; 

if isempty(maxfunc), maxfunc = 20; end;
if isempty(maxorder), maxorder = 15; end;
%
if isempty(a), a=0; end;
if isempty(b), b=0; end;
if isempty(i), i=0; end;
if isempty(ihi), ihi=0; end;
if isempty(ilo), ilo=0; end;
if isempty(nfunc), nfunc=0; end;
if isempty(norder), norder=0; end;
if isempty(nsub), nsub=0; end;
if isempty(result), result=zeros(1,maxfunc); end;
if isempty(weight), weight=zeros(1,maxorder); end;
if isempty(xhi), xhi=0; end;
if isempty(xlo), xlo=0; end;
if isempty(xtab), xtab=zeros(1,maxorder); end;
%
[dumvar1, nfunc ]=func_set ( 'COUNT', nfunc );
a = 0.0d+00;
b = 1.0d+00;
nsub = 1;
xlo = -1.0d+00;
xhi = +1.0d+00;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'TEST39');
writef(1,['%s','\n'], '  RADAU_SET sets up Radau quadrature;');
writef(1,['%s','\n'], '  SUM_SUB carries it out.');
writef(1,['%s','\n'], ' ');
writef(1,['%s',repmat('%14.6f',1,2),'\n'], '  The integration interval is ', a, b);
writef(1,['%s','%6i','\n'], '  Number of subintervals is ', nsub);
writef(1,['%s','\n'], '  Quadrature order will vary.');
writef(1,['%s','\n'], '  Integrand will vary.');
writef(1,['%s','\n'], ' ');
for ilo = 1: 5: nfunc;
ihi = fix(min( ilo + 4, nfunc ));
writef(1,['%s','\n'], ' ');
for  i =( ilo):( ihi ), writef(1,[repmat(' ',1,4),repmat('%14c',1,5),'\n'], fname(i)); end;
writef(1,['%s','\n'], ' ');
for norder = 1: maxorder;
for i = ilo: ihi;
[dumvar1, i ]=func_set ( 'SET', i );
[ norder, xtab, weight ]=radau_set( norder, xtab, weight );
[dumvar1, a, b, nsub, norder, xlo, xhi, xtab, weight,result(i) ]=sum_sub( @func, a, b, nsub, norder, xlo, xhi, xtab, weight,result(i) );
end; i = fix(ihi+1);
writef(1,['%2i',repmat(' ',1,2),repmat('%14.8f',1,5),'\n'], norder, result([ilo:ihi]));
end; norder = fix(maxorder+1);

end; ilo = fix(nfunc+1);
return;
end
function [f1sd1result, x ]=f1sd1( x );
%
%*******************************************************************************
%
%! F1SD1 evaluates the function 1.0D+00/ sqrt ( 1.1 - x**2 ).
%
%
%  Parameters:
%
%    Input, doubleprecision X, the argument of the function.
%
%    Output, doubleprecision F1SD1, the value of the function.
%
%
f1sd1result=[];
persistent f1sd1 ; 

if isempty(f1sd1result), f1sd1result=0; end;
%
f1sd1result = 1.0d+00 ./ sqrt( 1.1d+00 - x.^2 );

csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',x); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [fxsd1result, x ]=fxsd1( x );
%
%*******************************************************************************
%
%! FXSD1 evaluates the function x / sqrt ( 1.1 - x**2 ).
%
%
%  Parameters:
%
%    Input, doubleprecision X, the argument of the function.
%
%    Output, doubleprecision FXSD1, the value of the function.
%
%
fxsd1result=[];
persistent fxsd1 ; 

if isempty(fxsd1result), fxsd1result=0; end;
%
fxsd1result = x ./ sqrt( 1.1d+00 - x.^2 );

csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',x); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [fx2sd1result, x ]=fx2sd1( x );
%
%*******************************************************************************
%
%! FX2SD1 evaluates the function x**2 / sqrt ( 1.1 - x**2 ).
%
%
%  Parameters:
%
%    Input, doubleprecision X, the argument of the function.
%
%    Output, doubleprecision FX2SD1, the value of the function.
%
%
fx2sd1result=[];
persistent fx2sd1 ; 

if isempty(fx2sd1result), fx2sd1result=0; end;
%
fx2sd1result = x.^2 ./ sqrt( 1.1d+00 - x.^2 );

csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',x); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [funcresult, x ]=func( x );
%
%*******************************************************************************
%
%! FUNC evaluates a function of X, as chosen by the user.
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
%    Input, doubleprecision X, the argument of the function.
%
%    Output, doubleprecision FUNC, the value of the function.
%
%
funcresult=[];
persistent func ifunc ; 

if isempty(funcresult), funcresult=0; end;
if isempty(ifunc), ifunc=0; end;
%
[dumvar1, ifunc ]=func_set ( 'GET', ifunc );
if( ifunc == 1 )
funcresult = 1.0d+00;
elseif( ifunc == 2 ) ;
funcresult = x;
elseif( ifunc == 3 ) ;
funcresult = x.^2;
elseif( ifunc == 4 ) ;
funcresult = x.^3;
elseif( ifunc == 5 ) ;
funcresult = x.^4;
elseif( ifunc == 6 ) ;
funcresult = x.^5;
elseif( ifunc == 7 ) ;
funcresult = x.^6;
elseif( ifunc == 8 ) ;
funcresult = x.^7;
elseif( ifunc == 9 ) ;
funcresult = sin( x );
elseif( ifunc == 10 ) ;
funcresult = exp( x );
elseif( ifunc == 11 ) ;
funcresult = sqrt( abs( x ) );
else;
funcresult = 0.0d+00;
end;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',x); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [action, i]=func_set( action, i );
%
%*******************************************************************************
%
%! FUNC_SET sets the function to be returned by FUNC.
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
%    Input, character ( len = * ) ACTION, the action to be carried out.
%    'COUNT' means the call is made to count the number of functions available.
%    'GET' means the call is made to find out the current function index.
%    'SET' means the call is made to set the current function index.
%
%    Input/output, integer I.
%    For 'COUNT', I is the number of functions available;
%    For 'GET', I is the currently chosen function;
%    For 'SET', I is the user's new choice for the function.
%
%
persistent ival ; 

if isempty(ival), ival = 0; end;
%
if( strcmp(deblank(action),deblank('COUNT')) )
i = 11;
elseif( strcmp(deblank(action),deblank('GET')) ) ;
i = fix(ival);
elseif( strcmp(deblank(action),deblank('SET')) ) ;
ival = fix(i);
end;
return;
end
function [fnameresult, ifunc ]=fname( ifunc );
%
%*******************************************************************************
%
%! FNAME returns the name of the function that will be evaluated in FUNC.
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
%    Output, character ( len = 10 ) FNAME, the name of the function.
%
%
fnameresult=[];
persistent fname ; 

if isempty(fnameresult), fnameresult=repmat(' ',1,10); end;
%
if( ifunc == 1 )
fnameresult = '         1';
elseif( ifunc == 2 ) ;
fnameresult = '         X';
elseif( ifunc == 3 ) ;
fnameresult = '      X**2';
elseif( ifunc == 4 ) ;
fnameresult = '      X**3';
elseif( ifunc == 5 ) ;
fnameresult = '      X**4';
elseif( ifunc == 6 ) ;
fnameresult = '      X**5';
elseif( ifunc == 7 ) ;
fnameresult = '      X**6';
elseif( ifunc == 8 ) ;
fnameresult = '      X**7';
elseif( ifunc == 9 ) ;
fnameresult = '    SIN(X)';
elseif( ifunc == 10 ) ;
fnameresult = '    EXP(X)';
elseif( ifunc == 11 ) ;
fnameresult = ' SQRT(|X|)';
else;
fnameresult = '??????';
end;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',ifunc); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [norder, xtab, weight]=bashforth_set( norder, xtab, weight );
%*******************************************************************************
%
%! BASHFORTH_SET sets abscissas and weights for Adams-Bashforth quadrature.
%
%  Definition:
%
%    Adams-Bashforth quadrature formulas are normally used in solving
%    ordinary differential equations, and are not really suitable for
%    general quadrature computations.  However, an Adams-Bashforth formula
%    is equivalent to approximating the integral of F(Y(X)) between X(M)
%    and X(M+1), using an explicit formula that relies only on known values
%    of F(Y(X)) at X(M-N+1) through X(M).  For this reason, the formulas
%    have been included here.
%
%    Suppose the unknown function is denoted by Y(X), with derivative
%    F(Y(X)), and that approximate values of the function are known at a
%    series of X values, which we write as X(1), X(2), ..., X(M).  We write
%    the value Y(X(1)) as Y(1) and so on.
%
%    Then the solution of the ODE Y'=F(X,Y) at the next point X(M+1) is
%    computed by:
%
%      Y(M+1) = Y(M) + Integral ( X(M) < X < X(M+1) ) F(Y(X)) dX
%             = Y(M) + H * Sum ( 1 <= I <= N ) W(I) * F(Y(M+1-I)) approximately.
%
%    In the documentation that follows, we replace F(Y(X)) by F(X).
%
%  Integration interval:
%
%    [ 0, 1 ].
%
%  Weight function:
%
%    1.0D+00
%
%  Integral to approximate:
%
%    Integral ( 0 <= X <= 1 ) F(X) dX.
%
%  Approximate integral:
%
%    Sum ( 1 <= I <= NORDER ) WEIGHT(I) * F ( 1 - I ),
%
%  Note:
%
%    The Adams-Bashforth formulas require equally spaced data.
%
%    Here is how the formula is applied in the case with non-unit spacing:
%
%      Integral ( A <= X <= A+H ) F(X) dX =
%      H * Sum ( 1 <= I <= NORDER ) WEIGHT(I) * F ( A - (I-1)*H ),
%      approximately.
%
%    The reference lists the second coefficient of the order 8 Adams-Bashforth
%    formula as
%      weight(2) =  -1162169.0D+00 / 120960.0D+00
%    but this should be
%      weight(2) =  -1152169.0D+00 / 120960.0D+00
%
%  Modified:
%
%    15 September 1998
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Abramowitz and Stegun,
%    Handbook of Mathematical Functions,
%    National Bureau of Standards, 1964.
%
%    Jean Lapidus and John Seinfeld,
%    Numerical Solution of Ordinary Differential Equations,
%    Academic Press, 1971.
%
%    Daniel Zwillinger, editor,
%    Standard Mathematical Tables and Formulae,
%    30th Edition,
%    CRC Press, 1996.
%
%  Parameters:
%
%    Input, integer NORDER, the order of the rule.  NORDER should be
%    between 1 and 8.
%
%    Output, real ( kind = 8 ) XTAB(NORDER), the abscissas of the rule.
%
%    Output, real ( kind = 8 ) WEIGHT(NORDER), the weights of the rule.
%    WEIGHT(1) is the weight at X = 0, WEIGHT(2) the weight at X = -1,
%    and so on.  The weights are rational, and should sum to 1.  Some
%    weights may be negative.
%
persistent d i ; 

if isempty(d), d=0; end;
if isempty(i), i=0; end;
if( norder == 1 )
weight(1) = 1.0d+00;
elseif( norder == 2 ) ;
d = 2.0d+00;
weight(1) =   3.0d+00 ./ d;
weight(2) = - 1.0d+00 ./ d;
elseif( norder == 3 ) ;
d = 12.0d+00;
weight(1) =   23.0d+00 ./ d;
weight(2) = - 16.0d+00 ./ d;
weight(3) =    5.0d+00 ./ d;
elseif( norder == 4 ) ;
d = 24.0d+00;
weight(1) =   55.0d+00 ./ d;
weight(2) = - 59.0d+00 ./ d;
weight(3) =   37.0d+00 ./ d;
weight(4) =  - 9.0d+00 ./ d;
elseif( norder == 5 ) ;
d = 720.0d+00;
weight(1) =   1901.0d+00 ./ d;
weight(2) = - 2774.0d+00 ./ d;
weight(3) =   2616.0d+00 ./ d;
weight(4) = - 1274.0d+00 ./ d;
weight(5) =    251.0d+00 ./ d;
elseif( norder == 6 ) ;
d = 1440.0d+00;
weight(1) =   4277.0d+00 ./ d;
weight(2) = - 7923.0d+00 ./ d;
weight(3) =   9982.0d+00 ./ d;
weight(4) = - 7298.0d+00 ./ d;
weight(5) =   2877.0d+00 ./ d;
weight(6) =  - 475.0d+00 ./ d;
elseif( norder == 7 ) ;
d = 60480.0d+00;
weight(1) =    198721.0d+00 ./ d;
weight(2) =  - 447288.0d+00 ./ d;
weight(3) =    705549.0d+00 ./ d;
weight(4) =  - 688256.0d+00 ./ d;
weight(5) =    407139.0d+00 ./ d;
weight(6) =  - 134472.0d+00 ./ d;
weight(7) =     19087.0d+00 ./ d;
elseif( norder == 8 ) ;
d = 120960.0d+00;
weight(1) =     434241.0d+00 ./ d;
weight(2) =  - 1152169.0d+00 ./ d;
weight(3) =    2183877.0d+00 ./ d;
weight(4) =  - 2664477.0d+00 ./ d;
weight(5) =    2102243.0d+00 ./ d;
weight(6) =  - 1041723.0d+00 ./ d;
weight(7) =     295767.0d+00 ./ d;
weight(8) =    - 36799.0d+00 ./ d;
else;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'BASHFORTH_SET - Fatal error!');
writef(1,['%s','%6i','\n'], '  Illegal value of NORDER = ', norder);
writef(1,['%s','\n'], '  Legal values are 1 through 8.');
error(['stop encountered in original fortran code  ',char(10),';']);
end;
for i = 1: norder;
xtab(i) = real( 1 - i);
end; i = fix(norder+1);
return;
end
function [norder, alpha, beta, gamma]=bdf_set( norder, alpha, beta, gamma );
%*******************************************************************************
%
%! BDF_SET sets weights for backward differentiation ODE weights.
%
%  Discussion:
%
%    GAMMA * Y(N+1) = Sum ( 1 <= I <= NORDER ) ALPHA(I) * Y(N+1-I)
%                     + dX * BETA * Y'(X(N+1),Y(N+1))
%
%    This is equivalent to the backward differentiation corrector formulas.
%
%  Modified:
%
%    30 December 1999
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, integer NORDER, the order of the rule, between 1 and 6.
%
%    Output, real ( kind = 8 ) ALPHA(NORDER), BETA, GAMMA, the weights.
%

if( norder == 1 )
beta =     1.0d+00;
gamma =    1.0d+00;
alpha(1) = 1.0d+00;
elseif( norder == 2 ) ;
beta =       2.0d+00;
gamma =      3.0d+00;
alpha(1) =   4.0d+00;
alpha(2) = - 1.0d+00;
elseif( norder == 3 ) ;
beta =       6.0d+00;
gamma =     11.0d+00;
alpha(1) =  18.0d+00;
alpha(2) = - 9.0d+00;
alpha(3) =   2.0d+00;
elseif( norder == 4 ) ;
beta =       12.0d+00;
gamma =      25.0d+00;
alpha(1) =   48.0d+00;
alpha(2) = - 36.0d+00;
alpha(3) =   16.0d+00;
alpha(4) =  - 3.0d+00;
elseif( norder == 5 ) ;
beta =        60.0d+00;
gamma =      137.0d+00;
alpha(1) =   300.0d+00;
alpha(2) = - 300.0d+00;
alpha(3) =   200.0d+00;
alpha(4) =  - 75.0d+00;
alpha(5) =    12.0d+00;
elseif( norder == 6 ) ;
beta =        60.0d+00;
gamma =      147.0d+00;
alpha(1) =   360.0d+00;
alpha(2) = - 450.0d+00;
alpha(3) =   400.0d+00;
alpha(4) = - 225.0d+00;
alpha(5) =    72.0d+00;
alpha(6) =  - 10.0d+00;
else;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'BDF_SET - Fatal error!');
writef(1,['%s','%6i','\n'], '  Illegal order requested = ', norder);
error(['stop encountered in original fortran code  ',char(10),';']);
end;
return;
end
function [norder, weight, xtab]=bdfc_set( norder, weight, xtab );
%*******************************************************************************
%
%! BDFC_SET sets weights for backward differentiation corrector quadrature.
%
%  Definition:
%
%    A backward differentiation corrector formula is defined for a set
%    of evenly spaced abscissas X(I) with X(1) = 1 and X(2) = 0.  Assuming
%    that the values of the function to be integrated are known at the
%    abscissas, the formula is written in terms of the function value at
%    X(1), and the backward differences at X(1) that approximate the
%    derivatives there.
%
%  Integration interval:
%
%    [ 0, 1 ]
%
%  Weight function:
%
%    1.0D+00
%
%  Integral to approximate:
%
%    Integral ( 0 <= X <= 1 ) F(X) dX
%
%  Approximate integral:
%
%    Sum ( 1 <= I <= NORDER ) WEIGHT(I) * BD**(I-1) F ( 1 ).
%
%    Here, 'BD**(I-1) F ( 1 )' denotes the (I-1)st backward difference
%    of F at X = 1, using a spacing of 1.  In particular,
%
%    BD**0 F(1) = F(1)
%    BD**1 F(1) = F(1) - F(0)
%    BD**2 F(1) = F(1) - 2 * F(0) + F(-1 )
%
%  Note:
%
%    The relationship between a backward difference corrector and the
%    corresponding Adams-Moulton formula may be illustrated for the
%    BDF corrector of order 4:
%
%      BD**0 F(1) - 1/2 * BD**1 F(1) - 1/12 * BD**2 F(1) - 1/24 * BDF**3 F(1)
%      =            F(1)
%        -  1/2 * ( F(1) -         F(0) )
%        - 1/12 * ( F(1) - 2     * F(0) +        F(-1) )
%        - 1/24 * ( F(1) - 3     * F(0) + 3    * F(-1) -        F(-2) )
%      =   9/24 *   F(1) + 19/24 * F(0) - 5/24 * F(-1) + 1/24 * F(-2)
%
%    which is the Adams-Moulton formula of order 4.
%
%  Modified:
%
%    28 February 2000
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Simeon Fatunla,
%    Numerical Methods for Initial Value Problems in Ordinary Differential
%      Equations,
%    Academic Press, 1988.
%
%  Parameters:
%
%    Input, integer NORDER, the order of the rule, which can be
%    any value from 1 to 19.
%
%    Output, real ( kind = 8 ) WEIGHT(NORDER), the weights of the rule.
%
%    Output, real ( kind = 8 ) XTAB(NORDER), the abscissas of the rule.
%
persistent i maxord w ; 

if isempty(maxord), maxord = 19; end;
if isempty(i), i=0; end;
if isempty(w), w=zeros(1,maxord); end;
w(1) =                 1.0d+00;
w(2) =               - 1.0d+00 ./               2.0d+00;
w(3) =               - 1.0d+00 ./              12.0d+00;
w(4) =               - 1.0d+00 ./              24.0d+00;
w(5) =              - 19.0d+00 ./             720.0d+00;
w(6) =               - 3.0d+00 ./             160.0d+00;
w(7) =             - 863.0d+00 ./           60480.0d+00;
w(8) =             - 275.0d+00 ./           24792.0d+00;
w(9) =           - 33953.0d+00 ./         3628800.0d+00;
w(10) =           - 8183.0d+00 ./         1036800.0d+00;
w(11) =        - 3250433.0d+00 ./       479001600.0d+00;
w(12) =           - 4671.0d+00 ./          788480.0d+00;
w(13) =    - 13695779093.0d+00 ./   2615348736000.0d+00;
w(14) =     - 2224234463.0d+00 ./    475517952000.0d+00;
w(15) =   - 132282840127.0d+00 ./  31384184832000.0d+00;
w(16) =     - 2639651053.0d+00 ./    689762304000.0d+00;
w(17) =  111956703448001.0d+00 ./   3201186852864.0d+00;
w(18) =         50188465.0d+00 ./     15613165568.0d+00;
w(19) = 2334028946344463.0d+00 ./ 786014494949376.0d+00;
for i = 1: min( norder, maxord );
weight(i) = w(i);
end; i = fix(min( norder, maxord )+1);
for i = 1: norder;
xtab(i) = real( 2 - i);
end; i = fix(norder+1);
return;
end
function [norder, weight, xtab]=bdfp_set( norder, weight, xtab );
%*******************************************************************************
%
%! BDFP_SET sets weights for backward differentiation predictor quadrature.
%
%  Definition:
%
%    A backward differentiation predictor formula is defined for a set
%    of evenly spaced abscissas X(I) with X(1) = 1 and X(2) = 0.  Assuming
%    that the values of the function to be integrated are known at the
%    abscissas, the formula is written in terms of the function value at
%    X(2), and the backward differences at X(2) that approximate the
%    derivatives there.  A backward differentiation predictor formula
%    is equivalent to an Adams-Bashforth formula of the same order.
%
%  Integration interval:
%
%    [ 0, 1 ]
%
%  Weight function:
%
%    1.0D+00
%
%  Integral to approximate:
%
%    Integral ( 0 <= X <= 1 ) F(X) dX
%
%  Approximate integral:
%
%    Sum ( 1 <= I <= NORDER ) WEIGHT(I) * BD**(I-1) F ( 0 ),
%
%    Here, 'BD**(I-1) F ( 0 )' denotes the (I-1)st backward difference
%    of F at X = 0, using a spacing of 1.  In particular,
%
%    BD**0 F(0) = F(0)
%    BD**1 F(0) = F(0) - F(-1)
%    BD**2 F(0) = F(0) - 2 * F(-1) + F(-2 )
%
%  Note:
%
%    The relationship between a backward difference predictor and the
%    corresponding Adams-Bashforth formula may be illustrated for the
%    BDF predictor of order 3:
%
%      BD**0 F(0) + 0.5 * BD**1 F(0) + 5/12 * BD**2 F(0)
%      =            F(0)
%        + 1/2  * ( F(0) -         F(1) )
%        + 5/12 * ( F(0) - 2     * F(-1) +      F(-2) )
%      =  23/12 *   F(0) - 16/12 * F(-1) + 5/12 F(-2)
%
%    which is the Adams-Bashforth formula of order 3.
%
%  Modified:
%
%    29 February 2000
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Simeon Fatunla,
%    Numerical Methods for Initial Value Problems in Ordinary Differential
%      Equations,
%    Academic Press, 1988.
%
%  Parameters:
%
%    Input, integer NORDER, the order of the rule, which can be
%    any value from 1 to 19.
%
%    Output, real ( kind = 8 ) WEIGHT(NORDER), the weight of the rule.
%
%    Output, real ( kind = 8 ) XTAB(NORDER), the abscissas of the rule.
%
persistent i maxord w ; 

if isempty(maxord), maxord = 19; end;
if isempty(i), i=0; end;
if isempty(w), w=zeros(1,maxord); end;
w(1) =                       1.0d+00;
w(2) =                       1.0d+00 ./                2.0d+00;
w(3) =                       5.0d+00 ./               12.0d+00;
w(4) =                       3.0d+00 ./                8.0d+00;
w(5) =                     251.0d+00 ./              720.0d+00;
w(6) =                      95.0d+00 ./              288.0d+00;
w(7) =                   19087.0d+00 ./            60480.0d+00;
w(8) =                    5257.0d+00 ./            17280.0d+00;
w(9) =                 1070017.0d+00 ./          3628800.0d+00;
w(10) =                  25713.0d+00 ./            89600.0d+00;
w(11) =               26842253.0d+00 ./         95800320.0d+00;
w(12) =                4777223.0d+00 ./         17418240.0d+00;
w(13) =           703604254357.0d+00 ./    2615348736000.0d+00;
w(14) =           106364763817.0d+00 ./     402361344000.0d+00;
w(15) =          1166309819657.0d+00 ./    4483454976000.0d+00;
w(16) =               25221445.0d+00 ./         98402304.0d+00;
w(17) =       8092989203533249.0d+00 ./    3201186852864.0d+00;
w(18) =         85455477715379.0d+00 ./      34237292544.0d+00;
w(19) =   12600467236042756559.0d+00 ./ 5109094217170944.0d+00;
for i = 1: min( norder, maxord );
weight(i) = w(i);
end; i = fix(min( norder, maxord )+1);
for i = 1: norder;
xtab(i) = real( 1 - i);
end; i = fix(norder+1);
return;
end
function [func, norder, weight, xtab, diftab, result]=bdf_sum( func, norder, weight, xtab, diftab, result );
%*******************************************************************************
%
%! BDF_SUM carries out an explicit backward difference quadrature rule for [0,1].
%
%  Integral to approximate:
%
%    Integral ( 0 <= X <= 1 ) F(X) dX
%
%  Formula:
%
%    RESULT = Sum ( 1 <= I <= NORDER ) WEIGHT(I) * BDF**(I-1) FUNC ( 0 )
%
%  Note:
%
%    The integral from 0 to 1 is approximated using data at X = 0,
%    -1, -2, ..., -NORDER+1.  This is a form of extrapolation, and
%    the approximation can become poor as NORDER increases.
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
%    Input, external FUNC, the name of the FORTRAN function which evaluates
%    the integrand.  The function must have the form
%      function func ( x ).
%
%    Input, integer NORDER, the order of the rule.
%
%    Input, real ( kind = 8 ) WEIGHT(NORDER), the weights of the rule.
%
%    Input, real ( kind = 8 ) XTAB(NORDER), the abscissas of the rule.
%
%    Workspace, real ( kind = 8 ) DIFTAB(NORDER).
%
%    Output, real ( kind = 8 ) RESULT, the approximate value of the integral.
%
persistent i j ; 

if isempty(i), i=0; end;
if isempty(j), j=0; end;
for i = 1: norder;
diftab(i) = func( xtab(i) );
end; i = fix(norder+1);
for i = 2: norder;
for j = i: norder;
diftab(norder+i-j) =( diftab(norder+i-j-1) - diftab(norder+i-j) );
end; j = fix(norder+1);
end; i = fix(norder+1);
result = dot( weight([1:norder]), diftab([1:norder]) );
return;
end
function [norder, xtab, weight]=cheb_set( norder, xtab, weight );
%*******************************************************************************
%
%! CHEB_SET sets abscissas and weights for Chebyshev quadrature.
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
%    Integral ( -1 <= X <= 1 ) F(X) dX
%
%  Approximate integral:
%
%    Sum ( 1 <= I <= NORDER ) WEIGHT(I) * F ( XTAB(I) )
%
%  Note:
%
%    The Chebyshev rule is distinguished by using equal weights.
%
%  Modified:
%
%    27 October 2000
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Abramowitz and Stegun,
%    Handbook of Mathematical Functions,
%    National Bureau of Standards, 1964.
%
%    H Engels,
%    Numerical Quadrature and Cubature,
%    Academic Press, 1980.
%
%    Zdenek Kopal,
%    Numerical Analysis,
%    John Wiley, 1955.
%
%    Daniel Zwillinger, editor,
%    Standard Mathematical Tables and Formulae,
%    30th Edition,
%    CRC Press, 1996.
%
%  Parameters:
%
%    Input, integer NORDER, the order of the rule.
%    NORDER may only have the values 1, 2, 3, 4, 5, 6, 7 or 9.
%    There are NO other Chebyshev rules with real abscissas.
%
%    Output, real ( kind = 8 ) XTAB(NORDER), the abscissas of the rule,
%    which are symmetric in [-1,1].
%
%    Output, real ( kind = 8 ) WEIGHT(NORDER), the weights of the rule,
%    which should each equal 2 / NORDER.
%
persistent i ; 

if isempty(i), i=0; end;
if( norder == 1 )
xtab(1) = 0.0d+00;
elseif( norder == 2 ) ;
xtab(1) = - 1.0d+00 ./ sqrt( 3.0d+00 );
xtab(2) =   1.0d+00 ./ sqrt( 3.0d+00 );
elseif( norder == 3 ) ;
xtab(1) = - 1.0d+00 ./ sqrt( 2.0d+00 );
xtab(2) =   0.0d+00;
xtab(3) =   1.0d+00 ./ sqrt( 2.0d+00 );
elseif( norder == 4 ) ;
xtab(1) =   - sqrt(( 1.0d+00 + 2.0d+00./ sqrt( 5.0d+00 ) ) ./ 3.0d+00 );
xtab(2) =   - sqrt(( 1.0d+00 - 2.0d+00./ sqrt( 5.0d+00 ) ) ./ 3.0d+00 );
xtab(3) =     sqrt(( 1.0d+00 - 2.0d+00./ sqrt( 5.0d+00 ) ) ./ 3.0d+00 );
xtab(4) =     sqrt(( 1.0d+00 + 2.0d+00./ sqrt( 5.0d+00 ) ) ./ 3.0d+00 );
elseif( norder == 5 ) ;
xtab(1) = - sqrt(( 5.0d+00 + sqrt( 11.0d+00) ) ./ 12.0d+00 );
xtab(2) = - sqrt(( 5.0d+00 - sqrt( 11.0d+00) ) ./ 12.0d+00 );
xtab(3) =   0.0d+00;
xtab(4) =   sqrt(( 5.0d+00 - sqrt( 11.0d+00) ) ./ 12.0d+00 );
xtab(5) =   sqrt(( 5.0d+00 + sqrt( 11.0d+00) ) ./ 12.0d+00 );
elseif( norder == 6 ) ;
xtab(1) = - 0.866246818107820591383598d+00;
xtab(2) = - 0.422518653761111529118546d+00;
xtab(3) = - 0.266635401516704720331534d+00;
xtab(4) =   0.266635401516704720331534d+00;
xtab(5) =   0.422518653761111529118546d+00;
xtab(6) =   0.866246818107820591383598d+00;
elseif( norder == 7 ) ;
xtab(1) = - 0.883861700758049035704224d+00;
xtab(2) = - 0.529656775285156811385048d+00;
xtab(3) = - 0.323911810519907637519673d+00;
xtab(4) =   0.0d+00;
xtab(5) =   0.323911810519907637519673d+00;
xtab(6) =   0.529656775285156811385048d+00;
xtab(7) =   0.883861700758049035704224d+00;
elseif( norder == 9 ) ;
xtab(1) = - 0.911589307728434473664949d+00;
xtab(2) = - 0.601018655380238071428128d+00;
xtab(3) = - 0.528761783057879993260181d+00;
xtab(4) = - 0.167906184214803943068031d+00;
xtab(5) =   0.0d+00;
xtab(6) =   0.167906184214803943068031d+00;
xtab(7) =   0.528761783057879993260181d+00;
xtab(8) =   0.601018655380238071428128d+00;
xtab(9) =   0.911589307728434473664949d+00;
else;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'CHEB_SET - Fatal error!');
writef(1,['%s','%6i','\n'], '  Illegal value of NORDER = ', norder);
writef(1,['%s','\n'], '  Legal values are 1 through 7, and 9.');
error(['stop encountered in original fortran code  ',char(10),';']);
end;
weight([1:norder]) = 2.0d+00 ./ real( norder);
return;
end
function [norder, xtab, weight]=cheb_to_set( norder, xtab, weight );
%*******************************************************************************
%
%! CHEB_TO_SET sets up open Gauss-Chebyshev (first kind) quadrature.
%
%  Integration interval:
%
%    [ -1, 1 ]
%
%  Weight function:
%
%    1 / SQRT ( 1 - X**2 )
%
%  Integral to approximate:
%
%    Integral ( -1 <= X <= 1 ) F(X) / SQRT ( 1 - X**2 ) dX
%
%  Approximate integral:
%
%    Sum ( 1 <= I <= NORDER ) WEIGHT(I) * F ( XTAB(I) )
%
%  Precision:
%
%    If NORDER points are used, then Gauss-Chebyshev quadrature
%    will compute the integral exactly, whenever F(X) is a polynomial
%    of degree 2*NORDER-1 or less.
%
%  Note:
%
%    The abscissas of the rule are zeroes of the Chebyshev polynomials
%    of the first kind, T(NORDER,X).
%
%  Modified:
%
%    15 September 1998
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Daniel Zwillinger, editor,
%    Standard Mathematical Tables and Formulae,
%    30th Edition,
%    CRC Press, 1996.
%
%  Parameters:
%
%    Input, integer NORDER, the order of the rule.
%
%    Output, real ( kind = 8 ) XTAB(NORDER), the abscissas of the rule.
%
%    Output, real ( kind = 8 ) WEIGHT(NORDER), the weights of the rule,
%    which are all equal to PI / NORDER.
%
persistent angle i ; 

if isempty(angle), angle=0; end;
if isempty(i), i=0; end;
for i = 1: norder;
angle = real( 2 .* i - 1) .* d_pi( )./ real( 2 .* norder);
xtab(i) = cos( angle );
end; i = fix(norder+1);
weight([1:norder]) = d_pi( ) ./ real( norder);
return;
end
function [norder, xtab, weight]=cheb_tc_set( norder, xtab, weight );
%*******************************************************************************
%
%! CHEB_TC_SET sets up closed Gauss-Chebyshev (first kind) quadrature.
%
%  Integration interval:
%
%    [ -1, 1 ]
%
%  Weight function:
%
%    1 / SQRT ( 1 - X**2 )
%
%  Integral to approximate:
%
%    Integral ( -1 <= X <= 1 ) F(X) / SQRT ( 1 - X**2 ) dX
%
%  Approximate integral:
%
%    Sum ( 1 <= I <= NORDER ) WEIGHT(I) * F ( XTAB(I) )
%
%  Precision:
%
%    If NORDER points are used, then Gauss-Chebyshev quadrature
%    will compute the integral exactly, whenever F(X) is a polynomial
%    of degree 2*NORDER-3 or less.
%
%  Note:
%
%    The abscissas include -1 and 1.
%
%    If the order is doubled, the abscissas of the new rule include
%    all the points of the old rule.  This fact can be used to
%    efficiently implement error estimation.
%
%  Modified:
%
%    15 September 1998
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Daniel Zwillinger, editor,
%    Standard Mathematical Tables and Formulae,
%    30th Edition,
%    CRC Press, 1996.
%
%  Parameters:
%
%    Input, integer NORDER, the order of the rule, which must be at least 2.
%
%    Output, real ( kind = 8 ) XTAB(NORDER), the abscissas of the rule.
%
%    Output, real ( kind = 8 ) WEIGHT(NORDER), the weights of the rule.
%    The first and last weights are 0.5 * PI / ( NORDER - 1),
%    and all other weights are PI / ( NORDER - 1 ).
%
persistent angle i ; 

if isempty(angle), angle=0; end;
if isempty(i), i=0; end;
if( norder < 2 )
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'CHEB_TC_SET - Fatal error!');
writef(1,['%s','\n'], '  NORDER must be at least 2.');
writef(1,['%s','%6i','\n'], '  The input value was NORDER = ', norder);
error(['stop encountered in original fortran code  ',char(10),';']);
end;
for i = 1: norder;
angle = real(      i - 1) .* d_pi( )./ real( norder - 1);
xtab(i) = cos( angle );
end; i = fix(norder+1);
weight(1) = d_pi( ) ./ real( 2 .*( norder - 1 ));
weight([2:norder-1]) = d_pi( ) ./ real( norder - 1);
weight(norder) = d_pi( ) ./ real( 2 .*( norder - 1 ));
return;
end
function [norder, xtab, weight]=cheb_u_set( norder, xtab, weight );
%*******************************************************************************
%
%! CHEB_U_SET sets abscissas and weights for Gauss-Chebyshev quadrature.
%
%  Integration interval:
%
%    [ -1, 1 ]
%
%  Weight function:
%
%    SQRT ( 1 - X**2 )
%
%  Integral to approximate:
%
%    Integral ( -1 <= X <= 1 ) SQRT ( 1 - X**2 ) * F(X) dX
%
%  Approximate integral:
%
%    Sum ( 1 <= I <= NORDER ) WEIGHT(I) * F ( XTAB(I) )
%
%  Precision:
%
%    If NORDER points are used, then Gauss-Chebyshev quadrature
%    will compute the integral exactly, whenever F(X) is a polynomial
%    of degree 2*NORDER-1 or less.
%
%  Note:
%
%    The abscissas are zeroes of the Chebyshev polynomials
%    of the second kind, U(NORDER,X).
%
%  Modified:
%
%    15 September 1998
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Daniel Zwillinger, editor,
%    Standard Mathematical Tables and Formulae,
%    30th Edition,
%    CRC Press, 1996.
%
%  Parameters:
%
%    Input, integer NORDER, the order of the rule.
%
%    Output, real ( kind = 8 ) XTAB(NORDER), the abscissas of the rule.
%
%    Output, real ( kind = 8 ) WEIGHT(NORDER), the weights of the rule,
%    which are all equal to PI / NORDER.
%
persistent angle i ; 

if isempty(angle), angle=0; end;
if isempty(i), i=0; end;
for i = 1: norder;
angle = real( i) .* d_pi( ) ./ real( norder + 1);
xtab(i) = cos( angle );
weight(i) = d_pi( ) .*( sin( angle ) ).^2 ./ real( norder + 1);
end; i = fix(norder+1);
return;
end
function [x, y]=d_swap( x, y );
%*******************************************************************************
%
%! D_SWAP switches two doubleprecision values.
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
%    Input/output, real ( kind = 8 ) X, Y.  On output, the values of X and
%    Y have been interchanged.
%
persistent z ; 

if isempty(z), z=0; end;
z = x;
x = y;
y = z;
return;
end
function [d_piresult]=d_pi( );
%*******************************************************************************
%
%! D_PI returns the value of pi as a doubleprecision quantity.
%
%  Modified:
%
%    28 April 2000
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Output, real ( kind = 8 ) D_PI, the value of pi.
%
d_piresult=[];
persistent d_pi ; 

if isempty(d_piresult), d_piresult=0; end;
d_piresult = 3.141592653589793d+00;
return;
end
function [n, a]=dvec_reverse( n, a );
%*******************************************************************************
%
%! DVEC_REVERSE reverses the elements of a doubleprecision vector.
%
%  Example:
%
%    Input:
%
%      N = 5, A = ( 11.0, 12.0, 13.0, 14.0, 15.0D+00 )
%
%    Output:
%
%      A = ( 15.0, 14.0, 13.0, 12.0, 11.0D+00 )
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
%    Input, integer N, the number of entries in the array.
%
%    Input/output, real ( kind = 8 ) A(N), the array to be reversed.
%
persistent i ; 

if isempty(i), i=0; end;
for i = 1: fix(n./2);
[ a(i), a(n+1-i) ]=d_swap( a(i), a(n+1-i) );
end; i = fix(fix(n./2)+1);
return;
end
function [gammaresult, x ]=gamma( x );
%*******************************************************************************
%
%! GAMMA computes the gamma function using Hastings's approximation.
%
%  Modified:
%
%    19 September 1998
%
%  Reference:
%
%    Arthur Stroud and Don Secrest,
%    Gaussian Quadrature Formulas,
%    Prentice Hall, 1966.
%
%  Parameters:
%
%    Input, real ( kind = 8 ) X, the argument at which the gamma function
%    is to be evaluated.  X must be greater than 0, and less than 70.
%
%    Output, real ( kind = 8 ) GAMMA, the gamma function at X.
%
% gam= @( y ) (((((((0.035868343d+00   * y- 0.193527818d+00 ) * y+ 0.482199394d+00 ) * y- 0.756704078d+00 ) * y+ 0.918206857d+00 ) * y- 0.897056937d+00 ) * y+ 0.988205891d+00 ) * y- 0.577191652d+00 ) * y + 1.0d+00;real( 8 ) :: gam;
gammaresult=[];
persistent gamma y z za ; 

if isempty(gammaresult), gammaresult=0; end;
if isempty(y), y=0; end;
if isempty(z), z=0; end;
if isempty(za), za=0; end;
gam= @( y ) (((((((0.035868343d+00   .* y- 0.193527818d+00 ) .* y+ 0.482199394d+00 ) .* y- 0.756704078d+00 ) .* y+ 0.918206857d+00 ) .* y- 0.897056937d+00 ) .* y+ 0.988205891d+00 ) .* y- 0.577191652d+00 ) .* y + 1.0d+00;
if( x <= 0.0d+00 )
gammaresult = 0.0d+00;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'GAMMA - Fatal error!');
writef(1,['%s','\n'], '  Input argument X <= 0.');
error(['stop encountered in original fortran code  ',char(10),';']);
end;
if( 70.0d+00 <= x )
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'GAMMA - Fatal error!');
writef(1,['%s','\n'], '  Input argument 70 <= X.');
error(['stop encountered in original fortran code  ',char(10),';']);
end;
if( x == 1.0d+00 )
gammaresult = 1.0d+00;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',x); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end;
if( x <= 1.0d+00 )
gam= @( y ) (((((((0.035868343d+00   .* y- 0.193527818d+00 ) .* y+ 0.482199394d+00 ) .* y- 0.756704078d+00 ) .* y+ 0.918206857d+00 ) .* y- 0.897056937d+00 ) .* y+ 0.988205891d+00 ) .* y- 0.577191652d+00 ) .* y + 1.0d+00;
gammaresult = gam( x ) ./ x;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',x); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end;
z = x;
za = 1.0d+00;
while (1);
z = z - 1.0d+00;
if( z < 1.0d+00 )
gam= @( y ) (((((((0.035868343d+00   .* y- 0.193527818d+00 ) .* y+ 0.482199394d+00 ) .* y- 0.756704078d+00 ) .* y+ 0.918206857d+00 ) .* y- 0.897056937d+00 ) .* y+ 0.988205891d+00 ) .* y- 0.577191652d+00 ) .* y + 1.0d+00;
gammaresult = za .* gam( z );
break;
elseif( z == 1.0d+00 ) ;
gammaresult = za;
break;
end;
za = za .* z;
end;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',x); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [norder, xtab, weight]=hermite_com( norder, xtab, weight );
%*******************************************************************************
%
%! HERMITE_COM computes the abscissa and weights for Gauss-Hermite quadrature.
%
%  Discussion:
%
%    The abscissas are the zeros of the N-th order Hermite polynomial.
%
%  Integration interval:
%
%    ( -Infinity, +Infinity )
%
%  Weight function:
%
%    exp ( - X**2 )
%
%  Integral to approximate:
%
%    Integral ( -INFINITY < X < +INFINITY ) exp ( - X**2 ) * F(X) dX
%
%  Approximate integral:
%
%    Sum ( 1 <= I <= NORDER ) WEIGHT(I) * F ( XTAB(I) )
%
%  Modified:
%
%    19 September 1998
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Arthur Stroud and Don Secrest,
%    Gaussian Quadrature Formulas,
%    Prentice Hall, 1966.
%
%  Parameters:
%
%    Input, integer NORDER, the order of the formula to be computed.
%
%    Output, real ( kind = 8 ) XTAB(NORDER), the Gauss-Hermite abscissas.
%
%    Output, real ( kind = 8 ) WEIGHT(NORDER), the Gauss-Hermite weights.
%
persistent cc dp2 i p1 s temp x ; 

if isempty(cc), cc=0; end;
if isempty(dp2), dp2=0; end;
if isempty(i), i=0; end;
if isempty(p1), p1=0; end;
if isempty(s), s=0; end;
if isempty(temp), temp=0; end;
if isempty(x), x=0; end;
cc = 1.7724538509d+00 .* gamma( real( norder) )./( 2.0d+00.^( norder - 1 ) );
s =( 2.0d+00 .* real( norder) + 1.0d+00 ).^( 1.0d+00 ./ 6.0d+00 );
for i = 1:fix(( norder + 1 ) ./ 2);
if( i == 1 )
x = s.^3 - 1.85575d+00 ./ s;
elseif( i == 2 ) ;
x = x - 1.14d+00 .*(( real( norder) ).^0.426d+00 ) ./ x;
elseif( i == 3 ) ;
x = 1.86d+00 .* x - 0.86d+00 .* xtab(1);
elseif( i == 4 ) ;
x = 1.91d+00 .* x - 0.91d+00 .* xtab(2);
else;
x = 2.0d+00 .* x - xtab(i-2);
end;
[ x, norder, dp2, p1 ]=hermite_root( x, norder, dp2, p1 );
xtab(i) = x;
weight(i) =( cc ./ dp2 ) ./ p1;
xtab(norder-i+1) = - x;
weight(norder-i+1) = weight(i);
end; i =fix(fix(( norder + 1 ) ./ 2)+1);
%
%  Reverse the order of the XTAB values.
%
[ norder, xtab ]=dvec_reverse( norder, xtab );
return;
end
function [p2, dp2, p1, x, norder]=hermite_recur( p2, dp2, p1, x, norder );
%*******************************************************************************
%
%! HERMITE_RECUR finds the value and derivative of a Hermite polynomial.
%
%  Modified:
%
%    19 September 1998
%
%  Reference:
%
%    Arthur Stroud and Don Secrest,
%    Gaussian Quadrature Formulas,
%    Prentice Hall, 1966.
%
%  Parameters:
%
%    Output, real ( kind = 8 ) P2, the value of H(NORDER,X).
%
%    Output, real ( kind = 8 ) DP2, the value of H'(NORDER,X).
%
%    Output, real ( kind = 8 ) P1, the value of H(NORDER-1,X).
%
%    Input, real ( kind = 8 ) X, the point at which polynomials are evaluated.
%
%    Input, integer NORDER, the order of the polynomial to be computed.
%
persistent dp0 dp1 i p0 ; 

if isempty(i), i=0; end;
if isempty(dp0), dp0=0; end;
if isempty(dp1), dp1=0; end;
if isempty(p0), p0=0; end;
p1 = 1.0d+00;
dp1 = 0.0d+00;
p2 = x;
dp2 = 1.0d+00;
for i = 2: norder;
p0 = p1;
dp0 = dp1;
p1 = p2;
dp1 = dp2;
p2  = x .* p1 - 0.5d+00 .*( real( i) - 1.0d+00 ) .* p0;
dp2 = x .* dp1 + p1 - 0.5d+00 .*( real( i) - 1.0d+00 ) .* dp0;
end; i = fix(norder+1);
return;
end
function [x, norder, dp2, p1]=hermite_root( x, norder, dp2, p1 );
%*******************************************************************************
%
%! HERMITE_ROOT improves an approximate root of a Hermite polynomial.
%
%  Modified:
%
%    19 September 1998
%
%  Reference:
%
%    Arthur Stroud and Don Secrest,
%    Gaussian Quadrature Formulas,
%    Prentice Hall, 1966.
%
%  Parameters:
%
%    Input/output, real ( kind = 8 ) X, the approximate root, which
%    should be improved on output.
%
%    Input, integer NORDER, the order of the Hermite polynomial.
%
%    Output, real ( kind = 8 ) DP2, the value of H'(NORDER,X).
%
%    Output, real ( kind = 8 ) P1, the value of H(NORDER-1,X).
%
persistent d eps i maxstep p2 ; 

if isempty(d), d=0; end;
if isempty(eps), eps = 1.0d-12; end;
if isempty(i), i=0; end;
if isempty(maxstep), maxstep = 10; end;
if isempty(p2), p2=0; end;
for i = 1: maxstep;
[ p2, dp2, p1, x, norder ]=hermite_recur( p2, dp2, p1, x, norder );
d = p2 ./ dp2;
x = x - d;
if( abs( d ) <= eps .*( abs( x ) + 1.0d+00 ) )
return;
end;
end; i = fix(maxstep+1);
return;
end
function [norder, xtab, weight]=hermite_set( norder, xtab, weight );
%*******************************************************************************
%
%! HERMITE_SET sets abscissas and weights for Hermite quadrature.
%
%  Integration interval:
%
%    ( -Infinity, +Infinity )
%
%  Weight function:
%
%    exp ( - X**2 )
%
%  Integral to approximate:
%
%    Integral ( -INFINITY < X < +INFINITY ) exp ( - X**2 ) * F(X) dX
%
%  Approximate integral:
%
%    Sum ( 1 <= I <= NORDER ) WEIGHT(I) * F ( XTAB(I) ).
%
%  Modified:
%
%    06 December 2000
%
%  Author:
%
%    John Burkardt
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
%  Parameters:
%
%    Input, integer NORDER, the order of the rule.
%    NORDER must be between 1 and 20, or one of the values
%    30, 32, 40, 50, 60 or 64.
%
%    Output, real ( kind = 8 ) XTAB(NORDER), the abscissas of the rule,
%    which are symmetrically placed around 0.
%
%    Output, real ( kind = 8 ) WEIGHT(NORDER), the weights of the rule.
%    The weights are positive and symmetric, and should sum
%    to SQRT(PI).
%

if( norder == 1 )
xtab(1) = 0.0d+00;
weight(1) = sqrt( d_pi( ) );
elseif( norder == 2 );
xtab(1) = - 0.707106781186547524400844362105d+00;
xtab(2) =   0.707106781186547524400844362105d+00;
weight(1) = 0.886226925452758013649083741671d+00;
weight(2) = 0.886226925452758013649083741671d+00;
elseif( norder == 3 ) ;
xtab(1) = - 0.122474487139158904909864203735d+01;
xtab(2) =   0.0d+00;
xtab(3) =   0.122474487139158904909864203735d+01;
weight(1) = 0.295408975150919337883027913890d+00;
weight(2) = 0.118163590060367735153211165556d+01;
weight(3) = 0.295408975150919337883027913890d+00;
elseif( norder == 4 ) ;
xtab(1) = - 0.165068012388578455588334111112d+01;
xtab(2) = - 0.524647623275290317884060253835d+00;
xtab(3) =   0.524647623275290317884060253835d+00;
xtab(4) =   0.165068012388578455588334111112d+01;
weight(1) = 0.813128354472451771430345571899d-01;
weight(2) = 0.804914090005512836506049184481d+00;
weight(3) = 0.804914090005512836506049184481d+00;
weight(4) = 0.813128354472451771430345571899d-01;
elseif( norder == 5 ) ;
xtab(1) = - 0.202018287045608563292872408814d+01;
xtab(2) = - 0.958572464613818507112770593893d+00;
xtab(3) =   0.0d+00;
xtab(4) =   0.958572464613818507112770593893d+00;
xtab(5) =   0.202018287045608563292872408814d+01;
weight(1) = 0.199532420590459132077434585942d-01;
weight(2) = 0.393619323152241159828495620852d+00;
weight(3) = 0.945308720482941881225689324449d+00;
weight(4) = 0.393619323152241159828495620852d+00;
weight(5) = 0.199532420590459132077434585942d-01;
elseif( norder == 6 ) ;
xtab(1) = - 0.235060497367449222283392198706d+01;
xtab(2) = - 0.133584907401369694971489528297d+01;
xtab(3) = - 0.436077411927616508679215948251d+00;
xtab(4) =   0.436077411927616508679215948251d+00;
xtab(5) =   0.133584907401369694971489528297d+01;
xtab(6) =   0.235060497367449222283392198706d+01;
weight(1) = 0.453000990550884564085747256463d-02;
weight(2) = 0.157067320322856643916311563508d+00;
weight(3) = 0.724629595224392524091914705598d+00;
weight(4) = 0.724629595224392524091914705598d+00;
weight(5) = 0.157067320322856643916311563508d+00;
weight(6) = 0.453000990550884564085747256463d-02;
elseif( norder == 7 ) ;
xtab(1) = - 0.265196135683523349244708200652d+01;
xtab(2) = - 0.167355162876747144503180139830d+01;
xtab(3) = - 0.816287882858964663038710959027d+00;
xtab(4) =   0.0d+00;
xtab(5) =   0.816287882858964663038710959027d+00;
xtab(6) =   0.167355162876747144503180139830d+01;
xtab(7) =   0.265196135683523349244708200652d+01;
weight(1) = 0.971781245099519154149424255939d-03;
weight(2) = 0.545155828191270305921785688417d-01;
weight(3) = 0.425607252610127800520317466666d+00;
weight(4) = 0.810264617556807326764876563813d+00;
weight(5) = 0.425607252610127800520317466666d+00;
weight(6) = 0.545155828191270305921785688417d-01;
weight(7) = 0.971781245099519154149424255939d-03;
elseif( norder == 8 ) ;
xtab(1) = - 0.293063742025724401922350270524d+01;
xtab(2) = - 0.198165675669584292585463063977d+01;
xtab(3) = - 0.115719371244678019472076577906d+01;
xtab(4) = - 0.381186990207322116854718885584d+00;
xtab(5) =   0.381186990207322116854718885584d+00;
xtab(6) =   0.115719371244678019472076577906d+01;
xtab(7) =   0.198165675669584292585463063977d+01;
xtab(8) =   0.293063742025724401922350270524d+01;
weight(1) = 0.199604072211367619206090452544d-03;
weight(2) = 0.170779830074134754562030564364d-01;
weight(3) = 0.207802325814891879543258620286d+00;
weight(4) = 0.661147012558241291030415974496d+00;
weight(5) = 0.661147012558241291030415974496d+00;
weight(6) = 0.207802325814891879543258620286d+00;
weight(7) = 0.170779830074134754562030564364d-01;
weight(8) = 0.199604072211367619206090452544d-03;
elseif( norder == 9 ) ;
xtab(1) = - 0.319099320178152760723004779538d+01;
xtab(2) = - 0.226658058453184311180209693284d+01;
xtab(3) = - 0.146855328921666793166701573925d+01;
xtab(4) = - 0.723551018752837573322639864579d+00;
xtab(5) =   0.0d+00;
xtab(6) =   0.723551018752837573322639864579d+00;
xtab(7) =   0.146855328921666793166701573925d+01;
xtab(8) =   0.226658058453184311180209693284d+01;
xtab(9) =   0.319099320178152760723004779538d+01;
weight(1) = 0.396069772632643819045862946425d-04;
weight(2) = 0.494362427553694721722456597763d-02;
weight(3) = 0.884745273943765732879751147476d-01;
weight(4) = 0.432651559002555750199812112956d+00;
weight(5) = 0.720235215606050957124334723389d+00;
weight(6) = 0.432651559002555750199812112956d+00;
weight(7) = 0.884745273943765732879751147476d-01;
weight(8) = 0.494362427553694721722456597763d-02;
weight(9) = 0.396069772632643819045862946425d-04;
elseif( norder == 10 ) ;
xtab(1) =  - 0.343615911883773760332672549432d+01;
xtab(2) =  - 0.253273167423278979640896079775d+01;
xtab(3) =  - 0.175668364929988177345140122011d+01;
xtab(4) =  - 0.103661082978951365417749191676d+01;
xtab(5) =  - 0.342901327223704608789165025557d+00;
xtab(6) =    0.342901327223704608789165025557d+00;
xtab(7) =    0.103661082978951365417749191676d+01;
xtab(8) =    0.175668364929988177345140122011d+01;
xtab(9) =    0.253273167423278979640896079775d+01;
xtab(10) =   0.343615911883773760332672549432d+01;
weight(1) =  0.764043285523262062915936785960d-05;
weight(2) =  0.134364574678123269220156558585d-02;
weight(3) =  0.338743944554810631361647312776d-01;
weight(4) =  0.240138611082314686416523295006d+00;
weight(5) =  0.610862633735325798783564990433d+00;
weight(6) =  0.610862633735325798783564990433d+00;
weight(7) =  0.240138611082314686416523295006d+00;
weight(8) =  0.338743944554810631361647312776d-01;
weight(9) =  0.134364574678123269220156558585d-02;
weight(10) = 0.764043285523262062915936785960d-05;
elseif( norder == 11 ) ;
xtab(1) =  - 0.366847084655958251845837146485d+01;
xtab(2) =  - 0.278329009978165177083671870152d+01;
xtab(3) =  - 0.202594801582575533516591283121d+01;
xtab(4) =  - 0.132655708449493285594973473558d+01;
xtab(5) =  - 0.656809566882099765024611575383d+00;
xtab(6) =    0.0d+00;
xtab(7) =    0.656809566882099765024611575383d+00;
xtab(8) =    0.132655708449493285594973473558d+01;
xtab(9) =    0.202594801582575533516591283121d+01;
xtab(10) =   0.278329009978165177083671870152d+01;
xtab(11) =   0.366847084655958251845837146485d+01;
weight(1) =  0.143956039371425822033088366032d-05;
weight(2) =  0.346819466323345510643413772940d-03;
weight(3) =  0.119113954449115324503874202916d-01;
weight(4) =  0.117227875167708503381788649308d+00;
weight(5) =  0.429359752356125028446073598601d+00;
weight(6) =  0.654759286914591779203940657627d+00;
weight(7) =  0.429359752356125028446073598601d+00;
weight(8) =  0.117227875167708503381788649308d+00;
weight(9) =  0.119113954449115324503874202916d-01;
weight(10) = 0.346819466323345510643413772940d-03;
weight(11) = 0.143956039371425822033088366032d-05;
elseif( norder == 12 ) ;
xtab(1) =  - 0.388972489786978191927164274724d+01;
xtab(2) =  - 0.302063702512088977171067937518d+01;
xtab(3) =  - 0.227950708050105990018772856942d+01;
xtab(4) =  - 0.159768263515260479670966277090d+01;
xtab(5) =  - 0.947788391240163743704578131060d+00;
xtab(6) =  - 0.314240376254359111276611634095d+00;
xtab(7) =    0.314240376254359111276611634095d+00;
xtab(8) =    0.947788391240163743704578131060d+00;
xtab(9) =    0.159768263515260479670966277090d+01;
xtab(10) =   0.227950708050105990018772856942d+01;
xtab(11) =   0.302063702512088977171067937518d+01;
xtab(12) =   0.388972489786978191927164274724d+01;
weight(1) =  0.265855168435630160602311400877d-06;
weight(2) =  0.857368704358785865456906323153d-04;
weight(3) =  0.390539058462906185999438432620d-02;
weight(4) =  0.516079856158839299918734423606d-01;
weight(5) =  0.260492310264161129233396139765d+00;
weight(6) =  0.570135236262479578347113482275d+00;
weight(7) =  0.570135236262479578347113482275d+00;
weight(8) =  0.260492310264161129233396139765d+00;
weight(9) =  0.516079856158839299918734423606d-01;
weight(10) = 0.390539058462906185999438432620d-02;
weight(11) = 0.857368704358785865456906323153d-04;
weight(12) = 0.265855168435630160602311400877d-06;
elseif( norder == 13 ) ;
xtab(1) =  - 0.410133759617863964117891508007d+01;
xtab(2) =  - 0.324660897837240998812205115236d+01;
xtab(3) =  - 0.251973568567823788343040913628d+01;
xtab(4) =  - 0.185310765160151214200350644316d+01;
xtab(5) =  - 0.122005503659074842622205526637d+01;
xtab(6) =  - 0.605763879171060113080537108602d+00;
xtab(7) =    0.0d+00;
xtab(8) =    0.605763879171060113080537108602d+00;
xtab(9) =    0.122005503659074842622205526637d+01;
xtab(10) =   0.185310765160151214200350644316d+01;
xtab(11) =   0.251973568567823788343040913628d+01;
xtab(12) =   0.324660897837240998812205115236d+01;
xtab(13) =   0.410133759617863964117891508007d+01;
weight(1) =  0.482573185007313108834997332342d-07;
weight(2) =  0.204303604027070731248669432937d-04;
weight(3) =  0.120745999271938594730924899224d-02;
weight(4) =  0.208627752961699392166033805050d-01;
weight(5) =  0.140323320687023437762792268873d+00;
weight(6) =  0.421616296898543221746893558568d+00;
weight(7) =  0.604393187921161642342099068579d+00;
weight(8) =  0.421616296898543221746893558568d+00;
weight(9) =  0.140323320687023437762792268873d+00;
weight(10) = 0.208627752961699392166033805050d-01;
weight(11) = 0.120745999271938594730924899224d-02;
weight(12) = 0.204303604027070731248669432937d-04;
weight(13) = 0.482573185007313108834997332342d-07;
elseif( norder == 14 ) ;
xtab(1) =  - 0.430444857047363181262129810037d+01;
xtab(2) =  - 0.346265693360227055020891736115d+01;
xtab(3) =  - 0.274847072498540256862499852415d+01;
xtab(4) =  - 0.209518325850771681573497272630d+01;
xtab(5) =  - 0.147668273114114087058350654421d+01;
xtab(6) =  - 0.878713787329399416114679311861d+00;
xtab(7) =  - 0.291745510672562078446113075799d+00;
xtab(8) =    0.291745510672562078446113075799d+00;
xtab(9) =    0.878713787329399416114679311861d+00;
xtab(10) =   0.147668273114114087058350654421d+01;
xtab(11) =   0.209518325850771681573497272630d+01;
xtab(12) =   0.274847072498540256862499852415d+01;
xtab(13) =   0.346265693360227055020891736115d+01;
xtab(14) =   0.430444857047363181262129810037d+01;
weight(1) =  0.862859116812515794532041783429d-08;
weight(2) =  0.471648435501891674887688950105d-05;
weight(3) =  0.355092613551923610483661076691d-03;
weight(4) =  0.785005472645794431048644334608d-02;
weight(5) =  0.685055342234652055387163312367d-01;
weight(6) =  0.273105609064246603352569187026d+00;
weight(7) =  0.536405909712090149794921296776d+00;
weight(8) =  0.536405909712090149794921296776d+00;
weight(9) =  0.273105609064246603352569187026d+00;
weight(10) = 0.685055342234652055387163312367d-01;
weight(11) = 0.785005472645794431048644334608d-02;
weight(12) = 0.355092613551923610483661076691d-03;
weight(13) = 0.471648435501891674887688950105d-05;
weight(14) = 0.862859116812515794532041783429d-08;
elseif( norder == 15 ) ;
xtab(1) =  - 0.449999070730939155366438053053d+01;
xtab(2) =  - 0.366995037340445253472922383312d+01;
xtab(3) =  - 0.296716692790560324848896036355d+01;
xtab(4) =  - 0.232573248617385774545404479449d+01;
xtab(5) =  - 0.171999257518648893241583152515d+01;
xtab(6) =  - 0.113611558521092066631913490556d+01;
xtab(7) =  - 0.565069583255575748526020337198d+00;
xtab(8) =    0.0d+00;
xtab(9) =    0.565069583255575748526020337198d+00;
xtab(10) =   0.113611558521092066631913490556d+01;
xtab(11) =   0.171999257518648893241583152515d+01;
xtab(12) =   0.232573248617385774545404479449d+01;
xtab(13) =   0.296716692790560324848896036355d+01;
xtab(14) =   0.366995037340445253472922383312d+01;
xtab(15) =   0.449999070730939155366438053053d+01;
weight(1) =  0.152247580425351702016062666965d-08;
weight(2) =  0.105911554771106663577520791055d-05;
weight(3) =  0.100004441232499868127296736177d-03;
weight(4) =  0.277806884291277589607887049229d-02;
weight(5) =  0.307800338725460822286814158758d-01;
weight(6) =  0.158488915795935746883839384960d+00;
weight(7) =  0.412028687498898627025891079568d+00;
weight(8) =  0.564100308726417532852625797340d+00;
weight(9) =  0.412028687498898627025891079568d+00;
weight(10) = 0.158488915795935746883839384960d+00;
weight(11) = 0.307800338725460822286814158758d-01;
weight(12) = 0.277806884291277589607887049229d-02;
weight(13) = 0.100004441232499868127296736177d-03;
weight(14) = 0.105911554771106663577520791055d-05;
weight(15) = 0.152247580425351702016062666965d-08;
elseif( norder == 16 ) ;
xtab(1) =  - 0.468873893930581836468849864875d+01;
xtab(2) =  - 0.386944790486012269871942409801d+01;
xtab(3) =  - 0.317699916197995602681399455926d+01;
xtab(4) =  - 0.254620215784748136215932870545d+01;
xtab(5) =  - 0.195178799091625397743465541496d+01;
xtab(6) =  - 0.138025853919888079637208966969d+01;
xtab(7) =  - 0.822951449144655892582454496734d+00;
xtab(8) =  - 0.273481046138152452158280401965d+00;
xtab(9) =    0.273481046138152452158280401965d+00;
xtab(10) =   0.822951449144655892582454496734d+00;
xtab(11) =   0.138025853919888079637208966969d+01;
xtab(12) =   0.195178799091625397743465541496d+01;
xtab(13) =   0.254620215784748136215932870545d+01;
xtab(14) =   0.317699916197995602681399455926d+01;
xtab(15) =   0.386944790486012269871942409801d+01;
xtab(16) =   0.468873893930581836468849864875d+01;
weight(1) =  0.265480747401118224470926366050d-09;
weight(2) =  0.232098084486521065338749423185d-06;
weight(3) =  0.271186009253788151201891432244d-04;
weight(4) =  0.932284008624180529914277305537d-03;
weight(5) =  0.128803115355099736834642999312d-01;
weight(6) =  0.838100413989858294154207349001d-01;
weight(7) =  0.280647458528533675369463335380d+00;
weight(8) =  0.507929479016613741913517341791d+00;
weight(9) =  0.507929479016613741913517341791d+00;
weight(10) = 0.280647458528533675369463335380d+00;
weight(11) = 0.838100413989858294154207349001d-01;
weight(12) = 0.128803115355099736834642999312d-01;
weight(13) = 0.932284008624180529914277305537d-03;
weight(14) = 0.271186009253788151201891432244d-04;
weight(15) = 0.232098084486521065338749423185d-06;
weight(16) = 0.265480747401118224470926366050d-09;
elseif( norder == 17 ) ;
xtab(1) =  - 0.487134519367440308834927655662d+01;
xtab(2) =  - 0.406194667587547430689245559698d+01;
xtab(3) =  - 0.337893209114149408338327069289d+01;
xtab(4) =  - 0.275776291570388873092640349574d+01;
xtab(5) =  - 0.217350282666662081927537907149d+01;
xtab(6) =  - 0.161292431422123133311288254454d+01;
xtab(7) =  - 0.106764872574345055363045773799d+01;
xtab(8) =  - 0.531633001342654731349086553718d+00;
xtab(9) =    0.0d+00;
xtab(10) =   0.531633001342654731349086553718d+00;
xtab(11) =   0.106764872574345055363045773799d+01;
xtab(12) =   0.161292431422123133311288254454d+01;
xtab(13) =   0.217350282666662081927537907149d+01;
xtab(14) =   0.275776291570388873092640349574d+01;
xtab(15) =   0.337893209114149408338327069289d+01;
xtab(16) =   0.406194667587547430689245559698d+01;
xtab(17) =   0.487134519367440308834927655662d+01;
weight(1) =  0.458057893079863330580889281222d-10;
weight(2) =  0.497707898163079405227863353715d-07;
weight(3) =  0.711228914002130958353327376218d-05;
weight(4) =  0.298643286697753041151336643059d-03;
weight(5) =  0.506734995762753791170069495879d-02;
weight(6) =  0.409200341495762798094994877854d-01;
weight(7) =  0.172648297670097079217645196219d+00;
weight(8) =  0.401826469470411956577635085257d+00;
weight(9) =  0.530917937624863560331883103379d+00;
weight(10) = 0.401826469470411956577635085257d+00;
weight(11) = 0.172648297670097079217645196219d+00;
weight(12) = 0.409200341495762798094994877854d-01;
weight(13) = 0.506734995762753791170069495879d-02;
weight(14) = 0.298643286697753041151336643059d-03;
weight(15) = 0.711228914002130958353327376218d-05;
weight(16) = 0.497707898163079405227863353715d-07;
weight(17) = 0.458057893079863330580889281222d-10;
elseif( norder == 18 ) ;
xtab(1) =  - 0.504836400887446676837203757885d+01;
xtab(2) =  - 0.424811787356812646302342016090d+01;
xtab(3) =  - 0.357376906848626607950067599377d+01;
xtab(4) =  - 0.296137750553160684477863254906d+01;
xtab(5) =  - 0.238629908916668600026459301424d+01;
xtab(6) =  - 0.183553160426162889225383944409d+01;
xtab(7) =  - 0.130092085838961736566626555439d+01;
xtab(8) =  - 0.776682919267411661316659462284d+00;
xtab(9) =  - 0.258267750519096759258116098711d+00;
xtab(10) =   0.258267750519096759258116098711d+00;
xtab(11) =   0.776682919267411661316659462284d+00;
xtab(12) =   0.130092085838961736566626555439d+01;
xtab(13) =   0.183553160426162889225383944409d+01;
xtab(14) =   0.238629908916668600026459301424d+01;
xtab(15) =   0.296137750553160684477863254906d+01;
xtab(16) =   0.357376906848626607950067599377d+01;
xtab(17) =   0.424811787356812646302342016090d+01;
xtab(18) =   0.504836400887446676837203757885d+01;
weight(1) =  0.782819977211589102925147471012d-11;
weight(2) =  0.104672057957920824443559608435d-07;
weight(3) =  0.181065448109343040959702385911d-05;
weight(4) =  0.918112686792940352914675407371d-04;
weight(5) =  0.188852263026841789438175325426d-02;
weight(6) =  0.186400423875446519219315221973d-01;
weight(7) =  0.973017476413154293308537234155d-01;
weight(8) =  0.284807285669979578595606820713d+00;
weight(9) =  0.483495694725455552876410522141d+00;
weight(10) = 0.483495694725455552876410522141d+00;
weight(11) = 0.284807285669979578595606820713d+00;
weight(12) = 0.973017476413154293308537234155d-01;
weight(13) = 0.186400423875446519219315221973d-01;
weight(14) = 0.188852263026841789438175325426d-02;
weight(15) = 0.918112686792940352914675407371d-04;
weight(16) = 0.181065448109343040959702385911d-05;
weight(17) = 0.104672057957920824443559608435d-07;
weight(18) = 0.782819977211589102925147471012d-11;
elseif( norder == 19 ) ;
xtab(1) =  - 0.522027169053748216460967142500d+01;
xtab(2) =  - 0.442853280660377943723498532226d+01;
xtab(3) =  - 0.376218735196402009751489394104d+01;
xtab(4) =  - 0.315784881834760228184318034120d+01;
xtab(5) =  - 0.259113378979454256492128084112d+01;
xtab(6) =  - 0.204923170985061937575050838669d+01;
xtab(7) =  - 0.152417061939353303183354859367d+01;
xtab(8) =  - 0.101036838713431135136859873726d+01;
xtab(9) =  - 0.503520163423888209373811765050d+00;
xtab(10) =   0.0d+00;
xtab(11) =   0.503520163423888209373811765050d+00;
xtab(12) =   0.101036838713431135136859873726d+01;
xtab(13) =   0.152417061939353303183354859367d+01;
xtab(14) =   0.204923170985061937575050838669d+01;
xtab(15) =   0.259113378979454256492128084112d+01;
xtab(16) =   0.315784881834760228184318034120d+01;
xtab(17) =   0.376218735196402009751489394104d+01;
xtab(18) =   0.442853280660377943723498532226d+01;
xtab(19) =   0.522027169053748216460967142500d+01;
weight(1) =  0.132629709449851575185289154385d-11;
weight(2) =  0.216305100986355475019693077221d-08;
weight(3) =  0.448824314722312295179447915594d-06;
weight(4) =  0.272091977631616257711941025214d-04;
weight(5) =  0.670877521407181106194696282100d-03;
weight(6) =  0.798886677772299020922211491861d-02;
weight(7) =  0.508103869090520673569908110358d-01;
weight(8) =  0.183632701306997074156148485766d+00;
weight(9) =  0.391608988613030244504042313621d+00;
weight(10) = 0.502974888276186530840731361096d+00;
weight(11) = 0.391608988613030244504042313621d+00;
weight(12) = 0.183632701306997074156148485766d+00;
weight(13) = 0.508103869090520673569908110358d-01;
weight(14) = 0.798886677772299020922211491861d-02;
weight(15) = 0.670877521407181106194696282100d-03;
weight(16) = 0.272091977631616257711941025214d-04;
weight(17) = 0.448824314722312295179447915594d-06;
weight(18) = 0.216305100986355475019693077221d-08;
weight(19) = 0.132629709449851575185289154385d-11;
elseif( norder == 20 ) ;
xtab(1) =  - 0.538748089001123286201690041068d+01;
xtab(2) =  - 0.460368244955074427307767524898d+01;
xtab(3) =  - 0.394476404011562521037562880052d+01;
xtab(4) =  - 0.334785456738321632691492452300d+01;
xtab(5) =  - 0.278880605842813048052503375640d+01;
xtab(6) =  - 0.225497400208927552308233334473d+01;
xtab(7) =  - 0.173853771211658620678086566214d+01;
xtab(8) =  - 0.123407621539532300788581834696d+01;
xtab(9) =  - 0.737473728545394358705605144252d+00;
xtab(10) = - 0.245340708300901249903836530634d+00;
xtab(11) =   0.245340708300901249903836530634d+00;
xtab(12) =   0.737473728545394358705605144252d+00;
xtab(13) =   0.123407621539532300788581834696d+01;
xtab(14) =   0.173853771211658620678086566214d+01;
xtab(15) =   0.225497400208927552308233334473d+01;
xtab(16) =   0.278880605842813048052503375640d+01;
xtab(17) =   0.334785456738321632691492452300d+01;
xtab(18) =   0.394476404011562521037562880052d+01;
xtab(19) =   0.460368244955074427307767524898d+01;
xtab(20) =   0.538748089001123286201690041068d+01;
weight(1) =  0.222939364553415129252250061603d-12;
weight(2) =  0.439934099227318055362885145547d-09;
weight(3) =  0.108606937076928169399952456345d-06;
weight(4) =  0.780255647853206369414599199965d-05;
weight(5) =  0.228338636016353967257145917963d-03;
weight(6) =  0.324377334223786183218324713235d-02;
weight(7) =  0.248105208874636108821649525589d-01;
weight(8) =  0.109017206020023320013755033535d+00;
weight(9) =  0.286675505362834129719659706228d+00;
weight(10) = 0.462243669600610089650328639861d+00;
weight(11) = 0.462243669600610089650328639861d+00;
weight(12) = 0.286675505362834129719659706228d+00;
weight(13) = 0.109017206020023320013755033535d+00;
weight(14) = 0.248105208874636108821649525589d-01;
weight(15) = 0.324377334223786183218324713235d-02;
weight(16) = 0.228338636016353967257145917963d-03;
weight(17) = 0.780255647853206369414599199965d-05;
weight(18) = 0.108606937076928169399952456345d-06;
weight(19) = 0.439934099227318055362885145547d-09;
weight(20) = 0.222939364553415129252250061603d-12;
elseif( norder == 30 ) ;
xtab( 1) =   -6.86334529352989158106110835756d+00;
xtab( 2) =   -6.13827922012393462039499237854d+00;
xtab( 3) =   -5.53314715156749572511833355558d+00;
xtab( 4) =   -4.98891896858994394448649710633d+00;
xtab( 5) =   -4.48305535709251834188703761971d+00;
xtab( 6) =   -4.00390860386122881522787601332d+00;
xtab( 7) =   -3.54444387315534988692540090217d+00;
xtab( 8) =   -3.09997052958644174868873332237d+00;
xtab( 9) =   -2.66713212453561720057110646422d+00;
xtab(10) =   -2.24339146776150407247297999483d+00;
xtab(11) =   -1.82674114360368803883588048351d+00;
xtab(12) =   -1.41552780019818851194072510555d+00;
xtab(13) =   -1.00833827104672346180498960870d+00;
xtab(14) =   -0.603921058625552307778155678757d+00;
xtab(15) =   -0.201128576548871485545763013244d+00;
xtab(16) =    0.201128576548871485545763013244d+00;
xtab(17) =    0.603921058625552307778155678757d+00;
xtab(18) =    1.00833827104672346180498960870d+00;
xtab(19) =    1.41552780019818851194072510555d+00;
xtab(20) =    1.82674114360368803883588048351d+00;
xtab(21) =    2.24339146776150407247297999483d+00;
xtab(22) =    2.66713212453561720057110646422d+00;
xtab(23) =    3.09997052958644174868873332237d+00;
xtab(24) =    3.54444387315534988692540090217d+00;
xtab(25) =    4.00390860386122881522787601332d+00;
xtab(26) =    4.48305535709251834188703761971d+00;
xtab(27) =    4.98891896858994394448649710633d+00;
xtab(28) =    5.53314715156749572511833355558d+00;
xtab(29) =    6.13827922012393462039499237854d+00;
xtab(30) =    6.86334529352989158106110835756d+00;
weight( 1) =   0.290825470013122622941102747365d-20;
weight( 2) =   0.281033360275090370876277491534d-16;
weight( 3) =   0.287860708054870606219239791142d-13;
weight( 4) =   0.810618629746304420399344796173d-11;
weight( 5) =   0.917858042437852820850075742492d-09;
weight( 6) =   0.510852245077594627738963204403d-07;
weight( 7) =   0.157909488732471028834638794022d-05;
weight( 8) =   0.293872522892298764150118423412d-04;
weight( 9) =   0.348310124318685523420995323183d-03;
weight(10) =   0.273792247306765846298942568953d-02;
weight(11) =   0.147038297048266835152773557787d-01;
weight(12) =   0.551441768702342511680754948183d-01;
weight(13) =   0.146735847540890099751693643152d+00;
weight(14) =   0.280130930839212667413493211293d+00;
weight(15) =   0.386394889541813862555601849165d+00;
weight(16) =   0.386394889541813862555601849165d+00;
weight(17) =   0.280130930839212667413493211293d+00;
weight(18) =   0.146735847540890099751693643152d+00;
weight(19) =   0.551441768702342511680754948183d-01;
weight(20) =   0.147038297048266835152773557787d-01;
weight(21) =   0.273792247306765846298942568953d-02;
weight(22) =   0.348310124318685523420995323183d-03;
weight(23) =   0.293872522892298764150118423412d-04;
weight(24) =   0.157909488732471028834638794022d-05;
weight(25) =   0.510852245077594627738963204403d-07;
weight(26) =   0.917858042437852820850075742492d-09;
weight(27) =   0.810618629746304420399344796173d-11;
weight(28) =   0.287860708054870606219239791142d-13;
weight(29) =   0.281033360275090370876277491534d-16;
weight(30) =   0.290825470013122622941102747365d-20;
elseif( norder == 32 ) ;
xtab( 1) =   -7.12581390983d+00;
xtab( 2) =   -6.40949814927d+00;
xtab( 3) =   -5.81222594952d+00;
xtab( 4) =   -5.27555098652d+00;
xtab( 5) =   -4.77716450350d+00;
xtab( 6) =   -4.30554795335d+00;
xtab( 7) =   -3.85375548547d+00;
xtab( 8) =   -3.41716749282d+00;
xtab( 9) =   -2.99249082500d+00;
xtab(10) =   -2.57724953773d+00;
xtab(11) =   -2.16949918361d+00;
xtab(12) =   -1.76765410946d+00;
xtab(13) =   -1.37037641095d+00;
xtab(14) =  -0.976500463590d+00;
xtab(15) =  -0.584978765436d+00;
xtab(16) =  -0.194840741569d+00;
xtab(17) =   0.194840741569d+00;
xtab(18) =   0.584978765436d+00;
xtab(19) =   0.976500463590d+00;
xtab(20) =    1.37037641095d+00;
xtab(21) =    1.76765410946d+00;
xtab(22) =    2.16949918361d+00;
xtab(23) =    2.57724953773d+00;
xtab(24) =    2.99249082500d+00;
xtab(25) =    3.41716749282d+00;
xtab(26) =    3.85375548547d+00;
xtab(27) =    4.30554795335d+00;
xtab(28) =    4.77716450350d+00;
xtab(29) =    5.27555098652d+00;
xtab(30) =    5.81222594952d+00;
xtab(31) =    6.40949814927d+00;
xtab(32) =    7.12581390983d+00;
weight( 1) =   0.731067642736d-22;
weight( 2) =   0.923173653649d-18;
weight( 3) =   0.119734401709d-14;
weight( 4) =   0.421501021125d-12;
weight( 5) =   0.593329146300d-10;
weight( 6) =   0.409883216476d-08;
weight( 7) =   0.157416779254d-06;
weight( 8) =   0.365058512955d-05;
weight( 9) =   0.541658406172d-04;
weight(10) =   0.536268365526d-03;
weight(11) =   0.365489032664d-02;
weight(12) =   0.175534288315d-01;
weight(13) =   0.604581309557d-01;
weight(14) =   0.151269734076d+00;
weight(15) =   0.277458142302d+00;
weight(16) =   0.375238352592d+00;
weight(17) =   0.375238352592d+00;
weight(18) =   0.277458142302d+00;
weight(19) =   0.151269734076d+00;
weight(20) =   0.604581309557d-01;
weight(21) =   0.175534288315d-01;
weight(22) =   0.365489032664d-02;
weight(23) =   0.536268365526d-03;
weight(24) =   0.541658406172d-04;
weight(25) =   0.365058512955d-05;
weight(26) =   0.157416779254d-06;
weight(27) =   0.409883216476d-08;
weight(28) =   0.593329146300d-10;
weight(29) =   0.421501021125d-12;
weight(30) =   0.119734401709d-14;
weight(31) =   0.923173653649d-18;
weight(32) =   0.731067642736d-22;
elseif( norder == 40 ) ;
xtab( 1) =   -8.09876113925d+00;
xtab( 2) =   -7.41158253149d+00;
xtab( 3) =   -6.84023730525d+00;
xtab( 4) =   -6.32825535122d+00;
xtab( 5) =   -5.85409505603d+00;
xtab( 6) =   -5.40665424797d+00;
xtab( 7) =   -4.97926097855d+00;
xtab( 8) =   -4.56750207284d+00;
xtab( 9) =   -4.16825706683d+00;
xtab(10) =   -3.77920675344d+00;
xtab(11) =   -3.39855826586d+00;
xtab(12) =   -3.02487988390d+00;
xtab(13) =   -2.65699599844d+00;
xtab(14) =   -2.29391714188d+00;
xtab(15) =   -1.93479147228d+00;
xtab(16) =   -1.57886989493d+00;
xtab(17) =   -1.22548010905d+00;
xtab(18) =  -0.874006612357d+00;
xtab(19) =  -0.523874713832d+00;
xtab(20) =  -0.174537214598d+00;
xtab(21) =   0.174537214598d+00;
xtab(22) =   0.523874713832d+00;
xtab(23) =   0.874006612357d+00;
xtab(24) =    1.22548010905d+00;
xtab(25) =    1.57886989493d+00;
xtab(26) =    1.93479147228d+00;
xtab(27) =    2.29391714188d+00;
xtab(28) =    2.65699599844d+00;
xtab(29) =    3.02487988390d+00;
xtab(30) =    3.39855826586d+00;
xtab(31) =    3.77920675344d+00;
xtab(32) =    4.16825706683d+00;
xtab(33) =    4.56750207284d+00;
xtab(34) =    4.97926097855d+00;
xtab(35) =    5.40665424797d+00;
xtab(36) =    5.85409505603d+00;
xtab(37) =    6.32825535122d+00;
xtab(38) =    6.84023730525d+00;
xtab(39) =    7.41158253149d+00;
xtab(40) =    8.09876113925d+00;
weight( 1) =   0.259104371384d-28;
weight( 2) =   0.854405696375d-24;
weight( 3) =   0.256759336540d-20;
weight( 4) =   0.198918101211d-17;
weight( 5) =   0.600835878947d-15;
weight( 6) =   0.880570764518d-13;
weight( 7) =   0.715652805267d-11;
weight( 8) =   0.352562079135d-09;
weight( 9) =   0.112123608322d-07;
weight(10) =   0.241114416359d-06;
weight(11) =   0.363157615067d-05;
weight(12) =   0.393693398108d-04;
weight(13) =   0.313853594540d-03;
weight(14) =   0.187149682959d-02;
weight(15) =   0.846088800823d-02;
weight(16) =   0.293125655361d-01;
weight(17) =   0.784746058652d-01;
weight(18) =   0.163378732713d+00;
weight(19) =   0.265728251876d+00;
weight(20) =   0.338643277425d+00;
weight(21) =   0.338643277425d+00;
weight(22) =   0.265728251876d+00;
weight(23) =   0.163378732713d+00;
weight(24) =   0.784746058652d-01;
weight(25) =   0.293125655361d-01;
weight(26) =   0.846088800823d-02;
weight(27) =   0.187149682959d-02;
weight(28) =   0.313853594540d-03;
weight(29) =   0.393693398108d-04;
weight(30) =   0.363157615067d-05;
weight(31) =   0.241114416359d-06;
weight(32) =   0.112123608322d-07;
weight(33) =   0.352562079135d-09;
weight(34) =   0.715652805267d-11;
weight(35) =   0.880570764518d-13;
weight(36) =   0.600835878947d-15;
weight(37) =   0.198918101211d-17;
weight(38) =   0.256759336540d-20;
weight(39) =   0.854405696375d-24;
weight(40) =   0.259104371384d-28;
elseif( norder == 50 ) ;
xtab( 1) =   -9.18240695813d+00;
xtab( 2) =   -8.52277103092d+00;
xtab( 3) =   -7.97562236821d+00;
xtab( 4) =   -7.48640942986d+00;
xtab( 5) =   -7.03432350977d+00;
xtab( 6) =   -6.60864797386d+00;
xtab( 7) =   -6.20295251927d+00;
xtab( 8) =   -5.81299467542d+00;
xtab( 9) =   -5.43578608722d+00;
xtab(10) =   -5.06911758492d+00;
xtab(11) =   -4.71129366617d+00;
xtab(12) =   -4.36097316045d+00;
xtab(13) =   -4.01706817286d+00;
xtab(14) =   -3.67867706252d+00;
xtab(15) =   -3.34503831394d+00;
xtab(16) =   -3.01549776957d+00;
xtab(17) =   -2.68948470227d+00;
xtab(18) =   -2.36649390430d+00;
xtab(19) =   -2.04607196869d+00;
xtab(20) =   -1.72780654752d+00;
xtab(21) =   -1.41131775490d+00;
xtab(22) =   -1.09625112896d+00;
xtab(23) =  -0.782271729555d+00;
xtab(24) =  -0.469059056678d+00;
xtab(25) =  -0.156302546889d+00;
xtab(26) =   0.156302546889d+00;
xtab(27) =   0.469059056678d+00;
xtab(28) =   0.782271729555d+00;
xtab(29) =    1.09625112896d+00;
xtab(30) =    1.41131775490d+00;
xtab(31) =    1.72780654752d+00;
xtab(32) =    2.04607196869d+00;
xtab(33) =    2.36649390430d+00;
xtab(34) =    2.68948470227d+00;
xtab(35) =    3.01549776957d+00;
xtab(36) =    3.34503831394d+00;
xtab(37) =    3.67867706252d+00;
xtab(38) =    4.01706817286d+00;
xtab(39) =    4.36097316045d+00;
xtab(40) =    4.71129366617d+00;
xtab(41) =    5.06911758492d+00;
xtab(42) =    5.43578608722d+00;
xtab(43) =    5.81299467542d+00;
xtab(44) =    6.20295251927d+00;
xtab(45) =    6.60864797386d+00;
xtab(46) =    7.03432350977d+00;
xtab(47) =    7.48640942986d+00;
xtab(48) =    7.97562236821d+00;
xtab(49) =    8.52277103092d+00;
xtab(50) =    9.18240695813d+00;
weight( 1) =   0.183379404857d-36;
weight( 2) =   0.167380166790d-31;
weight( 3) =   0.121524412340d-27;
weight( 4) =   0.213765830835d-24;
weight( 5) =   0.141709359957d-21;
weight( 6) =   0.447098436530d-19;
weight( 7) =   0.774238295702d-17;
weight( 8) =   0.809426189344d-15;
weight( 9) =   0.546594403180d-13;
weight(10) =   0.250665552389d-11;
weight(11) =   0.811187736448d-10;
weight(12) =   0.190904054379d-08;
weight(13) =   0.334679340401d-07;
weight(14) =   0.445702996680d-06;
weight(15) =   0.458168270794d-05;
weight(16) =   0.368401905377d-04;
weight(17) =   0.234269892109d-03;
weight(18) =   0.118901178175d-02;
weight(19) =   0.485326382616d-02;
weight(20) =   0.160319410684d-01;
weight(21) =   0.430791591566d-01;
weight(22) =   0.945489354768d-01;
weight(23) =   0.170032455676d+00;
weight(24) =   0.251130856331d+00;
weight(25) =   0.305085129203d+00;
weight(26) =   0.305085129203d+00;
weight(27) =   0.251130856331d+00;
weight(28) =   0.170032455676d+00;
weight(29) =   0.945489354768d-01;
weight(30) =   0.430791591566d-01;
weight(31) =   0.160319410684d-01;
weight(32) =   0.485326382616d-02;
weight(33) =   0.118901178175d-02;
weight(34) =   0.234269892109d-03;
weight(35) =   0.368401905377d-04;
weight(36) =   0.458168270794d-05;
weight(37) =   0.445702996680d-06;
weight(38) =   0.334679340401d-07;
weight(39) =   0.190904054379d-08;
weight(40) =   0.811187736448d-10;
weight(41) =   0.250665552389d-11;
weight(42) =   0.546594403180d-13;
weight(43) =   0.809426189344d-15;
weight(44) =   0.774238295702d-17;
weight(45) =   0.447098436530d-19;
weight(46) =   0.141709359957d-21;
weight(47) =   0.213765830835d-24;
weight(48) =   0.121524412340d-27;
weight(49) =   0.167380166790d-31;
weight(50) =   0.183379404857d-36;
elseif( norder == 60 ) ;
xtab( 1) =   -10.1591092462d+00;
xtab( 2) =   -9.52090367701d+00;
xtab( 3) =   -8.99239800140d+00;
xtab( 4) =   -8.52056928412d+00;
xtab( 5) =   -8.08518865425d+00;
xtab( 6) =   -7.67583993750d+00;
xtab( 7) =   -7.28627659440d+00;
xtab( 8) =   -6.91238153219d+00;
xtab( 9) =   -6.55125916706d+00;
xtab(10) =   -6.20077355799d+00;
xtab(11) =   -5.85929019639d+00;
xtab(12) =   -5.52552108614d+00;
xtab(13) =   -5.19842653458d+00;
xtab(14) =   -4.87715007747d+00;
xtab(15) =   -4.56097375794d+00;
xtab(16) =   -4.24928643596d+00;
xtab(17) =   -3.94156073393d+00;
xtab(18) =   -3.63733587617d+00;
xtab(19) =   -3.33620465355d+00;
xtab(20) =   -3.03780333823d+00;
xtab(21) =   -2.74180374807d+00;
xtab(22) =   -2.44790690231d+00;
xtab(23) =   -2.15583787123d+00;
xtab(24) =   -1.86534153123d+00;
xtab(25) =   -1.57617901198d+00;
xtab(26) =   -1.28812467487d+00;
xtab(27) =   -1.00096349956d+00;
xtab(28) =  -0.714488781673d+00;
xtab(29) =  -0.428500064221d+00;
xtab(30) =  -0.142801238703d+00;
xtab(31) =   0.142801238703d+00;
xtab(32) =   0.428500064221d+00;
xtab(33) =   0.714488781673d+00;
xtab(34) =    1.00096349956d+00;
xtab(35) =    1.28812467487d+00;
xtab(36) =    1.57617901198d+00;
xtab(37) =    1.86534153123d+00;
xtab(38) =    2.15583787123d+00;
xtab(39) =    2.44790690231d+00;
xtab(40) =    2.74180374807d+00;
xtab(41) =    3.03780333823d+00;
xtab(42) =    3.33620465355d+00;
xtab(43) =    3.63733587617d+00;
xtab(44) =    3.94156073393d+00;
xtab(45) =    4.24928643596d+00;
xtab(46) =    4.56097375794d+00;
xtab(47) =    4.87715007747d+00;
xtab(48) =    5.19842653458d+00;
xtab(49) =    5.52552108614d+00;
xtab(50) =    5.85929019639d+00;
xtab(51) =    6.20077355799d+00;
xtab(52) =    6.55125916706d+00;
xtab(53) =    6.91238153219d+00;
xtab(54) =    7.28627659440d+00;
xtab(55) =    7.67583993750d+00;
xtab(56) =    8.08518865425d+00;
xtab(57) =    8.52056928412d+00;
xtab(58) =    8.99239800140d+00;
xtab(59) =    9.52090367701d+00;
xtab(60) =    10.1591092462d+00;
weight( 1) =   0.110958724796d-44;
weight( 2) =   0.243974758810d-39;
weight( 3) =   0.377162672698d-35;
weight( 4) =   0.133255961176d-31;
weight( 5) =   0.171557314767d-28;
weight( 6) =   0.102940599693d-25;
weight( 7) =   0.334575695574d-23;
weight( 8) =   0.651256725748d-21;
weight( 9) =   0.815364047300d-19;
weight(10) =   0.692324790956d-17;
weight(11) =   0.415244410968d-15;
weight(12) =   0.181662457614d-13;
weight(13) =   0.594843051597d-12;
weight(14) =   0.148895734905d-10;
weight(15) =   0.289935901280d-09;
weight(16) =   0.445682277521d-08;
weight(17) =   0.547555461926d-07;
weight(18) =   0.543351613419d-06;
weight(19) =   0.439428693625d-05;
weight(20) =   0.291874190415d-04;
weight(21) =   0.160277334681d-03;
weight(22) =   0.731773556963d-03;
weight(23) =   0.279132482894d-02;
weight(24) =   0.893217836028d-02;
weight(25) =   0.240612727660d-01;
weight(26) =   0.547189709320d-01;
weight(27) =   0.105298763697d+00;
weight(28) =   0.171776156918d+00;
weight(29) =   0.237868904958d+00;
weight(30) =   0.279853117522d+00;
weight(31) =   0.279853117522d+00;
weight(32) =   0.237868904958d+00;
weight(33) =   0.171776156918d+00;
weight(34) =   0.105298763697d+00;
weight(35) =   0.547189709320d-01;
weight(36) =   0.240612727660d-01;
weight(37) =   0.893217836028d-02;
weight(38) =   0.279132482894d-02;
weight(39) =   0.731773556963d-03;
weight(40) =   0.160277334681d-03;
weight(41) =   0.291874190415d-04;
weight(42) =   0.439428693625d-05;
weight(43) =   0.543351613419d-06;
weight(44) =   0.547555461926d-07;
weight(45) =   0.445682277521d-08;
weight(46) =   0.289935901280d-09;
weight(47) =   0.148895734905d-10;
weight(48) =   0.594843051597d-12;
weight(49) =   0.181662457614d-13;
weight(50) =   0.415244410968d-15;
weight(51) =   0.692324790956d-17;
weight(52) =   0.815364047300d-19;
weight(53) =   0.651256725748d-21;
weight(54) =   0.334575695574d-23;
weight(55) =   0.102940599693d-25;
weight(56) =   0.171557314767d-28;
weight(57) =   0.133255961176d-31;
weight(58) =   0.377162672698d-35;
weight(59) =   0.243974758810d-39;
weight(60) =   0.110958724796d-44;
elseif( norder == 64 ) ;
xtab( 1) =   -10.5261231680d+00;
xtab( 2) =   -9.89528758683d+00;
xtab( 3) =   -9.37315954965d+00;
xtab( 4) =   -8.90724909996d+00;
xtab( 5) =   -8.47752908338d+00;
xtab( 6) =   -8.07368728501d+00;
xtab( 7) =   -7.68954016404d+00;
xtab( 8) =   -7.32101303278d+00;
xtab( 9) =   -6.96524112055d+00;
xtab(10) =   -6.62011226264d+00;
xtab(11) =   -6.28401122877d+00;
xtab(12) =   -5.95566632680d+00;
xtab(13) =   -5.63405216435d+00;
xtab(14) =   -5.31832522463d+00;
xtab(15) =   -5.00777960220d+00;
xtab(16) =   -4.70181564741d+00;
xtab(17) =   -4.39991716823d+00;
xtab(18) =   -4.10163447457d+00;
xtab(19) =   -3.80657151395d+00;
xtab(20) =   -3.51437593574d+00;
xtab(21) =   -3.22473129199d+00;
xtab(22) =   -2.93735082300d+00;
xtab(23) =   -2.65197243543d+00;
xtab(24) =   -2.36835458863d+00;
xtab(25) =   -2.08627287988d+00;
xtab(26) =   -1.80551717147d+00;
xtab(27) =   -1.52588914021d+00;
xtab(28) =   -1.24720015694d+00;
xtab(29) =  -0.969269423071d+00;
xtab(30) =  -0.691922305810d+00;
xtab(31) =  -0.414988824121d+00;
xtab(32) =  -0.138302244987d+00;
xtab(33) =   0.138302244987d+00;
xtab(34) =   0.414988824121d+00;
xtab(35) =   0.691922305810d+00;
xtab(36) =   0.969269423071d+00;
xtab(37) =    1.24720015694d+00;
xtab(38) =    1.52588914021d+00;
xtab(39) =    1.80551717147d+00;
xtab(40) =    2.08627287988d+00;
xtab(41) =    2.36835458863d+00;
xtab(42) =    2.65197243543d+00;
xtab(43) =    2.93735082300d+00;
xtab(44) =    3.22473129199d+00;
xtab(45) =    3.51437593574d+00;
xtab(46) =    3.80657151395d+00;
xtab(47) =    4.10163447457d+00;
xtab(48) =    4.39991716823d+00;
xtab(49) =    4.70181564741d+00;
xtab(50) =    5.00777960220d+00;
xtab(51) =    5.31832522463d+00;
xtab(52) =    5.63405216435d+00;
xtab(53) =    5.95566632680d+00;
xtab(54) =    6.28401122877d+00;
xtab(55) =    6.62011226264d+00;
xtab(56) =    6.96524112055d+00;
xtab(57) =    7.32101303278d+00;
xtab(58) =    7.68954016404d+00;
xtab(59) =    8.07368728501d+00;
xtab(60) =    8.47752908338d+00;
xtab(61) =    8.90724909996d+00;
xtab(62) =    9.37315954965d+00;
xtab(63) =    9.89528758683d+00;
xtab(64) =    10.5261231680d+00;
weight( 1) =   0.553570653584d-48;
weight( 2) =   0.167974799010d-42;
weight( 3) =   0.342113801099d-38;
weight( 4) =   0.155739062462d-34;
weight( 5) =   0.254966089910d-31;
weight( 6) =   0.192910359546d-28;
weight( 7) =   0.786179778889d-26;
weight( 8) =   0.191170688329d-23;
weight( 9) =   0.298286278427d-21;
weight(10) =   0.315225456649d-19;
weight(11) =   0.235188471067d-17;
weight(12) =   0.128009339117d-15;
weight(13) =   0.521862372645d-14;
weight(14) =   0.162834073070d-12;
weight(15) =   0.395917776693d-11;
weight(16) =   0.761521725012d-10;
weight(17) =   0.117361674232d-08;
weight(18) =   0.146512531647d-07;
weight(19) =   0.149553293672d-06;
weight(20) =   0.125834025103d-05;
weight(21) =   0.878849923082d-05;
weight(22) =   0.512592913577d-04;
weight(23) =   0.250983698512d-03;
weight(24) =   0.103632909950d-02;
weight(25) =   0.362258697852d-02;
weight(26) =   0.107560405098d-01;
weight(27) =   0.272031289536d-01;
weight(28) =   0.587399819634d-01;
weight(29) =   0.108498349306d+00;
weight(30) =   0.171685842349d+00;
weight(31) =   0.232994786062d+00;
weight(32) =   0.271377424940d+00;
weight(33) =   0.271377424940d+00;
weight(34) =   0.232994786062d+00;
weight(35) =   0.171685842349d+00;
weight(36) =   0.108498349306d+00;
weight(37) =   0.587399819634d-01;
weight(38) =   0.272031289536d-01;
weight(39) =   0.107560405098d-01;
weight(40) =   0.362258697852d-02;
weight(41) =   0.103632909950d-02;
weight(42) =   0.250983698512d-03;
weight(43) =   0.512592913577d-04;
weight(44) =   0.878849923082d-05;
weight(45) =   0.125834025103d-05;
weight(46) =   0.149553293672d-06;
weight(47) =   0.146512531647d-07;
weight(48) =   0.117361674232d-08;
weight(49) =   0.761521725012d-10;
weight(50) =   0.395917776693d-11;
weight(51) =   0.162834073070d-12;
weight(52) =   0.521862372645d-14;
weight(53) =   0.128009339117d-15;
weight(54) =   0.235188471067d-17;
weight(55) =   0.315225456649d-19;
weight(56) =   0.298286278427d-21;
weight(57) =   0.191170688329d-23;
weight(58) =   0.786179778889d-26;
weight(59) =   0.192910359546d-28;
weight(60) =   0.254966089910d-31;
weight(61) =   0.155739062462d-34;
weight(62) =   0.342113801099d-38;
weight(63) =   0.167974799010d-42;
weight(64) =   0.553570653584d-48;
else;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'HERMITE_SET - Fatal error!');
writef(1,['%s','%6i','\n'], '  Illegal value of NORDER = ', norder);
writef(1,['%s','\n'], '  Legal values are 1 to 20,');
writef(1,['%s','\n'], '  30, 32, 40, 50, 60 and 64.');
error(['stop encountered in original fortran code  ',char(10),';']);
end;
return;
end
function [norder, xtab, weight, alpha, beta]=jacobi_com( norder, xtab, weight, alpha, beta );
%*******************************************************************************
%
%! JACOBI_COM computes the abscissa and weights for Gauss-Jacobi quadrature.
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
%    Integral ( -1 <= X <= 1 ) (1+X)**ALPHA * (1-X)**BETA * F(X) dX
%
%  Approximate integral:
%
%    Sum ( 1 <= I <= NORDER ) WEIGHT(I) * F ( XTAB(I) )
%
%  Modified:
%
%    09 September 2000
%
%  Reference:
%
%    Arthur Stroud and Don Secrest,
%    Gaussian Quadrature Formulas,
%    Prentice Hall, 1966.
%
%  Parameters:
%
%    Input, integer NORDER, the order of the quadrature rule to be computed.
%
%    Output, real ( kind = 8 ) XTAB(NORDER), the Gauss-Jacobi abscissas.
%
%    Output, real ( kind = 8 ) WEIGHT(NORDER), the Gauss-Jacobi weights.
%
%    Input, real ( kind = 8 ) ALPHA, BETA, the exponents of (1+X) and
%    (1-X) in the quadrature rule.  For simple Gauss-Legendre quadrature,
%    set ALPHA = BETA = 0.0.
%
persistent an b bn c cc delta dp2 i p1 r1 r2 r3 temp x ; 

if isempty(an), an=0; end;
if isempty(b), b=zeros(1,norder); end;
if isempty(bn), bn=0; end;
if isempty(c), c=zeros(1,norder); end;
if isempty(cc), cc=0; end;
if isempty(delta), delta=0; end;
if isempty(dp2), dp2=0; end;
if isempty(i), i=0; end;
if isempty(p1), p1=0; end;
if isempty(r1), r1=0; end;
if isempty(r2), r2=0; end;
if isempty(r3), r3=0; end;
if isempty(temp), temp=0; end;
if isempty(x), x=0; end;
%
%  Set the recursion coefficients.
%
for i = 1: norder;
if( alpha + beta == 0.0d+00 || beta - alpha == 0.0d+00)
b(i) = 0.0d+00;
else;
b(i) =( alpha + beta ) .*( beta - alpha ) ./(( alpha + beta + real( 2 .* i) ).*( alpha + beta + real( 2 .* i - 2) ) );
end;
if( i == 1 )
c(i) = 0.0d+00;
else;
c(i) = 4.0d+00 .* real( i - 1).*( alpha + real( i - 1) ).*( beta + real( i - 1) ).*( alpha + beta + real( i - 1) ) ./(( alpha + beta + real( 2 .* i - 1) ).*( alpha + beta + real( 2 .* i - 2) ).^2.*( alpha + beta + real( 2 .* i - 3) ) );
end;
end; i = fix(norder+1);
delta = exp( log_gamma( alpha + 1.0d+00 ) + log_gamma( beta + 1.0d+00)+ log_gamma( alpha + beta + 2.0d+00 ) );
cc = delta .* 2.0d+00.^( alpha + beta + 1.0d+00 ) .* prod( c([2:norder]) );
for i = 1: norder;
if( i == 1 )
an = alpha ./ real( norder);
bn = beta ./ real( norder);
r1 =( 1.0d+00 + alpha ).*( 2.78d+00 ./( 4.0d+00+ real( norder.^2) )+ 0.768d+00 .* an ./ real( norder) );
r2 = 1.0d+00 + 1.48d+00 .* an + 0.96d+00 .* bn+ 0.452d+00 .* an.^2 + 0.83d+00 .* an .* bn;
x =( r2 - r1 ) ./ r2;
elseif( i == 2 ) ;
r1 =( 4.1d+00 + alpha ) ./(( 1.0d+00 + alpha ) .*( 1.0d+00 + 0.156d+00 .* alpha ) );
r2 = 1.0d+00 + 0.06d+00 .*( real( norder) - 8.0d+00 ) .*( 1.0d+00 + 0.12d+00 .* alpha ) ./ real( norder);
r3 = 1.0d+00 + 0.012d+00 .* beta .*( 1.0d+00 + 0.25d+00 .* abs( alpha ) ) ./ real( norder);
x = x - r1 .* r2 .* r3 .*( 1.0d+00 - x );
elseif( i == 3 ) ;
r1 =( 1.67d+00 + 0.28d+00 .* alpha ) ./( 1.0d+00 + 0.37d+00 .* alpha );
r2 = 1.0d+00 + 0.22d+00 .*( real( norder) - 8.0d+00 )./ real( norder);
r3 = 1.0d+00 + 8.0d+00 .* beta ./(( 6.28d+00 + beta ) .* real( norder.^2) );
x = x - r1 .* r2 .* r3 .*( xtab(1) - x );
elseif( i < norder - 1 ) ;
x = 3.0d+00 .* xtab(i-1) - 3.0d+00 .* xtab(i-2) + xtab(i-3);
elseif( i == norder - 1 ) ;
r1 =( 1.0d+00 + 0.235d+00 .* beta ) ./( 0.766d+00 + 0.119d+00 .* beta );
r2 = 1.0d+00 ./( 1.0d+00 + 0.639d+00.*( real( norder) - 4.0d+00 )./( 1.0d+00 + 0.71d+00 .*( real( norder) - 4.0d+00 ) ) );
r3 = 1.0d+00 ./( 1.0d+00 + 20.0d+00 .* alpha ./(( 7.5d+00 + alpha ) .*real( norder.^2) ) );
x = x + r1 .* r2 .* r3 .*( x - xtab(i-2) );
elseif( i == norder ) ;
r1 =( 1.0d+00 + 0.37d+00 .* beta ) ./( 1.67d+00 + 0.28d+00 .* beta );
r2 = 1.0d+00 ./( 1.0d+00 + 0.22d+00 .*( real( norder) - 8.0d+00 )./ real( norder) );
r3 = 1.0d+00 ./( 1.0d+00 + 8.0d+00 .* alpha ./(( 6.28d+00 + alpha ) .* real( norder.^2) ) );
x = x + r1 .* r2 .* r3 .*( x - xtab(i-2) );
end;
[ x, norder, alpha, beta, dp2, p1, b, c ]=jacobi_root( x, norder, alpha, beta, dp2, p1, b, c );
xtab(i) = x;
weight(i) = cc ./( dp2 .* p1 );
end; i = fix(norder+1);
%
%  Reverse the order of the XTAB values.
%
[ norder, xtab ]=dvec_reverse( norder, xtab );
return;
end
function [p2, dp2, p1, x, norder, alpha, beta, b, c]=jacobi_recur( p2, dp2, p1, x, norder, alpha, beta, b, c );
%*******************************************************************************
%
%! JACOBI_RECUR finds the value and derivative of a Jacobi polynomial.
%
%  Modified:
%
%    19 September 1998
%
%  Reference:
%
%    Arthur Stroud and Don Secrest,
%    Gaussian Quadrature Formulas,
%    Prentice Hall, 1966.
%
%  Parameters:
%
%    Output, real ( kind = 8 ) P2, the value of J(NORDER,X).
%
%    Output, real ( kind = 8 ) DP2, the value of J'(NORDER,X).
%
%    Output, real ( kind = 8 ) P1, the value of J(NORDER-1,X).
%
%    Input, real ( kind = 8 ) X, the point at which polynomials are evaluated.
%
%    Input, integer NORDER, the order of the polynomial to be computed.
%
%    Input, real ( kind = 8 ) ALPHA, BETA, the exponents of (1+X) and
%    (1-X) in the quadrature rule.
%
%    Input, real ( kind = 8 ) B(NORDER), C(NORDER), the recursion
%    coefficients.
%
persistent dp0 dp1 i p0 ; 

if isempty(dp0), dp0=0; end;
if isempty(dp1), dp1=0; end;
if isempty(i), i=0; end;
if isempty(p0), p0=0; end;
p1 = 1.0d+00;
dp1 = 0.0d+00;
p2 = x +( alpha - beta ) ./( alpha + beta + 2.0d+00 );
dp2 = 1.0d+00;
for i = 2: norder;
p0 = p1;
dp0 = dp1;
p1 = p2;
dp1 = dp2;
p2 =( x - b(i) ) .* p1 - c(i) .* p0;
dp2 =( x - b(i) ) .* dp1 + p1 - c(i) .* dp0;
end; i = fix(norder+1);
return;
end
function [x, norder, alpha, beta, dp2, p1, b, c]=jacobi_root( x, norder, alpha, beta, dp2, p1, b, c );
%*******************************************************************************
%
%! JACOBI_ROOT improves an approximate root of a Jacobi polynomial.
%
%  Modified:
%
%    09 December 2000
%
%  Reference:
%
%    Arthur Stroud and Don Secrest,
%    Gaussian Quadrature Formulas,
%    Prentice Hall, 1966.
%
%  Parameters:
%
%    Input/output, real ( kind = 8 ) X, the approximate root, which
%    should be improved on output.
%
%    Input, integer NORDER, the order of the polynomial to be computed.
%
%    Input, real ( kind = 8 ) ALPHA, BETA, the exponents of (1+X) and
%    (1-X) in the quadrature rule.
%
%    Output, real ( kind = 8 ) DP2, the value of J'(NORDER,X).
%
%    Output, real ( kind = 8 ) P1, the value of J(NORDER-1,X).
%
%    Input, real ( kind = 8 ) B(NORDER), C(NORDER), the recursion coefficients.
%
persistent d eps i maxstep p2 ; 

if isempty(d), d=0; end;
if isempty(eps), eps=0; end;
if isempty(i), i=0; end;
if isempty(maxstep), maxstep = 10; end;
if isempty(p2), p2=0; end;
eps = eps;
for i = 1: maxstep;
[ p2, dp2, p1, x, norder, alpha, beta, b, c ]=jacobi_recur( p2, dp2, p1, x, norder, alpha, beta, b, c );
d = p2 ./ dp2;
x = x - d;
if( abs( d ) <= eps .*( abs( x ) + 1.0d+00 ) )
return;
end;
end; i = fix(maxstep+1);
return;
end
function [norder, xtab, weight]=kronrod_set( norder, xtab, weight );
%*******************************************************************************
%
%! KRONROD_SET sets abscissas and weights for Gauss-Kronrod quadrature.
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
%    Integral ( -1 <= X <= 1 ) F(X) dX
%
%  Approximate integral:
%
%    Sum ( 1 <= I <= NORDER ) WEIGHT(I) * F ( XTAB(I) )
%
%  Note:
%
%    A Kronrod rule is used in conjunction with a lower order
%    Gauss rule, and provides an efficient error estimation.
%
%    The error may be estimated as the difference in the two integral
%    approximations.
%
%    The efficiency comes about because the Kronrod uses the abscissas
%    of the Gauss rule, thus saving on the number of function evaluations
%    necessary.  If the Kronrod rule were replaced by a Gauss rule of
%    the same order, a higher precision integral estimate would be
%    made, but the function would have to be evaluated at many more
%    points.
%
%    The Gauss Kronrod pair of rules involves an ( NORDER + 1 ) / 2
%    point Gauss-Legendre rule and an NORDER point Kronrod rule.
%    Thus, the 15 point Kronrod rule should be paired with the
%    Gauss-Legendre 7 point rule.
%
%  Modified:
%
%    16 September 1998
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    R Piessens, E de Doncker-Kapenger, C Ueberhuber, D Kahaner,
%    QUADPACK, A Subroutine Package for Automatic Integration,
%    Springer Verlag, 1983.
%
%  Parameters:
%
%    Input, integer NORDER, the order of the rule, which may be
%    15, 21, 31 or 41, corresponding to Gauss-Legendre rules of
%    order 7, 10, 15 or 20.
%
%    Output, real ( kind = 8 ) XTAB(NORDER), the abscissas of the rule, which
%    are symmetrically places in [-1,1].
%
%    Output, real ( kind = 8 ) WEIGHT(NORDER), the weights of the rule.
%    The weights are positive, symmetric, and should sum to 2.
%

if( norder == 15 )
xtab(1) =  - 0.9914553711208126d+00;
xtab(2) =  - 0.9491079123427585d+00;
xtab(3) =  - 0.8648644233597691d+00;
xtab(4) =  - 0.7415311855993944d+00;
xtab(5) =  - 0.5860872354676911d+00;
xtab(6) =  - 0.4058451513773972d+00;
xtab(7) =  - 0.2077849550789850d+00;
xtab(8) =    0.0d+00;
xtab(9) =    0.2077849550789850d+00;
xtab(10) =   0.4058451513773972d+00;
xtab(11) =   0.5860872354676911d+00;
xtab(12) =   0.7415311855993944d+00;
xtab(13) =   0.8648644233597691d+00;
xtab(14) =   0.9491079123427585d+00;
xtab(15) =   0.9914553711208126d+00;
weight(1) =  0.2293532201052922d-01;
weight(2) =  0.6309209262997855d-01;
weight(3) =  0.1047900103222502d+00;
weight(4) =  0.1406532597155259d+00;
weight(5) =  0.1690047266392679d+00;
weight(6) =  0.1903505780647854d+00;
weight(7) =  0.2044329400752989d+00;
weight(8) =  0.2094821410847278d+00;
weight(9) =  0.2044329400752989d+00;
weight(10) = 0.1903505780647854d+00;
weight(11) = 0.1690047266392679d+00;
weight(12) = 0.1406532597155259d+00;
weight(13) = 0.1047900103222502d+00;
weight(14) = 0.6309209262997855d-01;
weight(15) = 0.2293532201052922d-01;
elseif( norder == 21 ) ;
xtab(1) =  - 0.9956571630258081d+00;
xtab(2) =  - 0.9739065285171717d+00;
xtab(3) =  - 0.9301574913557082d+00;
xtab(4) =  - 0.8650633666889845d+00;
xtab(5) =  - 0.7808177265864169d+00;
xtab(6) =  - 0.6794095682990244d+00;
xtab(7) =  - 0.5627571346686047d+00;
xtab(8) =  - 0.4333953941292472d+00;
xtab(9) =  - 0.2943928627014602d+00;
xtab(10) = - 0.1488743389816312d+00;
xtab(11) =   0.0d+00;
xtab(12) =   0.1488743389816312d+00;
xtab(13) =   0.2943928627014602d+00;
xtab(14) =   0.4333953941292472d+00;
xtab(15) =   0.5627571346686047d+00;
xtab(16) =   0.6794095682990244d+00;
xtab(17) =   0.7808177265864169d+00;
xtab(18) =   0.8650633666889845d+00;
xtab(19) =   0.9301574913557082d+00;
xtab(20) =   0.9739065285171717d+00;
xtab(21) =   0.9956571630258081d+00;
weight(1) =  0.1169463886737187d-01;
weight(2) =  0.3255816230796473d-01;
weight(3) =  0.5475589657435200d-01;
weight(4) =  0.7503967481091995d-01;
weight(5) =  0.9312545458369761d-01;
weight(6) =  0.1093871588022976d+00;
weight(7) =  0.1234919762620659d+00;
weight(8) =  0.1347092173114733d+00;
weight(9) =  0.1427759385770601d+00;
weight(10) = 0.1477391049013385d+00;
weight(11) = 0.1494455540029169d+00;
weight(12) = 0.1477391049013385d+00;
weight(13) = 0.1427759385770601d+00;
weight(14) = 0.1347092173114733d+00;
weight(15) = 0.1234919762620659d+00;
weight(16) = 0.1093871588022976d+00;
weight(17) = 0.9312545458369761d-01;
weight(18) = 0.7503967481091995d-01;
weight(19) = 0.5475589657435200d-01;
weight(20) = 0.3255816230796473d-01;
weight(21) = 0.1169463886737187d-01;
elseif( norder == 31 ) ;
xtab(1) =  - 0.9980022986933971d+00;
xtab(2) =  - 0.9879925180204854d+00;
xtab(3) =  - 0.9677390756791391d+00;
xtab(4) =  - 0.9372733924007059d+00;
xtab(5) =  - 0.8972645323440819d+00;
xtab(6) =  - 0.8482065834104272d+00;
xtab(7) =  - 0.7904185014424659d+00;
xtab(8) =  - 0.7244177313601700d+00;
xtab(9) =  - 0.6509967412974170d+00;
xtab(10) = - 0.5709721726085388d+00;
xtab(11) = - 0.4850818636402397d+00;
xtab(12) = - 0.3941513470775634d+00;
xtab(13) = - 0.2991800071531688d+00;
xtab(14) = - 0.2011940939974345d+00;
xtab(15) = - 0.1011420669187175d+00;
xtab(16) =   0.0d+00;
xtab(17) =   0.1011420669187175d+00;
xtab(18) =   0.2011940939974345d+00;
xtab(19) =   0.2991800071531688d+00;
xtab(20) =   0.3941513470775634d+00;
xtab(21) =   0.4850818636402397d+00;
xtab(22) =   0.5709721726085388d+00;
xtab(23) =   0.6509967412974170d+00;
xtab(24) =   0.7244177313601700d+00;
xtab(25) =   0.7904185014424659d+00;
xtab(26) =   0.8482065834104272d+00;
xtab(27) =   0.8972645323440819d+00;
xtab(28) =   0.9372733924007059d+00;
xtab(29) =   0.9677390756791391d+00;
xtab(30) =   0.9879925180204854d+00;
xtab(31) =   0.9980022986933971d+00;
weight(1) =  0.5377479872923349d-02;
weight(2) =  0.1500794732931612d-01;
weight(3) =  0.2546084732671532d-01;
weight(4) =  0.3534636079137585d-01;
weight(5) =  0.4458975132476488d-01;
weight(6) =  0.5348152469092809d-01;
weight(7) =  0.6200956780067064d-01;
weight(8) =  0.6985412131872826d-01;
weight(9) =  0.7684968075772038d-01;
weight(10) = 0.8308050282313302d-01;
weight(11) = 0.8856444305621177d-01;
weight(12) = 0.9312659817082532d-01;
weight(13) = 0.9664272698362368d-01;
weight(14) = 0.9917359872179196d-01;
weight(15) = 0.1007698455238756d+00;
weight(16) = 0.1013300070147915d+00;
weight(17) = 0.1007698455238756d+00;
weight(18) = 0.9917359872179196d-01;
weight(19) = 0.9664272698362368d-01;
weight(20) = 0.9312659817082532d-01;
weight(21) = 0.8856444305621177d-01;
weight(22) = 0.8308050282313302d-01;
weight(23) = 0.7684968075772038d-01;
weight(24) = 0.6985412131872826d-01;
weight(25) = 0.6200956780067064d-01;
weight(26) = 0.5348152469092809d-01;
weight(27) = 0.4458975132476488d-01;
weight(28) = 0.3534636079137585d-01;
weight(29) = 0.2546084732671532d-01;
weight(30) = 0.1500794732931612d-01;
weight(31) = 0.5377479872923349d-02;
elseif( norder == 41 ) ;
xtab(1) =  - 0.9988590315882777d+00;
xtab(2) =  - 0.9931285991850949d+00;
xtab(3) =  - 0.9815078774502503d+00;
xtab(4) =  - 0.9639719272779138d+00;
xtab(5) =  - 0.9408226338317548d+00;
xtab(6) =  - 0.9122344282513259d+00;
xtab(7) =  - 0.8782768112522820d+00;
xtab(8) =  - 0.8391169718222188d+00;
xtab(9) =  - 0.7950414288375512d+00;
xtab(10) = - 0.7463319064601508d+00;
xtab(11) = - 0.6932376563347514d+00;
xtab(12) = - 0.6360536807265150d+00;
xtab(13) = - 0.5751404468197103d+00;
xtab(14) = - 0.5108670019508271d+00;
xtab(15) = - 0.4435931752387251d+00;
xtab(16) = - 0.3737060887154196d+00;
xtab(17) = - 0.3016278681149130d+00;
xtab(18) = - 0.2277858511416451d+00;
xtab(19) = - 0.1526054652409227d+00;
xtab(20) = - 0.7652652113349733d-01;
xtab(21) =   0.0d+00;
xtab(22) =   0.7652652113349733d-01;
xtab(23) =   0.1526054652409227d+00;
xtab(24) =   0.2277858511416451d+00;
xtab(25) =   0.3016278681149130d+00;
xtab(26) =   0.3737060887154196d+00;
xtab(27) =   0.4435931752387251d+00;
xtab(28) =   0.5108670019508271d+00;
xtab(29) =   0.5751404468197103d+00;
xtab(30) =   0.6360536807265150d+00;
xtab(31) =   0.6932376563347514d+00;
xtab(32) =   0.7463319064601508d+00;
xtab(33) =   0.7950414288375512d+00;
xtab(34) =   0.8391169718222188d+00;
xtab(35) =   0.8782768112522820d+00;
xtab(36) =   0.9122344282513259d+00;
xtab(37) =   0.9408226338317548d+00;
xtab(38) =   0.9639719272779138d+00;
xtab(39) =   0.9815078774502503d+00;
xtab(40) =   0.9931285991850949d+00;
xtab(41) =   0.9988590315882777d+00;
weight(1) =  0.3073583718520532d-02;
weight(2) =  0.8600269855642942d-02;
weight(3) =  0.1462616925697125d-01;
weight(4) =  0.2038837346126652d-01;
weight(5) =  0.2588213360495116d-01;
weight(6) =  0.3128730677703280d-01;
weight(7) =  0.3660016975820080d-01;
weight(8) =  0.4166887332797369d-01;
weight(9) =  0.4643482186749767d-01;
weight(10) = 0.5094457392372869d-01;
weight(11) = 0.5519510534828599d-01;
weight(12) = 0.5911140088063957d-01;
weight(13) = 0.6265323755478117d-01;
weight(14) = 0.6583459713361842d-01;
weight(15) = 0.6864867292852162d-01;
weight(16) = 0.7105442355344407d-01;
weight(17) = 0.7303069033278667d-01;
weight(18) = 0.7458287540049919d-01;
weight(19) = 0.7570449768455667d-01;
weight(20) = 0.7637786767208074d-01;
weight(21) = 0.7660071191799966d-01;
weight(22) = 0.7637786767208074d-01;
weight(23) = 0.7570449768455667d-01;
weight(24) = 0.7458287540049919d-01;
weight(25) = 0.7303069033278667d-01;
weight(26) = 0.7105442355344407d-01;
weight(27) = 0.6864867292852162d-01;
weight(28) = 0.6583459713361842d-01;
weight(29) = 0.6265323755478117d-01;
weight(30) = 0.5911140088063957d-01;
weight(31) = 0.5519510534828599d-01;
weight(32) = 0.5094457392372869d-01;
weight(33) = 0.4643482186749767d-01;
weight(34) = 0.4166887332797369d-01;
weight(35) = 0.3660016975820080d-01;
weight(36) = 0.3128730677703280d-01;
weight(37) = 0.2588213360495116d-01;
weight(38) = 0.2038837346126652d-01;
weight(39) = 0.1462616925697125d-01;
weight(40) = 0.8600269855642942d-02;
weight(41) = 0.3073583718520532d-02;
else;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'KRONROD_SET - Fatal error!');
writef(1,['%s','%6i','\n'], '  Illegal value of NORDER = ', norder);
writef(1,['%s','\n'], '  Legal values are 15, 21, 31 or 41.');
error(['stop encountered in original fortran code  ',char(10),';']);
end;
return;
end
function [norder, xtab, weight, alpha]=laguerre_com( norder, xtab, weight, alpha );
%*******************************************************************************
%
%! LAGUERRE_COM computes the abscissa and weights for Gauss-Laguerre quadrature.
%
%  Discussion:
%
%    In the simplest case, ALPHA is 0, and we are approximating the
%    integral from 0 to INFINITY of EXP(-X) * F(X).  When this is so,
%    it is easy to modify the rule to approximate the integral from
%    A to INFINITY as well.
%
%    If ALPHA is nonzero, then there is no simple way to extend the
%    rule to approximate the integral from A to INFINITY.  The simplest
%    procedures would be to approximate the integral from 0 to A.
%
%  Integration interval:
%
%      [ A, +Infinity ) or [ 0, +Infinity )
%
%  Weight function:
%
%      EXP ( - X ) or EXP ( - X ) * X**ALPHA
%
%  Integral to approximate:
%
%      Integral ( A <= X < +INFINITY ) EXP ( - X ) * F(X) dX
%    or
%      Integral ( 0 <= X < +INFINITY ) EXP ( - X ) * X**ALPHA * F(X) dX
%
%  Approximate integral:
%
%      EXP ( - A ) * Sum ( 1 <= I <= NORDER ) WEIGHT(I) * F ( A+XTAB(I) )
%    or
%      Sum ( 1 <= I <= NORDER ) WEIGHT(I) * F ( XTAB(I) )
%
%  Modified:
%
%    15 March 2000
%
%  Reference:
%
%    Arthur Stroud and Don Secrest,
%    Gaussian Quadrature Formulas,
%    Prentice Hall, 1966.
%
%  Parameters:
%
%    Input, integer NORDER, the order of the quadrature rule to be computed.
%    NORDER must be at least 1.
%
%    Output, real ( kind = 8 ) XTAB(NORDER), the Gauss-Laguerre abscissas.
%
%    Output, real ( kind = 8 ) WEIGHT(NORDER), the Gauss-Laguerre weights.
%
%    Input, real ( kind = 8 ) ALPHA, the exponent of the X factor.
%    Set ALPHA = 0.0D+00 for the simplest rule.
%    ALPHA must be nonnegative.
%
persistent b c cc dp2 i p1 r1 r2 ratio x ; 

if isempty(b), b=zeros(1,norder); end;
if isempty(c), c=zeros(1,norder); end;
if isempty(cc), cc=0; end;
if isempty(dp2), dp2=0; end;
if isempty(i), i=0; end;
if isempty(p1), p1=0; end;
if isempty(r1), r1=0; end;
if isempty(r2), r2=0; end;
if isempty(ratio), ratio=0; end;
if isempty(x), x=0; end;
%
%  Set the recursion coefficients.
%
for i = 1: norder;
b(i) =( alpha + real( 2 .* i - 1) );
end; i = fix(norder+1);
for i = 1: norder;
c(i) = real( i - 1) .*( alpha + real( i - 1) );
end; i = fix(norder+1);
cc = gamma( alpha + 1.0d+00 ) .* prod( c([2:norder]) );
for i = 1: norder;
%
%  Compute an estimate for the root.
%
if( i == 1 )
x =( 1.0d+00 + alpha ) .*( 3.0d+00+ 0.92 .* alpha ) ./( 1.0d+00 + 2.4d+00 .* real( norder) + 1.8d+00 .* alpha );
elseif( i == 2 ) ;
x = x +( 15.0d+00 + 6.25d+00 .* alpha ) ./( 1.0d+00 + 0.9d+00 .* alpha + 2.5d+00 .* real( norder) );
else;
r1 =( 1.0d+00 + 2.55d+00 .* real( i - 2) )./( 1.9d+00 .* real( i - 2) );
r2 = 1.26d+00 .* real( i - 2) .* alpha ./( 1.0d+00 + 3.5d+00 .* real( i - 2) );
ratio =( r1 + r2 ) ./( 1.0d+00 + 0.3d+00 .* alpha );
x = x + ratio .*( x - xtab(i-2) );
end;
%
%  use iteration to find the root.
%
[ x, norder, alpha, dp2, p1, b, c ]=laguerre_root( x, norder, alpha, dp2, p1, b, c );
%
%  Set the abscissa and weight.
%
xtab(i) = x;
weight(i) =( cc ./ dp2 ) ./ p1;
end; i = fix(norder+1);
return;
end
function [p2, dp2, p1, x, norder, alpha, b, c]=laguerre_recur( p2, dp2, p1, x, norder, alpha, b, c );
%*******************************************************************************
%
%! LAGUERRE_RECUR finds the value and derivative of a Laguerre polynomial.
%
%  Modified:
%
%    19 September 1998
%
%  Reference:
%
%    Arthur Stroud and Don Secrest,
%    Gaussian Quadrature Formulas,
%    Prentice Hall, 1966.
%
%  Parameters:
%
%    Output, real ( kind = 8 ) P2, the value of L(NORDER,X).
%
%    Output, real ( kind = 8 ) DP2, the value of L'(NORDER,X).
%
%    Output, real ( kind = 8 ) P1, the value of L(NORDER-1,X).
%
%    Input, real ( kind = 8 ) X, the point at which polynomials are evaluated.
%
%    Input, integer NORDER, the order of the polynomial to be computed.
%
%    Input, real ( kind = 8 ) ALPHA, the exponent of the X factor in the
%    integrand.
%
%    Input, real ( kind = 8 ) B(NORDER), C(NORDER), the recursion
%    coefficients.
%
persistent dp0 dp1 i p0 ; 

if isempty(dp0), dp0=0; end;
if isempty(dp1), dp1=0; end;
if isempty(i), i=0; end;
if isempty(p0), p0=0; end;
p1 = 1.0d+00;
dp1 = 0.0d+00;
p2 = x - alpha - 1.0d+00;
dp2 = 1.0d+00;
for i = 2: norder;
p0 = p1;
dp0 = dp1;
p1 = p2;
dp1 = dp2;
p2 =( x - b(i) ) .* p1 - c(i) .* p0;
dp2 =( x - b(i) ) .* dp1 + p1 - c(i) .* dp0;
end; i = fix(norder+1);
return;
end
function [x, norder, alpha, dp2, p1, b, c]=laguerre_root( x, norder, alpha, dp2, p1, b, c );
%*******************************************************************************
%
%! LAGUERRE_ROOT improves an approximate root of a Laguerre polynomial.
%
%  Modified:
%
%    09 December 2000
%
%  Reference:
%
%    Arthur Stroud and Don Secrest,
%    Gaussian Quadrature Formulas,
%    Prentice Hall, 1966.
%
%  Parameters:
%
%    Input/output, real ( kind = 8 ) X, the approximate root, which
%    should be improved on output.
%
%    Input, integer NORDER, the order of the polynomial to be computed.
%
%    Input, real ( kind = 8 ) ALPHA, the exponent of the X factor.
%
%    Output, real ( kind = 8 ) DP2, the value of L'(NORDER,X).
%
%    Output, real ( kind = 8 ) P1, the value of L(NORDER-1,X).
%
%    Input, real ( kind = 8 ) B(NORDER), C(NORDER), the recursion coefficients.
%
persistent d eps i maxstep p2 ; 

if isempty(d), d=0; end;
if isempty(eps), eps=0; end;
if isempty(i), i=0; end;
if isempty(maxstep), maxstep = 10; end;
if isempty(p2), p2=0; end;
eps = eps;
for i = 1: maxstep;
[ p2, dp2, p1, x, norder, alpha, b, c ]=laguerre_recur( p2, dp2, p1, x, norder, alpha, b, c );
d = p2 ./ dp2;
x = x - d;
if( abs( d ) <= eps .*( abs( x ) + 1.0d+00 ) )
return;
end;
end; i = fix(maxstep+1);
return;
end
function [norder, xtab, weight]=laguerre_set( norder, xtab, weight );
%*******************************************************************************
%
%! LAGUERRE_SET sets abscissas and weights for Laguerre quadrature.
%
%  Integration interval:
%
%    [ 0, +Infinity )
%
%  Weight function:
%
%    EXP ( - X )
%
%  Integral to approximate:
%
%    Integral ( 0 <= X < +INFINITY ) EXP ( - X ) * F(X) dX
%
%  Approximate integral:
%
%    Sum ( 1 <= I <= NORDER ) WEIGHT(I) * F ( XTAB(I) )
%
%  Note:
%
%    The abscissas are the zeroes of the Laguerre polynomial L(NORDER,X).
%
%  Modified:
%
%    17 September 1998
%
%  Author:
%
%    John Burkardt
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
%  Parameters:
%
%    Input, integer NORDER, the order of the rule.
%    NORDER must be between 1 and 20.
%
%    Output, real ( kind = 8 ) XTAB(NORDER), the abscissas of the rule.
%
%    Output, real ( kind = 8 ) WEIGHT(NORDER), the weights of the rule.
%    The weights are positive, and should add to 1.
%

if( norder == 1 )
xtab(1) =    1.0d+00;
weight(1) =  1.0d+00;
elseif( norder == 2 ) ;
xtab(1) =    0.585786437626904951198311275790d+00;
xtab(2) =    0.341421356237309504880168872421d+01;
weight(1) =  0.853553390593273762200422181052d+00;
weight(2) =  0.146446609406726237799577818948d+00;
elseif( norder == 3 ) ;
xtab(1) =    0.415774556783479083311533873128d+00;
xtab(2) =    0.229428036027904171982205036136d+01;
xtab(3) =    0.628994508293747919686641576551d+01;
weight(1) =  0.711093009929173015449590191143d+00;
weight(2) =  0.278517733569240848801444888457d+00;
weight(3) =  0.103892565015861357489649204007d-01;
elseif( norder == 4 ) ;
xtab(1) =    0.322547689619392311800361943361d+00;
xtab(2) =    0.174576110115834657568681671252d+01;
xtab(3) =    0.453662029692112798327928538496d+01;
xtab(4) =    0.939507091230113312923353644342d+01;
weight(1) =  0.603154104341633601635966023818d+00;
weight(2) =  0.357418692437799686641492017458d+00;
weight(3) =  0.388879085150053842724381681562d-01;
weight(4) =  0.539294705561327450103790567621d-03;
elseif( norder == 5 ) ;
xtab(1) =    0.263560319718140910203061943361d+00;
xtab(2) =    0.141340305910651679221840798019d+01;
xtab(3) =    0.359642577104072208122318658878d+01;
xtab(4) =    0.708581000585883755692212418111d+01;
xtab(5) =    0.126408008442757826594332193066d+02;
weight(1) =  0.521755610582808652475860928792d+00;
weight(2) =  0.398666811083175927454133348144d+00;
weight(3) =  0.759424496817075953876533114055d-01;
weight(4) =  0.361175867992204845446126257304d-02;
weight(5) =  0.233699723857762278911490845516d-04;
elseif( norder == 6 ) ;
xtab(1) =    0.222846604179260689464354826787d+00;
xtab(2) =    0.118893210167262303074315092194d+01;
xtab(3) =    0.299273632605931407769132528451d+01;
xtab(4) =    0.577514356910451050183983036943d+01;
xtab(5) =    0.983746741838258991771554702994d+01;
xtab(6) =    0.159828739806017017825457915674d+02;
weight(1) =  0.458964673949963593568284877709d+00;
weight(2) =  0.417000830772120994113377566193d+00;
weight(3) =  0.113373382074044975738706185098d+00;
weight(4) =  0.103991974531490748989133028469d-01;
weight(5) =  0.261017202814932059479242860001d-03;
weight(6) =  0.898547906429621238825292052825d-06;
elseif( norder == 7 ) ;
xtab(1) =    0.193043676560362413838247885004d+00;
xtab(2) =    0.102666489533919195034519944317d+01;
xtab(3) =    0.256787674495074620690778622666d+01;
xtab(4) =    0.490035308452648456810171437810d+01;
xtab(5) =    0.818215344456286079108182755123d+01;
xtab(6) =    0.127341802917978137580126424582d+02;
xtab(7) =    0.193957278622625403117125820576d+02;
weight(1) =  0.409318951701273902130432880018d+00;
weight(2) =  0.421831277861719779929281005417d+00;
weight(3) =  0.147126348657505278395374184637d+00;
weight(4) =  0.206335144687169398657056149642d-01;
weight(5) =  0.107401014328074552213195962843d-02;
weight(6) =  0.158654643485642012687326223234d-04;
weight(7) =  0.317031547899558056227132215385d-07;
elseif( norder == 8 ) ;
xtab(1) =    0.170279632305100999788861856608d+00;
xtab(2) =    0.903701776799379912186020223555d+00;
xtab(3) =    0.225108662986613068930711836697d+01;
xtab(4) =    0.426670017028765879364942182690d+01;
xtab(5) =    0.704590540239346569727932548212d+01;
xtab(6) =    0.107585160101809952240599567880d+02;
xtab(7) =    0.157406786412780045780287611584d+02;
xtab(8) =    0.228631317368892641057005342974d+02;
weight(1) =  0.369188589341637529920582839376d+00;
weight(2) =  0.418786780814342956076978581333d+00;
weight(3) =  0.175794986637171805699659866777d+00;
weight(4) =  0.333434922612156515221325349344d-01;
weight(5) =  0.279453623522567252493892414793d-02;
weight(6) =  0.907650877335821310423850149336d-04;
weight(7) =  0.848574671627253154486801830893d-06;
weight(8) =  0.104800117487151038161508853552d-08;
elseif( norder == 9 ) ;
xtab(1) =    0.152322227731808247428107073127d+00;
xtab(2) =    0.807220022742255847741419210952d+00;
xtab(3) =    0.200513515561934712298303324701d+01;
xtab(4) =    0.378347397333123299167540609364d+01;
xtab(5) =    0.620495677787661260697353521006d+01;
xtab(6) =    0.937298525168757620180971073215d+01;
xtab(7) =    0.134662369110920935710978818397d+02;
xtab(8) =    0.188335977889916966141498992996d+02;
xtab(9) =    0.263740718909273767961410072937d+02;
weight(1) =  0.336126421797962519673467717606d+00;
weight(2) =  0.411213980423984387309146942793d+00;
weight(3) =  0.199287525370885580860575607212d+00;
weight(4) =  0.474605627656515992621163600479d-01;
weight(5) =  0.559962661079458317700419900556d-02;
weight(6) =  0.305249767093210566305412824291d-03;
weight(7) =  0.659212302607535239225572284875d-05;
weight(8) =  0.411076933034954844290241040330d-07;
weight(9) =  0.329087403035070757646681380323d-10;
elseif( norder == 10 ) ;
xtab(1) =    0.137793470540492430830772505653d+00;
xtab(2) =    0.729454549503170498160373121676d+00;
xtab(3) =    0.180834290174031604823292007575d+01;
xtab(4) =    0.340143369785489951448253222141d+01;
xtab(5) =    0.555249614006380363241755848687d+01;
xtab(6) =    0.833015274676449670023876719727d+01;
xtab(7) =    0.118437858379000655649185389191d+02;
xtab(8) =    0.162792578313781020995326539358d+02;
xtab(9) =    0.219965858119807619512770901956d+02;
xtab(10) =   0.299206970122738915599087933408d+02;
weight(1) =  0.308441115765020141547470834678d+00;
weight(2) =  0.401119929155273551515780309913d+00;
weight(3) =  0.218068287611809421588648523475d+00;
weight(4) =  0.620874560986777473929021293135d-01;
weight(5) =  0.950151697518110055383907219417d-02;
weight(6) =  0.753008388587538775455964353676d-03;
weight(7) =  0.282592334959956556742256382685d-04;
weight(8) =  0.424931398496268637258657665975d-06;
weight(9) =  0.183956482397963078092153522436d-08;
weight(10) = 0.991182721960900855837754728324d-12;
elseif( norder == 11 ) ;
xtab(1) =    0.125796442187967522675794577516d+00;
xtab(2) =    0.665418255839227841678127839420d+00;
xtab(3) =    0.164715054587216930958700321365d+01;
xtab(4) =    0.309113814303525495330195934259d+01;
xtab(5) =    0.502928440157983321236999508366d+01;
xtab(6) =    0.750988786380661681941099714450d+01;
xtab(7) =    0.106059509995469677805559216457d+02;
xtab(8) =    0.144316137580641855353200450349d+02;
xtab(9) =    0.191788574032146786478174853989d+02;
xtab(10) =   0.252177093396775611040909447797d+02;
xtab(11) =   0.334971928471755372731917259395d+02;
weight(1) =  0.284933212894200605056051024724d+00;
weight(2) =  0.389720889527849377937553508048d+00;
weight(3) =  0.232781831848991333940223795543d+00;
weight(4) =  0.765644535461966864008541790132d-01;
weight(5) =  0.143932827673506950918639187409d-01;
weight(6) =  0.151888084648487306984777640042d-02;
weight(7) =  0.851312243547192259720424170600d-04;
weight(8) =  0.229240387957450407857683270709d-05;
weight(9) =  0.248635370276779587373391491114d-07;
weight(10) = 0.771262693369132047028152590222d-10;
weight(11) = 0.288377586832362386159777761217d-13;
elseif( norder == 12 ) ;
xtab(1) =    0.115722117358020675267196428240d+00;
xtab(2) =    0.611757484515130665391630053042d+00;
xtab(3) =    0.151261026977641878678173792687d+01;
xtab(4) =    0.283375133774350722862747177657d+01;
xtab(5) =    0.459922763941834848460572922485d+01;
xtab(6) =    0.684452545311517734775433041849d+01;
xtab(7) =    0.962131684245686704391238234923d+01;
xtab(8) =    0.130060549933063477203460524294d+02;
xtab(9) =    0.171168551874622557281840528008d+02;
xtab(10) =   0.221510903793970056699218950837d+02;
xtab(11) =   0.284879672509840003125686072325d+02;
xtab(12) =   0.370991210444669203366389142764d+02;
weight(1) =  0.264731371055443190349738892056d+00;
weight(2) =  0.377759275873137982024490556707d+00;
weight(3) =  0.244082011319877564254870818274d+00;
weight(4) =  0.904492222116809307275054934667d-01;
weight(5) =  0.201023811546340965226612867827d-01;
weight(6) =  0.266397354186531588105415760678d-02;
weight(7) =  0.203231592662999392121432860438d-03;
weight(8) =  0.836505585681979874533632766396d-05;
weight(9) =  0.166849387654091026116989532619d-06;
weight(10) = 0.134239103051500414552392025055d-08;
weight(11) = 0.306160163503502078142407718971d-11;
weight(12) = 0.814807746742624168247311868103d-15;
elseif( norder == 13 ) ;
xtab(1) =    0.107142388472252310648493376977d+00;
xtab(2) =    0.566131899040401853406036347177d+00;
xtab(3) =    0.139856433645101971792750259921d+01;
xtab(4) =    0.261659710840641129808364008472d+01;
xtab(5) =    0.423884592901703327937303389926d+01;
xtab(6) =    0.629225627114007378039376523025d+01;
xtab(7) =    0.881500194118697804733348868036d+01;
xtab(8) =    0.118614035888112425762212021880d+02;
xtab(9) =    0.155107620377037527818478532958d+02;
xtab(10) =   0.198846356638802283332036594634d+02;
xtab(11) =   0.251852638646777580842970297823d+02;
xtab(12) =   0.318003863019472683713663283526d+02;
xtab(13) =   0.407230086692655795658979667001d+02;
weight(1) =  0.247188708429962621346249185964d+00;
weight(2) =  0.365688822900521945306717530893d+00;
weight(3) =  0.252562420057658502356824288815d+00;
weight(4) =  0.103470758024183705114218631672d+00;
weight(5) =  0.264327544155616157781587735702d-01;
weight(6) =  0.422039604025475276555209292644d-02;
weight(7) =  0.411881770472734774892472527082d-03;
weight(8) =  0.235154739815532386882897300772d-04;
weight(9) =  0.731731162024909910401047197761d-06;
weight(10) = 0.110884162570398067979150974759d-07;
weight(11) = 0.677082669220589884064621459082d-10;
weight(12) = 0.115997995990507606094507145382d-12;
weight(13) = 0.224509320389275841599187226865d-16;
elseif( norder == 14 ) ;
xtab(1) =    0.997475070325975745736829452514d-01;
xtab(2) =    0.526857648851902896404583451502d+00;
xtab(3) =    0.130062912125149648170842022116d+01;
xtab(4) =    0.243080107873084463616999751038d+01;
xtab(5) =    0.393210282229321888213134366778d+01;
xtab(6) =    0.582553621830170841933899983898d+01;
xtab(7) =    0.814024014156514503005978046052d+01;
xtab(8) =    0.109164995073660188408130510904d+02;
xtab(9) =    0.142108050111612886831059780825d+02;
xtab(10) =   0.181048922202180984125546272083d+02;
xtab(11) =   0.227233816282696248232280886985d+02;
xtab(12) =   0.282729817232482056954158923218d+02;
xtab(13) =   0.351494436605924265828643121364d+02;
xtab(14) =   0.443660817111174230416312423666d+02;
weight(1) =  0.231815577144864977840774861104d+00;
weight(2) =  0.353784691597543151802331301273d+00;
weight(3) =  0.258734610245428085987320561144d+00;
weight(4) =  0.115482893556923210087304988673d+00;
weight(5) =  0.331920921593373600387499587137d-01;
weight(6) =  0.619286943700661021678785967675d-02;
weight(7) =  0.739890377867385942425890907080d-03;
weight(8) =  0.549071946684169837857331777667d-04;
weight(9) =  0.240958576408537749675775256553d-05;
weight(10) = 0.580154398167649518088619303904d-07;
weight(11) = 0.681931469248497411961562387084d-09;
weight(12) = 0.322120775189484793980885399656d-11;
weight(13) = 0.422135244051658735159797335643d-14;
weight(14) = 0.605237502228918880839870806281d-18;
elseif( norder == 15 ) ;
xtab(1) =    0.933078120172818047629030383672d-01;
xtab(2) =    0.492691740301883908960101791412d+00;
xtab(3) =    0.121559541207094946372992716488d+01;
xtab(4) =    0.226994952620374320247421741375d+01;
xtab(5) =    0.366762272175143727724905959436d+01;
xtab(6) =    0.542533662741355316534358132596d+01;
xtab(7) =    0.756591622661306786049739555812d+01;
xtab(8) =    0.101202285680191127347927394568d+02;
xtab(9) =    0.131302824821757235640991204176d+02;
xtab(10) =   0.166544077083299578225202408430d+02;
xtab(11) =   0.207764788994487667729157175676d+02;
xtab(12) =   0.256238942267287801445868285977d+02;
xtab(13) =   0.314075191697539385152432196202d+02;
xtab(14) =   0.385306833064860094162515167595d+02;
xtab(15) =   0.480260855726857943465734308508d+02;
weight(1) =  0.218234885940086889856413236448d+00;
weight(2) =  0.342210177922883329638948956807d+00;
weight(3) =  0.263027577941680097414812275022d+00;
weight(4) =  0.126425818105930535843030549378d+00;
weight(5) =  0.402068649210009148415854789871d-01;
weight(6) =  0.856387780361183836391575987649d-02;
weight(7) =  0.121243614721425207621920522467d-02;
weight(8) =  0.111674392344251941992578595518d-03;
weight(9) =  0.645992676202290092465319025312d-05;
weight(10) = 0.222631690709627263033182809179d-06;
weight(11) = 0.422743038497936500735127949331d-08;
weight(12) = 0.392189726704108929038460981949d-10;
weight(13) = 0.145651526407312640633273963455d-12;
weight(14) = 0.148302705111330133546164737187d-15;
weight(15) = 0.160059490621113323104997812370d-19;
elseif( norder == 16 ) ;
xtab(1) =    0.876494104789278403601980973401d-01;
xtab(2) =    0.462696328915080831880838260664d+00;
xtab(3) =    0.114105777483122685687794501811d+01;
xtab(4) =    0.212928364509838061632615907066d+01;
xtab(5) =    0.343708663389320664523510701675d+01;
xtab(6) =    0.507801861454976791292305830814d+01;
xtab(7) =    0.707033853504823413039598947080d+01;
xtab(8) =    0.943831433639193878394724672911d+01;
xtab(9) =    0.122142233688661587369391246088d+02;
xtab(10) =   0.154415273687816170767647741622d+02;
xtab(11) =   0.191801568567531348546631409497d+02;
xtab(12) =   0.235159056939919085318231872752d+02;
xtab(13) =   0.285787297428821403675206137099d+02;
xtab(14) =   0.345833987022866258145276871778d+02;
xtab(15) =   0.419404526476883326354722330252d+02;
xtab(16) =   0.517011603395433183643426971197d+02;
weight(1) =  0.206151714957800994334273636741d+00;
weight(2) =  0.331057854950884165992983098710d+00;
weight(3) =  0.265795777644214152599502020650d+00;
weight(4) =  0.136296934296377539975547513526d+00;
weight(5) =  0.473289286941252189780623392781d-01;
weight(6) =  0.112999000803394532312490459701d-01;
weight(7) =  0.184907094352631086429176783252d-02;
weight(8) =  0.204271915308278460126018338421d-03;
weight(9) =  0.148445868739812987713515067551d-04;
weight(10) = 0.682831933087119956439559590327d-06;
weight(11) = 0.188102484107967321388159920418d-07;
weight(12) = 0.286235024297388161963062629156d-09;
weight(13) = 0.212707903322410296739033610978d-11;
weight(14) = 0.629796700251786778717446214552d-14;
weight(15) = 0.505047370003551282040213233303d-17;
weight(16) = 0.416146237037285519042648356116d-21;
elseif( norder == 17 ) ;
xtab(1) =    0.826382147089476690543986151980d-01;
xtab(2) =    0.436150323558710436375959029847d+00;
xtab(3) =    0.107517657751142857732980316755d+01;
xtab(4) =    0.200519353164923224070293371933d+01;
xtab(5) =    0.323425612404744376157380120696d+01;
xtab(6) =    0.477351351370019726480932076262d+01;
xtab(7) =    0.663782920536495266541643929703d+01;
xtab(8) =    0.884668551116980005369470571184d+01;
xtab(9) =    0.114255293193733525869726151469d+02;
xtab(10) =   0.144078230374813180021982874959d+02;
xtab(11) =   0.178382847307011409290658752412d+02;
xtab(12) =   0.217782682577222653261749080522d+02;
xtab(13) =   0.263153178112487997766149598369d+02;
xtab(14) =   0.315817716804567331343908517497d+02;
xtab(15) =   0.377960938374771007286092846663d+02;
xtab(16) =   0.453757165339889661829258363215d+02;
xtab(17) =   0.553897517898396106640900199790d+02;
weight(1) =  0.195332205251770832145927297697d+00;
weight(2) =  0.320375357274540281336625631970d+00;
weight(3) =  0.267329726357171097238809604160d+00;
weight(4) =  0.145129854358758625407426447473d+00;
weight(5) =  0.544369432453384577793805803066d-01;
weight(6) =  0.143572977660618672917767247431d-01;
weight(7) =  0.266282473557277256843236250006d-02;
weight(8) =  0.343679727156299920611775097985d-03;
weight(9) =  0.302755178378287010943703641131d-04;
weight(10) = 0.176851505323167689538081156159d-05;
weight(11) = 0.657627288681043332199222748162d-07;
weight(12) = 0.146973093215954679034375821888d-08;
weight(13) = 0.181691036255544979555476861323d-10;
weight(14) = 0.109540138892868740297645078918d-12;
weight(15) = 0.261737388222337042155132062413d-15;
weight(16) = 0.167293569314615469085022374652d-18;
weight(17) = 0.106562631627404278815253271162d-22;
elseif( norder == 18 ) ;
xtab(1) =    0.781691666697054712986747615334d-01;
xtab(2) =    0.412490085259129291039101536536d+00;
xtab(3) =    0.101652017962353968919093686187d+01;
xtab(4) =    0.189488850996976091426727831954d+01;
xtab(5) =    0.305435311320265975115241130719d+01;
xtab(6) =    0.450420553888989282633795571455d+01;
xtab(7) =    0.625672507394911145274209116326d+01;
xtab(8) =    0.832782515660563002170470261564d+01;
xtab(9) =    0.107379900477576093352179033397d+02;
xtab(10) =   0.135136562075550898190863812108d+02;
xtab(11) =   0.166893062819301059378183984163d+02;
xtab(12) =   0.203107676262677428561313764553d+02;
xtab(13) =   0.244406813592837027656442257980d+02;
xtab(14) =   0.291682086625796161312980677805d+02;
xtab(15) =   0.346279270656601721454012429438d+02;
xtab(16) =   0.410418167728087581392948614284d+02;
xtab(17) =   0.488339227160865227486586093290d+02;
xtab(18) =   0.590905464359012507037157810181d+02;
weight(1) =  0.185588603146918805623337752284d+00;
weight(2) =  0.310181766370225293649597595713d+00;
weight(3) =  0.267866567148536354820854394783d+00;
weight(4) =  0.152979747468074906553843082053d+00;
weight(5) =  0.614349178609616527076780103487d-01;
weight(6) =  0.176872130807729312772600233761d-01;
weight(7) =  0.366017976775991779802657207890d-02;
weight(8) =  0.540622787007735323128416319257d-03;
weight(9) =  0.561696505121423113817929049294d-04;
weight(10) = 0.401530788370115755858883625279d-05;
weight(11) = 0.191466985667567497969210011321d-06;
weight(12) = 0.583609526863159412918086289717d-08;
weight(13) = 0.107171126695539012772851317562d-09;
weight(14) = 0.108909871388883385562011298291d-11;
weight(15) = 0.538666474837830887608094323164d-14;
weight(16) = 0.104986597803570340877859934846d-16;
weight(17) = 0.540539845163105364356554467358d-20;
weight(18) = 0.269165326920102862708377715980d-24;
elseif( norder == 19 ) ;
xtab(1) =    0.741587837572050877131369916024d-01;
xtab(2) =    0.391268613319994607337648350299d+00;
xtab(3) =    0.963957343997958058624879377130d+00;
xtab(4) =    0.179617558206832812557725825252d+01;
xtab(5) =    0.289365138187378399116494713237d+01;
xtab(6) =    0.426421553962776647436040018167d+01;
xtab(7) =    0.591814156164404855815360191408d+01;
xtab(8) =    0.786861891533473373105668358176d+01;
xtab(9) =    0.101324237168152659251627415800d+02;
xtab(10) =   0.127308814638423980045092979656d+02;
xtab(11) =   0.156912783398358885454136069861d+02;
xtab(12) =   0.190489932098235501532136429732d+02;
xtab(13) =   0.228508497608294829323930586693d+02;
xtab(14) =   0.271606693274114488789963947149d+02;
xtab(15) =   0.320691222518622423224362865906d+02;
xtab(16) =   0.377129058012196494770647508283d+02;
xtab(17) =   0.443173627958314961196067736013d+02;
xtab(18) =   0.523129024574043831658644222420d+02;
xtab(19) =   0.628024231535003758413504690673d+02;
weight(1) =  0.176768474915912502251035479815d+00;
weight(2) =  0.300478143607254379482156807712d+00;
weight(3) =  0.267599547038175030772695440648d+00;
weight(4) =  0.159913372135580216785512147895d+00;
weight(5) =  0.682493799761491134552355368344d-01;
weight(6) =  0.212393076065443249244062193091d-01;
weight(7) =  0.484162735114839596725013121019d-02;
weight(8) =  0.804912747381366766594647138204d-03;
weight(9) =  0.965247209315350170843161738801d-04;
weight(10) = 0.820730525805103054408982992869d-05;
weight(11) = 0.483056672473077253944806671560d-06;
weight(12) = 0.190499136112328569993615674552d-07;
weight(13) = 0.481668463092806155766936380273d-09;
weight(14) = 0.734825883955114437684376840171d-11;
weight(15) = 0.620227538757261639893719012423d-13;
weight(16) = 0.254143084301542272371866857954d-15;
weight(17) = 0.407886129682571235007187465134d-18;
weight(18) = 0.170775018759383706100412325084d-21;
weight(19) = 0.671506464990818995998969111749d-26;
elseif( norder == 20 ) ;
xtab(1) =    0.705398896919887533666890045842d-01;
xtab(2) =    0.372126818001611443794241388761d+00;
xtab(3) =    0.916582102483273564667716277074d+00;
xtab(4) =    0.170730653102834388068768966741d+01;
xtab(5) =    0.274919925530943212964503046049d+01;
xtab(6) =    0.404892531385088692237495336913d+01;
xtab(7) =    0.561517497086161651410453988565d+01;
xtab(8) =    0.745901745367106330976886021837d+01;
xtab(9) =    0.959439286958109677247367273428d+01;
xtab(10) =   0.120388025469643163096234092989d+02;
xtab(11) =   0.148142934426307399785126797100d+02;
xtab(12) =   0.179488955205193760173657909926d+02;
xtab(13) =   0.214787882402850109757351703696d+02;
xtab(14) =   0.254517027931869055035186774846d+02;
xtab(15) =   0.299325546317006120067136561352d+02;
xtab(16) =   0.350134342404790000062849359067d+02;
xtab(17) =   0.408330570567285710620295677078d+02;
xtab(18) =   0.476199940473465021399416271529d+02;
xtab(19) =   0.558107957500638988907507734445d+02;
xtab(20) =   0.665244165256157538186403187915d+02;
weight(1) =  0.168746801851113862149223899689d+00;
weight(2) =  0.291254362006068281716795323812d+00;
weight(3) =  0.266686102867001288549520868998d+00;
weight(4) =  0.166002453269506840031469127816d+00;
weight(5) =  0.748260646687923705400624639615d-01;
weight(6) =  0.249644173092832210728227383234d-01;
weight(7) =  0.620255084457223684744754785395d-02;
weight(8) =  0.114496238647690824203955356969d-02;
weight(9) =  0.155741773027811974779809513214d-03;
weight(10) = 0.154014408652249156893806714048d-04;
weight(11) = 0.108648636651798235147970004439d-05;
weight(12) = 0.533012090955671475092780244305d-07;
weight(13) = 0.175798117905058200357787637840d-08;
weight(14) = 0.372550240251232087262924585338d-10;
weight(15) = 0.476752925157819052449488071613d-12;
weight(16) = 0.337284424336243841236506064991d-14;
weight(17) = 0.115501433950039883096396247181d-16;
weight(18) = 0.153952214058234355346383319667d-19;
weight(19) = 0.528644272556915782880273587683d-23;
weight(20) = 0.165645661249902329590781908529d-27;
else;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'LAGUERRE_SET - Fatal error!');
writef(1,['%s','%6i','\n'], '  Illegal value of NORDER = ', norder);
writef(1,['%s','\n'], '  Legal values are 1 to 20.');
error(['stop encountered in original fortran code  ',char(10),';']);
end;
return;
end
function [func, a, norder, xtab, weight, result]=laguerre_sum( func, a, norder, xtab, weight, result );
%*******************************************************************************
%
%! LAGUERRE_SUM carries out Laguerre quadrature over [ A, +Infinity ).
%
%  Discussion:
%
%    The simplest Laguerre integral to approximate is the
%    integral from 0 to INFINITY of EXP(-X) * F(X).  When this is so,
%    it is easy to modify the rule to approximate the integral from
%    A to INFINITY as well.
%
%    Another common Laguerre integral to approximate is the
%    integral from 0 to Infinity of EXP(-X) * X**ALPHA * F(X).
%    This routine may be used to sum up the terms of the Laguerre
%    rule for such an integral as well.  However, if ALPHA is nonzero,
%    then there is no simple way to extend the rule to approximate the
%    integral from A to INFINITY.  The simplest procedures would be
%    to approximate the integral from 0 to A.
%
%  Integration interval:
%
%    [ A, +Infinity ) or [ 0, +Infinity )
%
%  Weight function:
%
%    EXP ( - X ) or EXP ( - X ) * X**ALPHA
%
%  Integral to approximate:
%
%    Integral ( A <= X <= +Infinity ) EXP ( -X ) * F(X) dX or
%    Integral ( 0 <= X <= +Infinity ) EXP ( -X ) * X**ALPHA * F(X) dX
%
%  Approximate integral:
%
%    EXP ( - A ) * Sum ( 1 <= I <= NORDER ) WEIGHT(I) * F ( XTAB(I) + A )
%
%    or
%
%    Sum ( 1 <= I <= NORDER ) WEIGHT(I) * F ( XTAB(I) )
%
%  Modified:
%
%    15 March 2000
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Abramowitz and Stegun,
%    Handbook of Mathematical Functions,
%    National Bureau of Standards, 1964.
%
%    Daniel Zwillinger, editor,
%    Standard Mathematical Tables and Formulae,
%    30th Edition,
%    CRC Press, 1996.
%
%  Parameters:
%
%    Input, external FUNC, the name of the FORTRAN function which
%    evaluates the integrand.  The function must have the form
%      function func ( x ).
%
%    Input, real ( kind = 8 ) A, the beginning of the integration interval.
%
%    Input, integer NORDER, the order of the rule.
%
%    Input, real ( kind = 8 ) XTAB(NORDER), the abscissas of the rule.
%
%    Input, real ( kind = 8 ) WEIGHT(NORDER), the weights of the rule.
%
%    Output, real ( kind = 8 ) RESULT, the approximate value of the integral.
%
persistent i ; 

if isempty(i), i=0; end;
if( norder < 1 )
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'LAGUERRE_SUM - Fatal error!');
writef(1,['%s','%6i','\n'], '  Nonpositive NORDER = ', norder);
error(['stop encountered in original fortran code  ',char(10),';']);
end;
result = 0.0d+00;
for i = 1: norder;
result = result + weight(i) .* func( xtab(i) + a );
end; i = fix(norder+1);
result = exp( - a ) .* result;
return;
end
function [norder, xtab, weight]=legendre_com( norder, xtab, weight );
%*******************************************************************************
%
%! LEGENDRE_COM computes abscissas and weights for Gauss-Legendre quadrature.
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
%    Integral ( -1 <= X <= 1 ) F(X) dX
%
%  Approximate integral:
%
%    Sum ( 1 <= I <= NORDER ) WEIGHT(I) * F ( XTAB(I) )
%
%  Modified:
%
%    16 September 1998
%
%  Parameters:
%
%    Input, integer NORDER, the order of the rule.
%    NORDER must be greater than 0.
%
%    Output, real ( kind = 8 ) XTAB(NORDER), the abscissas of the rule.
%
%    Output, real ( kind = 8 ) WEIGHT(NORDER), the weights of the rule.
%    The weights are positive, symmetric, and should sum to 2.
%
persistent d1 d2pn d3pn d4pn dp dpn e1 fx h i iback k m mp1mi ncopy nmove p pk pkm1 pkp1 t u v x0 xtemp ; 

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
if( norder < 1 )
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'LEGENDRE_COM - Fatal error!');
writef(1,['%s','%6i','\n'], '  Illegal value of NORDER = ', norder);
error(['stop encountered in original fortran code  ',char(10),';']);
end;
e1 = real( norder .*( norder + 1 ));
m =fix(fix(( norder + 1 ) ./ 2));
for i = 1:fix(( norder + 1 ) ./ 2);
mp1mi = fix(m + 1 - i);
t = real( 4 .* i - 1) .* d_pi( )./ real( 4 .* norder + 2);
x0 = cos(t) .*( 1.0d+00 -( 1.0d+00 - 1.0d+00./ real( norder) )./ real( 8 .* norder.^2) );
pkm1 = 1.0d+00;
pk = x0;
for k = 2: norder;
pkp1 = 2.0d+00 .* x0 .* pk - pkm1 -( x0 .* pk - pkm1 )./ real( k);
pkm1 = pk;
pk = pkp1;
end; k = fix(norder+1);
d1 = real( norder) .*( pkm1 - x0 .* pk );
dpn = d1 ./( 1.0d+00 - x0.^2 );
d2pn =( 2.0d+00 .* x0 .* dpn - e1 .* pk ) ./( 1.0d+00 - x0.^2 );
d3pn =( 4.0d+00 .* x0 .* d2pn +( 2.0d+00 - e1 ) .* dpn )./( 1.0d+00 - x0.^2 );
d4pn =( 6.0d+00 .* x0 .* d3pn +( 6.0d+00 - e1 ) .* d2pn ) ./( 1.0d+00 - x0.^2 );
u = pk ./ dpn;
v = d2pn ./ dpn;
%
%  Initial approximation H:
%
h = - u .*( 1.0d+00 + 0.5d+00 .* u .*( v + u .*( v.^2 - d3pn ./( 3.0d+00 .* dpn ) ) ) );
%
%  Refine H using one step of Newton's method:
%
p = pk + h .*( dpn + 0.5d+00 .* h .*( d2pn + h ./ 3.0d+00.*( d3pn + 0.25d+00 .* h .* d4pn ) ) );
dp = dpn + h .*( d2pn + 0.5d+00 .* h .*( d3pn + h .* d4pn ./ 3.0d+00 ) );
h = h - p ./ dp;
xtemp = x0 + h;
xtab(mp1mi) = xtemp;
fx = d1 - h .* e1 .*( pk + 0.5d+00 .* h .*( dpn + h ./ 3.0d+00.*( d2pn + 0.25d+00 .* h .*( d3pn + 0.2d+00 .* h .* d4pn ) ) ) );
weight(mp1mi) = 2.0d+00 .*( 1.0d+00 - xtemp.^2 ) ./ fx.^2;
end; i =fix(fix(( norder + 1 ) ./ 2)+1);
if( rem( norder, 2 ) == 1 )
xtab(1) = 0.0d+00;
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
function [p2, dp2, p1, x, norder]=legendre_recur( p2, dp2, p1, x, norder );
%*******************************************************************************
%
%! LEGENDRE_RECUR finds the value and derivative of a Legendre polynomial.
%
%  Modified:
%
%    19 September 1998
%
%  Reference:
%
%    Arthur Stroud and Don Secrest,
%    Gaussian Quadrature Formulas,
%    Prentice Hall, 1966.
%
%  Parameters:
%
%    Output, real ( kind = 8 ) P2, the value of P(NORDER,X).
%
%    Output, real ( kind = 8 ) DP2, the value of P'(NORDER,X).
%
%    Output, real ( kind = 8 ) P1, the value of P(NORDER-1,X).
%
%    Input, real ( kind = 8 ) X, the point at which polynomials are evaluated.
%
%    Input, integer NORDER, the order of the polynomial to be computed.
%
persistent dp0 dp1 i p0 ; 

if isempty(dp0), dp0=0; end;
if isempty(dp1), dp1=0; end;
if isempty(i), i=0; end;
if isempty(p0), p0=0; end;
p1 = 1.0d+00;
dp1 = 0.0d+00;
p2 = x;
dp2 = 1.0d+00;
for i = 2: norder;
p0 = p1;
dp0 = dp1;
p1 = p2;
dp1 = dp2;
p2 =( real( 2 .* i - 1) .* x .* p1+ real(   - i + 1)     .* p0 )./ real(     i);
dp2 =( real( 2 .* i - 1) .*( p1 + x .* dp1 )- real(     i - 1) .* dp0 )./ real(     i);
end; i = fix(norder+1);
return;
end
function [norder, xtab, weight]=legendre_set( norder, xtab, weight );
%*******************************************************************************
%
%! LEGENDRE_SET sets abscissas and weights for Gauss-Legendre quadrature.
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
%    Integral ( -1 <= X <= 1 ) F(X) dX
%
%  Approximate integral:
%
%    Sum ( 1 <= I <= NORDER ) WEIGHT(I) * F ( XTAB(I) )
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
%  Modified:
%
%    18 December 2000
%
%  Author:
%
%    John Burkardt
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
%  Parameters:
%
%    Input, integer NORDER, the order of the rule.
%    NORDER must be between 1 and 20, 32 or 64.
%
%    Output, real ( kind = 8 ) XTAB(NORDER), the abscissas of the rule.
%
%    Output, real ( kind = 8 ) WEIGHT(NORDER), the weights of the rule.
%    The weights are positive, symmetric and should sum to 2.
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
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'LEGENDRE_SET - Fatal error!');
writef(1,['%s','%6i','\n'], '  Illegal value of NORDER = ', norder);
writef(1,['%s','\n'], '  Legal values are 1 to 20, 32 or 64.');
error(['stop encountered in original fortran code  ',char(10),';']);
end;
return;
end
function [norder, xtab, weight]=legendre_set_cos( norder, xtab, weight );
%*******************************************************************************
%
%! LEGENDRE_SET_COS sets a Gauss-Legendre rule for COS(X) * F(X) on [-PI/2,PI/2].
%
%  Integration interval:
%
%    [ -PI/2, PI/2 ]
%
%  Weight function:
%
%    COS(X) * F(X)
%
%  Integral to approximate:
%
%    Integral ( -PI/2 <= X <= PI/2 ) COS(X) * F(X) dX
%
%  Approximate integral:
%
%    Sum ( 1 <= I <= NORDER ) WEIGHT(I) * F ( XTAB(I) )
%
%  Discussion:
%
%    The same rule can be used to approximate
%
%      Integral ( 0 <= X <= PI ) SIN(X) * F(X) dX
%
%    as
%
%      Sum ( 1 <= I <= NORDER ) WEIGHT(I) * F ( XTAB(I) + PI/2 )
%
%  Modified:
%
%    23 November 2000
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Gwynne Evans,
%    Practical Numerical Integration,
%    Wiley, 1993, QA299.3E93, page 310.
%
%  Parameters:
%
%    Input, integer NORDER, the order of the rule.
%    NORDER must be between 1, 2, 4, 8 or 16.
%
%    Output, real ( kind = 8 ) XTAB(NORDER), the abscissas of the rule.
%
%    Output, real ( kind = 8 ) WEIGHT(NORDER), the weights of the rule.
%

if( norder == 1 )
xtab(1) = 0.0d+00;
weight(1) = 2.0d+00;
elseif( norder == 2 ) ;
xtab(1) = - 0.68366739008990304094d+00;
xtab(2) =   0.68366739008990304094d+00;
weight(1) = 1.0d+00;
weight(2) = 1.0d+00;
elseif( norder == 4 ) ;
xtab(1) = - 1.1906765638948557415d+00;
xtab(2) = - 0.43928746686001514756d+00;
xtab(3) =   0.43928746686001514756d+00;
xtab(4) =   1.1906765638948557415d+00;
weight(1) = 0.22407061812762016065d+00;
weight(2) = 0.77592938187237983935d+00;
weight(3) = 0.77592938187237983935d+00;
weight(4) = 0.22407061812762016065d+00;
elseif( norder == 8 ) ;
xtab(1) = - 1.4414905401823575701d+00;
xtab(2) = - 1.1537256454567275850d+00;
xtab(3) = - 0.74346864787549244989d+00;
xtab(4) = - 0.25649650741623123020d+00;
xtab(5) =   0.25649650741623123020d+00;
xtab(6) =   0.74346864787549244989d+00;
xtab(7) =   1.1537256454567275850d+00;
xtab(8) =   1.4414905401823575701d+00;
weight(1) = 0.027535633513767011149d+00;
weight(2) = 0.14420409203022750950d+00;
weight(3) = 0.33626447785280459621d+00;
weight(4) = 0.49199579660320088314d+00;
weight(5) = 0.49199579660320088314d+00;
weight(6) = 0.33626447785280459621d+00;
weight(7) = 0.14420409203022750950d+00;
weight(8) = 0.027535633513767011149d+00;
elseif( norder == 16 ) ;
xtab( 1) = - 1.5327507132362304779d+00;
xtab( 2) = - 1.4446014873666514608d+00;
xtab( 3) = - 1.3097818904452936698d+00;
xtab( 4) = - 1.1330068786005003695d+00;
xtab( 5) = - 0.92027786206637096497d+00;
xtab( 6) = - 0.67861108097560545347d+00;
xtab( 7) = - 0.41577197673418943962d+00;
xtab( 8) = - 0.14003444424696773778d+00;
xtab( 9) =   0.14003444424696773778d+00;
xtab(10) =   0.41577197673418943962d+00;
xtab(11) =   0.67861108097560545347d+00;
xtab(12) =   0.92027786206637096497d+00;
xtab(13) =   1.1330068786005003695d+00;
xtab(14) =   1.3097818904452936698d+00;
xtab(15) =   1.4446014873666514608d+00;
xtab(16) =   1.5327507132362304779d+00;
weight( 1) = 0.0024194677567615628193d+00;
weight( 2) = 0.014115268156854008264d+00;
weight( 3) = 0.040437893946503669410d+00;
weight( 4) = 0.083026647573217742131d+00;
weight( 5) = 0.13834195526951273359d+00;
weight( 6) = 0.19741148870253455567d+00;
weight( 7) = 0.24763632094635522403d+00;
weight( 8) = 0.27661095764826050408d+00;
weight( 9) = 0.27661095764826050408d+00;
weight(10) = 0.24763632094635522403d+00;
weight(11) = 0.19741148870253455567d+00;
weight(12) = 0.13834195526951273359d+00;
weight(13) = 0.083026647573217742131d+00;
weight(14) = 0.040437893946503669410d+00;
weight(15) = 0.014115268156854008264d+00;
weight(16) = 0.0024194677567615628193d+00;
else;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'LEGENDRE_SET_COS - Fatal error!');
writef(1,['%s','%6i','\n'], '  Illegal value of NORDER = ', norder);
error(['stop encountered in original fortran code  ',char(10),';']);
end;
return;
end
function [norder, xtab, weight]=legendre_set_cos2( norder, xtab, weight );
%*******************************************************************************
%
%! LEGENDRE_SET_COS2 sets a Gauss-Legendre rule for COS(X) * F(X) on [0,PI/2].
%
%  Integration interval:
%
%    [ 0, PI/2 ]
%
%  Weight function:
%
%    COS(X) * F(X)
%
%  Integral to approximate:
%
%    Integral ( 0 <= X <= PI/2 ) COS(X) * F(X) dX
%
%  Approximate integral:
%
%    Sum ( 1 <= I <= NORDER ) WEIGHT(I) * F ( XTAB(I) )
%
%  Discussion:
%
%    The same rule can be used to approximate
%
%      Integral ( 0 <= X <= PI/2 ) SIN(X) * F(X) dX
%
%    as
%
%      Sum ( 1 <= I <= NORDER ) WEIGHT(I) * F ( PI/2 - XTAB(I) )
%
%  Modified:
%
%    24 November 2000
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Gwynne Evans,
%    Practical Numerical Integration,
%    Wiley, 1993, QA299.3E93, page 311.
%
%  Parameters:
%
%    Input, integer NORDER, the order of the rule.
%    NORDER must be between 2, 4, 8 or 16.
%
%    Output, real ( kind = 8 ) XTAB(NORDER), the abscissas of the rule.
%
%    Output, real ( kind = 8 ) WEIGHT(NORDER), the weights of the rule.
%

if( norder == 2 )
xtab(1) = 0.26587388056307823382d+00;
xtab(2) = 1.0351526093171315182d+00;
weight(1) = 0.60362553280827113087d+00;
weight(2) = 0.39637446719172886913d+00;
elseif( norder == 4 ) ;
xtab(1) = 0.095669389196858636773d+00;
xtab(2) = 0.45240902327067096554d+00;
xtab(3) = 0.93185057672024082424d+00;
xtab(4) = 1.3564439599666466230d+00;
weight( 1) = 0.23783071419515504517d+00;
weight( 2) = 0.40265695523581253512d+00;
weight( 3) = 0.28681737948564715225d+00;
weight( 4) = 0.072694951083385267446d+00;
elseif( norder == 8 ) ;
xtab(1) = 0.029023729768913933432d+00;
xtab(2) = 0.14828524404581819442d+00;
xtab(3) = 0.34531111151664787488d+00;
xtab(4) = 0.59447696797658360178d+00;
xtab(5) = 0.86538380686123504827d+00;
xtab(6) = 1.1263076093187456632d+00;
xtab(7) = 1.3470150460281258016d+00;
xtab(8) = 1.5015603622059195568d+00;
weight( 1) = 0.073908998095117384985d+00;
weight( 2) = 0.16002993702338006099d+00;
weight( 3) = 0.21444434341803549108d+00;
weight( 4) = 0.21979581268851903339d+00;
weight( 5) = 0.17581164478209568886d+00;
weight( 6) = 0.10560448025308322171d+00;
weight( 7) = 0.042485497299217201089d+00;
weight( 8) = 0.0079192864405519178899d+00;
elseif( norder == 16 ) ;
xtab( 1) = 0.0080145034906295973494d+00;
xtab( 2) = 0.041893031354246254797d+00;
xtab( 3) = 0.10149954486757579459d+00;
xtab( 4) = 0.18463185923836617507d+00;
xtab( 5) = 0.28826388487760574589d+00;
xtab( 6) = 0.40870579076464794191d+00;
xtab( 7) = 0.54176054986913847463d+00;
xtab( 8) = 0.68287636658719416893d+00;
xtab( 9) = 0.82729287620416833520d+00;
xtab(10) = 0.97018212594829367065d+00;
xtab(11) = 1.1067865150286247873d+00;
xtab(12) = 1.2325555697227748824d+00;
xtab(13) = 1.3432821921580721861d+00;
xtab(14) = 1.4352370549295032923d+00;
xtab(15) = 1.5052970876794669248d+00;
xtab(16) = 1.5510586944086135769d+00;
weight( 1) = 0.020528714977215248902d+00;
weight( 2) = 0.046990919853597958123d+00;
weight( 3) = 0.071441021312218541698d+00;
weight( 4) = 0.092350338329243052271d+00;
weight( 5) = 0.10804928026816236935d+00;
weight( 6) = 0.11698241243306261791d+00;
weight( 7) = 0.11812395361762037649d+00;
weight( 8) = 0.11137584940420091049d+00;
weight( 9) = 0.097778236145946543110d+00;
weight(10) = 0.079418758985944482077d+00;
weight(11) = 0.059039620053768691402d+00;
weight(12) = 0.039458876783728165671d+00;
weight(13) = 0.022987785677206847531d+00;
weight(14) = 0.011010405600421536861d+00;
weight(15) = 0.0038123928030499915653d+00;
weight(16) = 0.00065143375461266656171d+00;
else;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'LEGENDRE_SET_COS2 - Fatal error!');
writef(1,['%s','%6i','\n'], '  Illegal value of NORDER = ', norder);
error(['stop encountered in original fortran code  ',char(10),';']);
end;
return;
end
function [norder, xtab, weight]=legendre_set_log( norder, xtab, weight );
%*******************************************************************************
%
%! LEGENDRE_SET_LOG sets a Gauss-Legendre rule for - LOG(X) * F(X) on [0,1].
%
%  Integration interval:
%
%    [ 0, 1 ]
%
%  Weight function:
%
%    - LOG(X) * F(X)
%
%  Integral to approximate:
%
%    Integral ( 0 <= X <= 1 ) - LOG(X) * F(X) dX
%
%  Approximate integral:
%
%    Sum ( 1 <= I <= NORDER ) WEIGHT(I) * F ( XTAB(I) )
%
%  Modified:
%
%    05 December 2000
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Abramowitz and Stegun,
%    Handbook of Mathematical Functions,
%    National Bureau of Standards, 1964, page 920.
%
%    Gwynne Evans,
%    Practical Numerical Integration,
%    Wiley, 1993, QA299.3E93, page 309.
%
%    Arthur Stroud and Don Secrest,
%    Gaussian Quadrature Formulas,
%    Prentice Hall, 1966.
%
%  Parameters:
%
%    Input, integer NORDER, the order of the rule.
%    NORDER must be between 1 through 8, or 16.
%
%    Output, real ( kind = 8 ) XTAB(NORDER), the abscissas of the rule.
%
%    Output, real ( kind = 8 ) WEIGHT(NORDER), the weights of the rule.
%

if( norder == 1 )
xtab(1) = 0.25d+00;
weight(1) = 1.0d+00;
elseif( norder == 2 ) ;
xtab(1) = 0.112008806166976182957205488948d+00;
xtab(2) = 0.602276908118738102757080225338d+00;
weight(1) = 0.718539319030384440665510200891d+00;
weight(2) = 0.281460680969615559334489799109d+00;
elseif( norder == 3 ) ;
xtab(1) = 0.0638907930873254049961166031363d+00;
xtab(2) = 0.368997063715618765546197645857d+00;
xtab(3) = 0.766880303938941455423682659817d+00;
weight(1) = 0.513404552232363325129300497567d+00;
weight(2) = 0.391980041201487554806287180966d+00;
weight(3) = 0.0946154065661491200644123214672d+00;
elseif( norder == 4 ) ;
xtab(1) = 0.0414484801993832208033213101564d+00;
xtab(2) = 0.245274914320602251939675759523d+00;
xtab(3) = 0.556165453560275837180184354376d+00;
xtab(4) = 0.848982394532985174647849188085d+00;
weight(1) = 0.383464068145135124850046522343d+00;
weight(2) = 0.386875317774762627336008234554d+00;
weight(3) = 0.190435126950142415361360014547d+00;
weight(4) = 0.0392254871299598324525852285552d+00;
elseif( norder == 5 ) ;
xtab(1) = 0.0291344721519720533037267621154d+00;
xtab(2) = 0.173977213320897628701139710829d+00;
xtab(3) = 0.411702520284902043174931924646d+00;
xtab(4) = 0.677314174582820380701802667998d+00;
xtab(5) = 0.894771361031008283638886204455d+00;
weight(1) = 0.297893471782894457272257877888d+00;
weight(2) = 0.349776226513224180375071870307d+00;
weight(3) = 0.234488290044052418886906857943d+00;
weight(4) = 0.0989304595166331469761807114404d+00;
weight(5) = 0.0189115521431957964895826824218d+00;
elseif( norder == 6 ) ;
xtab(1) = 0.0216340058441169489956958558537d+00;
xtab(2) = 0.129583391154950796131158505009d+00;
xtab(3) = 0.314020449914765508798248188420d+00;
xtab(4) = 0.538657217351802144548941893993d+00;
xtab(5) = 0.756915337377402852164544156139d+00;
xtab(6) = 0.922668851372120237333873231507d+00;
weight(1) = 0.238763662578547569722268303330d+00;
weight(2) = 0.308286573273946792969383109211d+00;
weight(3) = 0.245317426563210385984932540188d+00;
weight(4) = 0.142008756566476685421345576030d+00;
weight(5) = 0.0554546223248862900151353549662d+00;
weight(6) = 0.0101689586929322758869351162755d+00;
elseif( norder == 7 ) ;
xtab(1) = 0.0167193554082585159416673609320d+00;
xtab(2) = 0.100185677915675121586885031757d+00;
xtab(3) = 0.246294246207930599046668547239d+00;
xtab(4) = 0.433463493257033105832882482601d+00;
xtab(5) = 0.632350988047766088461805812245d+00;
xtab(6) = 0.811118626740105576526226796782d+00;
xtab(7) = 0.940848166743347721760134113379d+00;
weight(1) = 0.196169389425248207525427377585d+00;
weight(2) = 0.270302644247272982145271719533d+00;
weight(3) = 0.239681873007690948308072785252d+00;
weight(4) = 0.165775774810432906560869687736d+00;
weight(5) = 0.0889432271376579644357238403458d+00;
weight(6) = 0.0331943043565710670254494111034d+00;
weight(7) = 0.00593278701512592399918517844468d+00;
elseif( norder == 8 ) ;
xtab(1) = 0.0133202441608924650122526725243d+00;
xtab(2) = 0.0797504290138949384098277291424d+00;
xtab(3) = 0.197871029326188053794476159516d+00;
xtab(4) = 0.354153994351909419671463603538d+00;
xtab(5) = 0.529458575234917277706149699996d+00;
xtab(6) = 0.701814529939099963837152670310d+00;
xtab(7) = 0.849379320441106676048309202301d+00;
xtab(8) = 0.953326450056359788767379678514d+00;
weight(1) = 0.164416604728002886831472568326d+00;
weight(2) = 0.237525610023306020501348561960d+00;
weight(3) = 0.226841984431919126368780402936d+00;
weight(4) = 0.175754079006070244988056212006d+00;
weight(5) = 0.112924030246759051855000442086d+00;
weight(6) = 0.0578722107177820723985279672940d+00;
weight(7) = 0.0209790737421329780434615241150d+00;
weight(8) = 0.00368640710402761901335232127647d+00;
elseif( norder == 16 ) ;
xtab( 1) = 0.00389783448711591592405360527037d+00;
xtab( 2) = 0.0230289456168732398203176309848d+00;
xtab( 3) = 0.0582803983062404123483532298394d+00;
xtab( 4) = 0.108678365091054036487713613051d+00;
xtab( 5) = 0.172609454909843937760843776232d+00;
xtab( 6) = 0.247937054470578495147671753047d+00;
xtab( 7) = 0.332094549129917155984755859320d+00;
xtab( 8) = 0.422183910581948600115088366745d+00;
xtab( 9) = 0.515082473381462603476277704052d+00;
xtab(10) = 0.607556120447728724086384921709d+00;
xtab(11) = 0.696375653228214061156318166581d+00;
xtab(12) = 0.778432565873265405203868167732d+00;
xtab(13) = 0.850850269715391083233822761319d+00;
xtab(14) = 0.911086857222271905418818994060d+00;
xtab(15) = 0.957025571703542157591520509383d+00;
xtab(16) = 0.987047800247984476758697436516d+00;
weight( 1) = 0.0607917100435912328511733871235d+00;
weight( 2) = 0.102915677517582144387691736210d+00;
weight( 3) = 0.122355662046009193557547513197d+00;
weight( 4) = 0.127569246937015988717042209239d+00;
weight( 5) = 0.123013574600070915423123365137d+00;
weight( 6) = 0.111847244855485722621848903429d+00;
weight( 7) = 0.0965963851521243412529681650802d+00;
weight( 8) = 0.0793566643514731387824416770520d+00;
weight( 9) = 0.0618504945819652070951360853113d+00;
weight(10) = 0.0454352465077266686288299526509d+00;
weight(11) = 0.0310989747515818064092528627927d+00;
weight(12) = 0.0194597659273608420780860268669d+00;
weight(13) = 0.0107762549632055256455393162159d+00;
weight(14) = 0.00497254289008764171250524951646d+00;
weight(15) = 0.00167820111005119451503546419059d+00;
weight(16) = 0.000282353764668436321778085987413d+00;
else;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'LEGENDRE_SET_LOG - Fatal error!');
writef(1,['%s','%6i','\n'], '  Illegal value of NORDER = ', norder);
error(['stop encountered in original fortran code  ',char(10),';']);
end;
return;
end
function [norder, xtab, weight]=legendre_set_sqrtx_01( norder, xtab, weight );
%*******************************************************************************
%
%! LEGENDRE_SET_SQRTX_01 sets a Gauss-Legendre rule for SQRT(X) * F(X) on [0,1].
%
%  Integration interval:
%
%    [ 0, 1 ]
%
%  Weight function:
%
%    SQRT ( X )
%
%  Integral to approximate:
%
%    Integral ( 0 <= X <= 1 ) SQRT ( X ) * F(X) dX =
%    Integral ( 0 <= Y <= 1 ) 2 * Y**2 * F(Y**2) dY.
%    (using Y = SQRT(X) )
%
%  Approximate integral:
%
%    Sum ( 1 <= I <= NORDER ) WEIGHT(I) * F ( XTAB(I) )
%
%  Modified:
%
%    23 January 2001
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Daniel Zwillinger, editor,
%    CRC Standard Mathematical Tables and Formulae,
%    CRC Press, 30th Edition, 2000, page 696.
%
%  Parameters:
%
%    Input, integer NORDER, the order of the rule.
%
%    Output, real ( kind = 8 ) XTAB(NORDER), the abscissas of the rule.
%
%    Output, real ( kind = 8 ) WEIGHT(NORDER), the weights of the rule.
%
persistent norder2 weight2 xtab2 ; 

if isempty(norder2), norder2=0; end;
if isempty(xtab2), xtab2=zeros(1,2.*norder+1); end;
if isempty(weight2), weight2=zeros(1,2.*norder+1); end;
norder2 = fix(2 .* norder + 1);
[ norder2, xtab2, weight2 ]=legendre_set( norder2, xtab2, weight2 );
xtab([1:norder]) = xtab2([norder+2:2.*norder+1]).^2;
weight([1:norder]) = 2.0d+00 .* weight2([norder+2:2.*norder+1]) .* xtab([1:norder]);
return;
end
function [norder, xtab, weight]=legendre_set_sqrtx2_01( norder, xtab, weight );
%*******************************************************************************
%
%! LEGENDRE_SET_SQRTX2_01 sets a Gauss-Legendre rule for F(X) / SQRT(X) on [0,1].
%
%  Integration interval:
%
%    [ 0, 1 ]
%
%  Weight function:
%
%    1 / SQRT ( X )
%
%  Integral to approximate:
%
%    Integral ( 0 <= X <= 1 ) F(X) / SQRT ( X ) dX
%
%  Approximate integral:
%
%    Sum ( 1 <= I <= NORDER ) WEIGHT(I) * F ( XTAB(I) )
%
%  Modified:
%
%    21 January 2001
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Daniel Zwillinger, editor,
%    CRC Standard Mathematical Tables and Formulae,
%    CRC Press, 30th Edition, 2000, page 696.
%
%  Parameters:
%
%    Input, integer NORDER, the order of the rule.
%
%    Output, real ( kind = 8 ) XTAB(NORDER), the abscissas of the rule.
%
%    Output, real ( kind = 8 ) WEIGHT(NORDER), the weights of the rule.
%
persistent norder2 weight2 xtab2 ; 

if isempty(norder2), norder2=0; end;
if isempty(xtab2), xtab2=zeros(1,2.*norder+1); end;
if isempty(weight2), weight2=zeros(1,2.*norder+1); end;
norder2 = fix(2 .* norder + 1);
[ norder2, xtab2, weight2 ]=legendre_set( norder2, xtab2, weight2 );
xtab([1:norder]) = xtab2([norder+2:2.*norder+1]).^2;
weight([1:norder]) = 2.0d+00 .* weight2([norder+2:2.*norder+1]);
return;
end
function [norder, xtab, weight]=legendre_set_x0_01( norder, xtab, weight );
%*******************************************************************************
%
%! LEGENDRE_SET_X0_01 sets a Gauss-Legendre rule for F(X) on [0,1].
%
%  Integration interval:
%
%    [ 0, 1 ]
%
%  Weight function:
%
%    1.0D+00
%
%  Integral to approximate:
%
%    Integral ( 0 <= X <= 1 ) F(X) dX
%
%  Approximate integral:
%
%    Sum ( 1 <= I <= NORDER ) WEIGHT(I) * F ( XTAB(I) )
%
%  Modified:
%
%    18 November 2000
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Abramowitz and Stegun,
%    Handbook of Mathematical Functions,
%    National Bureau of Standards, 1964, page 921.
%
%  Parameters:
%
%    Input, integer NORDER, the order of the rule.
%    NORDER must be between 1 and 8.
%
%    Output, real ( kind = 8 ) XTAB(NORDER), the abscissas of the rule.
%
%    Output, real ( kind = 8 ) WEIGHT(NORDER), the weights of the rule.
%

if( norder == 1 )
xtab(1) =   0.5d+00;
weight(1) = 1.0d+00;
elseif( norder == 2 ) ;
xtab(1) = 0.2113248654d+00;
xtab(2) = 0.7886751346d+00;
weight(1) = 0.5d+00;
weight(2) = 0.5d+00;
elseif( norder == 3 ) ;
xtab(1) = 0.1127016654d+00;
xtab(2) = 0.5000000000d+00;
xtab(3) = 0.8872983346d+00;
weight(1) = 5.0d+00 ./ 18.0d+00;
weight(2) = 8.0d+00 ./ 18.0d+00;
weight(3) = 5.0d+00 ./ 18.0d+00;
elseif( norder == 4 ) ;
xtab(1) = 0.0694318442d+00;
xtab(2) = 0.3300094782d+00;
xtab(3) = 0.6699905218d+00;
xtab(4) = 0.9305681558d+00;
weight(1) = 0.1739274226d+00;
weight(2) = 0.3260725774d+00;
weight(3) = 0.3260725774d+00;
weight(4) = 0.1739274226d+00;
elseif( norder == 5 ) ;
xtab(1) = 0.0469100770d+00;
xtab(2) = 0.2307653449d+00;
xtab(3) = 0.5000000000d+00;
xtab(4) = 0.7692346551d+00;
xtab(5) = 0.9530899230d+00;
weight(1) = 0.1184634425d+00;
weight(2) = 0.2393143352d+00;
weight(3) = 0.2844444444d+00;
weight(4) = 0.2393143352d+00;
weight(5) = 0.1184634425d+00;
elseif( norder == 6 ) ;
xtab(1) = 0.0337652429d+00;
xtab(2) = 0.1693953068d+00;
xtab(3) = 0.3806904070d+00;
xtab(4) = 0.6193095930d+00;
xtab(5) = 0.8306046932d+00;
xtab(6) = 0.9662347571d+00;
weight(1) = 0.0856622462d+00;
weight(2) = 0.1803807865d+00;
weight(3) = 0.2339569673d+00;
weight(4) = 0.2339569673d+00;
weight(5) = 0.1803807865d+00;
weight(6) = 0.0856622462d+00;
elseif( norder == 7 ) ;
xtab(1) = 0.0254460438d+00;
xtab(2) = 0.1292344072d+00;
xtab(3) = 0.2970774243d+00;
xtab(4) = 0.5000000000d+00;
xtab(5) = 0.7029225757d+00;
xtab(6) = 0.8707655928d+00;
xtab(7) = 0.9745539562d+00;
weight(1) = 0.0647424831d+00;
weight(2) = 0.1398526957d+00;
weight(3) = 0.1909150253d+00;
weight(4) = 0.2089795918d+00;
weight(5) = 0.1909150253d+00;
weight(6) = 0.1398526957d+00;
weight(7) = 0.0647424831d+00;
elseif( norder == 8 ) ;
xtab(1) = 0.0198550718d+00;
xtab(2) = 0.1016667613d+00;
xtab(3) = 0.2372337950d+00;
xtab(4) = 0.4082826788d+00;
xtab(5) = 0.5917173212d+00;
xtab(6) = 0.7627662050d+00;
xtab(7) = 0.8983332387d+00;
xtab(8) = 0.9801449282d+00;
weight(1) = 0.0506142681d+00;
weight(2) = 0.1111905172d+00;
weight(3) = 0.1568533229d+00;
weight(4) = 0.1813418917d+00;
weight(5) = 0.1813418917d+00;
weight(6) = 0.1568533229d+00;
weight(7) = 0.1111905172d+00;
weight(8) = 0.0506142681d+00;
else;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'LEGENDRE_SET_X0_01 - Fatal error!');
writef(1,['%s','%6i','\n'], '  Illegal value of NORDER = ', norder);
error(['stop encountered in original fortran code  ',char(10),';']);
end;
return;
end
function [norder, xtab, weight]=legendre_set_x1( norder, xtab, weight );
%*******************************************************************************
%
%! LEGENDRE_SET_X1 sets a Gauss-Legendre rule for ( 1 + X ) * F(X) on [-1,1].
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
%    Integral ( -1 <= X <= 1 ) ( 1 + X ) * F(X) dX
%
%  Approximate integral:
%
%    Sum ( 1 <= I <= NORDER ) WEIGHT(I) * F ( XTAB(I) )
%
%  Modified:
%
%    18 December 2000
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Arthur Stroud and Don Secrest,
%    Gaussian Quadrature Formulas,
%    Prentice Hall, 1966, Table #3.
%
%  Parameters:
%
%    Input, integer NORDER, the order of the rule.
%    NORDER must be between 1 and 9.
%
%    Output, real ( kind = 8 ) XTAB(NORDER), the abscissas of the rule.
%
%    Output, real ( kind = 8 ) WEIGHT(NORDER), the weights of the rule.
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
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'LEGENDRE_SET_X1 - Fatal error!');
writef(1,['%s','%6i','\n'], '  Illegal input value of NORDER = ', norder);
error(['stop encountered in original fortran code  ',char(10),';']);
end;
return;
end
function [norder, xtab, weight]=legendre_set_x1_01( norder, xtab, weight );
%*******************************************************************************
%
%! LEGENDRE_SET_X1_01 sets a Gauss-Legendre rule for X * F(X) on [0,1].
%
%  Integration interval:
%
%    [ 0, 1 ]
%
%  Weight function:
%
%    X
%
%  Integral to approximate:
%
%    Integral ( 0 <= X <= 1 ) X * F(X) dX
%
%  Approximate integral:
%
%    Sum ( 1 <= I <= NORDER ) WEIGHT(I) * F ( XTAB(I) )
%
%  Modified:
%
%    18 November 2000
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Abramowitz and Stegun,
%    Handbook of Mathematical Functions,
%    National Bureau of Standards, 1964, page 921.
%
%  Parameters:
%
%    Input, integer NORDER, the order of the rule.
%    NORDER must be between 1 and 8.
%
%    Output, real ( kind = 8 ) XTAB(NORDER), the abscissas of the rule.
%
%    Output, real ( kind = 8 ) WEIGHT(NORDER), the weights of the rule.
%

if( norder == 1 )
xtab(1) =   0.6666666667d+00;
weight(1) = 0.5000000000d+00;
elseif( norder == 2 ) ;
xtab(1) = 0.3550510257d+00;
xtab(2) = 0.8449489743d+00;
weight(1) = 0.1819586183d+00;
weight(2) = 0.3180413817d+00;
elseif( norder == 3 ) ;
xtab(1) = 0.2123405382d+00;
xtab(2) = 0.5905331356d+00;
xtab(3) = 0.9114120405d+00;
weight(1) = 0.0698269799d+00;
weight(2) = 0.2292411064d+00;
weight(3) = 0.2009319137d+00;
elseif( norder == 4 ) ;
xtab(1) = 0.1397598643d+00;
xtab(2) = 0.4164095676d+00;
xtab(3) = 0.7231569864d+00;
xtab(4) = 0.9428958039d+00;
weight(1) = 0.0311809710d+00;
weight(2) = 0.1298475476d+00;
weight(3) = 0.2034645680d+00;
weight(4) = 0.1355069134d+00;
elseif( norder == 5 ) ;
xtab(1) = 0.0985350858d+00;
xtab(2) = 0.3045357266d+00;
xtab(3) = 0.5620251898d+00;
xtab(4) = 0.8019865821d+00;
xtab(5) = 0.9601901429d+00;
weight(1) = 0.0157479145d+00;
weight(2) = 0.0739088701d+00;
weight(3) = 0.1463888701d+00;
weight(4) = 0.1671746381d+00;
weight(5) = 0.0967815902d+00;
elseif( norder == 6 ) ;
xtab(1) = 0.0730543287d+00;
xtab(2) = 0.2307661380d+00;
xtab(3) = 0.4413284812d+00;
xtab(4) = 0.6630153097d+00;
xtab(5) = 0.8519214003d+00;
xtab(6) = 0.9706835728d+00;
weight(1) = 0.0087383108d+00;
weight(2) = 0.0439551656d+00;
weight(3) = 0.0986611509d+00;
weight(4) = 0.1407925538d+00;
weight(5) = 0.1355424972d+00;
weight(6) = 0.0723103307d+00;
elseif( norder == 7 ) ;
xtab(1) = 0.0562625605d+00;
xtab(2) = 0.1802406917d+00;
xtab(3) = 0.3526247171d+00;
xtab(4) = 0.5471536263d+00;
xtab(5) = 0.7342101772d+00;
xtab(6) = 0.8853209468d+00;
xtab(7) = 0.9775206136d+00;
weight(1) = 0.0052143622d+00;
weight(2) = 0.0274083567d+00;
weight(3) = 0.0663846965d+00;
weight(4) = 0.1071250657d+00;
weight(5) = 0.1273908973d+00;
weight(6) = 0.1105092582d+00;
weight(7) = 0.0559673634d+00;
elseif( norder == 8 ) ;
xtab(1) = 0.0446339553d+00;
xtab(2) = 0.1443662570d+00;
xtab(3) = 0.2868247571d+00;
xtab(4) = 0.4548133152d+00;
xtab(5) = 0.6280678354d+00;
xtab(6) = 0.7856915206d+00;
xtab(7) = 0.9086763921d+00;
xtab(8) = 0.9822200849d+00;
weight(1) = 0.0032951914d+00;
weight(2) = 0.0178429027d+00;
weight(3) = 0.0454393195d+00;
weight(4) = 0.0791995995d+00;
weight(5) = 0.1060473594d+00;
weight(6) = 0.1125057995d+00;
weight(7) = 0.0911190236d+00;
weight(8) = 0.0445508044d+00;
else;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'LEGENDRE_SET_X1_01 - Fatal error!');
writef(1,['%s','%6i','\n'], '  Illegal value of NORDER = ', norder);
error(['stop encountered in original fortran code  ',char(10),';']);
end;
return;
end
function [norder, xtab, weight]=legendre_set_x2( norder, xtab, weight );
%*******************************************************************************
%
%! LEGENDRE_SET_X2 sets a Gauss-Legendre rule for ( 1 + X )**2 * F(X) on [-1,1].
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
%    Integral ( -1 <= X <= 1 ) ( 1 + X )**2 * F(X) dX
%
%  Approximate integral:
%
%    Sum ( 1 <= I <= NORDER ) WEIGHT(I) * F ( XTAB(I) )
%
%  Modified:
%
%    18 December 2000
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Arthur Stroud and Don Secrest,
%    Gaussian Quadrature Formulas,
%    Prentice Hall, 1966, Table #3.
%
%  Parameters:
%
%    Input, integer NORDER, the order of the rule.
%    NORDER must be between 1 and 9.
%
%    Output, real ( kind = 8 ) XTAB(NORDER), the abscissas of the rule.
%
%    Output, real ( kind = 8 ) WEIGHT(NORDER), the weights of the rule.
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
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'LEGENDRE_SET_X2 - Fatal error!');
writef(1,['%s','%6i','\n'], '  Illegal input value of NORDER = ', norder);
error(['stop encountered in original fortran code  ',char(10),';']);
end;
return;
end
function [norder, xtab, weight]=legendre_set_x2_01( norder, xtab, weight );
%*******************************************************************************
%
%! LEGENDRE_SET_X2_01 sets a Gauss-Legendre rule for X**2 * F(X) on [0,1].
%
%  Integration interval:
%
%    [ 0, 1 ]
%
%  Weight function:
%
%    X**2
%
%  Integral to approximate:
%
%    Integral ( 0 <= X <= 1 ) X*X * F(X) dX
%
%  Approximate integral:
%
%    Sum ( 1 <= I <= NORDER ) WEIGHT(I) * F ( XTAB(I) )
%
%  Modified:
%
%    18 November 2000
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Abramowitz and Stegun,
%    Handbook of Mathematical Functions,
%    National Bureau of Standards, 1964, page 921.
%
%  Parameters:
%
%    Input, integer NORDER, the order of the rule.
%    NORDER must be between 1 and 8.
%
%    Output, real ( kind = 8 ) XTAB(NORDER), the abscissas of the rule.
%
%    Output, real ( kind = 8 ) WEIGHT(NORDER), the weights of the rule.
%

if( norder == 1 )
xtab(1) =   0.75d+00;
weight(1) = 1.0d+00 ./ 3.0d+00;
elseif( norder == 2 ) ;
xtab(1) = 0.4558481560d+00;
xtab(2) = 0.8774851773d+00;
weight(1) = 0.1007858821d+00;
weight(2) = 0.2325474513d+00;
elseif( norder == 3 ) ;
xtab(1) = 0.2949977901d+00;
xtab(2) = 0.6529962340d+00;
xtab(3) = 0.9270059759d+00;
weight(1) = 0.0299507030d+00;
weight(2) = 0.1462462693d+00;
weight(3) = 0.1571363611d+00;
elseif( norder == 4 ) ;
xtab(1) = 0.2041485821d+00;
xtab(2) = 0.4829527049d+00;
xtab(3) = 0.7613992624d+00;
xtab(4) = 0.9514994506d+00;
weight(1) = 0.0103522408d+00;
weight(2) = 0.0686338872d+00;
weight(3) = 0.1434587898d+00;
weight(4) = 0.1108884156d+00;
elseif( norder == 5 ) ;
xtab(1) = 0.1489457871d+00;
xtab(2) = 0.3656665274d+00;
xtab(3) = 0.6101136129d+00;
xtab(4) = 0.8265196792d+00;
xtab(5) = 0.9654210601d+00;
weight(1) = 0.0041138252d+00;
weight(2) = 0.0320556007d+00;
weight(3) = 0.0892001612d+00;
weight(4) = 0.1261989619d+00;
weight(5) = 0.0817647843d+00;
elseif( norder == 6 ) ;
xtab(1) = 0.1131943838d+00;
xtab(2) = 0.2843188727d+00;
xtab(3) = 0.4909635868d+00;
xtab(4) = 0.6975630820d+00;
xtab(5) = 0.8684360583d+00;
xtab(6) = 0.9740954449d+00;
weight(1) = 0.0018310758d+00;
weight(2) = 0.0157202972d+00;
weight(3) = 0.0512895711d+00;
weight(4) = 0.0945771867d+00;
weight(5) = 0.1073764997d+00;
weight(6) = 0.0625387027d+00;
elseif( norder == 7 ) ;
xtab(1) = 0.0888168334d+00;
xtab(2) = 0.2264827534d+00;
xtab(3) = 0.3999784867d+00;
xtab(4) = 0.5859978554d+00;
xtab(5) = 0.7594458740d+00;
xtab(6) = 0.8969109709d+00;
xtab(7) = 0.9798672262d+00;
weight(1) = 0.0008926880d+00;
weight(2) = 0.0081629256d+00;
weight(3) = 0.0294222113d+00;
weight(4) = 0.0631463787d+00;
weight(5) = 0.0917338033d+00;
weight(6) = 0.0906988246d+00;
weight(7) = 0.0492765018d+00;
elseif( norder == 8 ) ;
xtab(1) = 0.0714910350d+00;
xtab(2) = 0.1842282964d+00;
xtab(3) = 0.3304477282d+00;
xtab(4) = 0.4944029218d+00;
xtab(5) = 0.6583480085d+00;
xtab(6) = 0.8045248315d+00;
xtab(7) = 0.9170993825d+00;
xtab(8) = 0.9839022404d+00;
weight(1) = 0.0004685178d+00;
weight(2) = 0.0044745217d+00;
weight(3) = 0.0172468638d+00;
weight(4) = 0.0408144264d+00;
weight(5) = 0.0684471834d+00;
weight(6) = 0.0852847692d+00;
weight(7) = 0.0768180933d+00;
weight(8) = 0.0397789578d+00;
else;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'LEGENDRE_SET_X2_01 - Fatal error!');
writef(1,['%s','%6i','\n'], '  Illegal value of NORDER = ', norder);
error(['stop encountered in original fortran code  ',char(10),';']);
end;
return;
end
function [norder, xtab, weight]=lobatto_set( norder, xtab, weight );
%*******************************************************************************
%
%! LOBATTO_SET sets abscissas and weights for Lobatto quadrature.
%
%  Integration interval:
%
%    [ -1, 1 ].
%
%  Weight function:
%
%    1.0D+00
%
%  Integral to approximate:
%
%    Integral ( -1 <= X <= 1 ) F(X) dX
%
%  Approximate integral:
%
%    Sum ( 1 <= I <= NORDER ) WEIGHT(I) * F ( XTAB(I) )
%
%  Precision:
%
%    The quadrature rule will integrate exactly all polynomials up to
%    X**(2*NORDER-3).
%
%  Note:
%
%    The Lobatto rule is distinguished by the fact that both endpoints
%    (-1 and 1) are always abscissas of the rule.
%
%  Modified:
%
%    20 September 1998
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Abramowitz and Stegun,
%    Handbook of Mathematical Functions,
%    National Bureau of Standards, 1964.
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
%  Parameters:
%
%    Input, integer NORDER, the order of the rule.
%    NORDER must be between 2 and 20.
%
%    Output, real ( kind = 8 ) XTAB(NORDER), the abscissas for the rule.
%
%    Output, real ( kind = 8 ) WEIGHT(NORDER), the weights of the rule.
%    The weights are positive, symmetric and should sum to 2.
%

if( norder == 2 )
xtab(1) =  - 1.0d+00;
xtab(2) =    1.0d+00;
weight(1) =  1.0d+00;
weight(2) =  1.0d+00;
elseif( norder == 3 ) ;
xtab(1) =  - 1.0d+00;
xtab(2) =    0.0d+00;
xtab(3) =    1.0d+00;
weight(1) =  1.0d+00 ./ 3.0d+00;
weight(2) =  4.0d+00 ./ 3.0d+00;
weight(3) =  1.0d+00 ./ 3.0d+00;
elseif( norder == 4 ) ;
xtab(1) =  - 1.0d+00;
xtab(2) =  - 0.447213595499957939281834733746d+00;
xtab(3) =    0.447213595499957939281834733746d+00;
xtab(4) =    1.0d+00;
weight(1) =  1.0d+00 ./ 6.0d+00;
weight(2) =  5.0d+00 ./ 6.0d+00;
weight(3) =  5.0d+00 ./ 6.0d+00;
weight(4) =  1.0d+00 ./ 6.0d+00;
elseif( norder == 5 ) ;
xtab(1) =  - 1.0d+00;
xtab(2) =  - 0.654653670707977143798292456247d+00;
xtab(3) =    0.0d+00;
xtab(4) =    0.654653670707977143798292456247d+00;
xtab(5) =    1.0d+00;
weight(1) =  9.0d+00 ./ 90.0d+00;
weight(2) = 49.0d+00 ./ 90.0d+00;
weight(3) = 64.0d+00 ./ 90.0d+00;
weight(4) = 49.0d+00 ./ 90.0d+00;
weight(5) =  9.0d+00 ./ 90.0d+00;
elseif( norder == 6 ) ;
xtab(1) =  - 1.0d+00;
xtab(2) =  - 0.765055323929464692851002973959d+00;
xtab(3) =  - 0.285231516480645096314150994041d+00;
xtab(4) =    0.285231516480645096314150994041d+00;
xtab(5) =    0.765055323929464692851002973959d+00;
xtab(6) =    1.0d+00;
weight(1) =  0.066666666666666666666666666667d+00;
weight(2) =  0.378474956297846980316612808212d+00;
weight(3) =  0.554858377035486353016720525121d+00;
weight(4) =  0.554858377035486353016720525121d+00;
weight(5) =  0.378474956297846980316612808212d+00;
weight(6) =  0.066666666666666666666666666667d+00;
elseif( norder == 7 ) ;
xtab(1) =  - 1.0d+00;
xtab(2) =  - 0.830223896278566929872032213967d+00;
xtab(3) =  - 0.468848793470714213803771881909d+00;
xtab(4) =    0.0d+00;
xtab(5) =    0.468848793470714213803771881909d+00;
xtab(6) =    0.830223896278566929872032213967d+00;
xtab(7) =    1.0d+00;
weight(1) =  0.476190476190476190476190476190d-01;
weight(2) =  0.276826047361565948010700406290d+00;
weight(3) =  0.431745381209862623417871022281d+00;
weight(4) =  0.487619047619047619047619047619d+00;
weight(5) =  0.431745381209862623417871022281d+00;
weight(6) =  0.276826047361565948010700406290d+00;
weight(7) =  0.476190476190476190476190476190d-01;
elseif( norder == 8 ) ;
xtab(1) =  - 1.0d+00;
xtab(2) =  - 0.871740148509606615337445761221d+00;
xtab(3) =  - 0.591700181433142302144510731398d+00;
xtab(4) =  - 0.209299217902478868768657260345d+00;
xtab(5) =    0.209299217902478868768657260345d+00;
xtab(6) =    0.591700181433142302144510731398d+00;
xtab(7) =    0.871740148509606615337445761221d+00;
xtab(8) =    1.0d+00;
weight(1) =  0.357142857142857142857142857143d-01;
weight(2) =  0.210704227143506039382991065776d+00;
weight(3) =  0.341122692483504364764240677108d+00;
weight(4) =  0.412458794658703881567052971402d+00;
weight(5) =  0.412458794658703881567052971402d+00;
weight(6) =  0.341122692483504364764240677108d+00;
weight(7) =  0.210704227143506039382991065776d+00;
weight(8) =  0.357142857142857142857142857143d-01;
elseif( norder == 9 ) ;
xtab(1) =  - 1.0d+00;
xtab(2) =  - 0.899757995411460157312345244418d+00;
xtab(3) =  - 0.677186279510737753445885427091d+00;
xtab(4) =  - 0.363117463826178158710752068709d+00;
xtab(5) =    0.0d+00;
xtab(6) =    0.363117463826178158710752068709d+00;
xtab(7) =    0.677186279510737753445885427091d+00;
xtab(8) =    0.899757995411460157312345244418d+00;
xtab(9) =    1.0d+00;
weight(1) =  0.277777777777777777777777777778d-01;
weight(2) =  0.165495361560805525046339720029d+00;
weight(3) =  0.274538712500161735280705618579d+00;
weight(4) =  0.346428510973046345115131532140d+00;
weight(5) =  0.371519274376417233560090702948d+00;
weight(6) =  0.346428510973046345115131532140d+00;
weight(7) =  0.274538712500161735280705618579d+00;
weight(8) =  0.165495361560805525046339720029d+00;
weight(9) =  0.277777777777777777777777777778d-01;
elseif( norder == 10 ) ;
xtab(1) =  - 1.0d+00;
xtab(2) =  - 0.919533908166458813828932660822d+00;
xtab(3) =  - 0.738773865105505075003106174860d+00;
xtab(4) =  - 0.477924949810444495661175092731d+00;
xtab(5) =  - 0.165278957666387024626219765958d+00;
xtab(6) =    0.165278957666387024626219765958d+00;
xtab(7) =    0.477924949810444495661175092731d+00;
xtab(8) =    0.738773865105505075003106174860d+00;
xtab(9) =    0.919533908166458813828932660822d+00;
xtab(10) =   1.0d+00;
weight(1) =  0.222222222222222222222222222222d-01;
weight(2) =  0.133305990851070111126227170755d+00;
weight(3) =  0.224889342063126452119457821731d+00;
weight(4) =  0.292042683679683757875582257374d+00;
weight(5) =  0.327539761183897456656510527917d+00;
weight(6) =  0.327539761183897456656510527917d+00;
weight(7) =  0.292042683679683757875582257374d+00;
weight(8) =  0.224889342063126452119457821731d+00;
weight(9) =  0.133305990851070111126227170755d+00;
weight(10) = 0.222222222222222222222222222222d-01;
elseif( norder == 11 ) ;
xtab(1) =  - 1.0d+00;
xtab(2) =  - 0.934001430408059134332274136099d+00;
xtab(3) =  - 0.784483473663144418622417816108d+00;
xtab(4) =  - 0.565235326996205006470963969478d+00;
xtab(5) =  - 0.295758135586939391431911515559d+00;
xtab(6) =    0.0d+00;
xtab(7) =    0.295758135586939391431911515559d+00;
xtab(8) =    0.565235326996205006470963969478d+00;
xtab(9) =    0.784483473663144418622417816108d+00;
xtab(10) =   0.934001430408059134332274136099d+00;
xtab(11) =   1.0d+00;
weight(1) =  0.181818181818181818181818181818d-01;
weight(2) =  0.109612273266994864461403449580d+00;
weight(3) =  0.187169881780305204108141521899d+00;
weight(4) =  0.248048104264028314040084866422d+00;
weight(5) =  0.286879124779008088679222403332d+00;
weight(6) =  0.300217595455690693785931881170d+00;
weight(7) =  0.286879124779008088679222403332d+00;
weight(8) =  0.248048104264028314040084866422d+00;
weight(9) =  0.187169881780305204108141521899d+00;
weight(10) = 0.109612273266994864461403449580d+00;
weight(11) = 0.181818181818181818181818181818d-01;
elseif( norder == 12 ) ;
xtab(1) =  - 1.0d+00;
xtab(2) =  - 0.944899272222882223407580138303d+00;
xtab(3) =  - 0.819279321644006678348641581717d+00;
xtab(4) =  - 0.632876153031869677662404854444d+00;
xtab(5) =  - 0.399530940965348932264349791567d+00;
xtab(6) =  - 0.136552932854927554864061855740d+00;
xtab(7) =    0.136552932854927554864061855740d+00;
xtab(8) =    0.399530940965348932264349791567d+00;
xtab(9) =    0.632876153031869677662404854444d+00;
xtab(10) =   0.819279321644006678348641581717d+00;
xtab(11) =   0.944899272222882223407580138303d+00;
xtab(12) =   1.0d+00;
weight(1) =  0.151515151515151515151515151515d-01;
weight(2) =  0.916845174131961306683425941341d-01;
weight(3) =  0.157974705564370115164671062700d+00;
weight(4) =  0.212508417761021145358302077367d+00;
weight(5) =  0.251275603199201280293244412148d+00;
weight(6) =  0.271405240910696177000288338500d+00;
weight(7) =  0.271405240910696177000288338500d+00;
weight(8) =  0.251275603199201280293244412148d+00;
weight(9) =  0.212508417761021145358302077367d+00;
weight(10) = 0.157974705564370115164671062700d+00;
weight(11) = 0.916845174131961306683425941341d-01;
weight(12) = 0.151515151515151515151515151515d-01;
elseif( norder == 13 ) ;
xtab(1) =  - 1.0d+00;
xtab(2) =  - 0.953309846642163911896905464755d+00;
xtab(3) =  - 0.846347564651872316865925607099d+00;
xtab(4) =  - 0.686188469081757426072759039566d+00;
xtab(5) =  - 0.482909821091336201746937233637d+00;
xtab(6) =  - 0.249286930106239992568673700374d+00;
xtab(7) =    0.0d+00;
xtab(8) =    0.249286930106239992568673700374d+00;
xtab(9) =    0.482909821091336201746937233637d+00;
xtab(10) =   0.686188469081757426072759039566d+00;
xtab(11) =   0.846347564651872316865925607099d+00;
xtab(12) =   0.953309846642163911896905464755d+00;
xtab(13) =   1.0d+00;
weight(1) =  0.128205128205128205128205128205d-01;
weight(2) =  0.778016867468189277935889883331d-01;
weight(3) =  0.134981926689608349119914762589d+00;
weight(4) =  0.183646865203550092007494258747d+00;
weight(5) =  0.220767793566110086085534008379d+00;
weight(6) =  0.244015790306676356458578148360d+00;
weight(7) =  0.251930849333446736044138641541d+00;
weight(8) =  0.244015790306676356458578148360d+00;
weight(9) =  0.220767793566110086085534008379d+00;
weight(10) = 0.183646865203550092007494258747d+00;
weight(11) = 0.134981926689608349119914762589d+00;
weight(12) = 0.778016867468189277935889883331d-01;
weight(13) = 0.128205128205128205128205128205d-01;
elseif( norder == 14 ) ;
xtab(1) =  - 1.0d+00;
xtab(2) =  - 0.959935045267260901355100162015d+00;
xtab(3) =  - 0.867801053830347251000220202908d+00;
xtab(4) =  - 0.728868599091326140584672400521d+00;
xtab(5) =  - 0.550639402928647055316622705859d+00;
xtab(6) =  - 0.342724013342712845043903403642d+00;
xtab(7) =  - 0.116331868883703867658776709736d+00;
xtab(8) =    0.116331868883703867658776709736d+00;
xtab(9) =    0.342724013342712845043903403642d+00;
xtab(10) =   0.550639402928647055316622705859d+00;
xtab(11) =   0.728868599091326140584672400521d+00;
xtab(12) =   0.867801053830347251000220202908d+00;
xtab(13) =   0.959935045267260901355100162015d+00;
xtab(14) =   1.0d+00;
weight(1) =  0.109890109890109890109890109890d-01;
weight(2) =  0.668372844976812846340706607461d-01;
weight(3) =  0.116586655898711651540996670655d+00;
weight(4) =  0.160021851762952142412820997988d+00;
weight(5) =  0.194826149373416118640331778376d+00;
weight(6) =  0.219126253009770754871162523954d+00;
weight(7) =  0.231612794468457058889628357293d+00;
weight(8) =  0.231612794468457058889628357293d+00;
weight(9) =  0.219126253009770754871162523954d+00;
weight(10) = 0.194826149373416118640331778376d+00;
weight(11) = 0.160021851762952142412820997988d+00;
weight(12) = 0.116586655898711651540996670655d+00;
weight(13) = 0.668372844976812846340706607461d-01;
weight(14) = 0.109890109890109890109890109890d-01;
elseif( norder == 15 ) ;
xtab(1) =  - 1.0d+00;
xtab(2) =  - 0.965245926503838572795851392070d+00;
xtab(3) =  - 0.885082044222976298825401631482d+00;
xtab(4) =  - 0.763519689951815200704118475976d+00;
xtab(5) =  - 0.606253205469845711123529938637d+00;
xtab(6) =  - 0.420638054713672480921896938739d+00;
xtab(7) =  - 0.215353955363794238225679446273d+00;
xtab(8) =    0.0d+00;
xtab(9) =    0.215353955363794238225679446273d+00;
xtab(10) =   0.420638054713672480921896938739d+00;
xtab(11) =   0.606253205469845711123529938637d+00;
xtab(12) =   0.763519689951815200704118475976d+00;
xtab(13) =   0.885082044222976298825401631482d+00;
xtab(14) =   0.965245926503838572795851392070d+00;
xtab(15) =   1.0d+00;
weight(1) =  0.952380952380952380952380952381d-02;
weight(2) =  0.580298930286012490968805840253d-01;
weight(3) =  0.101660070325718067603666170789d+00;
weight(4) =  0.140511699802428109460446805644d+00;
weight(5) =  0.172789647253600949052077099408d+00;
weight(6) =  0.196987235964613356092500346507d+00;
weight(7) =  0.211973585926820920127430076977d+00;
weight(8) =  0.217048116348815649514950214251d+00;
weight(9) =  0.211973585926820920127430076977d+00;
weight(10) = 0.196987235964613356092500346507d+00;
weight(11) = 0.172789647253600949052077099408d+00;
weight(12) = 0.140511699802428109460446805644d+00;
weight(13) = 0.101660070325718067603666170789d+00;
weight(14) = 0.580298930286012490968805840253d-01;
weight(15) = 0.952380952380952380952380952381d-02;
elseif( norder == 16 ) ;
xtab(1) =  - 1.0d+00;
xtab(2) =  - 0.969568046270217932952242738367d+00;
xtab(3) =  - 0.899200533093472092994628261520d+00;
xtab(4) =  - 0.792008291861815063931088270963d+00;
xtab(5) =  - 0.652388702882493089467883219641d+00;
xtab(6) =  - 0.486059421887137611781890785847d+00;
xtab(7) =  - 0.299830468900763208098353454722d+00;
xtab(8) =  - 0.101326273521949447843033005046d+00;
xtab(9) =    0.101326273521949447843033005046d+00;
xtab(10) =   0.299830468900763208098353454722d+00;
xtab(11) =   0.486059421887137611781890785847d+00;
xtab(12) =   0.652388702882493089467883219641d+00;
xtab(13) =   0.792008291861815063931088270963d+00;
xtab(14) =   0.899200533093472092994628261520d+00;
xtab(15) =   0.969568046270217932952242738367d+00;
xtab(16) =   1.0d+00;
weight(1) =  0.833333333333333333333333333333d-02;
weight(2) =  0.508503610059199054032449195655d-01;
weight(3) =  0.893936973259308009910520801661d-01;
weight(4) =  0.124255382132514098349536332657d+00;
weight(5) =  0.154026980807164280815644940485d+00;
weight(6) =  0.177491913391704125301075669528d+00;
weight(7) =  0.193690023825203584316913598854d+00;
weight(8) =  0.201958308178229871489199125411d+00;
weight(9) =  0.201958308178229871489199125411d+00;
weight(10) = 0.193690023825203584316913598854d+00;
weight(11) = 0.177491913391704125301075669528d+00;
weight(12) = 0.154026980807164280815644940485d+00;
weight(13) = 0.124255382132514098349536332657d+00;
weight(14) = 0.893936973259308009910520801661d-01;
weight(15) = 0.508503610059199054032449195655d-01;
weight(16) = 0.833333333333333333333333333333d-02;
elseif( norder == 17 ) ;
xtab(1) =  - 1.0d+00;
xtab(2) =  - 0.973132176631418314156979501874d+00;
xtab(3) =  - 0.910879995915573595623802506398d+00;
xtab(4) =  - 0.815696251221770307106750553238d+00;
xtab(5) =  - 0.691028980627684705394919357372d+00;
xtab(6) =  - 0.541385399330101539123733407504d+00;
xtab(7) =  - 0.372174433565477041907234680735d+00;
xtab(8) =  - 0.189511973518317388304263014753d+00;
xtab(9) =    0.0d+00;
xtab(10) =   0.189511973518317388304263014753d+00;
xtab(11) =   0.372174433565477041907234680735d+00;
xtab(12) =   0.541385399330101539123733407504d+00;
xtab(13) =   0.691028980627684705394919357372d+00;
xtab(14) =   0.815696251221770307106750553238d+00;
xtab(15) =   0.910879995915573595623802506398d+00;
xtab(16) =   0.973132176631418314156979501874d+00;
xtab(17) =   1.0d+00;
weight(1) =  0.735294117647058823529411764706d-02;
weight(2) =  0.449219405432542096474009546232d-01;
weight(3) =  0.791982705036871191902644299528d-01;
weight(4) =  0.110592909007028161375772705220d+00;
weight(5) =  0.137987746201926559056201574954d+00;
weight(6) =  0.160394661997621539516328365865d+00;
weight(7) =  0.177004253515657870436945745363d+00;
weight(8) =  0.187216339677619235892088482861d+00;
weight(9) =  0.190661874753469433299407247028d+00;
weight(10) = 0.187216339677619235892088482861d+00;
weight(11) = 0.177004253515657870436945745363d+00;
weight(12) = 0.160394661997621539516328365865d+00;
weight(13) = 0.137987746201926559056201574954d+00;
weight(14) = 0.110592909007028161375772705220d+00;
weight(15) = 0.791982705036871191902644299528d-01;
weight(16) = 0.449219405432542096474009546232d-01;
weight(17) = 0.735294117647058823529411764706d-02;
elseif( norder == 18 ) ;
xtab(1) =  - 1.0d+00;
xtab(2) =  - 0.976105557412198542864518924342d+00;
xtab(3) =  - 0.920649185347533873837854625431d+00;
xtab(4) =  - 0.835593535218090213713646362328d+00;
xtab(5) =  - 0.723679329283242681306210365302d+00;
xtab(6) =  - 0.588504834318661761173535893194d+00;
xtab(7) =  - 0.434415036912123975342287136741d+00;
xtab(8) =  - 0.266362652878280984167665332026d+00;
xtab(9) =  - 0.897490934846521110226450100886d-01;
xtab(10) =   0.897490934846521110226450100886d-01;
xtab(11) =   0.266362652878280984167665332026d+00;
xtab(12) =   0.434415036912123975342287136741d+00;
xtab(13) =   0.588504834318661761173535893194d+00;
xtab(14) =   0.723679329283242681306210365302d+00;
xtab(15) =   0.835593535218090213713646362328d+00;
xtab(16) =   0.920649185347533873837854625431d+00;
xtab(17) =   0.976105557412198542864518924342d+00;
xtab(18) =   1.0d+00;
weight(1) =  0.653594771241830065359477124183d-02;
weight(2) =  0.399706288109140661375991764101d-01;
weight(3) =  0.706371668856336649992229601678d-01;
weight(4) =  0.990162717175028023944236053187d-01;
weight(5) =  0.124210533132967100263396358897d+00;
weight(6) =  0.145411961573802267983003210494d+00;
weight(7) =  0.161939517237602489264326706700d+00;
weight(8) =  0.173262109489456226010614403827d+00;
weight(9) =  0.179015863439703082293818806944d+00;
weight(10) = 0.179015863439703082293818806944d+00;
weight(11) = 0.173262109489456226010614403827d+00;
weight(12) = 0.161939517237602489264326706700d+00;
weight(13) = 0.145411961573802267983003210494d+00;
weight(14) = 0.124210533132967100263396358897d+00;
weight(15) = 0.990162717175028023944236053187d-01;
weight(16) = 0.706371668856336649992229601678d-01;
weight(17) = 0.399706288109140661375991764101d-01;
weight(18) = 0.653594771241830065359477124183d-02;
elseif( norder == 19 ) ;
xtab(1) =  - 1.0d+00;
xtab(2) =  - 0.978611766222080095152634063110d+00;
xtab(3) =  - 0.928901528152586243717940258797d+00;
xtab(4) =  - 0.852460577796646093085955970041d+00;
xtab(5) =  - 0.751494202552613014163637489634d+00;
xtab(6) =  - 0.628908137265220497766832306229d+00;
xtab(7) =  - 0.488229285680713502777909637625d+00;
xtab(8) =  - 0.333504847824498610298500103845d+00;
xtab(9) =  - 0.169186023409281571375154153445d+00;
xtab(10) =   0.0d+00;
xtab(11) =   0.169186023409281571375154153445d+00;
xtab(12) =   0.333504847824498610298500103845d+00;
xtab(13) =   0.488229285680713502777909637625d+00;
xtab(14) =   0.628908137265220497766832306229d+00;
xtab(15) =   0.751494202552613014163637489634d+00;
xtab(16) =   0.852460577796646093085955970041d+00;
xtab(17) =   0.928901528152586243717940258797d+00;
xtab(18) =   0.978611766222080095152634063110d+00;
xtab(19) =   1.0d+00;
weight(1) =  0.584795321637426900584795321637d-02;
weight(2) =  0.357933651861764771154255690351d-01;
weight(3) =  0.633818917626297368516956904183d-01;
weight(4) =  0.891317570992070844480087905562d-01;
weight(5) =  0.112315341477305044070910015464d+00;
weight(6) =  0.132267280448750776926046733910d+00;
weight(7) =  0.148413942595938885009680643668d+00;
weight(8) =  0.160290924044061241979910968184d+00;
weight(9) =  0.167556584527142867270137277740d+00;
weight(10) = 0.170001919284827234644672715617d+00;
weight(11) = 0.167556584527142867270137277740d+00;
weight(12) = 0.160290924044061241979910968184d+00;
weight(13) = 0.148413942595938885009680643668d+00;
weight(14) = 0.132267280448750776926046733910d+00;
weight(15) = 0.112315341477305044070910015464d+00;
weight(16) = 0.891317570992070844480087905562d-01;
weight(17) = 0.633818917626297368516956904183d-01;
weight(18) = 0.357933651861764771154255690351d-01;
weight(19) = 0.584795321637426900584795321637d-02;
elseif( norder == 20 ) ;
xtab(1) =  - 1.0d+00;
xtab(2) =  - 0.980743704893914171925446438584d+00;
xtab(3) =  - 0.935934498812665435716181584931d+00;
xtab(4) =  - 0.866877978089950141309847214616d+00;
xtab(5) =  - 0.775368260952055870414317527595d+00;
xtab(6) =  - 0.663776402290311289846403322971d+00;
xtab(7) =  - 0.534992864031886261648135961829d+00;
xtab(8) =  - 0.392353183713909299386474703816d+00;
xtab(9) =  - 0.239551705922986495182401356927d+00;
xtab(10) = - 0.805459372388218379759445181596d-01;
xtab(11) =   0.805459372388218379759445181596d-01;
xtab(12) =   0.239551705922986495182401356927d+00;
xtab(13) =   0.392353183713909299386474703816d+00;
xtab(14) =   0.534992864031886261648135961829d+00;
xtab(15) =   0.663776402290311289846403322971d+00;
xtab(16) =   0.775368260952055870414317527595d+00;
xtab(17) =   0.866877978089950141309847214616d+00;
xtab(18) =   0.935934498812665435716181584931d+00;
xtab(19) =   0.980743704893914171925446438584d+00;
xtab(20) =   1.0d+00;
weight(1) =  0.526315789473684210526315789474d-02;
weight(2) =  0.322371231884889414916050281173d-01;
weight(3) =  0.571818021275668260047536271732d-01;
weight(4) =  0.806317639961196031447768461137d-01;
weight(5) =  0.101991499699450815683781205733d+00;
weight(6) =  0.120709227628674725099429705002d+00;
weight(7) =  0.136300482358724184489780792989d+00;
weight(8) =  0.148361554070916825814713013734d+00;
weight(9) =  0.156580102647475487158169896794d+00;
weight(10) = 0.160743286387845749007726726449d+00;
weight(11) = 0.160743286387845749007726726449d+00;
weight(12) = 0.156580102647475487158169896794d+00;
weight(13) = 0.148361554070916825814713013734d+00;
weight(14) = 0.136300482358724184489780792989d+00;
weight(15) = 0.120709227628674725099429705002d+00;
weight(16) = 0.101991499699450815683781205733d+00;
weight(17) = 0.806317639961196031447768461137d-01;
weight(18) = 0.571818021275668260047536271732d-01;
weight(19) = 0.322371231884889414916050281173d-01;
weight(20) = 0.526315789473684210526315789474d-02;
else;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'LOBATTO_SET - Fatal error!');
writef(1,['%s','%6i','\n'], '  Illegal value of NORDER = ', norder);
writef(1,['%s','\n'], '  Legal values are between 2 and 20.');
error(['stop encountered in original fortran code  ',char(10),';']);
end;
return;
end
function [log_gammaresult, x ]=log_gamma( x );
%*******************************************************************************
%
%! LOG_GAMMA calculates the natural logarithm of GAMMA(X).
%
%  Discussion:
%
%    The method uses Stirling's approximation, and is accurate to about
%    12 decimal places.
%
%  Modified:
%
%    19 September 1998
%
%  Reference:
%
%    Arthur Stroud and Don Secrest,
%    Gaussian Quadrature Formulas,
%    Prentice Hall, 1966.
%
%  Parameters:
%
%    Input, real ( kind = 8 ) X, the evaluation point.  The routine
%    will fail if GAMMA(X) is not positive.  X should be greater than 0.
%
%    Output, real ( kind = 8 ) LOG_GAMMA, the natural logarithm of the
%    gamma function of X.
%
log_gammaresult=[];
persistent i k log_gamma m p x2 y z ; 

if isempty(i), i=0; end;
if isempty(k), k=0; end;
if isempty(log_gammaresult), log_gammaresult=0; end;
if isempty(m), m=0; end;
if isempty(p), p=0; end;
if isempty(x2), x2=0; end;
if isempty(y), y=0; end;
if isempty(z), z=0; end;
if( x < 0.5d+00 )
m = 1;
x2 = 1.0d+00 - x;
else;
m = 0;
x2 = x;
end;
k = - 1;
while (1);
k = fix(k + 1);
if( 6.0d+00 < x2 + real( k) )
break;
end;
end;
z = x2 + real( k);
y =( z - 0.5d+00 ) .* log( z ) - z + 0.9189385332047d+00 +(((((- 4146.0d+00 ./ z.^2+ 1820.0d+00 ) ./ z.^2- 1287.0d+00 ) ./ z.^2+ 1716.0d+00 ) ./ z.^2- 6006.0d+00 ) ./ z.^2+ 180180.0d+00 ) ./ z ./ 2162160.0d+00;
if( 0 < k )
for i = 1: k;
y = y - log( x2 + real( k - i) );
end; i = fix(k+1);
end;
if( m ~= 0 )
p = d_pi( ) ./ sin( d_pi( ) .*( 1.0d+00 - x2 ) );
if( p <= 0.0d+00 )
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'LOG_GAMMA - fatal error!');
error(['stop encountered in original fortran code  ',char(10),';']);
else;
y = log( p ) - y;
end;
end;
log_gammaresult = y;
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',x); evalin('caller',[inputname(1),'=FUntemp;']); end
return;
end
function [norder, xtab, weight]=moulton_set( norder, xtab, weight );
%*******************************************************************************
%
%! MOULTON_SET sets weights for Adams-Moulton quadrature.
%
%  Discussion:
%
%    Adams-Moulton quadrature formulas are normally used in solving
%    ordinary differential equations, and are not suitable for general
%    quadrature computations.  However, an Adams-Moulton formula is
%    equivalent to approximating the integral of F(Y(X)) between X(M)
%    and X(M+1), using an implicit formula that relies on known values
%    of F(Y(X)) at X(M-N+1) through X(M), plus the unknown value at X(M+1).
%
%    Suppose the unknown function is denoted by Y(X), with derivative F(Y(X)),
%    and that approximate values of the function are known at a series of
%    X values, which we write as X(1), X(2), ..., X(M).  We write the value
%    Y(X(1)) as Y(1) and so on.
%
%    Then the solution of the ODE Y' = F(X,Y) at the next point X(M+1) is
%    computed by:
%
%      Y(M+1) = Y(M) + Integral ( X(M) < X < X(M+1) ) F(Y(X)) dX
%             = Y(M) + H * Sum ( 1 <= I <= N ) W(I) * F(Y(M+2-I)) approximately.
%
%    Note that this formula is implicit, since the unknown value Y(M+1)
%    appears on the right hand side.  Hence, in ODE applications, this
%    equation must be solved via a nonlinear equation solver.  For
%    quadrature problems, where the function to be integrated is known
%    beforehand, this is not a problem, and the calculation is explicit.
%
%    In the documentation that follows, we replace F(Y(X)) by F(X).
%
%
%    The Adams-Moulton formulas require equally spaced data.
%
%    Here is how the formula is applied in the case with non-unit spacing:
%
%      Integral ( A <= X <= A+H ) F(X) dX =
%      H * Sum ( 1 <= I <= NORDER ) WEIGHT(I) * F ( A - (I-2)*H ),
%      approximately.
%
%  Integration interval:
%
%    [ 0, 1 ]
%
%  Weight function:
%
%    1.0D+00
%
%  Integral to approximate:
%
%    Integral ( 0 <= X <= 1 ) F(X) dX
%
%  Approximate integral:
%
%    Sum ( 1 <= I <= NORDER ) WEIGHT(I) * F ( 2 - I )
%
%  Modified:
%
%    16 September 1998
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Abramowitz and Stegun,
%    Handbook of Mathematical Functions,
%    National Bureau of Standards, 1964,
%    page 915 ('Lagrangian Integration Coefficients').
%
%    Jean Lapidus and John Seinfeld,
%    Numerical Solution of Ordinary Differential Equations,
%    Academic Press, 1971.
%
%    Daniel Zwillinger, editor,
%    Standard Mathematical Tables and Formulae,
%    30th Edition,
%    CRC Press, 1996.
%
%  Parameters:
%
%    Input, integer NORDER, the order of the rule.  NORDER must be
%    between 1 and 10.
%
%    Output, real ( kind = 8 ) XTAB(NORDER), the abscissas of the rule.
%
%    Output, real ( kind = 8 ) WEIGHT(NORDER), the weights of the rule.
%    WEIGHT(1) is the weight at X = 1, WEIGHT(2) the weight at X = 0, and so on.
%    The weights are rational.  The weights are not symmetric, and
%    some weights may be negative.  They should sum to 1.
%
persistent d i ; 

if isempty(d), d=0; end;
if isempty(i), i=0; end;
if( norder == 1 )
weight(1) =  1.0d+00;
elseif( norder == 2 ) ;
d = 2.0d+00;
weight(1) =  1.0d+00 ./ d;
weight(2) =  1.0d+00 ./ d;
elseif( norder == 3 ) ;
d = 12.0d+00;
weight(1) =    5.0d+00 ./ d;
weight(2) =    8.0d+00 ./ d;
weight(3) =  - 1.0d+00 ./ d;
elseif( norder == 4 ) ;
d = 24.0d+00;
weight(1) =    9.0d+00 ./ d;
weight(2) =   19.0d+00 ./ d;
weight(3) =  - 5.0d+00 ./ d;
weight(4) =    1.0d+00 ./ d;
elseif( norder == 5 ) ;
d = 720.0d+00;
weight(1) =    251.0d+00 ./ d;
weight(2) =    646.0d+00 ./ d;
weight(3) =  - 264.0d+00 ./ d;
weight(4) =    106.0d+00 ./ d;
weight(5) =   - 19.0d+00 ./ d;
elseif( norder == 6 ) ;
d = 1440.0d+00;
weight(1) =    475.0d+00 ./ d;
weight(2) =   1427.0d+00 ./ d;
weight(3) =  - 798.0d+00 ./ d;
weight(4) =    482.0d+00 ./ d;
weight(5) =  - 173.0d+00 ./ d;
weight(6) =     27.0d+00 ./ d;
elseif( norder == 7 ) ;
d = 60480.0d+00;
weight(1) =    19087.0d+00 ./ d;
weight(2) =    65112.0d+00 ./ d;
weight(3) =  - 46461.0d+00 ./ d;
weight(4) =    37504.0d+00 ./ d;
weight(5) =  - 20211.0d+00 ./ d;
weight(6) =     6312.0d+00 ./ d;
weight(7) =    - 863.0d+00 ./ d;
elseif( norder == 8 ) ;
d = 120960.0d+00;
weight(1) =    36799.0d+00 ./ d;
weight(2) =   139849.0d+00 ./ d;
weight(3) = - 121797.0d+00 ./ d;
weight(4) =   123133.0d+00 ./ d;
weight(5) =  - 88547.0d+00 ./ d;
weight(6) =    41499.0d+00 ./ d;
weight(7) =  - 11351.0d+00 ./ d;
weight(8) =     1375.0d+00 ./ d;
elseif( norder == 9 ) ;
d = 3628800.0d+00;
weight(1) =   1070017.0d+00 ./ d;
weight(2) =   4467094.0d+00 ./ d;
weight(3) = - 4604594.0d+00 ./ d;
weight(4) =   5595358.0d+00 ./ d;
weight(5) = - 5033120.0d+00 ./ d;
weight(6) =   3146338.0d+00 ./ d;
weight(7) = - 1291214.0d+00 ./ d;
weight(8) =    312874.0d+00 ./ d;
weight(9) =   - 33953.0d+00 ./ d;
elseif( norder == 10 ) ;
d = 7257600.0d+00;
weight(1) =    2082753.0d+00 ./ d;
weight(2) =    9449717.0d+00 ./ d;
weight(3) = - 11271304.0d+00 ./ d;
weight(4) =   16002320.0d+00 ./ d;
weight(5) = - 17283646.0d+00 ./ d;
weight(6) =   13510082.0d+00 ./ d;
weight(7) =  - 7394032.0d+00 ./ d;
weight(8) =    2687864.0d+00 ./ d;
weight(9) =   - 583435.0d+00 ./ d;
weight(10) =     57281.0d+00 ./ d;
else;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'MOULTON_SET - Fatal error!');
writef(1,['%s','%6i','\n'], '  Illegal value of NORDER = ', norder);
writef(1,['%s','\n'], '  Legal values are 1 through 10.');
error(['stop encountered in original fortran code  ',char(10),';']);
end;
for i = 1: norder;
xtab(i) = real( 2 - i);
end; i = fix(norder+1);
return;
end
function [norder, a, b, xtab, weight]=nc_com( norder, a, b, xtab, weight );
%*******************************************************************************
%
%! NC_COM computes a Newton-Cotes quadrature rule with given nodes.
%
%  Discussion:
%
%    For the interval [A,B], the Newton-Cotes quadrature rule estimates
%
%      Integral ( A <= X <= B ) F(X) dX
%
%    using NORDER abscissas XTAB(I) and a weight vector WEIGHT(I):
%
%      Sum ( 1 <= I <= N ) WEIGHT(I) * F ( XTAB(I) ).
%
%    For the CLOSED rule, the equally spaced abscissas include A and B.
%    For the OPEN rule, the equally spaced abscissas do not include A and B.
%
%  Modified:
%
%    25 September 1998
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, integer NORDER, the order of the rule.
%
%    Input, real ( kind = 8 ) A, B, the left and right endpoints of the interval
%    over which the quadrature rule is to be applied.
%
%    Input, real ( kind = 8 ) XTAB(NORDER), the abscissas of the rule.
%
%    Output, real ( kind = 8 ) WEIGHT(NORDER), the weights of the rule.
%
persistent diftab i j k yvala yvalb ; 

if isempty(diftab), diftab=zeros(1,norder); end;
if isempty(i), i=0; end;
if isempty(j), j=0; end;
if isempty(k), k=0; end;
if isempty(yvala), yvala=0; end;
if isempty(yvalb), yvalb=0; end;
for i = 1: norder;
%
%  Compute the Lagrange basis polynomial which is 1 at XTAB(I),
%  and zero at the other nodes.
%
diftab([1:norder]) = 0.0d+00;
diftab(i) = 1.0d+00;
for j = 2: norder;
for k = j: norder;
diftab(norder+j-k) =( diftab(norder+j-k-1) - diftab(norder+j-k) )./( xtab(norder+1-k) - xtab(norder+j-k) );
end; k = fix(norder+1);
end; j = fix(norder+1);
for j = 1: norder-1;
for k = 1: norder-j;
diftab(norder-k) = diftab(norder-k) - xtab(norder-k-j+1) .*diftab(norder-k+1);
end; k = fix(norder-j+1);
end; j = fix(norder-1+1);
%
%  Evaluate the antiderivative of the polynomial at the left and
%  right endpoints.
%
yvala = diftab(norder) ./ real( norder);
for j = norder-1: -1: 1;
yvala = yvala .* a + diftab(j) ./ real( j);
end; j = fix(1-1);
yvala = yvala .* a;
yvalb = diftab(norder) ./ real( norder);
for j = norder-1: -1: 1;
yvalb = yvalb .* b + diftab(j) ./ real( j);
end; j = fix(1-1);
yvalb = yvalb .* b;
weight(i) = yvalb - yvala;
end; i = fix(norder+1);
return;
end
function [norder, xtab, weight]=ncc_com( norder, xtab, weight );
%*******************************************************************************
%
%! NCC_COM computes a Newton-Cotes closed quadrature rule of given order.
%
%  Definition:
%
%    For the interval [-1,1], the Newton-Cotes open quadrature rule
%    estimates
%
%      Integral ( -1 <= X <= 1 ) F(X) dX
%
%    using NORDER equally spaced abscissas XTAB(I) and a weight vector
%    WEIGHT(I):
%
%      Sum ( 1 <= I <= N ) WEIGHT(I) * F ( XTAB(I) ).
%
%    For the CLOSED rule, the abscissas include A and B.
%
%  Modified:
%
%    25 September 1998
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, integer NORDER, the order of the rule, which should be
%    at least 2.
%
%    Output, real ( kind = 8 ) XTAB(NORDER), the abscissas of the rule.
%
%    Output, real ( kind = 8 ) WEIGHT(NORDER), the weights of the rule.
%
persistent a b i ; 

if isempty(a), a=0; end;
if isempty(b), b=0; end;
if isempty(i), i=0; end;
%
%  Compute a closed quadrature rule.
%
a = -1.0d+00;
b =  1.0d+00;
if( norder == 1 )
xtab(1) = 0.0d+00;
weight(1) = 2.0d+00;
return;
else;
for i = 1: norder;
xtab(i) =( real( norder - i) .* a+ real(          i - 1) .* b )./ real( norder     - 1);
end; i = fix(norder+1);
end;
[ norder, a, b, xtab, weight ]=nc_com( norder, a, b, xtab, weight );
return;
end
function [norder, xtab, weight]=ncc_set( norder, xtab, weight );
%*******************************************************************************
%
%! NCC_SET sets abscissas and weights for closed Newton-Cotes quadrature.
%
%  Discussion:
%
%    The closed Newton-Cotes rules use equally spaced abscissas, and
%    hence may be used with tabulated function data.
%
%    The rules are called 'closed' because they include the endpoints.
%
%    The higher order rules involve negative weights.  These can produce
%    loss of accuracy due to the subtraction of large, nearly equal quantities.
%
%    NORDER = 2 is the trapezoidal rule.
%    NORDER = 3 is Simpson's rule.
%    NORDER = 4 is Simpson's 3/8 rule.
%    NORDER = 5 is Bode's rule.
%
%    The Kopal reference for NORDER = 12 lists
%      WEIGHT(6) = 15494566.0D+00 / 43545600.0D+00
%    but this results in a set of coeffients that don't add up to 2.
%    The correct value is
%      WEIGHT(6) = 15493566.0D+00 / 43545600.0.
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
%    Integral ( -1 <= X <= 1 ) F(X) dX
%
%  Approximate integral:
%
%    Sum ( 1 <= I <= NORDER ) WEIGHT(I) * F ( XTAB(I) )
%
%  Modified:
%
%    16 September 1998
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Abramowitz and Stegun,
%    Handbook of Mathematical Functions,
%    National Bureau of Standards, 1964.
%
%    Johnson,
%    Quarterly Journal of Mathematics,
%    Volume 46, Number 52, 1915.
%
%    Zdenek Kopal,
%    Numerical Analysis,
%    John Wiley, 1955.
%
%    Daniel Zwillinger, editor,
%    Standard Mathematical Tables and Formulae,
%    30th Edition,
%    CRC Press, 1996.
%
%  Parameters:
%
%    Input, integer NORDER, the order of the rule.
%    NORDER must be between 2 and 20.
%
%    Output, real ( kind = 8 ) XTAB(NORDER), the abscissas of the rule.
%    The abscissas are uniformly spaced in the interval, and include
%    -1 and 1.
%
%    Output, real ( kind = 8 ) WEIGHT(NORDER), the weights of the rule.
%    The weights are symmetric, rational, and should sum to 2.0.
%    Some weights may be negative.
%
persistent d i ; 

if isempty(d), d=0; end;
if isempty(i), i=0; end;
if( norder == 2 )
weight(1) = 1.0d+00;
weight(2) = 1.0d+00;
elseif( norder == 3 ) ;
d = 3.0d+00;
weight(1) = 1.0d+00 ./ d;
weight(2) = 4.0d+00 ./ d;
weight(3) = 1.0d+00 ./ d;
elseif( norder == 4 ) ;
d = 4.0d+00;
weight(1) = 1.0d+00 ./ d;
weight(2) = 3.0d+00 ./ d;
weight(3) = 3.0d+00 ./ d;
weight(4) = 1.0d+00 ./ d;
elseif( norder == 5 ) ;
d = 45.0d+00;
weight(1) =  7.0d+00 ./ d;
weight(2) = 32.0d+00 ./ d;
weight(3) = 12.0d+00 ./ d;
weight(4) = 32.0d+00 ./ d;
weight(5) =  7.0d+00 ./ d;
elseif( norder == 6 ) ;
d = 144.0d+00;
weight(1) = 19.0d+00 ./ d;
weight(2) = 75.0d+00 ./ d;
weight(3) = 50.0d+00 ./ d;
weight(4) = 50.0d+00 ./ d;
weight(5) = 75.0d+00 ./ d;
weight(6) = 19.0d+00 ./ d;
elseif( norder == 7 ) ;
d = 420.0d+00;
weight(1) =  41.0d+00 ./ d;
weight(2) = 216.0d+00 ./ d;
weight(3) =  27.0d+00 ./ d;
weight(4) = 272.0d+00 ./ d;
weight(5) =  27.0d+00 ./ d;
weight(6) = 216.0d+00 ./ d;
weight(7) =  41.0d+00 ./ d;
elseif( norder == 8 ) ;
d = 8640.0d+00;
weight(1) =  751.0d+00 ./ d;
weight(2) = 3577.0d+00 ./ d;
weight(3) = 1323.0d+00 ./ d;
weight(4) = 2989.0d+00 ./ d;
weight(5) = 2989.0d+00 ./ d;
weight(6) = 1323.0d+00 ./ d;
weight(7) = 3577.0d+00 ./ d;
weight(8) =  751.0d+00 ./ d;
elseif( norder == 9 ) ;
d = 14175.0d+00;
weight(1) =    989.0d+00 ./ d;
weight(2) =   5888.0d+00 ./ d;
weight(3) =  - 928.0d+00 ./ d;
weight(4) =  10496.0d+00 ./ d;
weight(5) = - 4540.0d+00 ./ d;
weight(6) =  10496.0d+00 ./ d;
weight(7) =  - 928.0d+00 ./ d;
weight(8) =   5888.0d+00 ./ d;
weight(9) =    989.0d+00 ./ d;
elseif( norder == 10 ) ;
d = 44800.0d+00;
weight(1) =   2857.0d+00 ./ d;
weight(2) =  15741.0d+00 ./ d;
weight(3) =   1080.0d+00 ./ d;
weight(4) =  19344.0d+00 ./ d;
weight(5) =   5778.0d+00 ./ d;
weight(6) =   5778.0d+00 ./ d;
weight(7) =  19344.0d+00 ./ d;
weight(8) =   1080.0d+00 ./ d;
weight(9) =  15741.0d+00 ./ d;
weight(10) =  2857.0d+00 ./ d;
elseif( norder == 11 ) ;
d = 299376.0d+00;
weight(1) =     16067.0d+00 ./ d;
weight(2) =    106300.0d+00 ./ d;
weight(3) =   - 48525.0d+00 ./ d;
weight(4) =    272400.0d+00 ./ d;
weight(5) =  - 260550.0d+00 ./ d;
weight(6) =    427368.0d+00 ./ d;
weight(7) =  - 260550.0d+00 ./ d;
weight(8) =    272400.0d+00 ./ d;
weight(9) =   - 48525.0d+00 ./ d;
weight(10) =   106300.0d+00 ./ d;
weight(11) =    16067.0d+00 ./ d;
elseif( norder == 12 ) ;
d = 43545600.0d+00;
weight(1) =     2171465.0d+00 ./ d;
weight(2) =    13486539.0d+00 ./ d;
weight(3) =   - 3237113.0d+00 ./ d;
weight(4) =    25226685.0d+00 ./ d;
weight(5) =   - 9595542.0d+00 ./ d;
weight(6) =    15493566.0d+00 ./ d;
weight(7) =    15493566.0d+00 ./ d;
weight(8) =   - 9595542.0d+00 ./ d;
weight(9) =    25226685.0d+00 ./ d;
weight(10) =  - 3237113.0d+00 ./ d;
weight(11) =   13486539.0d+00 ./ d;
weight(12) =    2171465.0d+00 ./ d;
elseif( norder == 13 ) ;
d = 31531500.0d+00;
weight(1) =      1364651.0d+00 ./ d;
weight(2) =      9903168.0d+00 ./ d;
weight(3) =    - 7587864.0d+00 ./ d;
weight(4) =     35725120.0d+00 ./ d;
weight(5) =   - 51491295.0d+00 ./ d;
weight(6) =     87516288.0d+00 ./ d;
weight(7) =   - 87797136.0d+00 ./ d;
weight(8) =     87516288.0d+00 ./ d;
weight(9) =   - 51491295.0d+00 ./ d;
weight(10) =    35725120.0d+00 ./ d;
weight(11) =   - 7587864.0d+00 ./ d;
weight(12) =     9903168.0d+00 ./ d;
weight(13) =     1364651.0d+00 ./ d;
elseif( norder == 14 ) ;
d = 150885504000.0d+00;
weight(1) =      6137698213.0d+00 ./ d;
weight(2) =     42194238652.0d+00 ./ d;
weight(3) =   - 23361540993.0d+00 ./ d;
weight(4) =    116778274403.0d+00 ./ d;
weight(5) =  - 113219777650.0d+00 ./ d;
weight(6) =    154424590209.0d+00 ./ d;
weight(7) =   - 32067978834.0d+00 ./ d;
weight(8) =   - 32067978834.0d+00 ./ d;
weight(9) =    154424590209.0d+00 ./ d;
weight(10) = - 113219777650.0d+00 ./ d;
weight(11) =   116778274403.0d+00 ./ d;
weight(12) =  - 23361540993.0d+00 ./ d;
weight(13) =    42194238652.0d+00 ./ d;
weight(14) =     6137698213.0d+00 ./ d;
elseif( norder == 15 ) ;
d = 2501928000.0d+00;
weight(1) =       90241897.0d+00 ./ d;
weight(2) =      710986864.0d+00 ./ d;
weight(3) =    - 770720657.0d+00 ./ d;
weight(4) =     3501442784.0d+00 ./ d;
weight(5) =   - 6625093363.0d+00 ./ d;
weight(6) =    12630121616.0d+00 ./ d;
weight(7) =  - 16802270373.0d+00 ./ d;
weight(8) =    19534438464.0d+00 ./ d;
weight(9) =  - 16802270373.0d+00 ./ d;
weight(10) =   12630121616.0d+00 ./ d;
weight(11) =  - 6625093363.0d+00 ./ d;
weight(12) =    3501442784.0d+00 ./ d;
weight(13) =   - 770720657.0d+00 ./ d;
weight(14) =     710986864.0d+00 ./ d;
weight(15) =      90241897.0d+00 ./ d;
elseif( norder == 16 ) ;
d = 3099672576.0d+00;
weight(1) =     105930069.0d+00 ./ d;
weight(2) =     796661595.0d+00 ./ d;
weight(3) =   - 698808195.0d+00 ./ d;
weight(4) =    3143332755.0d+00 ./ d;
weight(5) =  - 4688522055.0d+00 ./ d;
weight(6) =    7385654007.0d+00 ./ d;
weight(7) =  - 6000998415.0d+00 ./ d;
weight(8) =    3056422815.0d+00 ./ d;
weight(9) =    3056422815.0d+00 ./ d;
weight(10) = - 6000998415.0d+00 ./ d;
weight(11) =   7385654007.0d+00 ./ d;
weight(12) = - 4688522055.0d+00 ./ d;
weight(13) =   3143332755.0d+00 ./ d;
weight(14) =  - 698808195.0d+00 ./ d;
weight(15) =    796661595.0d+00 ./ d;
weight(16) =    105930069.0d+00 ./ d;
elseif( norder == 17 ) ;
d = 488462349375.0d+00;
weight(1) =       15043611773.0d+00 ./ d;
weight(2) =      127626606592.0d+00 ./ d;
weight(3) =    - 179731134720.0d+00 ./ d;
weight(4) =      832211855360.0d+00 ./ d;
weight(5) =   - 1929498607520.0d+00 ./ d;
weight(6) =     4177588893696.0d+00 ./ d;
weight(7) =   - 6806534407936.0d+00 ./ d;
weight(8) =     9368875018240.0d+00 ./ d;
weight(9) =  - 10234238972220.0d+00 ./ d;
weight(10) =    9368875018240.0d+00 ./ d;
weight(11) =  - 6806534407936.0d+00 ./ d;
weight(12) =    4177588893696.0d+00 ./ d;
weight(13) =  - 1929498607520.0d+00 ./ d;
weight(14) =     832211855360.0d+00 ./ d;
weight(15) =   - 179731134720.0d+00 ./ d;
weight(16) =     127626606592.0d+00 ./ d;
weight(17) =      15043611773.0d+00 ./ d;
elseif( norder == 18 ) ;
d = 1883051089920000.0d+00;
weight(1) =       55294720874657.0d+00 ./ d;
weight(2) =      450185515446285.0d+00 ./ d;
weight(3) =    - 542023437008852.0d+00 ./ d;
weight(4) =     2428636525764260.0d+00 ./ d;
weight(5) =   - 4768916800123440.0d+00 ./ d;
weight(6) =     8855416648684984.0d+00 ./ d;
weight(7) =  - 10905371859796660.0d+00 ./ d;
weight(8) =    10069615750132836.0d+00 ./ d;
weight(9) =   - 3759785974054070.0d+00 ./ d;
weight(10) =  - 3759785974054070.0d+00 ./ d;
weight(11) =   10069615750132836.0d+00 ./ d;
weight(12) = - 10905371859796660.0d+00 ./ d;
weight(13) =    8855416648684984.0d+00 ./ d;
weight(14) =  - 4768916800123440.0d+00 ./ d;
weight(15) =    2428636525764260.0d+00 ./ d;
weight(16) =   - 542023437008852.0d+00 ./ d;
weight(17) =     450185515446285.0d+00 ./ d;
weight(18) =      55294720874657.0d+00 ./ d;
elseif( norder == 19 ) ;
d = 7604556960000.0d+00;
weight(1) =       203732352169.0d+00 ./ d;
weight(2) =      1848730221900.0d+00 ./ d;
weight(3) =    - 3212744374395.0d+00 ./ d;
weight(4) =     15529830312096.0d+00 ./ d;
weight(5) =   - 42368630685840.0d+00 ./ d;
weight(6) =    103680563465808.0d+00 ./ d;
weight(7) =  - 198648429867720.0d+00 ./ d;
weight(8) =    319035784479840.0d+00 ./ d;
weight(9) =  - 419127951114198.0d+00 ./ d;
weight(10) =   461327344340680.0d+00 ./ d;
weight(11) = - 419127951114198.0d+00 ./ d;
weight(12) =   319035784479840.0d+00 ./ d;
weight(13) = - 198648429867720.0d+00 ./ d;
weight(14) =   103680563465808.0d+00 ./ d;
weight(15) =  - 42368630685840.0d+00 ./ d;
weight(16) =    15529830312096.0d+00 ./ d;
weight(17) =   - 3212744374395.0d+00 ./ d;
weight(18) =     1848730221900.0d+00 ./ d;
weight(19) =      203732352169.0d+00 ./ d;
elseif( norder == 20 ) ;
d = 2688996956405760000.0d+00;
weight(1) =       69028763155644023.0d+00 ./ d;
weight(2) =      603652082270808125.0d+00 ./ d;
weight(3) =    - 926840515700222955.0d+00 ./ d;
weight(4) =     4301581538450500095.0d+00 ./ d;
weight(5) =  - 10343692234243192788.0d+00 ./ d;
weight(6) =    22336420328479961316.0d+00 ./ d;
weight(7) =  - 35331888421114781580.0d+00 ./ d;
weight(8) =    43920768370565135580.0d+00 ./ d;
weight(9) =  - 37088370261379851390.0d+00 ./ d;
weight(10) =   15148337305921759574.0d+00 ./ d;
weight(11) =   15148337305921759574.0d+00 ./ d;
weight(12) = - 37088370261379851390.0d+00 ./ d;
weight(13) =   43920768370565135580.0d+00 ./ d;
weight(14) = - 35331888421114781580.0d+00 ./ d;
weight(15) =   22336420328479961316.0d+00 ./ d;
weight(16) = - 10343692234243192788.0d+00 ./ d;
weight(17) =    4301581538450500095.0d+00 ./ d;
weight(18) =   - 926840515700222955.0d+00 ./ d;
weight(19) =     603652082270808125.0d+00 ./ d;
weight(20) =      69028763155644023.0d+00 ./ d;
else;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'NCC_SET - Fatal error!');
writef(1,['%s','%6i','\n'], '  Illegal value of NORDER = ', norder);
writef(1,['%s','\n'], '  Legal values are 2 through 20.');
error(['stop encountered in original fortran code  ',char(10),';']);
end;
%
%  The abscissas are uniformly spaced.
%
for i = 1: norder;
xtab(i) = real( 2 .* i - 1 - norder)./ real( norder - 1);
end; i = fix(norder+1);
return;
end
function [norder, xtab, weight]=nco_com( norder, xtab, weight );
%*******************************************************************************
%
%! NCO_COM computes a Newton-Cotes open quadrature rule of given order.
%
%  Discussion:
%
%    For the interval [-1,1], the Newton-Cotes open quadrature rule
%    estimates
%
%      Integral ( -1 <= X <= 1 ) F(X) dX
%
%    using NORDER equally spaced abscissas XTAB(I) and a weight vector
%    WEIGHT(I):
%
%      Sum ( 1 <= I <= N ) WEIGHT(I) * F ( XTAB(I) ).
%
%    For the OPEN rule, the abscissas do not include A and B.
%
%  Modified:
%
%    25 September 1998
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, integer NORDER, the order of the rule.
%
%    Output, real ( kind = 8 ) XTAB(NORDER), the abscissas of the rule.
%
%    Output, real ( kind = 8 ) WEIGHT(NORDER), the weights of the  rule.
%
persistent a b i ; 

if isempty(a), a=0; end;
if isempty(b), b=0; end;
if isempty(i), i=0; end;
a = -1.0d+00;
b =  1.0d+00;
for i = 1: norder;
xtab(i) =( real( norder + 1 - i) .* a+ real(              i) .* b )./ real( norder + 1);
end; i = fix(norder+1);
[ norder, a, b, xtab, weight ]=nc_com( norder, a, b, xtab, weight );
return;
end
function [norder, xtab, weight]=nco_set( norder, xtab, weight );
%*******************************************************************************
%
%! NCO_SET sets abscissas and weights for open Newton-Cotes quadrature.
%
%  Discussion:
%
%    The open Newton-Cotes rules use equally spaced abscissas, and
%    hence may be used with equally spaced data.
%
%    The rules are called 'open' because they do not include the interval
%    endpoints.
%
%    Most of the rules involve negative weights.  These can produce loss
%    of accuracy due to the subtraction of large, nearly equal quantities.
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
%    Integral ( -1 <= X <= 1 ) F(X) dX
%
%  Approximate integral:
%
%    Sum ( 1 <= I <= NORDER ) WEIGHT(I) * F ( XTAB(I) )
%
%  Modified:
%
%    15 September 1998
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Abramowitz and Stegun,
%    Handbook of Mathematical Functions,
%    National Bureau of Standards, 1964.
%
%    Daniel Zwillinger, editor,
%    Standard Mathematical Tables and Formulae,
%    30th Edition,
%    CRC Press, 1996.
%
%  Parameters:
%
%    Input, integer NORDER, the order of the rule.
%    NORDER must be between 1 and 7, and 9.
%
%    Output, real ( kind = 8 ) XTAB(NORDER), the abscissas of the rule.
%
%    Output, real ( kind = 8 ) WEIGHT(NORDER), the weights of the rule.
%    The weights are rational, symmetric, and should sum to 2.
%    Some weights may be negative.
%
persistent d i ; 

if isempty(d), d=0; end;
if isempty(i), i=0; end;
if( norder == 1 )
weight(1) = 2.0d+00;
elseif( norder == 2 ) ;
weight(1) = 1.0d+00;
weight(2) = 1.0d+00;
elseif( norder == 3 ) ;
d = 3.0d+00;
weight(1) =   4.0d+00 ./ d;
weight(2) = - 2.0d+00 ./ d;
weight(3) =   4.0d+00 ./ d;
elseif( norder == 4 ) ;
d = 12.0d+00;
weight(1) = 11.0d+00 ./ d;
weight(2) =  1.0d+00 ./ d;
weight(3) =  1.0d+00 ./ d;
weight(4) = 11.0d+00 ./ d;
elseif( norder == 5 ) ;
d = 10.0d+00;
weight(1) =   11.0d+00 ./ d;
weight(2) = - 14.0d+00 ./ d;
weight(3) =   26.0d+00 ./ d;
weight(4) = - 14.0d+00 ./ d;
weight(5) =   11.0d+00 ./ d;
elseif( norder == 6 ) ;
d = 1440.0d+00;
weight(1) =  1222.0d+00 ./ d;
weight(2) = - 906.0d+00 ./ d;
weight(3) =  1124.0d+00 ./ d;
weight(4) =  1124.0d+00 ./ d;
weight(5) = - 906.0d+00 ./ d;
weight(6) =  1222.0d+00 ./ d;
elseif( norder == 7 ) ;
d = 945.0d+00;
weight(1) =    920.0d+00 ./ d;
weight(2) = - 1908.0d+00 ./ d;
weight(3) =   4392.0d+00 ./ d;
weight(4) = - 4918.0d+00 ./ d;
weight(5) =   4392.0d+00 ./ d;
weight(6) = - 1908.0d+00 ./ d;
weight(7) =    920.0d+00 ./ d;
elseif( norder == 9 ) ;
d = 4536.0d+00;
weight(1) =    4045.0d+00 ./ d;
weight(2) = - 11690.0d+00 ./ d;
weight(3) =   33340.0d+00 ./ d;
weight(4) = - 55070.0d+00 ./ d;
weight(5) =   67822.0d+00 ./ d;
weight(6) = - 55070.0d+00 ./ d;
weight(7) =   33340.0d+00 ./ d;
weight(8) = - 11690.0d+00 ./ d;
weight(9) =    4045.0d+00 ./ d;
else;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'NCO_SET - Fatal error!');
writef(1,['%s','%6i','\n'], '  Illegal value of NORDER = ', norder);
writef(1,['%s','\n'], '  Legal values are 1 to 7, and 9.');
error(['stop encountered in original fortran code  ',char(10),';']);
end;
%
%  Set the abscissas.
%
for i = 1: norder;
xtab(i) = real( 2 .* i - norder - 1)./ real( norder + 1);
end; i = fix(norder+1);
return;
end
function [norder, xtab, weight]=radau_set( norder, xtab, weight );
%*******************************************************************************
%
%! RADAU_SET sets abscissas and weights for Radau quadrature.
%
%  Discussion:
%
%    The Radau rule is distinguished by the fact that the left endpoint
%    (-1) is always an abscissa.
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
%    Integral ( -1 <= X <= 1 ) F(X) dX
%
%  Approximate integral:
%
%    Sum ( 1 <= I <= NORDER ) WEIGHT(I) * F ( XTAB(I) )
%
%  Precision:
%
%    The quadrature rule will integrate exactly all polynomials up to
%    X**(2*NORDER-2).
%
%  Modified:
%
%    16 September 1998
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Abramowitz and Stegun,
%    Handbook of Mathematical Functions,
%    National Bureau of Standards, 1964.
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
%  Parameters:
%
%    Input, integer NORDER, the order of the rule.
%    NORDER must be between 1 and 15.
%
%    Output, real ( kind = 8 ) XTAB(NORDER), the abscissas of the rule.
%
%    Output, real ( kind = 8 ) WEIGHT(NORDER), the weights of the rule.
%    The weights are positive.  The weights are not symmetric.
%    The weights should sum to 2.  WEIGHT(1) should equal 2 / NORDER**2.
%

if( norder == 1 )
xtab(1) =   - 1.0d+00;
weight(1) =   2.0d+00;
elseif( norder == 2 ) ;
xtab(1) =  - 1.0d+00;
xtab(2) =    1.0d+00 ./ 3.0d+00;
weight(1) =  0.5d+00;
weight(2) =  1.5d+00;
elseif( norder == 3 ) ;
xtab(1) =   - 1.0d+00;
xtab(2) =   - 0.289897948556635619639456814941d+00;
xtab(3) =     0.689897948556635619639456814941d+00;
weight(1) =  0.222222222222222222222222222222d+00;
weight(2) =  0.102497165237684322767762689304d+01;
weight(3) =  0.752806125400934550100150884739d+00;
elseif( norder == 4 ) ;
xtab(1) =  - 1.0d+00;
xtab(2) =  - 0.575318923521694112050483779752d+00;
xtab(3) =    0.181066271118530578270147495862d+00;
xtab(4) =    0.822824080974592105208907712461d+00;
weight(1) =  0.125d+00;
weight(2) =  0.657688639960119487888578442146d+00;
weight(3) =  0.776386937686343761560464613780d+00;
weight(4) =  0.440924422353536750550956944074d+00;
elseif( norder == 5 ) ;
xtab(1) =  - 1.0d+00;
xtab(2) =  - 0.720480271312438895695825837750d+00;
xtab(3) =  - 0.167180864737833640113395337326d+00;
xtab(4) =    0.446313972723752344639908004629d+00;
xtab(5) =    0.885791607770964635613757614892d+00;
weight(1) =  0.08d+00;
weight(2) =  0.446207802167141488805120436457d+00;
weight(3) =  0.623653045951482508163709823153d+00;
weight(4) =  0.562712030298924120384345300681d+00;
weight(5) =  0.287427121582451882646824439708d+00;
elseif( norder == 6 ) ;
xtab(1) =  - 1.0d+00;
xtab(2) =  - 0.802929828402347147753002204224d+00;
xtab(3) =  - 0.390928546707272189029229647442d+00;
xtab(4) =    0.124050379505227711989974959990d+00;
xtab(5) =    0.603973164252783654928415726409d+00;
xtab(6) =    0.920380285897062515318386619813d+00;
weight(1) =  0.555555555555555555555555555556d-01;
weight(2) =  0.319640753220510966545779983796d+00;
weight(3) =  0.485387188468969916159827915587d+00;
weight(4) =  0.520926783189574982570229406570d+00;
weight(5) =  0.416901334311907738959406382743d+00;
weight(6) =  0.201588385253480840209200755749d+00;
elseif( norder == 7 ) ;
xtab(1) =  - 1.0d+00;
xtab(2) =  - 0.853891342639482229703747931639d+00;
xtab(3) =  - 0.538467724060109001833766720231d+00;
xtab(4) =  - 0.117343037543100264162786683611d+00;
xtab(5) =    0.326030619437691401805894055838d+00;
xtab(6) =    0.703842800663031416300046295008d+00;
xtab(7) =    0.941367145680430216055899446174d+00;
weight(1) =  0.408163265306122448979591836735d-01;
weight(2) =  0.239227489225312405787077480770d+00;
weight(3) =  0.380949873644231153805938347876d+00;
weight(4) =  0.447109829014566469499348953642d+00;
weight(5) =  0.424703779005955608398308039150d+00;
weight(6) =  0.318204231467301481744870434470d+00;
weight(7) =  0.148988471112020635866497560418d+00;
elseif( norder == 8 ) ;
xtab(1) =  - 1.0d+00;
xtab(2) =  - 0.887474878926155707068695617935d+00;
xtab(3) =  - 0.639518616526215270024840114382d+00;
xtab(4) =  - 0.294750565773660725252184459658d+00;
xtab(5) =    0.943072526611107660028971153047d-01;
xtab(6) =    0.468420354430821063046421216613d+00;
xtab(7) =    0.770641893678191536180719525865d+00;
xtab(8) =    0.955041227122575003782349000858d+00;
weight(1) =  0.03125d+00;
weight(2) =  0.185358154802979278540728972699d+00;
weight(3) =  0.304130620646785128975743291400d+00;
weight(4) =  0.376517545389118556572129261442d+00;
weight(5) =  0.391572167452493593082499534004d+00;
weight(6) =  0.347014795634501280228675918422d+00;
weight(7) =  0.249647901329864963257869293513d+00;
weight(8) =  0.114508814744257199342353728520d+00;
elseif( norder == 9 ) ;
xtab(1) =  - 1.0d+00;
xtab(2) =  - 0.910732089420060298533757956283d+00;
xtab(3) =  - 0.711267485915708857029562959544d+00;
xtab(4) =  - 0.426350485711138962102627520502d+00;
xtab(5) =  - 0.903733696068532980645444599064d-01;
xtab(6) =    0.256135670833455395138292079035d+00;
xtab(7) =    0.571383041208738483284917464837d+00;
xtab(8) =    0.817352784200412087992517083851d+00;
xtab(9) =    0.964440169705273096373589797925d+00;
weight(1) =  0.246913580246913580246913580247d-01;
weight(2) =  0.147654019046315385819588499802d+00;
weight(3) =  0.247189378204593052361239794969d+00;
weight(4) =  0.316843775670437978338000849642d+00;
weight(5) =  0.348273002772966594071991031186d+00;
weight(6) =  0.337693966975929585803724239792d+00;
weight(7) =  0.286386696357231171146705637752d+00;
weight(8) =  0.200553298024551957421165090417d+00;
weight(9) =  0.907145049232829170128934984159d-01;
elseif( norder == 10 ) ;
xtab(1) =  - 1.0d+00;
xtab(2) =  - 0.927484374233581078117671398464d+00;
xtab(3) =  - 0.763842042420002599615429776011d+00;
xtab(4) =  - 0.525646030370079229365386614293d+00;
xtab(5) =  - 0.236234469390588049278459503207d+00;
xtab(6) =    0.760591978379781302337137826389d-01;
xtab(7) =    0.380664840144724365880759065541d+00;
xtab(8) =    0.647766687674009436273648507855d+00;
xtab(9) =    0.851225220581607910728163628088d+00;
xtab(10) =   0.971175180702246902734346518378d+00;
weight(1) =  0.02d+00;
weight(2) =  0.120296670557481631517310522702d+00;
weight(3) =  0.204270131879000675555788672223d+00;
weight(4) =  0.268194837841178696058554475262d+00;
weight(5) =  0.305859287724422621016275475401d+00;
weight(6) =  0.313582457226938376695902847302d+00;
weight(7) =  0.290610164832918311146863077963d+00;
weight(8) =  0.239193431714379713376571966160d+00;
weight(9) =  0.164376012736921475701681668908d+00;
weight(10) = 0.736170054867584989310512940790d-01;
elseif( norder == 11 ) ;
xtab(1) =  - 1.0d+00;
xtab(2) =  - 0.939941935677027005913871284731d+00;
xtab(3) =  - 0.803421975580293540697597956820d+00;
xtab(4) =  - 0.601957842073797690275892603234d+00;
xtab(5) =  - 0.351888923353330214714301017870d+00;
xtab(6) =  - 0.734775314313212657461903554238d-01;
xtab(7) =    0.210720306228426314076095789845d+00;
xtab(8) =    0.477680647983087519467896683890d+00;
xtab(9) =    0.705777100713859519144801128840d+00;
xtab(10) =   0.876535856245703748954741265611d+00;
xtab(11) =   0.976164773135168806180508826082d+00;
weight(1) =  0.165289256198347107438016528926d-01;
weight(2) =  0.998460819079680638957534695802d-01;
weight(3) =  0.171317619206659836486712649042d+00;
weight(4) =  0.228866123848976624401683231126d+00;
weight(5) =  0.267867086189684177806638163355d+00;
weight(6) =  0.285165563941007337460004408915d+00;
weight(7) =  0.279361333103383045188962195720d+00;
weight(8) =  0.250925377697128394649140267633d+00;
weight(9) =  0.202163108540024418349931754266d+00;
weight(10) = 0.137033682133202256310153880580d+00;
weight(11) = 0.609250978121311347072183268883d-01;
elseif( norder == 12 ) ;
xtab(1) =  - 1.0d+00;
xtab(2) =  - 0.949452759204959300493337627077d+00;
xtab(3) =  - 0.833916773105189706586269254036d+00;
xtab(4) =  - 0.661649799245637148061133087811d+00;
xtab(5) =  - 0.444406569781935851126642615609d+00;
xtab(6) =  - 0.196994559534278366455441427346d+00;
xtab(7) =    0.637247738208319158337792384845d-01;
xtab(8) =    0.319983684170669623532789532206d+00;
xtab(9) =    0.554318785912324288984337093085d+00;
xtab(10) =   0.750761549711113852529400825472d+00;
xtab(11) =   0.895929097745638894832914608454d+00;
xtab(12) =   0.979963439076639188313950540264d+00;
weight(1) =  0.138888888888888888888888888888d-01;
weight(2) =  0.841721349386809762415796536813d-01;
weight(3) =  0.145563668853995128522547654706d+00;
weight(4) =  0.196998534826089634656049637969d+00;
weight(5) =  0.235003115144985839348633985940d+00;
weight(6) =  0.256991338152707776127974253598d+00;
weight(7) =  0.261465660552133103438074715743d+00;
weight(8) =  0.248121560804009959403073107079d+00;
weight(9) =  0.217868879026192438848747482023d+00;
weight(10) = 0.172770639313308564306065766966d+00;
weight(11) = 0.115907480291738392750341908272d+00;
weight(12) = 0.512480992072692974680229451351d-01;
elseif( norder == 13 ) ;
xtab(1) =  - 1.0d+00;
xtab(2) =  - 0.956875873668299278183813833834d+00;
xtab(3) =  - 0.857884202528822035697620310269d+00;
xtab(4) =  - 0.709105087529871761580423832811d+00;
xtab(5) =  - 0.519197779050454107485205148087d+00;
xtab(6) =  - 0.299201300554509985532583446686d+00;
xtab(7) =  - 0.619016986256353412578604857936d-01;
xtab(8) =    0.178909837597084635021931298881d+00;
xtab(9) =    0.409238231474839556754166331248d+00;
xtab(10) =   0.615697890940291918017885487543d+00;
xtab(11) =   0.786291018233046684731786459135d+00;
xtab(12) =   0.911107073689184553949066402429d+00;
xtab(13) =   0.982921890023145161262671078244d+00;
weight(1) =  0.118343195266272189349112426036d-01;
weight(2) =  0.719024162924955289397537405641d-01;
weight(3) =  0.125103834331152358133769287976d+00;
weight(4) =  0.171003460470616642463758674512d+00;
weight(5) =  0.206960611455877074631132560829d+00;
weight(6) =  0.230888862886995434012203758668d+00;
weight(7) =  0.241398342287691148630866924129d+00;
weight(8) =  0.237878547660712031342685189180d+00;
weight(9) =  0.220534229288451464691077164199d+00;
weight(10) = 0.190373715559631732254759820746d+00;
weight(11) = 0.149150950090000205151491864242d+00;
weight(12) = 0.992678068818470859847363877478d-01;
weight(13) = 0.437029032679020748288533846051d-01;
elseif( norder == 14 ) ;
xtab(1) =  - 1.0d+00;
xtab(2) =  - 0.962779269978024297120561244319d+00;
xtab(3) =  - 0.877048918201462024795266773531d+00;
xtab(4) =  - 0.747389642613378838735429134263d+00;
xtab(5) =  - 0.580314056546874971105726664999d+00;
xtab(6) =  - 0.384202003439203313794083903375d+00;
xtab(7) =  - 0.168887928042680911008441695622d+00;
xtab(8) =    0.548312279917645496498107146428d-01;
xtab(9) =    0.275737205435522399182637403545d+00;
xtab(10) =   0.482752918588474966820418534355d+00;
xtab(11) =   0.665497977216884537008955042481d+00;
xtab(12) =   0.814809550601994729434217249123d+00;
xtab(13) =   0.923203722520643299246334950272d+00;
xtab(14) =   0.985270697947821356698617003172d+00;
weight(1) =  0.102040816326530612244897959184d-01;
weight(2) =  0.621220169077714601661329164668d-01;
weight(3) =  0.108607722744362826826720935229d+00;
weight(4) =  0.149620539353121355950520836946d+00;
weight(5) =  0.183127002125729654123867302103d+00;
weight(6) =  0.207449763335175672668082886489d+00;
weight(7) =  0.221369811499570948931671683021d+00;
weight(8) =  0.224189348002707794238414632220d+00;
weight(9) =  0.215767100604618851381187446115d+00;
weight(10) = 0.196525518452982430324613091930d+00;
weight(11) = 0.167429727891086278990102277038d+00;
weight(12) = 0.129939668737342347807425737146d+00;
weight(13) = 0.859405354429804030893077310866d-01;
weight(14) = 0.377071632698969142774627282919d-01;
elseif( norder == 15 ) ;
xtab(1) =  - 1.0d+00;
xtab(2) =  - 0.967550468197200476562456018282d+00;
xtab(3) =  - 0.892605400120550767066811886849d+00;
xtab(4) =  - 0.778685617639031079381743321893d+00;
xtab(5) =  - 0.630779478886949283946148437224d+00;
xtab(6) =  - 0.455352905778529370872053455981d+00;
xtab(7) =  - 0.260073376740807915768961188263d+00;
xtab(8) =  - 0.534757226797460641074538896258d-01;
xtab(9) =    0.155410685384859484319182024964d+00;
xtab(10) =   0.357456512022127651195319205174d+00;
xtab(11) =   0.543831458701484016930711802760d+00;
xtab(12) =   0.706390264637572540152679669478d+00;
xtab(13) =   0.838029000636089631215097384520d+00;
xtab(14) =   0.932997190935973719928072142859d+00;
xtab(15) =   0.987166478414363086378359071811d+00;
weight(1) =  0.888888888888888888888888888889d-02;
weight(2) =  0.542027800486444943382142368018d-01;
weight(3) =  0.951295994604808992038477266346d-01;
weight(4) =  0.131875462504951632186262157944d+00;
weight(5) =  0.162854477303832629448732245828d+00;
weight(6) =  0.186715145839450908083795103799d+00;
weight(7) =  0.202415187030618429872703310435d+00;
weight(8) =  0.209268608147694581430889790306d+00;
weight(9) =  0.206975960249553755479027321787d+00;
weight(10) = 0.195637503045116116473556617575d+00;
weight(11) = 0.175748872642447685670310440476d+00;
weight(12) = 0.148179527003467253924682058743d+00;
weight(13) = 0.114135203489752753013075582569d+00;
weight(14) = 0.751083927605064397329716653914d-01;
weight(15) = 0.328643915845935322530428528231d-01;
else;
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'RADAU_SET - Fatal error!');
writef(1,['%s','%6i','\n'], '  Illegal value of NORDER = ', norder);
writef(1,['%s','\n'], '  Legal values are 1 to 15.');
error(['stop encountered in original fortran code  ',char(10),';']);
end;
return;
end
function [a, b, c, d, norder, x, w]=rule_adjust( a, b, c, d, norder, x, w );
%*******************************************************************************
%
%! RULE_ADJUST maps a quadrature rule from [A,B] to [C,D].
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
%    Input, real ( kind = 8 ) A, B, the endpoints of the definition interval.
%
%    Input, real ( kind = 8 ) C, D, the endpoints of the integration interval.
%
%    Input, integer NORDER, the number of abscissas and weights.
%
%    Input/output, real ( kind = 8 ) X(NORDER), W(NORDER), the abscissas
%    and weights.
%

x([1:norder]) =(( b - x([1:norder]) ) .* c +( x([1:norder]) - a ) .* d )./( b - a );
w([1:norder]) =(( d - c ) ./( b - a ) ) .* w([1:norder]);
return;
end
function [func, norder, xtab, weight, result]=summer( func, norder, xtab, weight, result );
%*******************************************************************************
%
%! SUMMER carries out a quadrature rule over a single interval.
%
%  Formula:
%
%    RESULT = sum ( 1 <= I <= NORDER ) WEIGHT(I) * FUNC ( XTAB(I) )
%
%  Modified:
%
%    16 September 1998
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, external FUNC, the name of the FORTRAN function which
%    evaluates the integrand.  The function must have the form
%      function func ( x ).
%
%    Input, integer NORDER, the order of the rule.
%
%    Input, real ( kind = 8 ) XTAB(NORDER), the abscissas of the rule.
%
%    Input, real ( kind = 8 ) WEIGHT(NORDER), the weights of the rule.
%
%    Output, real ( kind = 8 ) RESULT, the approximate value of the integral.
%
persistent i ; 

if isempty(i), i=0; end;
if( norder < 1 )
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'SUMMER - Fatal error!');
writef(1,['%s','\n'], '  NORDER must be at least 1.');
writef(1,['%s','%6i','\n'], '  The input value was NORDER = ', norder);
error(['stop encountered in original fortran code  ',char(10),';']);
end;
result = 0.0d+00;
for i = 1: norder;
result = result + weight(i) .* func( xtab(i) );
end; i = fix(norder+1);
return;
end
function [func, norderg, weightg, resultg, norderk, xtabk,weightk, resultk]=summer_gk( func, norderg, weightg, resultg, norderk, xtabk,weightk, resultk );
%*******************************************************************************
%
%! SUMMER_GK carries out Gauss-Kronrod quadrature over a single interval.
%
%  Discussion:
%
%    The abscissas for the Gauss-Legendre rule of order NORDERG are
%    not required, since they are assumed to be the even-indexed
%    entries of the corresponding Kronrod rule.
%
%  Modified:
%
%    16 September 1998
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, external FUNC, the name of the FORTRAN function which
%    evaluates the integrand.  The function must have the form
%      function func ( x ).
%
%    Input, integer NORDERG, the order of the Gauss-Legendre rule.
%
%    Input, real ( kind = 8 ) WEIGHTG(NORDERG), the weights of the
%    Gauss-Legendre rule.
%
%    Output, real ( kind = 8 ) RESULTG, the approximate value of the
%    integral, based on the Gauss-Legendre rule.
%
%    Input, integer NORDERK, the order of the Kronrod rule.  NORDERK
%    must equal 2 * NORDERG + 1.
%
%    Input, real ( kind = 8 ) XTABK(NORDERK), the abscissas of the Kronrod rule.
%
%    Input, real ( kind = 8 ) WEIGHTK(NORDERK), the weights of the Kronrod rule.
%
%    Output, real ( kind = 8 ) RESULTK, the approximate value of the integral,
%    based on the Kronrod rule.
%
persistent fk i ; 

if isempty(fk), fk=0; end;
if isempty(i), i=0; end;
if( norderk ~= 2 .* norderg + 1 )
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'SUMMER_GK - Fatal error!');
writef(1,['%s','\n'], '  NORDERK must equal 2 * NORDERG + 1.');
writef(1,['%s','%6i','\n'], '  The input value was NORDERG = ', norderg);
writef(1,['%s','%6i','\n'], '  The input value was NORDERK = ', norderk);
error(['stop encountered in original fortran code  ',char(10),';']);
end;
resultg = 0.0d+00;
resultk = 0.0d+00;
for i = 1: norderk;
fk = func( xtabk(i) );
resultk = resultk + weightk(i) .* fk;
if( rem( i, 2 ) == 0 )
resultg = resultg + (weightg(fix(i./2)) .* fk);
end;
end; i = fix(norderk+1);
return;
end
function [func, a, b, nsub, norder, xlo, xhi, xtab, weight, result]=sum_sub( func, a, b, nsub, norder, xlo, xhi, xtab, weight, result );
%*******************************************************************************
%
%! SUM_SUB carries out a composite quadrature rule.
%
%  Discussion:
%
%    SUM_SUB assumes the original rule was written for [XLO,XHI].
%
%  Integration interval:
%
%    [ A, B ]
%
%  Integral to approximate:
%
%    Integral ( A <= X <= B ) F(X) dX
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
%    Input, external FUNC, the name of the FORTRAN function which
%    evaluates the integrand.  The function must have the form
%      function func ( x ).
%
%    Input, real ( kind = 8 ) A, B, the lower and upper limits of integration.
%
%    Input, integer NSUB, the number of equal subintervals into
%    which the finite interval (A,B) is to be subdivided for
%    higher accuracy.  NSUB must be at least 1.
%
%    Input, integer NORDER, the order of the rule.
%    NORDER must be at least 1.
%
%    Input, real ( kind = 8 ) XLO, XHI, the left and right endpoints of
%    the interval over which the quadrature rule was defined.
%
%    Input, real ( kind = 8 ) XTAB(NORDER), the abscissas of a quadrature
%    rule for the interval [XLO,XHI].
%
%    Input, real ( kind = 8 ) WEIGHT(NORDER), the weights of the
%    quadrature rule.
%
%    Output, real ( kind = 8 ) RESULT, the approximate value of the integral.
%
persistent a_sub b_sub h i j quad_sub result_sub volume volume_sub x xmid ; 

if isempty(a_sub), a_sub=0; end;
if isempty(b_sub), b_sub=0; end;
if isempty(h), h=0; end;
if isempty(i), i=0; end;
if isempty(j), j=0; end;
if isempty(quad_sub), quad_sub=0; end;
if isempty(result_sub), result_sub=0; end;
if isempty(x), x=0; end;
if isempty(xmid), xmid=0; end;
if isempty(volume), volume=0; end;
if isempty(volume_sub), volume_sub=0; end;
if( a == b )
result = 0.0d+00;
return;
end;
if( norder < 1 )
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'SUM_SUB - Fatal error!');
writef(1,['%s','%6i','\n'], '  Nonpositive value of NORDER = ', norder);
error(['stop encountered in original fortran code  ',char(10),';']);
end;
if( nsub < 1 )
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'SUM_SUB - Fatal error!');
writef(1,['%s','%6i','\n'], '  Nonpositive value of NSUB = ', nsub);
error(['stop encountered in original fortran code  ',char(10),';']);
end;
if( xlo == xhi )
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'SUM_SUB - Fatal error!');
writef(1,['%s','\n'], '  XLO = XHI.');
error(['stop encountered in original fortran code  ',char(10),';']);
end;
volume = 0.0d+00;
result = 0.0d+00;
for j = 1: nsub;
a_sub =( real( nsub - j + 1) .* a+ real(        j - 1) .* b )./ real( nsub);
b_sub =( real( nsub - j)     .* a+ real(        j)     .* b )./ real( nsub);
quad_sub = 0.0d+00;
for i = 1: norder;
x =(( xhi - xtab(i) ) .* a_sub +( xtab(i) - xlo ) .* b_sub )./( xhi - xlo );
quad_sub = quad_sub + weight(i) .* func( x );
end; i = fix(norder+1);
volume_sub =( b - a ) ./(( xhi - xlo ) .* real( nsub) );
result_sub = quad_sub .* volume_sub;
volume = volume + volume_sub;
result = result + result_sub;
end; j = fix(nsub+1);
return;
end
function [func, a, b, nsub, norderg, weightg, resultg, norderk,xtabk, weightk, resultk, error]=sum_sub_gk( func, a, b, nsub, norderg, weightg, resultg, norderk,xtabk, weightk, resultk, error );
%*******************************************************************************
%
%! SUM_SUB_GK carries out a composite Gauss-Kronrod rule.
%
%  Integration interval:
%
%    [ A, B ]
%
%  Integral to approximate:
%
%    Integral ( A <= X <= B ) F(X) dX
%
%  Approximate integral:
%
%    H = ( B - A ) / NSUB
%    XMID(J) = A + 0.5 * H * ( 2 * J - 1 )
%
%    Sum ( 1 <= J <= NSUB )
%      Sum ( 1 <= I <= NORDERK )
%        WEIGHTK(I) * F ( XMID(J) + 0.5 * H * XTABK(I) )
%
%  Note:
%
%    The Gauss-Legendre weights should be computed by LEGCOM or LEGSET.
%    The Kronrod abscissas and weights should be computed by KRONSET.
%
%    The orders of the Gauss-Legendre and Kronrod rules must satisfy
%    NORDERK = 2 * NORDERG + 1.
%
%    The Kronrod rule uses the abscissas of the Gauss-Legendre rule,
%    plus more points, resulting in an efficient and higher order estimate.
%
%    The difference between the Gauss-Legendre and Kronrod estimates
%    is taken as an estimate of the error in the approximation to the
%    integral.
%
%  Modified:
%
%    15 September 1998
%
%  Author:
%
%    John Burkardt
%
%  Parameters:
%
%    Input, external FUNC, the name of the FORTRAN function which
%    evaluates the integrand.  The function must have the form
%      function func ( x ).
%
%    Input, real ( kind = 8 ) A, B, the lower and upper limits of integration.
%
%    Input, integer NSUB, the number of equal subintervals into
%    which the finite interval (A,B) is to be subdivided for
%    higher accuracy.  NSUB must be at least 1.
%
%    Input, integer NORDERG, the order of the Gauss-Legendre rule.
%    NORDERG must be at least 1.
%
%    Input, real ( kind = 8 ) WEIGHTG(NORDERG), the weights of the
%    Gauss-Legendre rule.
%
%    Output, real ( kind = 8 ) RESULTG, the approximate value of the
%    integral based on the Gauss-Legendre rule.
%
%    Input, integer NORDERK, the order of the Kronrod rule.
%    NORDERK must be at least 1.
%
%    Input, real ( kind = 8 ) XTABK(NORDERK), the abscissas of the
%    Kronrod rule.
%
%    Input, real ( kind = 8 ) WEIGHTK(NORDERK), the weights of the
%    Kronrod rule.
%
%    Output, real ( kind = 8 ) RESULTK, the approximate value of the
%    integral based on the Kronrod rule.
%
%    Output, real ( kind = 8 ) ERROR, an estimate of the approximation
%    error.  This is computed by taking the sum of the absolute values of
%    the differences between the Gauss-Legendre and Kronrod rules
%    over each subinterval.  This is usually a good estimate of
%    the error in the value RESULTG.  The error in the Kronrod
%    estimate RESULTK is usually much smaller.
%
persistent fk h i j partg partk x xmid ; 

if isempty(fk), fk=0; end;
if isempty(h), h=0; end;
if isempty(i), i=0; end;
if isempty(j), j=0; end;
if isempty(partg), partg=0; end;
if isempty(partk), partk=0; end;
if isempty(x), x=0; end;
if isempty(xmid), xmid=0; end;
resultg = 0.0d+00;
resultk = 0.0d+00;
error = 0;
if( a == b )
return;
end;
if( norderk ~= 2 .* norderg + 1 )
writef(1,['%s','\n'], ' ');
writef(1,['%s','\n'], 'SUM_SUB_GK - Fatal error!');
writef(1,['%s','\n'], '  NORDERK must equal 2 * NORDERG + 1.');
writef(1,['%s','%6i','\n'], '  The input value was NORDERG = ', norderg);
writef(1,['%s','%6i','\n'], '  The input value was NORDERK = ', norderk);
error(['stop encountered in original fortran code  ',char(10),';']);
end;
h =( b - a ) ./ real( nsub);
for j = 1: nsub;
xmid = a + 0.5d0 .* h .* real( 2 .* j - 1);
partg = 0.0d+00;
partk = 0.0d+00;
for i = 1: norderk;
x = xmid + 0.5d0 .* h .* xtabk(i);
fk = func( x );
partk = partk + 0.5d0 .* h .* weightk(i) .* fk;
if( rem( i, 2 ) == 0 )
partg = partg + (((0.5d0 .* h) .* weightg(fix(i./2))) .* fk);
end;
end; i = fix(norderk+1);
resultg = resultg + partg;
resultk = resultk + partk;
error = error + abs( partk - partg );
end; j = fix(nsub+1);
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