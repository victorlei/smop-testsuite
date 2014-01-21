function [fn_val,xvalue]=alngam(xvalue) 

%     ALGORITHM AS245  APPL. STATIST. (1989) VOL. 38, NO. 2

%     Calculation of the logarithm of the gamma function

% Latest revision - 29 November 1997

% N.B. Argument IFAULT has been removed

clear global; clear functions;

fn_val=[];
persistent alr2pi dp four half one onep5 r1 r2 r3 r4 twelve x x1 x2 xlge xlgst y zero ; 

if isempty(dp), dp = 8; end;
if isempty(fn_val), fn_val=0; end;

% Local variables
if isempty(x), x=0; end;
if isempty(x1), x1=0; end;
if isempty(x2), x2=0; end;
if isempty(y), y=0; end;

%     Coefficients of rational functions

if isempty(r1), r1([1:9]) =[ -2.66685511495, -24.4387534237,-21.9698958928,  11.1667541262,3.13060547623,  0.607771387771,11.9400905721,  31.4690115749,15.2346874070 ]; end;
if isempty(r2), r2([1:9]) =[ -78.3359299449, -142.046296688,137.519416416,  78.6994924154,4.16438922228,  47.0668766060,313.399215894,  263.505074721,43.3400022514 ]; end;
if isempty(r3), r3([1:9]) =[ -2.12159572323e5,  2.30661510616e5,2.74647644705e4, -4.02621119975e4,-2.29660729780e3, -1.16328495004e5,-1.46025937511e5, -2.42357409629e4,-5.70691009324e2 ]; end;
if isempty(r4), r4([1:5]) =[ 0.279195317918525, 0.4917317610505968,0.0692910599291889, 3.350343815022304,6.012459259764103 ]; end;

%     Fixed constants

if isempty(alr2pi), alr2pi = 0.918938533204673; end;
if isempty(four), four = 4.; end;
if isempty(half), half = 0.5; end;
if isempty(one), one = 1.; end;
if isempty(onep5), onep5 = 1.5; end;
if isempty(twelve), twelve = 12.; end;
if isempty(zero), zero = 0.; end;

%     Machine-dependant constants.
%     A table of values is given at the top of page 399 of the paper.
%     These values are for the IEEE double-precision format for which
%     B = 2, t = 53 and U = 1023 in the notation of the paper.

if isempty(xlge), xlge = 5.10e6; end;
if isempty(xlgst), xlgst = realmax; end;

x = xvalue;
fn_val = zero;

%     Test for valid function argument

if(x >= xlgst)
writef(1,['%s \n'], 'AS 245: Argument x too large');
return;
end;
if(x <= zero)
writef(1,['%s \n'], 'AS 245: Argument x <= 0');
return;
end;

%     Calculation for 0 < X < 0.5 and 0.5 <= X < 1.5 combined

if(x < onep5)
if(x < half)
fn_val = -log(x);
y = x + one;

%     Test whether X < machine epsilon

if(y == one)
return;
end;
else;
fn_val = zero;
y = x;
x =(x - half) - half;
end;
fn_val = fn_val + x .*((((r1(5).*y + r1(4)).*y + r1(3)).*y + r1(2)).*y + r1(1)) ./((((y + r1(9)).*y + r1(8)).*y+ r1(7)).*y + r1(6));
return;
end;

%     Calculation for 1.5 <= X < 4.0

if(x < four)
y =(x - one) - one;
fn_val = y .*((((r2(5).*x + r2(4)).*x + r2(3)).*x + r2(2)).*x + r2(1)) ./((((x + r2(9)).*x + r2(8)).*x + r2(7)).*x+ r2(6));
return;
end;

%     Calculation for 4.0 <= X < 12.0

if(x < twelve)
fn_val =((((r3(5).*x + r3(4)).*x + r3(3)).*x + r3(2)).*x + r3(1)) ./((((x + r3(9)).*x + r3(8)).*x + r3(7)).*x + r3(6));
return;
end;

%     Calculation for X >= 12.0

y = log(x);
fn_val = x .*(y - one) - half .* y + alr2pi;
if(x > xlge)
return;
end;
x1 = one ./ x;
x2 = x1 .* x1;
fn_val = fn_val + x1 .*((r4(3).*x2 + r4(2)).*x2 + r4(1)) ./((x2 + r4(5)).*x2 + r4(4));
return;
end %function alngam




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