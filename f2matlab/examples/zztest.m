function zztest(varargin)
% compile with intel fortran
clear global; clear functions;

persistent m n rhs seed seed2 x ; 

if isempty(n), n=5; end;
if isempty(m), m=2; end;
if isempty(rhs), rhs=zeros(1,2.*n); end;
if isempty(x), x=zeros(1,n); end;
if isempty(seed), seed = 123456789; end;
if isempty(seed2), seed2=123; end;
rhs(:)=1;
x([1:n]) = rhs([1:n]);
writef(1,['%s %0.15g \n'], 'seed=',seed);
writef(1,['%s %0.15g \n'], 'rhs(1:)=',rhs(:));
for seed=1:10;
writef(1,['%s %0.15g \n'], 'seed=',seed);
end; seed=fix(10+1);
for seed=1:2:10;
writef(1,['%s %0.15g \n'], 'seed=',seed);
end; seed=fix(10+1);
for seed=1:10;
[x(1)]=func1(real(seed).^2);
writef(1,['%s %0.15g \n'], 'x(1)=',x(1));
end; seed=fix(10+1);
writef(1,['%s \n'], '  "Sector1" and "Sector2" are the CIRCLE_SECTOR computations');
end %program zztest
function [func1result, a_new ]=func1( a_new );
func1result=[];
persistent a_old ab1as action catchml counter counter2 counter3 debug err expmlv firstCall fmt fmtf fmti func1 fx_vec g i i1 i2 ipoint ix maxcol maxdig maxnam month my0 my1 my2 my3 n name names numnam onerpi p1 p2 p3 p4 p5 r0 r1 r2 r3 r4 r5 r6 radius1_test rval rvals seed seed2 summlv tryml xi ; if isempty(month),month={};end; if isempty(names),names={};end; if isempty(xi),xi={};end; if isempty(firstCall),firstCall=1;end; 

if isempty(func1result), func1result=0; end;
%! saved
if isempty(a_old), a_old=0; end;
% !saved
if isempty(counter), counter=0; end;
if isempty(counter2), counter2=0; end;
if isempty(counter3), counter3=1; end;
if isempty(i1), i1=0; end;
if isempty(i2), i2=0; end;
if isempty(r0), r0=zeros(10,2); end;
if isempty(r1), r1=zeros(10,1); end;
if isempty(r3), r3=zeros(1,10+1); end;
if isempty(r2), r2([1:10],[1:2])=1; end;
if isempty(r4), r4([1:10],[1:2])=2; end;
if isempty(r5), r5([0:3]+1,[0:5]+1,[-2:5]+3,[-10:4]+11)=-1; end;
if isempty(r6), r6=zeros(3+1,5+1,5+3,4+11); end;
if isempty(p1), p1=3; end;
if isempty(p2), p2=4; end;
a1=[];
if isempty(p3), p3=3; end;
if isempty(p4), p4=4  ; end;
if isempty(p5), p5=4 ; end;
if isempty(radius1_test), radius1_test([1:2]) =[ 0.0, 1.0 ]; end;
% gam= @( y ) (((((((0.035868343d+00   * y- 0.193527818d+00 ) * y+ 0.482199394d+00 ) * y- 0.756704078d+00 ) * y+ 0.918206857d+00 ) * y- 0.897056937d+00 ) * y+ 0.988205891d+00 ) * y- 0.577191652d+00 ) * y + 1.0d+00;real( 8 ) :: gam;
if isempty(g), g=zeros(10,10); end;
if isempty(debug), debug = false; end;
if isempty(month), month = {'January  ', 'February ', 'March    ', 'April    ','May      ', 'June     ', 'July     ', 'August   ','September', 'October  ', 'November ', 'December ' }; end;
if isempty(err), err=zeros(10+1,2); end;
if isempty(fx_vec), fx_vec([1:2]) =[real( 0.87377726306985360531d+00),real( 0.25628287737952698742e-09) ]; end;
if isempty(maxdig), maxdig = 20; end;
if isempty(seed), seed([1:1]) = 123456789; end;
if isempty(seed2), seed2=123; end;
if isempty(maxnam), maxnam = 100; end;
if isempty(maxcol), maxcol = 5; end;
if isempty(action), action=repmat(' ',1,10); end;
if isempty(i), i=0; end;
if isempty(ipoint), ipoint([1:maxnam]) =[ 0+[ 1: maxnam ] ]; end;
if isempty(name), name=repmat(' ',1,10); end;
for  i =( 1):( maxnam ),  if isempty(names), names ={  ' ' }; end; end;
if isempty(numnam), numnam = 0; end;
if isempty(rval), rval=0; end;
if isempty(rvals), rvals([1:maxnam],[1:maxcol]) =reshape([ones(size([ 1: maxnam .* maxcol ])).*( 0.0d+00) ],[ maxnam, maxcol ] ); end;
if isempty(summlv), summlv=zeros(1,p1); end;
if isempty(expmlv), expmlv=0; end;
if isempty(tryml), tryml=0; end;
if isempty(catchml), catchml=0; end;
if isempty(ab1as), ab1as=zeros(1,27+1); end;
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
if isempty(my0), my0=0; end;
if isempty(my1), my1=0; end;
if isempty(my2), my2=0; end;
if isempty(my3), my3=0; end;
if firstCall,   my0=[12];  end;
if firstCall, my1=[12];  end;
if firstCall, my2=[8];  end;
if firstCall, my3=[8];  end;
if isempty(fmt), fmt=repmat(' ',1,34); end;
if isempty(fmtf), fmtf=repmat(' ',1,34); end;
if isempty(fmti), fmti=repmat(' ',1,34); end;
if isempty(n), n=9 ; end;
if isempty(xi), xi=cell(9,1); end;
if isempty(ix), ix=zeros(9,1); end;
firstCall=0;
%DATA (xi(i,1),i=1,N)/'AC' , 'AZ' , 'AD' , 'AA' , 'AB' , 'ZZ' ,%'ZA' , 'ZX' , 'ZY'/
%DATA (ix(i,1),i=1,N)/4 , 5 , 1 , 3 , 2 , 7 , 8 , 9 , 6/
gam= @( y ) (((((((0.035868343d+00   .* y- 0.193527818d+00 ) .* y+ 0.482199394d+00 ) .* y- 0.756704078d+00 ) .* y+ 0.918206857d+00 ) .* y- 0.897056937d+00 ) .* y+ 0.988205891d+00 ) .* y- 0.577191652d+00 ) .* y + 1.0d+00;
fmt([1:20]) = '(1X, 6X, 4H   (,E30.';
fmt([23:34]) = ',1H,,I8,1H))';
fmtf([1:20]) = '(1X,F6.1,4H   (,E30.';
fmtf([23:34]) = ',1H,,I8,1H))';
fmti([1:20]) = '(1X, I6, 4H   (,E30.';
fmti([23:34]) = ',1H,,I8,1H))';
tryml=10;
catchml=fix(tryml.*10);
summlv([2:-1:2]) = 20;
expmlv=2.1;
writef(1,['%s %0.15g \n'], 'exp=',expmlv);
%print *,'exp(2.0)=',exp(2.0)
if( (strlexcmp( '1',  '0' ) >= 0) && (strlexcmp( 'a',  '9' ) <= 0) )
end;
rand('seed',sum(100000.*clock))
%write ( *, '(a,i6)' ) '    The value occurs in index ', index
writef(1,[repmat(' ',1,2),'%6i',repmat([repmat('%14.6f',1,5)] ,1,1),'\n'], maxdig, g([1:5],1));
writef(1,['%s %s \n'], 'names(1)=',names{1});
err(:)=20;
writef(1,['%s %s \n'],  month{1},month{2});
writef(1,['%s %s \n'], month{1},month{2});
g( 1, 1) =   1.0d+00;
rand('seed',seed);
seed=fix(rand('seed'));
rand('seed',sum(100000.*clock))
a_old = a_old+a_new;
counter = fix(counter+1);
writef(1,['%s %0.15g \n'], 'a_old=',a_old);
func1result=counter;
writef(1,['%s %0.15g \n'], 'shape(r1)=',size(r1));
writef(1,['%s %0.15g \n'], 'shape(r2)=',size(r2));
writef(1,['%s %0.15g \n'], 'shape(r3)=',size(r3));
writef(1,['%s %0.15g \n'], 'shape(err)=',size(err));
csnil=dbstack(1); csnil=csnil(1).name(1)~='@';
if csnil&&~isempty(inputname(1)), assignin('caller','FUntemp',a_new); evalin('caller',[inputname(1),'=FUntemp;']); end
end %function func1
function [d_piresult]=d_pi( );
d_piresult=[];

;
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
%  real ( kind = 8 ) d_pi
d_piresult = 3.141592653589793d+00;
return;
end




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%% extra functions as needed by the translation %%%%%%%%%%%

function tf = strlexcmp(a, b)
%STRLEXCMP Lexicographic comparison of two strings.
%
%   STRLEXCMP(A, B) returns -1, 0, or 1 depending on whether the left argument
%   is stringwise less than, equal to, or greater than the right argument.
%
%   This is a MATLAB version of the Perl `cmp' operator.
%
%   See also EQ, ISEQUAL.

%   Author:      Peter J. Acklam
%   Time-stamp:  2004-09-22 19:49:47 +0200
%   E-mail:      pjacklam@online.no
%   URL:         http://home.online.no/~pjacklam

% check arguments
error(nargchk(2, 2, nargin));
if ~ischar(a) || ~ischar(b)
error('Both arguments must be char arrays (strings).');
end

% get lengths of strings
na = length(a);
nb = length(b);
n = min(na, nb);

% find characters that differ
k = find(a(1:n) ~= b(1:n));
if isempty(k)
% all characters are identical -- compare lengths
tf = sign(na - nb);
else
% compare first character that is different
k = k(1);
tf = sign(a(k) - b(k));
end
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