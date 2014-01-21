function mcpbdn(varargin)

%From the book 'Computation of Special Functions.'
% by Shanjie Zhang, and Jianming Jin, New York : Wiley, c1996.
%       =============================================================
%       Purpose: This program computes parabolic cylinder functions
%                Dn(z) for an integer order and a complex argument
%                using subroutine CPBDN
%       Input :  x --- Real part of z
%                y --- Imaginary part of z
%                n --- Order of Dn(z)
%       Output:  CPB(|n|) --- Dn(z)
%                CPD(|n|) --- Dn'(z)
%       Example:
%                z = 5.0+ 5.0 i
%     n     Re[Dn(z)]      Im[Dn(z)]      Re[Dn'(z)]     Im[Dn'(z)]
%   -----------------------------------------------------------------
%     0   .99779828D+00  .66321897D-01 -.23286910D+01 -.26603004D+01
%     1   .46573819D+01  .53206009D+01  .26558457D+01 -.24878635D+02
%     2  -.43138931D+01  .49823592D+02  .14465848D+03 -.10313305D+03
%     3  -.28000219D+03  .21690729D+03  .12293320D+04  .30720802D+03
%     4  -.24716057D+04 -.46494526D+03  .38966424D+04  .82090067D+04
%    -1   .10813809D+00 -.90921592D-01 -.50014908D+00 -.23280660D-01
%    -2   .24998820D-02 -.19760577D-01 -.52486940D-01  .47769856D-01
%    -3  -.15821033D-02 -.23090595D-02 -.68249161D-03  .10032670D-01
%    -4  -.37829961D-03 -.10158757D-03  .89032322D-03  .11093416D-02
%       =============================================================
clear global; clear functions;

persistent cpb cpd ; 
n=[];z=[];
if isempty(cpb), cpb=zeros(1,100+1); end;
if isempty(cpd), cpd=zeros(1,100+1); end;
writef(1,['%s \n'],'Please enter n, x and y ');
% READ(*,*)N,X,Y
% WRITE(*,20)N,X,Y
n=5;
x=5;
y=5;
z=complex(x,y);
n0=abs(n);
[n,z,cpb,cpd]=cpbdn(n,z,cpb,cpd);
writef(1,['%0.15g \n']);
if(n >= 0)
writef(1,['%s %s \n'],'  n     Re[Dn(z)]       Im[Dn(z)]       ','Re[Dn''(Z)]      IM[DN''(Z)]');
else;
writef(1,['%s %s \n'],' -n     Re[Dn(z)]       Im[Dn(z)]       ','Re[Dn''(Z)]      IM[DN''(Z)]');
writef(1,['%s %s \n'], ' -n     Re[Dn(z)]       Im[Dn(z)]       ','Re[Dn''(Z)]      IM[DN''(Z)]');
end;
writef(1,['%s %s \n'],'-------------------------------------------','-------------------------');
for  i=0:n0;
writef(1,[repmat(' ',1,1),'%3i',repmat('%16.8f',1,4) ' \n'],i,cpb(i+1+1),cpd(i+1+1));
end;  i=n0+1;
%format(1x,i3,4d16.8);
%format(1X,'N =',i3,',   z =x+iy :',f6.2,'+',f6.2,' i');
end %program mcpbdn
function [n,z,cpb,cpd]=cpbdn(n,z,cpb,cpd);
%       ==================================================
%       Purpose: Compute the parabolic cylinder functions
%                 Dn(z) and Dn'(z) for a complex argument
%       Input:   z --- Complex argument of Dn(z)
%                n --- Order of Dn(z)  ( n=0,�,�,��)
%       Output:  CPB(|n|) --- Dn(z)
%                CPD(|n|) --- Dn'(z)
%       Routines called:
%            (1) CPDSA for computing Dn(z) for a small |z|
%            (2) CPDLA for computing Dn(z) for a large |z|
%       ==================================================
z1=[];cf1=[];n0=[];cfa=[];n1=[];cfb=[];
pi=3.141592653589793d0;
x=real(z);
a0=abs(z);
c0=complex(0.0d0,0.0d0);
ca0=exp(-0.25d0.*z.*z);
if(n >= 0)
cf0=ca0;
cf1=z.*ca0;
cpb(0+1+1)=cf0;
cpb(1+1+1)=cf1;
for  k=2:n;
cf=z.*cf1-(k-1.0d0).*cf0;
cpb(k+1+1)=cf;
cf0=cf1;
cf1=cf;
end;  k=n+1;
else;
n0=-n;
if(x <= 0.0||abs(z) == 0.0)
cf0=ca0;
cpb(0+1+1)=cf0;
z1=-z;
if(a0 <= 7.0)
[dumvar1,z1,cf1]=cpdsa(-1,z1,cf1);
else;
[dumvar1,z1,cf1]=cpdla(-1,z1,cf1);
end;
cf1=sqrt(2.0d0.*pi)./ca0-cf1;
cpb(1+1+1)=cf1;
for  k=2:n0;
cf=(-z.*cf1+cf0)./(k-1.0d0);
cpb(k+1+1)=cf;
cf0=cf1;
cf1=cf;
end;  k=n0+1;
else;
if(a0 <= 3.0)
[dumvar1,z,cfa]=cpdsa(-n0,z,cfa);
cpb(n0+1+1)=cfa;
n1=n0+1;
[dumvar1,z,cfb]=cpdsa(-n1,z,cfb);
cpb(n1+1+1)=cfb;
nm1=n0-1;
for  k=nm1:-1:0;
cf=z.*cfa+(k+1.0d0).*cfb;
cpb(k+1+1)=cf;
cfb=cfa;
cfa=cf;
end;  k=0-1;
else;
m=100+abs(n);
cfa=c0;
cfb=complex(1.0d-30,0.0d0);
for  k=m:-1:0;
cf=z.*cfb+(k+1.0d0).*cfa;
if(k <= n0)
cpb(k+1+1)=cf;
end;
cfa=cfb;
cfb=cf;
end;  k=0-1;
cs0=ca0./cf;
for  k=0:n0;
cpb(k+1+1)=cs0.*cpb(k+1+1);
end;  k=n0+1;
end;
end;
end;
cpd(0+1+1)=-0.5d0.*z.*cpb(0+1+1);
if(n >= 0)
for  k=1:n;
cpd(k+1+1)=-0.5d0.*z.*cpb(k+1+1)+k.*cpb(k-1+1+1);
end;  k=n+1;
else;
for  k=1:n0;
cpd(k+1+1)=0.5d0.*z.*cpb(k+1+1)-cpb(k-1+1+1);
end;  k=n0+1;
end;
return;
end %subroutine cpbdn
function [n,z,cdn]=cpdsa(n,z,cdn);
%       ===========================================================
%       Purpose: Compute complex parabolic cylinder function Dn(z)
%                for small argument
%       Input:   z   --- complex argument of D(z)
%                n   --- Order of D(z) (n = 0,-1,-2,��
%       Output:  CDN --- Dn(z)
%       Routine called: GAIH for computing �x), x=n/2 (n=1,2,...)
%       ===========================================================
va0=[];ga0=[];xn=[];g1=[];vt=[];g0=[];vm=[];gm=[];
eps=1.0d-15;
pi=3.141592653589793d0;
sq2=sqrt(2.0d0);
ca0=exp(-.25d0.*z.*z);
va0=0.5d0.*(1.0d0-n);
if(n == 0.0)
cdn=ca0;
else;
if(abs(z) == 0.0)
if(va0 <= 0.0&&va0 == fix(va0))
cdn=0.0d0;
else;
[va0,ga0]=gaih(va0,ga0);
pd=sqrt(pi)./(2.0d0.^(-.5d0.*n).*ga0);
cdn=complex(pd,0.0d0);
end;
else;
xn=-n;
[xn,g1]=gaih(xn,g1);
cb0=2.0d0.^(-0.5d0.*n-1.0d0).*ca0./g1;
vt=-.5d0.*n;
[vt,g0]=gaih(vt,g0);
cdn=complex(g0,0.0d0);
cr=complex(1.0d0,0.0d0);
for  m=1:250;
vm=.5d0.*(m-n);
[vm,gm]=gaih(vm,gm);
cr=-cr.*sq2.*z./m;
cdw=gm.*cr;
cdn=cdn+cdw;
if(abs(cdw) < abs(cdn).*eps)
break;
end;
end;
cdn=cb0.*cdn;
end;
end;
return;
end %subroutine cpdsa
function [n,z,cdn]=cpdla(n,z,cdn);
%       ===========================================================
%       Purpose: Compute complex parabolic cylinder function Dn(z)
%                for large argument
%       Input:   z   --- Complex argument of Dn(z)
%                n   --- Order of Dn(z) (n = 0,�,�,��
%       Output:  CDN --- Dn(z)
%       ===========================================================

cb0=z.^n.*exp(-.25d0.*z.*z);
cr=complex(1.0d0,0.0d0);
cdn=complex(1.0d0,0.0d0);
for  k=1:16;
cr=-0.5d0.*cr.*(2.0.*k-n-1.0).*(2.0.*k-n-2.0)./(k.*z.*z);
cdn=cdn+cr;
if(abs(cr) < abs(cdn).*1.0d-12)
break;
end;
end;
cdn=cb0.*cdn;
return;
end %subroutine cpdla
function [x,ga]=gaih(x,ga);
%       =====================================================
%       Purpose: Compute gamma function �x)
%       Input :  x  --- Argument of �x), x = n/2, n=1,2,��!       Output:  GA --- �x)
%       =====================================================

pi=3.141592653589793d0;
if(x == fix(x)&&x > 0.0)
ga=1.0d0;
m1=fix(x-1.0);
for  k=2:m1;
ga=ga.*k;
end;  k=m1+1;
elseif(x+.5d0 == fix(x+.5d0)&&x > 0.0) ;
m=fix(x);
ga=sqrt(pi);
for  k=1:m;
ga=0.5d0.*ga.*(2.0d0.*k-1.0d0);
end;  k=m+1;
end;
return;
end %subroutine gaih




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