function [x1,x2,x,w,n]=dgauleg(x1,x2,x,w,n);


%----------------------------------------------------------------
clear global; clear functions;

persistent eps i j m p1 p2 p3 pp xl xm z z1 ; 

if isempty(eps), eps=3.0d-14; end;
if isempty(i), i=0; end;
if isempty(j), j=0; end;
if isempty(m), m=0; end;
if isempty(p1), p1=0; end;
if isempty(p2), p2=0; end;
if isempty(p3), p3=0; end;
if isempty(pp), pp=0; end;
if isempty(xl), xl=0; end;
if isempty(xm), xm=0; end;
if isempty(z), z=0; end;
if isempty(z1), z1=0; end;
m=fix(fix((n+1)./2));
z1=realmax;
xm=0.5d0.*(x2+x1);
xl=0.5d0.*(x2-x1);
for  i=1:m;
z=cos(3.141592654d0.*(i-.25d0)./(n+.5d0));
while(abs(z-z1) > eps);
p1=1.0d0;
p2=0.0d0;
for  j=1:n;
p3=p2;
p2=p1;
p1=((2.0d0.*j-1.0d0).*z.*p2-(j-1.0d0).*p3)./j;
end;  j=fix(n+1);
pp=n.*(z.*p1-p2)./(z.*z-1.0d0);
z1=z;
z=z1-p1./pp;
end;
x(i)=xm-xl.*z;
x(n+1-i)=xm+xl.*z;
w(i)=2.0d0.*xl./((1.0d0-z.*z).*pp.*pp);
w(n+1-i)=w(i);
end;  i=fix(m+1);
return;
end %subroutine dgauleg

