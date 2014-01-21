function zztest
 a=zeros();
 b=zeros();
%allocate(a(1000,3000),b(1000,3000));
a(:)=2;
[a,b]=sub1(a,b);
%contains;
function [a,b]=sub1(a,b,varargin);
 vecsum=zeros(size(b,1),size(b,2));
vecsum(:)=2;
fprintf(1,'%s ','shape(vecSum)=');
fprintf(1,'%0.15g \n',size(vecsum));
end %subroutine sub1;
end %program zztest

