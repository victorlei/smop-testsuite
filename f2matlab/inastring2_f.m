function out=inastring2_f(str,loc)

out=[];

temp=strfind(str,'''''');
for ii=1:length(loc)

 temp1=find(temp<loc(ii));
 if length(temp1)/2~=floor(length(temp1)/2)
  out(ii)=1;
 else
  out(ii)=0;
 end
 
end
