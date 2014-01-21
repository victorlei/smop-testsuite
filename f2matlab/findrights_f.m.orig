function out=findrights_f(locs,str,ignoreInQuotes)

if nargin<3, ignoreInQuotes=0; end
openstr=str(locs(1));

switch openstr
 case '('
  closestr=')';
 case '['
  closestr=']';
 case '{'
  closestr='}';
end
if ignoreInQuotes
 temp5=str=='''';
 temp6=cumsum(temp5);
 temp7=temp6/2~=round(temp6/2);

 
%%% %make it better... HA!! 
%%% 'wwwwwww',locs,str,kb
%%% 
%%% %make this better
%%% temp1=find(str==closestr | str==str(locs(1)));
%%% temp7a=validSpot(str,temp1);
%%% temp7=zeros(size(temp5));
%%% temp7(temp1)=temp7a;
 
 l=(str==openstr) & ~temp7;  %fl1=find(l);  fl2=~inastring_f(str,fl1);  l(fl1)=fl2;
 r=(str==closestr) & ~temp7; %fr1=find(r);  fr2=~inastring_f(str,fr1);  r(fr1)=fr2;
%%% l=(str==openstr);  fl1=find(l);  fl2=~inastring_f(str,fl1);  l(fl1)=fl2;
%%% r=(str==closestr); fr1=find(r);  fr2=~inastring_f(str,fr1);  r(fr1)=fr2;
 %'ddddddd',kb
else
 l=(str==openstr);  %fl1=find(l);  fl2=~inastring_f(str,fl1);  l(fl1)=fl2;
 r=(str==closestr); %fr1=find(r);  fr2=~inastring_f(str,fr1);  r(fr1)=fr2;
end
%%%l=(str==openstr)&~inastring_f(str,1:length(str));
%%%r=(str==closestr)&~inastring_f(str,1:length(str));
both=l-r;            c=cumsum(both);
for i=1:length(locs)
 found=find(c==(c(locs(i))-1));
 found=found(found>locs(i));
 if ~isempty(found)
  out(i)=found(1);
 else
  out(i)=0;
 end
end
