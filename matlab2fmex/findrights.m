function out=findrights(locs,str)
if ~isempty(locs)
 openstr=str(locs(1));
 switch openstr
  case '('
   closestr=')';
  case '['
   closestr=']';
  case '{'
   closestr='}';
 end
 l=(str==openstr);    r=(str==closestr);
 both=l-r;            c=cumsum(both);
 for i=1:length(locs)
  found=find(c==(c(locs(i))-1));
  found=found(found>locs(i));
  out(i)=found(1);
 end
else
 out=[];
end
