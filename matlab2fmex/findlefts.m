function out=findlefts(locs,str)
closestr=str(locs(1));
switch closestr
 case ')'
  openstr='(';
 case ']'
  openstr='[';
 case '}'
  openstr='{';
end
for i=1:length(locs)
 left=0;right=1;
 count=1;
 while left~=right
  if str(locs(i)-count)==openstr, left=left+1; end
  if str(locs(i)-count)==closestr, right=right+1; end
  count=count+1;
 end
 out(i)=locs(i)-(count-1);
end
