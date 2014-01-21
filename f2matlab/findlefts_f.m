function out=findlefts_f(locs,str)
closestr=str(locs(1));
switch closestr
 case ')'
  openstr='(';
 case ']'
  openstr='[';
 case '}'
  openstr='{';
end
l=(str==openstr);    r=(str==closestr);
both=l-r;            c=cumsum(both);
fc=[fliplr(c) 0];
for i=1:length(locs)
 floc=length(str)-locs(i)+1;
 found=find(fc==(fc(floc)));
 found=found(found>floc+1);
 out(i)=length(str)-found(1)+1+1;
end
