function outflag=inastring(loc,tempstr)
apost=0;
for k=1:loc
 if strcmp(tempstr(k),''''), apost=apost+1; end
 if ((strcmp(tempstr(k),' '))&(mod(apost,2)==1)), tempstr(k)=','; end
end
outflag=mod(apost,2);
